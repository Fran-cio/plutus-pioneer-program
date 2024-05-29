{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext (scriptContextTxInfo), TxOutRef,
                                       mkMintingPolicyScript, TxInfo (txInfoInputs, txInfoMint), TxInInfo (txInInfoOutRef))
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.), traceIfFalse, (&&), any, (==), emptyByteString)
import           Utilities            (wrapPolicy)
import Plutus.V1.Ledger.Value (flattenValue, currencySymbol, tokenName, TokenName (TokenName))
import Data.ByteString (empty)


-- Definir un TokenName vacío
emptyTokenName :: TokenName
emptyTokenName = TokenName emptyByteString

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong name minted" checkMintedName
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = PlutusTx.Prelude.any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedName :: Bool
    checkMintedName = case flattenValue (txInfoMint info) of
        [(_, tn , amt)] -> tn == emptyTokenName && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> MintingPolicy
nftPolicy oref = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
