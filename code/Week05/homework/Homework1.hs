{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework1 where

import           Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V2.Ledger.Api
    ( to,
      mkMintingPolicyScript,
      PubKeyHash,
      MintingPolicy,
      POSIXTime,
      POSIXTimeRange,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoValidRange),
      BuiltinData )

import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (), ($), (&&))
import           Utilities            (wrapPolicy)
import Plutus.V1.Ledger.Interval (contains)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy pkh deadline () ctx =  checkDeadline && checkSig
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        txValidRange :: POSIXTimeRange
        txValidRange  = txInfoValidRange txInfo

        checkSig :: Bool
        checkSig = txSignedBy txInfo pkh

        checkDeadline :: Bool
        checkDeadline = contains (to deadline) txValidRange

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
