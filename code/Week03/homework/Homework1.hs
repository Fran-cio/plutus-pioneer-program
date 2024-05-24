{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains, overlaps)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,ScriptContext (scriptContextTxInfo)
                                       ,to, from, TxInfo(txInfoValidRange), mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
                                       
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..),traceIfFalse, not, ($), (&&), (||), (+),(-))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx =
    let txInfo = scriptContextTxInfo ctx
        currentSlot = txInfoValidRange txInfo
        beneficiary1Signature = txSignedBy txInfo (beneficiary1 dat)
        beneficiary2Signature = txSignedBy txInfo (beneficiary2 dat)
        slotBeforeDeadline = contains (to $ deadline dat) currentSlot 
        deadlinePassed = contains (from $ deadline dat + 1) currentSlot 
    in (beneficiary1Signature && slotBeforeDeadline) ||
       (beneficiary2Signature && deadlinePassed)

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
