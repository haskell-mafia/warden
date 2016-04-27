{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Commands.Check.Unit where

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S

import           Disorder.Core.IO (testIO)

import           P

import           System.FilePath ((</>))
import           System.IO
import           System.Process (proc)

import           Test.QuickCheck
import           Test.IO.Warden

import           Tine.Process (execProcess)

import           Warden.Commands
import           Warden.Data
import           Warden.Error
import           Warden.Param

import           X.Control.Monad.Trans.Either (runEitherT)

commandUnitCheckParams :: CheckParams
commandUnitCheckParams = CheckParams {
    checkSeparator = charToSeparator '|'
  , checkVerbosity = Quiet
  , checkForce = NoForce
  , checkLineBound = LineBound 65536
  , checkFreeformThreshold = TextFreeformThreshold 10
  , checkSchemaFile = Nothing
  , checkExitType = ExitWithSuccess
  , checkIncludeDotFiles = NoIncludeDotFiles
  , checkSamplingType = NoSampling
  , checkFileFormat = RFC4180
  , checkPIICheckType = PIIChecks (MaxPIIObservations 1000)
  }

checkUnitTest :: View -> CheckParams -> (Either WardenError (NonEmpty CheckResult) -> Bool) -> Property
checkUnitTest (View v) cps expected = testIO $ do
  wps <- buildWardenParams (WardenVersion "test-io")
  r <- withTestView $ \vd ->
    let v' = (unView vd) </> "view" in do
    void $ execProcess (proc "cp" ["-r", v, v'])
    runResourceT . runEitherT $
      check wps (View v') cps
  pure $ expected r === True

prop_check_FieldCountMismatch :: Property
prop_check_FieldCountMismatch =
  checkUnitTest
    (View "test/data/commands/check/field-counts")
    commandUnitCheckParams
    expected
  where
    expected (Left _) = False
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed (RowCheckFailure (FieldCountMismatch (S.fromList [FieldCount 2, FieldCount 3])) :| []))) rs

prop_check_ZeroRows :: Property
prop_check_ZeroRows =
  checkUnitTest
    (View "test/data/commands/check/zero-rows")
    commandUnitCheckParams
    expected
  where
    expected (Left _) = False
    -- We get this one twice, from checkFieldCounts and checkTotalRows.
    -- Consider nubbing the failure lists at some point?
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed ((RowCheckFailure ZeroRows) :| [RowCheckFailure ZeroRows]))) rs

prop_check_error :: Property
prop_check_error =
  checkUnitTest
    (View "test/data/commands/check/empty-view")
    commandUnitCheckParams
    expected
  where
    expected = (== Left (WardenTraversalError EmptyView))

prop_check_BadRows :: Property
prop_check_BadRows =
  checkUnitTest
    (View "test/data/commands/check/bad-rows")
    commandUnitCheckParams
    expected
  where
    expected (Left _) = False
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed ((RowCheckFailure (HasBadRows (RowCount 1))) :| []))) rs

prop_check_FieldCountObservationMismatch :: Property
prop_check_FieldCountObservationMismatch =
  checkUnitTest
    (View "test/data/commands/check/schema-field-type")
    (commandUnitCheckParams { checkSchemaFile = Just (SchemaFile "test/data/commands/check/schema-field-type.json") } )
    expected
  where
    expected (Left _) = False
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed ((SchemaCheckFailure (FieldCountObservationMismatch (FieldCount 2) (FieldCount 1))) :| []))) rs

prop_check_PotentialPIIFailure_email :: Property
prop_check_PotentialPIIFailure_email =
  checkUnitTest
    (View "test/data/commands/check/pii-email")
    commandUnitCheckParams
    expected
  where
    expected (Left _) = False
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed ((PIICheckFailure (PotentialPIIFailure (PotentialPII EmailAddress (FieldIndex 1) :| []))) :| []))) rs

prop_check_PotentialPIIFailure_phone :: Property
prop_check_PotentialPIIFailure_phone =
  checkUnitTest
    (View "test/data/commands/check/pii-phone")
    commandUnitCheckParams
    expected
  where
    expected (Left _) = False
    expected (Right rs) = elem (RowCheckResult ViewRowCounts (CheckFailed ((PIICheckFailure (PotentialPIIFailure (PotentialPII PhoneNumber (FieldIndex 0) :| []))) :| []))) rs

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
