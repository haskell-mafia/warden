{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Commands.Check.Unit where

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.List.NonEmpty (NonEmpty(..))

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
    checkSeparator = (charToSeparator '|')
  , checkVerbosity = Quiet
  , checkForce = NoForce
  , checkLineBound = LineBound 65536
  , checkFreeformThreshold = TextFreeformThreshold 10
  , checkSchemaFile = Nothing
  }

checkUnitTest :: View -> CheckParams -> Either WardenError (NonEmpty CheckResult) -> Property
checkUnitTest (View v) cps expected = testIO $ do
  wps <- buildWardenParams (WardenVersion "test-io")
  r <- withTestView $ \vd ->
    let v' = (unView vd) </> "view" in do
    void $ execProcess (proc "cp" ["-r", v, v'])
    runResourceT . runEitherT $
      check wps (View v') cps
  pure $ r === expected

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
