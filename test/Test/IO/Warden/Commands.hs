{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Commands where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (withCurrentDirectory)
import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.IO.Warden
import           Test.Warden.Arbitrary ()

import           Warden.Commands
import           Warden.Data
import           Warden.Schema

-- Generate a view, run checks over it, generate a schema from the
-- marker, and make sure the check passes run again with the schema it
-- just generated.
prop_infer_tripping :: WardenParams -> CheckParams -> Property
prop_infer_tripping wps cps = testIO $ withTestView $ \(View vd) ->
  let cps' = cps {
                 checkSeparator = (charToSeparator '|')
               , checkVerbosity = Quiet
               , checkLineBound = LineBound 65536
               , checkFreeformThreshold = TextFreeformThreshold 10
               , checkSchemaFile = Nothing
               } in do
  withCurrentDirectory vd $ unsafeWarden $ do
    v <- liftIO $ generateView NonDeterministic "." (RecordCount 1000) (GenSize 4) (LineSize 100)
    rs1 <- check wps v cps'
    when (checkHasFailures rs1) . fail . T.unpack $
      "Initial check unexpectedly failed.\n\n" <> T.intercalate "\n" (NE.toList . (=<<) renderCheckResult $ rs1)
    fs <- traverseTestDirectory (DirName "./_warden")
    case fs of
      [] -> fail "No view marker files written."
      [vmf] -> do
        sch <- infer Quiet (FieldMatchRatio 0.99) [vmf]
        writeSchema sch (SchemaFile "test-schema.json")
        let cps'' = cps' {
                        checkSchemaFile = Just (SchemaFile "test-schema.json")
                      , checkForce = Force
                      }
        rs2 <- check wps v cps''
        pure $ (checkHasFailures rs2) === False
      xs -> fail $ "Expected a single view marker file, got " <> show xs

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
