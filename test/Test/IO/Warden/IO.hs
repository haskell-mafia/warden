{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.IO.Warden.IO where

import P

import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO
import System.IO.Temp
import System.FilePath
import qualified Pipes.Prelude as PP
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Disorder.Core.IO

import Test.Warden.Arbitrary

import Warden.Data
import Warden.Error
import Warden.IO

prop_valid_svrows :: SVSep -> FieldCount -> RowCount -> Property
prop_valid_svrows s i n = forAll (vectorOf (getRowCount n) $ validSVRow s i) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "valid_sv"
    BL.writeFile fp $ encodeWith opts svrs
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVRows (getSVSep s) h
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right (SVEOF:rs) -> do
        let expected = reverse $ (SVFields . V.fromList . getValidSVRow) <$> svrs
        pure $ expected === rs
      Right x -> fail $ "impossible result from decoding: " <> show x
 where
  opts = defaultEncodeOptions { encDelimiter = getSVSep s }

prop_invalid_svrows :: SVSep -> RowCount -> Property
prop_invalid_svrows s n = forAll (vectorOf (getRowCount n) (invalidSVRow s)) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "sv"
    BL.writeFile fp $ (BL.intercalate "\r\n") svrs
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVRows (getSVSep s) h
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right (SVEOF:rs) ->
        pure $ [] === filter (not . rowFailed) rs
      Right x -> fail $ "impossible result from decoding: " <> show x
 where
  rowFailed (RowFailure _) = True
  rowFailed _              = False

prop_invalid_svdoc :: SVSep -> Property
prop_invalid_svdoc s = forAll invalidSVDocument $ \doc ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "sv"
    BL.writeFile fp doc
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVRows (getSVSep s) h
    pure $ True === isLeft res

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
