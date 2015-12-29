{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Rows where


import qualified Data.ByteString.Lazy       as BL
import           Data.Csv
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Disorder.Core.IO

import           P
import qualified Pipes.Prelude              as PP

import           System.FilePath
import           System.IO
import           System.IO.Temp

import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Error
import           Warden.Rows

import           X.Control.Monad.Trans.Either

prop_valid_svrows :: Separator -> FieldCount -> RowCount -> Property
prop_valid_svrows s i n = forAll (vectorOf (getRowCount n) $ validSVRow s i) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "valid_sv"
    BL.writeFile fp $ encodeWith opts svrs
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVHandle s h
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right (SVEOF:rs) -> do
        let expected = reverse $ (SVFields . V.fromList . getValidSVRow) <$> svrs
        pure $ expected === rs
      Right x -> fail $ "impossible result from decoding: " <> show x
 where
  opts = defaultEncodeOptions { encDelimiter = unSeparator s }

prop_invalid_svrows :: Separator -> RowCount -> Property
prop_invalid_svrows s n = forAll (vectorOf (getRowCount n) (invalidSVRow s)) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "sv"
    BL.writeFile fp $ (BL.intercalate "\r\n") svrs
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVHandle s h
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right (SVEOF:rs) ->
        pure $ [] === filter (not . rowFailed) rs
      Right x -> fail $ "impossible result from decoding: " <> show x
 where
  rowFailed (RowFailure _) = True
  rowFailed _              = False

prop_invalid_svdoc :: Separator -> Property
prop_invalid_svdoc s = forAll (invalidSVDocument s) $ \doc ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "sv"
    BL.writeFile fp doc
    res <- withFile fp ReadMode $ \h -> do
      runEitherT $ PP.fold (flip (:)) [] id $ readSVHandle s h
    pure $ True === isLeft res

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
