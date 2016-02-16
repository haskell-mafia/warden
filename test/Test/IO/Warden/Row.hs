{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Row where

import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString.Lazy       as BL
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.Csv
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Disorder.Core.IO

import           P

import           System.FilePath
import           System.IO
import           System.IO.Temp

import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Error
import           Warden.Row

import           X.Control.Monad.Trans.Either

prop_valid_svrows :: Separator -> FieldCount -> RowCount -> Property
prop_valid_svrows s i n = forAll (vectorOf (unRowCount n) $ validSVRow s i) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "valid_sv"
    BL.writeFile fp $ encodeWith opts svrs
    res <- runEitherT . mapEitherT runResourceT $ readViewFile s (LineBound 65536) (ViewFile fp) $$ CL.fold (flip (:)) []
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right rs -> do
        let expected = reverse $ (SVFields . V.fromList . getValidSVRow) <$> svrs
        pure $ expected === rs
 where
   opts = defaultEncodeOptions { encDelimiter = unSeparator s }

prop_invalid_svrows :: Separator -> RowCount -> Property
prop_invalid_svrows s n = forAll (vectorOf (unRowCount n) (invalidSVRow s)) $ \svrs ->
  testIO $ withSystemTempDirectory "warden-test" $ \tmp -> do
    let fp = tmp </> "sv"
    BL.writeFile fp $ (BL.intercalate "\r\n") svrs
    res <- runEitherT . mapEitherT runResourceT $ readViewFile s (LineBound 65536) (ViewFile fp) $$ CL.fold (flip (:)) []
    case res of
      Left err -> fail . T.unpack $ renderWardenError err
      Right rs ->
        pure $ [] === filter (not . rowFailed) rs
 where
  rowFailed (RowFailure _) = True
  rowFailed _              = False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })