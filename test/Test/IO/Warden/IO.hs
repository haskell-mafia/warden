{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

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

prop_svrows :: SVSep -> FieldCount -> RowCount -> Property
prop_svrows s i n = forAll (vectorOf (getRowCount n) $ validSVRow s i) $ \svrs ->
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

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
