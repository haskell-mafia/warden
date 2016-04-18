{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Parser.Row where

import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (muppets)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Parser.Row

prop_unescapedFieldP :: Separator -> Property
prop_unescapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) f) === Right f

prop_escapedFieldP :: Separator -> Property
prop_escapedFieldP s = forAll (validSVField s) $ \f ->
  (parseOnly (rawFieldP s) ("\"" <> f <> "\"")) === Right f

prop_escapedFieldP_infix :: Separator -> Property
prop_escapedFieldP_infix s = forAll (validSVField s) $ \f ->
  let s' = BS.pack . pure $ unSeparator s
      middle = f <> s' <> f <> "\"\"" <> s' <> f in
  (parseOnly (rawFieldP s) ("\"" <> middle <> "\"")) === Right middle

prop_numericFieldP_pos :: Int -> Double -> Property
prop_numericFieldP_pos n m =
  let tn = T.encodeUtf8 $ renderIntegral n
      tm = T.encodeUtf8 $ renderFractional m
      n' = parseOnly numericFieldP tn
      m' = parseOnly numericFieldP tm in
  (Right . NumericField $ fromIntegral n, Right $ NumericField m) === (n', m')

prop_numericFieldP_neg :: Property
prop_numericFieldP_neg = forAll (elements muppets) $ \t ->
  let r = parseOnly numericFieldP $ T.encodeUtf8 t in
  isLeft r === True

return []
tests :: IO Bool
tests = $quickCheckAll
