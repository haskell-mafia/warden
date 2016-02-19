{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Chunk where

import           Data.ByteString (hPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty as NE

import           Disorder.Core.IO (testIO)

import           P

import           System.Entropy (getEntropy)
import           System.IO (IO, hClose)

import           Test.IO.Warden
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Test.Warden.Arbitrary ()

import           Warden.Chunk
import           Warden.Data

prop_chunk_one :: ChunkCount -> Property
prop_chunk_one n = forAll (arbitrary `suchThat` ((< (1024*1024)) . BS.length)) $ \bs -> 
  testIO . withTestFile $ \(ViewFile fp) h -> do
  hPut h bs
  hClose h
  cs <- chunk n fp
  pure $ NE.length cs === 1

prop_chunk_many :: ChunkCount -> Property
prop_chunk_many n = forAll (choose (1024*1024, 10*1024*1024)) $ \m ->
  testIO . withTestFile $ \(ViewFile fp) h -> do
  bs <- getEntropy m
  let nls = BSC.count '\n' bs
  hPut h bs
  hClose h
  cs <- chunk n fp
  pure $ (NE.length cs >= 1, NE.length cs <= nls + 1) === (True, True)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
