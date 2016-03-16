{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Data.TextCounts where

import qualified Data.IntSet as IS
import           Data.List (take)
import           Data.Text (Text)
import qualified Data.Vector as V

import           Disorder.Core.Property (failWith)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Warden.Arbitrary ()

import           Warden.Data.TextCounts

prop_hashText :: UniquePair Text -> Property
prop_hashText (UniquePair x y) =
  let x' = hashText x
      y' = hashText y in
  (x' /= y') === True

prop_combineUniqueTextCounts :: Property
prop_combineUniqueTextCounts =
  let n = (+) 1 $ (unTextFreeformThreshold textFreeformThreshold) `div` 2
      l1 = take n [1..]
      l2 = take n [(unTextFreeformThreshold textFreeformThreshold)..]
      a = UniqueTextCount $ IS.fromList l1
      b = UniqueTextCount $ IS.fromList l2 in
  (combineUniqueTextCounts a b) === LooksFreeform

prop_combineTextCounts :: TextCounts -> TextCounts -> Property
prop_combineTextCounts a b =
  case combineTextCounts a b of
    NoTextCounts -> (noCounts a && noCounts b) === True
    TextCounts csc ->
      case (a, b) of
        (TextCounts csa, TextCounts csb) ->
           ((V.all id $ V.zipWith gte csc csa)
             && (V.all id $ V.zipWith gte csc csb)) === True
        (TextCounts csa, NoTextCounts) ->
           (V.all id $ V.zipWith gte csc csa) === True
        (NoTextCounts, TextCounts csb) ->
           (V.all id $ V.zipWith gte csc csb) === True
        (NoTextCounts, NoTextCounts) ->
           failWith "non-null result combining two null TextCounts"
  where
    noCounts NoTextCounts = True
    noCounts (TextCounts _) = False

    gte LooksFreeform _ = True
    gte (UniqueTextCount _) LooksFreeform = False
    gte (UniqueTextCount csc') (UniqueTextCount csa') =
      IS.size csc' >= IS.size csa'

return []
tests :: IO Bool
tests = $quickCheckAll
