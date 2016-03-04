{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Warden.Data.View where

import           Data.List (isInfixOf)
import qualified Data.Text as T

import           Disorder.Core.Tripping (tripping)
import           Disorder.Core.UniquePair (UniquePair(..))
import           Disorder.Corpus (muppets)

import           P

import           System.FilePath ((</>))
import           System.IO (IO)

import           Test.QuickCheck
import           Test.Warden.Arbitrary ()

import           Warden.Data.View

prop_joinDir :: [DirName] -> Property
prop_joinDir ds =
  let dp = joinDir ds in
  (all (flip isInfixOf dp) (unDirName <$> ds)) === True

prop_removeViewPrefix_pos :: View -> Property
prop_removeViewPrefix_pos (View v) = forAll (elements muppets) $ \suf ->
  let fp = v </> (T.unpack suf) in
  removeViewPrefix (View v) fp === Just (T.unpack suf)

prop_removeViewPrefix_neg :: Property
prop_removeViewPrefix_neg = forAll ((,) <$> (arbitrary `suchThat` divergent) <*> elements muppets) $ \(UniquePair (View v) w, suf) ->
  let fp = v </> (T.unpack suf) in
  removeViewPrefix w fp === Nothing
  where
    divergent (UniquePair (View v) (View w)) = head v /= head w

prop_viewFile :: ViewFile -> Property
prop_viewFile =
  tripping viewFilePath viewFile

return []
tests :: IO Bool
tests = $quickCheckAll

