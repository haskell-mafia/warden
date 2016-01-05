{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.View where

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.IO.Warden

import           Warden.View

import           X.Control.Monad.Trans.Either (runEitherT)

prop_traverseView_valid :: Property
prop_traverseView_valid = withValidDirTree $ \v -> do
  r <- runEitherT $ traverseView v
  pure $ (isRight r) === True

prop_traverseView_invalid :: Property
prop_traverseView_invalid = withInvalidDirTree $ \v -> do
  r <- runEitherT $ traverseView v
  pure $ (isLeft r) === True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10, maxSize = 20 })
