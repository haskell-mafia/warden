{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Schema where

import           Control.Monad.IO.Class (liftIO)

import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (removeFile)
import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.IO.Warden
import           Test.Warden.Arbitrary ()

import           Warden.Data
import           Warden.Schema

import           X.Control.Monad.Trans.Either (bracketEitherT')

prop_writeSchema :: Schema -> Property
prop_writeSchema s = testIO . withTestFile $ \fp h -> unsafeWarden $ do
  liftIO $ hClose h
  s' <- bracketEitherT' (writeSchema s (SchemaFile fp) >> pure fp)
                        (\fp' -> liftIO $ removeFile fp')
                        (\fp' -> readSchema (SchemaFile fp'))
  pure $ s === s'
  
return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
