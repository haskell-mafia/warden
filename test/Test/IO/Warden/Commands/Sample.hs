{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Commands.Sample where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Gen (genValidUtf81)

import           P

import           System.IO
import           System.FilePath ((</>))

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Test.IO.Warden
import           Test.Warden.Arbitrary

import           Warden.Commands.Sample
import           Warden.Data
import           Warden.Error
import           Warden.Marker

import           X.Control.Monad.Trans.Either (runEitherT)

prop_readNumericSummary :: ViewMarker -> Property
prop_readNumericSummary vm =
  forAll (genNumericFieldSummary `suchThat` (/= NoNumericFieldSummary)) $ \nfs ->
  testIO $ withTestView $ \v -> unsafeWarden $ do
  let
    summaries NoNumericFieldSummary = V.empty
    summaries (NumericFieldSummary xs) = xs

    vm' = setNumericFieldSummary nfs $ vm { vmView = v }
    mp = viewMarkerPath vm'
    nss = summaries nfs

  writeViewMarker vm'
  nss' <- readNumericSummary mp
  pure $ nss === nss'

prop_readNumericSummary_no :: ViewMarker -> Property
prop_readNumericSummary_no vm =
  testIO $ withTestView $ \v -> unsafeWarden $ do
  let
    vm' = setNumericFieldSummary NoNumericFieldSummary $ vm { vmView = v }
    mp = viewMarkerPath vm'

  writeViewMarker vm'
  nss' <- liftIO . runResourceT . runEitherT $ readNumericSummary mp
  pure $ Left (WardenSampleError (NoNumericSummaries mp)) === nss'

prop_writeFieldSummaryStats_header :: SummaryStatsRecord -> Property
prop_writeFieldSummaryStats_header ssr =
  forAll genValidUtf81 $ \name ->
  testIO $ withTestView $ \v -> unsafeWarden $ do
    let
      ssrs = V.singleton ssr
      fp = (unView v) </> "summary.csv"

    writeFieldSummaryStats fp name ssrs
    x <- liftIO $ withFile fp ReadMode T.hGetLine

    let
      commas = T.count "," $ renderSummaryStatsRecord ssr
      commas' = T.count "," x

    pure $ commas === commas'

setNumericFieldSummary :: NumericFieldSummary -> ViewMarker -> ViewMarker
setNumericFieldSummary nfs vm =
  let
    meta = vmMetadata vm
    rcs = vmViewCounts meta
    rcs' = rcs { rcsNumericSummaries = nfs }
    meta' = meta { vmViewCounts = rcs' }
  in
  vm { vmMetadata = meta' }

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
