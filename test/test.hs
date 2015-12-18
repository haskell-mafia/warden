import           Disorder.Core.Main

import qualified Test.Warden
import qualified Test.Warden.Data
import qualified Test.Warden.Fold
import qualified Test.Warden.Numeric
import qualified Test.Warden.Sampling.Reservoir

main :: IO ()
main = disorderMain [
           Test.Warden.tests
         , Test.Warden.Data.tests
         , Test.Warden.Fold.tests
         , Test.Warden.Numeric.tests
         , Test.Warden.Sampling.Reservoir.tests
         ]
