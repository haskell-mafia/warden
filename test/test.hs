import           Disorder.Core.Main

import qualified Test.Warden
import qualified Test.Warden.Data
import qualified Test.Warden.Data.Marker
import qualified Test.Warden.Data.View
import qualified Test.Warden.Numeric
import qualified Test.Warden.Sampling.Reservoir

main :: IO ()
main = disorderMain [
    Test.Warden.tests
  , Test.Warden.Data.tests
  , Test.Warden.Data.Marker.tests
  , Test.Warden.Data.View.tests
  , Test.Warden.Numeric.tests
  , Test.Warden.Sampling.Reservoir.tests
  ]
