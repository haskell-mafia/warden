import           Disorder.Core.Main

import qualified Test.Warden
import qualified Test.Warden.Fold
import qualified Test.Warden.Numeric

main :: IO ()
main = disorderMain [
           Test.Warden.tests
         , Test.Warden.Fold.tests
         , Test.Warden.Numeric.tests
         ]
