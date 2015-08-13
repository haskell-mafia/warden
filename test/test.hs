import           Disorder.Core.Main

import qualified Test.Warden
import qualified Test.Warden.Fold

main :: IO ()
main = disorderMain [
           Test.Warden.tests
         , Test.Warden.Fold.tests
         ]
