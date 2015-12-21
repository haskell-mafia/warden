import           Disorder.Core.Main

import qualified Test.IO.Warden.Check
import qualified Test.IO.Warden.IO
import qualified Test.IO.Warden.View

main :: IO ()
main = disorderMain [
           Test.IO.Warden.IO.tests
         , Test.IO.Warden.Check.tests
         , Test.IO.Warden.View.tests
         ]
