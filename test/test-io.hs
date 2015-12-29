import           Disorder.Core.Main

import qualified Test.IO.Warden.Check
import qualified Test.IO.Warden.Rows
import qualified Test.IO.Warden.View

main :: IO ()
main = disorderMain [
           Test.IO.Warden.Rows.tests
         , Test.IO.Warden.Check.tests
         , Test.IO.Warden.View.tests
         ]
