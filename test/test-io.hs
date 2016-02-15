import           Disorder.Core.Main

import qualified Test.IO.Warden.Check.File
import qualified Test.IO.Warden.Marker
import qualified Test.IO.Warden.Row
import qualified Test.IO.Warden.View
import qualified Test.IO.Warden.View.Unit

main :: IO ()
main = disorderMain [
           Test.IO.Warden.Check.File.tests
         , Test.IO.Warden.Marker.tests
         , Test.IO.Warden.Row.tests
         , Test.IO.Warden.View.tests
         , Test.IO.Warden.View.Unit.tests
         ]
