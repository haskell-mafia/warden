import           Disorder.Core.Main

import qualified Test.IO.Warden.Check.File
import qualified Test.IO.Warden.Chunk
import qualified Test.IO.Warden.Commands
import qualified Test.IO.Warden.Commands.Check.Unit
import qualified Test.IO.Warden.Marker
import qualified Test.IO.Warden.Row
import qualified Test.IO.Warden.Schema
import qualified Test.IO.Warden.Serial.Json.Marker.Unit
import qualified Test.IO.Warden.View
import qualified Test.IO.Warden.View.Unit

main :: IO ()
main = disorderMain [
           Test.IO.Warden.Check.File.tests
         , Test.IO.Warden.Chunk.tests
         , Test.IO.Warden.Commands.tests
         , Test.IO.Warden.Commands.Check.Unit.tests
         , Test.IO.Warden.Marker.tests
         , Test.IO.Warden.Row.tests
         , Test.IO.Warden.Schema.tests
         , Test.IO.Warden.Serial.Json.Marker.Unit.tests
         , Test.IO.Warden.View.tests
         , Test.IO.Warden.View.Unit.tests
         ]
