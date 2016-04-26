import           Disorder.Core.Main

import qualified Test.Warden.Data.Check
import qualified Test.Warden.Data.Field
import qualified Test.Warden.Data.FieldAnomaly
import qualified Test.Warden.Data.Marker
import qualified Test.Warden.Data.Numeric
import qualified Test.Warden.Data.Row
import qualified Test.Warden.Data.Schema
import qualified Test.Warden.Data.TextCounts
import qualified Test.Warden.Data.View
import qualified Test.Warden.Inference
import qualified Test.Warden.Numeric
import qualified Test.Warden.Parser.Common
import qualified Test.Warden.Parser.PII
import qualified Test.Warden.Parser.Row.RFC4180
import qualified Test.Warden.PII
import qualified Test.Warden.Row
import qualified Test.Warden.Serial.Json.Marker
import qualified Test.Warden.Serial.Json.Numeric
import qualified Test.Warden.Serial.Json.Schema

main :: IO ()
main = disorderMain [
    Test.Warden.Data.Check.tests
  , Test.Warden.Data.Field.tests
  , Test.Warden.Data.FieldAnomaly.tests
  , Test.Warden.Data.Marker.tests
  , Test.Warden.Data.Numeric.tests
  , Test.Warden.Data.Row.tests
  , Test.Warden.Data.Schema.tests
  , Test.Warden.Data.TextCounts.tests
  , Test.Warden.Data.View.tests
  , Test.Warden.Inference.tests
  , Test.Warden.Numeric.tests
  , Test.Warden.Parser.Common.tests
  , Test.Warden.Parser.PII.tests
  , Test.Warden.Parser.Row.RFC4180.tests
  , Test.Warden.PII.tests
  , Test.Warden.Row.tests
  , Test.Warden.Serial.Json.Marker.tests
  , Test.Warden.Serial.Json.Numeric.tests
  , Test.Warden.Serial.Json.Schema.tests
  ]
