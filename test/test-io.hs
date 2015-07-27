import           Disorder.Core.Main

import qualified Test.IO.Warden.IO

main :: IO ()
main = disorderMain [
           Test.IO.Warden.IO.tests
         ]
