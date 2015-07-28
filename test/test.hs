import           Disorder.Core.Main

import qualified Test.Warden

main :: IO ()
main = disorderMain [
           Test.Warden.tests
         ]
