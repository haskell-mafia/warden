import           Disorder.Core.Main

main :: IO ()
main =
  disorderCliMain ["./dist/build/warden/warden"]
