{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "psci-support"
  , "remotedata"
  , "routing-duplex"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
