{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ 
    "console",
    "datetime",
    "effect",
    "psci-support",
    "halogen",
    "aff",
    "affjax",
    "strings",
    "routing-duplex"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
