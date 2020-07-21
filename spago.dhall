{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "functions"
  , "identity"
  , "integers"
  , "newtype"
  , "partial"
  , "psci-support"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
