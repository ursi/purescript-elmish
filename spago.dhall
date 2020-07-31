{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
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
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
