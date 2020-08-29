{ name = "my-project"
, dependencies =
  [ "console"
  , "foreign-object"
  , "generics-rep"
  , "mason-prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
