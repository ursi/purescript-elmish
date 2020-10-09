{ name = "my-project"
, dependencies =
  [ "console"
  , "foreign-object"
  , "generics-rep"
  , "js-timers"
  , "mason-prelude"
  , "parallel"
  , "psci-support"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "whatwg-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
