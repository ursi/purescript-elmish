{ name = "my-project"
, dependencies =
  [ "console"
  , "debuggest"
  , "foreign-object"
  , "heterogeneous"
  , "js-timers"
  , "mason-prelude"
  , "parallel"
  , "psci-support"
  , "task"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "whatwg-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
