{ name = "my-project"
, dependencies = (../spago.dhall).dependencies
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "../src/**/*.purs" ]
}
