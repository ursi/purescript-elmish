let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let overrides = {=}

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/dev/packages.dhall sha256:83669ffb138d9c7f063f48e93e5b5ff226aaaec33ce0e946ee9167456606c3db

in  upstream // overrides // additions
