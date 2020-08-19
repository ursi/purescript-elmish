let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let overrides = {=}

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/1/packages.dhall sha256:b4cd948a32b22fd2093a8c69acb886671a024434a1c8c7e9762a2697bd9e6543

in  upstream // overrides // additions
