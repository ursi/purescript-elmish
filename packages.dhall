let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let overrides = {=}

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/dev/packages.dhall sha256:d0d6f5b652c4fa724f4b57f39b62c161e1679f344a2ffe8421926e416daf22fd

in  upstream ⫽ overrides ⫽ additions
