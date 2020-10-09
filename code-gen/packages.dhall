let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let overrides = {=}

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/dev/packages.dhall sha256:9507077571543a45fcb201a3fc7111758723955272b8b377f663ee76ae78a2d2

in  upstream // overrides // additions
