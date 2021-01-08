{
  outputs = { self, nixpkgs, utils }:
    utils.simpleShell
      [
        "dhall"
        "nodejs"
        "nodePackages.uglify-js"
        "purescript"
        "spago"
      ]
      nixpkgs;
}
