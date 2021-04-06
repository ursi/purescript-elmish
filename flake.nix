{ inputs.easy-ps =
    { url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

  outputs = { nixpkgs, utils, easy-ps, ... }:
    utils.mkShell
      ({ pkgs, ... }: with pkgs;
         { buildInputs =
             [ dhall
               nodejs
               nodePackages.bower
               nodePackages.pulp
               nodePackages.uglify-js
               purescript
             ]
             ++ (with import (easy-ps) { inherit pkgs; };
                 [ spago
                   spago2nix
                 ]
                );
         }
      )
      nixpkgs;
}
