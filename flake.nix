{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils/2";
    };

  outputs = { utils, ... }@inputs:
    utils.default-systems
      ({ make-shell, purs-nix, pkgs, ... }:
         let
           inherit (purs-nix) purs ps-pkgs;
           package = import ./package.nix purs-nix;
           inherit (purs { inherit (package) dependencies; }) command;
           testing = purs { dependencies = package.dependencies ++ [ ps-pkgs.now ]; };
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purs-nix.purescript
                     purs-nix.purescript-language-server
                     (command {})
                     (testing.command
                       { name = "purs-nix-test";
                         bundle = { module = "Test.Main"; };
                         srcs = [ "src" "test" ];
                       }
                     )
                     (command
                       { name = "codegen";
                         bundle = { module = "GenCss"; };
                         srcs = [ "src" "codegen" ];
                       }
                     )
                   ];

                 aliases.bundle-test = "purs-nix-test bundle";

                 functions.open =
                   ''
                   echo "<script defer src='index.js'></script>" > index.html
                   brave index.html
                   '';
               };
         }
      )
      inputs;
}
