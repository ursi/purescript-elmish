{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      make-shell.url = "github:ursi/nix-make-shell/1";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:ursi/purs-nix/ps-0.15";
      utils.url = "github:ursi/flake-utils/8";
    };

  outputs = { utils, ... }@inputs:
    utils.apply-systems
      { inherit inputs;
        systems = [ "x86_64-linux" "x86_64-darwin" ];
      }
      ({ make-shell, pkgs, ps-tools, purs-nix, ... }:
         let
           inherit (purs-nix) purs ps-pkgs;
           package = import ./package.nix purs-nix;
           inherit (purs { inherit (package) dependencies; }) command;
           testing =
             purs
               { dependencies = package.dependencies ++ [ ps-pkgs.now ];
                 dir = ./.;
                 srcs = [ "src" "test" ];
               };
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purs-nix.purescript
                     ps-tools.for-0_15.purescript-language-server
                     (command {})

                     (testing.command
                       { name = "purs-nix-test";
                         bundle = { module = "Test.Main"; };
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
      );
}
