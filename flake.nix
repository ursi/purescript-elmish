{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purescript-language-server.url = "github:ursi/purescript-language-server/purs-nix";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils/1";
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.default-systems
      ({ make-shell, purescript-language-server, purs-nix, pkgs, ... }:
         let
           inherit (purs-nix) purs;
           package = import ./package.nix purs-nix;

           inherit
             (purs
                { inherit (package) dependencies;
                  src = ./src;
                }
             )
             command;
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purescript-language-server
                     purs-nix.purescript
                     (command {})
                   ];
               };
         }
      )
      { inherit inputs nixpkgs; };
}
