{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils/2";
    };

  outputs = { utils, ... }@inputs:
    utils.default-systems
      ({ make-shell, purs-nix, pkgs, ... }:
         let
           inherit (purs-nix) purs;
           package = import ../package.nix purs-nix;
           inherit (purs { inherit (package) dependencies; }) command;
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     purs-nix.purescript
                     purs-nix.purescript-language-server
                     (command { srcs = [ "src" "../src" ]; })
                   ];
               };
         }
      )
      inputs;
}
