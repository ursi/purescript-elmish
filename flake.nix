{ inputs.purs-nix.url = "github:ursi/purs-nix";

  outputs = { nixpkgs, utils, purs-nix, ... }:
    utils.defaultSystems
      ({ pkgs, system }: with pkgs;
         let
           inherit (purs-nix { inherit system; }) purs ps-pkgs ps-pkgs-ns;

           inherit
             (purs
               { inherit (import ./package.nix { inherit ps-pkgs ps-pkgs-ns; }) dependencies;
                 src = ./src;
               }
             )
             shell;
         in
         { apps =
             { bundle = bundle {};
               compile = compile {};
             };

           devShell =
             mkShell
               { buildInputs =
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     nodePackages.uglify-js
                     purescript
                     (shell {})
                   ];
               };
         }
      )
      nixpkgs;
}
