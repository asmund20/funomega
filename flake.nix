{
  description = "Development setup for compiling Haskell using Cabal";

  inputs =
  {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.11" ;
    flake-utils.url = "github:numtide/flake-utils"       ;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          manager  = import nixpkgs { inherit system; } ;
          system   = "x86_64-linux"                     ;
          ghc      = manager.haskell.compiler.ghc96     ;
          cabal    = manager.cabal-install              ;
          terminal = manager.mkShell                    ;
          hlint    = manager.haskellPackages.hlint      ;
        in
        {
          devShell = terminal
          { buildInputs = [ ghc cabal hlint ];
          };
        }
      );
}
