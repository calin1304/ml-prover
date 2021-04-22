let
  # overlay = self: super: {
  #   haskell = super.haskell // {
  #     packages = super.haskell.packages // {
  #       ghc884 = super.haskell.packages.ghc884.extend (selfGHC: superGHC: {
  #           haskell-src-exts = self.haskell.packages.ghc884.haskell-src-exts_1_23_0;
  #         }
  #       );
  #     };
  #   };
  # };
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      haskell.packages.ghc884.hlint
      haskell.packages.ghc884.stylish-haskell
      haskell.packages.ghc884.haskell-language-server
    ];
  }
