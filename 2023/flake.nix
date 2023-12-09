{
  description = "A very basic flake";

  outputs = { self, this }: {
    devShells.x86_64-linux.default = let
      pkgs = this.legacyPackages.x86_64-linux;
      myGhc = pkgs.ghc.withPackages (p: [
        p.vector
        p.attoparsec
        p.text
        p.bytestring
        p.containers
        p.QuickCheck
      ]);
    in pkgs.mkShell {
      buildInputs = [
        myGhc
        pkgs.haskellPackages.fourmolu
        pkgs.haskellPackages.haskell-language-server
      ];
    };
  };
}
