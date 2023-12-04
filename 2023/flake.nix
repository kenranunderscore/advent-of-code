{
  description = "A very basic flake";

  outputs = { self, this }: {
    devShells.x86_64-linux.default =
      let pkgs = this.legacyPackages.x86_64-linux; in
      pkgs.mkShell {
        buildInputs = [ (pkgs.ghc.withPackages (p: [ p.vector ])) ];
      };
  };
}
