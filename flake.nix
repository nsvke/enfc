{
  description = "enfc - enf compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
  in
  {
    packages = forAllSystems (system:
      let
        pkgs = nixpkgsFor.${system};
      in
      {
        default = pkgs.rustPlatform.buildRustPackage {
          pname = "enfc";
          version = "0.1.0";
          
          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

          nativeBuildInputs = [ pkgs.makeWrapper ];

          postInstall = ''
            wrapProgram $out/bin/enfc \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.gcc ]}
          '';
        };
      }
    );

    devShells = forAllSystems (system:
      let
        pkgs = nixpkgsFor.${system};
      in
      {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            rustc
            rustfmt
            clippy
            gcc
          ];
        };
      }
    );
  };
}
