{
  description = "mugge CLI";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    bleepSrc.url = "github:KristianAN/bleep-flake";
    kvalreg-dev-env.url = "github:HNIKT-Tjenesteutvikling-Systemutvikling/kvalreg-dev-env";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "x86_64-apple-darwin"
        "aarch64-apple-darwin"
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          bleep = inputs.bleepSrc.defaultPackage.${"x86_64-linux"};
          customJava = inputs.kvalreg-dev-env.packages.${system}.java;
          commonInputs = [
            pkgs.maven
            pkgs.libnotify
          ];

          jvmInputs = with pkgs; [
            jdk
            scalafmt
            scala-cli
            coursier
            bleep
          ];

          jvmHook = ''
            JAVA_HOME="${customJava}"
          '';
        in
        {
          devShells.default = pkgs.mkShell {
            name = "Mugge Chat APP dev";
            buildInputs = commonInputs ++ jvmInputs;
            shellHook = jvmHook;
          };
        };
    };
}
