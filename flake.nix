{
  description = "mugge CLI";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    bleepSrc.url = "github:KristianAN/bleep-flake";
    kvalreg-dev-env.url = "github:HNIKT-Tjenesteutvikling-Systemutvikling/kvalreg-dev-env";
    pre-commit-hooks-nix.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem =
        { pkgs, system, ... }:
        let
          bleep = inputs.bleepSrc.defaultPackage.${system};
          customJava = inputs.kvalreg-dev-env.packages.${system}.java;
          pre-commit-lib = inputs.pre-commit-hooks-nix.lib.${system};

          commonInputs = [
            pkgs.maven
            pkgs.libnotify
            pkgs.openssl
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

          publishConfig = import ./publish/default.nix {
            inherit
              pkgs
              system
              customJava
              bleep
              ;
            src = ./.;
          };

        in
        {
          devShells.default = pkgs.mkShell {
            name = "Mugge Chat APP dev";
            buildInputs = commonInputs ++ jvmInputs;
            shellHook =
              jvmHook
              + "\n"
              + ''
                echo "Available commands:"
                echo ""
                echo "  nix build .#client   - Build the Client"
                echo "  nix build .#azure    - Build the Azure client"
                echo "  nix run .#client     - Run the Client"
                echo "  nix run .#azure      - Run the Azure client"
                echo ""
              '';
          };

          packages = {
            client = publishConfig.clientBuild;
            mugge = publishConfig.muggeClient;
            mugge-azure = publishConfig.muggeAzure;
            default = publishConfig.muggeClient;
          };

          apps = {
            client = {
              type = "app";
              program = "${publishConfig.muggeClient}/bin/mugge";
              meta.description = "Mugge chat client";
            };
            azure = {
              type = "app";
              program = "${publishConfig.muggeAzure}/bin/mugge-azure";
              meta.description = "Connect to Azure-hosted Mugge server";
            };
            default = {
              type = "app";
              program = "${publishConfig.muggeAzure}/bin/mugge-azure";
              meta.description = "Mugge chat client Azure (default)";
            };
          };

          formatter = pkgs.nixfmt-rfc-style;

          checks = {
            pre-commit-check = pre-commit-lib.run {
              src = ./.;
              hooks = {
                statix.enable = true;
                deadnix.enable = true;
                nil.enable = true;
                nixpkgs-fmt.enable = true;
                shellcheck.enable = true;
                beautysh.enable = true;
              };
            };
          };
        };
    };
}
