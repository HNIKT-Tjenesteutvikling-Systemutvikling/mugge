{
  description = "mugge CLI";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    kvalreg-dev-env.url = "github:HNIKT-Tjenesteutvikling-Systemutvikling/kvalreg-dev-env";
    pre-commit-hooks-nix.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      flake.homeManagerModules = rec {
        mugge-chat = import ./nix/hm-module.nix inputs.self;
        default = mugge-chat;
      };

      perSystem =
        { pkgs, system, ... }:
        let
          customJava = inputs.kvalreg-dev-env.packages.${system}.java;
          pre-commit-lib = inputs.pre-commit-hooks-nix.lib.${system};

          sbt = (pkgs.sbt.override { jre = customJava; }).overrideAttrs (oldAttrs: {
            nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
            postFixup = ''
              wrapProgram $out/bin/sbt --prefix PATH : "${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin:${pkgs.gnused}/bin"
            '';
          });

          commonInputs = [
            pkgs.maven
            pkgs.libnotify
            pkgs.openssl
          ];

          jvmInputs = [
            pkgs.jdk
            pkgs.scalafmt
            pkgs.scala-cli
            pkgs.coursier
            sbt
          ];

          jvmHook = ''
            JAVA_HOME="${customJava}"
            export SBT_OPTS="-Xmx4G -Xss10m"
          '';

          publishConfig = import ./publish/default.nix {
            inherit
              pkgs
              system
              customJava
              sbt
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
            mugge-bridge = publishConfig.muggeBridge;
            mugge-bridge-test = publishConfig.muggeBridgeTest;
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

          formatter = pkgs.nixfmt;

          checks = {
            pre-commit-check = pre-commit-lib.run {
              src = ./.;
              hooks = {
                statix.enable = true;
                deadnix.enable = true;
                nil.enable = true;
                nixfmt-rfc-style.enable = true;
                shellcheck.enable = true;
                beautysh.enable = true;
              };
            };
          };
        };
    };
}
