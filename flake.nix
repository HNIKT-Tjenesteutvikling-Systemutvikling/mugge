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
                echo "  nix build .#client                                     - Build the Client"
                echo "  nix run .#client                                       - Run the Client"
                echo "  nix build .#docker                                     - Build the Docker image for server"
                echo "  docker load < result                                   - Load the built image into Docker"
                echo "  docker run -p 5555:5555 mugge-chat-server:latest       - Run container with nginx"
                echo ""
              '';
          };
          packages = {
            docker = publishConfig.dockerImage;
            client = publishConfig.clientBuild;
            default = publishConfig.clientBuild;
          };
          apps = {
            client = {
              type = "app";
              program = "${publishConfig.clientBuild}/bin/mugge-client";
            };
            server = {
              type = "app";
              program = "${publishConfig.serverBuild}/bin/server";
            };
            default = {
              type = "app";
              program = "${publishConfig.clientBuild}/bin/mugge-client";
            };
          };
        };
    };
}
