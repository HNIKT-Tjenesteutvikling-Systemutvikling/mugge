{ flake-utils, pkgs, ... }:
let
  bleep = flake-utils.lib.eachDefaultSystem (system: {
    defaultPackage =
      let
        version = "0.0.7"; # Define the version here
        suffix = if pkgs.lib.strings.hasInfix "darwin" system then "-apple-darwin" else "-pc-linux";
      in
      pkgs.stdenv.mkDerivation {
        name = "bleep-${version}";

        nativeBuildInputs = [
          pkgs.installShellFiles
          pkgs.makeWrapper
        ] ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.autoPatchelfHook;

        buildInputs = [
          pkgs.glibc
          pkgs.zlib
          pkgs.stdenv.cc.cc
          pkgs.coreutils
        ];

        src = pkgs.fetchurl {
          url = "https://github.com/oyvindberg/bleep/releases/download/v${version}/bleep-x86_64${suffix}.tar.gz";
          sha256 = "sha256-ttfmTMSK6u2zX87CUYjibr675Xzn4/byWub67e5Ir+I=";
        };

        unpackPhase = ''
          runHook preUnpack
          tar -xf $src
          runHook postUnpack
        '';

        installPhase = ''
          runHook preInstall
          install -Dm755 bleep $out/bin/.bleep-wrapped
          makeWrapper $out/bin/.bleep-wrapped $out/bin/bleep \
            --argv0 "$out/bin/bleep"
          runHook postInstall
        '';

        dontAutoPatchelf = true;

        postFixup =
          pkgs.lib.optionalString pkgs.stdenv.isLinux ''
            autoPatchelf $out
          ''
          + ''
            mkdir temp
            cp $out/bin/.bleep-wrapped temp/bleep
            PATH="./temp:$PATH"

            installShellCompletion --cmd bleep \
              --bash <(bleep install-tab-completions-bash --stdout) \
              --zsh <(bleep install-tab-completions-zsh --stdout) \
          '';

        meta = with pkgs.lib; {
          homepage = "https://bleep.build";
          description = "A bleeping fast build tool for the Scala language";
        };
      };
  });
in
bleep
