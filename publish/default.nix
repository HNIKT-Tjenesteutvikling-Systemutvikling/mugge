{
  pkgs,
  src,
  customJava,
  sbt,
  ...
}:
rec {
  sbtDepsCache = pkgs.stdenv.mkDerivation {
    name = "sbt-deps-cache";
    inherit src;

    nativeBuildInputs = [
      sbt
      customJava
      pkgs.cacert
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${customJava}
      export SBT_OPTS="-Xmx4G -Xss10m"
      export COURSIER_CACHE=$out/.coursier
      export SBT_GLOBAL_BASE=$out/.sbt
      export SBT_BOOT_DIRECTORY=$out/.sbt/boot

      # Single invocation: a second `sbt` client racing the first one's
      # server shutdown exits 1 without output on slower CI runners.
      sbt -Dsbt.ci=true "update; compile"
    '';

    installPhase = ''
      echo "Dependencies cached"
    '';

    outputHashMode = "recursive";
    outputHash = "sha256-9Zu4QCFGigtcv7pEczU29E+AN5L5CYQL99QeHV0cVn8=";
    outputHashAlgo = "sha256";
  };

  clientBuild = pkgs.stdenv.mkDerivation {
    name = "mugge-chat-client";
    inherit src;

    nativeBuildInputs = [
      sbt
      customJava
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      export JAVA_HOME=${customJava}
      export SBT_OPTS="-Xmx4G -Xss10m"
      export COURSIER_CACHE=$PWD/.coursier
      export SBT_GLOBAL_BASE=$PWD/.sbt
      export SBT_BOOT_DIRECTORY=$PWD/.sbt/boot

      cp -r ${sbtDepsCache}/.coursier $PWD/ 2>/dev/null || true
      cp -r ${sbtDepsCache}/.sbt $PWD/ 2>/dev/null || true
      chmod -R u+w $PWD/.coursier $PWD/.sbt 2>/dev/null || true

      echo "Building mugge chat client distribution..."
      sbt -Dsbt.ci=true client/stage

      STAGE_DIR="target/out/jvm/scala-3.3.8/client/universal/stage"
      ls -la "$STAGE_DIR/bin" "$STAGE_DIR/lib"
    '';

    installPhase = ''
      mkdir -p $out
      cp -r target/out/jvm/scala-3.3.8/client/universal/stage/* $out/
      rm -f $out/bin/client.bat

      # The native-packager launcher resolves java from PATH/JAVA_HOME; pin it.
      mv $out/bin/client $out/bin/.client-unwrapped
      cat > $out/bin/client << EOF
      #!${pkgs.bash}/bin/bash
      export JAVA_HOME=${customJava}
      export PATH=${customJava}/bin:\$PATH
      export JAVA_OPTS="\''${JAVA_OPTS:--Xmx256m}"
      exec $out/bin/.client-unwrapped "\$@"
      EOF
      chmod +x $out/bin/client $out/bin/.client-unwrapped

      ln -s $out/bin/client $out/bin/mugge-client
    '';
  };

  muggeClient = pkgs.writeShellScriptBin "mugge" ''
    #!${pkgs.bash}/bin/bash

    export PATH=${
      pkgs.lib.makeBinPath [
        pkgs.openssh
        pkgs.openssl
        pkgs.git
        pkgs.libnotify
        pkgs.pipewire
        pkgs.util-linux
      ]
    }:$PATH

    export LD_LIBRARY_PATH=${
      pkgs.lib.makeLibraryPath [
        pkgs.alsa-plugins
      ]
    }''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

    GREEN='\033[0;32m'
    BLUE='\033[0;34m'
    YELLOW='\033[1;33m'
    NC='\033[0m' # No Color

    echo -e "''${BLUE}╔════════════════════════════════════╗''${NC}"
    echo -e "''${BLUE}║      Mugge Chat Client v1.0        ║''${NC}"
    echo -e "''${BLUE}╚════════════════════════════════════╝''${NC}"
    echo

    if [ $# -gt 0 ]; then
      exec ${clientBuild}/bin/mugge-client "$@"
    else
      HOST=''${CHAT_SERVER_HOST:-localhost}
      PORT=''${CHAT_SERVER_PORT:-20222}
      
      echo -e "''${YELLOW}Connecting to: ''${HOST}:''${PORT}''${NC}"
      echo -e "''${GREEN}Tip: Set CHAT_SERVER_HOST and CHAT_SERVER_PORT to change defaults''${NC}"
      echo
      
      exec ${clientBuild}/bin/mugge-client "$HOST" "$PORT"
    fi
  '';

  muggeAzure = pkgs.writeShellScriptBin "mugge-azure" ''
    #!${pkgs.bash}/bin/bash

    AZURE_HOST="mugge-chat-server.norwayeast.azurecontainer.io"
    AZURE_PORT="20222"

    echo "🌐 Connecting to Azure Container Instance..."
    echo "   Server: $AZURE_HOST:$AZURE_PORT"
    echo

    export CHAT_SERVER_HOST="$AZURE_HOST"
    export CHAT_SERVER_PORT="$AZURE_PORT"

    exec ${muggeClient}/bin/mugge
  '';

  # stdio TRAMP bridge: no banner, no output on stdout (shell channel only).
  muggeBridge = pkgs.writeShellScriptBin "mugge-bridge" ''
    #!${pkgs.bash}/bin/bash

    export PATH=${
      pkgs.lib.makeBinPath [
        pkgs.openssh
        pkgs.openssl
        pkgs.git
      ]
    }:$PATH

    export CHAT_SERVER_HOST="''${CHAT_SERVER_HOST:-mugge-chat-server.norwayeast.azurecontainer.io}"
    export CHAT_SERVER_PORT="''${CHAT_SERVER_PORT:-20222}"

    exec ${clientBuild}/bin/mugge-client --assist "$@"
  '';

  muggeBridgeTest = pkgs.writeShellScriptBin "mugge-bridge-test" ''
    #!${pkgs.bash}/bin/bash

    export PATH=${
      pkgs.lib.makeBinPath [
        pkgs.openssh
        pkgs.openssl
        pkgs.git
      ]
    }:$PATH

    export CHAT_SERVER_HOST="''${CHAT_SERVER_HOST:-mugge-chat-server.norwayeast.azurecontainer.io}"
    export CHAT_SERVER_PORT="''${CHAT_SERVER_PORT:-20222}"

    exec ${clientBuild}/bin/mugge-client --assist-test "$@"
  '';
}
