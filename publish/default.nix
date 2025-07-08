{ pkgs
, src
, customJava
, bleep
, ...
}:
rec {
  bleepDepsCache = pkgs.stdenv.mkDerivation {
    name = "bleep-deps-cache";
    inherit src;

    nativeBuildInputs = [
      bleep
      customJava
      pkgs.coursier
      pkgs.scala-cli
      pkgs.cacert
    ];

    buildPhase = ''
      export JAVA_HOME=${customJava}
      export COURSIER_CACHE=$out/.coursier
      export IVY_HOME=$out/.ivy2
      export BLEEP_HOME=$out/.bleep
      export BLEEP_SERVER_START_TIMEOUT=300
      export BLOOP_COMPUTATION_TIMEOUT=300
      export JAVA_OPTS="-Xmx2G -Xms512M"
      export COURSIER_PARALLEL_DOWNLOAD_COUNT=4

      ${bleep}/bin/bleep compile client
    '';

    installPhase = ''
      echo "Dependencies cached"
    '';

    outputHashMode = "recursive";
    outputHash = "sha256-vNaeIRZxGMWnp9i65tN8pzE4Dqg35f7nHtsPlIbjtU0=";
    outputHashAlgo = "sha256";
  };

  clientBuild = pkgs.stdenv.mkDerivation {
    name = "mugge-chat-client";
    inherit src;

    nativeBuildInputs = [
      bleep
      customJava
      pkgs.coursier
      pkgs.scala-cli
    ];

    buildPhase = ''
      export JAVA_HOME=${customJava}

      export COURSIER_CACHE=$PWD/.coursier
      export IVY_HOME=$PWD/.ivy2
      export BLEEP_HOME=$PWD/.bleep

      cp -r ${bleepDepsCache}/.coursier $PWD/ 2>/dev/null || true
      cp -r ${bleepDepsCache}/.ivy2 $PWD/ 2>/dev/null || true
      cp -r ${bleepDepsCache}/.bleep $PWD/ 2>/dev/null || true

      echo "Building mugge chat client distribution..."
      ${bleep}/bin/bleep dist client

      DIST_DIR=".bleep/builds/normal/.bloop/client/dist"

      echo "Distribution contents:"
      ls -la "$DIST_DIR"
      ls -la "$DIST_DIR/bin" || true
      ls -la "$DIST_DIR/lib" || true
    '';

    installPhase = ''
      mkdir -p $out

      DIST_DIR=".bleep/builds/normal/.bloop/client/dist"
      cp -r "$DIST_DIR"/* $out/

      if [ ! -f "$out/bin/client" ]; then
        mkdir -p $out/bin
        cat > $out/bin/mugge-client << 'EOF'
      #!/usr/bin/env bash
      export JAVA_HOME=${customJava}

      CLASSPATH=""
      for jar in $out/lib/*.jar; do
        CLASSPATH="$CLASSPATH:$jar"
      done
      CLASSPATH=''${CLASSPATH#:}  

      exec ${customJava}/bin/java -cp "$CLASSPATH" chat.ChatClient "$@"
      EOF
        chmod +x $out/bin/mugge-client
      else
        substituteInPlace $out/bin/client \
          --replace "#!/bin/sh" "#!${pkgs.bash}/bin/bash" \
          --replace "java" "${customJava}/bin/java" || true
        chmod +x $out/bin/client
        
        ln -s $out/bin/client $out/bin/mugge-client
      fi
    '';
  };

  muggeClient = pkgs.writeShellScriptBin "mugge" ''
    #!${pkgs.bash}/bin/bash

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
      PORT=''${CHAT_SERVER_PORT:-5555}
      
      echo -e "''${YELLOW}Connecting to: ''${HOST}:''${PORT}''${NC}"
      echo -e "''${GREEN}Tip: Set CHAT_SERVER_HOST and CHAT_SERVER_PORT to change defaults''${NC}"
      echo
      
      exec ${clientBuild}/bin/mugge-client "$HOST" "$PORT"
    fi
  '';

  muggeAzure = pkgs.writeShellScriptBin "mugge-azure" ''
    #!${pkgs.bash}/bin/bash

    AZURE_HOST="mugge-chat-server.norwayeast.azurecontainer.io"
    AZURE_PORT="5555"

    echo "🌐 Connecting to Azure Container Instance..."
    echo "   Server: $AZURE_HOST:$AZURE_PORT"
    echo

    export CHAT_SERVER_HOST="$AZURE_HOST"
    export CHAT_SERVER_PORT="$AZURE_PORT"

    exec ${muggeClient}/bin/mugge
  '';
}
