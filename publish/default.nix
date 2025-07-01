{
  pkgs,
  src,
  customJava,
  bleep,
  ...
}:
rec {
  bleepDepsCache = pkgs.stdenv.mkDerivation {
    name = "bleep-deps-cache";
    src = src;

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

      ${bleep}/bin/bleep compile server shared client
    '';

    installPhase = ''
      echo "Dependencies cached"
    '';

    outputHashMode = "recursive";
    outputHash = "sha256-sBzzPEbqDaXtzoL+bcu2Uqcfo13d37lOIYGW6+7vzkw=";
    outputHashAlgo = "sha256";
  };

  clientBuild = pkgs.stdenv.mkDerivation {
    name = "mugge-chat-client";
    src = src;

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

  serverBuild = pkgs.stdenv.mkDerivation {
    name = "mugge-chat-server";
    src = src;

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

      echo "Building mugge chat server distribution..."
      ${bleep}/bin/bleep dist server

      DIST_DIR=".bleep/builds/normal/.bloop/server/dist"

      echo "Distribution contents:"
      ls -la "$DIST_DIR"
      ls -la "$DIST_DIR/bin"
      ls -la "$DIST_DIR/lib"
    '';

    installPhase = ''
      mkdir -p $out

      DIST_DIR=".bleep/builds/normal/.bloop/server/dist"
      cp -r "$DIST_DIR"/* $out/

      substituteInPlace $out/bin/server \
        --replace "#!/bin/sh" "#!${pkgs.bash}/bin/bash" \
        --replace "java" "${customJava}/bin/java" || true

      chmod +x $out/bin/server
    '';
  };

  startupScript = pkgs.writeShellScript "start-server" ''
    set -e

    echo "Starting Mugge Chat Server..."
    export JAVA_HOME=${customJava}
    export PATH=${
      pkgs.lib.makeBinPath [
        customJava
        pkgs.coreutils
        pkgs.bash
      ]
    }:$PATH

    echo "Server configuration:"
    echo "  Listening on port: 5555"
    echo "  Log level: ''${LOG_LEVEL:-INFO}"
    echo "  Java: ${customJava}"

    JVM_OPTS="''${JVM_OPTS:--Xmx512m -Xms256m}"
    echo "  JVM Options: $JVM_OPTS"

    export JAVA_OPTS="$JVM_OPTS"
    exec ${serverBuild}/bin/server
  '';

  appEnv = pkgs.buildEnv {
    name = "mugge-server-env";
    paths = with pkgs; [
      customJava
      coreutils
      bash
      procps
      serverBuild
    ];
  };

  dockerImage = pkgs.dockerTools.buildImage {
    name = "mugge-chat-server";
    tag = "latest";

    copyToRoot = appEnv;

    runAsRoot = ''
      #!${pkgs.runtimeShell}
      ${pkgs.dockerTools.shadowSetup}

      groupadd -r mugge || true
      useradd -r -g mugge -d /home/mugge -s /bin/false mugge || true

      mkdir -p /var/log/mugge /home/mugge
      chown -R mugge:mugge /var/log/mugge /home/mugge

      chmod 1777 /tmp
    '';

    config = {
      Cmd = [ "${startupScript}" ];
      ExposedPorts = {
        "5555/tcp" = { };
      };
      WorkingDir = "/home/mugge";
      User = "mugge";
      Env = [
        "JAVA_HOME=${customJava}"
        "PATH=${
          pkgs.lib.makeBinPath [
            customJava
            pkgs.coreutils
            pkgs.bash
            pkgs.procps
          ]
        }"
        "LOG_LEVEL=INFO"
        "JVM_OPTS=-Xmx512m -Xms256m"
      ];
    };
  };
}
