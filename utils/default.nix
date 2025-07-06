{ pkgs, ... }:
rec {
  publishDocker = pkgs.writeShellScriptBin "publishDocker" ''
    set -e
    echo "🏗️ Building Docker image with Nix..."
    nix build .#docker

    echo "🐳 Loading image into Docker..."
    docker load < result

    VERSION_TAG=''${VERSION:-$(date +%Y%m%d-%H%M%S)}

    echo "🏷️ Tagging image..."
    docker tag mugge-chat-server:latest mugge-chat-server:$VERSION_TAG

    echo "📤 Pushing to GitHub Container Registry..."
    docker login ghcr.io -u $GITHUB_USER -p $GITHUB_TOKEN
    docker tag mugge-chat-server:$VERSION_TAG ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:$VERSION_TAG
    docker push ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:$VERSION_TAG

    docker tag mugge-chat-server:$VERSION_TAG ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:latest
    docker push ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:latest

    if [ -n "$COMMIT_SHA" ]; then
      docker tag mugge-chat-server:$VERSION_TAG ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:$COMMIT_SHA
      docker push ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:$COMMIT_SHA
    fi

    echo "✅ Docker images published successfully!"
  '';

  publishInfra = pkgs.writeShellScriptBin "publishInfra" ''
    set -euo pipefail  # Exit on error, undefined variables, and pipe failures

    echo "📦 Starting publishDocker..."
    ${publishDocker}/bin/publishDocker

    # Check if publishDocker succeeded
    if [ $? -ne 0 ]; then
      echo "❌ publishDocker failed! Exiting..."
      exit 1
    fi

    echo "☁️ Starting Pulumi deployment..."
    pulumi login azblob://besom
    pulumi stack select kvalreg-mugge-chat --cwd $PWD/besom
    pulumi config set hash ''${COMMIT_SHA:-latest} --cwd $PWD/besom
    pulumi up --yes --cwd $PWD/besom
  '';

  azureLogs = pkgs.writeShellScriptBin "azureLogs" ''
    if ! az account show > /dev/null 2>&1; then
      echo "Not logged in to Azure. Attempting login..."
      az login --service-principal --username $ARM_CLIENT_ID --password $ARM_CLIENT_SECRET --tenant $ARM_TENANT_ID
      if [ $? -ne 0 ]; then
        echo "Azure login failed. Exiting."
        exit 1
      fi
    else
      echo "Already logged in to Azure."
    fi
    scala-cli run ./scripts/azure-tool.scala -- logs
  '';

  azureEnv = pkgs.writeShellScriptBin "azureEnv" ''
    if ! az account show > /dev/null 2>&1; then
      echo "Not logged in to Azure. Attempting login..."
      az login --service-principal --username $ARM_CLIENT_ID --password $ARM_CLIENT_SECRET --tenant $ARM_TENANT_ID
      if [ $? -ne 0 ]; then
        echo "Azure login failed. Exiting."
        exit 1
      fi
    else
      echo "Already logged in to Azure."
    fi
    scala-cli run ./scripts/azure-tool.scala -- env
  '';
}
