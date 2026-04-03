#!/usr/bin/env bash
set -euo pipefail

# Builds and pushes both Sabela and hub images, registers task definitions.
# For deploying just one, use infra/deploy-sabela.sh or infra/deploy-hub.sh.

source "$(dirname "$0")/../infra/.env"

aws ecr get-login-password --region "$AWS_REGION" | \
  docker login --username AWS --password-stdin "$ECR_REGISTRY"

echo "=== Building and pushing Sabela ==="
docker build --platform linux/amd64 -t datahaskell/sabela .
docker tag datahaskell/sabela:latest "$ECR_SABELA:latest"
docker push "$ECR_SABELA:latest"

echo "=== Building and pushing Hub ==="
docker build --platform linux/amd64 -f sabela-hub/Dockerfile -t datahaskell/sabela-hub sabela-hub/
docker tag datahaskell/sabela-hub:latest "$ECR_HUB:latest"
docker push "$ECR_HUB:latest"

echo "=== Registering task definitions ==="
aws ecs register-task-definition \
  --cli-input-json file://infra/task-definition.json \
  --region "$AWS_REGION" > /dev/null

echo ""
echo "Images pushed and task definitions registered."
