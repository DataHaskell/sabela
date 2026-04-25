#!/usr/bin/env bash
set -euo pipefail

# Builds and pushes the Sabela image, registers the task definition.
# Does NOT start any tasks — the hub spawns them on demand.

source "$(dirname "$0")/.env"

echo "=== Authenticating with ECR ==="
aws ecr get-login-password --region "$AWS_REGION" | \
  docker login --username AWS --password-stdin "$ECR_REGISTRY"

# echo "=== Building Sabela image ==="
# docker build --platform linux/amd64 -t datahaskell/sabela .

echo "=== Pushing to ECR ==="
docker tag datahaskell/sabela:latest "$ECR_SABELA:latest"
docker push "$ECR_SABELA:latest"

echo "=== Registering task definition ==="
aws logs create-log-group --log-group-name /ecs/sabela \
  --region "$AWS_REGION" 2>/dev/null || true
aws ecs register-task-definition \
  --cli-input-json file://infra/task-definition.json \
  --region "$AWS_REGION" > /dev/null

REVISION=$(aws ecs describe-task-definition --task-definition sabela \
  --query 'taskDefinition.revision' --output text --region "$AWS_REGION")

echo ""
echo "  Sabela image pushed and task definition registered (revision $REVISION)."
echo "  The hub will use the new revision for new sessions."
