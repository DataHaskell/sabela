#!/usr/bin/env bash
set -euo pipefail

# Builds and runs the EFS builder task.
# Populates EFS with Python venv, Lean toolchain, and Mathlib.
# Takes 30-60 minutes.

source "$(dirname "$0")/.env"

echo "=== Creating ECR repo ==="
aws ecr create-repository --repository-name datahaskell/sabela-efs-builder \
  --region "$AWS_REGION" 2>/dev/null || echo "  (already exists)"

echo "=== Authenticating with ECR ==="
aws ecr get-login-password --region "$AWS_REGION" | \
  docker login --username AWS --password-stdin "$ECR_REGISTRY"

echo "=== Building builder image ==="
docker build --platform linux/amd64 \
  -f infra/Dockerfile.efs-builder \
  -t datahaskell/sabela-efs-builder .

echo "=== Pushing to ECR ==="
docker tag datahaskell/sabela-efs-builder:latest "$ECR_BUILDER:latest"
docker push "$ECR_BUILDER:latest"

echo "=== Setting up ECS ==="
aws logs create-log-group --log-group-name /ecs/sabela-builder \
  --region "$AWS_REGION" 2>/dev/null || echo "  (log group already exists)"
aws ecs create-cluster --cluster-name "$ECS_CLUSTER" \
  --region "$AWS_REGION" > /dev/null 2>&1 || echo "  (cluster already exists)"

echo "=== Registering task definition ==="
aws ecs register-task-definition \
  --cli-input-json file://infra/builder-task-definition.json \
  --region "$AWS_REGION" > /dev/null

echo "=== Running builder task ==="
TASK_ARN=$(aws ecs run-task \
  --cluster "$ECS_CLUSTER" \
  --task-definition sabela-efs-builder \
  --launch-type FARGATE \
  --network-configuration "awsvpcConfiguration={subnets=[$SUBNET_PRIVATE_A],securityGroups=[$SG_TASKS],assignPublicIp=DISABLED}" \
  --query 'tasks[0].taskArn' --output text \
  --region "$AWS_REGION")

echo ""
echo "========================================="
echo "  Builder task launched!"
echo "========================================="
echo ""
echo "  Task: $TASK_ARN"
echo ""
echo "Mathlib build takes 30-60 min. Monitor with:"
echo "  aws logs tail /ecs/sabela-builder --follow --region $AWS_REGION"
