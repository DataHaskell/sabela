#!/usr/bin/env bash
set -euo pipefail

# Builds, pushes, and deploys the hub service.
# If the ECS Service exists (from setup-alb.sh), updates it.
# Otherwise, launches a standalone task with a public IP.

source "$(dirname "$0")/.env"

echo "=== Authenticating with ECR ==="
aws ecr get-login-password --region "$AWS_REGION" | \
  docker login --username AWS --password-stdin "$ECR_REGISTRY"

echo "=== Creating ECR repo ==="
aws ecr create-repository --repository-name datahaskell/sabela-hub \
  --region "$AWS_REGION" 2>/dev/null || echo "  (already exists)"

echo "=== Building hub image ==="
docker build --platform linux/amd64 \
  -f sabela-hub/Dockerfile \
  -t datahaskell/sabela-hub \
  sabela-hub/

echo "=== Pushing to ECR ==="
docker tag datahaskell/sabela-hub:latest "$ECR_HUB:latest"
docker push "$ECR_HUB:latest"

echo "=== Registering task definition ==="
aws logs create-log-group --log-group-name /ecs/sabela-hub \
  --region "$AWS_REGION" 2>/dev/null || true
aws ecs register-task-definition \
  --cli-input-json file://infra/hub-task-definition.json \
  --region "$AWS_REGION" > /dev/null
rm -f /tmp/hub-task-def.json

# Check if the ECS Service exists (created by setup-alb.sh)
SERVICE_STATUS=$(aws ecs describe-services --cluster "$ECS_CLUSTER" --services sabela-hub \
  --query 'services[0].status' --output text --region "$AWS_REGION" 2>/dev/null || echo "MISSING")

if [ "$SERVICE_STATUS" = "ACTIVE" ]; then
  echo "=== Updating ECS Service ==="
  aws ecs update-service \
    --cluster "$ECS_CLUSTER" \
    --service sabela-hub \
    --task-definition sabela-hub \
    --force-new-deployment \
    --region "$AWS_REGION" > /dev/null

  echo "  Waiting for deployment..."
  aws ecs wait services-stable \
    --cluster "$ECS_CLUSTER" --services sabela-hub \
    --region "$AWS_REGION" 2>/dev/null || echo "  (timed out, check manually)"

  ALB_DNS="${ALB_DNS:-$(aws elbv2 describe-load-balancers --names sabela-hub \
    --query 'LoadBalancers[0].DNSName' --output text --region "$AWS_REGION" 2>/dev/null || echo "unknown")}"

  echo ""
  echo "  Hub updated via ECS Service."
  echo "  URL: http://$ALB_DNS"
else
  echo "=== No ECS Service found, launching standalone task ==="
  HUB_SG_ID=$(aws ec2 describe-security-groups \
    --filters "Name=group-name,Values=sabela-hub" "Name=vpc-id,Values=$VPC_ID" \
    --query 'SecurityGroups[0].GroupId' --output text --region "$AWS_REGION" 2>/dev/null || echo "")

  if [ -z "$HUB_SG_ID" ] || [ "$HUB_SG_ID" = "None" ]; then
    HUB_SG_ID="$SG_TASKS"
  fi

  TASK_ARN=$(aws ecs run-task \
    --cluster "$ECS_CLUSTER" --task-definition sabela-hub --launch-type FARGATE \
    --network-configuration "awsvpcConfiguration={subnets=[$SUBNET_PUBLIC],securityGroups=[$HUB_SG_ID,$SG_TASKS],assignPublicIp=ENABLED}" \
    --query 'tasks[0].taskArn' --output text --region "$AWS_REGION")

  echo "  Waiting for public IP..."
  sleep 20
  PUBLIC_IP=$(aws ecs describe-tasks --cluster "$ECS_CLUSTER" --tasks "$TASK_ARN" \
    --query 'tasks[0].attachments[0].details[?name==`networkInterfaceId`].value' \
    --output text --region "$AWS_REGION" | \
    xargs -I{} aws ec2 describe-network-interfaces --network-interface-ids {} \
      --query 'NetworkInterfaces[0].Association.PublicIp' --output text \
      --region "$AWS_REGION" 2>/dev/null || echo "pending")

  echo ""
  echo "  Hub launched as standalone task."
  echo "  IP: $PUBLIC_IP"
  echo ""
  echo "  Run infra/setup-alb.sh to set up the ALB + ECS Service for production."
fi

echo ""
echo "  Auth: Google Sign-In"
