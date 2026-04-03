#!/usr/bin/env bash
set -euo pipefail

# Shows the current state of the Sabela deployment.

source "$(dirname "$0")/.env"

echo "=== ECS Tasks ==="
TASKS=$(aws ecs list-tasks --cluster "$ECS_CLUSTER" \
  --query 'taskArns' --output text --region "$AWS_REGION" 2>/dev/null)

if [ -z "$TASKS" ] || [ "$TASKS" = "None" ]; then
  echo "  No running tasks"
else
  for ARN in $TASKS; do
    INFO=$(aws ecs describe-tasks --cluster "$ECS_CLUSTER" --tasks "$ARN" \
      --query 'tasks[0].{def:taskDefinitionArn,status:lastStatus,cpu:cpu,memory:memory}' \
      --output text --region "$AWS_REGION")
    TASKDEF=$(echo "$INFO" | awk '{print $1}' | grep -o '[^/]*$')
    STATUS=$(echo "$INFO" | awk '{print $4}')
    echo "  $TASKDEF  $STATUS  $ARN"
  done
fi

echo ""
echo "=== EFS ==="
echo "  ID: $EFS_ID"
SIZE=$(aws efs describe-file-systems --file-system-id "$EFS_ID" \
  --query 'FileSystems[0].SizeInBytes.Value' --output text \
  --region "$AWS_REGION" 2>/dev/null)
echo "  Size: $((SIZE / 1024 / 1024)) MB"

echo ""
echo "=== ECR Images ==="
for REPO in datahaskell/sabela datahaskell/sabela-hub datahaskell/sabela-efs-builder; do
  PUSHED=$(aws ecr describe-images --repository-name "$REPO" \
    --query 'sort_by(imageDetails, &imagePushedAt)[-1].imagePushedAt' \
    --output text --region "$AWS_REGION" 2>/dev/null || echo "none")
  echo "  $REPO  last pushed: $PUSHED"
done
