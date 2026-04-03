#!/usr/bin/env bash
set -euo pipefail

# Stops all running ECS tasks in the Sabela cluster.

source "$(dirname "$0")/.env"

TASKS=$(aws ecs list-tasks --cluster "$ECS_CLUSTER" \
  --query 'taskArns[]' --output text --region "$AWS_REGION" 2>/dev/null)

if [ -z "$TASKS" ] || [ "$TASKS" = "None" ]; then
  echo "No running tasks."
  exit 0
fi

COUNT=0
for ARN in $TASKS; do
  TASKDEF=$(aws ecs describe-tasks --cluster "$ECS_CLUSTER" --tasks "$ARN" \
    --query 'tasks[0].taskDefinitionArn' --output text --region "$AWS_REGION" | grep -o '[^/]*$')
  echo "Stopping $TASKDEF ($ARN)"
  aws ecs stop-task --cluster "$ECS_CLUSTER" --task "$ARN" \
    --region "$AWS_REGION" > /dev/null
  COUNT=$((COUNT + 1))
done

echo ""
echo "Stopped $COUNT task(s)."
