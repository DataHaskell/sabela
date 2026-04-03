#!/usr/bin/env bash
set -euo pipefail

# Creates IAM roles for ECS tasks.
# Run once after setup-vpc.sh.

source "$(dirname "$0")/.env"

TRUST_POLICY='{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": { "Service": "ecs-tasks.amazonaws.com" },
      "Action": "sts:AssumeRole"
    }
  ]
}'

echo "=== Creating ecsTaskExecutionRole ==="
aws iam create-role --role-name ecsTaskExecutionRole \
  --assume-role-policy-document "$TRUST_POLICY" \
  --region "$AWS_REGION" 2>/dev/null || echo "  (already exists)"
aws iam attach-role-policy --role-name ecsTaskExecutionRole \
  --policy-arn arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy

echo "=== Creating ecsTaskRole (EFS access) ==="
aws iam create-role --role-name ecsTaskRole \
  --assume-role-policy-document "$TRUST_POLICY" \
  --region "$AWS_REGION" 2>/dev/null || echo "  (already exists)"

EFS_POLICY="{
  \"Version\": \"2012-10-17\",
  \"Statement\": [
    {
      \"Effect\": \"Allow\",
      \"Action\": [
        \"elasticfilesystem:ClientMount\",
        \"elasticfilesystem:ClientWrite\",
        \"elasticfilesystem:ClientRootAccess\"
      ],
      \"Resource\": \"arn:aws:elasticfilesystem:${AWS_REGION}:${AWS_ACCOUNT}:file-system/${EFS_ID}\"
    }
  ]
}"
aws iam put-role-policy --role-name ecsTaskRole \
  --policy-name sabela-efs-access --policy-document "$EFS_POLICY"

echo "=== Creating ecsHubRole (ECS + IAM:PassRole) ==="
aws iam create-role --role-name ecsHubRole \
  --assume-role-policy-document "$TRUST_POLICY" \
  --region "$AWS_REGION" 2>/dev/null || echo "  (already exists)"

HUB_POLICY="{
  \"Version\": \"2012-10-17\",
  \"Statement\": [
    {
      \"Effect\": \"Allow\",
      \"Action\": [\"ecs:RunTask\", \"ecs:DescribeTasks\", \"ecs:StopTask\"],
      \"Resource\": \"*\"
    },
    {
      \"Effect\": \"Allow\",
      \"Action\": \"iam:PassRole\",
      \"Resource\": [
        \"arn:aws:iam::${AWS_ACCOUNT}:role/ecsTaskExecutionRole\",
        \"arn:aws:iam::${AWS_ACCOUNT}:role/ecsTaskRole\"
      ]
    }
  ]
}"
aws iam put-role-policy --role-name ecsHubRole \
  --policy-name sabela-hub-ecs --policy-document "$HUB_POLICY"

echo ""
echo "=== IAM roles ready ==="
