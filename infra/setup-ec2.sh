#!/usr/bin/env bash
set -euo pipefail
# Single-box (Phase 1): IAM instance role + profile, security group, launch
# template, and an ASG of size 1 that runs the hub + per-user sabela containers.
# Mostly idempotent; re-run to update the launch template, then do an instance
# refresh to roll the box. Update infra/.env with the printed outputs.
source "$(dirname "$0")/.env"

# Google OAuth creds live in the hub task definition, not .env; reuse them
# (overridable via the environment).
TASKDEF="$(dirname "$0")/hub-task-definition.json"
GOOGLE_CLIENT_ID="${GOOGLE_CLIENT_ID:-$(sed -n 's/.*"GOOGLE_CLIENT_ID", *"value": *"\([^"]*\)".*/\1/p' "$TASKDEF" 2>/dev/null)}"
GOOGLE_CLIENT_SECRET="${GOOGLE_CLIENT_SECRET:-$(sed -n 's/.*"GOOGLE_CLIENT_SECRET", *"value": *"\([^"]*\)".*/\1/p' "$TASKDEF" 2>/dev/null)}"
GOOGLE_REDIRECT_URI="${GOOGLE_REDIRECT_URI:-$(sed -n 's/.*"GOOGLE_REDIRECT_URI", *"value": *"\([^"]*\)".*/\1/p' "$TASKDEF" 2>/dev/null)}"

: "${AWS_REGION:?}"; : "${AWS_ACCOUNT:?}"; : "${VPC_ID:?}"
: "${SUBNET_PRIVATE_A:?}"; : "${SG_EFS:?}"; : "${EFS_ID:?}"
: "${EFS_ACCESS_POINT:?}"; : "${ECR_REGISTRY:?}"; : "${ECR_HUB:?}"
: "${ECR_SABELA:?}"; : "${ALB_SG:?}"; : "${GOOGLE_CLIENT_ID:?}"
: "${GOOGLE_CLIENT_SECRET:?}"

INSTANCE_TYPE="${EC2_INSTANCE_TYPE:-r7i.4xlarge}"
ROOT_GB="${EC2_ROOT_GB:-300}"
ROLE=sabelaEc2Role
PROFILE=sabelaEc2Profile
LT_NAME=sabela-box
ASG_NAME=sabela-box
HERE="$(cd "$(dirname "$0")" && pwd)"

echo "=== IAM role + instance profile ==="
aws iam create-role --role-name "$ROLE" \
  --assume-role-policy-document '{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"ec2.amazonaws.com"},"Action":"sts:AssumeRole"}]}' \
  --region "$AWS_REGION" >/dev/null 2>&1 || echo "  (role exists)"
for P in AmazonEC2ContainerRegistryReadOnly AmazonSSMManagedInstanceCore CloudWatchAgentServerPolicy; do
  aws iam attach-role-policy --role-name "$ROLE" \
    --policy-arn "arn:aws:iam::aws:policy/$P" --region "$AWS_REGION" 2>/dev/null || true
done
aws iam put-role-policy --role-name "$ROLE" --policy-name sabela-efs-access \
  --policy-document "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"elasticfilesystem:ClientMount\",\"elasticfilesystem:ClientWrite\",\"elasticfilesystem:ClientRootAccess\"],\"Resource\":\"arn:aws:elasticfilesystem:${AWS_REGION}:${AWS_ACCOUNT}:file-system/${EFS_ID}\"}]}" \
  --region "$AWS_REGION" >/dev/null
aws iam create-instance-profile --instance-profile-name "$PROFILE" \
  --region "$AWS_REGION" >/dev/null 2>&1 || echo "  (profile exists)"
aws iam add-role-to-instance-profile --instance-profile-name "$PROFILE" \
  --role-name "$ROLE" 2>/dev/null || true

echo "=== security group (sabela-ec2) ==="
SG_EC2=$(aws ec2 create-security-group --group-name sabela-ec2 \
  --description "Sabela single-box host" --vpc-id "$VPC_ID" \
  --query GroupId --output text --region "$AWS_REGION" 2>/dev/null || \
  aws ec2 describe-security-groups \
    --filters "Name=group-name,Values=sabela-ec2" "Name=vpc-id,Values=$VPC_ID" \
    --query 'SecurityGroups[0].GroupId' --output text --region "$AWS_REGION")
aws ec2 authorize-security-group-ingress --group-id "$SG_EC2" --protocol tcp \
  --port 8080 --source-group "$ALB_SG" --region "$AWS_REGION" 2>/dev/null || true
aws ec2 authorize-security-group-ingress --group-id "$SG_EFS" --protocol tcp \
  --port 2049 --source-group "$SG_EC2" --region "$AWS_REGION" 2>/dev/null || true
echo "  SG_EC2=$SG_EC2"

echo "=== EFS -> Elastic throughput (avoid burst-credit collapse) ==="
aws efs update-file-system --file-system-id "$EFS_ID" --throughput-mode elastic \
  --region "$AWS_REGION" >/dev/null 2>&1 \
  || echo "  (unchanged; already elastic or rate-limited - can change once/24h)"

echo "=== render user-data ==="
AMI_ID=$(aws ssm get-parameters \
  --names /aws/service/ami-amazon-linux-latest/al2023-ami-kernel-default-x86_64 \
  --query 'Parameters[0].Value' --output text --region "$AWS_REGION")
UD="$(mktemp)"
sed -e "s|@@AWS_REGION@@|${AWS_REGION}|g" \
    -e "s|@@ECR_REGISTRY@@|${ECR_REGISTRY}|g" \
    -e "s|@@EFS_ID@@|${EFS_ID}|g" \
    -e "s|@@EFS_ACCESS_POINT@@|${EFS_ACCESS_POINT}|g" \
    -e "s|@@HUB_IMAGE@@|${ECR_HUB}:latest|g" \
    -e "s|@@SABELA_IMAGE@@|${ECR_SABELA}:latest|g" \
    -e "s|@@HUB_DOCKER_MEMORY@@|${HUB_DOCKER_MEMORY:-6g}|g" \
    -e "s|@@HUB_DOCKER_CPUS@@|${HUB_DOCKER_CPUS:-2}|g" \
    -e "s|@@HUB_GHCI_CAPS@@|${HUB_GHCI_CAPS:-2}|g" \
    -e "s|@@HUB_GHCI_MAXHEAP@@|${HUB_GHCI_MAXHEAP:-4G}|g" \
    -e "s|@@GOOGLE_CLIENT_ID@@|${GOOGLE_CLIENT_ID}|g" \
    -e "s|@@GOOGLE_CLIENT_SECRET@@|${GOOGLE_CLIENT_SECRET}|g" \
    -e "s|@@GOOGLE_REDIRECT_URI@@|${GOOGLE_REDIRECT_URI:-https://sabela.datahaskell.com/_hub/oauth/callback}|g" \
    "$HERE/ec2-user-data.sh" > "$UD"

echo "=== launch template ($LT_NAME) ==="
LTDATA="$(mktemp)"
cat > "$LTDATA" <<JSON
{
  "ImageId": "${AMI_ID}",
  "InstanceType": "${INSTANCE_TYPE}",
  "IamInstanceProfile": { "Name": "${PROFILE}" },
  "SecurityGroupIds": ["${SG_EC2}"],
  "MetadataOptions": { "HttpTokens": "required", "HttpPutResponseHopLimit": 1, "HttpEndpoint": "enabled" },
  "BlockDeviceMappings": [{ "DeviceName": "/dev/xvda", "Ebs": { "VolumeSize": ${ROOT_GB}, "VolumeType": "gp3", "Iops": 6000, "Throughput": 250, "Encrypted": true, "DeleteOnTermination": true } }],
  "UserData": "$(base64 < "$UD" | tr -d '\n')",
  "TagSpecifications": [{ "ResourceType": "instance", "Tags": [{ "Key": "Name", "Value": "sabela-box" }] }]
}
JSON
aws ec2 create-launch-template --launch-template-name "$LT_NAME" \
  --launch-template-data "file://$LTDATA" --region "$AWS_REGION" >/dev/null 2>&1 \
  && echo "  created launch template" \
  || { aws ec2 create-launch-template-version --launch-template-name "$LT_NAME" \
         --launch-template-data "file://$LTDATA" --region "$AWS_REGION" >/dev/null \
       && echo "  added new launch-template version"; }
rm -f "$UD" "$LTDATA"

echo "=== ASG (size 1, auto-replaces a lost box) ==="
aws autoscaling create-auto-scaling-group --auto-scaling-group-name "$ASG_NAME" \
  --launch-template "LaunchTemplateName=${LT_NAME},Version=\$Latest" \
  --min-size 1 --max-size 1 --desired-capacity 1 \
  --vpc-zone-identifier "$SUBNET_PRIVATE_A" \
  --health-check-type EC2 --health-check-grace-period 300 \
  --region "$AWS_REGION" 2>/dev/null \
  && echo "  ASG created" \
  || echo "  (ASG exists; roll the box: aws autoscaling start-instance-refresh --auto-scaling-group-name $ASG_NAME --region $AWS_REGION)"

echo ""
echo "Add to infra/.env:   SG_EC2=\"$SG_EC2\"   INSTANCE_PROFILE=\"$PROFILE\""
echo "Next:  ./infra/setup-alb-ec2.sh   (register the box behind the ALB)"
echo "       ./infra/deploy-box.sh      (build/push images + SSM pull/restart)"
