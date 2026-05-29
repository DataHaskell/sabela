#!/usr/bin/env bash
set -euo pipefail
# Build + push the hub (and optionally the per-user sabela) image, then have the
# box pull and restart the hub via SSM. No SSH/keys needed.
#   ./infra/deploy-box.sh                # hub only
#   ./infra/deploy-box.sh --with-sabela  # also rebuild the per-user image
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"; : "${ECR_REGISTRY:?}"; : "${ECR_HUB:?}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

aws ecr get-login-password --region "$AWS_REGION" \
  | docker login --username AWS --password-stdin "$ECR_REGISTRY"

echo "=== build + push hub image ==="
docker build --platform linux/amd64 -f "$ROOT/sabela-hub/Dockerfile" \
  -t datahaskell/sabela-hub "$ROOT/sabela-hub"
docker tag datahaskell/sabela-hub:latest "$ECR_HUB:latest"
docker push "$ECR_HUB:latest"

PULLS="\"docker pull $ECR_HUB:latest\""
if [ "${1:-}" = "--with-sabela" ]; then
  : "${ECR_SABELA:?}"
  echo "=== build + push sabela image ==="
  docker build --platform linux/amd64 -t datahaskell/sabela "$ROOT"
  docker tag datahaskell/sabela:latest "$ECR_SABELA:latest"
  docker push "$ECR_SABELA:latest"
  PULLS="$PULLS,\"docker pull $ECR_SABELA:latest\""
fi

IID=$(aws autoscaling describe-auto-scaling-groups \
  --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' \
  --output text --region "$AWS_REGION")
if [ -z "$IID" ] || [ "$IID" = "None" ]; then
  echo "No running box in ASG 'sabela-box'. Run ./infra/setup-ec2.sh first." >&2
  exit 1
fi

echo "=== SSM pull + restart on $IID ==="
LOGIN="aws ecr get-login-password --region $AWS_REGION | docker login --username AWS --password-stdin $ECR_REGISTRY"
CMDS="[\"$LOGIN\",$PULLS,\"systemctl restart sabela-hub\",\"sleep 3\",\"systemctl is-active sabela-hub\",\"curl -fsS http://localhost:8080/_hub/health\"]"
CID=$(aws ssm send-command --instance-ids "$IID" --document-name AWS-RunShellScript \
  --comment "sabela box deploy" --parameters "commands=$CMDS" \
  --query 'Command.CommandId' --output text --region "$AWS_REGION")
aws ssm wait command-executed --command-id "$CID" --instance-id "$IID" \
  --region "$AWS_REGION" 2>/dev/null || true
aws ssm get-command-invocation --command-id "$CID" --instance-id "$IID" \
  --query '{Status:Status,Out:StandardOutputContent,Err:StandardErrorContent}' \
  --output json --region "$AWS_REGION"

echo ""
echo "Done. New sessions use the new image; existing user containers keep the"
echo "old one until reaped. A hub restart re-attaches running containers (it"
echo "does not kill them)."
