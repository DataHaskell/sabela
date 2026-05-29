#!/usr/bin/env bash
set -euo pipefail
# Cut the ALB's HTTPS listener over from the Fargate hub to the box with zero
# downtime, using weighted target groups: attach the new instance TG at weight
# 0 (so the ALB health-checks it while the old TG still serves 100%), wait for
# the box to pass health checks, then shift HTTPS 100% to it. The :80 listener
# (HTTP->HTTPS redirect) is left untouched; the old ip-TG ($TG_ARN) stays
# attached at weight 0 for instant rollback. Run after setup-ec2.sh.
#
# (A target group's targets are only health-checked once the TG is attached to
# a listener - hence the weight-0 attach instead of waiting before repointing.)
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"; : "${VPC_ID:?}"; : "${ALB_ARN:?}"; : "${TG_ARN:?}"

echo "=== instance target group (sabela-hub-ec2) ==="
NEWTG=$(aws elbv2 create-target-group --name sabela-hub-ec2 \
  --protocol HTTP --port 8080 --vpc-id "$VPC_ID" --target-type instance \
  --health-check-path /_hub/health --health-check-interval-seconds 15 \
  --healthy-threshold-count 2 --unhealthy-threshold-count 3 \
  --query 'TargetGroups[0].TargetGroupArn' --output text --region "$AWS_REGION" 2>/dev/null \
  || aws elbv2 describe-target-groups --names sabela-hub-ec2 \
       --query 'TargetGroups[0].TargetGroupArn' --output text --region "$AWS_REGION")
echo "  new TG: $NEWTG"

echo "=== register the box + attach ASG ==="
IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }
aws elbv2 register-targets --target-group-arn "$NEWTG" \
  --targets "Id=$IID,Port=8080" --region "$AWS_REGION"
aws autoscaling attach-load-balancer-target-groups --auto-scaling-group-name sabela-box \
  --target-group-arns "$NEWTG" --region "$AWS_REGION" 2>/dev/null || true

L443=$(aws elbv2 describe-listeners --load-balancer-arn "$ALB_ARN" \
  --query "Listeners[?Port==\`443\`].ListenerArn" --output text --region "$AWS_REGION")
[ -n "$L443" ] && [ "$L443" != "None" ] || { echo "no HTTPS:443 listener found" >&2; exit 1; }

echo "=== attach new TG at weight 0 (health-checked; old TG still serves) ==="
aws elbv2 modify-listener --listener-arn "$L443" --region "$AWS_REGION" \
  --default-actions "[{\"Type\":\"forward\",\"ForwardConfig\":{\"TargetGroups\":[{\"TargetGroupArn\":\"$TG_ARN\",\"Weight\":1},{\"TargetGroupArn\":\"$NEWTG\",\"Weight\":0}]}}]" >/dev/null

echo "=== wait for the box to pass health checks ==="
aws elbv2 wait target-in-service --target-group-arn "$NEWTG" \
  --targets "Id=$IID,Port=8080" --region "$AWS_REGION"
echo "  box is healthy"

echo "=== shift HTTPS 100% to the box ==="
aws elbv2 modify-listener --listener-arn "$L443" --region "$AWS_REGION" \
  --default-actions "Type=forward,TargetGroupArn=$NEWTG" >/dev/null

echo ""
echo "CUT OVER to the box. Record in infra/.env:  TG_ARN_INSTANCE=\"$NEWTG\""
echo "Verify:   curl https://sabela.datahaskell.com/_hub/health"
echo "ROLLBACK to Fargate (instant):"
echo "  aws elbv2 modify-listener --listener-arn $L443 --default-actions Type=forward,TargetGroupArn=$TG_ARN --region $AWS_REGION"
