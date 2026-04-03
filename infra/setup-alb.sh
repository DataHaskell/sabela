#!/usr/bin/env bash
set -euo pipefail

# Creates an ALB in front of the hub service, with an ECS Service
# that keeps the hub running and auto-restarts on crash.
#
# After running, update infra/.env with the output values.

source "$(dirname "$0")/.env"

echo "=== Creating ALB security group ==="
ALB_SG=$(aws ec2 create-security-group \
  --group-name sabela-alb \
  --description "Sabela ALB - public HTTP/HTTPS" \
  --vpc-id "$VPC_ID" \
  --query 'GroupId' --output text \
  --region "$AWS_REGION" 2>/dev/null || \
  aws ec2 describe-security-groups \
    --filters "Name=group-name,Values=sabela-alb" "Name=vpc-id,Values=$VPC_ID" \
    --query 'SecurityGroups[0].GroupId' --output text --region "$AWS_REGION")

aws ec2 authorize-security-group-ingress \
  --group-id "$ALB_SG" --protocol tcp --port 80 \
  --cidr 0.0.0.0/0 --region "$AWS_REGION" 2>/dev/null || true
aws ec2 authorize-security-group-ingress \
  --group-id "$ALB_SG" --protocol tcp --port 443 \
  --cidr 0.0.0.0/0 --region "$AWS_REGION" 2>/dev/null || true

# Allow ALB to reach the hub on port 8080
aws ec2 authorize-security-group-ingress \
  --group-id "$SG_HUB" --protocol tcp --port 8080 \
  --source-group "$ALB_SG" --region "$AWS_REGION" 2>/dev/null || true

echo "  ALB SG: $ALB_SG"

echo "=== Creating ALB ==="
ALB_ARN=$(aws elbv2 create-load-balancer \
  --name sabela-hub \
  --type application \
  --scheme internet-facing \
  --subnets "$SUBNET_PUBLIC" "$SUBNET_PRIVATE_B" \
  --security-groups "$ALB_SG" \
  --query 'LoadBalancers[0].LoadBalancerArn' --output text \
  --region "$AWS_REGION")

ALB_DNS=$(aws elbv2 describe-load-balancers \
  --load-balancer-arns "$ALB_ARN" \
  --query 'LoadBalancers[0].DNSName' --output text \
  --region "$AWS_REGION")

echo "  ALB: $ALB_ARN"
echo "  DNS: $ALB_DNS"

# Hub spawns Fargate tasks on first request (~2 min), so increase idle timeout
aws elbv2 modify-load-balancer-attributes \
  --load-balancer-arn "$ALB_ARN" \
  --attributes Key=idle_timeout.timeout_seconds,Value=300 \
  --region "$AWS_REGION" > /dev/null

echo "=== Creating target group ==="
TG_ARN=$(aws elbv2 create-target-group \
  --name sabela-hub \
  --protocol HTTP \
  --port 8080 \
  --vpc-id "$VPC_ID" \
  --target-type ip \
  --health-check-path "/_hub/health" \
  --health-check-interval-seconds 30 \
  --healthy-threshold-count 2 \
  --unhealthy-threshold-count 3 \
  --query 'TargetGroups[0].TargetGroupArn' --output text \
  --region "$AWS_REGION")

echo "  Target group: $TG_ARN"

echo "=== Creating HTTP listener ==="
aws elbv2 create-listener \
  --load-balancer-arn "$ALB_ARN" \
  --protocol HTTP \
  --port 80 \
  --default-actions "Type=forward,TargetGroupArn=$TG_ARN" \
  --region "$AWS_REGION" > /dev/null

echo "  Listener on port 80 -> target group"

echo "=== Creating ECS Service ==="
# Stop any existing hub tasks first (the service will manage them now)
for ARN in $(aws ecs list-tasks --cluster "$ECS_CLUSTER" --family sabela-hub \
  --query 'taskArns[]' --output text --region "$AWS_REGION" 2>/dev/null); do
  aws ecs stop-task --cluster "$ECS_CLUSTER" --task "$ARN" \
    --region "$AWS_REGION" > /dev/null 2>&1 || true
done

aws ecs create-service \
  --cluster "$ECS_CLUSTER" \
  --service-name sabela-hub \
  --task-definition sabela-hub \
  --desired-count 1 \
  --launch-type FARGATE \
  --network-configuration "awsvpcConfiguration={subnets=[$SUBNET_PUBLIC],securityGroups=[$SG_HUB,$SG_TASKS],assignPublicIp=ENABLED}" \
  --load-balancers "targetGroupArn=$TG_ARN,containerName=hub,containerPort=8080" \
  --deployment-configuration "minimumHealthyPercent=0,maximumPercent=200" \
  --region "$AWS_REGION" > /dev/null

echo "  Service created (desired=1)"

echo ""
echo "  Waiting for service to stabilize..."
aws ecs wait services-stable \
  --cluster "$ECS_CLUSTER" \
  --services sabela-hub \
  --region "$AWS_REGION" 2>/dev/null || echo "  (timed out waiting, check manually)"

echo ""
echo "========================================="
echo "  ALB + ECS Service ready!"
echo "========================================="
echo ""
echo "  ALB DNS: $ALB_DNS"
echo "  ALB ARN: $ALB_ARN"
echo "  ALB SG:  $ALB_SG"
echo "  TG ARN:  $TG_ARN"
echo ""
echo "Test:"
echo "  curl http://$ALB_DNS/_hub/health"
echo "  curl -H 'Authorization: Bearer $HUB_API_KEY' http://$ALB_DNS/api/notebook"
echo ""
echo "Add these to infra/.env:"
echo "  ALB_ARN=\"$ALB_ARN\""
echo "  ALB_DNS=\"$ALB_DNS\""
echo "  ALB_SG=\"$ALB_SG\""
echo "  TG_ARN=\"$TG_ARN\""
echo ""
echo "For HTTPS, add an ACM certificate and a port 443 listener:"
echo "  aws acm request-certificate --domain-name your-domain.com --validation-method DNS"
echo "  aws elbv2 create-listener --load-balancer-arn $ALB_ARN \\"
echo "    --protocol HTTPS --port 443 --certificates CertificateArn=<cert-arn> \\"
echo "    --default-actions Type=forward,TargetGroupArn=$TG_ARN"
