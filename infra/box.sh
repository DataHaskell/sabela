#!/usr/bin/env bash
set -euo pipefail
# Operate the single box over SSM (finds the instance via the ASG).
#   ./infra/box.sh status    instance + containers + disk + hub health
#   ./infra/box.sh stop       remove per-user / public sandbox containers (keeps the hub)
#   ./infra/box.sh restart    restart the hub container
#   ./infra/box.sh logs [N]   last N lines of the hub's CloudWatch logs (default 30)
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"

IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }

run() { # run a JSON command array on the box via SSM, print stdout
  local cid
  cid=$(aws ssm send-command --instance-ids "$IID" --document-name AWS-RunShellScript \
    --parameters "commands=$1" --query Command.CommandId --output text --region "$AWS_REGION")
  aws ssm wait command-executed --command-id "$cid" --instance-id "$IID" \
    --region "$AWS_REGION" 2>/dev/null || true
  aws ssm get-command-invocation --command-id "$cid" --instance-id "$IID" \
    --query 'StandardOutputContent' --output text --region "$AWS_REGION"
}

case "${1:-status}" in
  status)
    aws ec2 describe-instances --instance-ids "$IID" \
      --query 'Reservations[0].Instances[0].{State:State.Name,Type:InstanceType,Ip:PrivateIpAddress}' \
      --output table --region "$AWS_REGION"
    run '["echo === containers ===","docker ps --format {{.Names}}::{{.Status}}","echo === disk (docker) ===","df -h /var/lib/docker 2>/dev/null | tail -1","echo === hub ===","systemctl is-active sabela-hub","curl -fsS http://localhost:8080/_hub/health && echo \" hub-ok\""]'
    ;;
  stop)
    run '["ids=$(docker ps -q --filter name=sabela-user- --filter name=sabela-pub-)","if [ -n \"$ids\" ]; then docker rm -f $ids && echo removed; else echo \"no sandbox containers\"; fi"]'
    ;;
  restart)
    run '["systemctl restart sabela-hub","sleep 4","systemctl is-active sabela-hub"]'
    ;;
  logs)
    aws logs get-log-events --log-group-name /ecs/sabela-hub --log-stream-name hub \
      --limit "${2:-30}" --query 'events[].message' --output text --region "$AWS_REGION"
    ;;
  *) echo "usage: $0 {status|stop|restart|logs [N]}" >&2; exit 1 ;;
esac
