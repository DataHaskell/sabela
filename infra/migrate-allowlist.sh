#!/usr/bin/env bash
set -euo pipefail
# Enforce the signup allowlist (B2) on a RUNNING box. The live box's hub.env
# was written before HUB_ALLOWLIST_FILE existed, so signup is open; enabling it
# naively would lock out every existing user (empty file = deny-all). So this
# is a seed-FIRST, three-step flow:
#
#   ./infra/migrate-allowlist.sh candidates        # who logs in today (review)
#   ./infra/migrate-allowlist.sh seed <local-file> # push the reviewed list
#   ./infra/migrate-allowlist.sh enable            # add env var + restart
#
# `enable` refuses to proceed against an empty/missing allowlist unless --force.
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"
ALLOWLIST=/mnt/sabela/allowlist

IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }

run() { # run a base64'd script on the box via SSM; propagate its exit status
  local b64 cid status
  b64=$(printf '%s' "$1" | base64 | tr -d '\n')
  cid=$(aws ssm send-command --instance-ids "$IID" --document-name AWS-RunShellScript \
    --parameters "commands=[\"echo \\\"$b64\\\" | base64 -d | bash\"]" \
    --query Command.CommandId --output text --region "$AWS_REGION")
  aws ssm wait command-executed --command-id "$cid" --instance-id "$IID" \
    --region "$AWS_REGION" 2>/dev/null || true
  status=$(aws ssm get-command-invocation --command-id "$cid" --instance-id "$IID" \
    --query Status --output text --region "$AWS_REGION")
  aws ssm get-command-invocation --command-id "$cid" --instance-id "$IID" \
    --query '{status:Status,out:StandardOutputContent,err:StandardErrorContent}' \
    --output json --region "$AWS_REGION"
  [ "$status" = Success ] || { echo "remote step failed (status: $status)" >&2; exit 1; }
}

case "${1:-help}" in
  candidates)
    # Emails seen in the hub log (bounded by retention) plus the on-disk user
    # count as a cross-check — the two should roughly agree.
    "$(dirname "$0")/list-users.sh" || true
    echo "--- on-disk user dirs (sanitized; count cross-check) ---"
    run 'ls -1 /mnt/sabela/users 2>/dev/null | wc -l | sed "s/^/user dirs: /"'
    echo "Build a reviewed email list (one per line, or @domain), then: seed <file>"
    ;;
  seed)
    FILE="${2:?usage: seed <local-file>}"
    [ -f "$FILE" ] || { echo "no such file: $FILE" >&2; exit 1; }
    B64FILE=$(base64 < "$FILE" | tr -d '\n')
    run "umask 077; echo \"$B64FILE\" | base64 -d > $ALLOWLIST; \
         echo \"seeded \$(grep -cve '^[[:space:]]*\$' $ALLOWLIST) entries to $ALLOWLIST\""
    ;;
  enable)
    FORCE=${2:-}
    GUARD='n=$(grep -cve "^[[:space:]]*\(#.*\)\?$" '"$ALLOWLIST"' 2>/dev/null || echo 0)'
    if [ "$FORCE" = "--force" ]; then GUARD="$GUARD; true"; else
      GUARD="$GUARD; [ \"\$n\" -gt 0 ] || { echo \"refusing: $ALLOWLIST is empty (seed first, or --force)\" >&2; exit 1; }"
    fi
    run "set -euo pipefail
$GUARD
touch $ALLOWLIST
grep -q '^HUB_ALLOWLIST_FILE=' /etc/sabela/hub.env \
  || sed -i '/^HUB_DOCKER_DATA_ROOT=/a HUB_ALLOWLIST_FILE=$ALLOWLIST' /etc/sabela/hub.env
grep -q '^HUB_ALLOWLIST_FILE=' /etc/sabela/hub.env || \
  echo 'HUB_ALLOWLIST_FILE=$ALLOWLIST' >> /etc/sabela/hub.env
systemctl restart sabela-hub
for _ in \$(seq 1 30); do curl -fsS http://localhost:8080/_hub/health >/dev/null 2>&1 && break; sleep 1; done
systemctl is-active --quiet sabela-hub || { echo 'hub not active after restart' >&2; exit 1; }
echo \"allowlist enforced (\$n entries); do a canary login now\""
    ;;
  *)
    echo "usage: $0 {candidates | seed <local-file> | enable [--force]}" >&2
    exit 1
    ;;
esac
