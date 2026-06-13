#!/usr/bin/env bash
set -euo pipefail
# One-time migration for a RUNNING box (new boxes get this via
# ec2-user-data.sh): move docker-socket-proxy onto a control-plane network the
# user containers can't reach, dual-home the hub, then verify the escape path
# is closed (DNS + TCP-by-IP). Crash-safe: every prefix leaves a working box,
# so re-running from the top is safe in any partial state.
#
# This closes the network escape ONLY. Signup stays open until you run
# migrate-allowlist.sh — this script prints a reminder.
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"

IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }

REMOTE=$(cat <<'SCRIPT'
set -euo pipefail
UNIT=/etc/systemd/system/sabela-hub.service
assert() { eval "$1" || { echo "ASSERT FAILED: $2" >&2; exit 1; }; }

# (1) Networks, self-healing (a daily prune can delete an unused sabela-net).
docker network inspect sabela-net    >/dev/null 2>&1 || docker network create sabela-net
docker network inspect sabela-control >/dev/null 2>&1 || docker network create sabela-control

# (2) Dual-homing helper on disk.
cat > /usr/local/bin/sabela-hub-join-net <<'JOIN'
#!/bin/sh
for _ in $(seq 1 300); do
  docker inspect -f '{{.State.Running}}' sabela-hub 2>/dev/null | grep -q true && break
  sleep 1
done
for _ in $(seq 1 30); do
  docker network connect sabela-net sabela-hub 2>/dev/null && exit 0
  docker inspect --format '{{json .NetworkSettings.Networks}}' sabela-hub 2>/dev/null \
    | grep -q sabela-net && exit 0
  sleep 1
done
echo "sabela-hub failed to join sabela-net" >&2
exit 1
JOIN
chmod +x /usr/local/bin/sabela-hub-join-net

# (3) Proxy onto control BEFORE we touch the unit, so an interrupt here still
#     leaves a box whose next restart finds the proxy reachable.
assert "[ \"$(docker inspect -f '{{.State.Running}}' docker-socket-proxy)\" = true ]" \
  "docker-socket-proxy is not running"
docker network connect sabela-control docker-socket-proxy 2>/dev/null || true
assert "docker inspect -f '{{json .NetworkSettings.Networks}}' docker-socket-proxy | grep -q sabela-control" \
  "proxy did not attach to sabela-control"

# (4) Point the hub at the control network; assert the edits actually landed
#     (a hand-edited unit would silently no-op the sed otherwise).
sed -i 's/--network sabela-net /--network sabela-control /' "$UNIT"
grep -q sabela-hub-join-net "$UNIT" || \
  sed -i '/^ExecStop=/i ExecStartPost=/usr/local/bin/sabela-hub-join-net' "$UNIT"
assert "grep -q 'network sabela-control' \"$UNIT\"" "unit not pointed at sabela-control"
assert "! grep -q 'network sabela-net' \"$UNIT\"" "unit still references sabela-net"
assert "grep -q 'ExecStartPost=/usr/local/bin/sabela-hub-join-net' \"$UNIT\"" \
  "ExecStartPost join-net not present"
systemctl daemon-reload

# (5) Restart; wait for health (no fixed sleep).
systemctl restart sabela-hub
for _ in $(seq 1 30); do
  curl -fsS http://localhost:8080/_hub/health >/dev/null 2>&1 && break
  sleep 1
done
assert "systemctl is-active --quiet sabela-hub" "hub unit not active after restart"
assert "curl -fsS http://localhost:8080/_hub/health >/dev/null" "hub health check failed"
assert "docker inspect -f '{{json .NetworkSettings.Networks}}' sabela-hub | grep -q sabela-control" \
  "hub not on sabela-control"
assert "docker inspect -f '{{json .NetworkSettings.Networks}}' sabela-hub | grep -q sabela-net" \
  "hub did not dual-home onto sabela-net"

# (6) Only now remove the proxy from the user network.
docker network disconnect sabela-net docker-socket-proxy 2>/dev/null || true

# (7) Verify the escape is closed from a user container: name unresolvable AND
#     the proxy's control-net IP unreachable on :2375 (the DNS check alone
#     would miss a leaked IP).
IMG=$(sed -n 's/^HUB_DOCKER_IMAGE=//p' /etc/sabela/hub.env)
PROXY_IP=$(docker inspect -f '{{(index .NetworkSettings.Networks "sabela-control").IPAddress}}' docker-socket-proxy)
probe() { docker run --rm --memory 256m --cpus 0.5 --network sabela-net "$IMG" "$@"; }
if probe getent hosts docker-socket-proxy >/dev/null 2>&1; then
  echo "VERIFY FAILED: docker-socket-proxy still resolvable from sabela-net" >&2; exit 1
fi
if [ -n "$PROXY_IP" ] && probe curl -fsS --max-time 3 "http://$PROXY_IP:2375/_ping" >/dev/null 2>&1; then
  echo "VERIFY FAILED: proxy reachable by IP ($PROXY_IP:2375) from sabela-net" >&2; exit 1
fi
echo "OK: docker-socket-proxy unreachable (name + IP) from the user network"

grep -q '^HUB_ALLOWLIST_FILE=' /etc/sabela/hub.env \
  || echo "REMINDER: signup is still OPEN - run migrate-allowlist.sh to enforce B2"
SCRIPT
)

B64=$(printf '%s' "$REMOTE" | base64 | tr -d '\n')
CID=$(aws ssm send-command --instance-ids "$IID" --document-name AWS-RunShellScript \
  --parameters "commands=[\"echo \\\"$B64\\\" | base64 -d | bash\"]" \
  --query Command.CommandId --output text --region "$AWS_REGION")
aws ssm wait command-executed --command-id "$CID" --instance-id "$IID" \
  --region "$AWS_REGION" 2>/dev/null || true
STATUS=$(aws ssm get-command-invocation --command-id "$CID" --instance-id "$IID" \
  --query Status --output text --region "$AWS_REGION")
aws ssm get-command-invocation --command-id "$CID" --instance-id "$IID" \
  --query '{status:Status,out:StandardOutputContent,err:StandardErrorContent}' \
  --output json --region "$AWS_REGION"
[ "$STATUS" = Success ] || { echo "migration did not succeed (status: $STATUS)" >&2; exit 1; }
