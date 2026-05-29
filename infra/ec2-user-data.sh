#!/usr/bin/env bash
# Boot script for the Sabela single box (Phase 1). Rendered by setup-ec2.sh,
# which substitutes the @@PLACEHOLDER@@ values before injecting it as EC2
# user-data. Installs Docker + EFS, mounts shared storage, runs a
# docker-socket-proxy (so the hub never touches the raw socket), pre-pulls the
# images, and starts the hub as a systemd unit.
set -euxo pipefail
exec > >(tee /var/log/sabela-bootstrap.log) 2>&1

REGION="@@AWS_REGION@@"
REGISTRY="@@ECR_REGISTRY@@"
EFS_ID="@@EFS_ID@@"
EFS_AP="@@EFS_ACCESS_POINT@@"
HUB_IMAGE="@@HUB_IMAGE@@"
SABELA_IMAGE="@@SABELA_IMAGE@@"

# 1. Packages (SSM agent is preinstalled on AL2023).
dnf install -y docker amazon-efs-utils
systemctl enable --now docker

# 2. EFS at /mnt/sabela (TLS + access point — same auth as the Fargate tasks).
mkdir -p /mnt/sabela
grep -q " /mnt/sabela efs " /etc/fstab \
  || echo "${EFS_ID} /mnt/sabela efs _netdev,tls,accesspoint=${EFS_AP} 0 0" >> /etc/fstab
mount -a -t efs || mount -a
mkdir -p /mnt/sabela/users /mnt/sabela/shares

# 3. Shared bridge network so the hub reaches user containers by name.
docker network inspect sabela-net >/dev/null 2>&1 || docker network create sabela-net

# 4. ECR login + pre-pull (warm cache => ~1-3s container starts).
aws ecr get-login-password --region "$REGION" \
  | docker login --username AWS --password-stdin "$REGISTRY"
docker pull "$HUB_IMAGE"
docker pull "$SABELA_IMAGE"

# 5. docker-socket-proxy: least-privilege Docker API for the hub (defense in
#    depth for the docker.sock = host root concern). The hub talks to it over
#    sabela-net via DOCKER_HOST; it never mounts the raw socket itself.
docker rm -f docker-socket-proxy 2>/dev/null || true
docker run -d --name docker-socket-proxy --restart always \
  --network sabela-net \
  -v /var/run/docker.sock:/var/run/docker.sock:ro \
  -e CONTAINERS=1 -e IMAGES=1 -e NETWORKS=1 -e INFO=1 -e POST=1 -e EXEC=0 \
  -e VOLUMES=0 -e BUILD=0 -e SWARM=0 -e SYSTEM=0 \
  tecnativa/docker-socket-proxy:latest

# 6. Hub env consumed by the systemd unit.
mkdir -p /etc/sabela
cat > /etc/sabela/hub.env <<EOF
HUB_PORT=8080
HUB_BACKEND_PORT=3000
HUB_IDLE_TIMEOUT_MIN=30
HUB_BACKEND=docker
DOCKER_HOST=tcp://docker-socket-proxy:2375
HUB_DOCKER_IMAGE=${SABELA_IMAGE}
HUB_DOCKER_NETWORK=sabela-net
HUB_DOCKER_DATA_ROOT=/mnt/sabela
HUB_DOCKER_MEMORY=@@HUB_DOCKER_MEMORY@@
HUB_DOCKER_CPUS=@@HUB_DOCKER_CPUS@@
HUB_GHCI_CAPS=@@HUB_GHCI_CAPS@@
HUB_GHCI_MAXHEAP=@@HUB_GHCI_MAXHEAP@@
GOOGLE_CLIENT_ID=@@GOOGLE_CLIENT_ID@@
GOOGLE_CLIENT_SECRET=@@GOOGLE_CLIENT_SECRET@@
GOOGLE_REDIRECT_URI=@@GOOGLE_REDIRECT_URI@@
EOF
chmod 600 /etc/sabela/hub.env

# 7. Hub systemd unit. Logs to the existing CloudWatch group so list-users.sh
#    keeps working. Note: no docker.sock mount here — DOCKER_HOST points at the
#    socket-proxy. /mnt/sabela is mounted so the hub can manage shares.
cat > /etc/systemd/system/sabela-hub.service <<UNIT
[Unit]
Description=Sabela hub (front door + per-user container spawner)
After=docker.service network-online.target
Requires=docker.service
Wants=network-online.target

[Service]
Restart=always
RestartSec=5
TimeoutStartSec=0
# NB: do NOT use EnvironmentFile here - hub.env sets DOCKER_HOST (for the hub
# container's docker calls via the socket-proxy), and injecting it into the
# host docker run/rm commands makes them target the proxy by name, which the
# host cannot resolve. The container still receives every var via --env-file.
ExecStartPre=-/usr/bin/docker rm -f sabela-hub
ExecStart=/usr/bin/docker run --rm --name sabela-hub \\
  --network sabela-net \\
  -p 8080:8080 \\
  -v /mnt/sabela:/mnt/sabela \\
  --env-file /etc/sabela/hub.env \\
  --log-driver=awslogs \\
  --log-opt awslogs-group=/ecs/sabela-hub \\
  --log-opt awslogs-region=${REGION} \\
  --log-opt awslogs-create-group=true \\
  --log-opt awslogs-stream=hub \\
  ${HUB_IMAGE}
ExecStop=/usr/bin/docker stop sabela-hub

[Install]
WantedBy=multi-user.target
UNIT

systemctl daemon-reload
systemctl enable --now sabela-hub.service

# 8. Daily prune so cabal-build writable layers from reaped containers don't
#    fill the root volume.
cat > /etc/systemd/system/sabela-prune.service <<'PUNIT'
[Unit]
Description=Prune dangling Docker layers and stopped containers
[Service]
Type=oneshot
ExecStart=/usr/bin/docker system prune -f --filter "until=24h"
PUNIT
cat > /etc/systemd/system/sabela-prune.timer <<'PTIMER'
[Unit]
Description=Daily Docker prune
[Timer]
OnCalendar=daily
Persistent=true
[Install]
WantedBy=timers.target
PTIMER
systemctl enable --now sabela-prune.timer

echo "sabela-box bootstrap complete"
