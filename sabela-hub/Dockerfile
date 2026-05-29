FROM haskell:9.12.2-slim-bookworm AS build
ENV CABAL_DIR="/root/.cabal"
WORKDIR /opt/build

COPY ./sabela-hub.cabal /opt/build/
# -j2 bounds peak memory: cabal defaults to one job per core, and under amd64
# emulation (cross-building on arm64) 8 parallel GHC processes exhaust the VM
# and kill buildkit. Two at a time builds reliably; native amd64 builds are
# fast enough that the lower parallelism is not a concern.
RUN cabal update && cabal build --only-dependencies -j2

COPY . /opt/build
RUN mkdir -p /opt/bin \
  && cabal build exe:sabela-hub -j2 \
  && cp "$(cabal list-bin sabela-hub)" /opt/bin/sabela-hub \
  && strip /opt/bin/sabela-hub

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
  ca-certificates curl unzip \
  # AWS CLI — kept so HUB_BACKEND=ecs (rollback + the Phase 3b public Fargate
  # substrate) works from the same image.
  && curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o /tmp/awscli.zip \
  && unzip -q /tmp/awscli.zip -d /tmp \
  && /tmp/aws/install \
  && rm -rf /tmp/aws /tmp/awscli.zip \
  # Docker CLI client only — the hub spawns sibling containers through the
  # mounted docker socket (or a socket-proxy); no daemon ships in this image.
  && install -m 0755 -d /etc/apt/keyrings \
  && curl -fsSL https://download.docker.com/linux/debian/gpg \
       -o /etc/apt/keyrings/docker.asc \
  && chmod a+r /etc/apt/keyrings/docker.asc \
  && echo "deb [arch=amd64 signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/debian bookworm stable" \
       > /etc/apt/sources.list.d/docker.list \
  && apt-get update && apt-get install -y --no-install-recommends docker-ce-cli \
  && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/bin/sabela-hub /opt/bin/sabela-hub

CMD ["/opt/bin/sabela-hub"]
