FROM haskell:9.12.2-slim-bookworm AS build
ENV CABAL_DIR="/root/.cabal"
RUN mkdir /opt/build
WORKDIR /opt/build

COPY ./sabela.cabal /opt/build/

RUN cabal update

RUN cabal build --only-dependencies

COPY . /opt/build

RUN mkdir -p /opt/bin \
  && cabal build exe:sabela \
  && cp "$(cabal list-bin sabela)" /opt/bin/sabela \
  && strip /opt/bin/sabela

# Pre-install dataframe package in build stage so we can copy the store
RUN cabal install dataframe

# ---------- Runtime ----------
FROM haskell:9.12.2-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
  python3 \
  python3-venv \
  curl \
  && rm -rf /var/lib/apt/lists/*

# ---------- Assemble final image ----------
WORKDIR /opt/sabela

# Copy compiled binary
COPY --from=build /opt/bin/sabela /opt/bin/sabela

# Copy pre-built cabal store and package index from build stage
COPY --from=build /root/.cabal/store /root/.cabal/store
COPY --from=build /root/.cabal/packages /root/.cabal/packages

# Copy static assets
COPY --from=build /opt/build/static/ /opt/sabela/static/
COPY --from=build /opt/build/display/ /opt/sabela/display/

COPY ./examples /opt/sabela/examples/

ENV CABAL_DIR="/root/.cabal"

# Entrypoint script prepends EFS tool paths to PATH at runtime
# (avoids hardcoding PATH in task definition, which broke GHC discovery)
COPY <<'SCRIPT' /opt/bin/start.sh
#!/bin/sh
# Add EFS-mounted tools to PATH if they exist
[ -d "/mnt/sabela/python/venv/bin" ] && export PATH="/mnt/sabela/python/venv/bin:$PATH"

# If a user work directory is specified (3rd arg = sabela's 2nd arg), set it up
WORKDIR="${3:-.}"
if echo "$WORKDIR" | grep -q "^/mnt/sabela/users/"; then
  mkdir -p "$WORKDIR"
  # Copy examples into user dir if not already there
  if [ ! -d "$WORKDIR/examples" ]; then
    cp -r /opt/sabela/examples "$WORKDIR/examples"
  fi
fi

exec "$@"
SCRIPT
RUN chmod +x /opt/bin/start.sh

ENTRYPOINT ["/opt/bin/start.sh"]
CMD ["/opt/bin/sabela", "3000", "."]
