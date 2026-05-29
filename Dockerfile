FROM haskell:9.12.2-bookworm AS build
ENV CABAL_DIR="/root/.cabal"
# libtorch-ffi's Custom Setup downloads libtorch here (a stable path instead of
# the XDG cache) and bakes an absolute RPATH to it, so the runtime image can
# place it at the same path and the prebuilt hasktorch artifacts just load.
ENV LIBTORCH_HOME="/opt/libtorch"
RUN mkdir /opt/build
WORKDIR /opt/build

RUN cabal update

# Pre-build the heavy libraries notebooks pull in via `-- cabal:` into the
# shared cabal store (copied to the runtime image below) so those cells start
# fast. A throwaway package drags them in via --only-dependencies: no global
# package environment to fight scripths' per-notebook --package-env, and no bare
# `cabal install` (which errors on library-only packages like hasktorch).
# Building hasktorch runs libtorch-ffi's Custom Setup, which downloads libtorch
# 2.9.1 (CPU) into $LIBTORCH_HOME and RPATHs the artifacts to it. Kept above
# COPY ./sabela.cabal so source/cabal edits never re-trigger this slow step.
# dataframe-hasktorch ==0.2.0.0 pins dataframe ==2.1.0.0 (the notebook recipe).
RUN mkdir -p /opt/warm \
  && printf 'cabal-version: 3.0\nname: warm\nversion: 0\nlibrary\n  default-language: Haskell2010\n  build-depends: dataframe ==2.1.0.0, dataframe-hasktorch ==0.2.0.0, hasktorch, granite\n' > /opt/warm/warm.cabal \
  && cd /opt/warm \
  && cabal build --only-dependencies

COPY ./sabela.cabal /opt/build/

RUN cabal build --only-dependencies

COPY . /opt/build

RUN mkdir -p /opt/bin \
  && cabal build exe:sabela \
  && cp "$(cabal list-bin sabela)" /opt/bin/sabela \
  && strip /opt/bin/sabela

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

# libtorch (fetched by libtorch-ffi's Setup) must sit at the same path the
# prebuilt hasktorch artifacts RPATH to, so they load without LD_LIBRARY_PATH.
COPY --from=build /opt/libtorch /opt/libtorch

# Copy static assets
COPY --from=build /opt/build/static/ /opt/sabela/static/
COPY --from=build /opt/build/display/ /opt/sabela/display/

COPY ./examples /opt/sabela/examples/

ENV CABAL_DIR="/root/.cabal"
ENV LIBTORCH_HOME="/opt/libtorch"

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
