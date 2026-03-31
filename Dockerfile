FROM haskell:9.8.4-slim-bullseye AS build
RUN mkdir /opt/build
WORKDIR /opt/build

COPY ./sabela.cabal /opt/build/

RUN cabal update

RUN cabal build --only-dependencies

COPY . /opt/build

RUN cabal install --installdir=/opt/bin --install-method=copy

# Pre-install dataframe package in build stage so we can copy the store
RUN cabal install dataframe

# ---------- Runtime ----------
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  ca-certificates \
  curl \
  libffi-dev \
  libffi7 \
  libgmp-dev \
  libgmp10 \
  libncurses-dev \
  libncurses5 \
  libtinfo5 \
  pkg-config \
  python3 \
  python3-pip \
  tini \
  && rm -rf /var/lib/apt/lists/*

# Install GHC + cabal via GHCup (minimal footprint, no HLS/Stack)
ENV GHCUP_INSTALL_BASE_PREFIX="/opt"
ENV PATH="/opt/.ghcup/bin:${PATH}"
RUN curl -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.8.4 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
    BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=0 \
    sh \
  && rm -rf /opt/.ghcup/cache /opt/.ghcup/tmp

# Remove build-only packages now that GHCup is installed
RUN apt-get purge -y build-essential curl libgmp-dev libncurses-dev libffi-dev pkg-config \
  && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*

# Install Python ML packages (no cache, no .pyc)
RUN pip3 install --no-cache-dir --no-compile numpy pandas scikit-learn polars \
  && find /usr/lib/python3 -name '__pycache__' -exec rm -rf {} + 2>/dev/null || true

# Install elan (Lean 4 toolchain manager)
ENV ELAN_HOME="/root/.elan"
ENV PATH="${ELAN_HOME}/bin:${PATH}"
RUN apt-get update && apt-get install -y --no-install-recommends curl \
  && curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
     | sh -s -- -y --default-toolchain leanprover/lean4:v4.29.0 \
  && apt-get purge -y curl && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*

# Verify installations
RUN python3 --version && lean --version && lake --version

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

ENTRYPOINT ["/usr/bin/tini", "--"]

CMD ["/opt/bin/sabela", "3000", "."]
