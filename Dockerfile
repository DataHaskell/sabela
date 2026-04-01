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
  && cp "$(cabal list-bin sabela)" /opt/bin/sabela

# Pre-install dataframe package in build stage so we can copy the store
RUN cabal install dataframe

# ---------- Runtime ----------
FROM haskell:9.12.2-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
  python3 \
  python3-venv \
  tini \
  && rm -rf /var/lib/apt/lists/*


# Install Python ML packages in a venv (no cache, no .pyc)
ENV VIRTUAL_ENV="/opt/venv"
RUN python3 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:${PATH}"
RUN pip install --no-cache-dir --no-compile numpy pandas scikit-learn matplotlib \
  && find $VIRTUAL_ENV -name '__pycache__' -exec rm -rf {} + 2>/dev/null || true

# Install elan (Lean 4 toolchain manager)
ENV ELAN_HOME="/root/.elan"
ENV PATH="${ELAN_HOME}/bin:${PATH}"
RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
     | sh -s -- -y --default-toolchain leanprover/lean4:v4.29.0

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

ENV CABAL_DIR="/root/.cabal"

ENTRYPOINT ["/usr/bin/tini", "--"]

CMD ["/opt/bin/sabela", "3000", "."]
