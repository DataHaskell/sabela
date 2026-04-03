FROM haskell:9.12.2-slim-bookworm AS build
ENV CABAL_DIR="/root/.cabal"
WORKDIR /opt/build

COPY ./sabela-hub.cabal /opt/build/
RUN cabal update && cabal build --only-dependencies

COPY . /opt/build
RUN mkdir -p /opt/bin \
  && cabal build exe:sabela-hub \
  && cp "$(cabal list-bin sabela-hub)" /opt/bin/sabela-hub \
  && strip /opt/bin/sabela-hub

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
  ca-certificates curl unzip \
  && curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o /tmp/awscli.zip \
  && unzip -q /tmp/awscli.zip -d /tmp \
  && /tmp/aws/install \
  && rm -rf /tmp/aws /tmp/awscli.zip \
  && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/bin/sabela-hub /opt/bin/sabela-hub

CMD ["/opt/bin/sabela-hub"]
