FROM haskell:9.8.4-slim-bullseye AS build
RUN mkdir /opt/build
WORKDIR /opt/build

RUN cabal update

COPY ./sabela.cabal /opt/build/

RUN cabal build --only-dependencies

COPY . /opt/build

RUN cabal install --installdir=/opt/bin --install-method=copy

FROM haskell:9.8.4-slim-bullseye

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp10 \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/sabela

COPY --from=build /opt/bin/sabela /opt/bin/sabela

COPY --from=build /opt/build/static/ /opt/sabela/static/

COPY ./examples /opt/sabela/examples/

RUN cabal update
RUN cabal install --lib dataframe text granite

CMD ["/opt/bin/sabela", "3000", "static", "examples"]
