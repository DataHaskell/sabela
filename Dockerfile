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
  tini \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/sabela

COPY --from=build /opt/bin/sabela /opt/bin/sabela

COPY --from=build /opt/build/static/ /opt/sabela/static/
COPY --from=build /opt/build/display/ /opt/sabela/display/

COPY ./examples /opt/sabela/examples/

RUN cabal update

RUN cabal install dataframe

ENTRYPOINT ["/usr/bin/tini", "--"]

CMD ["timeout", "900", "/opt/bin/sabela", "3000", "examples", \
     "/root/.sabela/global.md", "granite", "text"]