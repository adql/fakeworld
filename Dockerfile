FROM haskell:9.2.5
WORKDIR /app

# Prepare dependencies
COPY fakeworld.cabal ./
RUN cabal update && cabal build --only-dependencies

# Build app
COPY app ./app
RUN cabal build

COPY . .
ENTRYPOINT ["make", "run-server"]
