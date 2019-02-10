FROM markhibberd/haskell-circleci:latest

RUN apt-get update -y && apt-get upgrade -y && apt-get install -y libssl-dev
RUN cabal update
COPY . .
RUN ./mafia build -f static
