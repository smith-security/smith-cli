FROM markhibberd/haskell-circleci:latest as builder

RUN apt-get update -y && apt-get upgrade -y && apt-get install -y libssl-dev
RUN cabal update
COPY . .
RUN ./mafia build -f static

FROM debian:stretch-slim

COPY --from=builder /work/dist/smith/smith /usr/local/bin/smith
COPY --from=builder /work/dist/smith-host/smith-host /usr/local/bin/smith-host
