FROM debian:stretch-slim

COPY dist/build/smith/smith /usr/local/bin/smith
COPY dist/build/smith-host/smith-host /usr/local/bin/smith-host
