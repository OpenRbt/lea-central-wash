FROM golang:1.20 as builder
ARG BINARY=storage

WORKDIR /src
COPY go.mod go.sum  /src/
COPY ./cmd /src/cmd
COPY ./storageapi /src/storageapi

RUN echo "${BINARY}"
RUN go build ./cmd/${BINARY}

FROM ubuntu:22.04

COPY --from=builder /src/$BIN ./main
