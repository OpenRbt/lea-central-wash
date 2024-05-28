FROM golang:1.20 as builder
ARG BINARY=storage

WORKDIR /src
COPY go.mod go.sum  /src/
COPY ./cmd /src/cmd
COPY ./storageapi /src/storageapi

RUN echo "${BINARY}"
RUN go build -o service ./cmd/${BINARY}

FROM ubuntu:22.04

RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y ca-certificates git && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    git config --global --add safe.directory /app/stations/openwashing

COPY --from=builder /src/service /app/service
COPY --from=builder src/cmd/storage/internal/migration app/migration
RUN chmod +x /app/service

CMD ["/app/service","-goose.dir=/app/migration"]
