FROM golang:1.20 as builder
ARG BINARY=storage

WORKDIR /src
COPY go.mod go.sum  /src/
COPY ./cmd /src/cmd
COPY ./storageapi /src/storageapi

RUN echo "${BINARY}"
RUN go build -o service ./cmd/${BINARY}

FROM ubuntu:22.04

COPY --from=builder /src/service /app/service
COPY --from=builder src/cmd/storage/internal/migration app/migration
RUN chmod +x /app/service

CMD ["/app/service","-goose.dir=/app/migration"] 
