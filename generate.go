package storageapi

//go:generate rm -rf storageapi/model storageapi/restapi storageapi/client
//go:generate swagger generate server --api-package op --model-package model -f ./storageapi/swagger.yml -t ./storageapi --strict-responders --strict-additional-properties --principal github.com/DiaElectronics/lea-central-wash/storageapi.Profile --exclude-main
//go:generate swagger generate client --api-package op --model-package model -f ./storageapi/swagger.yml -t ./storageapi --strict-responders --strict-additional-properties --principal github.com/DiaElectronics/lea-central-wash/storageapi.Profile

// go install google.golang.org/protobuf/cmd/protoc-gen-go@v1.30
// go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@v1.3

//go:generate protoc --go_out=./cmd/hal/internal/api/xgrpc --go_opt=paths=source_relative --go_opt=Mhal.proto=hal/internal/api/xgrpc --go-grpc_out=./cmd/hal/internal/api/xgrpc --go-grpc_opt=paths=source_relative --go-grpc_opt=Mhal.proto=hal/internal/api/xgrpc ./hal.proto
//go:generate protoc --go_out=./cmd/storage/internal/api/xgrpc --go_opt=paths=source_relative --go_opt=Mhal.proto=hal/internal/api/xgrpc --go-grpc_out=./cmd/storage/internal/api/xgrpc --go-grpc_opt=paths=source_relative --go-grpc_opt=Mhal.proto=hal/internal/api/xgrpc ./hal.proto
