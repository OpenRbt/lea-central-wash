package storageapi

//go:generate rm -rf storageapi/model storageapi/restapi storageapi/client
//go:generate swagger generate server --api-package op --model-package model -f ./storageapi/swagger.yml -t ./storageapi --strict-responders --strict-additional-properties --principal github.com/DiaElectronics/lea-central-wash/storageapi.Profile --exclude-main
//go:generate swagger generate client --api-package op --model-package model -f ./storageapi/swagger.yml -t ./storageapi --strict-responders --strict-additional-properties --principal github.com/DiaElectronics/lea-central-wash/storageapi.Profile
