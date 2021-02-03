package storageapi

//go:generate rm -rf storageapi/model storageapi/restapi storageapi/client
//go:generate swagger generate server --api-package op --model-package model --exclude-main --strict -f ./storageapi/swagger.yml -t ./storageapi  --principal storageapi.Profile  --existing-models=github.com/DiaElectronics/lea-central-wash/storageapi
//go:generate swagger generate client --api-package op --model-package model -f ./storageapi/swagger.yml -t ./storageapi
