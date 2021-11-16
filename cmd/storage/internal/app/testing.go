package app

import _ "github.com/golang/mock/mockgen/model"

//go:generate mockgen -source=app.go -destination=gen-testing.go -package app -self_package=github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app App,Repo,KasseSvc,WeatherSvc,HardwareAccessLayer
