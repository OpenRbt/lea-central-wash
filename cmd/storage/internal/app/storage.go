package app

func (a *app) Save(stationID string, key string, value []byte) error {
	return a.repo.Save(stationID, key, value)
}

func (a *app) Load(stationID string, key string) ([]byte, error) {
	return a.repo.Load(stationID, key)
}

func (a *app) Info() string {
	return a.repo.Info()
}

func (a *app) StatusReport() StatusReport {
	panic("not implment")
}

func (a *app) SetStation(station SetStation) error {
	panic("not implment")
}

func (a *app) DelStation(id int) error {
	panic("not implment")
}
