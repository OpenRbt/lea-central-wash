package app

func (a *app) Save(stationID string, key string, value []byte) error {
	return a.repo.Save(stationID, key, value)
}

func (a *app) Load(stationID string, key string) ([]byte, error) {
	return a.repo.Load(stationID, key)
}
