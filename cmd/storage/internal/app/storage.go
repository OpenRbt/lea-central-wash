package app

import "fmt"

// Save accepts key-value pair and writes it to DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Save(hash string, key string, value []byte) error {
	stationID, err := a.GetIdByHash(hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)
		return ErrNotFound
	}

	return a.repo.Save(stationID, key, value)
}

// Load accepts key and returns value from DAL
// Checks the pairment of hash and ID of specified wash machine
func (a *app) Load(hash string, key string) ([]byte, error) {
	stationID, err := a.GetIdByHash(hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)
		return ErrNotFound
	}

	return a.repo.Load(stationID, key)
}

func (a *app) Info() string {
	return a.repo.Info()
}

// SetServiceMoneyById changes service money in map to the specified value
// Returns ErrNotFound, if id is not valid, else nil
func (a *app) SetServiceMoneyById(id string, money int) error {
	if value, exist := a.washMap[id]; exist {
		value.ServiceMoney = money
		a.washMap[id] = value
	} else {
		return ErrNotFound
	}
	return nil
}

// GetServiceMoneyByHash finds data and it's ID by hash and gets service money
// Service money will be set to 0 in map
// Returns ErrNotFound, if hash is not valid, else nil
// Returns nil error and 0 value, if hash is valid, but money == 0
func (a *app) GetServiceMoneyByHash(hash string) (int, error) {
	for key, value := range app.washMap {
		if value.Hash == hash {
			result := value.ServiceMoney
			value.ServiceMoney = 0
			a.washMap[key] = value
			return result, nil
		}
	}
	return 0, ErrNotFound
}

// GetServiceMoneyByHash finds data by ID and gets service money
// Service money will be set to 0 in map
// Returns ErrNotFound, if ID is not valid, else nil
// Returns nil error and 0 value, if ID is valid, but money == 0
func (a *app) GetServiceMoneyById(id string) (int, error) {
	result := 0

	if value, exist := a.washMap[id]; exist {
		result = value.ServiceMoney
		value.ServiceMoney = 0
		a.washMap[id] = value
	} else {
		return 0, ErrNotFound
	}
	return result, nil
}

// GetIdByHash finds ID by hash
// Returns ErrNotFound, if hash is not valid, else nil
func (a *app) GetIdByHash(hash string) (string, error) {
	for key, value := range a.washMap {
		if value.Hash == hash {
			return key, nil
		}
	}
	return "", ErrNotFound
}

// PairIdAndHash adds a hash to the specified ID in map
// Returns ErrNotFound, if ID is not valid, else nil
func (a *app) PairIdAndHash(id string, hash string) error {
	if value, exist := a.washMap[id]; exist {
		value.Hash = hash
		return nil
	} else {
		return ErrNotFound
	}
}

// SaveMoneyReport gets app.MoneyReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveMoneyReport(report MoneyReport) error {
	stationID, err := a.GetIdByHash(report.Hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)
		return ErrNotFound
	}
	// TODO: convert to DAL money model here and save
	return nil
}

// SaveRelayReport gets app.RelayReport struct
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) SaveRelayReport(report RelayReport) error {
	stationID, err := a.GetIdByHash(report.Hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)
		return ErrNotFound
	}
	// TODO: convert to DAL report model here and save
	return nil
}

// LoadMoneyReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadMoneyReport(hash string) (MoneyReport, error) {
	stationID, err := a.GetIdByHash(hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}

	// TODO: call DAL method to load money
	return nil, nil
}

// LoadRelayReport gets hash string
// Checks pairment of hash in report and ID in the map
// Returns ErrNotFound in case of hash or ID failure
func (a *app) LoadRelayReport(hash string) (RelayReport, error) {
	stationID, err := a.GetIdByHash(hash)
	if err != nil {
		fmt.Printf("Hash %s is not paired with the ID\n", hash)

		// TODO: change nil to empty report here
		return nil, ErrNotFound
	}

	// TODO: call DAL method to load relays
	return nil, nil
}
