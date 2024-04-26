package app

type ManagementRabbitConfig struct {
	ServerID       string
	ServerPassword string
}

type Station struct {
	ID           StationID
	Name         string
	PreflightSec int
	RelayBoard   string
	Force        bool
	Version      int
	Deleted      bool
}

type StationHash struct {
	Hash      string
	StationID StationID
	Version   int
	Force     bool
}
