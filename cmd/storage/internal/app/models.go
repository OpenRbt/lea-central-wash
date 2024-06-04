package app

import (
	"time"

	uuid "github.com/satori/go.uuid"
)

// UserData describes a user of the system (a registered one)
type UserData struct {
	ID         int
	Login      string
	Password   string
	FirstName  *string
	MiddleName *string
	LastName   *string
	IsAdmin    *bool
	IsEngineer *bool
	IsOperator *bool
}

// UpdateUserData describes which data fields can be updated
type UpdateUserData struct {
	Login      string
	FirstName  *string
	MiddleName *string
	LastName   *string
	IsAdmin    *bool
	IsEngineer *bool
	IsOperator *bool
}

// UpdatePasswordData is a structure used in "change pass" procedure
type UpdatePasswordData struct {
	Login       string
	OldPassword string
	NewPassword string
}

// StationData represents current status of a station
type StationData struct {
	ID                  StationID
	CurrentSessionID    string
	PreviousSessionID   string
	AuthorizedSessionID string
	UserID              string
	Name                string
	ServiceMoney        int
	BonusMoney          int
	LastPing            time.Time
	RunProgram          time.Time
	OpenStation         bool
	CurrentBalance      int
	CurrentProgram      int
	ButtonID            int
	LastUpdate          int
	LastDiscountUpdate  int64
	IP                  string
	IsActive            bool
	KaspiMoney          int64
	Task                *Task
	Versions            []FirmwareVersion
	CurrentVersions     *FirmwareVersion
}

// MoneyReport is just to represent money in a station. All known kinds of money
type MoneyReport struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
	SessionID    string
	QrMoney      int
}

// MngtMoneyReport ...
type MngtMoneyReport struct {
	ID                  int
	StationID           StationID
	Banknotes           int
	CarsTotal           int
	Coins               int
	Electronical        int
	Service             int
	Bonuses             int
	QrMoney             int
	Ctime               time.Time
	ManagementMessageID uuid.UUID
	CollectionReportID  uuid.UUID
	LastMoneyReportID   int
}

func (m MngtMoneyReport) IsLastCollection() bool {
	return m.ID > m.LastMoneyReportID
}

// CollectionReport is how much was collected from a station + when
type CollectionReport struct {
	ID           int
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
	QrMoney      int
	Ctime        time.Time
	ManagementID uuid.UUID
}

// CollectionReportWithUser is how much was collected from a station + when with username who committed it
type CollectionReportWithUser struct {
	StationID    StationID
	Banknotes    int
	CarsTotal    int
	Coins        int
	Electronical int
	Service      int
	Bonuses      int
	QrMoney      int
	Ctime        time.Time
	User         string
}

// RelayStat is not used now, but generally shows how much do they work
type RelayStat struct {
	RelayID       int
	SwitchedCount int
	TotalTimeOn   int64
}

// RelayReport is RelayStat for multiple(actaully all) relays of one station
type RelayReport struct {
	StationID  StationID
	ProgramID  int
	TimeOn     int
	PumpTimeOn int
	RelayStats []RelayStat
}

type StationStat struct {
	StationID    StationID
	PumpTimeOn   int
	RelayStats   []RelayStat
	ProgramStats []ProgramStat
}

type ProgramStat struct {
	ProgramID   int
	ProgramName string
	TimeOn      int
}

type StationsStat map[StationID]StationStat

// CardReaderConfig is for Vendotek(Ethernet) or Paymentworld(their own binary exec)
type CardReaderConfig struct {
	StationID      StationID
	CardReaderType string
	Host           string
	Port           string
}

type DiscountProgram struct {
	Discount  int64
	ProgramID int64
}

type AdvertisingCampaign struct {
	ID               int64
	Name             string
	DefaultDiscount  int64
	DiscountPrograms []DiscountProgram
	StartDate        time.Time
	EndDate          time.Time
	StartMinute      int64
	EndMinute        int64
	Weekday          []string
	Enabled          bool
	Deleted          bool
	Version          int
}

type AdvertisingCampaignFilter struct {
	StartDate, EndDate *time.Time
	Pagination
}

type ProgramsDiscount struct {
	DefaultDiscount int64
	Discounts       map[int64]int64
}

type ButtonDiscount struct {
	ButtonID int64
	Discount int64
}

type StationDiscount struct {
	Discounts []ButtonDiscount
}

type ConfigInt struct {
	Name        string
	Value       int64
	Description string
	Note        string
	Version     int
}
type ConfigBool struct {
	Name        string
	Value       bool
	Description string
	Note        string
	Version     int
}
type ConfigString struct {
	Name        string
	Value       string
	Description string
	Note        string
	Deleted     bool
	Version     int
}

type StationConfigVar[T comparable] struct {
	Name        string
	Value       T
	Description string
	Note        string
	StationID   StationID
	Version     int
}

type RabbitConfig struct {
	ServerID  string
	ServerKey string
}

type ServiceStatus struct {
	Available        bool
	DisabledOnServer bool
	IsConnected      bool
	LastErr          string
	DateLastErr      *time.Time
	UnpaidStations   map[int]bool
	ReconnectCount   int64
}

type BuildScript struct {
	ID        int
	StationID StationID
	Name      string
	Commands  []string
}

type SetBuildScript struct {
	StationID         StationID
	CopyFromStationID *StationID
	Name              string
	Commands          []string
}

type OpenwashingLog struct {
	ID        int64
	StationID StationID
	Text      string
	Type      *string
	Level     LogLevel
	CreatedAt time.Time
}

type OpenwashingLogCreate struct {
	StationID StationID
	Text      string
	Type      *string
	Level     LogLevel
}

type LogLevel string
type TaskType string
type TaskStatus string
type TaskSort string

const (
	DebugLogLevel   LogLevel = "debug"
	InfoLogLevel    LogLevel = "info"
	WarningLogLevel LogLevel = "warning"
	ErrorLogLevel   LogLevel = "error"

	BuildTaskType        TaskType = "build"
	UpdateTaskType       TaskType = "update"
	RebootTaskType       TaskType = "reboot"
	GetVersionsTaskType  TaskType = "getVersions"
	PullFirmwareTaskType TaskType = "pullFirmware"
	SetVersionTaskType   TaskType = "setVersion"

	QueueTaskStatus     TaskStatus = "queue"
	StartedTaskStatus   TaskStatus = "started"
	CompletedTaskStatus TaskStatus = "completed"
	ErrorTaskStatus     TaskStatus = "error"
	CanceledTaskStatus  TaskStatus = "canceled"

	CreatedAtAscTaskSort  TaskSort = "createdAtAsc"
	CreatedAtDescTaskSort TaskSort = "createdAtDesc"
)

type Task struct {
	ID         int
	StationID  StationID
	VersionID  *int
	Type       TaskType
	Status     TaskStatus
	RetryCount int
	Error      *string
	CreatedAt  time.Time
	StartedAt  *time.Time
	StoppedAt  *time.Time
}

type CreateTask struct {
	StationID StationID
	VersionID *int
	Type      TaskType
}

type UpdateTask struct {
	Status     *TaskStatus
	Error      *string
	RetryCount *int
	StartedAt  *time.Time
	StoppedAt  *time.Time
}

type TaskFilter struct {
	Pagination
	StationsID []StationID
	Statuses   []TaskStatus
	Types      []TaskType
	Sort       *TaskSort
}

type FirmwareVersionJson struct {
	HashLua    string    `json:"hashLua"`
	HashEnv    string    `json:"hashEnv"`
	HashBinar  string    `json:"hashBinar"`
	BuiltAt    time.Time `json:"builtAt"`
	CommitedAt time.Time `json:"commitedAt"`
}

type FirmwareVersion struct {
	ID         int
	IsCurrent  bool
	HashLua    string
	HashEnv    string
	HashBinar  string
	BuiltAt    time.Time
	CommitedAt time.Time
}

func firmwareVersionFromJson(id int, current bool, jsonVersion FirmwareVersionJson) FirmwareVersion {
	return FirmwareVersion{
		ID:         id,
		IsCurrent:  current,
		HashLua:    jsonVersion.HashLua,
		HashEnv:    jsonVersion.HashEnv,
		HashBinar:  jsonVersion.HashBinar,
		BuiltAt:    jsonVersion.BuiltAt,
		CommitedAt: jsonVersion.CommitedAt,
	}
}

type ProgramFilter struct {
	ID *int64
	Pagination
}
