package rs485

import "time"

// SingleMotorStatus represents the status of one motor
type SingleMotorStatus struct {
	SpeedCurrent int
	SpeedDesired int
	IsRunning    bool
	LastAnswered time.Time
}

// MotorsContainer represents all motors that we have
type MotorsContainer struct {
	motors []SingleMotorStatus
}
