package app

import (
	"testing"
)

func TestCheckAccess(t *testing.T) {
	type args struct {
		auth        *Auth
		accessRules []func(*Auth) bool
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		{
			name: "Default user",
			args: args{
				auth:        &Auth{},
				accessRules: []func(auth *Auth) bool{},
			},
			want: true,
		},
		{
			name: "Need Admin #1",
			args: args{
				auth:        &Auth{IsAdmin: true},
				accessRules: []func(auth *Auth) bool{roleAdmin},
			},
			want: true,
		},
		{
			name: "Need Admin #2",
			args: args{
				auth:        &Auth{},
				accessRules: []func(auth *Auth) bool{roleAdmin},
			},
			want: false,
		},
		{
			name: "Need Admin and Operator #1",
			args: args{
				auth:        &Auth{},
				accessRules: []func(auth *Auth) bool{allRules(roleAdmin, roleOperator)},
			},
			want: false,
		},
		{
			name: "Need Admin and Operator #2",
			args: args{
				auth:        &Auth{},
				accessRules: []func(auth *Auth) bool{roleAdmin, roleOperator},
			},
			want: false,
		},
		{
			name: "Need Admin and Operator #3",
			args: args{
				auth:        &Auth{IsAdmin: true},
				accessRules: []func(auth *Auth) bool{allRules(roleAdmin, roleOperator)},
			},
			want: false,
		},
		{
			name: "Need Admin and Operator #4",
			args: args{
				auth:        &Auth{IsOperator: true},
				accessRules: []func(auth *Auth) bool{allRules(roleAdmin, roleOperator)},
			},
			want: false,
		},
		{
			name: "Need Admin and Operator #5",
			args: args{
				auth:        &Auth{IsAdmin: true, IsOperator: true},
				accessRules: []func(auth *Auth) bool{allRules(roleAdmin, roleOperator)},
			},
			want: true,
		},
		{
			name: "Admin or Operator #1",
			args: args{
				auth:        &Auth{},
				accessRules: []func(auth *Auth) bool{anyRule(roleAdmin, roleOperator)},
			},
			want: false,
		},
		{
			name: "Admin or Operator #2",
			args: args{
				auth:        &Auth{IsAdmin: true},
				accessRules: []func(auth *Auth) bool{anyRule(roleAdmin, roleOperator)},
			},
			want: true,
		},
		{
			name: "Admin or Operator #3",
			args: args{
				auth:        &Auth{IsOperator: true},
				accessRules: []func(auth *Auth) bool{anyRule(roleAdmin, roleOperator)},
			},
			want: true,
		},
		{
			name: "Admin or Operator #4",
			args: args{
				auth:        &Auth{IsAdmin: true, IsOperator: true},
				accessRules: []func(auth *Auth) bool{anyRule(roleAdmin, roleOperator)},
			},
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := CheckAccess(tt.args.auth, tt.args.accessRules...); got != tt.want {
				t.Errorf("CheckAccess() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_roleAdmin(t *testing.T) {
	type args struct {
		auth *Auth
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		{
			name: "roleAdmin #1",
			args: args{
				auth: &Auth{},
			},
			want: false,
		},
		{
			name: "roleAdmin #2",
			args: args{
				auth: &Auth{IsAdmin: true},
			},
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := roleAdmin(tt.args.auth); got != tt.want {
				t.Errorf("roleAdmin() = %v, want %v", got, tt.want)
			}
		})
	}
}
