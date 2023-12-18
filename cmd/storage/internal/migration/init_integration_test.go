package migration

import (
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"

	"github.com/powerman/gotest/testinit"
	"github.com/powerman/pqx"
)

var (
	dbCfg          pqx.Config
	connectTimeout = 3 * def.TestSecond
)

func init() { testinit.Setup(2, setupIntegration) }

func setupIntegration() {
	dbCfg = TestDBConfig()
}
