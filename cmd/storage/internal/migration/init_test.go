package migration

import (
	"testing"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/def"

	"github.com/powerman/gotest/testinit"
)

func TestMain(m *testing.M) { testinit.Main(m) }

func init() { testinit.Setup(1, setup) }

func setup() {
	def.Init()
}
