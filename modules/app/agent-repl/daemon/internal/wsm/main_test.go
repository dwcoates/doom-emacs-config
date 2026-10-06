package wsm

import (
	"os"
	"testing"

	"claude-repld/internal/tempdirs/tempdirstest"
)

// TestMain runs this package's tests under ONE exempt temporary root
// (tempdirstest.Main), and makes a handle opened without WithTemporaryGuard
// exempt that root: this package opens handles at dozens of sites, and the
// default guard is the production one.
func TestMain(m *testing.M) {
	productionTemporaryGuard = tempdirstest.RunGuard
	os.Exit(tempdirstest.Main(m))
}
