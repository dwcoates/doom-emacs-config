package rollout

import (
	"os"
	"testing"

	"claude-repld/internal/tempdirs/tempdirstest"
)

// TestMain runs this package's tests under ONE exempt temporary root, so the
// real registry its fixtures open (wsm.WithTemporaryGuard(tempdirstest.Guard))
// registers the directories they make and refuses every other temporary
// folder.
func TestMain(m *testing.M) { os.Exit(tempdirstest.Main(m)) }
