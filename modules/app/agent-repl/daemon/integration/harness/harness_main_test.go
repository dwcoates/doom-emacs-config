package harness

import (
	"os"
	"testing"
)

// The harness's own tests use its temp helpers, which live under the run root.
func TestMain(m *testing.M) { os.Exit(WithRunRoot(m.Run)) }
