package mergeworkspace

import (
	"os"
	"testing"

	"claude-repld/internal/tempdirs/tempdirstest"
)

// TestMain installs the temporary-directory guard the registry's tests
// register their workspaces under.
func TestMain(m *testing.M) { os.Exit(tempdirstest.Main(m)) }
