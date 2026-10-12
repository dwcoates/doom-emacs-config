//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

func logFileSizes(t *testing.T, d *harness.Daemon) map[string]int64 {
	t.Helper()
	paths, err := filepath.Glob(filepath.Join(d.StateDir, "logs", "*.log"))
	if err != nil {
		t.Fatalf("glob the logs: %v", err)
	}
	out := map[string]int64{}
	for _, p := range paths {
		info, err := os.Stat(p)
		if err != nil {
			t.Fatalf("stat %s: %v", p, err)
		}
		out[filepath.Base(p)] = info.Size()
	}
	return out
}

func TestAPipedDaemonsRecordsAreReadableWhileItsLogFilesStayEmpty(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	runLog := f.d.RunLog()
	workspaceLog := f.d.WorkspaceLog(f.repo.Dir, "daemon")
	sizes := logFileSizes(t, f.d)

	// Assert
	if len(runLog) == 0 || len(workspaceLog) == 0 {
		t.Fatalf("run log %d records, workspace log %d records; want both read over the pipe", len(runLog), len(workspaceLog))
	}
	for name, size := range sizes {
		if size != 0 && !isShimTarget(name) {
			t.Fatalf("%s holds %d bytes on disk, want every piped log empty (sizes %v)", name, size, sizes)
		}
	}
}

func TestADaemonOnFileLogsWritesItsRecordsToDisk(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{FileLogs: true})

	// Act
	records := d.RunLog()
	sizes := logFileSizes(t, d)

	// Assert
	if len(records) == 0 || sizes["daemon.run.log"] == 0 {
		t.Fatalf("run log %d records, %d bytes on disk; want both under FileLogs", len(records), sizes["daemon.run.log"])
	}
}

// isShimTarget reports whether a log target is a shim sink's, which the shim
// writes itself and the pipe never carries.
func isShimTarget(name string) bool { return strings.Contains(name, "-shim-") }
