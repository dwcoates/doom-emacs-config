//go:build integration

package integration

import (
	"path/filepath"
	"testing"

	"agent-repld/integration/harness"

	"agentrepl/testrun/testenv"
)

// A TEST RUN COUNTS EACH TEST'S DAEMON LOG RECORDS (testenv.LogRecordsOut):
// the harness's sweep, which reads every record the test's daemon wrote,
// appends their count under the test's name as the test ends.
//
// Not parallel: the counts file is the process's environment.
func TestTheHarnessCountsTheLogRecordsOfEachTestsDaemon(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "records.jsonl")
	t.Setenv(testenv.LogRecordsOut, path)

	// Act: the inner test's cleanups, the sweep among them, run as it ends.
	var inner string
	t.Run("a daemon serving one workspace", func(t *testing.T) {
		inner = t.Name()
		d := harness.StartDaemon(t, harness.Opts{})
		harness.Register(t, d, harness.NewRepo(t).Dir)
	})

	// Assert
	counts, err := testenv.ReadLogRecords(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(counts) != 1 || counts[inner] == 0 {
		t.Fatalf("counts = %v, want one positive count for %s", counts, inner)
	}
}
