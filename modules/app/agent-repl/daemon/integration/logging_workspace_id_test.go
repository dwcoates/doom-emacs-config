//go:build integration

package integration

import (
	"testing"

	"agent-repld/integration/harness"
)

// logging_workspace_id_test.go pins the owner's ruling of 2026-09-11: the
// workspace id on every log record, in every runtime, is the daemon-minted
// 16-hex ids.WorkspaceID. A reader grouping a workspace's records by
// workspace_id must see ONE group spanning the daemon's records and the
// shim's, not two named by two different derivations.

func TestAWorkspacesDaemonAndShimRecordsCarryTheSameWorkspaceID(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace, so both runtimes have written records.
	f := newOpened(t, harness.Opts{})
	want := f.ws.GetId()

	// Act: wait until each runtime has attributed at least one record, then
	// read both sinks whole.
	stamped := func(r harness.LogRecord) bool { return r.WorkspaceID != "" }
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "a daemon record carrying a workspace_id", stamped)
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "shim"), "a shim record carrying a workspace_id", stamped)
	daemonRecords := f.d.WorkspaceLog(f.repo.Dir, "daemon")
	shimRecords := f.d.WorkspaceLog(f.repo.Dir, "shim")

	// Assert: the same minted id on both sides, and nothing else anywhere.
	for _, group := range []struct {
		sink    string
		records []harness.LogRecord
	}{{"daemon", daemonRecords}, {"shim", shimRecords}} {
		stamped := 0
		for _, r := range group.records {
			if r.WorkspaceID == "" {
				continue
			}
			stamped++
			if r.WorkspaceID != want {
				t.Fatalf("%s.log record %q carries workspace_id %q, want the daemon-minted %q",
					group.sink, r.Operation, r.WorkspaceID, want)
			}
		}
		if stamped == 0 {
			t.Fatalf("%s.log carries no record with a workspace_id", group.sink)
		}
	}
}
