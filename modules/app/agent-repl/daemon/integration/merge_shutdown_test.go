//go:build integration

package integration

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_shutdown_test.go is the deliberate stop landing INSIDE a merge's
// terminal: the window where the run is writing merged_at, closed and its
// lease release, and where closing the state client underneath it used to lose
// every one of those writes to a failed transaction.

// TestStoppingTheDaemonInsideAMergesTerminalStampsTheLandingWithNoFailedWrites
// covers the shutdown drain end to end. The run is held at the top of its
// terminal by the daemon's own test seam, which releases the instant the
// orderly exit begins — so the stamps really do race the store's close, and
// the drain is the only reason they land.
func TestStoppingTheDaemonInsideAMergesTerminalStampsTheLandingWithNoFailedWrites(t *testing.T) {
	t.Parallel()
	// Arrange: a merge on a repository that is NOT the daemon's own checkout,
	// so the run reaches its terminal straight from the prompts, with no
	// worktree removal and no self-reload in the way.
	rendezvous := filepath.Join(t.TempDir(), "merge-terminal")
	selfRepo := harness.NewRepo(t) // distinct identity; never a target.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{
		SelfRepo: selfRepo.Dir,
		// The daemon's own MergeTerminalPauseEnv. It is spelled here rather
		// than imported: the knob lives in package main, which no test binary
		// can import.
		ExtraEnv: []string{"AGENT_REPL_MERGE_PAUSE_IN_TERMINAL=" + rendezvous},
	})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	d.AwaitFileExists(rendezvous)

	// Act: SIGTERM with the run inside its terminal.
	d.Stop()
	d.AwaitExit()

	// Assert: the landing's durable stamps are on the row.
	var mergedAt sql.NullInt64
	var closed int
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow("SELECT merged_at, closed FROM workspaces WHERE id = ?", f.ws.GetId()).
			Scan(&mergedAt, &closed); err != nil {
			t.Fatalf("reading the merged workspace's row: %v", err)
		}
	})
	if !mergedAt.Valid {
		t.Fatal("merged_at is unstamped after a stop inside the terminal, want the drain to have held the store open")
	}
	if closed != 1 {
		t.Fatalf("the merged workspace's closed = %d, want 1", closed)
	}

	// Assert: nothing of the merge, and nothing of the store, failed. The
	// defect surfaced as "could not begin the transaction" against a closed
	// state client, recorded by the terminal's own writes -- so both the merge
	// operations and that signature are swept, across every sink under the
	// state root (the per-workspace sink is symlinked into it, so it is
	// readable whatever happened to the worktree).
	for _, record := range daemonErrorRecords(t, d) {
		if strings.HasPrefix(record.Operation, "daemon.merge.") {
			t.Errorf("the merge recorded an error under a stop inside its terminal: %s: %s %v",
				record.Operation, record.Message, record.Context)
		}
		if strings.Contains(record.Raw, "transaction") || strings.Contains(record.Raw, "database is closed") {
			t.Errorf("a write failed against the closing state client: %s: %s %v",
				record.Operation, record.Message, record.Context)
		}
	}
}

// daemonErrorRecords is every ERROR record in every sink under the state
// root's logs directory.
func daemonErrorRecords(t *testing.T, d *harness.Daemon) []harness.LogRecord {
	t.Helper()
	sinks, err := filepath.Glob(filepath.Join(d.StateDir, "logs", "*.log"))
	if err != nil {
		t.Fatalf("globbing the daemon's log sinks: %v", err)
	}
	var out []harness.LogRecord
	for _, sink := range sinks {
		for _, record := range harness.ReadLog(t, sink) {
			if record.Level == "error" || record.Level == "fatal" {
				out = append(out, record)
			}
		}
	}
	return out
}

// TestStoppingTheDaemonWithAMergeParkedRecordsNoFailedWrites is the OTHER
// shutdown window: the run is not in its terminal at all, it is PARKED on a
// conflict awaiting the user. The orderly exit cancels it, so the park returns
// and the run takes its stop-teardown -- and that teardown's lease release and
// queue-entry removal used to run after the state client had already closed,
// recording the merge's give-back as daemon.merge.teardown errors over a
// "could not begin the transaction" from the store.
func TestStoppingTheDaemonWithAMergeParkedRecordsNoFailedWrites(t *testing.T) {
	t.Parallel()
	// Arrange: a merge parked on a scripted conflict.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// The park itself is the arrangement: its conflict records are expected.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.conflicts")
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")
	host := f.d.WatchHost(f.ws)
	awaitView(t, f, host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})

	// Act: SIGTERM with the run parked.
	d.Stop()
	d.AwaitExit()

	// Assert: the give-back recorded nothing as a fault.
	for _, record := range daemonErrorRecords(t, d) {
		if strings.HasPrefix(record.Operation, "daemon.merge.") {
			t.Errorf("the merge recorded an error under a stop while it was parked: %s: %s %v",
				record.Operation, record.Message, record.Context)
		}
		if strings.Contains(record.Raw, "transaction") || strings.Contains(record.Raw, "database is closed") {
			t.Errorf("a write failed against the closing state client: %s: %s %v",
				record.Operation, record.Message, record.Context)
		}
	}
}
