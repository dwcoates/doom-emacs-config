//go:build integration

package integration

import (
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"syscall"
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
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws, Source: harness.OwnBranch(false)})); err != nil {
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

// TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands is the owner's
// ruling end to end (2026-10-06): a daemon stopped while a merge runs its test
// gate -- with the target carrying content the merge never wrote -- comes back
// and finishes THAT merge, under the same lease (its bubble's identity), from
// the step it stood on. Nothing about the target's tree is read as evidence.
func TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands(t *testing.T) {
	t.Parallel()
	// Arrange: a gate whose FIRST run holds on a pipe nobody writes -- the
	// daemon's exit cuts it -- and whose every later run passes.
	repo := harness.NewRepo(t)
	gateDir := t.TempDir()
	started := filepath.Join(gateDir, "first-run")
	if err := syscall.Mkfifo(filepath.Join(gateDir, "hold"), 0o600); err != nil {
		t.Fatalf("making the gate's pipe: %v", err)
	}
	gate := filepath.Join(gateDir, "test-all.sh")
	body := "#!/bin/sh\n" +
		"if [ ! -f '" + started + "' ]; then : > '" + started + "'; cat '" + filepath.Join(gateDir, "hold") + "' > /dev/null; fi\n" +
		"echo 'daemon: passed in 1s'\nexit 0\n"
	if err := os.WriteFile(gate, []byte(body), 0o755); err != nil {
		t.Fatalf("writing the gate: %v", err)
	}
	env := []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + gate}
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir, Timeout: harness.MergeChainTimeout, ExtraEnv: env})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "resumed", "do the resumed thing", nil)
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	d.AwaitFileExists(started)
	// The target now carries content the merge never wrote.
	repo.SetDirty(repo.Dir, true)
	var lease string
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow("SELECT id FROM leases WHERE workspace_id = ?", f.ws.GetId()).Scan(&lease); err != nil {
			t.Fatalf("reading the merge's lease: %v", err)
		}
	})

	// Act: an orderly stop mid-gate, and a fresh daemon on the same state root.
	d.Stop()
	d.AwaitExit()
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, SelfRepo: repo.Dir, Timeout: harness.MergeChainTimeout,
		ExtraEnv: append(env, "AGENT_REPL_LOCK_DIR="+d.LockDir)})
	f.d = d2

	// Assert: the stop cut the gate and said so, the restart resumed the merge
	// at its tests under the lease it already held, and it landed.
	d.AwaitLogRecord(d.RunLogPath(), "the gate's cut", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.drain" && strings.Contains(r.Message, "runs it again from its start")
	})
	resumed := d2.AwaitLogRecord(d2.RunLogPath(), "the merge's resume", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.recover" && strings.Contains(r.Message, "resumes at the step it recorded")
	})
	if resumed.Context["step"] != "tests" || resumed.Context["lease"] != lease {
		t.Fatalf("the resume record is %v, want step tests under lease %s", resumed.Context, lease)
	}
	d2.AwaitLandingDeployed()
	for _, record := range daemonErrorRecords(t, d2) {
		if strings.HasPrefix(record.Operation, "daemon.merge.") {
			t.Errorf("the resumed merge recorded an error: %s: %s %v", record.Operation, record.Message, record.Context)
		}
	}
}
