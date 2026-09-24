//go:build integration

package integration

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// staleBounceHoldBound is how long the successor's stale-build bounce of an
// adopted shim may hold the workspace's prompts. The fake leaves on
// KillSession in under a millisecond; this is DefaultTimeout-sized headroom
// for a loaded run, and a sixth of the 30s stand-down window the bounce used
// to wait out in full.
const staleBounceHoldBound = harness.DefaultTimeout

// TestAHandoverWithANewerShimBuildBouncesTheAdoptedShimCleanly is the first
// live handover that carried sessions (2026-09-24T18:06, incumbent 28276,
// successor 69484), end to end: a COLD-STARTED daemon with a live session
// hands over to a successor that finds a newer shim build installed, and the
// successor bounces the shim it adopted.
//
// Three records made that run loud, and each is a defect this test pins:
//   - the incumbent kept watching the shim it had handed over, and read the
//     successor's bounce of it as ERROR severings, a `link_severed` WARN and a
//     health fault (swept by the cold daemon's own cleanup);
//   - the takeover's staleness re-check called the still-running bounce
//     "already bounced for" at ERROR;
//   - the relaunch could not see the ADOPTED shim exit, waited out the whole
//     30s stand-down window, force-killed a shim already gone and held the
//     workspace's prompts all the while.
func TestAHandoverWithANewerShimBuildBouncesTheAdoptedShimCleanly(t *testing.T) {
	t.Parallel()

	// Arrange: a session on a first daemon, which then stands down its shim,
	// so the cold boot below SPAWNS the session afresh -- the live
	// incumbent's own shims were its cold boot's spawns.
	first := newDaemon(t, harness.Opts{})
	f := drainOpenWorkspace(t, first)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	expectSessionKillRecords(first)
	if _, err := first.Client().UpdateShutdownSchedule(first.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the cold start under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	first.AwaitExit()

	// Arrange: the cold daemon, on a shim bundle this test can replace.
	shimMain := filepath.Join(t.TempDir(), "main.js")
	if err := os.WriteFile(shimMain, []byte("// the running shim build\n"), 0o644); err != nil {
		t.Fatalf("write the shim bundle: %v", err)
	}
	selfRepo, cold := coldStartSelfRepoDaemonOn(t, first, shimMain)
	if resp, err := cold.Client().OpenWorkspace(cold.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace on the cold daemon = (%v, %v), want the session live", resp, err)
	}
	host := cold.WatchHost(f.ws)
	harness.AwaitNext(t, cold.Ctx(), host, "the fresh host push")
	daemonStream := cold.WatchDaemonStream()

	// Arrange: a newer shim build is installed. The running shim still
	// reports the one it was spawned from.
	if err := os.WriteFile(shimMain, []byte("// a newer shim build\n"), 0o644); err != nil {
		t.Fatalf("install the newer shim bundle: %v", err)
	}

	// Act: the cold daemon hands over, and the successor adopts.
	drainTriggerDeploy(t, cold, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, cold.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	successor := drainDial(announced.GetAddress())
	harness.AwaitView(t, cold.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})

	// Assert: the incumbent closed its watches on the shim WITH the detach,
	// before the transfer notice went out -- so nothing it still holds can
	// read the successor's bounce of that shim as a severing.
	handedOver := false
	for _, r := range cold.RunLog() {
		if r.Operation == "daemon.workspace.fleet_rollout" && r.Context["watched"] == true &&
			strings.HasPrefix(r.Message, "handed the workspace's shim over") {
			handedOver = true
		}
	}
	if !handedOver {
		t.Errorf("the incumbent announced the transfer without closing its watches on the handed-over shim")
	}

	if resp, err := successor.AdoptHostWorkspace(cold.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptHostWorkspace on the successor = (%v, %v), want success", resp, err)
	}

	// Assert: the successor bounces the adopted shim for its stale build, and
	// the bounce finishes as soon as the shim has stood down.
	bouncing := awaitRunLogRecord(t, cold, "the successor's stale-build bounce", func(r harness.LogRecord) bool {
		return r.PID != cold.PID() && r.Operation == "daemon.rollout.staleness" &&
			strings.HasPrefix(r.Message, "the shim runs an older build than the installed one")
	})
	relaunched := awaitRunLogRecord(t, cold, "the successor's relaunch", func(r harness.LogRecord) bool {
		return r.PID == bouncing.PID && r.Operation == "daemon.rollout.relaunch" &&
			r.Message == "relaunched the workspace's shim"
	})
	held := recordTime(t, relaunched).Sub(recordTime(t, bouncing))
	t.Logf("the stale-build bounce of the adopted shim held the workspace's prompts for %s", held)
	if held >= staleBounceHoldBound {
		t.Fatalf("the bounce held the workspace's prompts for %s, want well under the 30s stand-down window (< %s)", held, staleBounceHoldBound)
	}
	if code := cold.AwaitExit(); code != 0 {
		t.Fatalf("the cold daemon's exit code = %d, want an orderly 0 after the transfer", code)
	}

	// Assert: the SUCCESSOR recorded no WARN or ERROR anywhere. The cold
	// daemon's own sweep covers the incumbent's records at cleanup.
	for _, r := range successorWarnings(t, cold, bouncing.PID) {
		t.Errorf("the successor recorded %s %s: %s %v", r.Level, r.Operation, r.Message, r.Context)
	}
}

// coldStartSelfRepoDaemonOn is coldStartSelfRepoDaemon on a named shim bundle,
// so the test can install a newer one under the running daemon.
func coldStartSelfRepoDaemonOn(t *testing.T, first *harness.Daemon, shimMain string) (*harness.Repo, *harness.Daemon) {
	t.Helper()
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{
		StateDir:   first.StateDir,
		ProfileDir: first.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", first.DefaultConfigDir},
		SelfRepo:   selfRepo.Dir,
		ExtraEnv:   []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
		Timeout:    harness.HandoverChainTimeout,
		ShimMain:   shimMain,
	})
	return selfRepo, d
}

// awaitRunLogRecord polls the shared run log for a record from ANY process,
// bounded by one harness wait: Daemon.AwaitLogRecord reads only the daemon's
// own pid, and the records awaited here are its successor's.
func awaitRunLogRecord(t *testing.T, d *harness.Daemon, what string, pred func(harness.LogRecord) bool) harness.LogRecord {
	t.Helper()
	wait, cancel := context.WithTimeout(d.Ctx(), harness.DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(5 * time.Millisecond)
	defer ticker.Stop()
	for {
		for _, r := range harness.ReadLog(t, d.RunLogPath()) {
			if pred(r) {
				return r
			}
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			t.Fatalf("waiting for %s in the run log: %v", what, wait.Err())
			return harness.LogRecord{}
		}
	}
}

// recordTime parses a record's timestamp.
func recordTime(t *testing.T, r harness.LogRecord) time.Time {
	t.Helper()
	at, err := time.Parse(time.RFC3339Nano, r.Timestamp)
	if err != nil {
		t.Fatalf("record timestamp %q: %v", r.Timestamp, err)
	}
	return at
}

// successorWarnings answers every WARN or ERROR pid wrote to the run log and
// to the state root's per-workspace daemon sinks.
func successorWarnings(t *testing.T, d *harness.Daemon, pid int) []harness.LogRecord {
	t.Helper()
	paths := append([]string{d.RunLogPath()}, d.WorkspaceLogTargets()...)
	var out []harness.LogRecord
	for _, path := range paths {
		for _, r := range harness.ReadLog(t, path) {
			if r.PID == pid && (r.Level == "warn" || r.Level == "error") {
				out = append(out, r)
			}
		}
	}
	return out
}
