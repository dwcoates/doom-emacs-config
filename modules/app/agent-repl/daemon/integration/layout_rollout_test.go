//go:build integration

package integration

import (
	"context"
	"database/sql"
	"os"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	_ "modernc.org/sqlite"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// THE 2026-09-27 INCIDENT, END TO END.
//
// master landed a state layout bump; the deploy handed the daemon over; the
// successor bound, reported its address, and exited 3ms later refusing the
// older layout on its read-only handle. The incumbent read the report as "the
// successor is up", quiesced and transferred five workspaces to a dead
// address, and their quiesce holds outlived every daemon after it: the
// composer drew `restarting` and Emacs refused every prompt.
//
// These tests stage that world for real -- a real claude-repld successor,
// really spawned, really dying on a state database stamped one layout older
// -- and pin each invariant the fix stands on.

// stateDB answers the daemon's state database path.
func stateDB(t *testing.T, d *harness.Daemon) string {
	t.Helper()
	layout, err := stateroot.Root(d.StateDir, "")
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}
	return layout.DB()
}

// rawStateDB opens the state database with no layout check at all: the
// incident's database carries a layout this build's own handles refuse.
func rawStateDB(t *testing.T, d *harness.Daemon) *sql.DB {
	t.Helper()
	db, err := sql.Open("sqlite", stateDB(t, d)+"?_pragma=busy_timeout(5000)")
	if err != nil {
		t.Fatalf("open the state database: %v", err)
	}
	t.Cleanup(func() {
		if err := db.Close(); err != nil {
			t.Errorf("close the state database: %v", err)
		}
	})
	return db
}

// stampOlderLayout stamps the state database one layout older than this
// build -- what a successor finds when master lands a layout its incumbent
// never migrated to. The running incumbent never re-reads the stamp, so it
// serves on exactly as the incident's did.
func stampOlderLayout(t *testing.T, d *harness.Daemon) {
	t.Helper()
	if _, err := rawStateDB(t, d).ExecContext(context.Background(), `UPDATE layout SET version = ? WHERE id = 1`, wsm.LayoutVersion-1); err != nil {
		t.Fatalf("stamp the older layout: %v", err)
	}
}

// heldLeases answers every lease row in the state database.
func heldLeases(t *testing.T, d *harness.Daemon) []string {
	t.Helper()
	rows, err := rawStateDB(t, d).QueryContext(context.Background(), `SELECT id || ' ' || workspace_id || ' holder=' || holder FROM leases`)
	if err != nil {
		t.Fatalf("read the leases: %v", err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var row string
		if err := rows.Scan(&row); err != nil {
			t.Fatalf("scan a lease: %v", err)
		}
		out = append(out, row)
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("read the leases: %v", err)
	}
	return out
}

func TestAHandoverWhoseSuccessorDiesAtBootTransfersNothingAndKeepsServing(t *testing.T) {
	t.Parallel()
	// Arrange: an open, idle workspace on a daemon whose state database is
	// stamped one layout older than the successor it is about to spawn.
	selfRepo, d := drainSelfRepoDaemon(t)
	d.ExpectWarnings(
		// The incumbent's abandonment of the handover, and the deploy and
		// landing that asked for it: the loud record the incident lacked.
		// (The successor's own refusal of the layout is its pid's, not this
		// daemon's.)
		"daemon.rollout.handover", "daemon.deploy.decide", "daemon.deploy.landing",
	)
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	daemonStream := d.WatchDaemonStream()
	stampOlderLayout(t, d)

	// Act
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	abandoned := d.AwaitLogRecord(d.RunLogPath(), "the abandoned handover", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.handover" && r.Level == "error" &&
			strings.Contains(r.Message, "the successor never proved it was serving")
	})

	// Assert: the ERROR names the successor's exit.
	if cause, _ := abandoned.Context["cause"].(string); !strings.Contains(cause, "exited before it answered") {
		t.Fatalf("the abandonment's cause = %q, want the successor's exit named", cause)
	}
	// Assert: nothing was announced or transferred.
	harness.ExpectNoPush(t, daemonStream, harness.ProbeWindow, "shutdown_announced for a successor that never served")
	// Assert: no lease was left behind.
	if leases := heldLeases(t, d); len(leases) != 0 {
		t.Fatalf("leases after the abandoned handover = %v, want none", leases)
	}
	// Assert: the incumbent still serves the workspace.
	resp, err := d.Client().SubmitPrompt(d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said("still served"), IdempotencyKey: "k-after-abandon",
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt after the abandoned handover = error %v, want the incumbent to answer", err)
	}
	if resp.Msg.GetError() != nil {
		t.Fatalf("SubmitPrompt after the abandoned handover = %v, want it accepted here", resp.Msg.GetError())
	}
}

// TestAFreshBootReleasesTheHoldsADeadDaemonLeft is the incident's tail: the
// predecessor holding five quiesce leases was SIGQUIT-killed, and the fresh
// daemon that booted after it kept all five. A lease a process took dies with
// it now: the boot releases every lease its own handle did not take.
func TestAFreshBootReleasesTheHoldsADeadDaemonLeft(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace carrying a handover's quiesce hold (holder
	// restart, policy hold) that no living process owns.
	first := newRegistered(t, harness.Opts{})
	first.d.Kill()
	if _, err := rawStateDB(t, first.d).ExecContext(context.Background(),
		`INSERT INTO leases (id, workspace_id, holder, policy, acquired_at) VALUES (?, ?, ?, ?, ?)`,
		"36bf89e3b1404f24", first.ws.GetId(), int(wsm.HolderRestart), int(wsm.PolicyHold), time.Now().UnixNano()); err != nil {
		t.Fatalf("leave the dead daemon's hold: %v", err)
	}

	// Act
	second := harness.StartDaemon(t, harness.Opts{StateDir: first.d.StateDir})
	second.ExpectWarnings("daemon.boot.orphan_leases")
	released := second.AwaitLogRecord(second.RunLogPath(), "the orphan's release", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.boot.orphan_leases" && r.Level == "error"
	})

	// Assert
	if lease, _ := released.Context["lease"].(string); lease != "36bf89e3b1404f24" {
		t.Fatalf("the released lease = %q, want the dead daemon's hold named", lease)
	}
	if leases := heldLeases(t, second); len(leases) != 0 {
		t.Fatalf("leases after the fresh boot = %v, want the orphan released", leases)
	}
}

func TestALayoutChangeRestartsTheDaemonAndItsReplacementServes(t *testing.T) {
	t.Parallel()
	// Arrange
	selfRepo, d := drainSelfRepoDaemon(t)
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	daemonStream := d.WatchDaemonStream()
	before, err := os.ReadFile(d.AddrFile())
	if err != nil {
		t.Fatalf("read daemon.addr: %v", err)
	}
	incumbent := harness.AddrLine(string(before))

	// Act
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemonNewLayout)

	// Assert: a plain bounce is announced -- no successor is listening.
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	if announced.Address != nil || announced.GetCause().GetSelfMergeRollout() == nil {
		t.Fatalf("shutdown_announced = %v, want a self-merge rollout with no successor address", announced)
	}
	// Assert: the incumbent exits in order, and a replacement takes the claim.
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0", code)
	}
	replacement := awaitNewAdvertisement(t, d, incumbent)
	health, err := drainDial(replacement).DaemonHealth(d.Ctx(), connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))
	if err != nil || health.Msg == nil {
		t.Fatalf("DaemonHealth on the replacement = (%v, %v), want it serving", health, err)
	}
	// Assert: the replacement adopted the running shim rather than starting
	// another session, and no hold outlived the incumbent.
	if got := f.shim.Count(harness.RPCStartSession); got != 1 {
		t.Fatalf("StartSession was called %d times across the restart, want the original 1", got)
	}
	if leases := heldLeases(t, d); len(leases) != 0 {
		t.Fatalf("leases after the restart = %v, want none", leases)
	}
}

// awaitNewAdvertisement polls daemon.addr until it names an address other than
// old, bounded by the daemon's own context -- never a fixed sleep.
func awaitNewAdvertisement(t *testing.T, d *harness.Daemon, old string) string {
	t.Helper()
	ticker := time.NewTicker(5 * time.Millisecond)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(d.AddrFile()); err == nil {
			if addr := harness.AddrLine(string(body)); addr != "" && addr != old {
				return addr
			}
		}
		select {
		case <-ticker.C:
		case <-d.Ctx().Done():
			t.Fatalf("daemon.addr never named a replacement: %v", d.Ctx().Err())
		}
	}
}
