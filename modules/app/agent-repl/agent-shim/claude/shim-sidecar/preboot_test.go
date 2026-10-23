package main

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
)

// The machine booted at bootMs; launches before it predate the boot.
const (
	bootMs      = int64(1_000_000)
	beforeBoot  = bootMs - 1
	afterBootMs = bootMs + 1
)

// launchAt is a launch read at ms from no transcript in particular, which
// this harness never watches, so it has no unread bytes.
func launchAt(ms int64) handler.LaunchSite { return handler.LaunchSite{AtMs: ms} }

// prebootHarness is a sidecar in production whose machine booted at bootMs.
func prebootHarness(t *testing.T, store *fakeStore) *harness {
	t.Helper()
	h := newHarness(t, store)
	h.sc.bootTimeMs = func() int64 { return bootMs }
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	return h
}

func TestAPreBootShellWithNoSpoolIsConcludedLostAtTheSweep(t *testing.T) {
	cases := []struct {
		name   string
		output func(h *harness) string
	}{
		{name: "the launch named no spool", output: func(*harness) string { return "" }},
		{name: "the launch named a spool the boot wiped", output: func(h *harness) string {
			return filepath.Join(h.spool, "claude-501", "proj", "sess", "tasks", "b1gone.output")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{}
			h := prebootHarness(t, store)
			h.sc.TaskSpawned("b1gone", "toolu_gone", "agent-1", tc.output(h), false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
			h.sc.rescan()

			// Act
			h.sc.sweep()

			// Assert
			if got := lostTerminalsFor(store.writes, "toolu_gone"); got != 1 {
				t.Fatalf("LOST terminals = %d, want 1", got)
			}
			if len(h.sc.preBoot) != 0 {
				t.Fatalf("preBoot = %v, want the concluded run released", h.sc.preBoot)
			}
		})
	}
}

func TestOnlyAPreBootShellWithNoReadableSpoolIsHeld(t *testing.T) {
	cases := []struct {
		name         string
		task         string
		backgrounded bool
		launchedAt   int64
		spoolExists  bool
	}{
		{name: "a shell launched after the boot", task: "b1", launchedAt: afterBootMs},
		{name: "a launch with no instant (a claimed spool)", task: "b1", launchedAt: 0},
		{name: "a backgrounded subagent", task: "a1", backgrounded: true, launchedAt: beforeBoot},
		{name: "a workflow run", task: "w1", launchedAt: beforeBoot},
		{name: "a shell whose spool survived the boot", task: "b1", launchedAt: beforeBoot, spoolExists: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{}
			h := prebootHarness(t, store)
			output := ""
			if tc.spoolExists {
				output = filepath.Join(t.TempDir(), tc.task+".output")
				if err := os.WriteFile(output, []byte("x\n"), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			h.sc.TaskSpawned(tc.task, "toolu_run", "agent-1", output, tc.backgrounded, "/workspace", "workspace-id", "session-1", launchAt(tc.launchedAt))
			h.sc.concludePreBoot()

			// Assert
			if len(h.sc.preBoot) != 0 || lostTerminalsFor(store.writes, "toolu_run") != 0 {
				t.Fatalf("preBoot = %v, LOST = %d; want nothing held or written", h.sc.preBoot, lostTerminalsFor(store.writes, "toolu_run"))
			}
		})
	}
}

func TestNoBootTimeHoldsNothing(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	h := prebootHarness(t, store)
	h.sc.bootTimeMs = func() int64 { return 0 }

	// Act
	h.sc.TaskSpawned("b1", "toolu_run", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))

	// Assert
	if len(h.sc.preBoot) != 0 {
		t.Fatalf("preBoot = %v, want nothing held without a boot time", h.sc.preBoot)
	}
}

func TestAPreBootShellAlreadyEndedOnRecordGetsNoLost(t *testing.T) {
	// Arrange
	store := &fakeStore{settled: map[string]int64{"toolu_done": 42}}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1done", "toolu_done", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	h.sc.rescan()

	// Act
	h.sc.sweep()

	// Assert
	if got := lostTerminalsFor(store.writes, "toolu_done"); got != 0 {
		t.Fatalf("LOST terminals = %d, want none over a run already ended", got)
	}
	if len(h.sc.preBoot) != 0 {
		t.Fatalf("preBoot = %v, want the settled run released", h.sc.preBoot)
	}
}

func TestAPreBootShellAnotherFactEndsGetsNoLost(t *testing.T) {
	// Arrange: the transcript also carried the run's stop.
	store := &fakeStore{}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1stopped", "toolu_stopped", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	h.sc.stopped["b1stopped"] = 1
	h.sc.rescan()

	// Act
	h.sc.concludePreBoot()

	// Assert
	if got := lostTerminalsFor(store.writes, "toolu_stopped"); got != 0 {
		t.Fatalf("LOST terminals = %d, want none for a run its stop ends", got)
	}
	if len(h.sc.preBoot) != 0 {
		t.Fatalf("preBoot = %v, want the run released", h.sc.preBoot)
	}
}

func TestAPreBootShellStaysHeldWhileTheStoreCannotSaySettled(t *testing.T) {
	// Arrange
	store := &fakeStore{settlementsFail: "disk I/O error"}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1held", "toolu_held", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	h.sc.rescan()

	// Act
	h.sc.concludePreBoot()

	// Assert
	if lostTerminalsFor(store.writes, "toolu_held") != 0 || len(h.sc.preBoot) != 1 {
		t.Fatalf("LOST = %d, held = %d; want no LOST and the run still held", lostTerminalsFor(store.writes, "toolu_held"), len(h.sc.preBoot))
	}
	h.requireOnce(t, "preboot-run", "error")
}

func TestAPreBootShellStaysHeldWhenItsTerminalIsNotCommitted(t *testing.T) {
	// Arrange
	store := &fakeStore{writeFail: "disk full"}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1unwritten", "toolu_unwritten", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	h.sc.rescan()

	// Act
	h.sc.concludePreBoot()

	// Assert
	if len(h.sc.preBoot) != 1 {
		t.Fatalf("held = %d, want the run held for the next sweep", len(h.sc.preBoot))
	}
	h.requireOnce(t, "preboot-run", "error")
}

func TestPreBootSettlementsAreAskedInBatches(t *testing.T) {
	// Arrange: one more run than a batch holds, all already ended.
	store := &fakeStore{settled: map[string]int64{}}
	h := prebootHarness(t, store)
	for i := range preBootBatch + 1 {
		run := fmt.Sprintf("toolu_%04d", i)
		store.settled[run] = 1
		h.sc.TaskSpawned(fmt.Sprintf("b%04d", i), run, "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	}
	h.sc.rescan()
	asked := len(store.settlementsAsked)

	// Act
	h.sc.concludePreBoot()

	// Assert
	if got := len(store.settlementsAsked) - asked; got != 2 {
		t.Fatalf("settlement questions = %d, want 2 (a batch of %d, then 1)", got, preBootBatch)
	}
	if len(h.sc.preBoot) != 0 {
		t.Fatalf("held = %d, want every settled run released", len(h.sc.preBoot))
	}
}

func TestAPreBootShellWaitsForAFullScanAfterItWasHeld(t *testing.T) {
	// Arrange: held, and no full scan has completed since.
	store := &fakeStore{}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1wait", "toolu_wait", "agent-1", "", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))

	// Act
	h.sc.concludePreBoot()

	// Assert
	if lostTerminalsFor(store.writes, "toolu_wait") != 0 || len(h.sc.preBoot) != 1 {
		t.Fatalf("LOST = %d, held = %d; want it held until a full scan has looked for its spool", lostTerminalsFor(store.writes, "toolu_wait"), len(h.sc.preBoot))
	}
}

func TestAPreBootShellDiscoveryFindsASpoolForIsReleased(t *testing.T) {
	// Arrange: the launch named a path that does not exist, but discovery
	// holds a spool for the same task id elsewhere.
	store := &fakeStore{}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1found", "toolu_found", "agent-1", "/nowhere/b1found.output", false, "/workspace", "workspace-id", "session-1", launchAt(beforeBoot))
	h.sc.unclaimedShells["b1found"] = "/elsewhere/tasks/b1found.output" // the launch named the task, so its run is known
	h.sc.rescan()

	// Act
	h.sc.concludePreBoot()

	// Assert
	if lostTerminalsFor(store.writes, "toolu_found") != 0 || len(h.sc.preBoot) != 0 {
		t.Fatalf("LOST = %d, held = %d; want it released to its spool's reader", lostTerminalsFor(store.writes, "toolu_found"), len(h.sc.preBoot))
	}
}

// recordRun is a live shell the record names, announced at ms.
func recordRun(run, owner string, ms int64) *storev1.GetLiveShellRunsAnnouncedBeforeRun {
	return &storev1.GetLiveShellRunsAnnouncedBeforeRun{
		Detached:      &storev1.GetLiveWorkDetached{Work: &conversationv1.DetachedWorkId{Value: run}, Owner: &conversationv1.AgentId{Value: owner}},
		Run:           &conversationv1.AgentActivityId{Value: run},
		AnnouncedAtMs: ms,
	}
}

func TestAShellTheRecordNamesFromBeforeTheBootIsConcludedLost(t *testing.T) {
	// Arrange: no launch is read by this process; only the record names it.
	store := &fakeStore{liveShells: []*storev1.GetLiveShellRunsAnnouncedBeforeRun{recordRun("toolu_ghost", "agent-1", beforeBoot)}}
	h := prebootHarness(t, store)
	h.sc.rescan()

	// Act: the first sweep asks the record and holds the run; a full scan
	// later, the next sweep concludes it.
	h.sc.sweep()
	h.sc.rescan()
	h.sc.sweep()

	// Assert
	if got := lostTerminalsFor(store.writes, "toolu_ghost"); got != 1 {
		t.Fatalf("LOST terminals = %d, want 1", got)
	}
	if len(store.liveShellsAsked) != 1 || store.liveShellsAsked[0] != bootMs {
		t.Fatalf("the record was asked %v, want once with the boot instant %d", store.liveShellsAsked, bootMs)
	}
}

func TestTheRecordIsNotAskedBeforeTheFirstFullScan(t *testing.T) {
	// Arrange: a process that has completed no full scan yet.
	store := &fakeStore{}
	h := prebootHarness(t, store)
	h.sc.fullScans = 0

	// Act
	h.sc.askRecordForPreBoot()

	// Assert
	if len(store.liveShellsAsked) != 0 {
		t.Fatalf("the record was asked %v before any full scan", store.liveShellsAsked)
	}
}

func TestTheRecordIsAskedAgainOnlyAfterItFailed(t *testing.T) {
	// Arrange
	store := &fakeStore{liveShellsFail: "database is locked"}
	h := prebootHarness(t, store)
	h.sc.rescan()

	// Act
	h.sc.askRecordForPreBoot()
	store.liveShellsFail = ""
	h.sc.askRecordForPreBoot()
	h.sc.askRecordForPreBoot()

	// Assert
	if len(store.liveShellsAsked) != 2 {
		t.Fatalf("the record was asked %d times, want 2: once failing, once answering, never after", len(store.liveShellsAsked))
	}
	h.requireOnce(t, "preboot-run", "error")
}

func TestARecordRunWithASpoolIsReleasedToItsReader(t *testing.T) {
	// Arrange: the record names the run, and a launch read here ties it to a
	// spool discovery holds.
	store := &fakeStore{liveShells: []*storev1.GetLiveShellRunsAnnouncedBeforeRun{recordRun("toolu_spooled", "agent-1", beforeBoot)}}
	h := prebootHarness(t, store)
	h.sc.TaskSpawned("b1spooled", "toolu_spooled", "agent-1", "", false, "/workspace", "workspace-id", "session-1", noLaunch)
	h.sc.unclaimedShells["b1spooled"] = "/elsewhere/tasks/b1spooled.output"
	h.sc.rescan()
	h.sc.askRecordForPreBoot()
	h.sc.rescan()

	// Act
	h.sc.concludePreBoot()

	// Assert
	if lostTerminalsFor(store.writes, "toolu_spooled") != 0 || len(h.sc.preBoot) != 0 {
		t.Fatalf("LOST = %d, held = %d; want it released to its spool's reader", lostTerminalsFor(store.writes, "toolu_spooled"), len(h.sc.preBoot))
	}
}
