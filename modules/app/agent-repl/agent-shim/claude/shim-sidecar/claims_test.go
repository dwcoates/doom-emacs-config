package main

import (
	"os"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// shellClaim is the claim the shim wrote for task b1: run call-1, whose
// launching call is on record in `owner`'s book ("" when it is not yet).
func shellClaim(owner string) *storev1.ShellRunClaimed {
	claimed := &storev1.ShellRunClaimed{Claim: &storev1.ShellRunClaim{
		VendorTaskId: "b1",
		Run:          &conversationv1.AgentActivityId{Value: "call-1"},
	}}
	if owner != "" {
		claimed.Owner = &conversationv1.AgentId{Value: owner}
	}
	return claimed
}

// claimsHarness is a live session "sess-1" whose transcript is watched, and a
// held shell spool b1 no transcript line claimed, after one cycle.
func claimsHarness(t *testing.T, store *fakeStore) (*harness, string) {
	t.Helper()
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	spool := h.spoolFile(t, "b1", "hello\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("the spool was read before any claim named it")
	}
	return h, spool
}

func TestAHeldShellSpoolIsReadOnceTheShimsClaimNamesItsRun(t *testing.T) {
	// Arrange: the shim claimed b1 for a call booked in the session's own book.
	h, spool := claimsHarness(t, &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("sess-1")}})

	// Act.
	h.sc.rescan()

	// Assert: claimed as that run's spawn, with the book's attribution.
	watched, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatal("the claimed spool was not read")
	}
	if watched.target.WorkspaceDir != h.workspace || watched.target.ClaudeSessionID != "sess-1" {
		t.Fatalf("spool attribution = %+v, want the owning book's workspace and session", watched.target)
	}
	if got := h.sc.owners.activityFor("b1"); got != "call-1" {
		t.Fatalf("owner run = %q, want call-1", got)
	}
	h.requireOnce(t, "shell-run-claim", "info")
}

// A RESTARTED SIDECAR REBUILDS ITS SHELL TRACKING FROM THE STORE. The launch
// that claimed a spool lies before the restarted reader's rewind point, so no
// transcript line claims it again, and the spool is startup backlog; the
// shim's durable claim is what brings it back under a tailer, whose terminal
// or LOST conclusion then closes the run. On 2026-09-30 a spool orphaned this
// way kept its run open in the store and held a daemon restart for minutes.
func TestARestartedSidecarReadsABacklogShellSpoolTheStoreClaims(t *testing.T) {
	// Arrange: the spool was last written well before this process started.
	h := newHarness(t, &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("sess-1")}})
	h.transcript(t, "sess-1", promptLine)
	spool := h.spoolFile(t, "b1", "[killed]\n")
	past := time.Now().Add(-2 * time.Hour)
	if err := os.Chtimes(spool, past, past); err != nil {
		t.Fatalf("Chtimes: %v", err)
	}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("the backlog spool the store claims was not read")
	}
	if got := h.sc.owners.activityFor("b1"); got != "call-1" {
		t.Fatalf("owner run = %q, want call-1", got)
	}
}

func TestAShellClaimWhoseCallIsNotOnRecordLeavesTheSpoolHeld(t *testing.T) {
	// Arrange.
	h, spool := claimsHarness(t, &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("")}})

	// Act.
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("a spool whose claim names no book was read")
	}
	if _, pending := h.sc.unclaimedShells["b1"]; !pending {
		t.Fatal("the spool is no longer asked about; it must stay held and be asked again")
	}
}

func TestAShellClaimWhoseBookIsNotAttributedLeavesTheSpoolHeld(t *testing.T) {
	// Arrange: the claim names a book whose transcript this reader never watched.
	h, spool := claimsHarness(t, &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("agent-elsewhere")}})

	// Act.
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("a spool whose owning book is not attributed was read")
	}
}

func TestNoHeldShellSpoolAsksTheStoreNothing(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.rescan()

	// Assert.
	if len(store.claimsAsked) != 0 {
		t.Fatalf("claims asked = %v, want none with no held shell spool", store.claimsAsked)
	}
}

func TestAHeldShellSpoolIsAskedForByItsTaskID(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h, _ := claimsHarness(t, store)

	// Act.
	h.sc.rescan()

	// Assert.
	if len(store.claimsAsked) != 1 || len(store.claimsAsked[0]) != 1 || store.claimsAsked[0][0] != "b1" {
		t.Fatalf("claims asked = %v, want [[b1]]", store.claimsAsked)
	}
}

func TestASpoolALaunchClaimedIsNotAskedAbout(t *testing.T) {
	// Arrange: the transcript's own launch claims b1.
	store := &fakeStore{}
	h, _ := claimsHarness(t, store)
	h.sc.TaskSpawned("b1", "call-1", "sess-1", "", false, h.workspace, "workspace-id", "sess-1", noLaunch)
	h.sc.rescan()

	// Act.
	h.sc.rescan()

	// Assert: only the first pass, before the launch resolved it, asked.
	if len(store.claimsAsked) != 1 {
		t.Fatalf("claims asked %d time(s), want 1", len(store.claimsAsked))
	}
}

func TestAStoreThatCannotAnswerClaimsIsReportedAndClaimsNothing(t *testing.T) {
	// Arrange.
	store := &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("sess-1")}, claimsFail: "disk I/O error"}
	h, spool := claimsHarness(t, store)

	// Act.
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("a spool was claimed from a store that refused to answer")
	}
	h.requireOnce(t, "storeclient-shell-run-claims", "error")
}

// A CLAIM'S WAIT IS STATED ONCE, AT INFO, WHEN IT BEGINS OR ITS REASON
// CHANGES (2026-11-23): six shells of the work account's sessions waited for
// hours on books no watched transcript attributed, and every pass said so
// only verbosely, so nothing in the logs showed why their runs stayed live.
func TestAShellClaimsWaitIsStatedAtInfoOnlyWhenItBeginsOrChanges(t *testing.T) {
	tests := []struct {
		name       string
		owners     []string
		wantInfos  []string
		wantDebugs int
	}{
		{name: "a wait on an unattributed book, asked twice", owners: []string{"agent-elsewhere", "agent-elsewhere"},
			wantInfos: []string{claimWaitUnattributed}, wantDebugs: 1},
		{name: "a wait on an unrecorded launch, asked twice", owners: []string{"", ""},
			wantInfos: []string{claimWaitUnrecorded}, wantDebugs: 1},
		{name: "a wait whose reason changes is stated again", owners: []string{"", "agent-elsewhere"},
			wantInfos: []string{claimWaitUnrecorded, claimWaitUnattributed}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			store := &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim(tt.owners[0])}}
			h, _ := claimsHarness(t, store)

			// Act.
			for _, owner := range tt.owners {
				store.claims = []*storev1.ShellRunClaimed{shellClaim(owner)}
				h.sc.rescan()
			}

			// Assert.
			var infos []string
			debugs := 0
			for _, rec := range h.records(t) {
				if rec.Operation != "shell-run-claim" {
					continue
				}
				switch rec.Level {
				case "info":
					reason, _ := rec.Context["reason"].(string)
					infos = append(infos, reason)
				case "debug":
					debugs++
				}
			}
			if len(infos) != len(tt.wantInfos) || debugs != tt.wantDebugs {
				t.Fatalf("info reasons %v and %d debug records, want %v and %d", infos, debugs, tt.wantInfos, tt.wantDebugs)
			}
			for i := range infos {
				if infos[i] != tt.wantInfos[i] {
					t.Fatalf("info reasons = %v, want %v", infos, tt.wantInfos)
				}
			}
		})
	}
}

func TestAClaimedSpoolForgetsItsWait(t *testing.T) {
	// Arrange: the claim waits on an unattributed book, then names the
	// session's own.
	store := &fakeStore{claims: []*storev1.ShellRunClaimed{shellClaim("agent-elsewhere")}}
	h, spool := claimsHarness(t, store)
	h.sc.rescan()
	store.claims = []*storev1.ShellRunClaimed{shellClaim("sess-1")}

	// Act.
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("the claimed spool was not read")
	}
	if reason, waiting := h.sc.claimWaits["b1"]; waiting {
		t.Fatalf("claim wait = %q after the claim, want none", reason)
	}
}
