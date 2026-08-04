package server

import (
	"context"
	"testing"

	"claude-repld/internal/registry"
	"claude-repld/internal/sessioncontroller"
)

// THE DEAD PERSPECTIVE SWITCH.
//
// Every field on a SessionView except shim_attached is read back off the
// durable registry record, so after a daemon restart a workspace with no
// controller at all still reported non-terminal and fully backfilled. Emacs's
// switch-ensure skipped on exactly that pair, so switching to an unwired
// workspace sent no openWorkspace and nothing ever brought it up.
//
// These pin the daemon's half: the shaping must carry controller liveness as a
// fact of its own, the session controller must answer it truthfully, and the open path
// must ensure regardless of what the record durably says.

// settledRecord is the RESTART SHAPE: every durable field says the workspace
// is up and running, which is exactly what survives a daemon restart.
func settledRecord() registry.Record {
	return registry.Record{
		SessionID:       "s_restart",
		CWD:             "/w",
		ClaudeSessionID: "uuid-1",
		BackfillState:   sessioncontroller.BackfillDone,
		CreatedAt:       "2026-07-25T10:00:00Z",
	}
}

// TestSessionViewCarriesSessionControllerLiveness — the shaping does not derive the fact
// from the record, because the record cannot know it.
func TestSessionViewCarriesSessionControllerLiveness(t *testing.T) {
	// Arrange / Act.
	v := SessionViewFromRecord(nil, settledRecord(), nil, false)

	// Assert — the durable fields all say "up", and the one non-durable field
	// says the truth.
	if v.GetTerminal() {
		t.Fatal("the settled record read as terminal; this case is only meaningful with it alive")
	}
	if v.GetBackfill() == 0 {
		t.Fatal("the settled record lost its backfill verdict; this case needs it settled")
	}
	if v.GetShimAttached() {
		t.Fatal("a record with no live session controller reported shim_attached; a frontend would skip its bootstrap")
	}
}

// The other edge, so the field is not merely always-false.
func TestSessionViewReportsALiveSessionController(t *testing.T) {
	// Arrange / Act.
	v := SessionViewFromRecord(nil, settledRecord(), nil, true)

	// Assert.
	if !v.GetShimAttached() {
		t.Fatal("a workspace with a live session controller reported no shim attached")
	}
}

// TestSessionControllerLivenessIsFalseForAnUnbroughtUpWorkspace — the session controller's own
// answer, which is what a fresh daemon boot looks like for every workspace.
func TestSessionControllerLivenessIsFalseForAnUnbroughtUpWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act / Assert.
	if h.controller.Live("/w") {
		t.Fatal("a workspace nothing has brought up reported a live session controller")
	}
}

// TestSessionControllerLivenessIsTrueAfterEnsure — and it flips on the bring-up.
func TestSessionControllerLivenessIsTrueAfterEnsure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	if err := h.controller.Ensure("/w"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}

	// Assert.
	if !h.controller.Live("/w") {
		t.Fatal("a workspace with a brought-up controller reported none")
	}
}

// TestOpenWaitsForDriveabilityDespiteASettledRecord — the open path itself must never
// second-guess the switch. A record that durably claims to be fully backfilled
// says nothing about whether THIS daemon is driving it.
func TestOpenWaitsForDriveabilityDespiteASettledRecord(t *testing.T) {
	// Arrange.
	o, reg, ens, _ := openerRig(t)
	if err := reg.Put(settledRecord()); err != nil {
		t.Fatalf("Put: %v", err)
	}

	// Act.
	if err := o.Open(context.Background(), "/w"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(ens.driveable) != 1 || ens.driveable[0] != "/w" || len(ens.calls) != 0 {
		t.Fatalf("driveable=%v ensure=%v, want exactly one driveable bring-up for /w — a settled record must not skip it", ens.driveable, ens.calls)
	}
}
