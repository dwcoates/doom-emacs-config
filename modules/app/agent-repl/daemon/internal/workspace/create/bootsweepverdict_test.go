package create

import (
	"context"
	"encoding/json"
	"errors"
	"path/filepath"
	"testing"
)

// errNoHost stands in for an Emacs that is not listening right now.
var errNoHost = errors.New("no host is connected")

// TestBootSweepVerdictIsRetainedAndDelivered: the verdict rides the same
// retained-until-completed envelope every other host action uses.
func TestBootSweepVerdictIsRetainedAndDelivered(t *testing.T) {
	// Arrange
	statePath := filepath.Join(t.TempDir(), "jobs.json")
	f := newFixture(t, statePath)

	// Act
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_no_live_shim", "its agent process is gone",
	); err != nil {
		t.Fatalf("SurfaceBootSweepVerdict: %v", err)
	}

	// Assert
	if f.actions.calls != 1 {
		t.Fatalf("publish calls = %d, want the verdict delivered once", f.actions.calls)
	}
	published := f.actions.items[0]
	if published.Type != HostActionTypeBootSweepSessionUnwired {
		t.Fatalf("published type = %q, want %q", published.Type, HostActionTypeBootSweepSessionUnwired)
	}
	var payload BootSweepSessionUnwired
	if err := json.Unmarshal(published.Payload, &payload); err != nil {
		t.Fatalf("unmarshal verdict payload: %v", err)
	}
	want := BootSweepSessionUnwired{
		Workspace: "/ws", SessionID: "s_1",
		Verdict: "boot_sweep_no_live_shim", Reason: "its agent process is gone",
	}
	if payload != want {
		t.Fatalf("payload = %#v, want %#v", payload, want)
	}
	stored := storedHostActions(t, statePath)
	if _, ok := stored[published.ID]; !ok {
		t.Fatalf("stored actions = %#v, want the verdict persisted under %q", stored, published.ID)
	}
}

// TestBootSweepVerdictSurvivesAnUndeliverableHost: an unreachable host is not a
// lost verdict — the action stays retained for the next drain.
func TestBootSweepVerdictSurvivesAnUndeliverableHost(t *testing.T) {
	// Arrange
	statePath := filepath.Join(t.TempDir(), "jobs.json")
	f := newFixture(t, statePath)
	f.actions.err = errNoHost

	// Act
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_probe_failed", "the daemon could not tell",
	); err != nil {
		t.Fatalf("SurfaceBootSweepVerdict = %v, want a retained verdict rather than a caller-visible failure", err)
	}

	// Assert
	pending, err := f.store.HostActionsForDelivery()
	if err != nil {
		t.Fatalf("HostActionsForDelivery: %v", err)
	}
	if len(pending) != 1 || pending[0].Type != HostActionTypeBootSweepSessionUnwired {
		t.Fatalf("pending = %#v, want the undelivered verdict still queued", pending)
	}
}

// TestBootSweepVerdictIsAcknowledgedLikeAnyOtherAction: completion releases it
// from redelivery and from the reconnect snapshot.
func TestBootSweepVerdictIsAcknowledgedLikeAnyOtherAction(t *testing.T) {
	// Arrange
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_no_live_shim", "its agent process is gone",
	); err != nil {
		t.Fatalf("SurfaceBootSweepVerdict: %v", err)
	}
	id := f.actions.items[0].ID

	// Act
	if err := f.manager.CompleteHostAction(id, true, ""); err != nil {
		t.Fatalf("CompleteHostAction: %v", err)
	}

	// Assert
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatalf("PendingHostActions: %v", err)
	}
	if len(pending) != 0 {
		t.Fatalf("pending after acknowledgement = %#v, want none", pending)
	}
}

// TestAnUnchangedVerdictDoesNotNagTwice: the id is keyed on session+verdict, so
// a daemon restarted over an unchanged situation says it once.
func TestAnUnchangedVerdictDoesNotNagTwice(t *testing.T) {
	// Arrange
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_no_live_shim", "its agent process is gone",
	); err != nil {
		t.Fatalf("first SurfaceBootSweepVerdict: %v", err)
	}

	// Act
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_no_live_shim", "its agent process is gone",
	); err != nil {
		t.Fatalf("second SurfaceBootSweepVerdict: %v", err)
	}

	// Assert
	if f.actions.calls != 1 {
		t.Fatalf("publish calls = %d, want the unchanged conclusion announced once", f.actions.calls)
	}
}

// TestADifferentVerdictAboutTheSameSessionIsStillDelivered: the situation
// changed, so the account of it must too.
func TestADifferentVerdictAboutTheSameSessionIsStillDelivered(t *testing.T) {
	// Arrange
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_probe_failed", "the daemon could not tell",
	); err != nil {
		t.Fatalf("first SurfaceBootSweepVerdict: %v", err)
	}

	// Act
	if err := f.manager.SurfaceBootSweepVerdict(
		context.Background(), "/ws", "s_1", "boot_sweep_no_live_shim", "its agent process is gone",
	); err != nil {
		t.Fatalf("second SurfaceBootSweepVerdict: %v", err)
	}

	// Assert
	if f.actions.calls != 2 {
		t.Fatalf("publish calls = %d, want both conclusions delivered", f.actions.calls)
	}
}

// TestBootSweepVerdictRequiresACompleteAccount: every field is load-bearing —
// the host renders the reason verbatim and keys nothing without the session.
func TestBootSweepVerdictRequiresACompleteAccount(t *testing.T) {
	tests := []struct {
		name                               string
		workspace, sessionID, verdict, why string
	}{
		{name: "no workspace", sessionID: "s_1", verdict: "v", why: "reason"},
		{name: "no session", workspace: "/ws", verdict: "v", why: "reason"},
		{name: "no verdict", workspace: "/ws", sessionID: "s_1", why: "reason"},
		{name: "no reason", workspace: "/ws", sessionID: "s_1", verdict: "v"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))

			// Act
			err := f.manager.SurfaceBootSweepVerdict(context.Background(), tc.workspace, tc.sessionID, tc.verdict, tc.why)

			// Assert
			if err == nil {
				t.Fatalf("SurfaceBootSweepVerdict = nil, want a refusal for an incomplete verdict")
			}
			if f.actions.calls != 0 {
				t.Fatalf("publish calls = %d, want nothing published for a refused verdict", f.actions.calls)
			}
		})
	}
}
