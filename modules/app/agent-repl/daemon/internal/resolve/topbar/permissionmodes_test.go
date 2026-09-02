package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// newModesResolver arranges a resolver with the workspace bound, which is what
// every topbar call requires before a fact can land on it.
func newModesResolver(t *testing.T) (Resolver, ids.WorkspaceID) {
	t.Helper()
	r, err := New(testColors(), dlog.NewTestSurfaces())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	ws := ids.WorkspaceID("ws-1")
	if err := r.SetWorkspaceDir(ws, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	return r, ws
}

// TestPermissionModesReportsFalseBeforeAPickerIsServed covers the honest
// absence: a workspace whose session never stated a picker has no set to
// validate a mode switch against.
func TestPermissionModesReportsFalseBeforeAPickerIsServed(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)

	// Act.
	modes, ok := r.PermissionModes(ws)

	// Assert.
	if ok {
		t.Fatalf("PermissionModes = %v, true before any picker was served, want false", modes)
	}
}

// TestSessionStartServesTheFixedSwitchableSet pins that the session's opening
// installs the picker: nothing else serves one, so without this every
// SetPermissionMode would be refused as mode_not_served.
func TestSessionStartServesTheFixedSwitchableSet(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)

	// Act.
	r.OnSessionStarted(ws, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert.
	modes, ok := r.PermissionModes(ws)
	if !ok {
		t.Fatal("PermissionModes reported false after the session started")
	}
	if len(modes) != len(SwitchableModes) {
		t.Fatalf("modes = %v, want %v", modes, SwitchableModes)
	}
	for i, mode := range modes {
		if mode != SwitchableModes[i] {
			t.Fatalf("modes = %v, want %v", modes, SwitchableModes)
		}
	}
}

// TestTheServedPickerCarriesTheModeInForceAsCurrent pins that the mode the
// session opened with is the picker's current option.
func TestTheServedPickerCarriesTheModeInForceAsCurrent(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)

	// Act.
	r.OnSessionStarted(ws, &conversationv1.SessionStarted{
		VendorSessionId: "vendor-1",
		PermissionMode: &conversationv1.AgentPermissionMode{
			Mode: &conversationv1.AgentPermissionMode_Plan{Plan: &conversationv1.AgentPermissionModePlan{}},
		},
	})

	// Assert.
	facts, ok := r.StatusFacts(ws)
	if !ok || facts.PermissionMode != "plan" {
		t.Fatalf("permission mode = %q (held %v), want plan", facts.PermissionMode, ok)
	}
}
