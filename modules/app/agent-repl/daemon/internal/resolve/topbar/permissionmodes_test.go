package topbar

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

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

// TestPermissionModesAnswersExactlyTheServedSet covers the served set: the
// picker's options are what a switch is checked against, in the order served.
func TestPermissionModesAnswersExactlyTheServedSet(t *testing.T) {
	// Arrange.
	r, ws := newModesResolver(t)
	r.SetPermissionModePicker(ws, &frontendv1.TopbarPermissionModePicker{
		Options: []*frontendv1.TopbarPermissionModeOption{
			{Mode: "default"}, {Mode: "plan"}, {Mode: "accept_edits"},
		},
	})

	// Act.
	modes, ok := r.PermissionModes(ws)

	// Assert.
	if !ok {
		t.Fatal("PermissionModes reported no set after a picker was served")
	}
	want := []string{"default", "plan", "accept_edits"}
	if len(modes) != len(want) {
		t.Fatalf("PermissionModes = %v, want %v", modes, want)
	}
	for i := range want {
		if modes[i] != want[i] {
			t.Fatalf("PermissionModes = %v, want %v in the served order", modes, want)
		}
	}
}
