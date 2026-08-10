package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// awaitmatchers_test.go — the two shared await predicates, and the guard that
// keeps their call sites sharing them.

// THE PERMISSION MATCHER CAPTURES, which is the part a hand-rolled closure gets
// wrong: an await that matches without storing the uuid returns "" and the
// answer that follows is aimed at nothing.
func TestPendingPermissionMatcherCapturesTheUUID(t *testing.T) {
	// Arrange.
	var id string
	match := pendingPermissionMatcher("/ws", &id)

	// Act.
	matched := match(permissionFrame(t, "/ws", "perm-uuid"))

	// Assert.
	if !matched || id != "perm-uuid" {
		t.Fatalf("match = %v id = %q, want the pending permission matched and its uuid captured", matched, id)
	}
}

// A FRAME FOR ANOTHER WORKSPACE IS NOT THIS AWAIT'S. Matching one would settle
// an await on evidence about a session the test is not driving.
func TestPendingPermissionMatcherIgnoresAnotherWorkspace(t *testing.T) {
	// Arrange.
	var id string
	match := pendingPermissionMatcher("/ws", &id)

	// Act.
	matched := match(permissionFrame(t, "/other", "perm-uuid"))

	// Assert.
	if matched || id != "" {
		t.Fatalf("match = %v id = %q, want a frame for another workspace ignored", matched, id)
	}
}

// THE SETTLE MATCHER IS THE OPERATIONAL EDGE and nothing weaker: a workspace
// still connecting has not completed the reattach handshake, and an await that
// accepted it would resume against a successor mid-bring-up.
func TestOperationalMatcherAcceptsOnlyOperational(t *testing.T) {
	tests := []struct {
		name         string
		connectivity frontendv1.SessionConnectivity
		want         bool
	}{
		{name: "operational settles", connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL, want: true},
		{name: "connecting does not", connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_CONNECTING},
		{name: "hibernated does not", connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			_, match := operationalMatcher("/ws")

			// Act.
			got := match(workspaceStateFrame("/ws", tc.connectivity))

			// Assert.
			if got != tc.want {
				t.Fatalf("match(%s) = %v, want %v", tc.connectivity, got, tc.want)
			}
		})
	}
}

// THE CALL SITES ACTUALLY SHARE IT. A hand-rolled copy of either predicate is
// a second definition of what "settled" or "a question is pending" means, and
// the two drift the first time either is reworded — so a new await that spells
// its own must fail here rather than pass silently.
func TestAwaitPredicatesHaveExactlyOneDefinition(t *testing.T) {
	tests := []struct {
		name    string
		needle  string
		defined string
	}{
		{
			name:    "the operational settle edge",
			needle:  "SESSION_CONNECTIVITY_OPERATIONAL",
			defined: "isOperationalState",
		},
		{
			name:    "the pending-permission capture",
			needle:  "pendingPermissionIn(frame, workspace)",
			defined: "pendingPermissionMatcher",
		},
	}
	sources := packageSources(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act — every file in the package that spells the predicate.
			var spelled []string
			for path, body := range sources {
				if strings.Contains(body, tc.needle) {
					spelled = append(spelled, filepath.Base(path))
				}
			}

			// Assert — only this file (the definition and these tests).
			for _, name := range spelled {
				if name != "awaitmatchers_test.go" && name != "bounceresilienceharness_test.go" {
					t.Fatalf("%s spells %q itself; it must take %s instead, or the two definitions of what this means drift the first time either is reworded",
						name, tc.needle, tc.defined)
				}
			}
			if len(spelled) == 0 {
				t.Fatalf("no file spells %q at all; the guard is watching a predicate that no longer exists", tc.needle)
			}
		})
	}
}

// permissionFrame is one ConversationDelta carrying a pending permission item
// for workspace, which is the exact shape the daemon pushes for a blocked
// canUseTool round-trip.
func permissionFrame(t *testing.T, workspace, uuid string) *frontendv1.FrontendFrame {
	t.Helper()
	return &frontendv1.FrontendFrame{
		Frame: &frontendv1.FrontendFrame_ConversationDelta{
			ConversationDelta: &frontendv1.ConversationDelta{
				Workspace: workspace,
				Items: []*frontendv1.ConversationItem{{
					Uuid: uuid,
					Item: &frontendv1.ConversationItem_Permission{
						Permission: &corev1.PermissionItem{Resolution: corev1.PermissionItem_RESOLUTION_PENDING},
					},
				}},
			},
		},
	}
}

// workspaceStateFrame is one WorkspaceState frame at a given connectivity.
func workspaceStateFrame(workspace string, connectivity frontendv1.SessionConnectivity) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{
		Frame: &frontendv1.FrontendFrame_WorkspaceState{
			WorkspaceState: &frontendv1.WorkspaceState{Workspace: workspace, Connectivity: connectivity},
		},
	}
}

// packageSources reads every Go file in this package, keyed by path.
func packageSources(t *testing.T) map[string]string {
	t.Helper()
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the e2e package directory: %v", err)
	}
	out := map[string]string{}
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") {
			continue
		}
		body, err := os.ReadFile(entry.Name())
		if err != nil {
			t.Fatalf("read %s: %v", entry.Name(), err)
		}
		out[entry.Name()] = string(body)
	}
	return out
}
