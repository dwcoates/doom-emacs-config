package frontend

import (
	"errors"
	"net/http"
	"net/url"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// knownOnly builds a WorkspaceKnown admitting exactly the listed workspaces.
func knownOnly(workspaces ...string) WorkspaceKnown {
	set := make(map[string]struct{}, len(workspaces))
	for _, w := range workspaces {
		set[w] = struct{}{}
	}
	return func(workspace string) (bool, error) {
		_, ok := set[workspace]
		return ok, nil
	}
}

func TestWorkspaceScopeFromQueryAdmitsKnownWorkspace(t *testing.T) {
	// Arrange — the wire form is percent-encoded, so the paths that exercise
	// the decode are the interesting ones: spaces and non-ASCII.
	cases := []struct {
		name string
		path string
	}{
		{"plain", "/Users/dev/proj"},
		{"spaces", "/Users/dev/My Projects/agent repl"},
		{"non-ascii", "/Users/dev/prosjekt/æøå/日本語"},
		{"reserved characters", "/Users/dev/a&b=c?d#e/+plus"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			raw := url.Values{"workspace": {c.path}}.Encode()

			// Act.
			scope, err := WorkspaceScopeFromQuery(raw, knownOnly(c.path))

			// Assert.
			if err != nil {
				t.Fatalf("WorkspaceScopeFromQuery(%q) errored: %v", raw, err)
			}
			if scope != (Scope{Workspace: c.path}) {
				t.Fatalf("scope = %+v, want Scope{Workspace: %q}", scope, c.path)
			}
		})
	}
}

func TestWorkspaceScopeFromQueryRefuses(t *testing.T) {
	// Arrange — every refusal must be typed, must name its reason, and must
	// leave the Scope zero so no caller can serve a widened connection off it.
	boom := errors.New("registry unavailable")
	cases := []struct {
		name       string
		rawQuery   string
		known      WorkspaceKnown
		wantReason ScopeRefusalReason
		wantStatus int
	}{
		{
			name:       "undecodable percent escape",
			rawQuery:   "workspace=%zz",
			known:      knownOnly("/w"),
			wantReason: ScopeRefusalMalformedQuery,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "no workspace key",
			rawQuery:   "composer=0",
			known:      knownOnly("/w"),
			wantReason: ScopeRefusalMissingWorkspace,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "empty workspace value",
			rawQuery:   "workspace=",
			known:      knownOnly("/w"),
			wantReason: ScopeRefusalMissingWorkspace,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "empty query",
			rawQuery:   "",
			known:      knownOnly("/w"),
			wantReason: ScopeRefusalMissingWorkspace,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "repeated workspace key",
			rawQuery:   "workspace=%2Fw&workspace=%2Fother",
			known:      knownOnly("/w", "/other"),
			wantReason: ScopeRefusalRepeatedWorkspace,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "relative path",
			rawQuery:   "workspace=proj%2Fsub",
			known:      knownOnly("proj/sub"),
			wantReason: ScopeRefusalRelativeWorkspace,
			wantStatus: http.StatusBadRequest,
		},
		{
			name:       "workspace the daemon has no state for",
			rawQuery:   "workspace=%2Fnot%2Fhere",
			known:      knownOnly("/w"),
			wantReason: ScopeRefusalUnknownWorkspace,
			wantStatus: http.StatusNotFound,
		},
		{
			name:     "lookup failure is not an unknown workspace",
			rawQuery: "workspace=%2Fw",
			known: func(string) (bool, error) {
				return false, boom
			},
			wantReason: ScopeRefusalWorkspaceLookup,
			wantStatus: http.StatusInternalServerError,
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			// Act.
			scope, err := WorkspaceScopeFromQuery(c.rawQuery, c.known)

			// Assert.
			if scope != (Scope{}) {
				t.Fatalf("a refused query yielded scope %+v; a non-zero scope would be served", scope)
			}
			var refusal *ScopeRefusal
			if !errors.As(err, &refusal) {
				t.Fatalf("error = %v (%T), want a *ScopeRefusal", err, err)
			}
			if refusal.Reason != c.wantReason {
				t.Fatalf("reason = %q, want %q (message: %v)", refusal.Reason, c.wantReason, err)
			}
			if refusal.HTTPStatus() != c.wantStatus {
				t.Fatalf("HTTPStatus = %d, want %d", refusal.HTTPStatus(), c.wantStatus)
			}
		})
	}
	// Assert — the lookup failure keeps the underlying cause reachable rather
	// than flattening it into prose.
	_, err := WorkspaceScopeFromQuery("workspace=%2Fw", func(string) (bool, error) { return false, boom })
	if !errors.Is(err, boom) {
		t.Fatalf("lookup failure discarded its cause: %v", err)
	}
}

func TestWorkspaceScopeFromQueryPanicsWithoutAuthority(t *testing.T) {
	// Arrange / Assert — a nil authority cannot check anything, so admitting
	// the connection would scope it to an unchecked string.
	defer func() {
		if recover() == nil {
			t.Fatal("WorkspaceScopeFromQuery admitted a nil WorkspaceKnown")
		}
	}()

	// Act.
	_, _ = WorkspaceScopeFromQuery("workspace=%2Fw", nil)
}

func TestWorkspaceScopedConnectionSeesOnlyItsOwnWorkspace(t *testing.T) {
	// Arrange — the Scope a workspace-addressed query produces carries NO
	// session id, so this pins that the frame filter still partitions by
	// workspace with that half of the key absent.
	scope, err := WorkspaceScopeFromQuery(url.Values{"workspace": {"/w"}}.Encode(), knownOnly("/w"))
	if err != nil {
		t.Fatalf("WorkspaceScopeFromQuery errored: %v", err)
	}
	cases := []struct {
		name  string
		frame *frontendv1.FrontendFrame
		want  bool
	}{
		{"own workspace state", WorkspaceStateFrame(&frontendv1.WorkspaceState{SessionId: "s1", Workspace: "/w"}), true},
		{"other workspace state", WorkspaceStateFrame(&frontendv1.WorkspaceState{SessionId: "s2", Workspace: "/other"}), false},
		{"own session view", SessionViewFrame(&frontendv1.SessionView{SessionId: "s1", Workspace: "/w"}), true},
		{"other session view", SessionViewFrame(&frontendv1.SessionView{SessionId: "s2", Workspace: "/other"}), false},
		{"own conversation delta", ConversationDeltaFrame(&frontendv1.ConversationDelta{Fence: "vendor", Workspace: "/w"}), true},
		{"other conversation delta", ConversationDeltaFrame(&frontendv1.ConversationDelta{Fence: "vendor", Workspace: "/other"}), false},
		{"own progress", ProgressViewFrame(&frontendv1.ProgressView{Fence: "s1", Workspace: "/w"}), true},
		{"other progress", ProgressViewFrame(&frontendv1.ProgressView{Fence: "s2", Workspace: "/other"}), false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			// Act.
			_, keep := scopeFrame(c.frame, scope)

			// Assert.
			if keep != c.want {
				t.Fatalf("scopeFrame kept = %v, want %v", keep, c.want)
			}
		})
	}
}

func TestWorkspaceScopedSnapshotCarriesOnlyItsOwnWorkspace(t *testing.T) {
	// Arrange — the connect/resync snapshot is the other delivery path, and a
	// session-less scope must filter it just as tightly as the frame stream.
	scope, err := WorkspaceScopeFromQuery(url.Values{"workspace": {"/w"}}.Encode(), knownOnly("/w"))
	if err != nil {
		t.Fatalf("WorkspaceScopeFromQuery errored: %v", err)
	}
	snap := &frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{
			{SessionId: "s1", Workspace: "/w"},
			{SessionId: "s2", Workspace: "/other"},
		},
		Sessions: []*frontendv1.SessionView{
			{SessionId: "s1", Workspace: "/w"},
			{SessionId: "s2", Workspace: "/other"},
		},
	}

	// Act.
	got := filterSnapshot(snap, scope)

	// Assert.
	if len(got.GetWorkspaces()) != 1 || got.GetWorkspaces()[0].GetWorkspace() != "/w" {
		t.Fatalf("workspaces = %v, want only /w", got.GetWorkspaces())
	}
	if len(got.GetSessions()) != 1 || got.GetSessions()[0].GetWorkspace() != "/w" {
		t.Fatalf("sessions = %v, want only /w", got.GetSessions())
	}
}
