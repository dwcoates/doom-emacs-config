package topbar

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// loginSession is a session a login began, with some traffic.
func loginSession() *frontendv1.TopbarAgentReplSession {
	return &frontendv1.TopbarAgentReplSession{
		StartedAtMs:   1_791_270_000_000,
		Began:         &frontendv1.TopbarAgentReplSession_Login{Login: &frontendv1.TopbarSessionBeganLogin{}},
		BytesReceived: 412 << 20,
		BytesSent:     38 << 20,
	}
}

func TestTheConnectivityIndicatorCarriesNoSessionBeforeOneBegan(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.ready(t)

	// Assert.
	view, ok := h.r.Topic(testWS).Latest()
	if !ok {
		t.Fatal("no topbar was published")
	}
	if view.GetConnectivity().GetSession() != nil {
		t.Fatalf("session = %v, want absent", view.GetConnectivity().GetSession())
	}
}

func TestTheSessionStandsOnEveryStrip(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, h *harness, set func())
	}{
		{"strips that stood before it was set", func(t *testing.T, h *harness, set func()) {
			h.ready(t)
			readyOther(t, h)
			set()
		}},
		{"a strip made after it was set", func(t *testing.T, h *harness, set func()) {
			h.ready(t)
			set()
			readyOther(t, h)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			tc.setup(t, h, func() { h.r.SetAgentReplSession(loginSession()) })

			// Assert.
			for _, ws := range []ids.WorkspaceID{testWS, otherWS} {
				view, ok := h.r.Topic(ws).Latest()
				if !ok {
					t.Fatalf("no topbar for %s", ws)
				}
				got := view.GetConnectivity().GetSession()
				if got.GetLogin() == nil || got.GetBytesReceived() != 412<<20 || got.GetBytesSent() != 38<<20 {
					t.Fatalf("%s session = %v, want the login session with its traffic", ws, got)
				}
			}
		})
	}
}

func TestANewerSessionReplacesTheOneOnTheStrip(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetAgentReplSession(loginSession())
	newer := &frontendv1.TopbarAgentReplSession{
		StartedAtMs: 1_791_280_000_000,
		Began:       &frontendv1.TopbarAgentReplSession_EditorStart{EditorStart: &frontendv1.TopbarSessionBeganEditorStart{}},
	}

	// Act.
	h.r.SetAgentReplSession(newer)

	// Assert.
	view, _ := h.r.Topic(testWS).Latest()
	got := view.GetConnectivity().GetSession()
	if got.GetEditorStart() == nil || got.GetStartedAtMs() != 1_791_280_000_000 || got.GetBytesReceived() != 0 {
		t.Fatalf("session = %v, want the editor-start session", got)
	}
}
