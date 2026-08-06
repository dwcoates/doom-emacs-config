package server

import (
	"net/http"
	"net/url"
	"strings"
	"testing"

	"claude-repld/internal/registry"

	"github.com/gorilla/websocket"
)

// workspaceStreamURL builds the endpoint's URL against base with workspace
// percent-encoded exactly as a browser would send it.
func workspaceStreamURL(base, workspace string) string {
	return base + "/workspace-stream?" + url.Values{"workspace": {workspace}}.Encode()
}

func TestWorkspaceStreamServesAKnownWorkspace(t *testing.T) {
	// Arrange — a workspace the daemon holds a live record for. The paths
	// exercise the percent-decode as well as the routing.
	cases := []struct {
		name      string
		workspace string
	}{
		{"plain", "/tmp/proj"},
		{"spaces", "/tmp/My Projects/agent repl"},
		{"non-ascii", "/tmp/prosjekt/æøå/日本語"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			h := newHarness(t)
			if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: c.workspace}); err != nil {
				t.Fatalf("put: %v", err)
			}

			// Act.
			wsURL := "ws" + strings.TrimPrefix(workspaceStreamURL(h.ts.URL, c.workspace), "http")
			conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
			if err != nil {
				status := 0
				if resp != nil {
					status = resp.StatusCode
				}
				t.Fatalf("ws dial: %v (status %d)", err, status)
			}
			defer conn.Close()

			// Assert — the connection is served, and the first frame is the
			// scope-filtered connect snapshot.
			var frame map[string]any
			if err := conn.ReadJSON(&frame); err != nil {
				t.Fatalf("read first frame: %v", err)
			}
			if _, ok := frame["snapshot"]; !ok {
				t.Fatalf("first frame = %v, want a snapshot", frame)
			}
		})
	}
}

func TestWorkspaceStreamRefusesRatherThanServingUnscoped(t *testing.T) {
	// Arrange — every query that does not name a servable workspace must be
	// answered with a status, never upgraded: an unscoped socket would carry
	// every workspace's frames to one browser.
	h := newHarness(t)
	if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: "/tmp/proj"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := h.reg.Put(registry.Record{SessionID: "s_dead", CWD: "/tmp/gone", Terminal: true}); err != nil {
		t.Fatalf("put: %v", err)
	}
	cases := []struct {
		name       string
		query      string
		wantStatus int
	}{
		{"no workspace", "", http.StatusBadRequest},
		{"empty workspace", "workspace=", http.StatusBadRequest},
		{"undecodable escape", "workspace=%zz", http.StatusBadRequest},
		{"repeated workspace", "workspace=%2Ftmp%2Fproj&workspace=%2Ftmp%2Fother", http.StatusBadRequest},
		{"relative path", "workspace=proj", http.StatusBadRequest},
		{"unknown workspace", "workspace=%2Ftmp%2Fnowhere", http.StatusNotFound},
		{"workspace of a terminal session only", "workspace=%2Ftmp%2Fgone", http.StatusNotFound},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			// Act.
			wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/workspace-stream?" + c.query
			conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
			if err == nil {
				conn.Close()
				t.Fatal("the daemon upgraded a socket for a query naming no servable workspace")
			}

			// Assert.
			if resp == nil {
				t.Fatalf("dial failed without an HTTP response: %v", err)
			}
			if resp.StatusCode != c.wantStatus {
				t.Fatalf("status = %d, want %d", resp.StatusCode, c.wantStatus)
			}
		})
	}
}

func TestWorkspaceKnownUnionsRenderStateAndLiveRecords(t *testing.T) {
	// Arrange.
	cases := []struct {
		name      string
		record    *registry.Record
		workspace string
		want      bool
	}{
		{"live record", &registry.Record{SessionID: "s1", CWD: "/tmp/proj"}, "/tmp/proj", true},
		{"terminal record only", &registry.Record{SessionID: "s1", CWD: "/tmp/proj", Terminal: true}, "/tmp/proj", false},
		{"another workspace", &registry.Record{SessionID: "s1", CWD: "/tmp/proj"}, "/tmp/other", false},
		{"nothing at all", nil, "/tmp/proj", false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			h := newHarness(t)
			if c.record != nil {
				if err := h.reg.Put(*c.record); err != nil {
					t.Fatalf("put: %v", err)
				}
			}

			// Act.
			got, err := h.srv.workspaceKnown(c.workspace)

			// Assert.
			if err != nil {
				t.Fatalf("workspaceKnown errored: %v", err)
			}
			if got != c.want {
				t.Fatalf("workspaceKnown(%q) = %v, want %v", c.workspace, got, c.want)
			}
		})
	}
}
