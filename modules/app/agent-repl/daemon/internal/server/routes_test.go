package server

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestEveryRouteIsCoveredByAnAPIPrefix is the regression guard for a routing
// bug that shipped: the process mux mounted a stale "/workspaces/" prefix whose
// route had been deleted, and never mounted "/workspace-command". Because the
// webapp SPA is mounted at "/", the unmounted route did not 404 from the API
// mux — it fell through to the static file server, so every webapp sidebar
// gesture failed with a file-server 404 while the handler sat unreachable.
//
// A route the process cannot route to is a dead route, so assert the coverage.
func TestEveryRouteIsCoveredByAnAPIPrefix(t *testing.T) {
	h := newHarness(t)
	for _, rt := range h.srv.routes() {
		path := rt.pattern
		if _, rest, found := strings.Cut(rt.pattern, " "); found {
			path = rest
		}
		if !coveredByAPIPrefix(path) {
			t.Errorf("route %q is not covered by any server.APIPrefixes entry — the process mux would leak it to the webapp file server", rt.pattern)
		}
	}
}

// TestAPIPrefixesAreAllUsed is the other half of the guard: a prefix naming no
// route is the stale "/workspaces/" mount, which is how the gap hid.
func TestAPIPrefixesAreAllUsed(t *testing.T) {
	h := newHarness(t)
	for _, prefix := range APIPrefixes {
		used := false
		for _, rt := range h.srv.routes() {
			path := rt.pattern
			if _, rest, found := strings.Cut(rt.pattern, " "); found {
				path = rest
			}
			if prefixMatches(prefix, path) {
				used = true
				break
			}
		}
		if !used {
			t.Errorf("APIPrefixes entry %q matches no route — a stale mount", prefix)
		}
	}
}

// TestWorkspaceCommandIsReachableThroughTheAPIPrefixes pins the specific route
// the bug hid, through a mux assembled the way the process assembles it.
func TestWorkspaceCommandIsReachableThroughTheAPIPrefixes(t *testing.T) {
	// Arrange — the process mux: the API at its prefixes, an SPA stand-in at
	// "/" that fails the test if it is ever reached by an API call.
	h := newHarness(t)
	mux := http.NewServeMux()
	for _, prefix := range APIPrefixes {
		mux.Handle(prefix, h.srv.Handler())
	}
	mux.Handle("/", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		t.Errorf("%s %s fell through to the SPA handler", r.Method, r.URL.Path)
		http.NotFound(w, r)
	}))
	// Act
	rec := httptest.NewRecorder()
	mux.ServeHTTP(rec, httptest.NewRequest(http.MethodPost, "/workspace-command", strings.NewReader(`[]`)))
	// Assert — the real handler answered (any status but the file server's).
	if rec.Code == http.StatusNotFound {
		t.Fatalf("POST /workspace-command = 404, want the real handler to answer")
	}
}

func coveredByAPIPrefix(path string) bool {
	for _, prefix := range APIPrefixes {
		if prefixMatches(prefix, path) {
			return true
		}
	}
	return false
}

// prefixMatches mirrors net/http.ServeMux subtree semantics: a prefix ending in
// "/" covers everything beneath it, otherwise it must match exactly.
func prefixMatches(prefix, path string) bool {
	if strings.HasSuffix(prefix, "/") {
		return strings.HasPrefix(path, prefix)
	}
	return path == prefix
}
