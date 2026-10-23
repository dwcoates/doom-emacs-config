package server

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestWithWebhostCORS(t *testing.T) {
	tests := []struct {
		name        string
		method      string
		origin      string
		preflight   bool
		wantAllowed string
		wantStatus  int
		wantServed  bool
	}{
		{name: "the host's preflight from an earlier daemon's port is answered", method: http.MethodOptions, origin: "http://host.localhost:61048", preflight: true,
			wantAllowed: "http://host.localhost:61048", wantStatus: http.StatusNoContent},
		{name: "the host's call is served and names the host allowed", method: http.MethodPost, origin: "http://host.localhost:61048",
			wantAllowed: "http://host.localhost:61048", wantStatus: http.StatusOK, wantServed: true},
		{name: "a page's same-origin call passes through untouched", method: http.MethodPost, origin: "http://ws-abc.localhost:5000",
			wantStatus: http.StatusOK, wantServed: true},
		{name: "another site's preflight passes through untouched", method: http.MethodOptions, origin: "http://example.com", preflight: true,
			wantStatus: http.StatusOK, wantServed: true},
		{name: "a host origin on another scheme is not the host", method: http.MethodPost, origin: "https://host.localhost:61048",
			wantStatus: http.StatusOK, wantServed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			served := false
			h := withWebhostCORS(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				served = true
				w.WriteHeader(http.StatusOK)
			}))
			r := httptest.NewRequest(tt.method, "/agentrepl.v1.AgentRepl/WatchPage", nil)
			r.Header.Set("Origin", tt.origin)
			if tt.preflight {
				r.Header.Set("Access-Control-Request-Method", http.MethodPost)
				r.Header.Set("Access-Control-Request-Headers", "content-type,connect-protocol-version")
			}
			w := httptest.NewRecorder()

			// Act
			h.ServeHTTP(w, r)

			// Assert
			if got := w.Header().Get("Access-Control-Allow-Origin"); got != tt.wantAllowed {
				t.Fatalf("Allow-Origin = %q, want %q", got, tt.wantAllowed)
			}
			if w.Code != tt.wantStatus || served != tt.wantServed {
				t.Fatalf("status %d served %v, want %d served %v", w.Code, served, tt.wantStatus, tt.wantServed)
			}
			if tt.preflight && tt.wantAllowed != "" && w.Header().Get("Access-Control-Allow-Headers") != "content-type,connect-protocol-version" {
				t.Fatalf("Allow-Headers = %q, want the requested headers", w.Header().Get("Access-Control-Allow-Headers"))
			}
			if !tt.preflight && tt.wantAllowed != "" && w.Header().Get("Access-Control-Expose-Headers") != connectExposedHeaders {
				t.Fatalf("Expose-Headers = %q", w.Header().Get("Access-Control-Expose-Headers"))
			}
		})
	}
}
