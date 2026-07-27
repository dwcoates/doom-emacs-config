package main

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/server"
)

func TestWebappHandlerEmptyDirReturnsNil(t *testing.T) {
	if webappHandler("", func(string, ...any) {}) != nil {
		t.Fatal("expected nil handler when -webapp is empty")
	}
}

func TestHealthzRequiresExplicitReadiness(t *testing.T) {
	ready := &daemonReadiness{}
	h := healthzHandler(ready)

	// Before all listeners/dependencies are live, health must reject rather
	// than treating process existence as readiness.
	first := httptest.NewRecorder()
	h.ServeHTTP(first, httptest.NewRequest(http.MethodGet, "/healthz", nil))
	if first.Code != http.StatusServiceUnavailable {
		t.Fatalf("unready /healthz status=%d, want 503", first.Code)
	}

	ready.ready.Store(true)
	second := httptest.NewRecorder()
	h.ServeHTTP(second, httptest.NewRequest(http.MethodGet, "/healthz", nil))
	if second.Code != http.StatusNoContent {
		t.Fatalf("ready /healthz status=%d, want 204", second.Code)
	}
}

func TestWebappHandlerServesIndexWhenPresent(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "index.html"), []byte("<!doctype html>SPA"), 0o644); err != nil {
		t.Fatal(err)
	}
	warned := false
	h := webappHandler(dir, func(string, ...any) { warned = true })
	if warned {
		t.Fatal("did not expect a warning when index.html exists")
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusOK {
		t.Fatalf("got status %d, want 200", rec.Code)
	}
	if !strings.Contains(rec.Body.String(), "SPA") {
		t.Fatalf("body %q missing index.html content", rec.Body.String())
	}
}

func TestWebappHandlerDiagnosesMissingIndex(t *testing.T) {
	dir := t.TempDir() // exists, but no index.html
	warned := false
	h := webappHandler(dir, func(string, ...any) { warned = true })
	if !warned {
		t.Fatal("expected a startup warning when index.html is missing")
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusServiceUnavailable {
		t.Fatalf("got status %d, want 503", rec.Code)
	}
	if strings.Contains(rec.Body.String(), "404 page not found") {
		t.Fatal("must not serve the bare Go 404 body")
	}
	if !strings.Contains(rec.Body.String(), "webapp assets not found") {
		t.Fatalf("body %q missing the diagnostic message", rec.Body.String())
	}
}

func TestWebappHandlerSelfCorrectsWhenIndexAppears(t *testing.T) {
	dir := t.TempDir() // starts without index.html
	h := webappHandler(dir, func(string, ...any) {})
	// Assets get built after the daemon started.
	if err := os.WriteFile(filepath.Join(dir, "index.html"), []byte("<!doctype html>LATE"), 0o644); err != nil {
		t.Fatal(err)
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, "/", nil))
	if rec.Code != http.StatusOK {
		t.Fatalf("got status %d, want 200 after index.html appeared", rec.Code)
	}
	if !strings.Contains(rec.Body.String(), "LATE") {
		t.Fatalf("body %q missing late index.html content", rec.Body.String())
	}
}

func TestLaunchedBinaryMTimeMatchesExecutableStat(t *testing.T) {
	// Arrange — the running test binary IS an executable on disk, so
	// launchedBinaryMTime must report exactly its stat mtime.
	exe, err := os.Executable()
	if err != nil {
		t.Skipf("os.Executable unavailable in this environment: %v", err)
	}
	info, err := os.Stat(exe)
	if err != nil {
		t.Fatalf("stat %q: %v", exe, err)
	}
	// Act
	got := launchedBinaryMTime()
	// Assert
	if want := info.ModTime().Unix(); got != want {
		t.Fatalf("launchedBinaryMTime() = %d, want %d (mtime of %q)", got, want, exe)
	}
	if got <= 0 {
		t.Fatalf("launchedBinaryMTime() = %d, want a positive Unix mtime", got)
	}
}

func TestParseAccounts(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    []server.Account
		wantErr bool
	}{
		{
			name: "empty flag is an unconfigured roster, not an error",
			raw:  "",
			want: nil,
		},
		{
			name: "one pair",
			raw:  "work=/home/u/.claude-chesscom",
			want: []server.Account{{Label: "work", ConfigDir: "/home/u/.claude-chesscom"}},
		},
		{
			name: "empty dir names the CLI default root",
			raw:  "personal=",
			want: []server.Account{{Label: "personal", ConfigDir: ""}},
		},
		{
			name: "two pairs keep roster order",
			raw:  "personal=,work=/home/u/.claude-chesscom",
			want: []server.Account{
				{Label: "personal", ConfigDir: ""},
				{Label: "work", ConfigDir: "/home/u/.claude-chesscom"},
			},
		},
		{
			name:    "pair without an equals sign is malformed",
			raw:     "personal",
			wantErr: true,
		},
		{
			name:    "empty label is malformed",
			raw:     "=/home/u/.claude",
			wantErr: true,
		},
		{
			name:    "duplicate label is rejected",
			raw:     "work=/a,work=/b",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseAccounts(tt.raw)

			// Assert
			if tt.wantErr {
				if err == nil {
					t.Fatalf("parseAccounts(%q) = %v, want error", tt.raw, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("parseAccounts(%q): %v", tt.raw, err)
			}
			if len(got) != len(tt.want) {
				t.Fatalf("parseAccounts(%q) = %v, want %v", tt.raw, got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Errorf("account[%d] = %v, want %v", i, got[i], tt.want[i])
				}
			}
		})
	}
}
