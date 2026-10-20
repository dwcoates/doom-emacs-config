package main

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/logging/metrics"
)

func TestOpenMetricsSurface(t *testing.T) {
	tests := []struct {
		name      string
		root      func(t *testing.T) string
		wantLevel string
		wantText  string
		wantErr   bool
		wantOpen  bool
	}{
		{name: "off without a root", root: func(*testing.T) string { return "" }, wantLevel: "debug", wantText: "the metrics surface is off"},
		{name: "listening and advertised", root: func(t *testing.T) string { return t.TempDir() }, wantLevel: "info", wantText: "the metrics surface is listening", wantOpen: true},
		{
			name: "a root that cannot hold targets is a boot error",
			root: func(t *testing.T) string {
				root := filepath.Join(t.TempDir(), "root")
				if err := os.WriteFile(root, nil, 0o644); err != nil {
					t.Fatal(err)
				}
				return root
			},
			wantLevel: "error", wantText: "the metrics surface could not open", wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, durable := storePprofLogger()
			root := tt.root(t)

			// Act
			surface, err := openMetricsSurface(root, filepath.Join(t.TempDir(), "events.db"), log)
			t.Cleanup(func() { closeMetricsSurface(surface, log) })

			// Assert
			if tt.wantErr != (err != nil) {
				t.Fatalf("err = %v, want error = %v", err, tt.wantErr)
			}
			if tt.wantOpen != (surface != nil) {
				t.Fatalf("surface open = %v, want %v", surface != nil, tt.wantOpen)
			}
			if tt.wantOpen {
				if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramStore)); statErr != nil {
					t.Fatalf("no store target advertised: %v", statErr)
				}
			}
			var found bool
			for _, r := range decodeStoreRecords(t, durable) {
				if r.Operation == "store.metrics" && r.Level == tt.wantLevel && strings.Contains(r.Message, tt.wantText) {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want a store.metrics %s record containing %q", decodeStoreRecords(t, durable), tt.wantLevel, tt.wantText)
			}
		})
	}
}

func TestTheStoreSurfaceServesItsDatabaseSize(t *testing.T) {
	// Arrange
	log, _ := storePprofLogger()
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.WriteFile(dbPath, make([]byte, 2048), 0o644); err != nil {
		t.Fatal(err)
	}
	surface, err := openMetricsSurface(t.TempDir(), dbPath, log)
	if err != nil {
		t.Fatalf("openMetricsSurface: %v", err)
	}
	t.Cleanup(func() { closeMetricsSurface(surface, log) })

	// Act
	resp, err := http.Get("http://" + surface.Addr() + metrics.Path)
	if err != nil {
		t.Fatalf("scrape: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)

	// Assert
	if !strings.Contains(string(body), `agentrepl_db_bytes{db="events",file="db"} 2048`) {
		t.Fatalf("scrape lacks the database size:\n%s", body)
	}
}
