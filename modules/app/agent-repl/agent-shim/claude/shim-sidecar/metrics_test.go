package main

import (
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
			logf, durable := sidecarPprofLogger()
			root := tt.root(t)

			// Act
			surface, err := openMetricsSurface(root, logf)
			t.Cleanup(func() { closeMetricsSurface(surface, logf) })

			// Assert
			if tt.wantErr != (err != nil) {
				t.Fatalf("err = %v, want error = %v", err, tt.wantErr)
			}
			if tt.wantOpen != (surface != nil) {
				t.Fatalf("surface open = %v, want %v", surface != nil, tt.wantOpen)
			}
			if tt.wantOpen {
				if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramSidecar)); statErr != nil {
					t.Fatalf("no sidecar target advertised: %v", statErr)
				}
			}
			var found bool
			for _, r := range decodeSidecarPprofRecords(t, durable) {
				if r.Operation == "metrics" && r.Level == tt.wantLevel && strings.Contains(r.Message, tt.wantText) {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want a metrics %s record containing %q", decodeSidecarPprofRecords(t, durable), tt.wantLevel, tt.wantText)
			}
		})
	}
}
