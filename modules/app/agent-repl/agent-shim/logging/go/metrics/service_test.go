package metrics

import (
	"github.com/prometheus/client_golang/prometheus"

	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// recorded is every record a service surface wrote.
type recorded struct {
	mu   sync.Mutex
	recs [][2]string
}

func (r *recorded) record(level, message string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.recs = append(r.recs, [2]string{level, message})
}

func (r *recorded) has(level, text string) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, rec := range r.recs {
		if rec[0] == level && strings.Contains(rec[1], text) {
			return true
		}
	}
	return false
}

func TestOpenService(t *testing.T) {
	tests := []struct {
		name      string
		root      func(t *testing.T) string
		wantLevel string
		wantText  string
		wantErr   bool
		wantOpen  bool
	}{
		{name: "off without a root, at debug", root: func(*testing.T) string { return "" }, wantLevel: "debug", wantText: "the metrics surface is off flag=--metrics-root"},
		{name: "listening and advertised, at info", root: func(t *testing.T) string { return t.TempDir() }, wantLevel: "info", wantText: "the metrics surface is listening address=127.0.0.1:", wantOpen: true},
		{
			name: "a root that cannot hold targets is a boot error, at error",
			root: func(t *testing.T) string {
				root := filepath.Join(t.TempDir(), "root")
				if err := os.WriteFile(root, nil, 0o644); err != nil {
					t.Fatal(err)
				}
				return root
			},
			wantLevel: "error", wantText: "the metrics surface could not open root=", wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := &recorded{}

			// Act
			surface, err := OpenService(ProgramStore, tt.root(t), rec.record)
			t.Cleanup(func() { CloseService(surface, rec.record) })

			// Assert
			if tt.wantErr != (err != nil) || tt.wantOpen != (surface != nil) {
				t.Fatalf("OpenService = (%v, %v), want open %v, error %v", surface, err, tt.wantOpen, tt.wantErr)
			}
			if !rec.has(tt.wantLevel, tt.wantText) {
				t.Fatalf("records = %v, want %s %q", rec.recs, tt.wantLevel, tt.wantText)
			}
		})
	}
}

func TestOpenServiceRecordsALaterFailureAtError(t *testing.T) {
	// Arrange
	rec := &recorded{}
	var sink func(op string, err error)
	open := func(program, root string, onError func(op string, err error), _ ...prometheus.Collector) (*Server, error) {
		sink = onError
		return nil, nil
	}
	if _, err := openService(ProgramSidecar, "", rec.record, open); err != nil {
		t.Fatalf("openService: %v", err)
	}

	// Act
	sink("metrics.scrape", errors.New("the process table could not be read"))

	// Assert
	if !rec.has("error", "the metrics surface failed step=metrics.scrape: the process table could not be read") {
		t.Fatalf("records = %v, want the later failure at error", rec.recs)
	}
}

func TestCloseServiceOfAnOffSurfaceRecordsNothing(t *testing.T) {
	// Arrange
	rec := &recorded{}

	// Act
	CloseService(nil, rec.record)

	// Assert
	if len(rec.recs) != 0 {
		t.Fatalf("an off surface's close recorded %v", rec.recs)
	}
}

func TestOpenServiceHandsItsExtraCollectorsToTheSurface(t *testing.T) {
	// Arrange
	rec := &recorded{}
	extra := NewDBFiles("events", "/nowhere/events.db")
	var got []prometheus.Collector
	open := func(program, root string, onError func(op string, err error), collectors ...prometheus.Collector) (*Server, error) {
		got = collectors
		return nil, nil
	}

	// Act
	if _, err := openService(ProgramStore, "", rec.record, open, extra); err != nil {
		t.Fatalf("openService: %v", err)
	}

	// Assert
	if len(got) != 1 || got[0] != extra {
		t.Fatalf("collectors handed to the surface = %v, want the one extra", got)
	}
}
