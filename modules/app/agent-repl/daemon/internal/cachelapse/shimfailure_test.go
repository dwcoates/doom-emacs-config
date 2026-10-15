package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/shimclient"
)

func TestShimCallFailedTriagesTheFailure(t *testing.T) {
	words := shimFailure{doing: "doing the call", died: "the shim died", failed: "the call failed"}
	cancelled, cancel := context.WithCancel(context.Background())
	cancel()
	tests := []struct {
		name    string
		ctx     context.Context
		err     error
		level   string
		message string
	}{
		{name: "a shim that died under the call", ctx: context.Background(),
			err: fmt.Errorf("%w: unexpected EOF", shimclient.ErrShimDied), level: "info", message: "the shim died"},
		{name: "a call the daemon's exit cancelled", ctx: cancelled,
			err: context.Canceled, level: "info", message: "the daemon is leaving; doing the call ended with it"},
		{name: "any other failure", ctx: context.Background(),
			err: errors.New("connection refused"), level: "error", message: "the call failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			surfaces := dlog.NewTestSurfaces()

			// Act
			shimCallFailed(tt.ctx, surfaces.Global(), "op.test", words, dlog.Context{"workspace": "w1"}, tt.err)

			// Assert
			rec, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: tt.level, Operation: "op.test", Message: tt.message})
			if !ok || rec.Context["cause"] != tt.err.Error() {
				t.Fatalf("%s record %q = %+v, %v; want one carrying the cause", tt.level, tt.message, rec, ok)
			}
			if len(surfaces.Records()) != 1 {
				t.Fatalf("records = %v, want exactly one", surfaces.Records())
			}
		})
	}
}

func TestShimCallFailedKeepsTheCallersFields(t *testing.T) {
	// Arrange
	surfaces := dlog.NewTestSurfaces()

	// Act
	shimCallFailed(context.Background(), surfaces.Global(), "op.test",
		shimFailure{doing: "d", died: "died", failed: "failed"}, dlog.Context{"cadence": "hold"}, errors.New("boom"))

	// Assert
	rec, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: "error", Operation: "op.test"})
	if !ok || rec.Context["cadence"] != "hold" {
		t.Fatalf("error record = %+v, %v; want the caller's fields kept", rec, ok)
	}
}

// TestShimCallFailureTriageHasOneHome fails when a source file in this
// package triages a dead shim by hand instead of through shimCallFailed.
func TestShimCallFailureTriageHasOneHome(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var sites []string
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		for range strings.Count(string(src), "shimclient.ErrShimDied") {
			sites = append(sites, file)
		}
	}

	// Assert
	if len(sites) != 1 || sites[0] != "keeper.go" {
		t.Fatalf("shimclient.ErrShimDied is triaged at %v, want only shimCallFailed in keeper.go", sites)
	}
}
