package main

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/runtimemetrics"

	"agentrepl/logging/metrics"
)

// recordingClaim is a claim whose Publish succeeds or fails on demand.
type recordingClaim struct {
	err       error
	published int
}

func (c *recordingClaim) Publish() error {
	c.published++
	return c.err
}

func openOwnershipSurface(t *testing.T, root string) *runtimemetrics.Surface {
	t.Helper()
	s, err := runtimemetrics.Open(dlog.NewTestLogger(), root)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	return s
}

func TestPublishOwnership(t *testing.T) {
	tests := []struct {
		name       string
		publishErr error
		blockRoot  bool
		metricsOff bool
		wantErr    string
		wantTarget bool
	}{
		{name: "both advertisements are written", wantTarget: true},
		{name: "a failed daemon.addr writes no metrics target", publishErr: errors.New("disk full"), wantErr: "publish daemon.addr: disk full"},
		{name: "a failed target is the caller's error", blockRoot: true, wantErr: "advertise the metrics target"},
		{name: "an off metrics surface publishes only daemon.addr", metricsOff: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			claim := &recordingClaim{err: tt.publishErr}
			var surface *runtimemetrics.Surface
			if !tt.metricsOff {
				surface = openOwnershipSurface(t, root)
			}
			if tt.blockRoot {
				if err := os.WriteFile(filepath.Join(root, metrics.TargetsDir), nil, 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := publishOwnership(claim, surface)

			// Assert
			if tt.wantErr == "" && err != nil {
				t.Fatalf("publishOwnership: %v", err)
			}
			if tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
			}
			if claim.published != 1 {
				t.Fatalf("daemon.addr published %d times, want once", claim.published)
			}
			_, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramDaemon))
			if tt.wantTarget != (statErr == nil) {
				t.Fatalf("target present = %v, want %v", statErr == nil, tt.wantTarget)
			}
		})
	}
}
