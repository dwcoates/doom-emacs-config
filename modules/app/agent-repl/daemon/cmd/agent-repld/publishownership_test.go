package main

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/runtimemetrics"
	"agent-repld/internal/server"

	"agentrepl/logging/metrics"
)

// recordingClaim is a claim whose Publish succeeds or fails on demand, and
// which answers Departed as told.
type recordingClaim struct {
	err       error
	departed  bool
	published int
}

func (c *recordingClaim) Publish() error {
	c.published++
	return c.err
}

func (c *recordingClaim) Departed() bool { return c.departed }

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
			err := publishOwnership(claim, surface, nil)

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

// TestADepartedDaemonPublishesNoOwnership pins the incident of 2026-11-22: a
// successor that had begun standing down took the shared listener and
// published daemon.addr once its adoption finished. A departed claim takes no
// shared listener, writes no daemon.addr and no metrics target, and answers
// ErrDeparted.
func TestADepartedDaemonPublishesNoOwnership(t *testing.T) {
	// Arrange
	root := t.TempDir()
	surface := openOwnershipSurface(t, root)
	claim := &recordingClaim{departed: true}
	var touched []string
	shared := &sharedTake{holder: "/h/daemon-holder.sock", accept: newPrivateAccept(t), claim: &advertisingClaim{}, log: &spyLogger{},
		ensure: func(context.Context, string) error {
			touched = append(touched, "ensure")
			return nil
		},
		takeFn: func(context.Context, string, string) (net.Listener, error) {
			touched = append(touched, "take")
			return nil, errors.New("a departed daemon took the shared listener")
		},
		subscribeFn: func(context.Context, string, string) (server.Returned, error) {
			touched = append(touched, "subscribe")
			return nil, errors.New("a departed daemon opened the returned stream")
		}}

	// Act
	err := publishOwnership(claim, surface, shared)

	// Assert
	if !errors.Is(err, daemonaddr.ErrDeparted) {
		t.Fatalf("publishOwnership = %v, want ErrDeparted", err)
	}
	if len(touched) != 0 {
		t.Fatalf("the shared take ran %v, want nothing taken", touched)
	}
	if shared.address() != "" {
		t.Fatalf("shared address = %q, want none taken", shared.address())
	}
	if claim.published != 0 {
		t.Fatalf("daemon.addr published %d times, want never", claim.published)
	}
	if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramDaemon)); !os.IsNotExist(statErr) {
		t.Fatalf("metrics target advertised by a departed daemon (stat err = %v)", statErr)
	}
}
