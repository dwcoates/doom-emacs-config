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

// TestATakeAfterTheSharedStopPublishesNothing pins the other ordering of a
// departure racing publishOwnership: the claim had not departed at the check,
// but the shutdown's StopShared ran before the take's add. The stopped accept
// refuses the add, so the daemon advertises nothing, ends the stream it
// opened, and answers ErrDeparted for the rollout's one standing-down record.
// Only a stream that cannot be ended is an ERROR of the take's own.
func TestATakeAfterTheSharedStopPublishesNothing(t *testing.T) {
	endErr := errors.New("sockethold: end the accept stream of \"daemon\": write: bad file descriptor")
	cases := []struct {
		name      string
		stopErr   error
		wantError string
	}{
		{name: "the stream it opened is ended, with no ERROR"},
		{name: "a stream that cannot be ended is an ERROR", stopErr: endErr,
			wantError: "the holder's stream of handed-back connections could not be ended after the stopped shared listener refused its add"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			surface := openOwnershipSurface(t, root)
			claim := &recordingClaim{}
			accept := newPrivateAccept(t)
			if err := accept.StopShared(); err != nil {
				t.Fatalf("StopShared: %v", err)
			}
			listener, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { listener.Close() })
			stream := newFakeReturned()
			var returned server.Returned = stream
			if tc.stopErr != nil {
				returned = stopFailingReturned{fakeReturned: stream, err: tc.stopErr}
			}
			advertised := &advertisingClaim{}
			log := &spyLogger{}
			shared := &sharedTake{holder: "/h/daemon-holder.sock", accept: accept, claim: advertised, log: log, ensure: holderServes,
				takeFn: func(context.Context, string, string) (net.Listener, error) { return listener, nil },
				subscribeFn: func(context.Context, string, string) (server.Returned, error) {
					return returned, nil
				}}

			// Act
			err = publishOwnership(claim, surface, shared)

			// Assert
			if !errors.Is(err, daemonaddr.ErrDeparted) || !errors.Is(err, server.ErrSharedStopped) {
				t.Fatalf("publishOwnership = %v, want ErrDeparted carrying ErrSharedStopped", err)
			}
			if len(advertised.advertised) != 0 {
				t.Fatalf("advertised %v, want nothing", advertised.advertised)
			}
			if claim.published != 0 {
				t.Fatalf("daemon.addr published %d times, want never", claim.published)
			}
			if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramDaemon)); !os.IsNotExist(statErr) {
				t.Fatalf("metrics target advertised (stat err = %v)", statErr)
			}
			select {
			case <-stream.stop:
			default:
				t.Fatal("the holder's stream was left open, want it released")
			}
			msg, ok := log.message(dlog.LevelError)
			if tc.wantError == "" && ok {
				t.Fatalf("ERROR %q, want none for a standing-down refusal", msg)
			}
			if tc.wantError != "" && msg != tc.wantError {
				t.Fatalf("ERROR = %q (present %v), want %q", msg, ok, tc.wantError)
			}
		})
	}
}
