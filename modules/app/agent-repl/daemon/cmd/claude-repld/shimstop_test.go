package main

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
)

func levelRecords(log *dlog.TestLogger, level string) int {
	n := 0
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == "daemon.cmd.state_root" {
			n++
		}
	}
	return n
}

func TestStandDownShimsStopsTheFleetOnlyWhenTheRootIsLost(t *testing.T) {
	cases := []struct {
		name       string
		reason     standDownReason
		stopErr    error
		wantStop   bool
		wantInfos  int
		wantErrors int
	}{
		{name: "a state-root loss stops every shim", reason: standDownStateRootLost, wantStop: true, wantInfos: 1},
		{name: "a state-root loss reports a shim that would not stop", reason: standDownStateRootLost, stopErr: errors.New("kill: no reap"), wantStop: true, wantErrors: 1},
		{name: "an ordinary stand-down leaves the shims for adoption", reason: standDownOrderly},
		{name: "a handover leaves the shims for the successor", reason: standDownHandover},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			stopped := false
			stop := func(context.Context) (int, error) {
				stopped = true
				return 2, tc.stopErr
			}

			// Act
			standDownShims(context.Background(), tc.reason, stop, log)

			// Assert
			if stopped != tc.wantStop {
				t.Fatalf("fleet stop invoked = %v, want %v", stopped, tc.wantStop)
			}
			if got := levelRecords(log, dlog.LevelInfo); got != tc.wantInfos {
				t.Fatalf("INFO records = %d, want %d", got, tc.wantInfos)
			}
			if got := levelRecords(log, dlog.LevelError); got != tc.wantErrors {
				t.Fatalf("ERROR records = %d, want %d", got, tc.wantErrors)
			}
		})
	}
}

func TestStandDownShimsReportsAMissingStop(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()

	// Act
	standDownShims(context.Background(), standDownStateRootLost, nil, log)

	// Assert
	if got := levelRecords(log, dlog.LevelError); got != 1 {
		t.Fatalf("ERROR records = %d, want 1", got)
	}
}

type fakeStopFleet struct {
	held   []ids.WorkspaceID
	fail   map[ids.WorkspaceID]error
	killed []ids.WorkspaceID
	forced bool
}

func (f *fakeStopFleet) Workspaces() []ids.WorkspaceID { return f.held }

func (f *fakeStopFleet) KillSession(_ context.Context, ws ids.WorkspaceID, force bool) error {
	f.killed = append(f.killed, ws)
	f.forced = force
	return f.fail[ws]
}

type fakeStopSupervisor struct {
	shimclient.Supervisor
	calls    []string
	sweepErr error
}

func (s *fakeStopSupervisor) BeginStandDown() bool {
	s.calls = append(s.calls, "latch")
	return true
}

func (s *fakeStopSupervisor) StandDownEverySpawn(context.Context, string) error {
	s.calls = append(s.calls, "sweep")
	return s.sweepErr
}

func TestStopEveryShim(t *testing.T) {
	cases := []struct {
		name        string
		held        []ids.WorkspaceID
		fail        map[ids.WorkspaceID]error
		sweepErr    error
		wantStopped int
		wantErr     bool
	}{
		{name: "every held session is stopped", held: []ids.WorkspaceID{"w1", "w2"}, wantStopped: 2},
		{name: "a session that will not stop is reported", held: []ids.WorkspaceID{"w1", "w2"}, fail: map[ids.WorkspaceID]error{"w1": errors.New("no reap")}, wantStopped: 1, wantErr: true},
		{name: "a spawn that will not stop is reported", sweepErr: errors.New("no reap"), wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fleet := &fakeStopFleet{held: tc.held, fail: tc.fail}
			sup := &fakeStopSupervisor{sweepErr: tc.sweepErr}

			// Act
			stopped, err := stopEveryShim(fleet, sup, rootLossShimStopBound)(context.Background())

			// Assert
			if stopped != tc.wantStopped || (err != nil) != tc.wantErr {
				t.Fatalf("stopped %d, err %v; want %d, err: %v", stopped, err, tc.wantStopped, tc.wantErr)
			}
			if len(fleet.killed) != len(tc.held) || (len(tc.held) > 0 && !fleet.forced) {
				t.Fatalf("killed %v (forced %v), want every held session forced", fleet.killed, fleet.forced)
			}
			if len(sup.calls) != 2 || sup.calls[0] != "latch" || sup.calls[1] != "sweep" {
				t.Fatalf("supervisor calls = %v, want the latch then the sweep", sup.calls)
			}
		})
	}
}
