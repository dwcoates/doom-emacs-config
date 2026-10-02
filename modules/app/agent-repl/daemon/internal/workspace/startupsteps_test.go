package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/startup"
	"claude-repld/internal/wsm"
)

// stepNames is the fixture's reported steps by name, with a vendor retry's
// attempt and a step's text appended.
func stepNames(f *fleetFixture) []string {
	out := []string{}
	for _, s := range f.steps {
		name := s.Kind.String()
		if s.Kind == startup.StepVendorRetrying {
			name += "#" + string(rune('0'+s.Attempt))
		}
		if s.Text != "" && s.Kind != startup.StepFailed {
			name += ":" + s.Text
		}
		out = append(out, name)
	}
	return out
}

// THE FLEET TELLS THE STARTUP EVERY STEP OF EVERY BRING-UP, one table row per
// path a bring-up can take.
func TestTheFleetReportsEveryBringUpStep(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture)
		want    []string
	}{
		{name: "a fresh start", want: []string{"starting_session", "serving", "up"}},
		{name: "a resumed conversation", arrange: func(f *fleetFixture) {
			f.db.sessions["w1"] = wsm.Session{Workspace: "w1", VendorSessionID: "vendor-1"}
		}, want: []string{"starting_session", "serving", "resuming", "up"}},
		{name: "a hibernated workspace is woken", arrange: func(f *fleetFixture) {
			f.db.sessions["w1"] = wsm.Session{Workspace: "w1", VendorSessionID: "vendor-1",
				Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}}
		}, want: []string{"waking", "serving", "resuming", "up"}},
		{name: "an adopted survivor", arrange: func(f *fleetFixture) {
			f.probeState = sessionlock.StateHeld
			f.socketState = shimsocket.StateLive
		}, want: []string{"starting_session", "serving", "up"}},
		{name: "a resume parked at its cold gate", arrange: func(f *fleetFixture) {
			f.db.sessions["w1"] = wsm.Session{Workspace: "w1", VendorSessionID: "vendor-1"}
			f.client.response = coldResponse()
		}, want: []string{"starting_session", "serving", "resuming", "cold_gate"}},
		{name: "a vendor start retried, then up", arrange: func(f *fleetFixture) {
			f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "silent")}
		}, want: []string{"starting_session", "serving", "vendor_retrying#1", "up"}},
		{name: "the network unreachable, then up", arrange: func(f *fleetFixture) {
			f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(offlineVendorStart(), "offline")}
		}, want: []string{"starting_session", "serving", "offline", "up"}},
		{name: "a vendor that refused", arrange: func(f *fleetFixture) {
			f.client.response = vendorRefusal(rejectedVendorStart(), "bad credentials")
		}, want: []string{"starting_session", "serving", "vendor_rejected:bad credentials"}},
		{name: "a vendor that failed for the window", arrange: func(f *fleetFixture) {
			f.client.response = vendorRefusal(retryableVendorStart(), "silent")
		}, want: nil},
		{name: "a shim that would not spawn", arrange: func(f *fleetFixture) {
			f.supervisor.spawnErr = errors.New("exec: node: not found")
		}, want: []string{"starting_session", "failed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			_ = f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			got := stepNames(f)
			if tt.want == nil {
				// The exhausted window: the run's last two steps.
				if n := len(got); n < 2 || got[n-1] != "vendor_failed" || got[n-2][:len("vendor_retrying")] != "vendor_retrying" {
					t.Fatalf("steps end %v, want the last retry then vendor_failed", got)
				}
				return
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("steps = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAFailedBringUpNamesItsReason(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.spawnErr = errors.New("exec: node: not found")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	last := f.steps[len(f.steps)-1]
	if last.Kind != startup.StepFailed || last.Text != err.Error() {
		t.Fatalf("last step = %+v, want failed with the start's own error %q", last, err)
	}
}

func TestAnAlreadyLiveWorkspaceReportsNoStep(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("first Start: %v", err)
	}
	f.steps = nil

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.steps) != 0 {
		t.Fatalf("steps = %v, want none for a session already up", stepNames(f))
	}
}

// A SPAWN REFUSED BECAUSE THE SUPERVISOR BEGAN STANDING DOWN after the
// fleet's own check is the same refusal as one the check caught: INFO, no
// fault, and the error says the daemon is standing down.
func TestASpawnRefusedByAStandingDownSupervisorIsNotAFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.supervisor.spawnErr = shimclient.ErrStandingDown

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if !errors.Is(err, shimclient.ErrStandingDown) {
		t.Fatalf("Start = %v, want the standing-down refusal", err)
	}
	if got := openKinds(f); got[health.KindShimStartFailed] != 0 {
		t.Fatalf("open faults = %v, want no start failure for a daemon leaving", got)
	}
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("an ERROR was recorded for a daemon leaving: %+v", r)
		}
	}
}
