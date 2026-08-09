package server

import (
	"context"
	"errors"
	"strings"
	"testing"
)

type recordedStateVerdict struct{ workspace, sessionID, verdict string }

type recordedHostVerdict struct{ workspace, sessionID, verdict, reason string }

type fakeVerdictState struct {
	got []recordedStateVerdict
	err error
}

func (f *fakeVerdictState) ApplyBootSweepVerdict(workspace, sessionID, verdict string) error {
	f.got = append(f.got, recordedStateVerdict{workspace, sessionID, verdict})
	return f.err
}

type fakeVerdictHost struct {
	got []recordedHostVerdict
	err error
}

func (f *fakeVerdictHost) SurfaceBootSweepVerdict(_ context.Context, workspace, sessionID, verdict, reason string) error {
	f.got = append(f.got, recordedHostVerdict{workspace, sessionID, verdict, reason})
	return f.err
}

func newVerdictRouter(state *fakeVerdictState, host *fakeVerdictHost) *BootSweepVerdictRouter {
	return &BootSweepVerdictRouter{State: state, Host: host, Logf: func(string, ...any) {}}
}

// TestEveryVerdictReachesBothSurfaces is the per-verdict routing case: each of
// the sweep's four conclusions must land on the pushed state AND in front of a
// person, with the verdict token intact on both.
func TestEveryVerdictReachesBothSurfaces(t *testing.T) {
	tests := []struct {
		name    string
		verdict string
		// reasonSays is a phrase the composed sentence must contain, so a
		// verdict silently rendered as one of its siblings is caught.
		reasonSays string
	}{
		{
			name:       "the shim is genuinely gone",
			verdict:    BootSweepUnwiredNoLiveShim,
			reasonSays: "neither a connection nor a session lock",
		},
		{
			name:       "a live holder never dialled in",
			verdict:    BootSweepUnwiredLockHeldWithoutConnection,
			reasonSays: "never reconnected",
		},
		{
			name:       "the connection probe failed twice",
			verdict:    BootSweepUnwiredProbeFailed,
			reasonSays: "connection probe failed twice",
		},
		{
			name:       "the lock probe could not tell",
			verdict:    BootSweepUnwiredLockProbeFailed,
			reasonSays: "session-lock probe failed",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			state, host := &fakeVerdictState{}, &fakeVerdictHost{}
			router := newVerdictRouter(state, host)

			// Act
			if err := router.Route("/ws", "session-1", tc.verdict); err != nil {
				t.Fatalf("Route(%s): %v", tc.verdict, err)
			}

			// Assert
			wantState := recordedStateVerdict{"/ws", "session-1", tc.verdict}
			if len(state.got) != 1 || state.got[0] != wantState {
				t.Fatalf("state sink got %#v, want exactly %#v", state.got, wantState)
			}
			if len(host.got) != 1 {
				t.Fatalf("host sink got %#v, want exactly one notice", host.got)
			}
			notice := host.got[0]
			if notice.workspace != "/ws" || notice.sessionID != "session-1" || notice.verdict != tc.verdict {
				t.Fatalf("host notice = %#v, want ws /ws session session-1 verdict %s", notice, tc.verdict)
			}
			if !strings.Contains(notice.reason, tc.reasonSays) {
				t.Fatalf("host reason = %q, want it to say %q", notice.reason, tc.reasonSays)
			}
			if !strings.Contains(notice.reason, tc.verdict) {
				t.Fatalf("host reason = %q, want the verdict token %q in it: the arm carries no separate field for it",
					notice.reason, tc.verdict)
			}
		})
	}
}

// TestAnUnknownVerdictIsRefusedRatherThanRendered keeps a new sweep branch from
// being silently displayed as one of the four that already exist.
func TestAnUnknownVerdictIsRefusedRatherThanRendered(t *testing.T) {
	// Arrange
	state, host := &fakeVerdictState{}, &fakeVerdictHost{}
	router := newVerdictRouter(state, host)

	// Act
	err := router.Route("/ws", "session-1", "boot_sweep_something_new")

	// Assert
	if err == nil {
		t.Fatalf("Route = nil, want a refusal for a verdict with no display sentence")
	}
	if len(state.got) != 0 || len(host.got) != 0 {
		t.Fatalf("state=%#v host=%#v, want neither surface touched", state.got, host.got)
	}
}

// TestOneFailedSurfaceNeverHidesTheOther: a verdict that reached half the user
// is a partial account and both halves are reported.
func TestOneFailedSurfaceNeverHidesTheOther(t *testing.T) {
	stateErr := errors.New("state sink is down")
	hostErr := errors.New("host sink is down")
	tests := []struct {
		name      string
		stateFail error
		hostFail  error
		wantErrs  []error
		// wantOther is the surface that must still have been attempted.
		wantStateCalls, wantHostCalls int
	}{
		{
			name: "the state sink fails", stateFail: stateErr,
			wantErrs: []error{stateErr}, wantStateCalls: 1, wantHostCalls: 1,
		},
		{
			name: "the host sink fails", hostFail: hostErr,
			wantErrs: []error{hostErr}, wantStateCalls: 1, wantHostCalls: 1,
		},
		{
			name: "both sinks fail", stateFail: stateErr, hostFail: hostErr,
			wantErrs: []error{stateErr, hostErr}, wantStateCalls: 1, wantHostCalls: 1,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			state := &fakeVerdictState{err: tc.stateFail}
			host := &fakeVerdictHost{err: tc.hostFail}
			router := newVerdictRouter(state, host)

			// Act
			err := router.Route("/ws", "session-1", BootSweepUnwiredNoLiveShim)

			// Assert
			for _, want := range tc.wantErrs {
				if !errors.Is(err, want) {
					t.Fatalf("Route = %v, want it to carry %v", err, want)
				}
			}
			if len(state.got) != tc.wantStateCalls || len(host.got) != tc.wantHostCalls {
				t.Fatalf("state calls=%d host calls=%d, want %d and %d: a failed surface must not skip the other",
					len(state.got), len(host.got), tc.wantStateCalls, tc.wantHostCalls)
			}
		})
	}
}

// TestAnIncompleteRouterIsRefusedAtTheDoor: half a router is a verdict that
// reaches half the user, which is the silence this path exists to end.
func TestAnIncompleteRouterIsRefusedAtTheDoor(t *testing.T) {
	tests := []struct {
		name   string
		router *BootSweepVerdictRouter
	}{
		{
			name:   "no state sink",
			router: &BootSweepVerdictRouter{Host: &fakeVerdictHost{}, Logf: func(string, ...any) {}},
		},
		{
			name:   "no host sink",
			router: &BootSweepVerdictRouter{State: &fakeVerdictState{}, Logf: func(string, ...any) {}},
		},
		{
			name:   "no logger",
			router: &BootSweepVerdictRouter{State: &fakeVerdictState{}, Host: &fakeVerdictHost{}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			err := tc.router.Route("/ws", "session-1", BootSweepUnwiredNoLiveShim)

			// Assert
			if err == nil {
				t.Fatalf("Route = nil, want a refusal from an incomplete router")
			}
		})
	}
}

// TestTheSweepRoutesEveryUnwiredVerdict wires the router to the sweeper itself,
// so the four constants and the four display sentences cannot drift apart.
func TestTheSweepRoutesEveryUnwiredVerdict(t *testing.T) {
	for _, verdict := range []string{
		BootSweepUnwiredNoLiveShim,
		BootSweepUnwiredLockHeldWithoutConnection,
		BootSweepUnwiredProbeFailed,
		BootSweepUnwiredLockProbeFailed,
	} {
		t.Run(verdict, func(t *testing.T) {
			// Arrange / Act
			reason, err := BootSweepVerdictReason(verdict)

			// Assert
			if err != nil {
				t.Fatalf("BootSweepVerdictReason(%s): %v", verdict, err)
			}
			if reason == "" {
				t.Fatalf("BootSweepVerdictReason(%s) is empty; the host renders it verbatim", verdict)
			}
		})
	}
}
