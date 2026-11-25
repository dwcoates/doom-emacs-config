package wsm

import (
	"strings"
	"testing"
	"time"
)

func sessionWithTerminal(kind string) Session {
	if kind == "" {
		return Session{}
	}
	return Session{Terminal: &SessionTerminal{Kind: kind}}
}

func TestSessionParked(t *testing.T) {
	tests := []struct {
		name     string
		terminal string
		want     bool
	}{
		{name: "a live session", terminal: "", want: false},
		{name: "a parked session", terminal: TerminalParked, want: true},
		{name: "a deleted session", terminal: "deleted", want: false},
		{name: "a session whose shim died", terminal: "shim_died", want: false},
		{name: "a retired hibernated kind is not read as a park", terminal: "hibernated", want: false},
		{name: "a retired keep-alive cycle kind is not read as a park", terminal: "keepalive_cycle", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := sessionWithTerminal(tt.terminal)

			// Act
			got := s.Parked()

			// Assert
			if got != tt.want {
				t.Fatalf("Parked() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParkNextKeepalive(t *testing.T) {
	at := time.Date(2026, 11, 9, 14, 3, 0, 0, time.UTC)
	tests := []struct {
		name         string
		park         Park
		wantStanding bool
		wantNext     time.Time
		wantOK       bool
	}{
		{name: "no park carries nothing", park: ParkNone},
		{name: "a park with a scheduled keep-alive carries it", park: Parked(at), wantStanding: true, wantNext: at, wantOK: true},
		{name: "a park with none scheduled carries none", park: Parked(time.Time{}), wantStanding: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			next, ok := tt.park.NextKeepalive()

			// Assert
			if tt.park.Standing() != tt.wantStanding {
				t.Errorf("Standing() = %v, want %v", tt.park.Standing(), tt.wantStanding)
			}
			if ok != tt.wantOK || !next.Equal(tt.wantNext) {
				t.Errorf("NextKeepalive() = %v, %v; want %v, %v", next, ok, tt.wantNext, tt.wantOK)
			}
		})
	}
}

func TestParkString(t *testing.T) {
	at := time.Date(2026, 11, 9, 14, 3, 0, 0, time.UTC)
	tests := []struct {
		name string
		park Park
		want string
	}{
		{name: "no park", park: ParkNone, want: "none"},
		{name: "a park with none scheduled", park: Parked(time.Time{}), want: "parked"},
		{name: "a park with a scheduled keep-alive", park: Parked(at), want: "parked until 2026-11-09T14:03:00Z"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.park.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestNextKeepalives(t *testing.T) {
	at := time.Date(2026, 11, 9, 14, 3, 0, 0, time.UTC)
	tests := []struct {
		name    string
		arrange func(k NextKeepalives)
		parked  bool
		want    Park
		wantLen int
	}{
		{name: "nothing held and no park is no park", arrange: func(NextKeepalives) {}, want: ParkNone},
		{name: "a schedule alone raises no park", arrange: func(k NextKeepalives) { k.Set("ws-1", at) }, want: ParkNone, wantLen: 1},
		{name: "a park carries the schedule held", arrange: func(k NextKeepalives) { k.Set("ws-1", at) }, parked: true, want: Parked(at), wantLen: 1},
		{name: "a park with none scheduled carries none", arrange: func(NextKeepalives) {}, parked: true, want: Parked(time.Time{})},
		{name: "a schedule moved to none forgets the workspace", arrange: func(k NextKeepalives) {
			k.Set("ws-1", at)
			k.Set("ws-1", time.Time{})
		}, parked: true, want: Parked(time.Time{})},
		{name: "another workspace's schedule is not carried", arrange: func(k NextKeepalives) { k.Set("ws-2", at) }, parked: true, want: Parked(time.Time{}), wantLen: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			k := NextKeepalives{}
			tt.arrange(k)

			// Act
			got := k.Park("ws-1", tt.parked)

			// Assert
			if got != tt.want {
				t.Errorf("Park() = %v, want %v", got, tt.want)
			}
			if len(k) != tt.wantLen {
				t.Errorf("held %d schedules, want %d", len(k), tt.wantLen)
			}
		})
	}
}

func TestMediatorRoleName(t *testing.T) {
	cases := []struct {
		name string
		role MediatorRole
		want string
	}{
		{name: "the deploy mediator", role: MediatorDeploy, want: "deploy-mediator"},
		{name: "the production mediator", role: MediatorProduction, want: "prod-mediator"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.role.Name()

			// Assert
			if got != tc.want || strings.Contains(got, "/") {
				t.Fatalf("%s.Name() = %q, want %q", tc.role, got, tc.want)
			}
		})
	}
}

func TestEveryMediatorRoleHasAName(t *testing.T) {
	for _, role := range MediatorRoles {
		if role.Name() == "" {
			t.Errorf("%s has no name", role)
		}
	}
}

func TestTheNameOfNoMediatorRoleIsABug(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("NotMediator.Name() answered, want a panic")
		}
	}()

	// Act
	_ = NotMediator.Name()
}

func TestWorkspaceEndedAt(t *testing.T) {
	merged, closed := time.Date(2026, 11, 25, 9, 0, 0, 0, time.UTC), time.Date(2026, 11, 25, 8, 0, 0, 0, time.UTC)
	cases := []struct {
		name      string
		ws        Workspace
		want      *time.Time
		wantEnded bool
	}{
		{name: "an open workspace has not ended", ws: Workspace{}},
		{name: "a landed workspace ended at its landing", ws: Workspace{Closed: true, MergedAt: &merged, Closure: &Closure{At: closed}}, want: &merged, wantEnded: true},
		{name: "a closed workspace ended at its close", ws: Workspace{Closed: true, Closure: &Closure{At: closed}}, want: &closed, wantEnded: true},
		{name: "a close with no record ended at no known instant", ws: Workspace{Closed: true}, wantEnded: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ended := tc.ws.EndedAt()

			// Assert
			if ended != tc.wantEnded || (got == nil) != (tc.want == nil) || (got != nil && !got.Equal(*tc.want)) {
				t.Fatalf("EndedAt = (%v, %v), want (%v, %v)", got, ended, tc.want, tc.wantEnded)
			}
		})
	}
}
