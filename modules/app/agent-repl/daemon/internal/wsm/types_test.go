package wsm

import (
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

func TestSessionDormant(t *testing.T) {
	restored := time.Date(2026, 11, 23, 12, 0, 0, 0, time.UTC)
	before, after := restored.Add(-time.Minute), restored.Add(time.Minute)
	tests := []struct {
		name        string
		session     Session
		turnStarted *time.Time
		running     bool
		want        bool
	}{
		{name: "a live session up for work", session: Session{}, want: false},
		{name: "a parked session", session: sessionWithTerminal(TerminalParked), want: true},
		{name: "a dead session", session: sessionWithTerminal("shim_died"), want: false},
		{name: "a keep-alive's restore with no turn ever", session: Session{KeepaliveRestoredAt: &restored}, want: true},
		{name: "a keep-alive's restore whose last turn started before it", session: Session{KeepaliveRestoredAt: &restored}, turnStarted: &before, want: true},
		{name: "a keep-alive's restore a turn has started since", session: Session{KeepaliveRestoredAt: &restored}, turnStarted: &after, want: false},
		{name: "a keep-alive's restore under a running turn", session: Session{KeepaliveRestoredAt: &restored}, turnStarted: &before, running: true, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.session.Dormant(tt.turnStarted, tt.running)

			// Assert
			if got != tt.want {
				t.Fatalf("Dormant() = %v, want %v", got, tt.want)
			}
		})
	}
}
