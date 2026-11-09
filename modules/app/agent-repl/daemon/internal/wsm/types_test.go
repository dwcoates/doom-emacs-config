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

func TestParkState(t *testing.T) {
	at := time.Date(2026, 11, 9, 14, 3, 0, 0, time.UTC)
	tests := []struct {
		name    string
		arrange func(s *ParkState)
		want    Park
	}{
		{name: "nothing held is no park", arrange: func(*ParkState) {}, want: ParkNone},
		{name: "a schedule alone raises no park", arrange: func(s *ParkState) { s.SetNextKeepalive(at) }, want: ParkNone},
		{name: "a park raised after the schedule carries it", arrange: func(s *ParkState) {
			s.SetNextKeepalive(at)
			s.SetParked(true)
		}, want: Parked(at)},
		{name: "a park raised before the schedule carries it once stated", arrange: func(s *ParkState) {
			s.SetParked(true)
			s.SetNextKeepalive(at)
		}, want: Parked(at)},
		{name: "a park with none scheduled carries none", arrange: func(s *ParkState) { s.SetParked(true) }, want: Parked(time.Time{})},
		{name: "a lifted park is no park, whatever is scheduled", arrange: func(s *ParkState) {
			s.SetNextKeepalive(at)
			s.SetParked(true)
			s.SetParked(false)
		}, want: ParkNone},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var s ParkState
			tt.arrange(&s)

			// Act
			got := s.Park()

			// Assert
			if got != tt.want {
				t.Fatalf("Park() = %v, want %v", got, tt.want)
			}
		})
	}
}
