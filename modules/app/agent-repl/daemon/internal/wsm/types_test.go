package wsm

import "testing"

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
		{name: "a session the idle sweep hibernated", terminal: TerminalHibernated, want: true},
		{name: "a session stood down between keep-alives", terminal: TerminalKeepaliveCycle, want: true},
		{name: "a deleted session", terminal: "deleted", want: false},
		{name: "a session whose shim died", terminal: "shim_died", want: false},
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

func TestSessionKeepaliveCycled(t *testing.T) {
	tests := []struct {
		name     string
		terminal string
		want     bool
	}{
		{name: "a session stood down between keep-alives", terminal: TerminalKeepaliveCycle, want: true},
		{name: "a session the idle sweep hibernated", terminal: TerminalHibernated, want: false},
		{name: "a live session", terminal: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := sessionWithTerminal(tt.terminal)

			// Act
			got := s.KeepaliveCycled()

			// Assert
			if got != tt.want {
				t.Fatalf("KeepaliveCycled() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParkOf(t *testing.T) {
	tests := []struct {
		name     string
		terminal string
		want     Park
	}{
		{name: "a live session draws no park", terminal: "", want: ParkNone},
		{name: "a hibernated session draws hibernated", terminal: TerminalHibernated, want: ParkHibernated},
		{name: "a session stood down between keep-alives draws the cycle", terminal: TerminalKeepaliveCycle, want: ParkKeepaliveCycle},
		{name: "a killed session draws no park", terminal: "killed", want: ParkNone},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := sessionWithTerminal(tt.terminal)

			// Act
			got := ParkOf(s)

			// Assert
			if got != tt.want {
				t.Fatalf("ParkOf() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParkString(t *testing.T) {
	tests := []struct {
		name string
		park Park
		want string
	}{
		{name: "no park", park: ParkNone, want: "none"},
		{name: "hibernated", park: ParkHibernated, want: "hibernated"},
		{name: "the keep-alive cycle", park: ParkKeepaliveCycle, want: "keepalive_cycle"},
		{name: "an unnamed park", park: Park(9), want: "park(9)"},
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
