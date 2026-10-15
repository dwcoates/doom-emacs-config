package e2e

import (
	"errors"
	"strings"
	"testing"
	"time"
)

// stampScript answers awaitDoomStamp's readers from fixed answers, one look
// at a time, and counts its stall reports.
type stampScript struct {
	writtenAt int // the look the stamp is first found at; -1 never
	exitAt    int // the look Emacs has exited by; -1 never
	cpu       []time.Duration
	readErr   error
	looks     int
	stalls    int
}

func (s *stampScript) wait(poll, stalled, capped chan time.Time) doomStampWait {
	return doomStampWait{
		poll: poll, stalled: stalled, capped: capped,
		read: func() ([]byte, bool, error) {
			s.looks++
			if s.readErr != nil {
				return nil, false, s.readErr
			}
			if s.writtenAt >= 0 && s.looks > s.writtenAt {
				return []byte(`{"ok":true}`), true, nil
			}
			return nil, false, nil
		},
		exited: func() bool { return s.exitAt >= 0 && s.looks > s.exitAt },
		cpu: func() (time.Duration, error) {
			i := min(s.looks-1, len(s.cpu)-1)
			return s.cpu[i], nil
		},
		onStall: func() { s.stalls++ },
	}
}

func TestAwaitDoomStamp(t *testing.T) {
	tests := []struct {
		name       string
		script     stampScript
		fire       func(poll, stalled, capped chan time.Time)
		wantKind   doomOutcomeKind
		wantStalls int
	}{
		{name: "a stamp already written ends the wait", script: stampScript{writtenAt: 0, exitAt: -1, cpu: []time.Duration{0}},
			wantKind: doomStamped},
		{name: "a stamp written after a stall report ends the wait, the stall reported once",
			script: stampScript{writtenAt: 2, exitAt: -1, cpu: []time.Duration{0}},
			fire: func(poll, stalled, _ chan time.Time) {
				stalled <- time.Time{}
				poll <- time.Time{}
			},
			wantKind: doomStamped, wantStalls: 1},
		{name: "an Emacs that exits before stamping ends the wait", script: stampScript{writtenAt: -1, exitAt: 0, cpu: []time.Duration{0}},
			wantKind: doomExited},
		{name: "a boot that spent more CPU than its budget fails", script: stampScript{writtenAt: -1, exitAt: -1, cpu: []time.Duration{doomBootCPUBound + time.Millisecond}},
			wantKind: doomOverBudget},
		{name: "a boot exactly at its CPU budget waits on", script: stampScript{writtenAt: 1, exitAt: -1, cpu: []time.Duration{doomBootCPUBound}},
			fire:     func(poll, _, _ chan time.Time) { poll <- time.Time{} },
			wantKind: doomStamped},
		{name: "a boot that spends no CPU and never stamps ends at the last-resort cap",
			script:   stampScript{writtenAt: -1, exitAt: -1, cpu: []time.Duration{0}},
			fire:     func(_, _, capped chan time.Time) { capped <- time.Time{} },
			wantKind: doomCapped},
		{name: "a stamp that cannot be read ends the wait", script: stampScript{writtenAt: -1, exitAt: -1, cpu: []time.Duration{0}, readErr: errors.New("permission denied")},
			wantKind: doomReadFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			poll, stalled, capped := make(chan time.Time, 1), make(chan time.Time, 1), make(chan time.Time, 1)
			w := tt.script.wait(poll, stalled, capped)
			done := make(chan doomOutcome, 1)

			// Act
			go func() { done <- awaitDoomStamp(w) }()
			if tt.fire != nil {
				tt.fire(poll, stalled, capped)
			}
			got := <-done

			// Assert
			if got.kind != tt.wantKind || tt.script.stalls != tt.wantStalls {
				t.Fatalf("outcome %v with %d stall reports, want %v with %d", got.kind, tt.script.stalls, tt.wantKind, tt.wantStalls)
			}
		})
	}
}

func TestStatCPUTicks(t *testing.T) {
	tests := []struct {
		name    string
		stat    string
		want    int64
		wantErr string
	}{
		{name: "fields 14 to 17 are summed",
			stat: "42 (emacs) S 1 42 42 0 -1 4194560 100 0 0 0 30 12 5 3 20 0 1 0 100 1000 200", want: 50},
		{name: "a command holding spaces and parentheses is skipped whole",
			stat: "42 (my (odd) cmd) R 1 42 42 0 -1 4194560 100 0 0 0 7 1 0 2 20 0 1 0 100 1000 200", want: 10},
		{name: "a line with no command field is refused", stat: "42 emacs S", wantErr: "no command field"},
		{name: "a short line is refused", stat: "42 (emacs) S 1 2", wantErr: "want at least 15"},
		{name: "a field that is not a number is refused",
			stat: "42 (emacs) S 1 42 42 0 -1 4194560 100 0 0 0 x 12 5 3 20", wantErr: "CPU field"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := statCPUTicks(tt.stat)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("statCPUTicks = %d, %v; want %d", got, err, tt.want)
			}
		})
	}
}

func TestEmacsCPUOfAnUnknownPidIsNothingSpent(t *testing.T) {
	// Act
	got, err := emacsCPU(0)

	// Assert
	if got != 0 || err != nil {
		t.Fatalf("emacsCPU(0) = %s, %v; want nothing spent", got, err)
	}
}
