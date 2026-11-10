package deploy

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func newSystemdUser(answers map[string]runAnswer) (*SystemdUser, *scriptedRunner) {
	runner := &scriptedRunner{answers: answers}
	return &SystemdUser{Binary: "systemctl", UnitDir: "/u", Dir: "/", Runner: runner}, runner
}

const systemdShowX = "systemctl show --user x.service --property=ActiveState --property=MainPID"

func TestSystemdUserStatus(t *testing.T) {
	tests := []struct {
		name     string
		answer   runAnswer
		wantHeld bool
		wantPID  int
		wantErr  string
	}{
		{name: "a running unit", answer: runAnswer{out: "ActiveState=active\nMainPID=4242\n"}, wantHeld: true, wantPID: 4242},
		{name: "a unit between automatic restarts is held with no process", answer: runAnswer{out: "ActiveState=activating\nMainPID=0\n"}, wantHeld: true},
		{name: "a unit on its way down is still held", answer: runAnswer{out: "ActiveState=deactivating\nMainPID=77\n"}, wantHeld: true, wantPID: 77},
		{name: "a stopped unit is not held", answer: runAnswer{out: "ActiveState=inactive\nMainPID=0\n"}},
		{name: "a failed unit is not held", answer: runAnswer{out: "ActiveState=failed\nMainPID=0\n"}},
		{name: "an answer with no ActiveState is an error", answer: runAnswer{out: "MainPID=0\n"}, wantErr: "no ActiveState"},
		{name: "an unreadable MainPID is an error", answer: runAnswer{out: "ActiveState=active\nMainPID=x\n"}, wantErr: "unreadable MainPID"},
		{name: "a non-zero exit is an error", answer: runAnswer{out: "Failed to connect to bus", code: 1}, wantErr: "exited 1: Failed to connect to bus"},
		{name: "a run that could not start is an error", answer: runAnswer{err: errors.New("exec: not found")}, wantErr: "not found"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := newSystemdUser(map[string]runAnswer{systemdShowX: tc.answer})

			// Act
			held, pid, err := s.Status(context.Background(), "x")

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Status error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || held != tc.wantHeld || pid != tc.wantPID {
				t.Fatalf("Status = %v, %d, %v; want %v, %d", held, pid, err, tc.wantHeld, tc.wantPID)
			}
		})
	}
}

func TestSystemdUserVerbs(t *testing.T) {
	tests := []struct {
		name string
		call func(s *SystemdUser) error
		argv string
	}{
		{name: "restart restarts the unit", call: func(s *SystemdUser) error { return s.Restart(context.Background(), "x") }, argv: "systemctl restart --user x.service"},
		{name: "stop stops the unit", call: func(s *SystemdUser) error { return s.Stop(context.Background(), "x") }, argv: "systemctl stop --user x.service"},
		{name: "start starts the unit", call: func(s *SystemdUser) error { return s.Start(context.Background(), "x") }, argv: "systemctl start --user x.service"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, runner := newSystemdUser(map[string]runAnswer{})

			// Act
			err := tc.call(s)

			// Assert
			if err != nil {
				t.Fatalf("verb: %v", err)
			}
			calls := runner.Calls()
			if len(calls) != 1 || strings.Join(calls[0], " ") != tc.argv {
				t.Fatalf("argv = %v, want %q", calls, tc.argv)
			}
		})
	}
}

func TestASystemctlVerbThatExitsNonZeroFails(t *testing.T) {
	tests := []struct {
		name string
		argv string
		call func(s *SystemdUser) error
	}{
		{name: "restart", argv: "systemctl restart --user x.service", call: func(s *SystemdUser) error { return s.Restart(context.Background(), "x") }},
		{name: "start", argv: "systemctl start --user x.service", call: func(s *SystemdUser) error { return s.Start(context.Background(), "x") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := newSystemdUser(map[string]runAnswer{tc.argv: {out: "Job for x.service failed", code: 1}})

			// Act
			err := tc.call(s)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "Job for x.service failed") || !strings.Contains(err.Error(), "systemctl "+tc.name) {
				t.Fatalf("%s = %v, want the exit and systemctl's words", tc.name, err)
			}
		})
	}
}

func TestSystemdUserStopAnswers(t *testing.T) {
	tests := []struct {
		name          string
		answer        runAnswer
		wantErr       bool
		wantNotLoaded bool
	}{
		{name: "a held unit is stopped", answer: runAnswer{}},
		{name: "exit 5 is a unit the manager has not loaded", answer: runAnswer{out: "Failed to stop x.service: Unit x.service not loaded.", code: 5}, wantErr: true, wantNotLoaded: true},
		{name: "any other exit is a failed stop", answer: runAnswer{out: "Failed to connect to bus", code: 1}, wantErr: true},
		{name: "a run that could not start is a failed stop", answer: runAnswer{err: errors.New("exec: not found")}, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := newSystemdUser(map[string]runAnswer{"systemctl stop --user x.service": tc.answer})

			// Act
			err := s.Stop(context.Background(), "x")

			// Assert
			if (err != nil) != tc.wantErr || errors.Is(err, ErrServiceNotLoaded) != tc.wantNotLoaded {
				t.Fatalf("Stop = %v; want error %v, not-loaded %v", err, tc.wantErr, tc.wantNotLoaded)
			}
		})
	}
}

func TestSystemdUserDefinitionIsTheLabelsUnit(t *testing.T) {
	// Arrange
	s, _ := newSystemdUser(map[string]runAnswer{})

	// Act
	got := s.Definition("x")

	// Assert
	if got != "/u/x.service" {
		t.Fatalf("Definition = %q, want /u/x.service", got)
	}
}
