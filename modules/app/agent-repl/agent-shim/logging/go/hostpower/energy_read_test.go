package hostpower

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// fakePmset writes an executable that runs SCRIPT, standing in for pmset.
func fakePmset(t *testing.T, script string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "pmset")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+script+"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestReadEnergyMode(t *testing.T) {
	cases := []struct {
		name    string
		script  string
		bound   time.Duration
		want    string
		wantErr string
	}{
		{name: "the energy mode pmset names", script: `[ "$1" = -g ] && printf 'Currently in use:\n powermode            2\n'`, bound: EnergyModeBound, want: EnergyModeHighPower},
		{name: "a machine whose pmset names none", script: `printf 'Currently in use:\n sleep 0\n'`, bound: EnergyModeBound, want: ""},
		{name: "a pmset that fails answers its output", script: `echo 'no such setting'; exit 3`, bound: EnergyModeBound, wantErr: "exit status 3: no such setting"},
		{name: "a pmset that does not answer within its bound", script: `exec sleep 5`, bound: 50 * time.Millisecond, wantErr: "did not answer within 50ms"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			pmset := fakePmset(t, tc.script)

			// Act
			got, err := readEnergyMode(pmset, tc.bound)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("readEnergyMode = %q, %v; want an error naming %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("readEnergyMode = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestParseLidClosed(t *testing.T) {
	cases := []struct {
		name, out string
		want      bool
	}{
		{name: "a closed lid", out: `    | |   "AppleClamshellState" = Yes`, want: true},
		{name: "an open lid", out: `    | |   "AppleClamshellState" = No`, want: false},
		{name: "a machine naming no lid", out: ``, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := ParseLidClosed(tc.out); got != tc.want {
				t.Fatalf("ParseLidClosed(%q) = %v, want %v", tc.out, got, tc.want)
			}
		})
	}
}

func TestParseSleepDisabled(t *testing.T) {
	cases := []struct {
		name, out string
		want      bool
	}{
		{name: "sleep disabled", out: "System-wide power settings:\n SleepDisabled\t\t1\n", want: true},
		{name: "sleep enabled", out: "System-wide power settings:\n SleepDisabled\t\t0\n", want: false},
		{name: "no line", out: "Currently in use:\n sleep 0\n", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := ParseSleepDisabled(tc.out); got != tc.want {
				t.Fatalf("ParseSleepDisabled(%q) = %v, want %v", tc.out, got, tc.want)
			}
		})
	}
}

func TestReadLidClosedAwake(t *testing.T) {
	cases := []struct {
		name         string
		ioreg, pmset string
		want         bool
		wantErr      string
	}{
		{name: "a closed lid with sleep disabled", ioreg: `echo '"AppleClamshellState" = Yes'`, pmset: `printf ' SleepDisabled 1\n'`, want: true},
		{name: "a closed lid with sleep enabled", ioreg: `echo '"AppleClamshellState" = Yes'`, pmset: `printf ' SleepDisabled 0\n'`, want: false},
		{name: "an open lid never asks pmset", ioreg: `echo '"AppleClamshellState" = No'`, pmset: `exit 9`, want: false},
		{name: "an ioreg that fails", ioreg: `echo broken; exit 2`, pmset: `true`, wantErr: "exit status 2: broken"},
		{name: "a pmset that fails", ioreg: `echo '"AppleClamshellState" = Yes'`, pmset: `echo nope; exit 3`, wantErr: "exit status 3: nope"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ioreg, pmset := fakePmset(t, tc.ioreg), fakePmset(t, tc.pmset)

			// Act
			got, err := readLidClosedAwake(ioreg, pmset, EnergyModeBound)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("readLidClosedAwake = %v, %v; want an error naming %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("readLidClosedAwake = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}
