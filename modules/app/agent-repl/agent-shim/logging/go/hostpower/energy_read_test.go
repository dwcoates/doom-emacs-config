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
