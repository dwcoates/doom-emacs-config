package main

import (
	"strings"
	"testing"
)

func TestParseArgvAcceptsTheSpawnContract(t *testing.T) {
	// Arrange
	args := []string{"/opt/shim/dist/main.js", "--listen", "/tmp/ws.sock", "--store-socket", "/tmp/store.sock", "--log-fd", "3", "--spawn-gate-fd", "4", "--fake"}

	// Act
	got, err := ParseArgv(args)

	// Assert
	if err != nil {
		t.Fatalf("ParseArgv = error %v, want the parsed contract", err)
	}
	want := Argv{MainJS: "/opt/shim/dist/main.js", Listen: "/tmp/ws.sock", StoreSocket: "/tmp/store.sock", LogFD: 3, SpawnGateFD: 4, Fake: true}
	if got != want {
		t.Fatalf("ParseArgv = %+v, want %+v", got, want)
	}
}

func TestParseArgvWithoutFakeFlag(t *testing.T) {
	// Arrange
	args := []string{"main.js", "--listen", "a", "--store-socket", "b", "--log-fd", "3"}

	// Act
	got, err := ParseArgv(args)

	// Assert
	if err != nil {
		t.Fatalf("ParseArgv = error %v, want the parsed contract", err)
	}
	if got.Fake {
		t.Fatalf("ParseArgv Fake = true, want false when --fake is absent")
	}
}

func TestParseArgvRejectsIncompleteContracts(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		wantSub string
	}{
		{name: "no module path", args: []string{"--listen", "a", "--store-socket", "b", "--log-fd", "3"}, wantSub: "module path is missing"},
		{name: "no listen", args: []string{"main.js", "--store-socket", "b", "--log-fd", "3"}, wantSub: "--listen is missing"},
		{name: "no store socket", args: []string{"main.js", "--listen", "a", "--log-fd", "3"}, wantSub: "--store-socket is missing"},
		{name: "no log fd", args: []string{"main.js", "--listen", "a", "--store-socket", "b"}, wantSub: "--log-fd is missing"},
		{name: "dangling value", args: []string{"main.js", "--listen"}, wantSub: "wants a value"},
		{name: "non numeric log fd", args: []string{"main.js", "--listen", "a", "--store-socket", "b", "--log-fd", "three"}, wantSub: "--log-fd"},
		{name: "unknown flag", args: []string{"main.js", "--turbo", "--listen", "a", "--store-socket", "b", "--log-fd", "3"}, wantSub: "unknown flag"},
		{name: "second positional", args: []string{"main.js", "other.js", "--listen", "a", "--store-socket", "b", "--log-fd", "3"}, wantSub: "unexpected positional"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			_, err := ParseArgv(tc.args)

			// Assert
			if err == nil {
				t.Fatalf("ParseArgv(%v) = no error, want one naming %q", tc.args, tc.wantSub)
			}
			if !strings.Contains(err.Error(), tc.wantSub) {
				t.Fatalf("ParseArgv(%v) = %v, want an error naming %q", tc.args, err, tc.wantSub)
			}
		})
	}
}

func TestParseArgvGatesNothingWhenTheLauncherGatesNothing(t *testing.T) {
	// Arrange
	args := []string{"main.js", "--listen", "a", "--store-socket", "b", "--log-fd", "3"}

	// Act
	got, err := ParseArgv(args)

	// Assert
	if err != nil || got.SpawnGateFD != -1 {
		t.Fatalf("ParseArgv = (%+v, %v), want no spawn gate", got, err)
	}
}
