package reuse

import (
	"errors"
	"slices"
	"strings"
	"testing"
)

func TestConfinedEnvKeepsOnlyWhatTheKeyCoversOrTheRunOwns(t *testing.T) {
	// Arrange
	environ := []string{
		"HOME=/h", "PATH=/bin", "GOFLAGS=-p=1", "CGO_ENABLED=0", // keyed
		"TMPDIR=/t", "AGENT_REPL_TEST_SHORT_BASE=/s", "AGENT_REPL_BACKGROUND_PRIORITY=nice-19", // run-scoped
		"AGENT_REPL_TEST_PROGRAMS=/p",                                             // the unit's own
		"CLAUDE_CONFIG_DIR=/c", "AGENT_REPL_LOG_LEVEL=debug", "TERM_SESSION_ID=x", // dropped
	}

	// Act
	got := ConfinedEnv(environ, map[string]bool{"AGENT_REPL_TEST_PROGRAMS": true})

	// Assert
	want := []string{"HOME=/h", "PATH=/bin", "GOFLAGS=-p=1", "CGO_ENABLED=0", "TMPDIR=/t",
		"AGENT_REPL_TEST_SHORT_BASE=/s", "AGENT_REPL_BACKGROUND_PRIORITY=nice-19", "AGENT_REPL_TEST_PROGRAMS=/p"}
	if !slices.Equal(got, want) {
		t.Fatalf("ConfinedEnv = %v, want %v", got, want)
	}
}

func TestKeyedEnvHoldsOnlyKeyedValues(t *testing.T) {
	// Act
	got := KeyedEnv([]string{"PATH=/bin", "GOFLAGS=-p=1", "TMPDIR=/t", "AGENT_REPL_LOG_LEVEL=debug"})

	// Assert
	if len(got) != 2 || got["PATH"] != "/bin" || got["GOFLAGS"] != "-p=1" {
		t.Fatalf("KeyedEnv = %v, want PATH and GOFLAGS only", got)
	}
}

func TestConfineCommandRoundTripsThroughParseConfine(t *testing.T) {
	// Arrange
	argv := ConfineCommand("/bin/testrun", "/w/daemon.sb", []string{"GOMAXPROCS=2", "GOFLAGS=-p=1"}, []string{"go", "test", "-c", "--"})

	// Act
	got, err := ParseConfine(argv[2:])

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if argv[0] != "/bin/testrun" || argv[1] != "confine" {
		t.Fatalf("argv = %v, want the runner's confine subcommand", argv)
	}
	if got.Profile != "/w/daemon.sb" || !got.Keep["GOMAXPROCS"] || !got.Keep["GOFLAGS"] || len(got.Keep) != 2 {
		t.Fatalf("ParseConfine = %+v", got)
	}
	if !slices.Equal(got.Argv, []string{"go", "test", "-c", "--"}) {
		t.Fatalf("Argv = %v, want the unit's own command whole", got.Argv)
	}
}

func TestParseConfineRefusesAnIncompleteLine(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{"no profile", []string{"--", "true"}, "--profile is required"},
		{"no command", []string{"--profile", "p"}, "no command"},
		{"a dangling profile", []string{"--profile"}, "--profile needs a path"},
		{"a dangling keep", []string{"--profile", "p", "--keep"}, "--keep needs"},
		{"an unknown flag", []string{"--bogus"}, "unknown argument"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseConfine(tt.args)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestProbeSandbox(t *testing.T) {
	tests := []struct {
		name    string
		run     error
		wantErr string
	}{
		{"a sandbox can be applied", nil, ""},
		{"a nested sandbox is refused", errors.New("exit status 71"), "cannot be applied here"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var called []string

			// Act
			err := ProbeSandbox(func(name string, args ...string) error {
				called = append([]string{name}, args...)
				return tt.run
			})

			// Assert
			if called[0] != SandboxExec {
				t.Fatalf("probed with %v", called)
			}
			if (err == nil) != (tt.wantErr == "") || (err != nil && !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("err = %v, want %q", err, tt.wantErr)
			}
		})
	}
}
