package reuse

import (
	"errors"
	"maps"
	"slices"
	"strings"
	"testing"
)

func TestConfinedEnvKeepsOnlyWhatTheKeyCoversOrTheRunOwns(t *testing.T) {
	// Arrange
	environ := []string{
		"HOME=/h", "PATH=/bin", "GOFLAGS=-p=1", "CGO_ENABLED=0", // keyed
		"TMPDIR=/t", "AGENT_REPL_TEST_SHORT_BASE=/s", "AGENT_REPL_BACKGROUND_PRIORITY=nice-19", // run-scoped
		"AGENT_REPL_TEST_CORE_SEATS_HELD=42",                                      // run-scoped
		"AGENT_REPL_TEST_PROGRAMS=/p",                                             // the unit's own
		"CLAUDE_CONFIG_DIR=/c", "AGENT_REPL_LOG_LEVEL=debug", "TERM_SESSION_ID=x", // dropped
		"GOPRIVATE=example.com/*", "GOPROXY=off", "LANG=en_US.UTF-8", "LC_ALL=C", "TZ=UTC", // dropped
		"GOENV=/somewhere/go.env", // canonical, overridden
	}

	// Act
	got := ConfinedEnv(environ, map[string]bool{"AGENT_REPL_TEST_PROGRAMS": true})

	// Assert
	want := []string{"HOME=/h", "PATH=/bin", "GOFLAGS=-p=1", "CGO_ENABLED=0", "TMPDIR=/t",
		"AGENT_REPL_TEST_SHORT_BASE=/s", "AGENT_REPL_BACKGROUND_PRIORITY=nice-19", "AGENT_REPL_TEST_CORE_SEATS_HELD=42", "AGENT_REPL_TEST_PROGRAMS=/p",
		"GOENV=off"}
	if !slices.Equal(got, want) {
		t.Fatalf("ConfinedEnv = %v, want %v", got, want)
	}
}

func TestKeyedEnvHoldsOnlyKeyedValues(t *testing.T) {
	// Act
	got := KeyedEnv([]string{"PATH=/bin", "GOFLAGS=-p=1", "TMPDIR=/t", "AGENT_REPL_LOG_LEVEL=debug", "GOPRIVATE=x", "LANG=C", "GOENV=/e"})

	// Assert
	want := map[string]string{"PATH": "/bin", "GOFLAGS": "-p=1", "GOENV": "off"}
	if !maps.Equal(got, want) {
		t.Fatalf("KeyedEnv = %v, want %v", got, want)
	}
}

// TestTheConfinedEnvironmentIsTheSameUnderEveryStartingProcess pins the
// canonical environment: a run started by the daemon (Doom's env file) and
// one started by the tests service (launchd) give a unit the same variables
// and the same key, however their module-fetch, locale and go env file
// settings differ.
func TestTheConfinedEnvironmentIsTheSameUnderEveryStartingProcess(t *testing.T) {
	// Arrange
	shared := []string{"HOME=/h", "PATH=/bin", "USER=u", "LOGNAME=u", "SHELL=/bin/zsh"}
	daemon := append(slices.Clone(shared), "GOPRIVATE=example.com/*", "LANG=en_US.UTF-8", "LC_TERMINAL=iTerm2")
	service := append(slices.Clone(shared), "XPC_SERVICE_NAME=agent-repl-tests", "GOENV=/elsewhere")

	// Act
	keyDaemon, keyService := KeyedEnv(daemon), KeyedEnv(service)
	envDaemon, envService := ConfinedEnv(daemon, nil), ConfinedEnv(service, nil)

	// Assert
	if !maps.Equal(keyDaemon, keyService) {
		t.Errorf("KeyedEnv differs: daemon %v, service %v", keyDaemon, keyService)
	}
	if !slices.Equal(envDaemon, envService) {
		t.Errorf("ConfinedEnv differs: daemon %v, service %v", envDaemon, envService)
	}
}

func TestKeyedEnvKeysEveryGoSettingThatChangesABuild(t *testing.T) {
	tests := []string{"GOFLAGS", "GOEXPERIMENT", "GODEBUG", "GOARCH", "GOTOOLCHAIN", "CGO_ENABLED", "CGO_CFLAGS"}
	for _, name := range tests {
		t.Run(name, func(t *testing.T) {
			// Act
			a := KeyedEnv([]string{name + "=a"})
			b := KeyedEnv([]string{name + "=b"})

			// Assert
			if maps.Equal(a, b) {
				t.Fatalf("KeyedEnv keys %s alike for two values: %v", name, a)
			}
		})
	}
}

func TestCanonicalOver(t *testing.T) {
	tests := []struct {
		name    string
		environ []string
		want    []string
	}{
		{"no canonical name inherited", []string{"A=1"}, []string{"A=1", "GOENV=off"}},
		{"a canonical name inherited", []string{"GOENV=/e", "A=1"}, []string{"A=1", "GOENV=off"}},
		{"nothing inherited", nil, []string{"GOENV=off"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := CanonicalOver(tt.environ)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("CanonicalOver = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestConfinedEnvSetsTheCanonicalEnvironmentThroughCanonicalOver pins that
// ConfinedEnv's canonical part is CanonicalOver's, so a hand-rolled override
// that drifts from the one the go list uses fails here.
func TestConfinedEnvSetsTheCanonicalEnvironmentThroughCanonicalOver(t *testing.T) {
	// Arrange
	environ := []string{"HOME=/h", "GOENV=/e", "GOFLAGS=-p=1"}

	// Act
	got := ConfinedEnv(environ, nil)

	// Assert
	if want := CanonicalOver([]string{"HOME=/h", "GOENV=/e", "GOFLAGS=-p=1"}); !slices.Equal(got, want) {
		t.Fatalf("ConfinedEnv = %v, want CanonicalOver's %v", got, want)
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
