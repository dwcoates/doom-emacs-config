package reuse

import (
	"errors"
	"fmt"
	"os"
	"sort"
	"strings"
	"syscall"

	"agentrepl/testrun/internal/cores"
)

// SandboxExec is the seatbelt launcher every keyed unit starts under.
const SandboxExec = "/usr/bin/sandbox-exec"

// A KEYED UNIT RUNS IN ONE CANONICAL ENVIRONMENT, whichever process started
// its run. A workspace's run inherits the daemon's environment (Doom's saved
// env file) and a merge gate's the launchd tests service's, and the two
// differ: on 2026-11-20 every daemon-started run carried GOPRIVATE and LANG
// and every gate run neither, so a gate keyed on records a workspace made
// reran every unit ("@env GOPRIVATE and 1 more"). So an inherited variable is
// kept only when its value can change what a test observes, and then its
// value is part of the key (keyedEnvNames, keyedEnvPrefixes); a variable that
// only names where one run puts things is kept unkeyed (runScopedEnvNames);
// the runner sets canonicalEnv itself, over whatever was inherited; and
// everything else, droppedGoEnvNames and the locale and zone variables
// included, never reaches a unit. Which variables a unit's tests read cannot
// be enumerated soundly (any library may call getenv), so the environment is
// fixed instead of the key narrowed.

// keyedEnvNames are the inherited variables a keyed unit keeps whose VALUES
// are part of the key: who and where the host's user is, and what the unit's
// commands resolve to.
var keyedEnvNames = map[string]bool{
	"HOME": true, "PATH": true, "USER": true, "LOGNAME": true, "SHELL": true,
}

// keyedEnvPrefixes extend keyedEnvNames to whole families: the Go
// toolchain's own settings decide what a build compiles (GOFLAGS,
// GOEXPERIMENT, GODEBUG, CGO_ENABLED), so a change to one reruns every unit.
var keyedEnvPrefixes = []string{"GO", "CGO_"}

// droppedGoEnvNames are the Go settings that only govern fetching modules,
// never what a build compiles: every module's bytes are pinned by go.sum,
// which is itself an input, so a fetch either yields those bytes or fails.
// They are dropped rather than keyed, so a host whose processes disagree on
// them (GOPRIVATE, set in a shell and in Doom's env file but not in launchd's)
// keys alike. GOENV names the go env file, which canonicalEnv turns off.
var droppedGoEnvNames = map[string]bool{
	"GOPRIVATE": true, "GONOPROXY": true, "GONOSUMDB": true, "GONOSUMCHECK": true,
	"GOPROXY": true, "GOSUMDB": true, "GOINSECURE": true, "GOAUTH": true, "GOVCS": true,
	"GOENV": true,
}

// canonicalEnv are the variables the runner sets for every keyed unit, over
// any inherited value, and keys like any other.
//
// GOENV=off: the go env file (`go env -w`) is read by every go command and is
// no input the key reads, so a GOFLAGS written there would change a build
// without changing its key. Off, a Go setting reaches a unit only as an
// inherited variable, which the key holds.
var canonicalEnv = map[string]string{
	"GOENV": "off",
}

// runScopedEnvNames are the inherited variables a keyed unit keeps whose
// values are NOT part of the key: each names one run's own directories or
// conditions (its temp root, its RAM disk, its logs, its priority), which a
// test uses to place things and never as an input.
var runScopedEnvNames = map[string]bool{
	"TMPDIR":                           true,
	"AGENT_REPL_TEST_SHORT_BASE":       true,
	"AGENT_REPL_TEST_RUN_LOG":          true,
	"AGENT_REPL_INTEGRATION_ARTIFACTS": true,
	"AGENT_REPL_E2E_ARTIFACTS":         true,
	"AGENT_REPL_PROFILE_ROOT":          true,
	cores.EnvBackground:                true,
	"AGENT_REPL_TEST_NICENESS":         true,
	// The run's mark on its units, so a run nested in one is refused.
	cores.NestedEnv: true,
}

// keyedEnv reports whether an inherited variable's value is part of the key.
func keyedEnv(name string) bool {
	if keyedEnvNames[name] {
		return true
	}
	if droppedGoEnvNames[name] {
		return false
	}
	for _, p := range keyedEnvPrefixes {
		if strings.HasPrefix(name, p) {
			return true
		}
	}
	return false
}

// KeyedEnv is the part of a keyed unit's environment whose values go into
// its key: the inherited variables Confine keeps for their value, and
// canonicalEnv.
func KeyedEnv(environ []string) map[string]string {
	out := map[string]string{}
	for _, kv := range environ {
		name, value, ok := strings.Cut(kv, "=")
		if ok && keyedEnv(name) {
			out[name] = value
		}
	}
	for name, value := range canonicalEnv {
		out[name] = value
	}
	return out
}

// ConfinedEnv filters a unit's environment to what a keyed unit may see: the
// keyed and run-scoped variables, and the unit's own variables (unitNames),
// which the runner sets from its roster entry and its run layout, then
// canonicalEnv, which overrides all three. Everything else the runner
// inherited (a session's own settings, an agent's markers, the locale) is
// dropped, so a test cannot depend on a variable the key does not cover.
func ConfinedEnv(environ []string, unitNames map[string]bool) []string {
	var out []string
	for _, kv := range environ {
		name, _, ok := strings.Cut(kv, "=")
		if !ok {
			continue
		}
		if keyedEnv(name) || runScopedEnvNames[name] || unitNames[name] {
			out = append(out, kv)
		}
	}
	return CanonicalOver(out)
}

// CanonicalOver is environ with canonicalEnv set over it: every inherited
// value of a canonical name replaced. Each Go command the runner starts for
// a keyed unit, the unit's own and the `go list` that derives its inputs,
// runs under it, so both see the same Go settings.
func CanonicalOver(environ []string) []string {
	var out []string
	for _, kv := range environ {
		name, _, _ := strings.Cut(kv, "=")
		if _, canonical := canonicalEnv[name]; !canonical {
			out = append(out, kv)
		}
	}
	names := make([]string, 0, len(canonicalEnv))
	for name := range canonicalEnv {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		out = append(out, name+"="+canonicalEnv[name])
	}
	return out
}

// ConfineCommand is the `testrun confine` line a keyed unit's argv becomes:
// the runner binary re-executes itself to scrub the environment, then execs
// the unit's own command under the suite's profile.
func ConfineCommand(self, profile string, unitEnv []string, argv []string) []string {
	var names []string
	for _, kv := range unitEnv {
		if name, _, ok := strings.Cut(kv, "="); ok {
			names = append(names, name)
		}
	}
	sort.Strings(names)
	out := []string{self, "confine", "--profile", profile, "--keep", strings.Join(names, ","), "--"}
	return append(out, argv...)
}

// ConfineArgs is a parsed `testrun confine` line.
type ConfineArgs struct {
	Profile string
	Keep    map[string]bool
	Argv    []string
}

// ParseConfine reads `confine --profile P --keep A,B -- argv...`.
func ParseConfine(args []string) (ConfineArgs, error) {
	var c ConfineArgs
	c.Keep = map[string]bool{}
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--profile":
			if i+1 >= len(args) {
				return ConfineArgs{}, errors.New("confine: --profile needs a path")
			}
			i++
			c.Profile = args[i]
		case "--keep":
			if i+1 >= len(args) {
				return ConfineArgs{}, errors.New("confine: --keep needs a comma-separated list")
			}
			i++
			for _, n := range strings.Split(args[i], ",") {
				if n != "" {
					c.Keep[n] = true
				}
			}
		case "--":
			c.Argv = args[i+1:]
			i = len(args)
		default:
			return ConfineArgs{}, fmt.Errorf("confine: unknown argument %q", args[i])
		}
	}
	if c.Profile == "" {
		return ConfineArgs{}, errors.New("confine: --profile is required")
	}
	if len(c.Argv) == 0 {
		return ConfineArgs{}, errors.New("confine: no command after --")
	}
	return c, nil
}

// Exec replaces the current process with c's command under its profile, in
// the scrubbed environment. It returns only on failure.
func Exec(c ConfineArgs, environ []string) error {
	argv := append([]string{SandboxExec, "-f", c.Profile}, c.Argv...)
	err := syscall.Exec(SandboxExec, argv, ConfinedEnv(environ, c.Keep))
	return fmt.Errorf("confine: exec %s: %w", strings.Join(argv, " "), err)
}

// ProbeSandbox reports whether this process may start a sandboxed child: a
// process already inside a seatbelt sandbox may not (sandbox_apply refuses a
// nested one), and then no suite can be confined, so none is reused or
// recorded this run.
func ProbeSandbox(run func(name string, args ...string) error) error {
	if _, err := os.Stat(SandboxExec); err != nil {
		return fmt.Errorf("reuse: %s is not available: %w", SandboxExec, err)
	}
	if err := run(SandboxExec, "-p", "(version 1)(allow default)", "/usr/bin/true"); err != nil {
		return fmt.Errorf("reuse: a seatbelt sandbox cannot be applied here (is this run itself sandboxed?): %w", err)
	}
	return nil
}
