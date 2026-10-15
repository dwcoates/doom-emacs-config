package reuse

import (
	"errors"
	"fmt"
	"os"
	"sort"
	"strings"
	"syscall"
)

// SandboxExec is the seatbelt launcher every keyed unit starts under.
const SandboxExec = "/usr/bin/sandbox-exec"

// keyedEnvNames are the inherited variables a keyed unit keeps whose VALUES
// are part of the key: what a test could behave differently on.
var keyedEnvNames = map[string]bool{
	"HOME": true, "PATH": true, "USER": true, "LOGNAME": true, "SHELL": true,
	"LANG": true, "LC_ALL": true, "LC_CTYPE": true, "TZ": true,
}

// keyedEnvPrefixes extend keyedEnvNames to whole families: the Go
// toolchain's own settings decide what a build compiles.
var keyedEnvPrefixes = []string{"GO", "CGO_"}

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
	"AGENT_REPL_BACKGROUND_PRIORITY":   true,
	"AGENT_REPL_TEST_NICENESS":         true,
}

// keyedEnv reports whether an inherited variable's value is part of the key.
func keyedEnv(name string) bool {
	if keyedEnvNames[name] {
		return true
	}
	for _, p := range keyedEnvPrefixes {
		if strings.HasPrefix(name, p) {
			return true
		}
	}
	return false
}

// KeyedEnv is the part of environ whose values go into every keyed suite's
// key: exactly the inherited variables Confine keeps for their value.
func KeyedEnv(environ []string) map[string]string {
	out := map[string]string{}
	for _, kv := range environ {
		name, value, ok := strings.Cut(kv, "=")
		if ok && keyedEnv(name) {
			out[name] = value
		}
	}
	return out
}

// ConfinedEnv filters a unit's environment to what a keyed unit may see: the
// keyed and run-scoped variables, and the unit's own variables (unitNames),
// which the runner sets from its roster entry and its run layout. Everything
// else the runner inherited (a session's own settings, an agent's markers) is
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
