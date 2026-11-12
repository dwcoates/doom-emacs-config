package e2e

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// AREA A of EMACS-LAYER-SPEC.md: cold start and the daemon launch, six
// scenarios, all Emacs-only.
//
// The whole area exists because EMACS composes the daemon's argv. A test
// that starts `agent-repld` from Go writes a SECOND spelling of the launch
// contract and can never disagree with the launcher — which is exactly how
// the missing account flags survived every direct test. Here the launcher
// performs the launch and the assertions read the OS's view of what it did.
//
// Area A asserts no keybinding: per the spec's "Which scenarios should
// assert a binding" table, "Area A (cold start) — none. Cold start has no
// keystroke."

// coldStartWorld brings a world up with Emacs running and the daemon NOT
// started, which is every Area A scenario's starting point: each one drives
// `agent-repl-frontend-daemon-ensure` itself, after arranging the launcher's
// configuration for the case under test.
func coldStartWorld(t *testing.T) *EmacsWorld {
	t.Helper()
	return NewEmacsWorld(t, requireSandbox(t))
}

// daemonPID reads the launched daemon's OS process id out of Emacs's own
// process object. Nil (no process) reads back as JSON null and returns 0.
func daemonPID(e *Emacs) int {
	e.t.Helper()
	raw := e.Eval(`(and agent-repl--frontend-daemon-process
                        (process-live-p agent-repl--frontend-daemon-process)
                        (process-id agent-repl--frontend-daemon-process))`)
	if isJSONNull(raw) {
		return 0
	}
	var pid int
	if err := json.Unmarshal(raw, &pid); err != nil {
		return 0
	}
	return pid
}

// procFields reads one NUL-separated /proc file for a pid. The layer runs
// INSIDE the container that runs the daemon, so /proc is the daemon's own,
// and this is deliberately the OS's account of the spawn rather than a
// re-evaluation of `agent-repl-daemon--argv` — re-evaluating the builder
// would assert the builder against itself.
func procFields(t *testing.T, pid int, file string) []string {
	t.Helper()
	body, err := os.ReadFile(filepath.Join("/proc", strconv.Itoa(pid), file))
	if err != nil {
		t.Fatalf("read /proc/%d/%s for the daemon Emacs spawned: %v", pid, file, err)
	}
	return strings.FieldsFunc(string(body), func(r rune) bool { return r == 0 })
}

// flagValue returns the value following a `--flag` in an argv, either as the
// next element or as the `--flag=value` tail.
func flagValue(argv []string, flag string) (string, bool) {
	for i, arg := range argv {
		if arg == flag && i+1 < len(argv) {
			return argv[i+1], true
		}
		if strings.HasPrefix(arg, flag+"=") {
			return strings.TrimPrefix(arg, flag+"="), true
		}
	}
	return "", false
}
