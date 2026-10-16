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

// TestEmacsLauncherArgvCarriesBothAccountRoots is scenario 2, and it is THE
// account-flags regression pinned: the defect reached the user's live logs
// and SPEC.md §G raised it as unresolved, because only a test where Emacs
// spawns the daemon can see the argv the launcher actually composed.
func TestEmacsLauncherArgvCarriesBothAccountRoots(t *testing.T) {
	t.Parallel()
	w := coldStartWorld(t)
	e := w.Emacs

	e.EnsureDaemon()

	pid := daemonPID(e)
	if pid == 0 {
		t.Fatal("no live daemon process to read an argv from")
	}
	argv := procFields(t, pid, "cmdline")

	for _, tc := range []struct {
		name string
		flag string
		want string
	}{
		{name: "the default account root", flag: "--default-config-dir", want: e.DefaultConfigDir},
		{name: "the multi-repo account root", flag: "--multi-repo-config-dir", want: e.MultiRepoConfigDir},
	} {
		t.Run(tc.name, func(t *testing.T) {
			value, ok := flagValue(argv, tc.flag)
			if !ok {
				t.Fatalf("the launcher's argv carries no %s; argv was %q", tc.flag, argv)
			}
			// `expand-file-name`d, per the launcher's own contract: the
			// daemon compares config roots against resolved workspace
			// paths, so a `~' the shell never saw would never match.
			if !filepath.IsAbs(value) {
				t.Fatalf("%s was passed unexpanded as %q", tc.flag, value)
			}
			if value != filepath.Clean(tc.want) {
				t.Fatalf("%s = %q, want %q", tc.flag, value, filepath.Clean(tc.want))
			}
		})
	}
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
