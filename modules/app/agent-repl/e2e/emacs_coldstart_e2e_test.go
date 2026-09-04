package e2e

import (
	"strconv"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// AREA A of EMACS-LAYER-SPEC.md: cold start and the daemon launch, six
// scenarios, all Emacs-only.
//
// The whole area exists because EMACS composes the daemon's argv. A test
// that starts `claude-repld` from Go writes a SECOND spelling of the launch
// contract and can never disagree with the launcher — which is exactly how
// the missing account flags survived every direct test. Here the launcher
// performs the launch and the assertions read the OS's view of what it did.
//
// Area A asserts no keybinding: per the spec's "Which scenarios should
// assert a binding" table, "Area A (cold start) — none. Cold start has no
// keystroke."

// daemonLaunchBound bounds the launcher's own settling — a spawn that
// succeeded, or a refusal that was reported. It is the module's own boot
// budget (`emacsBootBound`); the launcher cannot outlive the budget it
// gives its own daemon, so a wait longer than this would be waiting for
// something the launcher has already given up on.
const daemonLaunchBound = emacsBootBound

// daemonStateBound bounds the daemon's own first writes under the state root
// (`logs/`), which follow the spawn. Same budget, same reason.
const daemonStateBound = emacsBootBound

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

func strconv.Itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var digits []byte
	for n > 0 {
		digits = append([]byte{byte('0' + n%10)}, digits...)
		n /= 10
	}
	return string(digits)
}

// TestEmacsColdStartBuildsAndSpawnsTheDaemon is scenario 1.
func TestEmacsColdStartBuildsAndSpawnsTheDaemon(t *testing.T) {
	w := coldStartWorld(t)
	e := w.Emacs

	e.EnsureDaemon()

	if !e.EvalBool(`(and agent-repl--frontend-daemon-process
                         (process-live-p agent-repl--frontend-daemon-process)
                         t)`) {
		t.Fatal("emacs holds no live daemon process after its own cold-start ensure")
	}
	if !e.EvalBool(`(and agent-repl-link--primary t)`) {
		t.Fatal("emacs holds no primary link after the ensure settled")
	}
	if addr := e.DaemonAddr(); addr == "" {
		t.Fatal("the daemon published an empty address into daemon.addr under the state root")
	}
}

// TestEmacsLauncherArgvCarriesBothAccountRoots is scenario 2, and it is THE
// account-flags regression pinned: the defect reached the user's live logs
// and SPEC.md §G raised it as unresolved, because only a test where Emacs
// spawns the daemon can see the argv the launcher actually composed.
func TestEmacsLauncherArgvCarriesBothAccountRoots(t *testing.T) {
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

// TestEmacsLauncherRefusesWithoutAnAccountRoot is scenario 3. It pairs with
// daemon/integration/boot_test.go's TestBootRefusesWithoutAnAccountRoot from
// the other side: the daemon exits 2, and the launcher refuses to spend the
// boot timeout finding that out.
func TestEmacsLauncherRefusesWithoutAnAccountRoot(t *testing.T) {
	w := coldStartWorld(t)
	e := w.Emacs

	// Ordinary configuration, set the way a user's own `setq' sets it.
	e.Eval(`(setq agent-repl-daemon-default-config-dir "")`)

	e.Eval(`(agent-repl-frontend-daemon-ensure)`)

	e.AwaitEvalFor(daemonLaunchBound, "the launcher to report a launch failure",
		`agent-repl-daemon-launch-failure`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	if pid := daemonPID(e); pid != 0 {
		t.Fatalf("the launcher spawned a daemon (pid %d) with no default account root", pid)
	}
	missing := e.EvalStrings(`(agent-repl-daemon--missing-config-flags)`)
	if len(missing) == 0 {
		t.Fatal("the launcher named no missing account flag")
	}
	named := false
	for _, m := range missing {
		if strings.Contains(m, "--default-config-dir") {
			named = true
		}
	}
	if !named {
		t.Fatalf("the missing flags %q do not name --default-config-dir", missing)
	}
}

// TestEmacsAdoptsAnAlreadyAnsweringDaemon is scenario 4. Per elisp.md, Emacs
// adopts any daemon that answers DaemonHealth, healthy or not, and NEVER
// kills one.
func TestEmacsAdoptsAnAlreadyAnsweringDaemon(t *testing.T) {
	w := coldStartWorld(t)
	e := w.Emacs

	e.EnsureDaemon()
	first := daemonPID(e)
	if first == 0 {
		t.Fatal("the first ensure spawned no daemon")
	}
	addr := e.DaemonAddr()

	// A second ensure against a daemon that is already up and has already
	// published its address.
	e.EnsureDaemon()

	if second := daemonPID(e); second != first {
		t.Fatalf("the second ensure changed the daemon process: pid %d -> %d; the running daemon must be adopted, never replaced", first, second)
	}
	if got := e.DaemonAddr(); got != addr {
		t.Fatalf("the published daemon address changed across an adopting ensure: %q -> %q", addr, got)
	}
	if !e.EvalBool(`(and agent-repl-link--primary t)`) {
		t.Fatal("emacs dropped its primary link across an adopting ensure")
	}
}

// TestEmacsStateRootTravelsInTheEnvironment is scenario 5. ONE state root is
// the cross-system contract, so the launcher STATES it on the spawn rather
// than letting the child inherit whatever the session had.
func TestEmacsStateRootTravelsInTheEnvironment(t *testing.T) {
	w := coldStartWorld(t)
	e := w.Emacs

	e.EnsureDaemon()

	pid := daemonPID(e)
	if pid == 0 {
		t.Fatal("no live daemon process to read an environment from")
	}
	want := strings.TrimSuffix(e.EvalString(`(directory-file-name (agent-repl--global-state-dir))`), "/")
	if want == "" {
		t.Fatal("emacs resolved an empty global state dir")
	}

	env := procFields(t, pid, "environ")
	got, ok := envValue(env, "AGENT_REPL_STATE_DIR")
	if !ok {
		t.Fatal("the spawn environment carries no AGENT_REPL_STATE_DIR")
	}
	if strings.TrimSuffix(got, "/") != want {
		t.Fatalf("the daemon's AGENT_REPL_STATE_DIR = %q, want Emacs's own state root %q", got, want)
	}

	// And the daemon really writes under it: one state root, observed
	// rather than declared.
	logs := filepath.Join(want, "logs")
	deadline := time.After(daemonStateBound)
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if info, err := os.Stat(logs); err == nil && info.IsDir() {
			return
		}
		select {
		case <-deadline:
			t.Fatalf("the daemon's logs/ never materialized under the state root %s within %s", want, daemonStateBound)
		case <-ticker.C:
		}
	}
}

func envValue(env []string, key string) (string, bool) {
	for _, entry := range env {
		if strings.HasPrefix(entry, key+"=") {
			return strings.TrimPrefix(entry, key+"="), true
		}
	}
	return "", false
}

// TestEmacsBuildFailureSurfacesInTheModeline is scenario 6.
//
// This one reads a RENDERED STRING deliberately, and the spec sanctions it:
// the modeline segment's own composition IS the subject, so there is no
// variable to read instead.
func TestEmacsBuildFailureSurfacesInTheModeline(t *testing.T) {
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box)
	e := w.Emacs

	failing := filepath.Join(box.Scratch(), "build-fails.sh")
	if err := os.WriteFile(failing, []byte("#!/usr/bin/env bash\necho 'frontend build blew up' >&2\nexit 3\n"), 0o755); err != nil {
		t.Fatalf("write the failing build script: %v", err)
	}
	e.Eval(`(setq agent-repl-daemon-build-script ` + elispString(failing) + `)`)

	e.Eval(`(agent-repl-frontend-daemon-ensure)`)

	e.AwaitEvalFor(daemonLaunchBound, "the launcher to record a build failure",
		`agent-repl-daemon-build-failure`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	segment := e.EvalString(`(format "%s" (or agent-repl-daemon-mode-line-segment ""))`)
	if segment == "" {
		t.Fatal("the daemon modeline segment is empty after a failed build")
	}
	if !strings.Contains(strings.ToLower(segment), "build") {
		t.Fatalf("the modeline segment %q does not name the build failure", segment)
	}
}
