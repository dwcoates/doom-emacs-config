package e2e

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// THE SANDBOX SEAM.
//
// The Emacs client layer runs Emacs inside the container sandbox at
// `modules/app/agent-repl/e2e/sandbox/`, because it starts a REAL editor that
// spawns a REAL daemon: nothing it does may reach the host's own Emacs,
// `~/.claude`, `~/.emacs.d` or `~/.config`, and nothing it writes may land
// outside a scratch directory that is swept on the way out.
//
// THE SANDBOX'S MODEL IS INSIDE-OUT FROM WHAT EMACS-LAYER-SPEC.md FIRST
// ASSUMED, and this file is written to the model that actually shipped.
// `bin/e2e-sandbox.sh` has four verbs -- build, run, shell, preflight -- and
// `run` is a one-shot `docker run --rm`: there is no persistent container and
// no `exec` verb. Its documented usage is to run the WHOLE test binary inside
// the container (`e2e-sandbox.sh run --dir e2e go test ./`).
//
// So this layer does not drive the container from the host. It detects
// whether IT IS ITSELF running inside the sandbox, and every Exec is then an
// ordinary local process. That needs nothing new from the sandbox: `/tmp` is
// already a writable exec tmpfs swept with the container, and a pty comes
// from `script`, which is Debian-essential.
//
// Run on the HOST, every scenario skips loudly and says how to run it
// properly. With no usable image, preflight's own message is quoted VERBATIM,
// per the sandbox README: "The harness must turn a non-zero exit into a loud
// skip that quotes this output verbatim -- never a silent pass, and never a
// fallback to an unsandboxed run."

// sandbox is the container the Emacs client layer runs inside.
type sandbox interface {
	// Available reports whether a container-sandboxed Emacs can run here,
	// and why not when it cannot. The reason is used VERBATIM in the skip.
	Available() (ok bool, reason string)

	// HasEmacs reports whether the image carries an Emacs new enough for
	// this module -- `tab-bar-tabs` is required, so 27 or later -- and the
	// version string it found either way.
	HasEmacs() (ok bool, version string)

	// Scratch is an absolute, writable directory, unique per test and swept
	// on cleanup. EVERYTHING this layer writes lives under it.
	Scratch() string

	// Exec runs one short command and returns its combined output. This is
	// how `emacsclient` is invoked, so it is on the hot path of every
	// readback and every heartbeat probe.
	Exec(ctx context.Context, argv ...string) (string, error)

	// StartPTY launches a long-lived process attached to a pty. Emacs is
	// started this way so it keeps a controlling terminal for its stdin and
	// so everything it writes outside its GUI frame -- GTK diagnostics, an
	// early elisp backtrace, the reason a boot died -- lands in one buffer
	// the failure artifacts can carry.
	StartPTY(ctx context.Context, argv ...string) (sandboxProc, error)

	// StartProcess launches a long-lived process with NO terminal, routing
	// its stdout and stderr to the two files given. Xvfb is started this
	// way: it needs no tty, and its two output streams have to stay apart
	// because `-displayfd 1` reports the bound display on stdout while
	// every diagnostic goes to stderr.
	StartProcess(ctx context.Context, stdout, stderr *os.File, argv ...string) (sandboxProc, error)
}

// sandboxProc is a process running inside the sandbox.
type sandboxProc interface {
	// Kill terminates the process and reaps it. Idempotent: the Emacs
	// teardown path calls it unconditionally after asking Emacs to exit
	// politely, precisely so a WEDGED Emacs cannot leak its daemon.
	Kill()

	// Exited reports whether the process is already gone, so a scenario can
	// fail loudly when Emacs died before its own cleanup ran instead of
	// reporting the timeout that death causes downstream.
	Exited() bool

	// Output returns whatever the pty has produced so far, for artifacts.
	Output() string

	// Pid is the process id, so a caller can declare the process its OWN
	// through harness.SpareFromStrayReaping. Zero once the process has been
	// reaped, which is never a pid anything may be declared under.
	Pid() int
}

// sandboxScriptRel is the sandbox entry point, relative to the repo root.
const sandboxScriptRel = "modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh"

// insideSandboxEnv is set by `e2e-sandbox.sh run` on every container it
// starts, and is this layer's primary in-container signal.
const insideSandboxEnv = "AGENT_REPL_SANDBOX_SHA"

// sandboxReadOnlyMount is where the sandbox bind-mounts the repo read-only.
// Checked alongside the env var so a stray environment variable on the host
// cannot convince this layer it is containerized.
const sandboxReadOnlyMount = "/repo-src"

// requireSandbox returns the sandbox for one test, or skips loudly.
func requireSandbox(t *testing.T) sandbox {
	t.Helper()
	s := &localSandbox{t: t}
	ok, reason := s.Available()
	if !ok {
		noteEnvironmentSkipAs(t,
			"the Emacs client layer runs only INSIDE the e2e sandbox container "+
				"(`"+sandboxRunCommand("<name>")+"`); it was not exercised by this run",
			"emacs client layer needs the e2e sandbox: %s", reason)
	}
	ok, version := s.HasEmacs()
	if !ok {
		requireDependency(t, "emacs client layer needs Emacs 27 or later in the sandbox image; found %q "+
			"(the image installs it; rebuild it with `%s build`)", version, sandboxScriptRel)
	}
	// `script` is INSTALLED BY NAME in the image now (bsdutils + util-linux
	// in the Dockerfile's apt list), so this is a backstop rather than the
	// guarantee: an image built before that change, or a base image that
	// drops it, must skip loudly instead of failing obscurely inside
	// StartPTY.
	if _, err := exec.LookPath("script"); err != nil {
		requireDependency(t, "emacs client layer needs 'script' for a pty (the Dockerfile installs bsdutils/util-linux for it; rebuild the image with `%s build`): %v", sandboxScriptRel, err)
	}
	// Xvfb is what makes a GRAPHICAL frame possible, and the panel this
	// layer opens is an `xwidget-webkit` webview, which cannot exist without
	// one. The Dockerfile installs it by name and its build asserts it, so
	// this is a backstop against an older image rather than the guarantee --
	// but without it every scenario would fail at `make-xwidget` with "GTK
	// has not been initialized", which names nothing that is actually wrong.
	if _, err := exec.LookPath("Xvfb"); err != nil {
		requireDependency(t, "emacs client layer needs 'Xvfb' for a GUI frame (the panel is an xwidget-webkit webview; the Dockerfile installs xvfb for it; rebuild the image with `%s build`): %v", sandboxScriptRel, err)
	}
	assertHostIsolation(t)
	return s
}

// containerHomePrefix is where the image puts its container-local HOME. It
// exists only inside the image and no mount ever covers it, which is what
// keeps the host's own HOME out of reach.
const containerHomePrefix = "/sandbox/"

// assertHostIsolation fails the test unless the four isolation properties
// the sandbox README claims are TRUE OF THIS PROCESS.
//
// The properties are enforced by the run script's `docker run` flags, which
// means a test invoked some other way -- a hand-rolled `docker run`, a
// future runner, a changed flag -- could satisfy `insideSandbox()` and still
// be able to write to the host. This layer starts a real Emacs that spawns a
// real daemon, so "probably isolated" is not good enough: it is checked from
// the inside, once, before anything starts.
func assertHostIsolation(t *testing.T) {
	t.Helper()

	// 1. HOME is the image's, not a host path.
	home := os.Getenv("HOME")
	if !strings.HasPrefix(home, containerHomePrefix) {
		t.Fatalf("sandbox isolation: HOME=%q is not under %s; the container-local HOME is not in effect",
			home, containerHomePrefix)
	}

	mounts, err := mountFilesystems()
	if err != nil {
		t.Fatalf("sandbox isolation: read the mount table: %v", err)
	}

	// 2. HOME and /tmp are container tmpfs, so everything this layer writes
	//    lives in the runtime's memory and dies with the container. /tmp is
	//    where Scratch() puts every file the Emacs layer creates.
	for _, dir := range []string{"/tmp", home} {
		fstype, ok := mounts[dir]
		if !ok {
			t.Fatalf("sandbox isolation: %s is not a mount point of its own; expected a container tmpfs", dir)
		}
		if fstype != "tmpfs" {
			t.Fatalf("sandbox isolation: %s is a %q mount, not tmpfs", dir, fstype)
		}
	}

	// 3. The repo mount is READ-ONLY. The entrypoint checks this too; it is
	//    re-checked here because the check that matters is the one in the
	//    process that is about to write.
	if _, ok := mounts[sandboxReadOnlyMount]; !ok {
		t.Fatalf("sandbox isolation: %s is not a mount point; the repo is not bind-mounted", sandboxReadOnlyMount)
	}
	probe := filepath.Join(sandboxReadOnlyMount, ".e2e-write-probe")
	if err := os.WriteFile(probe, []byte("x"), 0o644); err == nil {
		_ = os.Remove(probe)
		t.Fatalf("sandbox isolation: %s is WRITABLE; the repo must be mounted read-only", sandboxReadOnlyMount)
	}
}

// mountFilesystems maps each mount point to its filesystem type, read from
// the kernel's own view rather than inferred from the flags a runner passed.
func mountFilesystems() (map[string]string, error) {
	body, err := os.ReadFile("/proc/self/mountinfo")
	if err != nil {
		return nil, err
	}
	out := map[string]string{}
	for _, line := range strings.Split(string(body), "\n") {
		// mountinfo: ... 4:mount-point ... - fstype source [super-options]
		fields := strings.Fields(line)
		if len(fields) < 5 {
			continue
		}
		sep := -1
		for i, f := range fields {
			if f == "-" {
				sep = i
				break
			}
		}
		if sep < 0 || sep+1 >= len(fields) {
			continue
		}
		out[unescapeMountPath(fields[4])] = fields[sep+1]
	}
	return out, nil
}

// unescapeMountPath undoes mountinfo's octal escaping of space, tab,
// newline and backslash.
func unescapeMountPath(s string) string {
	for from, to := range map[string]string{`\040`: " ", `\011`: "\t", `\012`: "\n", `\134`: `\`} {
		s = strings.ReplaceAll(s, from, to)
	}
	return s
}

// localSandbox is the sandbox as seen from INSIDE it: every process it starts
// is an ordinary child of the test binary, which is itself containerized.
type localSandbox struct {
	t *testing.T
}

// scratchEntry is one test's scratch directory, made once.
type scratchEntry struct {
	once sync.Once
	dir  string
}

// scratchByTest holds each running test's scratch, keyed by its *testing.T.
//
// THE SCRATCH IS THE TEST'S, NOT THE SANDBOX VALUE'S. A scenario helper
// (newEmacsScenario) and the test that called it each call requireSandbox,
// and with the directory held on the value each got its OWN scratch: the
// world's Emacs root in one, the test's second repository in another. The
// daemon exempts exactly one directory from its temporary-folder refusal --
// the scratch the world states (StartEmacs) -- so the second repository was
// refused and the scenario never saw its workspace (2026-10-06).
var scratchByTest sync.Map

// Available reports readiness, and is deliberately three-way.
//
// The three cases have completely different answers, so a reader of a skipped
// run must be able to tell them apart:
//   - inside the sandbox: ready.
//   - on the host with a usable image: the test was invoked the wrong way, and
//     the skip names the command that invokes it the right way.
//   - on the host with no usable image: preflight's own message, verbatim.
//
// hostPreflightOnce guards the one preflight a host-side process performs;
// hostPreflightReason is empty when the image is usable.
var (
	hostPreflightOnce   sync.Once
	hostPreflightReason string
)

func (s *localSandbox) Available() (bool, string) {
	if insideSandbox() {
		return true, ""
	}

	// The host-side verdict is the same for every scenario in one process,
	// and preflight talks to Docker (tens of seconds under load), so it runs
	// ONCE; only the per-test instruction below is composed per test.
	hostPreflightOnce.Do(func() {
		hostPreflightReason = runHostPreflight(
			filepath.Join(repoRoot(), sandboxScriptRel), hostPreflightTimeout)
	})
	if hostPreflightReason != "" {
		return false, hostPreflightReason
	}

	return false, fmt.Sprintf(
		"the sandbox image is ready, but this test process is running ON THE HOST. "+
			"This layer starts a real Emacs that spawns a real daemon, so it must run INSIDE the container. Run:\n"+
			"    %s",
		sandboxRunCommand(s.t.Name()))
}

// hostPreflightTimeout bounds the WHOLE preflight run, so a preflight either
// answers or is reported as not answering. It never blocks a test process.
//
// Observed 2026-09-12: with Docker Desktop's backend alive but its socket
// never answering, this exec held the sync.Once above for 9m34s, every
// TestEmacs* queued behind it, and the package died on `go test`'s 10m
// timeout with no verdict at all.
//
// The derivation: preflight makes two bounded runtime calls, each capped at
// AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS (default 10s, itself 3x the ~3s
// a healthy loaded `docker info` costs -- it is sub-second on an idle box,
// and this script's own callers describe preflight as "tens of seconds under
// load"). Two calls plus the script's own startup is 20s worst case, so 30s
// here EXCEEDS the script's internal bound on purpose: when the script can
// diagnose the wedge itself, its own actionable message wins, and this bound
// is only the backstop for a script that cannot return at all.
const hostPreflightTimeout = 30 * time.Second

// runHostPreflight runs the sandbox preflight under a deadline and returns the
// skip reason, or "" when the sandbox image is usable.
//
// It is a free function rather than an inlined closure so a test can drive it
// without racing the package-level sync.Once that guards the real probe.
func runHostPreflight(script string, bound time.Duration) string {
	if _, err := os.Stat(script); err != nil {
		return fmt.Sprintf("%s not found: %v", sandboxScriptRel, err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	return hostPreflightUntil(ctx, script, bound)
}

// hostPreflightUntil is runHostPreflight with its bound as a context: the
// preflight is reported as not answering the moment ctx is done, and bound
// only names the wait in that report. A test ends the wait itself, at a
// point it has observed, rather than racing a wall-clock bound.
func hostPreflightUntil(ctx context.Context, script string, bound time.Duration) string {
	out, err := exec.CommandContext(ctx, script, "preflight").CombinedOutput()
	partial := strings.TrimRight(string(out), "\n")

	if ctx.Err() != nil {
		// The bound fired. Say so, and carry whatever partial output there
		// was: a preflight that got as far as naming its runtime tells the
		// reader which engine is wedged.
		reason := fmt.Sprintf(
			"the sandbox preflight (%s preflight) did not answer within %s, so the sandbox is "+
				"presumed unusable and this layer never falls back to an unsandboxed run. "+
				"The container runtime's engine is likely wedged (Docker Desktop backend alive "+
				"but the socket unresponsive): restart Docker Desktop, then re-run.",
			sandboxScriptRel, bound)
		if partial != "" {
			return reason + "\nPartial preflight output before the bound expired:\n" + partial
		}
		return reason + "\nThe preflight produced no output before the bound expired."
	}

	if err != nil {
		// Quoted VERBATIM: preflight's message is actionable (start
		// Docker, build the image) and paraphrasing it would throw away
		// the only instructions the reader needs.
		return fmt.Sprintf(
			"the sandbox is not usable, and this layer never falls back to an unsandboxed run.\n%s",
			partial)
	}
	return ""
}

// sandboxRunCommand spells the module-aware invocation a host-side skip hands
// back. The module root has no go.mod; the sandbox must enter e2e before Go
// can discover e2e/go.mod.
func sandboxRunCommand(testPattern string) string {
	return fmt.Sprintf("%s run --dir e2e go test ./ -run %s -v", sandboxScriptRel, testPattern)
}

func TestSandboxRunCommandEntersTheE2EModule(t *testing.T) {
	// Arrange.
	const pattern = "TestEmacsExample"

	// Act.
	got := sandboxRunCommand(pattern)

	// Assert.
	want := sandboxScriptRel + " run --dir e2e go test ./ -run TestEmacsExample -v"
	if got != want {
		t.Fatalf("sandbox run command = %q, want %q", got, want)
	}
}

// repoRoot is the checkout root, three levels above the module.
func repoRoot() string {
	return filepath.Clean(filepath.Join(repo.repoDir, "..", "..", ".."))
}

// insideSandbox reports whether this process is running in the sandbox. It
// requires BOTH signals: the env var the runner sets, and the read-only mount
// the entrypoint verifies.
func insideSandbox() bool {
	if os.Getenv(insideSandboxEnv) == "" {
		return false
	}
	info, err := os.Stat(sandboxReadOnlyMount)
	return err == nil && info.IsDir()
}

// HasEmacs asks the Emacs in the image for its own version.
//
// The requirement is 27 or later, for `tab-bar-tabs`. The image now builds
// Emacs 30.2 from source (with xwidgets and native-comp), so `--init-directory`
// (Emacs 29+) is available, but the layer still aims Emacs at its Doom tree
// through HOME instead: the container runs `--read-only` and the staged
// `/sandbox/emacs.d` is not a tmpfs mount, so that choice is not a version
// workaround and does not go away on a newer Emacs.
func (s *localSandbox) HasEmacs() (bool, string) {
	if !insideSandbox() {
		return false, "not inside the sandbox"
	}
	out, err := exec.Command("emacs", "--version").CombinedOutput()
	if err != nil {
		return false, fmt.Sprintf("emacs --version failed: %v", err)
	}
	version := strings.TrimSpace(strings.SplitN(string(out), "\n", 2)[0])
	major, ok := majorVersion(version)
	if !ok {
		return false, version
	}
	return major >= 27, version
}

// majorVersion pulls the leading major number out of "GNU Emacs 28.2".
func majorVersion(line string) (int, bool) {
	for _, field := range strings.Fields(line) {
		head := field
		if dot := strings.IndexByte(head, '.'); dot >= 0 {
			head = head[:dot]
		}
		if n, err := strconv.Atoi(head); err == nil {
			return n, true
		}
	}
	return 0, false
}

// Scratch is a per-TEST directory under /tmp (every sandbox value one test
// holds answers the same one), which the sandbox mounts as a
// writable exec tmpfs and which dies with the container. It is removed on
// cleanup as well, so a `-count=N` run does not accumulate.
func (s *localSandbox) Scratch() string {
	held, _ := scratchByTest.LoadOrStore(s.t, &scratchEntry{})
	entry := held.(*scratchEntry)
	entry.once.Do(func() {
		dir, err := os.MkdirTemp("/tmp", "emacs-e2e-")
		if err != nil {
			s.t.Fatalf("e2e: create the sandbox scratch directory: %v", err)
		}
		entry.dir = dir
		s.t.Cleanup(func() {
			_ = os.RemoveAll(dir)
			scratchByTest.Delete(s.t)
		})
	})
	return entry.dir
}

func (s *localSandbox) Exec(ctx context.Context, argv ...string) (string, error) {
	if len(argv) == 0 {
		return "", fmt.Errorf("exec: no command given")
	}
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	out, err := cmd.CombinedOutput()
	return string(out), err
}

// StartPTY runs argv under `script`, which allocates a pty.
//
// A pty is what gives Emacs a REAL tty frame. `e2e-sandbox.sh run` only
// passes `-t` when its own stdout is a terminal, and under `go test` it is
// not, so the container's stdin/stdout cannot be relied on to be a terminal:
// this layer allocates its own rather than depending on how it was invoked.
func (s *localSandbox) StartPTY(ctx context.Context, argv ...string) (sandboxProc, error) {
	if len(argv) == 0 {
		return nil, fmt.Errorf("startpty: no command given")
	}

	// `script -q -c CMD /dev/null` runs CMD on a pty and discards the
	// typescript. CMD is one shell word, so the argv is quoted for the shell
	// rather than passed through; every element is a path or a flag this
	// layer composed itself.
	quoted := make([]string, 0, len(argv))
	for _, a := range argv {
		quoted = append(quoted, shellQuote(a))
	}
	cmd := exec.CommandContext(ctx, "script", "-q", "-c", strings.Join(quoted, " "), "/dev/null")

	p := &localProc{done: make(chan struct{})}
	cmd.Stdout = &p.out
	cmd.Stderr = &p.out
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("start %q on a pty: %w", argv[0], err)
	}
	p.cmd = cmd
	go func() {
		_ = cmd.Wait()
		close(p.done)
	}()
	return p, nil
}

// StartProcess runs argv directly, with its two streams routed to files.
//
// Unlike StartPTY it allocates no pty, so the started process's Output() is
// empty by construction: everything it says is in the files the caller
// opened, which is what makes an Xvfb log preservable as its own artifact.
func (s *localSandbox) StartProcess(ctx context.Context, stdout, stderr *os.File, argv ...string) (sandboxProc, error) {
	if len(argv) == 0 {
		return nil, fmt.Errorf("startprocess: no command given")
	}
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdout = stdout
	cmd.Stderr = stderr

	p := &localProc{done: make(chan struct{})}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("start %q: %w", argv[0], err)
	}
	p.cmd = cmd
	go func() {
		_ = cmd.Wait()
		close(p.done)
	}()
	return p, nil
}

// shellQuote renders one argv element as a single shell word.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// localProc is a process started by localSandbox.
type localProc struct {
	cmd  *exec.Cmd
	done chan struct{}
	out  syncBuffer
}

func (p *localProc) Kill() {
	if p.Exited() {
		return
	}
	if p.cmd.Process != nil {
		_ = p.cmd.Process.Kill()
	}
	<-p.done
}

func (p *localProc) Exited() bool {
	select {
	case <-p.done:
		return true
	default:
		return false
	}
}

func (p *localProc) Output() string { return p.out.String() }

func (p *localProc) Pid() int {
	if p.cmd == nil || p.cmd.Process == nil {
		return 0
	}
	return p.cmd.Process.Pid
}

// syncBuffer is an io.Writer safe for the pty reader goroutine to write while
// a test reads it for failure artifacts.
type syncBuffer struct {
	mu  sync.Mutex
	buf []byte
}

func (b *syncBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.buf = append(b.buf, p...)
	return len(p), nil
}

func (b *syncBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return string(b.buf)
}
