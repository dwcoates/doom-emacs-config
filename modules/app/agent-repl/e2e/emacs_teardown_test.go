package e2e

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"
)

// TEARDOWN'S OWN TESTS.
//
// The guarantee under test is the one the Emacs layer had been missing: a
// scenario is not torn down until the process it started is GONE. It used to
// ask Emacs to exit, kill the pty parent, and assume; `script' forks Emacs
// into a session of its own, so the assumption was false and the reaper
// found a live 200 MiB Emacs after scenarios that otherwise passed.
//
// None of this starts a real Emacs. What teardown reasons about is a pid
// whose /proc entry says `comm' is "emacs" and whose environment carries this
// scenario's HOME -- so a copy of a small system binary UNDER THAT NAME is
// the same process to every line of code being tested, and it can be made to
// exit promptly, to refuse SIGTERM, or to hold a child in its process group
// on demand, which a real Emacs cannot.

// requireProcfs skips a teardown test where /proc cannot be read. Every
// scenario in this layer runs inside the Linux sandbox, where it can be.
func requireProcfs(t *testing.T) {
	t.Helper()
	if _, err := os.ReadFile("/proc/self/stat"); err != nil {
		t.Skipf("emacs teardown reads /proc, which this host does not serve: %v", err)
	}
}

// fakeEmacsBinary copies one system binary into dir under the name `emacs',
// so a process exec'd from it reports `emacs` as its /proc comm.
func fakeEmacsBinary(t *testing.T, dir, source string) string {
	t.Helper()
	body, err := os.ReadFile(source)
	if err != nil {
		t.Fatalf("read %s to stand in for emacs: %v", source, err)
	}
	path := filepath.Join(dir, "emacs")
	if err := os.WriteFile(path, body, 0o755); err != nil {
		t.Fatalf("write the stand-in emacs at %s: %v", path, err)
	}
	return path
}

// startFakeEmacs runs one stand-in Emacs with this scenario's HOME, in a
// process group of its own -- which is what `script' gives the real one, and
// what the escalation signals.
func startFakeEmacs(t *testing.T, root, bin string, args ...string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command(bin, args...)
	cmd.Env = []string{"HOME=" + root, "PATH=/usr/bin:/bin"}
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the stand-in emacs: %v", err)
	}
	waited := make(chan struct{})
	go func() {
		_ = cmd.Wait()
		close(waited)
	}()
	t.Cleanup(func() {
		// Belt for the test itself, never for the code under test: if an
		// assertion fails before teardown ran, the stand-in must not outlive
		// the test that started it.
		if cmd.Process != nil {
			_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		}
		<-waited
	})
	return cmd
}

// teardownFixture is an Emacs reduced to what teardown needs: the scenario
// root that identifies its processes.
func teardownFixture(t *testing.T) (*Emacs, string) {
	t.Helper()
	root, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("resolve the scenario root: %v", err)
	}
	return &Emacs{t: t, Root: root, reap: []string{root}}, root
}

func TestEmacsTeardownWaitsForTheProcessToExit(t *testing.T) {
	requireProcfs(t)
	// Arrange: a stand-in Emacs that is alive now and exits on its own
	// shortly, well inside emacsExitBound. `sleep' is chosen because its
	// exit is a clock rather than a signal, so nothing teardown does can
	// bring it forward: if the wait returns before that clock runs out, the
	// wait did not happen.
	e, root := teardownFixture(t)
	bin := fakeEmacsBinary(t, root, "/bin/sleep")
	// 350ms is inside emacsExitBound, so this exercises the polite wait and
	// not the escalation; 250ms is what the assertion holds the wait to,
	// leaving room for the arrange step's own /proc read. An implementation
	// that does not wait returns in microseconds, so the margin is ample.
	const minimumWait = 250 * time.Millisecond
	startFakeEmacs(t, root, bin, "0.35")
	if len(e.emacsProcs()) != 1 {
		t.Fatalf("arrange: want exactly one stand-in emacs before teardown, got %d", len(e.emacsProcs()))
	}

	// Act.
	started := time.Now()
	e.awaitEmacsExit(true)
	took := time.Since(started)

	// Assert: it returned only after the process was actually gone.
	if left := e.emacsProcs(); len(left) != 0 {
		t.Fatalf("awaitEmacsExit returned with %s still alive", pidList(left))
	}
	if took < minimumWait {
		t.Fatalf("awaitEmacsExit returned after %s, well before the process's own 350ms lifetime: it did not wait",
			took.Round(time.Millisecond))
	}
}

func TestEmacsTeardownEscalatesToTheProcessGroup(t *testing.T) {
	requireProcfs(t)
	// Arrange: a stand-in Emacs that REFUSES SIGTERM and holds a child that
	// refuses it too. Only a signal to the process GROUP reaches the child:
	// SIGKILL to the leader alone would leave it running, which is precisely
	// the reparenting trap this escalation exists for.
	e, root := teardownFixture(t)
	bin := fakeEmacsBinary(t, root, "/bin/sh")
	childPID := filepath.Join(root, "child.pid")
	script := filepath.Join(root, "hold.sh")
	// The child ignores SIGTERM too, and never execs, so its pid is stable
	// and nothing but SIGKILL removes it. Its own `sleep' is short and
	// re-run in a loop rather than long and waited on, because a long
	// `sleep' would die of the group's SIGTERM and let its parent's `wait'
	// return -- which would tear the tree down and let a pid-only
	// escalation pass this test.
	holdScript := filepath.Join(root, "child.sh")
	hold := "trap '' TERM\n" +
		"echo $$ > " + childPID + "\n" +
		"while :; do /bin/sleep 0.1; done\n"
	if err := os.WriteFile(holdScript, []byte(hold), 0o755); err != nil {
		t.Fatalf("write the refusing child script: %v", err)
	}
	body := "trap '' TERM\n" +
		"/bin/sh " + holdScript + " &\n" +
		"wait\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the refusing script: %v", err)
	}
	startFakeEmacs(t, root, bin, script)
	child := awaitPIDFile(t, childPID)

	// Act.
	e.awaitEmacsExit(true)

	// Assert: the leader is gone, and so is the child that only a group
	// signal could have reached.
	if left := e.emacsProcs(); len(left) != 0 {
		t.Fatalf("awaitEmacsExit left %s alive after escalating", pidList(left))
	}
	if state := liveProcessState(child); state != "" {
		t.Fatalf("the child in the process group (pid %d) is still running (/proc state %q): the escalation signalled the pid, not the group",
			child, state)
	}
	tried := e.teardownTried()
	for _, want := range []string{"process group", "terminated", "killed"} {
		if !strings.Contains(tried, want) {
			t.Fatalf("teardown recorded %q, which does not name %q", tried, want)
		}
	}
}

func TestEmacsTeardownReportsAStrayAsAFailure(t *testing.T) {
	requireProcfs(t)
	// Arrange: a process of this scenario that outlives teardown -- the
	// shape the reaper exists to catch -- and a teardown that has already
	// recorded what it tried.
	e, root := teardownFixture(t)
	var failures []string
	e.fail = func(format string, args ...any) { failures = append(failures, fmt.Sprintf(format, args...)) }
	e.noteTeardownStep("emacs exited on (kill-emacs) within 51ms")
	bin := fakeEmacsBinary(t, root, "/bin/sleep")
	cmd := startFakeEmacs(t, root, bin, "60")
	pid := cmd.Process.Pid

	// Act.
	e.reapStrays()

	// Assert: the scenario FAILED for it, the message names the pid and what
	// teardown had tried, and the process was still reaped.
	if len(failures) != 1 {
		t.Fatalf("reapStrays reported %d failures, want exactly 1: %v", len(failures), failures)
	}
	if !strings.Contains(failures[0], fmt.Sprintf("pid %d", pid)) {
		t.Fatalf("the failure does not name pid %d: %s", pid, failures[0])
	}
	if !strings.Contains(failures[0], "emacs exited on (kill-emacs) within 51ms") {
		t.Fatalf("the failure does not say what teardown had already tried: %s", failures[0])
	}
	if left := e.findStrays(); len(left) != 0 {
		t.Fatalf("reapStrays failed the scenario but left %s running, leaking it into the next one", pidList(left))
	}
}

// liveProcessState answers the /proc state letter of a RUNNING process, or
// "" when the pid names nothing runnable any more.
//
// A killed grandchild of this test is reparented rather than reaped by it, so
// it can linger as a zombie for as long as the container's init takes to
// collect it: `kill(pid, 0)` still succeeds on one, which would report a
// process that holds nothing as a survivor. A zombie has no cmdline, which is
// the same discriminator findStrays uses.
func liveProcessState(pid int) string {
	raw, err := os.ReadFile(filepath.Join("/proc", strconv.Itoa(pid), "cmdline"))
	if err != nil || len(raw) == 0 {
		return ""
	}
	state := procField(strconv.Itoa(pid), "stat")
	if state == "Z" {
		return ""
	}
	return state
}

// awaitPIDFile waits for a stand-in to record its child's pid.
func awaitPIDFile(t *testing.T, path string) int {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for {
		raw, err := os.ReadFile(path)
		if err == nil {
			text := strings.TrimSpace(string(raw))
			if text != "" {
				var pid int
				if _, scanErr := fmt.Sscanf(text, "%d", &pid); scanErr == nil && pid > 0 {
					return pid
				}
			}
		}
		if time.Now().After(deadline) {
			t.Fatalf("the stand-in emacs never recorded its child's pid at %s", path)
		}
		time.Sleep(20 * time.Millisecond)
	}
}
