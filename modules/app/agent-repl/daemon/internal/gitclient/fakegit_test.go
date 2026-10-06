// fakegit_test.go is the package's test harness: a SCRIPTED FAKE `git`
// executable, placed first on PATH by each test.
//
// WHY NO REAL GIT. The tests never invoke the git binary — not once, not
// against a temp repository. What this package is responsible for is the
// CONVERSATION with git: the argument vector it builds, the environment it
// hands the child, and the answer it derives from what comes back. A fake lets
// every one of those be asserted exactly, including the ones a real repository
// makes awkward (an unreadable author date, a path with unusual bytes, a git
// that refuses a removal whose tree is gone anyway).
//
// HOW IT WORKS. The fake `git` on PATH is a two-line shell script that execs
// THIS TEST BINARY back into itself with GITCLIENT_FAKE_GIT=1; TestMain sees
// that and runs fakeGitMain instead of the suite. So the fake's behavior is
// written in Go, in this file, with no build step and no second binary. Each
// invocation is recorded — argv, working directory and the FULL environment —
// and answered from a fixture table keyed by the argument vector.
package gitclient

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"syscall"
	"testing"

	"claude-repld/internal/dlog"
)

// The fake's channel to itself. None of these names is repository-selecting,
// so none is scrubbed and all of them reach the child.
const (
	fakeGitEnabled  = "GITCLIENT_FAKE_GIT"
	fakeGitStateDir = "GITCLIENT_FAKE_GIT_DIR"
)

// TestMain routes the process: the suite normally, the fake git when this
// binary was re-executed as one.
func TestMain(m *testing.M) {
	if os.Getenv(fakeGitEnabled) == "1" {
		fakeGitMain()
		return
	}
	os.Exit(m.Run())
}

// gitCall is one recorded invocation of the fake.
type gitCall struct {
	// Cwd is the working directory git was spawned in. It must NOT be the
	// repository directory: `-C dir` is the only selector.
	Cwd string `json:"cwd"`
	// Args is the whole argument vector, `-C dir` included.
	Args []string `json:"args"`
	// Env is the child's complete environment, as the scrub left it.
	Env []string `json:"env"`
	// Pid is the fake's own process id. The PATH script execs this binary,
	// so it is the pid the client spawned.
	Pid int `json:"pid"`
}

// subject is the argument vector with the leading `-C dir` removed: the
// method's own git command, which is what fixtures and assertions key on.
func (c gitCall) subject() []string {
	if len(c.Args) >= 2 && c.Args[0] == "-C" {
		return c.Args[2:]
	}
	return c.Args
}

// dashCDir is the directory the call selected with `-C`.
func (c gitCall) dashCDir() string {
	if len(c.Args) >= 2 && c.Args[0] == "-C" {
		return c.Args[1]
	}
	return ""
}

// gitFixture is one scripted answer. The first fixture whose Match is a prefix
// of the call's subject wins; an empty Match matches anything.
type gitFixture struct {
	Match []string `json:"match"`
	// Dir, when set, restricts the fixture to calls whose `-C` directory is
	// exactly this. It is what lets one script answer two directories
	// differently — the repository-identity comparisons need that.
	Dir    string `json:"dir"`
	Stdout string `json:"stdout"`
	Stderr string `json:"stderr"`
	Exit   int    `json:"exit"`
	// RemovePath, when set, is deleted before the fixture answers. It models
	// the one case a pure script cannot: a git that took the worktree away and
	// then reported a failure anyway.
	RemovePath string `json:"remove_path"`
	// BlockOnFifo, when set, is a named pipe the fake opens for reading and
	// then reads from, which never returns. It models a git STILL RUNNING when
	// the daemon's context ends — the case the cancellation classification is
	// about — and the pipe is the rendezvous rather than a sleep: the test's
	// open-for-write returns exactly when the child has opened its end, so the
	// test knows the process is live before it cancels.
	BlockOnFifo string `json:"block_on_fifo"`
	// KillSelfWith, when non-zero, is the signal the fake sends ITSELF instead
	// of answering. It models the one death this client cannot cause and must
	// still classify: a git ended by somebody else while the daemon's context
	// is perfectly alive.
	KillSelfWith int `json:"kill_self_with"`
}

// ok is a fixture that succeeds with that stdout.
func ok(stdout string, match ...string) gitFixture {
	return gitFixture{Match: match, Stdout: stdout}
}

// fails is a fixture that exits nonzero with that stderr.
func fails(exit int, stderr string, match ...string) gitFixture {
	return gitFixture{Match: match, Stderr: stderr, Exit: exit}
}

// killedBy is a fixture whose git is killed by that signal, with the caller's
// context untouched — the wrong-victim death, not a cancellation.
func killedBy(sig syscall.Signal, match ...string) gitFixture {
	return gitFixture{Match: match, KillSelfWith: int(sig)}
}

// blocks is a fixture that hangs on that named pipe until the process is
// killed, which is how a test gets a git that is still running when the
// context ends.
func blocks(fifo string, match ...string) gitFixture {
	return gitFixture{Match: match, BlockOnFifo: fifo}
}

// newFifo makes a named pipe for the blocks fixture and returns its path.
func newFifo(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "block")
	if err := syscall.Mkfifo(path, 0o600); err != nil {
		t.Fatalf("making the rendezvous pipe: %v", err)
	}
	return path
}

// awaitOpen returns once the fake git has opened the pipe's read end, which is
// the proof the child process is live. It is a kernel rendezvous, not a wait
// on the clock: opening a fifo for writing blocks until a reader arrives.
func awaitOpen(t *testing.T, fifo string) {
	t.Helper()
	writer, err := os.OpenFile(fifo, os.O_WRONLY, 0)
	if err != nil {
		t.Fatalf("meeting the fake git at the pipe: %v", err)
	}
	t.Cleanup(func() { writer.Close() })
}

// fakeGit is the harness handle a test asserts against.
type fakeGit struct {
	t   *testing.T
	dir string
}

// newFakeGit installs the fake as the only `git` on PATH and scripts it with
// these fixtures. A call matching no fixture is a LOUD failure rather than a
// silent success, so a test can never pass on a command it did not intend.
func newFakeGit(t *testing.T, fixtures ...gitFixture) *fakeGit {
	t.Helper()

	dir := t.TempDir()
	encoded, err := json.Marshal(fixtures)
	if err != nil {
		t.Fatalf("encoding the fixtures: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "fixtures.json"), encoded, 0o600); err != nil {
		t.Fatalf("writing the fixtures: %v", err)
	}

	self, err := os.Executable()
	if err != nil {
		t.Fatalf("locating the test binary: %v", err)
	}
	binDir := filepath.Join(dir, "bin")
	if err := os.MkdirAll(binDir, 0o755); err != nil {
		t.Fatalf("making the fake bin directory: %v", err)
	}
	script := fmt.Sprintf("#!/bin/sh\nexec %q \"$@\"\n", self)
	if err := os.WriteFile(filepath.Join(binDir, "git"), []byte(script), 0o755); err != nil {
		t.Fatalf("writing the fake git: %v", err)
	}

	t.Setenv("PATH", binDir)
	t.Setenv(fakeGitEnabled, "1")
	t.Setenv(fakeGitStateDir, dir)

	return &fakeGit{t: t, dir: dir}
}

// calls returns every invocation the fake recorded, in order.
func (f *fakeGit) calls() []gitCall {
	f.t.Helper()
	raw, err := os.ReadFile(filepath.Join(f.dir, "calls.jsonl"))
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		f.t.Fatalf("reading the recorded calls: %v", err)
	}
	var calls []gitCall
	for _, line := range strings.Split(strings.TrimRight(string(raw), "\n"), "\n") {
		if line == "" {
			continue
		}
		var call gitCall
		if err := json.Unmarshal([]byte(line), &call); err != nil {
			f.t.Fatalf("decoding a recorded call %q: %v", line, err)
		}
		calls = append(calls, call)
	}
	return calls
}

// call returns the nth recorded invocation, failing when there is none.
func (f *fakeGit) call(n int) gitCall {
	f.t.Helper()
	calls := f.calls()
	if n >= len(calls) {
		f.t.Fatalf("git was invoked %d times, want at least %d: %v", len(calls), n+1, subjects(calls))
	}
	return calls[n]
}

// only returns the single recorded invocation, failing when there was more or
// less than one.
func (f *fakeGit) only() gitCall {
	f.t.Helper()
	calls := f.calls()
	if len(calls) != 1 {
		f.t.Fatalf("git was invoked %d times, want exactly 1: %v", len(calls), subjects(calls))
	}
	return calls[0]
}

// find returns the first recorded invocation whose subject starts with prefix.
func (f *fakeGit) find(prefix ...string) (gitCall, bool) {
	f.t.Helper()
	for _, call := range f.calls() {
		if hasPrefix(call.subject(), prefix) {
			return call, true
		}
	}
	return gitCall{}, false
}

// assertNever fails when any recorded invocation starts with prefix. It is how
// a test asserts a command was NOT issued — that a conflicted merge was never
// aborted, that a branch was never deleted.
func (f *fakeGit) assertNever(prefix ...string) {
	f.t.Helper()
	if call, found := f.find(prefix...); found {
		f.t.Fatalf("git %v was issued and must not have been", call.subject())
	}
}

// assertSubject fails unless the nth invocation's subject is exactly want.
func (f *fakeGit) assertSubject(n int, want ...string) {
	f.t.Helper()
	got := f.call(n).subject()
	if !reflect.DeepEqual(got, want) {
		f.t.Fatalf("call %d subject = %v, want %v", n, got, want)
	}
}

// subjects renders every call's subject, for a failure message.
func subjects(calls []gitCall) [][]string {
	out := make([][]string, 0, len(calls))
	for _, call := range calls {
		out = append(out, call.subject())
	}
	return out
}

// hasPrefix reports whether args starts with prefix.
func hasPrefix(args, prefix []string) bool {
	if len(args) < len(prefix) {
		return false
	}
	for i, want := range prefix {
		if args[i] != want {
			return false
		}
	}
	return true
}

// envValues returns every binding of that variable in the recorded child
// environment.
func envValues(env []string, name string) []string {
	var found []string
	for _, entry := range env {
		if strings.HasPrefix(entry, name+"=") {
			found = append(found, entry)
		}
	}
	return found
}

// fakeGitMain is the fake git itself, running in the re-executed test binary.
func fakeGitMain() {
	stateDir := os.Getenv(fakeGitStateDir)
	args := os.Args[1:]

	cwd, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "fake git: reading the working directory: %v\n", err)
		os.Exit(120)
	}

	record, err := json.Marshal(gitCall{Cwd: cwd, Args: args, Env: os.Environ(), Pid: os.Getpid()})
	if err != nil {
		fmt.Fprintf(os.Stderr, "fake git: encoding the call: %v\n", err)
		os.Exit(120)
	}
	log, err := os.OpenFile(filepath.Join(stateDir, "calls.jsonl"), os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
	if err != nil {
		fmt.Fprintf(os.Stderr, "fake git: opening the call log: %v\n", err)
		os.Exit(120)
	}
	if _, err := log.Write(append(record, '\n')); err != nil {
		fmt.Fprintf(os.Stderr, "fake git: recording the call: %v\n", err)
		os.Exit(120)
	}
	if err := log.Close(); err != nil {
		fmt.Fprintf(os.Stderr, "fake git: closing the call log: %v\n", err)
		os.Exit(120)
	}

	raw, err := os.ReadFile(filepath.Join(stateDir, "fixtures.json"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "fake git: reading the fixtures: %v\n", err)
		os.Exit(120)
	}
	var fixtures []gitFixture
	if err := json.Unmarshal(raw, &fixtures); err != nil {
		fmt.Fprintf(os.Stderr, "fake git: decoding the fixtures: %v\n", err)
		os.Exit(120)
	}

	subject, dashC := args, ""
	if len(args) >= 2 && args[0] == "-C" {
		dashC, subject = args[1], args[2:]
	}

	for _, fixture := range fixtures {
		if fixture.Dir != "" && fixture.Dir != dashC {
			continue
		}
		if !hasPrefix(subject, fixture.Match) {
			continue
		}
		if fixture.BlockOnFifo != "" {
			pipe, err := os.OpenFile(fixture.BlockOnFifo, os.O_RDONLY, 0)
			if err != nil {
				fmt.Fprintf(os.Stderr, "fake git: opening %s: %v\n", fixture.BlockOnFifo, err)
				os.Exit(120)
			}
			// The write end is never written to, so this read blocks until the
			// process is killed. That is the whole point.
			var one [1]byte
			if _, err := pipe.Read(one[:]); err != nil {
				fmt.Fprintf(os.Stderr, "fake git: reading %s: %v\n", fixture.BlockOnFifo, err)
				os.Exit(120)
			}
		}
		if fixture.KillSelfWith != 0 {
			if err := syscall.Kill(os.Getpid(), syscall.Signal(fixture.KillSelfWith)); err != nil {
				fmt.Fprintf(os.Stderr, "fake git: killing itself with %d: %v\n", fixture.KillSelfWith, err)
				os.Exit(120)
			}
			// A SIGKILL SENT TO ITSELF IS NOT DEATH BEFORE kill(2) RETURNS.
			// The kernel tears the process down on some thread's way back to
			// user space, and this thread raced on to the "survived" exit
			// below and exited 120 under load (TestAGitKilledByASignalIsRecordedWithTheSignal
			// read a record with no signal, 2026-10-06). SIGKILL cannot be
			// survived, so it is waited for: a read on a pipe this process
			// holds both ends of blocks until the process is gone (blocked I/O
			// is not a deadlock to the Go runtime, as an empty select is).
			if syscall.Signal(fixture.KillSelfWith) == syscall.SIGKILL {
				r, w, err := os.Pipe()
				if err != nil {
					fmt.Fprintf(os.Stderr, "fake git: a pipe to await its own SIGKILL: %v\n", err)
					os.Exit(120)
				}
				var one [1]byte
				_, err = r.Read(one[:])
				runtime.KeepAlive(w)
				fmt.Fprintf(os.Stderr, "fake git: awaiting its own SIGKILL: %v\n", err)
				os.Exit(120)
			}
			// A catchable signal can be survived, and saying so is better
			// than pretending the fixture answered.
			fmt.Fprintf(os.Stderr, "fake git: survived signal %d\n", fixture.KillSelfWith)
			os.Exit(120)
		}
		if fixture.RemovePath != "" {
			if err := os.RemoveAll(fixture.RemovePath); err != nil {
				fmt.Fprintf(os.Stderr, "fake git: removing %s: %v\n", fixture.RemovePath, err)
				os.Exit(120)
			}
		}
		io.WriteString(os.Stdout, fixture.Stdout)
		io.WriteString(os.Stderr, fixture.Stderr)
		os.Exit(fixture.Exit)
	}

	fmt.Fprintf(os.Stderr, "fake git: no fixture matches %v\n", subject)
	os.Exit(127)
}

// --- the client under test ---------------------------------------------

// testSurfaces is the dlog.Surfaces double. gitclient logs everything
// globally — it is a leaf handed arbitrary repository and worktree directories
// with no workspace identity — so every other method here panics rather than
// pretending to be reachable.
type testSurfaces struct {
	global *dlog.TestLogger
	// dirEvents records every DetachDir and AttachDir call, in order, as
	// "detach <dir>" and "attach <dir>".
	dirEvents []string
	// dirFailure, when set, is what DetachDir and AttachDir answer.
	dirFailure error
	// attachFailure, when set, is what AttachDir answers in dirFailure's
	// place, so a detach can succeed and the re-attach after it fail.
	attachFailure error
}

func newTestSurfaces() *testSurfaces { return &testSurfaces{global: dlog.NewTestLogger()} }

func (s *testSurfaces) Global() dlog.Logger { return s.global }

func (s *testSurfaces) Workspace(string) (dlog.Logger, error) {
	panic("gitclient must never resolve a workspace sink: it is a leaf with no workspace identity")
}

func (s *testSurfaces) WorkspaceOrCentral(string) dlog.Logger {
	panic("gitclient must never resolve a workspace sink: it is a leaf with no workspace identity")
}

func (s *testSurfaces) ShimSink(string) (dlog.Borrowed, error) {
	panic("gitclient must never borrow a shim sink")
}

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (s *testSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (s *testSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

// BindRecordTee implements dlog.Surfaces; this double tees nothing.
func (s *testSurfaces) BindRecordTee(dlog.RecordTee) {}

func (s *testSurfaces) DetachDir(dir string) error {
	s.dirEvents = append(s.dirEvents, "detach "+dir)
	return s.dirFailure
}

func (s *testSurfaces) AttachDir(dir string) error {
	s.dirEvents = append(s.dirEvents, "attach "+dir)
	if s.attachFailure != nil {
		return s.attachFailure
	}
	return s.dirFailure
}

func (s *testSurfaces) ClientLog(string, dlog.ClientRecord) error {
	panic("gitclient must never persist a client record")
}

func (s *testSurfaces) Evict(string) error {
	panic("gitclient must never evict a workspace's sinks")
}

func (s *testSurfaces) Close() error { return nil }

func (s *testSurfaces) records() []dlog.Record { return s.global.Records() }

// newTestClient builds the client under test.
func newTestClient(t *testing.T) (Git, *testSurfaces) {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")

	surfaces := newTestSurfaces()
	git, err := New(surfaces)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return git, surfaces
}

// recordFor finds the first captured record with that level and operation.
func recordFor(records []dlog.Record, level, operation string) (dlog.Record, bool) {
	for _, record := range records {
		if record.Level == level && record.Operation == operation {
			return record, true
		}
	}
	return dlog.Record{}, false
}

// existingDir makes a real directory, for the two methods whose answer depends
// on the filesystem rather than on git.
func existingDir(t *testing.T, name string) string {
	t.Helper()
	dir := filepath.Join(t.TempDir(), name)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("making %s: %v", dir, err)
	}
	return dir
}
