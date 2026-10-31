package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

const mainOnly = "worktree /repo\nHEAD abc\nbranch refs/heads/master\n\nworktree /wt/other\nHEAD def\nbranch refs/heads/other-work\n"

// fakeWorkspaces is the investigation's view of git and the workspace CLI.
type fakeWorkspaces struct {
	mu          sync.Mutex
	porcelain   string
	gitErr      error
	gitBound    time.Duration
	dispatched  [][]string
	printed     string
	dispatchErr error
}

func (f *fakeWorkspaces) git(bound time.Duration, name string, args ...string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.gitBound = bound
	return f.porcelain, f.gitErr
}

func (f *fakeWorkspaces) dispatch(args []string) ([]byte, error) {
	f.dispatched = append(f.dispatched, args)
	return []byte(f.printed), f.dispatchErr
}

// newInvestigation answers an Investigation over f whose state root has a
// daemon serving.
func newInvestigation(t *testing.T, f *fakeWorkspaces) Investigation {
	t.Helper()
	state := t.TempDir()
	if err := os.WriteFile(filepath.Join(state, "daemon.addr"), []byte("127.0.0.1:1\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	return Investigation{Module: "/repo/modules/app/agent-repl", Git: f.git, Dispatch: f.dispatch, StateDir: state, RunLog: "/tmp/runs/1/test-all.log"}
}

var leakTrouble = &Trouble{Leaks: []Leak{{Device: "/dev/disk4", Pid: 7583, PidAlive: true, Mounts: []string{"/private/tmp/artr-7583"}}}}

func TestEnsureInvestigationDispatchesOneCreate(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})

	// Act
	out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err != nil || out.Existing != "" || len(f.dispatched) != 1 {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want one create", out, err, len(f.dispatched))
	}
}

func TestEnsureInvestigationCreatesInTheMainWorktree(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})

	// Act
	if _, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble); err != nil {
		t.Fatalf("EnsureInvestigation: %v", err)
	}

	// Assert
	got := f.dispatched[0]
	want := createArgs("/repo", investigationPrompt(leakTrouble, "/tmp/runs/1/test-all.log"))
	if strings.Join(got, "\x00") != strings.Join(want, "\x00") {
		t.Fatalf("dispatch = %q, want %q", got, want)
	}
}

func TestCreateArgsNameEveryChoiceAndTheUserAsAsker(t *testing.T) {
	// Act
	got := strings.Join(createArgs("/repo", "look into it"), " ")

	// Assert
	want := "workspace create -repository /repo -relationship peer -context fresh -name " + InvestigationName + " -prompt look into it -as-user"
	if got != want {
		t.Fatalf("createArgs = %q, want %q", got, want)
	}
}

func TestEnsureInvestigationListsWorktreesWithinTheGitBound(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})

	// Act
	if _, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble); err != nil {
		t.Fatalf("EnsureInvestigation: %v", err)
	}

	// Assert
	if f.gitBound != GitBound {
		t.Fatalf("git ran under %s, want GitBound %s", f.gitBound, GitBound)
	}
}

func TestEnsureInvestigationFailsWithoutADaemon(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})
	inv := newInvestigation(t, f)
	if err := os.Remove(filepath.Join(inv.StateDir, "daemon.addr")); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := m.EnsureInvestigation(inv, leakTrouble)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no agent-repl daemon serves") || len(f.dispatched) != 0 {
		t.Fatalf("EnsureInvestigation = %v with %d dispatches; want the missing daemon named", err, len(f.dispatched))
	}
}

func TestEnsureInvestigationSurfacesAGitFailure(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{gitErr: errors.New("git worktree list did not answer within 300ms")}
	m := newManager(t, &fakeHost{})

	// Act
	_, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the repository's worktrees: git worktree list did not answer") {
		t.Fatalf("EnsureInvestigation = %v, want git's failure named", err)
	}
}

func TestEnsureInvestigationFailsWithoutAMainWorktree(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: ""}
	m := newManager(t, &fakeHost{})

	// Act
	_, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "named no main worktree") {
		t.Fatalf("EnsureInvestigation = %v, want the missing main worktree named", err)
	}
}

// heldAnswer is the agent CLI's printed answer to a create the daemon refused
// as name_held.
const heldAnswer = `{"error":{"nameHeld":{"branch":"ramdisk-leak-investigation","workspace":{"id":"ws-inv","dir":"/wt/ramdisk-leak-investigation"},"open":%s}}}`

// TestEnsureInvestigationTakesTheDaemonsNameHeldAsAlreadyThere covers the one
// dedup the runner has: the daemon refusing the name as name_held, in the
// same step as the create, read as "the investigation is already there".
func TestEnsureInvestigationTakesTheDaemonsNameHeldAsAlreadyThere(t *testing.T) {
	cases := []struct {
		name string
		open string
		want string
	}{
		{name: "an open investigation", open: "true", want: "the workspace ws-inv at /wt/ramdisk-leak-investigation holds the name ramdisk-leak-investigation (open)"},
		{name: "a closed investigation that never landed", open: "false", want: "the workspace ws-inv at /wt/ramdisk-leak-investigation holds the name ramdisk-leak-investigation (closed, not landed)"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &fakeWorkspaces{porcelain: mainOnly, printed: fmt.Sprintf(heldAnswer, tc.open), dispatchErr: errors.New("exit status 6")}
			m := newManager(t, &fakeHost{})

			// Act
			out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

			// Assert
			if err != nil || out.Existing != tc.want || len(f.dispatched) != 1 {
				t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want %q after one create", out, err, len(f.dispatched), tc.want)
			}
		})
	}
}

// TestEnsureInvestigationIgnoresAWorktreeOnTheInvestigationsBranch covers the
// stale branch: a worktree or branch of the investigation's name is no longer
// read as an open investigation, so the create is always asked, and only the
// daemon decides.
func TestEnsureInvestigationIgnoresAWorktreeOnTheInvestigationsBranch(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly + "\nworktree /wt/ramdisk-leak-investigation\nHEAD 123\nbranch refs/heads/ramdisk-leak-investigation\n"}
	m := newManager(t, &fakeHost{})

	// Act
	out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err != nil || out.Existing != "" || len(f.dispatched) != 1 {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want the create asked", out, err, len(f.dispatched))
	}
}

// TestEnsureInvestigationSurfacesAnyOtherRefusal covers a refused create
// whose arm is not name_held: it is the dispatch's failure, with what the
// CLI printed.
func TestEnsureInvestigationSurfacesAnyOtherRefusal(t *testing.T) {
	cases := []struct {
		name    string
		printed string
	}{
		{name: "another arm", printed: `{"error":{"worktreeCreationFailed":{"detail":"fatal"}}}`},
		{name: "an answer that is no JSON", printed: "agent-repld workspace create: the daemon failed the call"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &fakeWorkspaces{porcelain: mainOnly, printed: tc.printed, dispatchErr: errors.New("exit status 6: " + tc.printed)}
			m := newManager(t, &fakeHost{})

			// Act
			out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

			// Assert
			if err == nil || out.Existing != "" || !strings.Contains(err.Error(), "dispatch the investigation workspace's create: exit status 6") {
				t.Fatalf("EnsureInvestigation = %+v, %v; want the dispatch's failure", out, err)
			}
		})
	}
}

func TestEnsureInvestigationSurfacesADispatchFailure(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly, dispatchErr: errors.New("the agent CLI is missing")}
	m := newManager(t, &fakeHost{})

	// Act
	_, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "dispatch the investigation workspace's create: the agent CLI is missing") {
		t.Fatalf("EnsureInvestigation = %v, want the dispatch's failure named", err)
	}
}

var stuckLock = &Trouble{Lock: &LockTimeoutError{Path: "/r/registry.mutex", Bound: time.Second, Holders: "testrun[7583]"}}

func TestInvestigationPrompt(t *testing.T) {
	wedged := &Trouble{ListErr: errors.New("diskutil list did not answer within 100ms"), ListTook: 100 * time.Millisecond, Info: "framework : 1\n", InfoTook: 9 * time.Millisecond}
	tests := []struct {
		name    string
		trouble *Trouble
		runLog  string
		want    string
	}{
		{name: "names a leaked device and its pid", trouble: leakTrouble, want: "- Leaked RAM disk /dev/disk4 (owning run pid 7583, still alive, mounted at /private/tmp/artr-7583, named by no record).\n"},
		{name: "says the cause is a leak", trouble: leakTrouble, want: "because RAM disks it made had leaked."},
		{name: "says the cause is a wedged disk manager", trouble: wedged, want: "because the macOS disk manager (diskarbitrationd) did not answer."},
		{name: "names a probe's timeout", trouble: wedged, want: "- the disk manager did not answer `diskutil list` after 100ms: diskutil list did not answer within 100ms.\n"},
		{name: "names a probe's answer time", trouble: wedged, want: "- the disk manager answered `hdiutil info` in 9ms.\n"},
		{name: "carries hdiutil info's answer", trouble: wedged, want: "`hdiutil info` answered:\n\n```\nframework : 1\n```\n"},
		{name: "names the run's log", trouble: leakTrouble, runLog: "/tmp/runs/1/test-all.log", want: "- The run's log: /tmp/runs/1/test-all.log\n"},
		{name: "says when the run's log is unnamed", trouble: leakTrouble, want: "- The run's log: not named (AGENT_REPL_TEST_RUN_LOG was unset)"},
		{name: "says the cause is a stuck registry lock", trouble: stuckLock, want: "because the RAM disk registry lock /r/registry.mutex was not taken within 1s (processes with it open: testrun[7583])."},
		{name: "says the leak check never ran", trouble: stuckLock, want: "- Leaked RAM disks: none found (the check runs under the registry lock, so it never ran).\n"},
		{name: "asks for the root cause", trouble: leakTrouble, want: "- Find why the runner's RAM disks leak, why the disk manager wedged and why the registry lock's holder is stuck"},
		{name: "forbids hand repair", trouble: leakTrouble, want: "- Never hand-repair:"},
		{name: "says when the check began", trouble: &Trouble{At: time.Date(2026, 10, 13, 3, 19, 9, 120e6, time.UTC)}, want: "- The check began at 2026-10-13T03:19:09.120Z.\n"},
		{name: "carries the host's state", trouble: &Trouble{InfoErr: errors.New("timed out"), Host: "load average 28.72 21.45 17.25\nno disk process running\n"}, want: "The host when the disk manager did not answer:\n\n```\nload average 28.72 21.45 17.25\nno disk process running\n```\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := investigationPrompt(tc.trouble, tc.runLog)

			// Assert
			if !strings.Contains(got, tc.want) {
				t.Fatalf("prompt =\n%s\nwant it to contain\n%s", got, tc.want)
			}
		})
	}
}

// script writes an executable shell script and answers its path.
func script(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "run.sh")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body+"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestScriptDispatchRunsTheCLIWithItsArguments(t *testing.T) {
	// Arrange
	got := filepath.Join(t.TempDir(), "got")
	dispatch := ScriptDispatch(script(t, `printf '%s|' "$@" >"`+got+`"`), 10*time.Second)

	// Act
	_, err := dispatch([]string{"workspace", "create", "-prompt", "two words"})

	// Assert
	raw, readErr := os.ReadFile(got)
	if err != nil || readErr != nil || string(raw) != "workspace|create|-prompt|two words|" {
		t.Fatalf("dispatch = %v; the CLI saw %q (%v)", err, raw, readErr)
	}
}

func TestScriptDispatchNamesAMissingScript(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(filepath.Join(t.TempDir(), "absent.sh"), 10*time.Second)

	// Act
	_, err := dispatch(nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the agent CLI is missing") {
		t.Fatalf("dispatch = %v, want the missing script named", err)
	}
}

func TestScriptDispatchReportsAFailingScriptsOutput(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(script(t, `echo "source_ws invariant" >&2; exit 2`), 10*time.Second)

	// Act
	_, err := dispatch(nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "source_ws invariant") {
		t.Fatalf("dispatch = %v, want the script's own failure with its output", err)
	}
}

func TestScriptDispatchFailsAScriptThatOutlivesItsBound(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(script(t, `exec sleep 10`), 100*time.Millisecond)

	// Act
	_, err := dispatch(nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer within 100ms") {
		t.Fatalf("dispatch = %v, want the bound's failure", err)
	}
}

func TestWriteBlock(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{name: "fences the body under its heading", body: "a\nb\n", want: "\nH:\n\n```\na\nb\n```\n"},
		{name: "writes nothing for an empty body", body: " \n", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var b strings.Builder

			// Act
			writeBlock(&b, "H:", tc.body)

			// Assert
			if b.String() != tc.want {
				t.Fatalf("block = %q, want %q", b.String(), tc.want)
			}
		})
	}
}

func TestScriptDispatchAnswersTheCLIsStdoutBesideARefusal(t *testing.T) {
	// Arrange: the CLI prints the answer on stdout and its verdict on stderr,
	// and exits refused.
	dispatch := ScriptDispatch(script(t, `echo '{"error":{}}'; echo "the daemon refused" >&2; exit 6`), 10*time.Second)

	// Act
	printed, err := dispatch(nil)

	// Assert
	if strings.TrimSpace(string(printed)) != `{"error":{}}` {
		t.Fatalf("printed = %q, want stdout alone", printed)
	}
	if err == nil || !strings.Contains(err.Error(), "the daemon refused") {
		t.Fatalf("dispatch = %v, want the refusal with the CLI's stderr", err)
	}
}
