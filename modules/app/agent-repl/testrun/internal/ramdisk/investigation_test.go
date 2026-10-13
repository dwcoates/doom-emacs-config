package ramdisk

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

const mainOnly = "worktree /repo\nHEAD abc\nbranch refs/heads/master\n\nworktree /wt/other\nHEAD def\nbranch refs/heads/other-work\n"

// fakeWorkspaces is the investigation's view of git and the workspace CLI.
type fakeWorkspaces struct {
	porcelain   string
	gitErr      error
	gitBound    time.Duration
	dispatched  [][]byte
	dispatchErr error
}

func (f *fakeWorkspaces) git(bound time.Duration, name string, args ...string) (string, error) {
	f.gitBound = bound
	return f.porcelain, f.gitErr
}

func (f *fakeWorkspaces) dispatch(commands []byte) error {
	f.dispatched = append(f.dispatched, commands)
	return f.dispatchErr
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

func pendingFile(t *testing.T, dir, content string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "workspace_commands_1.json"), []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
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
	var got []createCommand
	if err := json.Unmarshal(f.dispatched[0], &got); err != nil {
		t.Fatalf("decode the dispatch: %v", err)
	}
	want := createCommand{Type: "create", Name: InvestigationName, GitRoot: "/repo", SourceWS: sourceWS{Name: "repo", Path: "/repo"}}
	got[0].Prompt = ""
	if len(got) != 1 || got[0] != want {
		t.Fatalf("dispatch = %+v, want %+v", got, want)
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

func TestEnsureInvestigationLeavesAnOpenWorktreeAlone(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly + "\nworktree /wt/ramdisk-leak-investigation-xnh\nHEAD 123\nbranch refs/heads/ramdisk-leak-investigation-xnh\n"}
	m := newManager(t, &fakeHost{})

	// Act
	out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err != nil || len(f.dispatched) != 0 || !strings.Contains(out.Existing, "/wt/ramdisk-leak-investigation-xnh") {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want the open worktree named and none dispatched", out, err, len(f.dispatched))
	}
}

func TestEnsureInvestigationRecognizesAPrefixedBranch(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly + "\nworktree /wt/x\nHEAD 123\nbranch refs/heads/DWC/ramdisk-leak-investigation-xnh\n"}
	m := newManager(t, &fakeHost{})

	// Act
	out, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err != nil || len(f.dispatched) != 0 || out.Existing == "" {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want the prefixed branch taken as open", out, err, len(f.dispatched))
	}
}

func TestEnsureInvestigationLeavesACreateWaitingInTheIngressAlone(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})
	inv := newInvestigation(t, f)
	pendingFile(t, filepath.Join(inv.StateDir, "output"), `[{"type":"create","name":"ramdisk-leak-investigation-abc"}]`)

	// Act
	out, err := m.EnsureInvestigation(inv, leakTrouble)

	// Assert
	if err != nil || len(f.dispatched) != 0 || !strings.Contains(out.Existing, "waiting in the daemon's ingress") {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want the pending create named", out, err, len(f.dispatched))
	}
}

func TestEnsureInvestigationLeavesACreateTheDaemonClaimedAlone(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})
	inv := newInvestigation(t, f)
	pendingFile(t, filepath.Join(inv.StateDir, "output", "claimed"), `[{"type":"create","name":"ramdisk-leak-investigation-abc"}]`)

	// Act
	out, err := m.EnsureInvestigation(inv, leakTrouble)

	// Assert
	if err != nil || len(f.dispatched) != 0 || out.Existing == "" {
		t.Fatalf("EnsureInvestigation = %+v, %v with %d dispatches; want the claimed create taken as open", out, err, len(f.dispatched))
	}
}

func TestEnsureInvestigationIgnoresAnotherPendingCreate(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})
	inv := newInvestigation(t, f)
	pendingFile(t, filepath.Join(inv.StateDir, "output"), `[{"type":"create","name":"other-work"}]`)

	// Act
	if _, err := m.EnsureInvestigation(inv, leakTrouble); err != nil {
		t.Fatalf("EnsureInvestigation: %v", err)
	}

	// Assert
	if len(f.dispatched) != 1 {
		t.Fatalf("%d dispatches, want the investigation dispatched beside an unrelated create", len(f.dispatched))
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

func TestEnsureInvestigationSurfacesADispatchFailure(t *testing.T) {
	// Arrange
	f := &fakeWorkspaces{porcelain: mainOnly, dispatchErr: errors.New("the workspace CLI is missing")}
	m := newManager(t, &fakeHost{})

	// Act
	_, err := m.EnsureInvestigation(newInvestigation(t, f), leakTrouble)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "dispatch the investigation workspace's create: the workspace CLI is missing") {
		t.Fatalf("EnsureInvestigation = %v, want the dispatch's failure named", err)
	}
}

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
		{name: "asks for the root cause", trouble: leakTrouble, want: "- Find why the runner's RAM disks leak and why the disk manager wedged"},
		{name: "forbids hand repair", trouble: leakTrouble, want: "- Never hand-repair:"},
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

func TestScriptDispatchPipesTheCommandsIntoEmitCommands(t *testing.T) {
	// Arrange
	got := filepath.Join(t.TempDir(), "got")
	dispatch := ScriptDispatch(script(t, `echo "$1" >"`+got+`"; cat >>"`+got+`"`), 10*time.Second)

	// Act
	err := dispatch([]byte(`[{"type":"create"}]`))

	// Assert
	raw, readErr := os.ReadFile(got)
	if err != nil || readErr != nil || string(raw) != "--emit-commands\n[{\"type\":\"create\"}]" {
		t.Fatalf("dispatch = %v; the script saw %q (%v)", err, raw, readErr)
	}
}

func TestScriptDispatchNamesAMissingScript(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(filepath.Join(t.TempDir(), "absent.sh"), 10*time.Second)

	// Act
	err := dispatch([]byte(`[]`))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the workspace CLI is missing") {
		t.Fatalf("dispatch = %v, want the missing script named", err)
	}
}

func TestScriptDispatchReportsAFailingScriptsOutput(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(script(t, `echo "source_ws invariant" >&2; exit 2`), 10*time.Second)

	// Act
	err := dispatch([]byte(`[]`))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "source_ws invariant") {
		t.Fatalf("dispatch = %v, want the script's own failure with its output", err)
	}
}

func TestScriptDispatchFailsAScriptThatOutlivesItsBound(t *testing.T) {
	// Arrange
	dispatch := ScriptDispatch(script(t, `exec sleep 10`), 100*time.Millisecond)

	// Act
	err := dispatch([]byte(`[]`))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer within 100ms") {
		t.Fatalf("dispatch = %v, want the bound's failure", err)
	}
}

func TestEnsureInvestigationWaitsForTheRegistryLock(t *testing.T) {
	// Arrange: another run holds the registry lock, as while it dispatches.
	f := &fakeWorkspaces{porcelain: mainOnly}
	m := newManager(t, &fakeHost{})
	inv := newInvestigation(t, f)
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}
	unlock, err := m.lockRegistry()
	if err != nil {
		t.Fatalf("lockRegistry: %v", err)
	}
	other := m
	other.Pid = 5151
	answered := make(chan error, 1)
	go func() {
		_, err := other.EnsureInvestigation(inv, leakTrouble)
		answered <- err
	}()

	// Act: the other run's dispatch lands in the ingress, then it lets go.
	pendingFile(t, filepath.Join(inv.StateDir, "output"), `[{"type":"create","name":"ramdisk-leak-investigation-abc"}]`)
	if err := unlock(); err != nil {
		t.Fatal(err)
	}
	err = <-answered

	// Assert
	if err != nil || len(f.dispatched) != 0 {
		t.Fatalf("EnsureInvestigation = %v with %d dispatches; want it to have waited and seen the other run's create", err, len(f.dispatched))
	}
}
