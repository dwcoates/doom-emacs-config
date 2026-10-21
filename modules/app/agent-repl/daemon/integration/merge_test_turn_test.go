//go:build integration

package integration

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_test_turn_test.go covers a repository's TEST TURN
// (internal/merge/testturn.go): only one merge of a repository runs its test
// gate at a time, and two merges alternate the turn while each fixes.

// turnGate is a test gate whose every run says where it started and holds
// until the test releases it with an exit status, through two named pipes, so
// the test sees each run's start and decides its end with no timing at all.
// Every start and end is also appended to a log, the record the overlap
// assertion reads.
type turnGate struct {
	t      *testing.T
	dir    string
	path   string
	starts chan string
}

// turnGateScript is the gate's body: record the start, tell the test where
// it runs, wait for its verdict, record the end, and exit with it.
const turnGateScript = `#!/bin/bash
ctl=%q
printf 'start %%s\n' "$PWD" >> "$ctl/log"
printf '%%s\n' "$PWD" > "$ctl/started"
read -r code < "$ctl/release"
printf 'end %%s\n' "$PWD" >> "$ctl/log"
echo "daemon: starting"
if [ "$code" = 0 ]; then echo "daemon: passed in 1s"; else echo "daemon failed after 1s with exit code $code"; fi
exit "$code"
`

func newTurnGate(t *testing.T) *turnGate {
	t.Helper()
	dir := t.TempDir()
	for _, fifo := range []string{"started", "release"} {
		if err := syscall.Mkfifo(filepath.Join(dir, fifo), 0o600); err != nil {
			t.Fatalf("making the gate's %s pipe: %v", fifo, err)
		}
	}
	g := &turnGate{t: t, dir: dir, path: filepath.Join(dir, "test-all.sh"), starts: make(chan string, 16)}
	if err := os.WriteFile(g.path, []byte(fmt.Sprintf(turnGateScript, dir)), 0o755); err != nil {
		t.Fatalf("writing the gate: %v", err)
	}
	go g.follow()
	t.Cleanup(g.stop)
	return g
}

// follow hands every run's start to starts, until stop writes the end mark.
func (g *turnGate) follow() {
	for {
		pipe, err := os.Open(filepath.Join(g.dir, "started"))
		if err != nil {
			return
		}
		lines := bufio.NewScanner(pipe)
		for lines.Scan() {
			if lines.Text() == "stop" {
				pipe.Close()
				return
			}
			g.starts <- lines.Text()
		}
		pipe.Close()
	}
}

// stop ends follow.
func (g *turnGate) stop() {
	pipe, err := os.OpenFile(filepath.Join(g.dir, "started"), os.O_WRONLY, 0)
	if err != nil {
		return
	}
	defer pipe.Close()
	fmt.Fprintln(pipe, "stop")
}

// awaitStart waits for the next run's start, and fails unless it runs in dir.
func (g *turnGate) awaitStart(d *harness.Daemon, dir string) {
	g.t.Helper()
	wait, cancel := d.WaitCtx()
	defer cancel()
	select {
	case got := <-g.starts:
		if filepath.Clean(got) != filepath.Clean(dir) {
			g.t.Fatalf("the gate started in %s, want %s", got, dir)
		}
	case <-wait.Done():
		g.t.Fatalf("no gate run started in %s", dir)
	}
}

// release ends the running gate run with code.
func (g *turnGate) release(code int) {
	g.t.Helper()
	pipe, err := os.OpenFile(filepath.Join(g.dir, "release"), os.O_WRONLY, 0)
	if err != nil {
		g.t.Fatalf("opening the gate's release pipe: %v", err)
	}
	defer pipe.Close()
	fmt.Fprintln(pipe, code)
}

// runs answers the log's start and end lines, in order.
func (g *turnGate) runs() []string {
	g.t.Helper()
	body, err := os.ReadFile(filepath.Join(g.dir, "log"))
	if err != nil {
		g.t.Fatalf("reading the gate's log: %v", err)
	}
	return strings.Split(strings.TrimSpace(string(body)), "\n")
}

func TestTwoMergesOfARepositoryAlternateTheTestTurnAndTheirGatesNeverOverlap(t *testing.T) {
	t.Parallel()
	// Arrange: two children of one repository, each with work to land.
	repo := harness.NewRepo(t)
	gate := newTurnGate(t)
	repo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.MergeChainTimeout,
		ExtraEnv: []string{harness.TestAllScriptEnv(gate.path)}})
	d.ExpectWarnings("daemon.merge.tests", "daemon.scriptrunner.run")
	repoRef := mergeRepositoryRef(t, d, repo)
	a := mergeCreateChild(t, d, repoRef, "alpha", "the first work", nil)
	b := mergeCreateChild(t, d, repoRef, "beta", "the second work", nil)
	harness.CommitWork(t, a.ws.GetDir())
	harness.CommitWork(t, b.ws.GetDir())
	aRoot, bRoot := a.watchRootFeed(), b.watchRootFeed()
	turnLog := func(f *fixture, text string) {
		t.Helper()
		d.AwaitLogRecord(harness.WorkspaceLogPath(f.ws.GetDir(), "daemon"), text, func(r harness.LogRecord) bool {
			return r.Operation == "daemon.merge.test_turn" && strings.Contains(r.Message, text)
		})
	}

	// Act: A, the head, runs first; B is put in line once A is testing.
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(a.ws))); err != nil {
		t.Fatalf("FinishWorkspace(alpha): %v", err)
	}
	gate.awaitStart(d, a.ws.GetDir())
	aBubble := awaitRow(t, a, aRoot, "alpha's merge bubble", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(b.ws))); err != nil {
		t.Fatalf("FinishWorkspace(beta): %v", err)
	}
	turnLog(b, "joined the line")
	// A fails: B's tests run while A fixes.
	gate.release(1)
	a.shim.ExpectStartTurn()
	gate.awaitStart(d, b.ws.GetDir())
	// A's fix ends while B's tests run: A waits in line for them.
	pushConcludedTurn(a.shim, mainAgent, "alpha-fix")
	turnLog(a, "joined the line")
	waiting := a.awaitRowInFeed(aBubble.GetId(), "alpha's run waiting for the turn", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTestsRun().GetWaitingForTurn() != nil
	})
	// B fails: A runs again while B fixes, and passes.
	gate.release(1)
	b.shim.ExpectStartTurn()
	gate.awaitStart(d, a.ws.GetDir())
	gate.release(0)
	aMerge := awaitRow(t, a, aRoot, "alpha's terminal", isMergeTerminal)
	d.AwaitLandingDeployed()
	// B's fix ends; B runs alone and passes.
	pushConcludedTurn(b.shim, mainAgent, "beta-fix")
	gate.awaitStart(d, b.ws.GetDir())
	gate.release(0)
	bMerge := awaitRow(t, b, bRoot, "beta's terminal", isMergeTerminal)
	d.AwaitLandingDeployed()

	// Assert.
	if aMerge.GetActivity().GetMerge().GetSuccess() == nil || bMerge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("alpha ended %v, beta %v; want both landed", aMerge.GetActivity().GetMerge(), bMerge.GetActivity().GetMerge())
	}
	if got := waiting.GetMergeTab().GetTestsRun().GetWaitingForTurn().GetHolder().GetWorkspaceName(); got != "beta" {
		t.Fatalf("alpha's waiting run named %q holding the turn, want beta", got)
	}
	runs := gate.runs()
	want := []string{a.ws.GetDir(), b.ws.GetDir(), a.ws.GetDir(), b.ws.GetDir()}
	if len(runs) < 2*len(want) {
		t.Fatalf("the gate ran %v, want four runs, each ended before the next began", runs)
	}
	starts := 0
	for i, line := range runs {
		kind, dir, _ := strings.Cut(line, " ")
		wantKind := map[bool]string{true: "start", false: "end"}[i%2 == 0]
		if kind != wantKind {
			t.Fatalf("gate log line %d is %q, want a %s: two gates of the repository overlapped (%v)", i, line, wantKind, runs)
		}
		if kind == "start" {
			if starts < len(want) && filepath.Clean(dir) != filepath.Clean(want[starts]) {
				t.Fatalf("run %d started in %s, want %s: the turn did not alternate (%v)", starts+1, dir, want[starts], runs)
			}
			starts++
		}
	}
}
