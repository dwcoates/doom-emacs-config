package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/testrun/seatwait"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// streamingGate scripts one passing gate run that announces each suite.
func (h *harness) streamingGate(suites ...string) {
	out := ""
	for _, suite := range suites {
		out += "[agent-repl-tests] " + suite + ": starting\n"
		out += "[agent-repl-tests] " + suite + ": passed in 2s\n"
	}
	h.runner.runs = append(h.runner.runs, scriptedRun{Output: out, Code: 0})
}

func TestTheTestsTabFollowsEachSuiteFromRunningToPassed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var states []string
	h.feed.mu.Lock()
	for _, row := range h.feed.rows {
		for _, suite := range row.Row.GetMergeTab().GetTestsRun().GetOutput().GetSuites() {
			if suite.GetName() != "daemon" {
				continue
			}
			name := "running"
			if suite.GetPassed() != nil {
				name = "passed"
			}
			if len(states) == 0 || states[len(states)-1] != name {
				states = append(states, name)
			}
		}
	}
	h.feed.mu.Unlock()
	if want := []string{"running", "passed"}; !equal(states, want) {
		t.Fatalf("tab suite states = %v, want %v", states, want)
	}
}

func TestTheTestingLineIsEachSuitesEdge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var edges []string
	for _, f := range h.footer.all() {
		if suite := f.Line.GetTesting(); suite != nil {
			edge := "started"
			if suite.GetPassed() != nil {
				edge = "passed"
			}
			if len(edges) == 0 || edges[len(edges)-1] != edge {
				edges = append(edges, edge)
			}
		}
	}
	if want := []string{"started", "passed"}; !equal(edges, want) {
		t.Fatalf("testing lines = %v, want %v", edges, want)
	}
}

func TestAReusedSuiteSettlesItsRowAndDrawsNoFooterEdge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = []scriptedRun{{Output: reusedLine + "\n[agent-repl-tests] e2e: starting\n[agent-repl-tests] e2e: passed in 2s\n", Code: 0}}

	// Act.
	admitted(t, h)

	// Assert.
	var reused bool
	h.feed.mu.Lock()
	for _, row := range h.feed.rows {
		for _, suite := range row.Row.GetMergeTab().GetTestsRun().GetOutput().GetSuites() {
			if suite.GetName() == "daemon" && suite.GetReused() != nil {
				reused = true
			}
		}
	}
	h.feed.mu.Unlock()
	if !reused {
		t.Fatal("the daemon suite's row never read reused")
	}
	for _, f := range h.footer.all() {
		if suite := f.Line.GetTesting(); suite != nil && suite.GetName() == "daemon" {
			t.Fatalf("a reused suite drew a footer edge %v", suite)
		}
	}
}

func TestTheTestsTabLinksTheRoundsLog(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	link := h.feed.lastTabOfKind(rowTestsRun).GetTestsRun().GetOutput().GetLog()
	lease := h.db.ledger[theWorkspace][0].Lease
	if link.GetToken().GetValue() != string(lease)+"/1" {
		t.Fatalf("log token = %q, want the lease and round", link.GetToken().GetValue())
	}
	if !strings.HasSuffix(link.GetLabel().GetText(), string(lease)+"-tests-1.log") {
		t.Fatalf("log label = %q, want the log's path", link.GetLabel().GetText())
	}
}

func TestTheRoundsLogHoldsTheWholeRun(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	lease := h.db.ledger[theWorkspace][0].Lease
	body, err := os.ReadFile(h.o.testLog(lease, 1).path)
	if err != nil || !strings.Contains(string(body), "daemon: starting") || !strings.Contains(string(body), "daemon: passed in 2s") {
		t.Fatalf("log = (%q, %v), want the whole run", body, err)
	}
}

func TestABrokenGateFailsTheMergeWithNoFixingAttempt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateBroken(1)

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
	for _, f := range h.footer.all() {
		if f.Step == "fixing" {
			t.Fatal("a broken gate had a fixing attempt")
		}
	}
}

func TestSuiteEdgeReadsTheScriptsLines(t *testing.T) {
	tests := []struct {
		line  string
		name  string
		state suiteState
		ok    bool
	}{
		{line: "[agent-repl-tests] daemon: starting", name: "daemon", state: suiteStateRunning, ok: true},
		{line: "[agent-repl-tests] daemon: passed in 3s", name: "daemon", state: suiteStatePassed, ok: true},
		{line: "[agent-repl-tests] ERROR: webapp failed after 4s with exit code 1", name: "webapp", state: suiteStateFailed, ok: true},
		{line: "[agent-repl-tests] proto: DECLINED after 0.125s — its precondition is not met (exit 77)", ok: false},
		{line: reusedLine, name: "daemon", state: suiteStateReused, ok: true},
		{line: "some output", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act.
			name, state, ok := suiteEdge(tt.line)

			// Assert.
			if ok != tt.ok || (ok && (name != tt.name || state != tt.state)) {
				t.Fatalf("suiteEdge = (%q, %v, %v), want (%q, %v, %v)", name, state, ok, tt.name, tt.state, tt.ok)
			}
		})
	}
}

// docsOnlyMerge is a merge whose branch changes only documentation, so the
// gate selects no suite.
func docsOnlyMerge(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/docs/USER-GUIDE.md"}
	enqueue(t, h)
	return h
}

func TestAnEmptySelectionStartsNoRunner(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 0 {
		t.Fatalf("the runner was started %d times, want none", len(h.runner.argv))
	}
}

func TestAnEmptySelectionLands(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if got := h.footer.last(); got.State != StateMerged {
		t.Fatalf("facts = %+v, want the merge landed", got)
	}
}

func TestAnEmptySelectionDrawsASettledTestsTabWithNoSuites(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	tests := h.feed.lastTabOfKind(TabTests).GetTests()
	run := h.feed.lastTabOfKind(rowTestsRun).GetTestsRun()
	if tests.GetSettled().GetSucceeded() == nil || run.GetSettled().GetSucceeded() == nil || len(run.GetOutput().GetSuites()) != 0 {
		t.Fatalf("tests tab = %v, run = %v; want both settled succeeded with no suite rows", tests, run)
	}
}

func TestAnEmptySelectionsLogSaysNoSuiteWasSelected(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	lease := h.db.ledger[theWorkspace][0].Lease
	body, err := os.ReadFile(h.o.testLog(lease, 1).path)
	if err != nil || !strings.Contains(string(body), "no suite selected") || !strings.Contains(string(body), "docs/USER-GUIDE.md") {
		t.Fatalf("log = (%q, %v), want why no suite ran and the paths", body, err)
	}
}

func TestAnEmptySelectionIsRecordedAtInfoNamingThePaths(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	for _, rec := range h.logs.Records() {
		if rec.Level == "info" && strings.Contains(rec.Message, "no suite is selected") {
			if !strings.Contains(fmt.Sprint(rec.Context["paths"]), "docs/USER-GUIDE.md") {
				t.Fatalf("record context = %v, want the paths named", rec.Context)
			}
			return
		}
	}
	t.Fatal("no INFO record says no suite was selected")
}

func TestAnEmptyDiffStartsNoRunner(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 0 {
		t.Fatalf("the runner was started %d times for an empty diff, want none", len(h.runner.argv))
	}
}

func TestUnreadablePathsRunEverySuite(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changedErr = errors.New("git diff failed")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 1 || strings.Contains(strings.Join(h.runner.argv[0], " "), "--suites") {
		t.Fatalf("runner argv = %v, want one unnarrowed run", h.runner.argv)
	}
}

func TestAGateWhosePathsReadIsCutByTheRunsEndRunsNothing(t *testing.T) {
	// Arrange: the run is abandoned, and its git diff answers the cut.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	ctx, cancel := context.WithCancelCause(context.Background())
	r.ctx, r.cancel = ctx, cancel
	cancel(errors.New("abandoned"))
	h.git.changedErr = fmt.Errorf("git diff was cancelled: %w", context.Canceled)
	round := r.openTab(context.Background(), TabTests)
	h.o.mu.Lock()
	h.o.running[h.repoKey()] = r
	h.o.mu.Unlock()

	// Act.
	_, err := r.gate(ctx, round, "tip", "head")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("gate err = %v, want the cut answered", err)
	}
	h.runner.mu.Lock()
	started := len(h.runner.argv)
	h.runner.mu.Unlock()
	if started != 0 {
		t.Fatalf("the runner was started %d times for an abandoned run, want none", started)
	}
	if rec, ok := h.recordFor(dlog.LevelWarn, "daemon.merge.tests"); ok {
		t.Fatalf("the cut was recorded as a warning: %+v", rec)
	}
}

// gateRecord answers the first INFO record of the gate whose message holds
// message.
func gateRecord(t *testing.T, h *harness, message string) map[string]any {
	t.Helper()
	for _, rec := range h.logs.Records() {
		if rec.Level == "info" && rec.Operation == "daemon.merge.tests" && strings.Contains(rec.Message, message) {
			return rec.Context
		}
	}
	t.Fatalf("no INFO daemon.merge.tests record says %q", message)
	return nil
}

// gateRecords counts the INFO records of the gate whose message holds message.
func gateRecords(h *harness, message string) int {
	n := 0
	for _, rec := range h.logs.Records() {
		if rec.Level == "info" && rec.Operation == "daemon.merge.tests" && strings.Contains(rec.Message, message) {
			n++
		}
	}
	return n
}

// gateRuns scripts one gate run with output and exit code, and runs the merge.
func gateRuns(t *testing.T, output string, code int) *harness {
	t.Helper()
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = []scriptedRun{{Output: output, Code: code}}
	admitted(t, h)
	return h
}

func TestAGatesStartIsRecordedWithItsSelection(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] daemon: passed in 3s\n", 0)

	// Assert.
	got := gateRecord(t, h, "the merge's test gate started")
	if got["suites"] != "daemon,repo-scans,e2e-kit,e2e" {
		t.Fatalf("start = %v, want the daemon's selection", got)
	}
}

func TestAGatesRunnerStartIsRecorded(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] daemon: passed in 3s\n", 0)

	// Assert.
	if got := gateRecord(t, h, "the merge's test runner began"); got["round"] == nil {
		t.Fatalf("runner start = %v, want it to name the round", got)
	}
}

func TestAGatesRunnerStartIsRecordedOnce(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] daemon: starting\n[agent-repl-tests] daemon: passed in 3s\n", 0)

	// Assert.
	if got := gateRecords(h, "the merge's test runner began"); got != 1 {
		t.Fatalf("runner start records = %d, want one", got)
	}
}

func TestALineBeforeTheRunnerRecordsNoRunnerStart(t *testing.T) {
	// Act.
	h := gateRuns(t, "[safe-test-run] checkpoint taken\n", 0)

	// Assert.
	if got := gateRecords(h, "the merge's test runner began"); got != 0 {
		t.Fatalf("runner start records = %d, want none before the runner's first line", got)
	}
}

func TestAGatesRunnerEndIsRecordedWithItsExit(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", 1)

	// Assert.
	got := gateRecord(t, h, "test runner finished")
	if got["exit_code"] != 1 {
		t.Fatalf("runner end = %v, want exit 1", got)
	}
	if _, timed := got["run_seconds"]; !timed {
		t.Fatalf("runner end = %v, want its run seconds", got)
	}
}

func TestAPassingGatesVerdictIsRecordedWithItsTotal(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] daemon: passed in 3s\n", 0)

	// Assert.
	got := gateRecord(t, h, "the merge's suites passed")
	if _, timed := got["total_seconds"]; !timed || got["verdict"] != "passed" {
		t.Fatalf("verdict = %v, want passed with its total", got)
	}
}

func TestAFailingGatesVerdictIsRecordedWithItsTotal(t *testing.T) {
	// Act.
	h := gateRuns(t, "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", 1)

	// Assert.
	got := gateRecord(t, h, "the merge's suites failed")
	if _, timed := got["total_seconds"]; !timed || got["verdict"] != "failed" {
		t.Fatalf("verdict = %v, want failed with its total", got)
	}
}

func TestAnEmptySelectionsVerdictIsRecordedWithItsTotal(t *testing.T) {
	// Arrange.
	h := docsOnlyMerge(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	got := gateRecord(t, h, "no suite is selected")
	if _, timed := got["total_seconds"]; !timed || got["verdict"] != "no suite selected" {
		t.Fatalf("verdict = %v, want no suite selected with its total", got)
	}
}

func TestAGateTheDaemonsExitCutIsRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.landsCleanly("abc123def4567")

	// Act.
	heldInGate(t, h)

	// Assert.
	got := gateRecord(t, h, "test gate was cut before its verdict")
	if _, timed := got["since_gate_start_seconds"]; !timed {
		t.Fatalf("cut = %v, want how long the gate had run", got)
	}
}

func TestAGateTheDaemonsExitCutNamesTheExit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.landsCleanly("abc123def4567")

	// Act.
	heldInGate(t, h)

	// Assert.
	got := gateRecord(t, h, "test gate was cut before its verdict")
	if got["by"] != suspendExit.by {
		t.Fatalf("cut by = %v, want %q", got["by"], suspendExit.by)
	}
	if gateRecords(h, suspendExit.gateRerun) != 1 {
		t.Fatalf("the cut does not say %q", suspendExit.gateRerun)
	}
}

func TestAGateAnEvictionCutNamesTheAbandonment(t *testing.T) {
	// Arrange: the head holds in its gate.
	c, done := headInGate(t)

	// Act.
	if err := c.h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	<-done

	// Assert.
	got := gateRecord(t, c.h, "test gate was cut before its verdict")
	if by, _ := got["by"].(string); !strings.HasPrefix(by, "the merge's abandonment") {
		t.Fatalf("cut by = %v, want the merge's abandonment", got["by"])
	}
	if gateRecords(c.h, "it does not run again") != 1 {
		t.Fatal("the cut does not say the gate does not run again")
	}
}

func TestSeatWaitView(t *testing.T) {
	reading := seatwait.Reading{
		Holders:      []seatwait.Holder{{Workspace: "fix-footer"}, {Pid: 4242}},
		AvailableMiB: 17408, WiredMiB: 8192, CeilingMiB: 20480, FloorMiB: 10240, Rank: 10,
	}
	holders := []*frontendv1.FeedMergeTestsRunSeatHolder{
		{Holder: &frontendv1.FeedMergeTestsRunSeatHolder_Workspace{Workspace: &frontendv1.FeedMergeTestsRunSeatHolderWorkspace{WorkspaceName: "fix-footer"}}},
		{Holder: &frontendv1.FeedMergeTestsRunSeatHolder_Outside{Outside: &frontendv1.FeedMergeTestsRunSeatHolderOutside{Pid: 4242}}},
	}
	cases := []struct {
		name   string
		reason seatwait.Reason
		want   *frontendv1.FeedMergeTestsRunSeatWait
	}{
		{name: "every seat held names the ceiling", reason: seatwait.Ceiling, want: &frontendv1.FeedMergeTestsRunSeatWait{StartedAtMs: 7, Holders: holders,
			Reason: &frontendv1.FeedMergeTestsRunSeatWait_Ceiling{Ceiling: &frontendv1.FeedMergeTestsRunSeatWaitCeiling{CeilingMib: 20480}}}},
		{name: "a memory wait names what is available and the floor", reason: seatwait.Memory, want: &frontendv1.FeedMergeTestsRunSeatWait{StartedAtMs: 7, Holders: holders,
			Reason: &frontendv1.FeedMergeTestsRunSeatWait_Memory{Memory: &frontendv1.FeedMergeTestsRunSeatWaitMemory{AvailableMib: 17408, FloorMib: 10240}}}},
		{name: "a wait behind a better rank", reason: seatwait.Outranked, want: &frontendv1.FeedMergeTestsRunSeatWait{StartedAtMs: 7, Holders: holders,
			Reason: &frontendv1.FeedMergeTestsRunSeatWait_Outranked{Outranked: &frontendv1.FeedMergeTestsRunSeatWaitOutranked{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := seatWaitView(seatwait.Wait{Reason: tc.reason, Reading: reading}, 7)

			// Assert
			if !proto.Equal(got, tc.want) {
				t.Fatalf("seatWaitView = %v, want %v", got, tc.want)
			}
		})
	}
}

// seatWaitGate scripts one passing gate whose runner waits for a RAM disk
// seat (all held by fix-footer, then for memory behind fix-footer and a run
// started by hand), takes one, and runs the daemon suite.
func (h *harness) seatWaitGate() {
	full := seatwait.Wait{Reason: seatwait.Ceiling, Reading: seatwait.Reading{Holders: []seatwait.Holder{{Workspace: "fix-footer"}}, AvailableMiB: 30720, WiredMiB: 20480, CeilingMiB: 20480, FloorMiB: 10240, Rank: 10}}
	memory := seatwait.Wait{Reason: seatwait.Memory, Reading: seatwait.Reading{Holders: []seatwait.Holder{{Workspace: "fix-footer"}, {Pid: 99}}, AvailableMiB: 17408, WiredMiB: 8192, CeilingMiB: 20480, FloorMiB: 10240, Rank: 10}}
	out := "[agent-repl-tests] " + full.Line() + "\n" +
		"[agent-repl-tests] " + memory.Line() + "\n" +
		"[agent-repl-tests] " + seatwait.TookLine(1, 5, "1m40s", memory.Reading) + "\n" +
		"[agent-repl-tests] daemon: starting\n" +
		"[agent-repl-tests] daemon: passed in 2s\n"
	h.runner.runs = append(h.runner.runs, scriptedRun{Output: out, Code: 0})
}

func TestTheTestsTabDrawsTheRunnersSeatWaitUntilItTakesASeat(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.seatWaitGate()

	// Act
	admitted(t, h)

	// Assert: the waits drawn, in order, each run's live output once.
	type drawn struct {
		reason  string
		holders int
		started int64
	}
	var waits []drawn
	clearedAfterWait := false
	h.feed.mu.Lock()
	for _, row := range h.feed.rows {
		run := row.Row.GetMergeTab().GetTestsRun()
		if run == nil || run.GetLive() == nil {
			continue
		}
		w := run.GetOutput().GetSeatWait()
		if w == nil {
			clearedAfterWait = clearedAfterWait || len(waits) > 0
			continue
		}
		reason := "memory"
		if w.GetCeiling() != nil {
			reason = "ceiling"
		}
		d := drawn{reason: reason, holders: len(w.GetHolders()), started: w.GetStartedAtMs()}
		if len(waits) == 0 || waits[len(waits)-1] != d {
			waits = append(waits, d)
		}
	}
	h.feed.mu.Unlock()
	if len(waits) != 2 || waits[0].reason != "ceiling" || waits[0].holders != 1 || waits[1].reason != "memory" || waits[1].holders != 2 {
		t.Fatalf("drawn waits = %+v, want the ceiling wait behind one holder, then the memory wait behind two", waits)
	}
	if waits[0].started != waits[1].started || waits[0].started == 0 {
		t.Fatalf("the waits began at %d and %d; want one wait, one start", waits[0].started, waits[1].started)
	}
	if !clearedAfterWait {
		t.Fatal("the seat wait was never cleared once the runner took its seat")
	}
}

func TestAnUnreadableSeatWaitLineIsRecordedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := &gateRun{r: &run{o: h.o, ws: theWorkspace}, states: map[string]suiteState{}, counts: newSuiteCounts()}
	line := "[agent-repl-tests] " + seatwait.Marker + "waiting, reason=gremlins (x); holders=[]; available=1MiB wired=0MiB ceiling=1MiB floor=1MiB rank=0"

	// Act
	g.edge(line)

	// Assert
	recorded := false
	for _, rec := range h.logs.Records() {
		if rec.Operation == "daemon.merge.tests" && rec.Level == "error" && strings.Contains(fmt.Sprint(rec.Context["error"]), "gremlins") {
			recorded = true
		}
	}
	if !recorded || g.seatWait != nil {
		t.Fatalf("recorded %v, seat wait %v; want the line recorded at ERROR and no wait drawn", recorded, g.seatWait)
	}
}

// THE GATE NAMES ITS REQUESTING WORKSPACE TO ITS RUNNER, which writes the name
// into the RAM disk seat it holds, so a run waiting behind it names it.
func TestAGatesCommandNamesTheRequestingWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)

	// Act
	admitted(t, h)

	// Assert
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.gateWorkspaces) == 0 || h.gateWorkspaces[0] != "ws-one" {
		t.Fatalf("the gate's commands named %v, want the requesting workspace ws-one", h.gateWorkspaces)
	}
}

// runnerUnavailable scripts a gate whose runner could not be built
// (bin/test-all.sh RUNNER_UNAVAILABLE), as the 2026-11-14 20:47:31 run of
// resolved-vendor-notices-clear printed it.
var runnerUnavailable = scriptedRun{Code: gateRunnerUnavailable, Output: "" +
	"go/src/crypto/tls/handshake_client.go:14:2: open go/src/crypto/internal/fips140/tls13/cast.go: too many open files in system\n" +
	"[agent-repl-tests] ERROR: building the test runner in /w/modules/app/agent-repl/testrun failed\n"}

// hostFaulted runs the head's gate to a runner that could not be built, and
// answers once the hold's pause has let the run go.
func hostFaulted(t *testing.T) *chain {
	t.Helper()
	c := newChain(t)
	c.runner.script(c.dirs[theWorkspace], runnerUnavailable)
	enqueue(t, c.h)
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}
	return c
}

func TestAGateWhoseRunnerCannotBeBuiltHoldsTheQueueInTheHostsName(t *testing.T) {
	// Arrange, Act.
	c := hostFaulted(t)

	// Assert.
	c.h.db.mu.Lock()
	pause := c.h.db.pauses[c.h.repoKey()]
	c.h.db.mu.Unlock()
	if pause == nil || pause.By.Kind != wsm.MergeQueuePauserHost || !strings.Contains(pause.By.Cause, "too many open files in system") || pause.ToLand {
		t.Fatalf("pause = %+v, want the host's pause naming the cause", pause)
	}
}

func TestAGateWhoseRunnerCannotBeBuiltSpendsNoFixingAttempt(t *testing.T) {
	// Arrange, Act.
	c := hostFaulted(t)

	// Assert.
	for _, f := range c.h.footer.all() {
		if f.Step == "fixing" || f.State == StateFailed {
			t.Fatalf("facts %+v: the host's fault was taken as the branch's", f)
		}
	}
	queued, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil || len(queued) != 1 || queued[0].State != wsm.MergeAdmitted {
		t.Fatalf("queue = (%v, %v), want the merge still admitted in line", queued, err)
	}
}

func TestAGateWhoseRunnerCannotBeBuiltIsRecordedAtErrorNamingTheCause(t *testing.T) {
	// Arrange, Act.
	c := hostFaulted(t)

	// Assert.
	rec, ok := c.h.recordFor("error", "daemon.merge.tests")
	if !ok || rec.Context["verdict"] != "host_fault" || !strings.Contains(rec.Context["cause"].(string), "too many open files in system") ||
		!strings.Contains(rec.Context["cause"].(string), "building the test runner") {
		t.Fatalf("ERROR record = %+v (%v), want the host's fault naming its cause", rec, ok)
	}
}

func TestAGateTheHostHeldRunsAgainFromItsStartOnceTheQueueIsResumed(t *testing.T) {
	// Arrange.
	c := hostFaulted(t)

	// Act.
	if err := c.h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}
	c.promote(t)

	// Assert.
	if ran := c.runner.ran(c.dirs[theWorkspace]); ran != 2 {
		t.Fatalf("gate ran %d times, want the faulted run and its rerun", ran)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings %v, want the merge landed once", got)
	}
}

func TestAGateWhoseRunnerCannotBeStartedHoldsTheQueueInTheHostsName(t *testing.T) {
	// Arrange.
	c := newChain(t)
	c.runner.script(c.dirs[theWorkspace], scriptedRun{Err: errors.New("fork/exec bash: too many open files")})
	enqueue(t, c.h)

	// Act.
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	c.h.db.mu.Lock()
	pause := c.h.db.pauses[c.h.repoKey()]
	c.h.db.mu.Unlock()
	if pause == nil || pause.By.Kind != wsm.MergeQueuePauserHost || !strings.Contains(pause.By.Cause, "could not be started") {
		t.Fatalf("pause = %+v, want the host's pause naming the start's failure", pause)
	}
}

func TestAHoldThatCannotBeWrittenFailsTheMergeLoudly(t *testing.T) {
	// Arrange.
	c := newChain(t)
	c.runner.script(c.dirs[theWorkspace], runnerUnavailable)
	c.h.db.mu.Lock()
	c.h.db.pauseWriteErr = errors.New("db gone")
	c.h.db.mu.Unlock()
	enqueue(t, c.h)

	// Act.
	<-admitAsync(c.h, context.Background())

	// Assert.
	if !c.recorded("error", "daemon.merge.tests", "could not hold the queue for the host's fault") {
		t.Fatal("the hold's failure was not recorded at ERROR")
	}
	c.h.db.mu.Lock()
	defer c.h.db.mu.Unlock()
	if c.h.db.paused[c.h.repoKey()] {
		t.Fatal("a hold that could not be written left the queue paused")
	}
}

func TestRunnerUnavailableCauseNamesWhatTheRunnerHit(t *testing.T) {
	tests := []struct {
		name string
		tail string
		want string
	}{
		{name: "the descriptor table full", tail: runnerUnavailable.Output,
			want: "go/src/crypto/tls/handshake_client.go:14:2: open go/src/crypto/internal/fips140/tls13/cast.go: too many open files in system; [agent-repl-tests] ERROR: building the test runner in /w/modules/app/agent-repl/testrun failed"},
		{name: "no go on PATH", tail: "[agent-repl-tests] ERROR: go is not on PATH; the test runner (testrun) is a Go program\n",
			want: "[agent-repl-tests] ERROR: go is not on PATH; the test runner (testrun) is a Go program"},
		{name: "a tail naming nothing", tail: "\n", want: "the test runner could not be built or started (exit 75)"},
		{name: "only the last three lines", tail: "ERROR a\nERROR b\nERROR c\nERROR d\n", want: "ERROR b; ERROR c; ERROR d"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := runnerUnavailableCause(tt.tail)

			// Assert.
			if got != tt.want {
				t.Fatalf("cause = %q, want %q", got, tt.want)
			}
		})
	}
}

// branchConfigFails arranges a landing whose rebased branch's lifecycle
// config fails its read with ERR, while its target's reads as configured.
func branchConfigFails(t *testing.T, err error) *harness {
	t.Helper()
	h := newHarness(t)
	landing(h, 1)
	h.mu.Lock()
	h.configErrAt = map[string]error{h.sourceD: err}
	h.mu.Unlock()
	return h
}

func TestABranchConfigThisDaemonCannotReadFailsTheMergeBeforeItsGate(t *testing.T) {
	tests := []struct {
		name       string
		err        error
		wantDetail string
	}{
		{name: "a field this build does not read", err: &lifecycleconfig.InvalidError{Path: "/ws/.agent-repl/lifecycle.yaml", Detail: `unknown field "new_field"`},
			wantDetail: "cannot read (unknown field \"new_field\"); land the support for a new field first"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := branchConfigFails(t, tt.err)

			// Act.
			admitted(t, h)

			// Assert.
			if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" || !strings.Contains(got.Detail, tt.wantDetail) {
				t.Fatalf("facts = %+v, want the merge failed in other naming %q", got, tt.wantDetail)
			}
		})
	}
}

func TestABranchCarryingNoLifecycleConfigIsGatedAsUsual(t *testing.T) {
	// Arrange: the repository's config lives in its main checkout alone.
	h := branchConfigFails(t, &lifecycleconfig.MissingError{Path: "/ws/.agent-repl/lifecycle.yaml"})

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateMerged {
		t.Fatalf("facts = %+v, want the merge landed", got)
	}
}

func TestABranchConfigThisDaemonCannotReadStartsNoRunner(t *testing.T) {
	// Arrange.
	h := branchConfigFails(t, &lifecycleconfig.InvalidError{Path: "/ws/.agent-repl/lifecycle.yaml", Detail: "bad"})

	// Act.
	admitted(t, h)

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 0 {
		t.Fatalf("the runner was started %d times, want none", len(h.runner.argv))
	}
}

func TestABranchConfigThisDaemonCannotReadIsRecordedAtWarn(t *testing.T) {
	// Arrange.
	h := branchConfigFails(t, &lifecycleconfig.InvalidError{Path: "/ws/.agent-repl/lifecycle.yaml", Detail: "bad"})

	// Act.
	admitted(t, h)

	// Assert.
	for _, rec := range h.logs.Records() {
		if rec.Operation == "daemon.merge.branch_config" && rec.Level == "warn" {
			if rec.Context["dir"] != h.sourceD || !strings.Contains(fmt.Sprint(rec.Context["error"]), "bad") {
				t.Fatalf("record context = %v, want the branch's dir and the read's error", rec.Context)
			}
			return
		}
	}
	t.Fatalf("no daemon.merge.branch_config warning among %v", h.logs.Records())
}

func TestABranchConfigReadThatFailsOtherwiseEndsTheMergeOnTheError(t *testing.T) {
	// Arrange.
	h := branchConfigFails(t, errors.New("permission denied"))

	// Act.
	admitted(t, h)

	// Assert: the run aborts on the read's error.
	if got := h.footer.last(); got.State != StateFailed || !strings.Contains(got.Detail, "permission denied") {
		t.Fatalf("facts = %+v, want the merge failed on the read's error", got)
	}
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 0 {
		t.Fatalf("the runner was started %d times, want none", len(h.runner.argv))
	}
}

func TestAGateWhoseWorkspaceNameCannotBeReadRunsNothing(t *testing.T) {
	// Arrange: the merging workspace's record is gone, so the runner's RAM
	// disk seat has no name to be written under.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	h.git.changedErr = errors.New("git diff failed")
	round := r.openTab(context.Background(), TabTests)
	h.o.mu.Lock()
	h.o.running[h.repoKey()] = r
	h.o.mu.Unlock()
	h.db.mu.Lock()
	delete(h.db.workspaces, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	_, err := r.gate(context.Background(), round, "tip", "head")

	// Assert.
	if !errors.Is(err, wsm.ErrNotFound) {
		t.Fatalf("gate err = %v, want the name's read failure", err)
	}
	h.runner.mu.Lock()
	started := len(h.runner.argv)
	h.runner.mu.Unlock()
	if started != 0 {
		t.Fatalf("the runner was started %d times with no name for its seat, want none", started)
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.tests", text: "could not read a workspace's name", context: map[string]string{"workspace": string(theWorkspace)}}) {
		t.Fatalf("no ERROR record names the unreadable workspace: %+v", h.logs.Records())
	}
}
