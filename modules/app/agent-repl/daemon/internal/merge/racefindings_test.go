package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/encoding/protodelim"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// unit is a unit verdict of SUITE under DIGEST ("" for none) with TESTS, each
// "name" passed or "!name" failed; a unit with a failed test failed, its
// output tail TAIL.
func unit(suite, digest, tail string, tests ...string) *testrunv1.TestRunUnitEnded {
	u := &testrunv1.TestRunUnitEnded{UnitId: suite + ":x#00", Suite: suite}
	if digest != "" {
		u.InputsDigest = &digest
	}
	failed := false
	for _, name := range tests {
		r := &testrunv1.TestRunTestResult{Name: strings.TrimPrefix(name, "!")}
		if strings.HasPrefix(name, "!") {
			failed = true
			r.Outcome = &testrunv1.TestRunTestResult_Failed{Failed: &testrunv1.TestRunTestFailed{}}
		} else {
			r.Outcome = &testrunv1.TestRunTestResult_Passed{Passed: &testrunv1.TestRunTestPassed{}}
		}
		u.Tests = append(u.Tests, r)
	}
	if failed {
		u.Verdict = &testrunv1.TestRunUnitEnded_Failed{Failed: &testrunv1.TestRunUnitFailed{ExitCode: 1, OutputTail: tail}}
	} else {
		u.Verdict = &testrunv1.TestRunUnitEnded_Passed{Passed: &testrunv1.TestRunUnitPassed{}}
	}
	return u
}

var racyRef = wsm.RaceFindingRef{Suite: "daemon", Test: "TestRacy"}

func TestTestOutcomesReadsEachTestUnderItsUnitsDigest(t *testing.T) {
	tests := []struct {
		name       string
		units      []*testrunv1.TestRunUnitEnded
		wantFailed []string
		wantPassed []string
	}{
		{name: "a failed and a passed test under the unit's digest",
			units:      []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "boom", "!TestA", "TestB")},
			wantFailed: []string{"daemon/TestA@d1:boom"}, wantPassed: []string{"daemon/TestB@d1"}},
		{name: "a unit with no digest proves nothing",
			units: []*testrunv1.TestRunUnitEnded{unit("daemon", "", "boom", "!TestA", "TestB")}},
		{name: "a subtest's failure fails its top-level test",
			units:      []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "boom", "!TestA/case_one")},
			wantFailed: []string{"daemon/TestA@d1:boom"}},
		{name: "a test is no pass while one of its subtests failed",
			units:      []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "boom", "TestA/case_one", "!TestA/case_two")},
			wantFailed: []string{"daemon/TestA@d1:boom"}},
		{name: "a test passed in two units of one digest is one pass",
			units:      []*testrunv1.TestRunUnitEnded{unit("e2e", "d1", "", "TestLanding/a"), unit("e2e", "d1", "", "TestLanding/b")},
			wantPassed: []string{"e2e/TestLanding@d1"}},
		{name: "a vitest name keeps its slashes",
			units:      []*testrunv1.TestRunUnitEnded{unit("webapp", "d1", "", "test/a.test.ts > a / b")},
			wantPassed: []string{"webapp/test/a.test.ts > a / b@d1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			failed, passed := TestOutcomes(tt.units)

			// Assert
			var gotFailed, gotPassed []string
			for _, f := range failed {
				gotFailed = append(gotFailed, f.ref.String()+"@"+f.digest+":"+f.failure)
			}
			for _, p := range passed {
				gotPassed = append(gotPassed, p.Ref.String()+"@"+p.InputsDigest)
			}
			if !slices.Equal(gotFailed, tt.wantFailed) || !slices.Equal(gotPassed, tt.wantPassed) {
				t.Fatalf("failed %v passed %v, want %v and %v", gotFailed, gotPassed, tt.wantFailed, tt.wantPassed)
			}
		})
	}
}

// writeEvents writes UNITS to a run's events file under T's temp dir.
func writeEvents(t *testing.T, units ...*testrunv1.TestRunUnitEnded) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "x-tests-1-events.log")
	if err := writeRunEvents([]string{"env", testEventsFileEnv + "=" + path}, units); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestReadRunEvents(t *testing.T) {
	tests := []struct {
		name      string
		path      func(t *testing.T) string
		wantUnits int
		wantErr   string
	}{
		{name: "every unit verdict is read, and no other event",
			path: func(t *testing.T) string {
				path := writeEvents(t, unit("daemon", "d1", "", "TestA"), unit("e2e", "d2", "", "TestB"))
				file, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0)
				if err != nil {
					t.Fatal(err)
				}
				defer file.Close()
				if _, err := protodelim.MarshalTo(file, &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: &testrunv1.TestRunEnded{}}}); err != nil {
					t.Fatal(err)
				}
				return path
			},
			wantUnits: 2},
		{name: "a missing file is an error",
			path:    func(t *testing.T) string { return filepath.Join(t.TempDir(), "none") },
			wantErr: "open the run's events"},
		{name: "a file that is no event stream is an error",
			path: func(t *testing.T) string {
				path := filepath.Join(t.TempDir(), "bad")
				if err := os.WriteFile(path, []byte{0xff, 0xff, 0xff, 0xff, 0x0f}, 0o644); err != nil {
					t.Fatal(err)
				}
				return path
			},
			wantErr: "read the run's events"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ReadRunEvents(tt.path(t))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("ReadRunEvents = %v, want an error naming %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || len(got) != tt.wantUnits {
				t.Fatalf("ReadRunEvents = (%d units, %v), want %d", len(got), err, tt.wantUnits)
			}
		})
	}
}

func TestTheGateHandsTheRunnerAnEventsFileBesideItsLog(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)

	// Act
	admitted(t, h)

	// Assert
	var events string
	for _, arg := range h.runner.argv[0] {
		if path, ok := strings.CutPrefix(arg, testEventsFileEnv+"="); ok {
			events = path
		}
	}
	if !strings.HasSuffix(events, "-tests-1-events.log") || filepath.Dir(events) != LogsDir(h.stateDir) {
		t.Fatalf("gate argv = %v, want the events file beside the gate's log in its environment", h.runner.argv[0])
	}
	if !IsLog(filepath.Base(events)) {
		t.Fatalf("%s is no log the retention pass keeps and prunes", events)
	}
}

func TestAGateFailureThatPassesInTheRepairsRerunIsARaceAndTheSuitesRunAgain(t *testing.T) {
	// Arrange: the gate fails TestRacy under d1; the repair's rerun passes it
	// under d1 and commits nothing; the next gate passes.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = []scriptedRun{
		{Output: "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", Code: 1, Units: []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "racy_test.go:9: lost the race", "!TestRacy")}},
		{Output: "[agent-repl-tests] daemon: passed in 3s\n"},
	}
	h.git.fixesCommitNothing = true
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR {
			h.o.promoteRaces(context.Background(), h.repoKey(), []wsm.TestPass{{Ref: racyRef, InputsDigest: "d1"}}, nil)
		}
	}

	// Act
	admitted(t, h)

	// Assert
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged after the suites ran again", got)
	}
	findings, err := h.db.RaceFindings(context.Background(), h.repoKey())
	if err != nil || len(findings) != 1 || findings[0].Ref != racyRef || len(findings[0].Merges) != 1 || !strings.Contains(findings[0].Failure, "lost the race") {
		t.Fatalf("findings = (%+v, %v), want TestRacy found in the merge with its failure", findings, err)
	}
	if !slices.Contains(h.meter.all(), "race_finding:new") {
		t.Fatalf("metrics %v, want the new finding counted", h.meter.all())
	}
	if got := h.recordsFor("info", "daemon.merge.fixes", "the fixing attempt committed nothing, but every test the gate failed passed on its inputs and is a race finding the deploy mediator owns; the suites run again"); len(got) != 1 {
		t.Fatalf("records %+v, want the rerun of the suites recorded once", got)
	}
}

func TestAFixThatCommitsNothingFailsTheMergeWhileAFailureIsNoRace(t *testing.T) {
	tests := []struct {
		name  string
		units []*testrunv1.TestRunUnitEnded
	}{
		{name: "a failure no pass promoted", units: []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "boom", "!TestRacy")}},
		{name: "a failed unit with no digest", units: []*testrunv1.TestRunUnitEnded{unit("daemon", "", "boom", "!TestRacy")}},
		{name: "a failed unit naming no test", units: []*testrunv1.TestRunUnitEnded{{UnitId: "daemon:build", Suite: "daemon", InputsDigest: ptrTo("d1"),
			Verdict: &testrunv1.TestRunUnitEnded_Failed{Failed: &testrunv1.TestRunUnitFailed{ExitCode: 2}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			landing(h, 1)
			h.runner.runs = []scriptedRun{
				{Output: "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", Code: 1, Units: tt.units},
				{Output: "[agent-repl-tests] daemon: passed in 3s\n"},
			}
			h.git.fixesCommitNothing = true

			// Act
			admitted(t, h)

			// Assert
			if got := h.footer.last(); got.State != StateFailed || !strings.Contains(got.Detail, "committed nothing") {
				t.Fatalf("facts = %+v, want the merge failed for a fix that committed nothing", got)
			}
		})
	}
}

// ptrTo is a pointer to S.
func ptrTo(s string) *string { return &s }

func TestAPassingGatePromotesAnEarlierGatesFailure(t *testing.T) {
	// Arrange: another merge's gate failed TestRacy under d1.
	h := newHarness(t)
	landing(h, 1)
	ctx := context.Background()
	if err := h.db.RecordGateFailures(ctx, h.repoKey(), []wsm.GateTestFailure{{Ref: racyRef, InputsDigest: "d1", Merge: "other-merge", FailedAt: time.Unix(1, 0), Failure: "boom"}}); err != nil {
		t.Fatal(err)
	}
	h.runner.runs = []scriptedRun{{Output: "[agent-repl-tests] daemon: passed in 3s\n", Units: []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "", "TestRacy")}}}

	// Act
	admitted(t, h)

	// Assert
	findings, err := h.db.RaceFindings(ctx, h.repoKey())
	if err != nil || len(findings) != 1 || !slices.Equal(findings[0].Merges, []string{"other-merge"}) {
		t.Fatalf("findings = (%+v, %v), want the other merge's failure found a race", findings, err)
	}
	logged := h.recordsFor("info", opRaceFinding, "a test that failed in a merge gate passed on the same inputs: its failure is a race, handed to the deploy mediator's next check")
	if len(logged) != 1 || logged[0].Context["finding"] != "daemon/TestRacy" || logged[0].Context["event"] != RaceFindingNew {
		t.Fatalf("records %+v, want the finding recorded once", logged)
	}
}

func TestAGateWhoseResultsCannotBeReadIsRecordedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.observeGateRaces(context.Background(), h.repoKey(), "m", filepath.Join(t.TempDir(), "gone"), "/archive", time.Unix(1, 0))

	// Assert
	logged := h.recordsFor("error", opRaceFinding, "could not read the gate's test results; its failures are no race candidates and its passes promote none")
	if len(logged) != 1 || logged[0].Context["merge"] != "m" {
		t.Fatalf("records %+v, want the unread results recorded at ERROR", logged)
	}
}

func TestAWorkspacesRunPromotesItsRepositorysCandidates(t *testing.T) {
	tests := []struct {
		name         string
		commonDirErr error
		wantFindings int
		wantError    bool
	}{
		{name: "a pass on the failure's digest is a race", wantFindings: 1},
		{name: "a repository that cannot be resolved promotes nothing, at ERROR", commonDirErr: errors.New("not a repository"), wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx := context.Background()
			repo := wsm.RepoKey(h.targetD + "/.git")
			if err := h.db.RecordGateFailures(ctx, repo, []wsm.GateTestFailure{{Ref: racyRef, InputsDigest: "d1", Merge: "m", FailedAt: time.Unix(1, 0)}}); err != nil {
				t.Fatal(err)
			}
			h.git.commonDirErr = tt.commonDirErr

			// Act
			h.o.ObserveTestRun(ctx, h.targetD, "ws-a", []*testrunv1.TestRunUnitEnded{unit("daemon", "d1", "", "TestRacy")})

			// Assert
			findings, err := h.db.RaceFindings(ctx, repo)
			if err != nil || len(findings) != tt.wantFindings {
				t.Fatalf("findings = (%+v, %v), want %d", findings, err, tt.wantFindings)
			}
			logged := h.recordsFor("error", opRaceFinding, "could not resolve the repository of a workspace's test run; its passes promote no race candidate")
			if (len(logged) == 1) != tt.wantError {
				t.Fatalf("records %+v, want the resolution failure recorded: %v", logged, tt.wantError)
			}
		})
	}
}

// foundRace records racyRef as a race finding of the checked repository.
func foundRace(t *testing.T, h *harness) {
	t.Helper()
	ctx := context.Background()
	if err := h.db.RecordGateFailures(ctx, mediationKey(h), []wsm.GateTestFailure{{Ref: racyRef, InputsDigest: "d1", Merge: "merge-a", FailedAt: time.Unix(1, 0), Failure: "racy_test.go:9: lost the race", Artifacts: "/archive/a"}}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.PromoteRaces(ctx, mediationKey(h), []wsm.TestPass{{Ref: racyRef, InputsDigest: "d1"}}); err != nil {
		t.Fatal(err)
	}
}

func TestAnOpenRaceFindingIsPutToTheMediatorWhateverTheWindowHolds(t *testing.T) {
	// Arrange: a quiet window, and an open race.
	h := checked(t)
	owe(t, h)
	foundRace(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if owedCheck(t, h) == nil {
		t.Fatal("the check was released, want it held for the mediator")
	}
	var races string
	for _, values := range h.briefValues {
		if told, ok := values["race_findings"]; ok {
			races = told
		}
	}
	if !strings.Contains(races, "`daemon/TestRacy`") || !strings.Contains(races, "merge-a") || !strings.Contains(races, "lost the race") || !strings.Contains(races, "/archive/a") {
		t.Fatalf("race_findings = %q, want the open race with its merge, failure and artifacts", races)
	}
}

func TestADispatchNamingARaceFindingTakesItOn(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	foundRace(t, h)

	// Act
	err := dispatched(h, Decision{Problems: []DeployProblem{{Statement: "TestRacy races", Evidence: []string{"lost the race"}, RaceFindings: []wsm.RaceFindingRef{racyRef}}}})

	// Assert
	findings, readErr := h.db.RaceFindings(context.Background(), mediationKey(h))
	if err != nil || readErr != nil || len(findings) != 1 || findings[0].Open() {
		t.Fatalf("dispatch = %v, findings (%+v, %v); want the race taken on", err, findings, readErr)
	}
	if got := RenderRaceFindings(findings); !strings.Contains(got, "At work") || strings.Contains(got, "Open,") {
		t.Fatalf("the next brief's races = %q, want it at work and not open", got)
	}
}

func TestADecisionNamingAnUnknownRaceFindingIsRefused(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	unknown := wsm.RaceFindingRef{Suite: "daemon", Test: "TestNobodySaw"}

	// Act
	err := dispatched(h, Decision{Problems: []DeployProblem{{Statement: "s", Evidence: []string{"e"}, RaceFindings: []wsm.RaceFindingRef{unknown}}}})

	// Assert
	refused, ok := Refused(err)
	if !ok || refused.Arm != ArmUnknownRaceFinding {
		t.Fatalf("DecideMediation = %v, want unknown_race_finding", err)
	}
	finding, _ := refused.Fields["finding"].(*agentreplv1.QueueRaceFindingRef)
	if finding.GetSuite() != "daemon" || finding.GetTest() != "TestNobodySaw" {
		t.Fatalf("refusal fields %+v, want the unknown finding named", refused.Fields)
	}
	if m := owedCheck(t, h); m == nil || m.Dispatched {
		t.Fatalf("check %+v, want it owed and nothing dispatched", m)
	}
	if got := h.recordsFor("info", opMediation, "refused a decision naming a race finding the repository does not hold"); len(got) != 1 {
		t.Fatalf("records %+v, want the refusal recorded once", got)
	}
}

func TestARemediationsDeployMarksItsRaceFindingsFixed(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx := context.Background()
	foundRace(t, h)
	if err := h.db.TakeOnRaceFindings(ctx, mediationKey(h), []wsm.RaceFindingRef{racyRef}, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, "ws-fixer", tipAfter, nil, deployStarted); err != nil {
		t.Fatal(err)
	}
	coverLandings(t, h)

	// Act
	err := h.o.OweMediations(ctx, deployStarted.Add(time.Minute))

	// Assert
	findings, readErr := h.db.RaceFindings(ctx, mediationKey(h))
	if err != nil || readErr != nil || len(findings) != 1 || findings[0].FixedAt == nil {
		t.Fatalf("OweMediations = %v, findings (%+v, %v); want the race fixed", err, findings, readErr)
	}
}

func TestRenderRaceFindings(t *testing.T) {
	at := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	long := strings.Repeat("x", raceFailureBytes) + "THE END"
	tests := []struct {
		name     string
		findings []wsm.RaceFinding
		want     []string
		wantNot  []string
	}{
		{name: "none", want: []string{"None:"}},
		{name: "an open race names its merges, failure and artifacts",
			findings: []wsm.RaceFinding{{Ref: racyRef, LastSeen: at, Recurrences: 2, Merges: []string{"a", "b"}, Failure: "lost", Artifacts: "/x"}},
			want:     []string{"Open, each yours to dispatch:", "`daemon/TestRacy`: failed in 2 merge gate(s), last at 2026-11-25T12:00:00Z, in a, b", "Artifacts: `/x`.", "  > lost"}},
		{name: "a reopened race says its fixes did not end it",
			findings: []wsm.RaceFinding{{Ref: racyRef, LastSeen: at, Recurrences: 3, Fixes: 1, Failure: "lost"}},
			want:     []string{"it came back after 1 deployed fix(es)"}},
		{name: "a long failure is quoted from its end",
			findings: []wsm.RaceFinding{{Ref: racyRef, LastSeen: at, Recurrences: 1, Failure: long}},
			want:     []string{"> ...", "THE END"}, wantNot: []string{strings.Repeat("x", raceFailureBytes)}},
		{name: "a race taken on is at work",
			findings: []wsm.RaceFinding{{Ref: racyRef, TakenOnAt: &at, Recurrences: 1}},
			want:     []string{"At work, taken on by your earlier dispatch; never dispatch one again:", "`daemon/TestRacy`: taken on at 2026-11-25T12:00:00Z"}, wantNot: []string{"Open,"}},
		{name: "a fixed race is not listed",
			findings: []wsm.RaceFinding{{Ref: racyRef, TakenOnAt: &at, FixedAt: &at, Recurrences: 1}},
			want:     []string{"None:"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderRaceFindings(tt.findings)

			// Assert
			for _, w := range tt.want {
				if !strings.Contains(got, w) {
					t.Fatalf("RenderRaceFindings = %q, want it to hold %q", got, w)
				}
			}
			for _, w := range tt.wantNot {
				if strings.Contains(got, w) {
					t.Fatalf("RenderRaceFindings = %q, want it without %q", got, w)
				}
			}
		})
	}
}

func TestRenderProblemsNamesTheRacesAProblemIs(t *testing.T) {
	// Act
	got := renderProblems([]DeployProblem{{Statement: "TestRacy races", Evidence: []string{"lost"}, RaceFindings: []wsm.RaceFindingRef{racyRef}}})

	// Assert
	if !strings.Contains(got, "1. TestRacy races\n   race `daemon/TestRacy`: fix it at its source, never by loosening a bound, a retry or a sleep\n   > lost") {
		t.Fatalf("renderProblems = %q, want the race named between the statement and its evidence", got)
	}
}
