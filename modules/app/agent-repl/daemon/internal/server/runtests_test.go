package server

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protodelim"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/wsm"
)

// fakeTests is the test script: it writes its scripted events to the events
// pipe it is handed, prints its lines, and answers its exit.
type fakeTests struct {
	mu       sync.Mutex
	argv     []string
	dir      string
	calls    int
	selected *testrunv1.TestRunRequest
	// request is the last --redundancy request handed over.
	request *testrunv1.RedundancyRequest
	events  []*testrunv1.TestRunEvent
	// runEvents, when set, are each call's events in turn, in place of events.
	runEvents [][]*testrunv1.TestRunEvent
	garbage   bool
	lines     []string
	exit      int
	err       error
}

func (f *fakeTests) RunLinesWith(_ context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.argv, f.dir = argv, dir
	f.calls++
	events := f.events
	if f.runEvents != nil {
		events = f.runEvents[f.calls-1]
	}
	for i, a := range argv {
		if a == "--selection" {
			data, err := os.ReadFile(argv[i+1])
			if err != nil {
				return scriptrunner.Run{}, err
			}
			f.selected = &testrunv1.TestRunRequest{}
			if err := proto.Unmarshal(data, f.selected); err != nil {
				return scriptrunner.Run{}, err
			}
		}
		if a == "--redundancy" {
			data, err := os.ReadFile(argv[i+1])
			if err != nil {
				return scriptrunner.Run{}, err
			}
			f.request = &testrunv1.RedundancyRequest{}
			if err := proto.Unmarshal(data, f.request); err != nil {
				return scriptrunner.Run{}, err
			}
		}
	}
	if f.err != nil {
		return scriptrunner.Run{}, f.err
	}
	if len(opts.ExtraFiles) == 0 {
		for _, l := range f.lines {
			onLine(l)
		}
		return scriptrunner.Run{ExitCode: f.exit}, nil
	}
	pipe := opts.ExtraFiles[0]
	for _, ev := range events {
		if _, err := protodelim.MarshalTo(pipe, ev); err != nil {
			return scriptrunner.Run{}, err
		}
	}
	if f.garbage {
		if _, err := pipe.Write([]byte{0xff, 0xff, 0xff, 0xff, 0x0f, 1, 2}); err != nil {
			return scriptrunner.Run{}, err
		}
	}
	for _, l := range f.lines {
		onLine(l)
	}
	return scriptrunner.Run{ExitCode: f.exit}, nil
}

// testsWorkspace registers a workspace whose worktree holds a deploy config
// naming DEPLOY's text, and answers its ref.
func testsWorkspace(t *testing.T, h *harness, deploy string) *workspacev1.WorkspaceRef {
	t.Helper()
	dir := t.TempDir()
	if deploy != "" {
		if err := os.MkdirAll(filepath.Join(dir, ".agent-repl"), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, ".agent-repl", "deploy.yaml"), []byte(deploy), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	id := ids.WorkspaceID("ws-tests")
	h.DB.workspaces[id] = wsm.Workspace{ID: id, Dir: dir, Name: "tests"}
	return &workspacev1.WorkspaceRef{Id: string(id), Dir: dir}
}

const testsDeploy = `pre_deploy:
  local_merge:
    tests:
      agent_repl_suites:
        script_path: bin/test-all.sh
deploy:
  hook:
    script:
      path: bin/deploy.sh
      timeout_seconds: 60
  runs_for:
    main: {}
`

// agent is a workspace's agent asking.
func agent() *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_Workspace{Workspace: &agentreplv1.WorkspaceRequesterWorkspace{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-asker"}}}}
}

// user is the user asking.
func user() *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_User{User: &agentreplv1.WorkspaceRequesterUser{}}}
}

func oneHarness() *testrunv1.TestRunRequest {
	return &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_Harness{Harness: &testrunv1.TestSelectionHarness{
		Script: "bin/test-x.sh", Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}}}}}
}

func endedPassed() *testrunv1.TestRunEvent {
	return &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: &testrunv1.TestRunEnded{Outcome: &testrunv1.TestRunEnded_Passed{Passed: &testrunv1.TestRunEndedPassed{PassedUnits: 1}}}}}
}

// runTests calls RunTests and answers every frame it streamed.
func runTests(t *testing.T, h *harness, req *agentreplv1.RunTestsRequest) []*agentreplv1.RunTestsResponse {
	t.Helper()
	stream, err := h.Client.RunTests(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatal(err)
	}
	defer stream.Close()
	var frames []*agentreplv1.RunTestsResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("stream: %v", err)
	}
	return frames
}

func TestRunTestsRelaysEveryEventTheRunWrites(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ref := testsWorkspace(t, h, testsDeploy)
	planned := &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Planned{Planned: &testrunv1.TestRunPlanned{Log: &testrunv1.TestRunLog{Path: "/tmp/run.log"}}}}
	unit := &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_UnitEnded{UnitEnded: &testrunv1.TestRunUnitEnded{UnitId: "u", Verdict: &testrunv1.TestRunUnitEnded_Passed{Passed: &testrunv1.TestRunUnitPassed{}}}}}
	h.Tests.events = []*testrunv1.TestRunEvent{planned, unit, endedPassed()}

	// Act
	frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), Requester: agent()})

	// Assert
	if len(frames) != 3 || !proto.Equal(frames[0].GetEvent(), planned) || !proto.Equal(frames[1].GetEvent(), unit) || frames[2].GetEvent().GetEnded().GetPassed() == nil {
		t.Fatalf("frames = %v, want the run's three events in order", frames)
	}
	if h.Tests.dir != ref.GetDir() || !proto.Equal(h.Tests.selected, oneHarness()) {
		t.Fatalf("the runner ran in %s with %v, want the worktree and the request's selection", h.Tests.dir, h.Tests.selected)
	}
	argv := strings.Join(h.Tests.argv, " ")
	if !strings.Contains(argv, filepath.Join(ref.GetDir(), "bin/test-all.sh")) || !strings.Contains(argv, "--events-fd 3") {
		t.Fatalf("argv = %s, want the worktree's configured script with the events fd", argv)
	}
}

func TestRunTestsRefuses(t *testing.T) {
	cases := []struct {
		name  string
		req   func(h *harness) *agentreplv1.RunTestsRequest
		check func(*agentreplv1.RunTestsError) bool
	}{
		{name: "a request naming no test", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: &testrunv1.TestRunRequest{}, Requester: agent()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return e.GetNoSelection() != nil }},
		{name: "a request naming no requester", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: oneHarness()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return strings.Contains(e.GetInvalid().GetDetail(), "names no requester") }},
		{name: "a recording run repeated", req: func(h *harness) *agentreplv1.RunTestsRequest {
			run := oneHarness()
			run.Record = true
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: run, Runs: 2, Requester: user()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return strings.Contains(e.GetInvalid().GetDetail(), "a recording run runs once") }},
		{name: "CPU load of no loop", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: oneHarness(), CpuLoad: &agentreplv1.RunTestsCpuLoad{}, Requester: user()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return strings.Contains(e.GetInvalid().GetDetail(), "names no busy loop") }},
		{name: "CPU load a workspace asks for", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: oneHarness(), CpuLoad: &agentreplv1.RunTestsCpuLoad{Loops: 4}, Requester: agent()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return e.GetCpuLoadRefused() != nil }},
		{name: "a workspace the daemon does not serve", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope", Dir: "/nowhere"}, Run: oneHarness(), Requester: agent()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return e.GetUnknownWorkspace() != nil }},
		{name: "a worktree with no deploy config", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, ""), Run: oneHarness(), Requester: agent()}
		}, check: func(e *agentreplv1.RunTestsError) bool {
			return e.GetRunnerFailed().GetExitCode() == -1 && strings.Contains(e.GetRunnerFailed().GetDetail(), "deploy config")
		}},
		{name: "a deploy config naming no test script", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, "pre_deploy: {}\ndeploy:\n  hook:\n    script:\n      path: bin/deploy.sh\n      timeout_seconds: 60\n  runs_for:\n    main: {}\n"), Run: oneHarness(), Requester: agent()}
		}, check: func(e *agentreplv1.RunTestsError) bool {
			return strings.Contains(e.GetRunnerFailed().GetDetail(), "names no agent_repl_suites test script")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			req := tc.req(h)

			// Act
			frames := runTests(t, h, req)

			// Assert
			if len(frames) != 1 || !tc.check(frames[0].GetError()) || frames[0].GetRun() != 1 {
				t.Fatalf("frames = %v, want the one refusal, framed as run 1", frames)
			}
			if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRunTests}); !found {
				t.Fatalf("records = %+v, want the refusal recorded", log.Records())
			}
			if h.Tests.argv != nil {
				t.Fatalf("the runner ran %v for a refused request", h.Tests.argv)
			}
		})
	}
}

func TestRunTestsAnswersARunThatNeverSaidHowItEnded(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fakeTests)
		want    string
		exit    int32
	}{
		{name: "the runner died before its end", arrange: func(f *fakeTests) { f.exit, f.lines = 2, []string{"panic: boom"} }, want: "panic: boom", exit: 2},
		{name: "the script could not run", arrange: func(f *fakeTests) { f.err = errors.New("exec: no such file") }, want: "exec: no such file", exit: -1},
		{name: "the events were not readable", arrange: func(f *fakeTests) { f.garbage = true }, want: "without reporting how its run ended", exit: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			log := dlog.NewTestLogger()
			h.Surfaces.workspace, h.Surfaces.global = log, log
			ref := testsWorkspace(t, h, testsDeploy)
			tc.arrange(h.Tests)

			// Act
			frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), Requester: agent()})

			// Assert
			failed := frames[len(frames)-1].GetError().GetRunnerFailed()
			if failed == nil || failed.GetExitCode() != tc.exit || !strings.Contains(failed.GetDetail(), tc.want) {
				t.Fatalf("frames = %v, want runner_failed exiting %d naming %q", frames, tc.exit, tc.want)
			}
			if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opRunTests}); !found {
				t.Fatalf("records = %+v, want the failure recorded at error", log.Records())
			}
		})
	}
}

func TestNewRefusesAMissingTestRunner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.Tests = nil })

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "a test runner") {
		t.Fatalf("New = %v, want the refusal naming the test runner", err)
	}
}

func endedFailed() *testrunv1.TestRunEvent {
	return &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: &testrunv1.TestRunEnded{Outcome: &testrunv1.TestRunEnded_Failed{Failed: &testrunv1.TestRunEndedFailed{FailedUnits: 1}}}}}
}

func refusedEvent() *testrunv1.TestRunEvent {
	return &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Refused{Refused: &testrunv1.TestRunRefused{Detail: "no"}}}
}

// runsOf answers the run index of every frame.
func runsOf(frames []*agentreplv1.RunTestsResponse) []uint32 {
	var out []uint32
	for _, f := range frames {
		out = append(out, f.GetRun())
	}
	return out
}

func TestRunTestsRepeatsTheRun(t *testing.T) {
	cases := []struct {
		name      string
		runs      uint32
		stop      bool
		runEvents [][]*testrunv1.TestRunEvent
		wantCalls int
		wantRuns  []uint32
	}{
		{name: "every run passing runs them all, each framed", runs: 3, runEvents: [][]*testrunv1.TestRunEvent{{endedPassed()}, {endedPassed()}, {endedPassed()}}, wantCalls: 3, wantRuns: []uint32{1, 2, 3}},
		{name: "a failing run is repeated past", runs: 3, runEvents: [][]*testrunv1.TestRunEvent{{endedFailed()}, {endedPassed()}, {endedFailed()}}, wantCalls: 3, wantRuns: []uint32{1, 2, 3}},
		{name: "stop at the first failure", runs: 3, stop: true, runEvents: [][]*testrunv1.TestRunEvent{{endedPassed()}, {endedFailed()}, {endedPassed()}}, wantCalls: 2, wantRuns: []uint32{1, 2}},
		{name: "a refused run is never repeated", runs: 3, runEvents: [][]*testrunv1.TestRunEvent{{refusedEvent()}}, wantCalls: 1, wantRuns: []uint32{1}},
		{name: "zero runs runs once", runs: 0, runEvents: [][]*testrunv1.TestRunEvent{{endedPassed()}}, wantCalls: 1, wantRuns: []uint32{1}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ref := testsWorkspace(t, h, testsDeploy)
			h.Tests.runEvents = tc.runEvents

			// Act
			frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), Runs: tc.runs, StopAtFirstFailure: tc.stop, Requester: agent()})

			// Assert
			if h.Tests.calls != tc.wantCalls || !slices.Equal(runsOf(frames), tc.wantRuns) {
				t.Fatalf("ran %d times, frames framed %v; want %d runs framed %v", h.Tests.calls, runsOf(frames), tc.wantCalls, tc.wantRuns)
			}
		})
	}
}

func TestRunTestsStopsRepeatingARunThatNeverSaidHowItEnded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.runEvents = [][]*testrunv1.TestRunEvent{{endedPassed()}, nil, {endedPassed()}}
	h.Tests.exit = 2

	// Act
	frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), Runs: 3, Requester: agent()})

	// Assert
	last := frames[len(frames)-1]
	if h.Tests.calls != 2 || last.GetError().GetRunnerFailed() == nil || last.GetRun() != 2 {
		t.Fatalf("ran %d times, frames %v; want the second run's runner_failed, framed as run 2, and no third run", h.Tests.calls, frames)
	}
}

func TestRunTestsHoldsTheUsersRunUnderCPULoad(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.events = []*testrunv1.TestRunEvent{endedPassed()}

	// Act
	frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), CpuLoad: &agentreplv1.RunTestsCpuLoad{Loops: 4}, Requester: user()})

	// Assert
	bin := filepath.Join(ref.GetDir(), "bin")
	want := "env AGENT_REPL_TEST_RUN_BY_DAEMON=1 bash " + filepath.Join(bin, "with-cpu-load.sh") + " 4 bash " + filepath.Join(bin, "test-all.sh") + " --selection "
	if got := strings.Join(h.Tests.argv, " "); !strings.HasPrefix(got, want) || frames[len(frames)-1].GetEvent().GetEnded().GetPassed() == nil {
		t.Fatalf("argv = %s, frames %v; want the run wrapped in the load script: %s...", got, frames, want)
	}
}

func TestRunTestsArgv(t *testing.T) {
	command := []string{"env", "M=1", "bash", "/w/bin/test-all.sh"}
	cases := []struct {
		name  string
		loops uint32
		want  []string
	}{
		{name: "no load", want: []string{"env", "M=1", "bash", "/w/bin/test-all.sh", "--selection", "/s.pb", "--events-fd", "3"}},
		{name: "under load", loops: 2, want: []string{"env", "M=1", "bash", "/w/bin/with-cpu-load.sh", "2", "bash", "/w/bin/test-all.sh", "--selection", "/s.pb", "--events-fd", "3"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := runTestsArgv(command, tc.loops, "/s.pb")

			// Assert
			if !slices.Equal(got, tc.want) || !slices.Equal(command, []string{"env", "M=1", "bash", "/w/bin/test-all.sh"}) {
				t.Fatalf("runTestsArgv = %q (command now %q), want %q and the command untouched", got, command, tc.want)
			}
		})
	}
}

// EVERY TEST RUN THE DAEMON STARTS CARRIES THE MARKER, through the one command
// TestCommandFor builds.
func TestRunTestsRunsTheMarkedTestCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.events = []*testrunv1.TestRunEvent{endedPassed()}

	// Act
	runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness(), Requester: agent()})

	// Assert
	if want := merge.TestCommandFor(ref.GetDir(), "bin/test-all.sh"); !slices.Equal(h.Tests.argv[:len(want)], want) {
		t.Fatalf("argv = %q, want it to begin with the daemon's test command %q", h.Tests.argv, want)
	}
}
