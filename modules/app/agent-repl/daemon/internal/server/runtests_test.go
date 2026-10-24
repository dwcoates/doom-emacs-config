package server

import (
	"context"
	"errors"
	"os"
	"path/filepath"
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
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/wsm"
)

// fakeTests is the test script: it writes its scripted events to the events
// pipe it is handed, prints its lines, and answers its exit.
type fakeTests struct {
	mu       sync.Mutex
	argv     []string
	dir      string
	selected *testrunv1.TestRunRequest
	events   []*testrunv1.TestRunEvent
	garbage  bool
	lines    []string
	exit     int
	err      error
}

func (f *fakeTests) RunLinesWith(_ context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.argv, f.dir = argv, dir
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
	}
	if f.err != nil {
		return scriptrunner.Run{}, f.err
	}
	pipe := opts.ExtraFiles[0]
	for _, ev := range f.events {
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
	frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness()})

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
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, testsDeploy), Run: &testrunv1.TestRunRequest{}}
		}, check: func(e *agentreplv1.RunTestsError) bool { return e.GetNoSelection() != nil }},
		{name: "a workspace the daemon does not serve", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope", Dir: "/nowhere"}, Run: oneHarness()}
		}, check: func(e *agentreplv1.RunTestsError) bool { return e.GetUnknownWorkspace() != nil }},
		{name: "a worktree with no deploy config", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, ""), Run: oneHarness()}
		}, check: func(e *agentreplv1.RunTestsError) bool {
			return e.GetRunnerFailed().GetExitCode() == -1 && strings.Contains(e.GetRunnerFailed().GetDetail(), "deploy config")
		}},
		{name: "a deploy config naming no test script", req: func(h *harness) *agentreplv1.RunTestsRequest {
			return &agentreplv1.RunTestsRequest{Workspace: testsWorkspace(t, h, "pre_deploy: {}\ndeploy:\n  hook:\n    script:\n      path: bin/deploy.sh\n      timeout_seconds: 60\n  runs_for:\n    main: {}\n"), Run: oneHarness()}
		}, check: func(e *agentreplv1.RunTestsError) bool {
			return strings.Contains(e.GetRunnerFailed().GetDetail(), "names no agent_repl_suites test script")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			req := tc.req(h)

			// Act
			frames := runTests(t, h, req)

			// Assert
			if len(frames) != 1 || !tc.check(frames[0].GetError()) {
				t.Fatalf("frames = %v, want the one refusal", frames)
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
			frames := runTests(t, h, &agentreplv1.RunTestsRequest{Workspace: ref, Run: oneHarness()})

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
