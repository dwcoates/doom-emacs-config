package main

import (
	"bytes"
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
)

// testRunsDaemon streams FRAMES for any RunTests, recording the request.
type testRunsDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	frames []*agentreplv1.RunTestsResponse
	got    *agentreplv1.RunTestsRequest
}

func (d *testRunsDaemon) RunTests(_ context.Context, req *connect.Request[agentreplv1.RunTestsRequest], stream *connect.ServerStream[agentreplv1.RunTestsResponse]) error {
	d.got = req.Msg
	for _, f := range d.frames {
		if err := stream.Send(f); err != nil {
			return err
		}
	}
	return nil
}

// runTestCLI runs `agent-repld test ARGS` against DAEMON from /wt/caller/src.
func runTestCLI(t *testing.T, daemon *testRunsDaemon, args ...string) (int, string, string) {
	t.Helper()
	_, handler := agentreplv1connect.NewAgentReplHandler(daemon)
	server := httptest.NewServer(handler)
	t.Cleanup(server.Close)
	var out, errOut bytes.Buffer
	deps := cliDeps{
		address: func(string) (string, error) { return server.URL, nil },
		client: func(string, ...connect.ClientOption) agentreplv1connect.AgentReplClient {
			return agentreplv1connect.NewAgentReplClient(http.DefaultClient, server.URL)
		},
		roster: func(context.Context, agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error) {
			return roster(cliRow("ws-caller", "/wt/caller", "caller")), nil
		},
		getwd:   func() (string, error) { return "/wt/caller/src", nil },
		mintKey: func(prefix string) (string, error) { return prefix + "-1", nil },
	}
	code := runNoun(context.Background(), testNoun, args, deps, &out, &errOut)
	return code, out.String(), errOut.String()
}

func eventFrame(ev *testrunv1.TestRunEvent) *agentreplv1.RunTestsResponse {
	return &agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Event{Event: ev}}
}

func endedFrame(passed bool) *agentreplv1.RunTestsResponse {
	ended := &testrunv1.TestRunEnded{Outcome: &testrunv1.TestRunEnded_Passed{Passed: &testrunv1.TestRunEndedPassed{}}}
	if !passed {
		ended.Outcome = &testrunv1.TestRunEnded_Failed{Failed: &testrunv1.TestRunEndedFailed{FailedUnits: 1}}
	}
	return eventFrame(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: ended}})
}

func TestTestRunExitsByHowTheRunEnded(t *testing.T) {
	cases := []struct {
		name   string
		frames []*agentreplv1.RunTestsResponse
		want   int
	}{
		{name: "a passing run", frames: []*agentreplv1.RunTestsResponse{endedFrame(true)}, want: exitSuccess},
		{name: "a failing run", frames: []*agentreplv1.RunTestsResponse{endedFrame(false)}, want: exitTestsFailed},
		{name: "a refused selection", frames: []*agentreplv1.RunTestsResponse{eventFrame(&testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Refused{Refused: &testrunv1.TestRunRefused{Detail: "no"}}})}, want: exitRefused},
		{name: "a daemon error", frames: []*agentreplv1.RunTestsResponse{{Frame: &agentreplv1.RunTestsResponse_Error{Error: &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_NoSelection{NoSelection: &agentreplv1.RunTestsNoSelection{}}}}}}, want: exitRefused},
		{name: "a stream that ends without its end", frames: nil, want: exitFailure},
		{name: "a repeat whose first run failed and last passed", frames: []*agentreplv1.RunTestsResponse{endedFrame(false), endedFrame(true)}, want: exitTestsFailed},
		{name: "a run the runner never ended", frames: []*agentreplv1.RunTestsResponse{{Frame: &agentreplv1.RunTestsResponse_Error{Error: &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_RunnerFailed{RunnerFailed: &agentreplv1.RunTestsRunnerFailed{ExitCode: 2}}}}}}, want: exitFailure},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			daemon := &testRunsDaemon{frames: tc.frames}

			// Act
			code, out, errOut := runTestCLI(t, daemon, "run", "-go", "../daemon/internal/merge")

			// Assert
			if code != tc.want {
				t.Fatalf("exit = %d, want %d; stdout %q stderr %q", code, tc.want, out, errOut)
			}
			if lines := strings.Count(out, "\n"); lines != len(tc.frames) {
				t.Fatalf("printed %d lines, want one per frame:\n%s", lines, out)
			}
		})
	}
}

func TestTestRunSendsTheCallingWorkspaceAndItsSelection(t *testing.T) {
	// Arrange
	daemon := &testRunsDaemon{frames: []*agentreplv1.RunTestsResponse{endedFrame(true)}}

	// Act
	code, _, errOut := runTestCLI(t, daemon, "run", "-go", "pkg=TestA")

	// Assert
	if code != exitSuccess || daemon.got.GetWorkspace().GetId() != "ws-caller" {
		t.Fatalf("exit %d, request %v, stderr %q; want the caller's workspace", code, daemon.got, errOut)
	}
	if got := daemon.got.GetRun().GetSelections()[0].GetGoPackage().GetPackageDir(); got != "src/pkg" {
		t.Fatalf("package dir = %q, want src/pkg: relative to the worktree", got)
	}
}

func TestTestRunRefusesARunNamingNoTest(t *testing.T) {
	// Act
	code, _, errOut := runTestCLI(t, &testRunsDaemon{}, "run")

	// Assert
	if code != exitFailure || !strings.Contains(errOut, "the full suite is the merge queue's to run") {
		t.Fatalf("exit %d, stderr %q; want the usage refusal", code, errOut)
	}
}

func TestBuildSelections(t *testing.T) {
	goAll := func(dir string) *testrunv1.TestSelection {
		return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{PackageDir: dir,
			Tests: &testrunv1.TestSelectionGoPackage_All{All: &testrunv1.TestSelectionGoAllTests{}}}}}
	}
	cases := []struct {
		name string
		sel  testSelections
		want []*testrunv1.TestSelection
	}{
		{name: "a package relative to the working directory", sel: testSelections{goSel: []string{"../daemon/internal/merge"}}, want: []*testrunv1.TestSelection{goAll("daemon/internal/merge")}},
		{name: "an absolute package path", sel: testSelections{goSel: []string{"/wt/caller/x/y"}}, want: []*testrunv1.TestSelection{goAll("x/y")}},
		{name: "named tests and subtest paths of one test", sel: testSelections{goSel: []string{"p=TestA,TestB/case/inner,TestB/other"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
			PackageDir: "src/p", Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: []*testrunv1.TestSelectionGoTest{
				{Name: "TestA", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}},
				{Name: "TestB", Scope: &testrunv1.TestSelectionGoTest_Subtests{Subtests: &testrunv1.TestSelectionGoSubtests{Paths: []*testrunv1.TestSelectionGoSubtestPath{
					{Segments: []string{"case", "inner"}}, {Segments: []string{"other"}}}}}},
			}}}}}}}},
		{name: "a whole test and one of its subtests run it whole", sel: testSelections{goSel: []string{"p=TestB/x,TestB"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
			PackageDir: "src/p", Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: []*testrunv1.TestSelectionGoTest{
				{Name: "TestB", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}}}}}}}}}},
		{name: "a vitest file's names gather into one selection", sel: testSelections{vitest: []string{"a.test.ts=one, with a comma", "a.test.ts=two"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "src/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: []string{"one, with a comma", "two"}}}}}}}},
		{name: "a whole vitest file outranks its names", sel: testSelections{vitest: []string{"a.test.ts=one", "a.test.ts"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "src/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_All{All: &testrunv1.TestSelectionVitestAllTests{}}}}}}},
		{name: "named ERT tests", sel: testSelections{ert: []string{"test-x.el=a-test,b-test"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_ErtFile{ErtFile: &testrunv1.TestSelectionErtFile{
			File: "src/test-x.el", Tests: &testrunv1.TestSelectionErtFile_Named{Named: &testrunv1.TestSelectionErtNamedTests{TestNames: []string{"a-test", "b-test"}}}}}}}},
		{name: "whole suites by name", sel: testSelections{suites: []string{"daemon", "ert"}}, want: []*testrunv1.TestSelection{
			{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: "daemon"}}},
			{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: "ert"}}}}},
		{name: "every suite", sel: testSelections{every: true}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}}},
		{name: "named harness groups", sel: testSelections{harness: []string{"t.sh=g1,g2"}}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_Harness{Harness: &testrunv1.TestSelectionHarness{
			Script: "src/t.sh", Groups: &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: []string{"g1", "g2"}}}}}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := buildSelections("/wt/caller", "/wt/caller/src", tc.sel)

			// Assert
			if err != nil || !proto.Equal(got, &testrunv1.TestRunRequest{Selections: tc.want}) {
				t.Fatalf("buildSelections = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}

func TestBuildSelectionsRefuses(t *testing.T) {
	cases := []struct {
		name string
		sel  testSelections
		want string
	}{
		{name: "a path outside the worktree", sel: testSelections{goSel: []string{"../../elsewhere"}}, want: "outside the workspace's worktree"},
		{name: "an empty Go test name", sel: testSelections{goSel: []string{"p=TestA,,TestB"}}, want: "an empty name in the list"},
		{name: "an empty vitest name", sel: testSelections{vitest: []string{"a.test.ts= "}}, want: "an empty test name"},
		{name: "an empty ERT name", sel: testSelections{ert: []string{"x.el=a,"}}, want: "an empty name in the list"},
		{name: "an empty harness group", sel: testSelections{harness: []string{"t.sh=,g"}}, want: "an empty name in the list"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := buildSelections("/wt/caller", "/wt/caller/src", tc.sel)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("buildSelections = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

func TestTestRunSendsItsOptions(t *testing.T) {
	cases := []struct {
		name  string
		args  []string
		check func(*agentreplv1.RunTestsRequest) bool
	}{
		{name: "an agent's run names its workspace as the requester", args: []string{"-go", "pkg"},
			check: func(r *agentreplv1.RunTestsRequest) bool {
				return r.GetRequester().GetWorkspace().GetWorkspace().GetId() == "ws-caller" && r.GetCpuLoad() == nil
			}},
		{name: "coverage of a suite", args: []string{"-suite", "webapp", "-coverage"},
			check: func(r *agentreplv1.RunTestsRequest) bool {
				return r.GetRun().GetCoverage() && r.GetRun().GetSelections()[0].GetSuite().GetName() == "webapp"
			}},
		{name: "a record of every suite", args: []string{"-every-suite", "-record"},
			check: func(r *agentreplv1.RunTestsRequest) bool {
				return r.GetRun().GetRecord() && r.GetRun().GetSelections()[0].GetEverySuite() != nil
			}},
		{name: "a repeat stopping at the first failure", args: []string{"-go", "pkg=TestA", "-runs", "9", "-stop-at-first-failure"},
			check: func(r *agentreplv1.RunTestsRequest) bool { return r.GetRuns() == 9 && r.GetStopAtFirstFailure() }},
		{name: "the user's run under load", args: []string{"-go", "pkg", "-cpu-load", "8", "-as-user"},
			check: func(r *agentreplv1.RunTestsRequest) bool {
				return r.GetCpuLoad().GetLoops() == 8 && r.GetRequester().GetUser() != nil
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			daemon := &testRunsDaemon{frames: []*agentreplv1.RunTestsResponse{endedFrame(true)}}

			// Act
			code, _, errOut := runTestCLI(t, daemon, append([]string{"run"}, tc.args...)...)

			// Assert
			if code != exitSuccess || !tc.check(daemon.got) {
				t.Fatalf("exit %d, request %v, stderr %q", code, daemon.got, errOut)
			}
		})
	}
}

func TestTestRunRefusesLoadTheUserDidNotAskFor(t *testing.T) {
	// Arrange
	daemon := &testRunsDaemon{}

	// Act
	code, _, errOut := runTestCLI(t, daemon, "run", "-go", "pkg", "-cpu-load", "8")

	// Assert
	if code != exitFailure || daemon.got != nil || !strings.Contains(errOut, "-cpu-load is the user's alone") {
		t.Fatalf("exit %d, request %v, stderr %q; want the refusal and nothing sent", code, daemon.got, errOut)
	}
}
