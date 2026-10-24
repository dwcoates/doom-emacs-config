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
		name                        string
		goSel, vitest, ert, harness []string
		want                        []*testrunv1.TestSelection
	}{
		{name: "a package relative to the working directory", goSel: []string{"../daemon/internal/merge"}, want: []*testrunv1.TestSelection{goAll("daemon/internal/merge")}},
		{name: "an absolute package path", goSel: []string{"/wt/caller/x/y"}, want: []*testrunv1.TestSelection{goAll("x/y")}},
		{name: "named tests and subtest paths of one test", goSel: []string{"p=TestA,TestB/case/inner,TestB/other"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
			PackageDir: "src/p", Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: []*testrunv1.TestSelectionGoTest{
				{Name: "TestA", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}},
				{Name: "TestB", Scope: &testrunv1.TestSelectionGoTest_Subtests{Subtests: &testrunv1.TestSelectionGoSubtests{Paths: []*testrunv1.TestSelectionGoSubtestPath{
					{Segments: []string{"case", "inner"}}, {Segments: []string{"other"}}}}}},
			}}}}}}}},
		{name: "a whole test and one of its subtests run it whole", goSel: []string{"p=TestB/x,TestB"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
			PackageDir: "src/p", Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: []*testrunv1.TestSelectionGoTest{
				{Name: "TestB", Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}}}}}}}}}},
		{name: "a vitest file's names gather into one selection", vitest: []string{"a.test.ts=one, with a comma", "a.test.ts=two"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "src/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: []string{"one, with a comma", "two"}}}}}}}},
		{name: "a whole vitest file outranks its names", vitest: []string{"a.test.ts=one", "a.test.ts"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
			File: "src/a.test.ts", Tests: &testrunv1.TestSelectionVitestFile_All{All: &testrunv1.TestSelectionVitestAllTests{}}}}}}},
		{name: "named ERT tests", ert: []string{"test-x.el=a-test,b-test"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_ErtFile{ErtFile: &testrunv1.TestSelectionErtFile{
			File: "src/test-x.el", Tests: &testrunv1.TestSelectionErtFile_Named{Named: &testrunv1.TestSelectionErtNamedTests{TestNames: []string{"a-test", "b-test"}}}}}}}},
		{name: "named harness groups", harness: []string{"t.sh=g1,g2"}, want: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_Harness{Harness: &testrunv1.TestSelectionHarness{
			Script: "src/t.sh", Groups: &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: []string{"g1", "g2"}}}}}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := buildSelections("/wt/caller", "/wt/caller/src", tc.goSel, tc.vitest, tc.ert, tc.harness)

			// Assert
			if err != nil || !proto.Equal(got, &testrunv1.TestRunRequest{Selections: tc.want}) {
				t.Fatalf("buildSelections = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}

func TestBuildSelectionsRefuses(t *testing.T) {
	cases := []struct {
		name                        string
		goSel, vitest, ert, harness []string
		want                        string
	}{
		{name: "a path outside the worktree", goSel: []string{"../../elsewhere"}, want: "outside the workspace's worktree"},
		{name: "an empty Go test name", goSel: []string{"p=TestA,,TestB"}, want: "an empty name in the list"},
		{name: "an empty vitest name", vitest: []string{"a.test.ts= "}, want: "an empty test name"},
		{name: "an empty ERT name", ert: []string{"x.el=a,"}, want: "an empty name in the list"},
		{name: "an empty harness group", harness: []string{"t.sh=,g"}, want: "an empty name in the list"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := buildSelections("/wt/caller", "/wt/caller/src", tc.goSel, tc.vitest, tc.ert, tc.harness)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("buildSelections = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}
