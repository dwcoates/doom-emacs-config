package main

import (
	"bytes"
	"context"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
)

// redundancyDaemon streams FRAMES for any RunRedundancy, recording the
// request.
type redundancyDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	frames []*agentreplv1.RunRedundancyResponse
	got    *agentreplv1.RunRedundancyRequest
}

func (d *redundancyDaemon) RunRedundancy(_ context.Context, req *connect.Request[agentreplv1.RunRedundancyRequest], stream *connect.ServerStream[agentreplv1.RunRedundancyResponse]) error {
	d.got = req.Msg
	for _, f := range d.frames {
		if err := stream.Send(f); err != nil {
			return err
		}
	}
	return nil
}

// runRedundancyCLI runs `agent-repld test redundancy ARGS` against DAEMON
// from /wt/caller/src.
func runRedundancyCLI(t *testing.T, daemon *redundancyDaemon, args ...string) (int, string, string) {
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
	code := runNoun(context.Background(), testNoun, append([]string{"redundancy"}, args...), deps, &out, &errOut)
	return code, out.String(), errOut.String()
}

func endedWith(code int32) *agentreplv1.RunRedundancyResponse {
	return &agentreplv1.RunRedundancyResponse{Frame: &agentreplv1.RunRedundancyResponse_Ended{Ended: &agentreplv1.RunRedundancyEnded{ExitCode: code}}}
}

func TestTestRedundancyBuildsTheRequest(t *testing.T) {
	cases := []struct {
		name string
		args []string
		want *testrunv1.RedundancyRequest
	}{
		{name: "a package with only what is required", args: []string{"go", "-out", "/o", "-module", "../daemon", "-pkg", "./internal/x"},
			want: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: &testrunv1.RedundancyGo{
				Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/o"}, ModuleDir: "daemon",
				Subject: &testrunv1.RedundancyGo_Package{Package: &testrunv1.RedundancyGoPackage{Package: "./internal/x"}}}}}},
		{name: "a package with every setting", args: []string{"go", "-out", "/o", "-universe", "strict", "-max-coverage-drop", "0.5", "-baseline", "b.json",
			"-pin", "TestA,TestB", "-overhead", "250us", "-keep-scratch", "-max-load", "6", "-quiet-wait", "1.5s", "-procs", "2",
			"-pkg", ".", "-tests", "a_test.go", "-tags", "integration", "-programs", "git", "-mutate", "./x,./y", "-runs", "5", "-pin-calls", "", "-estimate", "-hang-bound", "11s"},
			want: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: &testrunv1.RedundancyGo{
				Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/o", Universe: testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_STRICT,
					MaxCoverageDrop: proto.Float64(0.5), Baseline: "src/b.json", Pins: []string{"TestA", "TestB"}, OverheadUs: proto.Int64(250),
					KeepScratch: true, MaxLoad: proto.Float64(6), QuietWaitMs: proto.Int64(1500), Procs: proto.Int32(2)},
				Subject: &testrunv1.RedundancyGo_Package{Package: &testrunv1.RedundancyGoPackage{Package: ".", TestFiles: []string{"a_test.go"}, Tags: "integration", Programs: []string{"git"}}},
				Mutate:  []string{"./x", "./y"}, Runs: proto.Int32(5), PinCalls: &testrunv1.RedundancyGoPinCalls{}, Estimate: true, HangBoundMs: proto.Int64(11000)}}}},
		{name: "roster suites", args: []string{"go", "-out", "/o", "-roster-suite", "daemon,e2e"},
			want: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: &testrunv1.RedundancyGo{
				Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/o"},
				Subject:     &testrunv1.RedundancyGo_RosterSuites{RosterSuites: &testrunv1.RedundancyGoRosterSuites{Names: []string{"daemon", "e2e"}}}}}}},
		{name: "a vitest measurement", args: []string{"vitest", "-out", "/o", "-package", "../webapp", "-mutate", "src/**/*.ts,!src/fake/**", "-runs", "2"},
			want: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Vitest{Vitest: &testrunv1.RedundancyVitest{
				Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/o"}, PackageDir: "webapp", Mutate: []string{"src/**/*.ts", "!src/fake/**"}, Runs: proto.Int32(2)}}}},
		{name: "a report", args: []string{"report", "-in", "/o/measurement.json", "-universe", "kills-only"},
			want: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{
				MeasurementFile: "/o/measurement.json", Universe: testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_KILLS_ONLY}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			daemon := &redundancyDaemon{frames: []*agentreplv1.RunRedundancyResponse{endedWith(0)}}

			// Act
			code, _, errOut := runRedundancyCLI(t, daemon, tc.args...)

			// Assert
			if code != exitSuccess || daemon.got.GetWorkspace().GetId() != "ws-caller" || !proto.Equal(daemon.got.GetRedundancy(), tc.want) {
				t.Fatalf("exit %d, stderr %q, request %v; want %v", code, errOut, daemon.got.GetRedundancy(), tc.want)
			}
		})
	}
}

func TestTestRedundancyReadsItsFiles(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	deleteSet := filepath.Join(dir, "delete.txt")
	suites := filepath.Join(dir, "suites.json")
	if err := os.WriteFile(deleteSet, []byte("# kept out\nTestA\n\nTestB/x\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(suites, []byte(`[{"Package":"./internal/x","Tags":"integration","Tests":["a_test.go"],"Programs":["git"],"Kept":true,"Module":"e2e"}]`), 0o644); err != nil {
		t.Fatal(err)
	}
	daemon := &redundancyDaemon{frames: []*agentreplv1.RunRedundancyResponse{endedWith(0)}}

	// Act
	code, _, errOut := runRedundancyCLI(t, daemon, "go", "-out", "/o", "-delete", deleteSet, "-suites", suites)

	// Assert
	g := daemon.got.GetRedundancy().GetGo()
	want := &testrunv1.RedundancyGoSuite{Package: "./internal/x", Tags: "integration", TestFiles: []string{"a_test.go"}, Programs: []string{"git"}, Kept: true, Module: "e2e"}
	if code != exitSuccess || strings.Join(g.GetMeasurement().GetDeleteCases(), ",") != "TestA,TestB/x" || len(g.GetSuites().GetSuites()) != 1 || !proto.Equal(g.GetSuites().GetSuites()[0], want) {
		t.Fatalf("exit %d, stderr %q, request %v", code, errOut, g)
	}
}

func TestTestRedundancyRefuses(t *testing.T) {
	empty := filepath.Join(t.TempDir(), "empty.txt")
	if err := os.WriteFile(empty, []byte("# nothing\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name string
		args []string
		want string
	}{
		{name: "no measurement named", args: nil, want: "name the measurement"},
		{name: "a flag where the measurement goes", args: []string{"-out", "/o"}, want: "name the measurement"},
		{name: "an unknown measurement", args: []string{"bogus"}, want: `unknown measurement "bogus"`},
		{name: "no -out", args: []string{"go", "-pkg", "."}, want: "-out is required"},
		{name: "two subjects", args: []string{"go", "-out", "/o", "-pkg", ".", "-roster-suite", "daemon"}, want: "name exactly one of -pkg, -roster-suite and -suites"},
		{name: "no subject", args: []string{"go", "-out", "/o"}, want: "name exactly one of -pkg, -roster-suite and -suites"},
		{name: "an unknown universe", args: []string{"go", "-out", "/o", "-pkg", ".", "-universe", "wide"}, want: `unknown -universe "wide"`},
		{name: "a module outside the worktree", args: []string{"go", "-out", "/o", "-pkg", ".", "-module", "/elsewhere"}, want: "outside the workspace's worktree"},
		{name: "a deletion set naming no case", args: []string{"go", "-out", "/o", "-pkg", ".", "-delete", empty}, want: "names no case"},
		{name: "an unreadable deletion set", args: []string{"go", "-out", "/o", "-pkg", ".", "-delete", "/no/such/file"}, want: "read -delete"},
		{name: "a vitest measurement mutating nothing", args: []string{"vitest", "-out", "/o", "-package", "."}, want: "-mutate is required"},
		{name: "a vitest measurement of no package", args: []string{"vitest", "-out", "/o", "-mutate", "x"}, want: "-package is required"},
		{name: "a report of nothing", args: []string{"report"}, want: "-in is required"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			daemon := &redundancyDaemon{}

			// Act
			code, _, errOut := runRedundancyCLI(t, daemon, tc.args...)

			// Assert
			if code != exitFailure || daemon.got != nil || !strings.Contains(errOut, tc.want) {
				t.Fatalf("exit %d, request %v, stderr %q; want a refusal containing %q and nothing sent", code, daemon.got, errOut, tc.want)
			}
		})
	}
}

func TestTestRedundancyPrintsTheOutputAndExitsByTheEnd(t *testing.T) {
	line := func(s string) *agentreplv1.RunRedundancyResponse {
		return &agentreplv1.RunRedundancyResponse{Frame: &agentreplv1.RunRedundancyResponse_OutputLine{OutputLine: s}}
	}
	cases := []struct {
		name    string
		frames  []*agentreplv1.RunRedundancyResponse
		want    int
		wantOut string
	}{
		{name: "a measurement that succeeded", frames: []*agentreplv1.RunRedundancyResponse{line("measuring"), endedWith(0)}, want: exitSuccess, wantOut: "measuring\n"},
		{name: "a measurement that failed", frames: []*agentreplv1.RunRedundancyResponse{line("boom"), endedWith(1)}, want: exitTestsFailed, wantOut: "boom\n"},
		{name: "a refusal", frames: []*agentreplv1.RunRedundancyResponse{{Frame: &agentreplv1.RunRedundancyResponse_Error{Error: &agentreplv1.RunRedundancyError{Cause: &agentreplv1.RunRedundancyError_NoCommand{NoCommand: &agentreplv1.RunRedundancyNoCommand{}}}}}}, want: exitRefused, wantOut: "noCommand"},
		{name: "a runner that could not start", frames: []*agentreplv1.RunRedundancyResponse{{Frame: &agentreplv1.RunRedundancyResponse_Error{Error: &agentreplv1.RunRedundancyError{Cause: &agentreplv1.RunRedundancyError_RunnerFailed{RunnerFailed: &agentreplv1.RunRedundancyRunnerFailed{Detail: "x"}}}}}}, want: exitFailure, wantOut: "runnerFailed"},
		{name: "a stream with no end", frames: []*agentreplv1.RunRedundancyResponse{line("cut")}, want: exitFailure, wantOut: "cut\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			daemon := &redundancyDaemon{frames: tc.frames}

			// Act
			code, out, errOut := runRedundancyCLI(t, daemon, "report", "-in", "/m.json")

			// Assert
			if code != tc.want || !strings.Contains(out, tc.wantOut) || (len(tc.frames) == 1 && tc.frames[0].GetOutputLine() != "" && !strings.Contains(errOut, "did not report how it ended")) {
				t.Fatalf("exit %d, stdout %q, stderr %q; want %d with %q", code, out, errOut, tc.want, tc.wantOut)
			}
		})
	}
}
