package server

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
)

func aReport() *testrunv1.RedundancyRequest {
	return &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{MeasurementFile: "/m.json"}}}
}

// runRedundancy calls RunRedundancy and answers every frame it streamed.
func runRedundancy(t *testing.T, h *harness, req *agentreplv1.RunRedundancyRequest) []*agentreplv1.RunRedundancyResponse {
	t.Helper()
	stream, err := h.Client.RunRedundancy(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatal(err)
	}
	defer stream.Close()
	var frames []*agentreplv1.RunRedundancyResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("stream: %v", err)
	}
	return frames
}

func TestRunRedundancyRelaysTheMeasurementsOutputAndEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.lines, h.Tests.exit = []string{"measuring", "wrote report.md"}, 1

	// Act
	frames := runRedundancy(t, h, &agentreplv1.RunRedundancyRequest{Workspace: ref, Redundancy: aReport()})

	// Assert
	if len(frames) != 3 || frames[0].GetOutputLine() != "measuring" || frames[1].GetOutputLine() != "wrote report.md" || frames[2].GetEnded().GetExitCode() != 1 {
		t.Fatalf("frames = %v, want both lines and the end with exit 1", frames)
	}
	if !proto.Equal(h.Tests.request, aReport()) || h.Tests.dir != ref.GetDir() {
		t.Fatalf("the runner ran in %s with %v, want the worktree and the request", h.Tests.dir, h.Tests.request)
	}
	want := merge.TestCommandFor(ref.GetDir(), "bin/test-all.sh")
	if !slices.Equal(h.Tests.argv[:len(want)], want) || h.Tests.argv[len(want)] != "--redundancy" {
		t.Fatalf("argv = %q, want the daemon's marked test command with --redundancy", h.Tests.argv)
	}
}

func TestRunRedundancyRefuses(t *testing.T) {
	cases := []struct {
		name  string
		req   func(h *harness) *agentreplv1.RunRedundancyRequest
		check func(*agentreplv1.RunRedundancyError) bool
	}{
		{name: "a request naming no command", req: func(h *harness) *agentreplv1.RunRedundancyRequest {
			return &agentreplv1.RunRedundancyRequest{Workspace: testsWorkspace(t, h, testsDeploy), Redundancy: &testrunv1.RedundancyRequest{}}
		}, check: func(e *agentreplv1.RunRedundancyError) bool { return e.GetNoCommand() != nil }},
		{name: "a workspace the daemon does not serve", req: func(h *harness) *agentreplv1.RunRedundancyRequest {
			return &agentreplv1.RunRedundancyRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope", Dir: "/nowhere"}, Redundancy: aReport()}
		}, check: func(e *agentreplv1.RunRedundancyError) bool { return e.GetUnknownWorkspace() != nil }},
		{name: "a worktree with no deploy config", req: func(h *harness) *agentreplv1.RunRedundancyRequest {
			return &agentreplv1.RunRedundancyRequest{Workspace: testsWorkspace(t, h, ""), Redundancy: aReport()}
		}, check: func(e *agentreplv1.RunRedundancyError) bool {
			return strings.Contains(e.GetRunnerFailed().GetDetail(), "deploy config")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			req := tc.req(h)

			// Act
			frames := runRedundancy(t, h, req)

			// Assert
			if len(frames) != 1 || !tc.check(frames[0].GetError()) {
				t.Fatalf("frames = %v, want the one refusal", frames)
			}
			if h.Tests.argv != nil {
				t.Fatalf("the runner ran %v for a refused request", h.Tests.argv)
			}
			if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRunRedundancy}); !found {
				t.Fatalf("records = %+v, want the refusal recorded", log.Records())
			}
		})
	}
}

func TestRunRedundancyAnswersAMeasurementThatCouldNotRun(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.err = errors.New("exec: no such file")

	// Act
	frames := runRedundancy(t, h, &agentreplv1.RunRedundancyRequest{Workspace: ref, Redundancy: aReport()})

	// Assert
	if len(frames) != 1 || !strings.Contains(frames[0].GetError().GetRunnerFailed().GetDetail(), "exec: no such file") {
		t.Fatalf("frames = %v, want runner_failed naming the cause", frames)
	}
	if _, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opRunRedundancy}); !found {
		t.Fatalf("records = %+v, want the failure recorded at error", log.Records())
	}
}
