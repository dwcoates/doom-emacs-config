package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
)

// runredundancy.go — RunRedundancy: a `testrun redundancy` measurement, run
// where every test run is, by the daemon.
//
// A MEASUREMENT IS A TEST RUN. It runs every case alone and under every
// mutant, so it goes through the workspace's own test script
// (`bin/test-all.sh --redundancy FILE`) exactly as RunTests does: the
// worktree's own runner, background priority, the git-state net, and the
// daemon's test-run marker. Its output is relayed line by line; its result is
// the tool's measurement.json and report.md under the request's out_dir.

const opRunRedundancy = "daemon.server.run_redundancy"

// RunRedundancy runs one measurement or report from one workspace's worktree
// and streams its output.
func (s *server) RunRedundancy(
	ctx context.Context,
	req *connect.Request[agentreplv1.RunRedundancyRequest],
	stream *connect.ServerStream[agentreplv1.RunRedundancyResponse],
) error {
	const rpc = "RunRedundancy"
	msg := req.Msg
	if msg.GetRedundancy().GetCommand() == nil {
		s.log.Info(opRunRedundancy, "refused a measurement that names no command", nil)
		return sendRedundancyError(stream, &agentreplv1.RunRedundancyError{Cause: &agentreplv1.RunRedundancyError_NoCommand{NoCommand: &agentreplv1.RunRedundancyNoCommand{}}})
	}
	w, err := s.testWorktreeOf(ctx, rpc, opRunRedundancy, msg.GetWorkspace())
	var noScript *noTestScriptError
	switch {
	case errors.Is(err, errUnknownTestWorkspace):
		return sendRedundancyError(stream, &agentreplv1.RunRedundancyError{Cause: &agentreplv1.RunRedundancyError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.RunRedundancyUnknownWorkspace{}}})
	case errors.As(err, &noScript):
		return sendRedundancyRunnerFailed(stream, noScript.Error())
	case err != nil:
		return err
	}
	log, dir, script := w.log, w.dir, w.script
	request, err := writeRequest(msg.GetRedundancy(), "agent-repl-run-redundancy-*.pb")
	if err != nil {
		log.Error(opRunRedundancy, "a measurement's request could not be handed to its runner", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRedundancyRunnerFailed(stream, err.Error())
	}
	defer os.Remove(request) //nolint:errcheck // a temp file the run is done with
	argv := append(merge.TestCommandFor(dir, script), "--redundancy", request)
	log.Info(opRunRedundancy, "running a workspace's measurement", dlog.Context{"dir": dir, "argv": strings.Join(argv, " ")})

	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	// THE LINES ARRIVE FROM THE RUNNER'S READERS; one send at a time, and a
	// caller gone stops the measurement.
	var mu sync.Mutex
	var sendErr error
	onLine := func(line string) {
		mu.Lock()
		defer mu.Unlock()
		if sendErr != nil {
			return
		}
		if sendErr = stream.Send(&agentreplv1.RunRedundancyResponse{Frame: &agentreplv1.RunRedundancyResponse_OutputLine{OutputLine: line}}); sendErr != nil {
			log.Info(opRunRedundancy, "a measurement's caller went away; the measurement is stopped", dlog.Context{"dir": dir, "cause": sendErr.Error()})
			cancel()
		}
	}
	result, err := s.deps.Tests.RunLinesWith(runCtx, dir, argv, scriptrunner.Options{}, onLine)
	mu.Lock()
	defer mu.Unlock()
	if sendErr != nil {
		return sendErr
	}
	if err != nil {
		log.Error(opRunRedundancy, "a measurement could not be run", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRedundancyRunnerFailed(stream, fmt.Sprintf("run %s: %v", strings.Join(argv, " "), err))
	}
	log.Info(opRunRedundancy, "a workspace's measurement ended", dlog.Context{"dir": dir, "exit_code": result.ExitCode, "wall_seconds": result.Wall.Seconds()})
	return stream.Send(&agentreplv1.RunRedundancyResponse{Frame: &agentreplv1.RunRedundancyResponse_Ended{Ended: &agentreplv1.RunRedundancyEnded{ExitCode: int32(result.ExitCode)}}})
}

func sendRedundancyError(stream *connect.ServerStream[agentreplv1.RunRedundancyResponse], e *agentreplv1.RunRedundancyError) error {
	return stream.Send(&agentreplv1.RunRedundancyResponse{Frame: &agentreplv1.RunRedundancyResponse_Error{Error: e}})
}

func sendRedundancyRunnerFailed(stream *connect.ServerStream[agentreplv1.RunRedundancyResponse], detail string) error {
	return sendRedundancyError(stream, &agentreplv1.RunRedundancyError{Cause: &agentreplv1.RunRedundancyError_RunnerFailed{
		RunnerFailed: &agentreplv1.RunRedundancyRunnerFailed{Detail: detail}}})
}
