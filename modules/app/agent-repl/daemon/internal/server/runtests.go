package server

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protodelim"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
)

// runtests.go — RunTests: the daemon is the one runner of tests on the host.
//
// AN AGENT'S RUN IS THE WORKSPACE'S OWN. The daemon runs the test script the
// workspace's own deploy config names (the agent_repl_suites step, the same
// script the merge queue gates on) from the workspace's worktree, handing it
// the selection as a file and an events pipe as fd 3. The script builds and
// runs the worktree's own runner (testrun), at background priority and inside
// the host's one test-CPU budget, exactly as a merge's run does.
//
// THE STREAM IS RELAYED, TYPED. The runner writes testrun.v1.TestRunEvent
// messages to the pipe as it plans, as each unit ends and as it ends; each is
// sent on as it arrives. Nothing is parsed out of the run's text: the text is
// the run's log, which the runner keeps and names in its first event.
//
// A RUN THAT DOES NOT SAY HOW IT ENDED ENDS IN AN ERROR. A runner that dies,
// or a script that fails before the runner starts, leaves the stream without
// its `ended` (or `refused`); the daemon then answers runner_failed with the
// exit status and the script's last lines, never a silent close.

const opRunTests = "daemon.server.run_tests"

// runTestsEventsFD is where the script holds the events pipe: its first file
// after the standard streams.
const runTestsEventsFD = 3

// runTestsTailLines bounds how much of a failed script's output a
// runner_failed carries.
const runTestsTailLines = 40

// TestRunner runs a test script, streaming its output lines (scriptrunner).
type TestRunner interface {
	RunLinesWith(ctx context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error)
}

// RunTests runs the named tests from one workspace's worktree and streams what
// the run reports.
func (s *server) RunTests(
	ctx context.Context,
	req *connect.Request[agentreplv1.RunTestsRequest],
	stream *connect.ServerStream[agentreplv1.RunTestsResponse],
) error {
	const rpc = "RunTests"
	if len(req.Msg.GetRun().GetSelections()) == 0 {
		s.log.Info(opRunTests, "refused a test run that names no test", nil)
		return sendRunTestsError(stream, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_NoSelection{NoSelection: &agentreplv1.RunTestsNoSelection{}}})
	}
	subject, refused, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return failResolution(s.log, rpc, err)
	}
	if refused != nil {
		s.log.Info(opRunTests, "refused a test run for a workspace this daemon does not serve", dlog.Context{
			"workspace_id": req.Msg.GetWorkspace().GetId(), "arm": refused.Arm, "reason": refused.Reason})
		return sendRunTestsError(stream, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.RunTestsUnknownWorkspace{}}})
	}
	log, dir := subject.Log, subject.Record.Dir
	script, err := testScriptOf(dir)
	if err != nil {
		log.Error(opRunTests, "a test run has no test script to run", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRunnerFailed(stream, -1, err.Error())
	}
	selection, err := writeSelection(req.Msg.GetRun())
	if err != nil {
		log.Error(opRunTests, "a test run's selection could not be handed to its runner", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRunnerFailed(stream, -1, err.Error())
	}
	defer os.Remove(selection) //nolint:errcheck // a temp file the run is done with
	return s.relayTestRun(ctx, log, stream, dir, append(merge.TestCommandFor(dir, script),
		"--selection", selection, "--events-fd", fmt.Sprint(runTestsEventsFD)))
}

// relayTestRun runs argv in dir with an events pipe as its fd 3, sending every
// event on as it arrives, and answers runner_failed when the run ends without
// saying how.
func (s *server) relayTestRun(ctx context.Context, log dlog.Logger, stream *connect.ServerStream[agentreplv1.RunTestsResponse], dir string, argv []string) error {
	events, pipe, err := os.Pipe()
	if err != nil {
		log.Error(opRunTests, "a test run's events pipe could not be made", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRunnerFailed(stream, -1, fmt.Sprintf("make the events pipe: %v", err))
	}
	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	tail := &lineTail{max: runTestsTailLines}
	type ran struct {
		run scriptrunner.Run
		err error
	}
	done := make(chan ran, 1)
	log.Info(opRunTests, "running a workspace's selected tests", dlog.Context{"dir": dir, "argv": strings.Join(argv, " ")})
	go func() {
		run, err := s.deps.Tests.RunLinesWith(runCtx, dir, argv, scriptrunner.Options{ExtraFiles: []*os.File{pipe}}, tail.add)
		// THE WRITE END CLOSES WITH THE RUN, so the read below ends once
		// every event the run wrote has been read.
		pipe.Close() //nolint:errcheck // the reader sees its end either way
		done <- ran{run, err}
	}()

	reader := bufio.NewReader(events)
	ended, sendErr := false, error(nil)
	for {
		ev := &testrunv1.TestRunEvent{}
		err := protodelim.UnmarshalFrom(reader, ev)
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			// A stream that cannot be read cannot be relayed: the run is
			// stopped, and its end is said as the runner's failure.
			log.Error(opRunTests, "a test run's events could not be read; the run is stopped", dlog.Context{"dir": dir, "cause": err.Error()})
			cancel()
			_, _ = io.Copy(io.Discard, reader)
			break
		}
		if ev.GetEnded() != nil || ev.GetRefused() != nil {
			ended = true
		}
		if sendErr == nil {
			sendErr = stream.Send(&agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Event{Event: ev}})
			if sendErr != nil {
				// The caller is gone: the run is stopped, and what it still
				// writes is drained so it is never held on a full pipe.
				log.Info(opRunTests, "a test run's caller went away; the run is stopped", dlog.Context{"dir": dir, "cause": sendErr.Error()})
				cancel()
			}
		}
	}
	events.Close() //nolint:errcheck // read to its end
	result := <-done
	if sendErr != nil {
		return sendErr
	}
	switch {
	case result.err != nil:
		log.Error(opRunTests, "a test run could not be run", dlog.Context{"dir": dir, "cause": result.err.Error()})
		return sendRunnerFailed(stream, -1, fmt.Sprintf("run %s: %v", strings.Join(argv, " "), result.err))
	case !ended:
		log.Error(opRunTests, "a test run ended without reporting its end", dlog.Context{"dir": dir, "exit_code": result.run.ExitCode, "tail": tail.String()})
		return sendRunnerFailed(stream, int32(result.run.ExitCode), fmt.Sprintf(
			"the runner exited %d without reporting how its run ended; its last output:\n%s", result.run.ExitCode, tail.String()))
	}
	log.Info(opRunTests, "a workspace's selected tests ran", dlog.Context{"dir": dir, "exit_code": result.run.ExitCode, "wall_seconds": result.run.Wall.Seconds()})
	return nil
}

// testScriptOf is the test script the worktree's own deploy config names.
func testScriptOf(dir string) (string, error) {
	cfg, err := deployconfig.OnDisk{}.Load(dir)
	if err != nil {
		return "", fmt.Errorf("read the worktree's deploy config: %w", err)
	}
	script := deployconfig.TestScript(cfg)
	if script == "" {
		return "", fmt.Errorf("the worktree's deploy config %s names no agent_repl_suites test script, so this repository's tests are not the daemon's to run", deployconfig.Path(dir))
	}
	return script, nil
}

// writeSelection writes the run's selection where its runner reads it.
func writeSelection(run *testrunv1.TestRunRequest) (string, error) {
	data, err := proto.Marshal(run)
	if err != nil {
		return "", fmt.Errorf("encode the selection: %w", err)
	}
	f, err := os.CreateTemp("", "agent-repl-run-tests-*.pb")
	if err != nil {
		return "", fmt.Errorf("create the selection file: %w", err)
	}
	_, werr := f.Write(data)
	cerr := f.Close()
	if err := errors.Join(werr, cerr); err != nil {
		os.Remove(f.Name()) //nolint:errcheck // the write already failed
		return "", fmt.Errorf("write the selection file %s: %w", f.Name(), err)
	}
	return f.Name(), nil
}

func sendRunTestsError(stream *connect.ServerStream[agentreplv1.RunTestsResponse], e *agentreplv1.RunTestsError) error {
	return stream.Send(&agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Error{Error: e}})
}

func sendRunnerFailed(stream *connect.ServerStream[agentreplv1.RunTestsResponse], exit int32, detail string) error {
	return sendRunTestsError(stream, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_RunnerFailed{
		RunnerFailed: &agentreplv1.RunTestsRunnerFailed{ExitCode: exit, Detail: detail}}})
}

// lineTail keeps the last lines of a script's output.
type lineTail struct {
	mu    sync.Mutex
	max   int
	lines []string
}

func (t *lineTail) add(line string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.lines = append(t.lines, line)
	if len(t.lines) > t.max {
		t.lines = t.lines[len(t.lines)-t.max:]
	}
}

func (t *lineTail) String() string {
	t.mu.Lock()
	defer t.mu.Unlock()
	return strings.Join(t.lines, "\n")
}
