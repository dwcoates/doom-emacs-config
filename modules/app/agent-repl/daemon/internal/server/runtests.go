package server

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protodelim"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

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
// the run reports, once or as many times as the request repeats it.
func (s *server) RunTests(
	ctx context.Context,
	req *connect.Request[agentreplv1.RunTestsRequest],
	stream *connect.ServerStream[agentreplv1.RunTestsResponse],
) error {
	const rpc = "RunTests"
	msg := req.Msg
	if len(msg.GetRun().GetSelections()) == 0 {
		s.log.Info(opRunTests, "refused a test run that names no test", nil)
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_NoSelection{NoSelection: &agentreplv1.RunTestsNoSelection{}}})
	}
	if detail := invalidRunTests(msg); detail != "" {
		s.log.Info(opRunTests, "refused a test run whose options contradict each other", dlog.Context{"workspace_id": msg.GetWorkspace().GetId(), "detail": detail})
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_Invalid{Invalid: &agentreplv1.RunTestsInvalid{Detail: detail}}})
	}
	if msg.GetCpuLoad() != nil && msg.GetRequester().GetUser() == nil {
		// LOAD IS THE USER'S ALONE (owner ruling, 2026-10-06).
		s.log.Info(opRunTests, "refused CPU load a workspace asked for", dlog.Context{"workspace_id": msg.GetWorkspace().GetId(),
			"requester": msg.GetRequester().GetWorkspace().GetWorkspace().GetId(), "loops": msg.GetCpuLoad().GetLoops()})
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_CpuLoadRefused{CpuLoadRefused: &agentreplv1.RunTestsCpuLoadRefused{}}})
	}
	w, err := s.testWorktreeOf(ctx, rpc, opRunTests, msg.GetWorkspace())
	var noScript *noTestScriptError
	switch {
	case errors.Is(err, errUnknownTestWorkspace):
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.RunTestsUnknownWorkspace{}}})
	case errors.As(err, &noScript):
		return sendRunnerFailed(stream, 1, -1, noScript.Error())
	case err != nil:
		return err
	}
	log, dir, script := w.log, w.dir, w.script
	selection, err := writeRequest(msg.GetRun(), "agent-repl-run-tests-*.pb")
	if err != nil {
		log.Error(opRunTests, "a test run's selection could not be handed to its runner", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRunnerFailed(stream, 1, -1, err.Error())
	}
	defer os.Remove(selection) //nolint:errcheck // a temp file the run is done with
	argv := runTestsArgv(merge.TestCommandFor(dir, script), msg.GetCpuLoad().GetLoops(), selection)
	runs := max(msg.GetRuns(), 1)
	for run := uint32(1); run <= runs; run++ {
		outcome, err := s.relayTestRun(ctx, log, stream, dir, argv, run)
		if err != nil || outcome != runPassed && (outcome != runFailed || msg.GetStopAtFirstFailure()) {
			// A send that failed, a run that never said how it ended or was
			// refused, or the first failure of a run told to stop there: no
			// later run follows.
			if run < runs {
				log.Info(opRunTests, "a repeated test run stopped before its last run", dlog.Context{"dir": dir, "run": run, "runs": runs, "outcome": outcome.String()})
			}
			return err
		}
	}
	return nil
}

// invalidRunTests answers why a request's options contradict each other, or
// "" when they do not.
func invalidRunTests(msg *agentreplv1.RunTestsRequest) string {
	switch {
	case msg.GetRequester().GetAsker() == nil:
		return "the request names no requester"
	case msg.GetRun().GetRecord() && msg.GetRuns() > 1:
		return "a recording run runs once: a repeat would append its timings again"
	case msg.GetCpuLoad() != nil && msg.GetCpuLoad().GetLoops() == 0:
		return "CPU load names no busy loop"
	}
	return ""
}

// runTestsArgv is a run's command line: the test command, held under CPU
// load when loops > 0 (bin/with-cpu-load.sh, beside the test script, wrapping
// the script's own bash), handed the selection and the events pipe.
func runTestsArgv(command []string, loops uint32, selection string) []string {
	argv := append([]string(nil), command...)
	if loops > 0 {
		// command is `env MARKER bash SCRIPT`: the load wraps `bash SCRIPT`.
		script := argv[len(argv)-1]
		load := filepath.Join(filepath.Dir(script), "with-cpu-load.sh")
		argv = append(append(argv[:len(argv)-2:len(argv)-2], "bash", load, fmt.Sprint(loops)), argv[len(argv)-2:]...)
	}
	return append(argv, "--selection", selection, "--events-fd", fmt.Sprint(runTestsEventsFD))
}

// runOutcome is how one run of a request ended.
type runOutcome int

const (
	// runUnended is a run that never said how it ended (runner_failed sent).
	runUnended runOutcome = iota
	runPassed
	runFailed
	runRefused
)

func (o runOutcome) String() string {
	return [...]string{"unended", "passed", "failed", "refused"}[o]
}

// relayTestRun runs argv in dir with an events pipe as its fd 3, sending every
// event on as it arrives, each framed as run RUN, and answers runner_failed
// when the run ends without saying how.
func (s *server) relayTestRun(ctx context.Context, log dlog.Logger, stream *connect.ServerStream[agentreplv1.RunTestsResponse], dir string, argv []string, run uint32) (runOutcome, error) {
	events, pipe, err := os.Pipe()
	if err != nil {
		log.Error(opRunTests, "a test run's events pipe could not be made", dlog.Context{"dir": dir, "run": run, "cause": err.Error()})
		return runUnended, sendRunnerFailed(stream, run, -1, fmt.Sprintf("make the events pipe: %v", err))
	}
	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	tail := &lineTail{max: runTestsTailLines}
	type ran struct {
		run scriptrunner.Run
		err error
	}
	done := make(chan ran, 1)
	log.Info(opRunTests, "running a workspace's selected tests", dlog.Context{"dir": dir, "run": run, "argv": strings.Join(argv, " ")})
	go func() {
		result, err := s.deps.Tests.RunLinesWith(runCtx, dir, argv, scriptrunner.Options{ExtraFiles: []*os.File{pipe}}, tail.add)
		// THE WRITE END CLOSES WITH THE RUN, so the read below ends once
		// every event the run wrote has been read.
		pipe.Close() //nolint:errcheck // the reader sees its end either way
		done <- ran{result, err}
	}()

	reader := bufio.NewReader(events)
	outcome, sendErr := runUnended, error(nil)
	for {
		ev := &testrunv1.TestRunEvent{}
		err := protodelim.UnmarshalFrom(reader, ev)
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			// A stream that cannot be read cannot be relayed: the run is
			// stopped, and its end is said as the runner's failure.
			log.Error(opRunTests, "a test run's events could not be read; the run is stopped", dlog.Context{"dir": dir, "run": run, "cause": err.Error()})
			cancel()
			_, _ = io.Copy(io.Discard, reader)
			break
		}
		switch {
		case ev.GetEnded().GetPassed() != nil:
			outcome = runPassed
		case ev.GetEnded() != nil:
			outcome = runFailed
		case ev.GetRefused() != nil:
			outcome = runRefused
		}
		if sendErr == nil {
			sendErr = stream.Send(&agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Event{Event: ev}, Run: run})
			if sendErr != nil {
				// The caller is gone: the run is stopped, and what it still
				// writes is drained so it is never held on a full pipe.
				log.Info(opRunTests, "a test run's caller went away; the run is stopped", dlog.Context{"dir": dir, "run": run, "cause": sendErr.Error()})
				cancel()
			}
		}
	}
	events.Close() //nolint:errcheck // read to its end
	result := <-done
	if sendErr != nil {
		return outcome, sendErr
	}
	switch {
	case result.err != nil:
		log.Error(opRunTests, "a test run could not be run", dlog.Context{"dir": dir, "run": run, "cause": result.err.Error()})
		return runUnended, sendRunnerFailed(stream, run, -1, fmt.Sprintf("run %s: %v", strings.Join(argv, " "), result.err))
	case outcome == runUnended:
		log.Error(opRunTests, "a test run ended without reporting its end", dlog.Context{"dir": dir, "run": run, "exit_code": result.run.ExitCode, "tail": tail.String()})
		return runUnended, sendRunnerFailed(stream, run, int32(result.run.ExitCode), fmt.Sprintf(
			"the runner exited %d without reporting how its run ended; its last output:\n%s", result.run.ExitCode, tail.String()))
	}
	log.Info(opRunTests, "a workspace's selected tests ran", dlog.Context{"dir": dir, "run": run, "outcome": outcome.String(), "exit_code": result.run.ExitCode, "wall_seconds": result.run.Wall.Seconds()})
	return outcome, nil
}

// errUnknownTestWorkspace is a test run's workspace this daemon does not
// serve (testWorktreeOf).
var errUnknownTestWorkspace = errors.New("no workspace this daemon serves is the one named")

// noTestScriptError is a test run's worktree with no test script to run.
type noTestScriptError struct{ cause error }

func (e *noTestScriptError) Error() string { return e.cause.Error() }
func (e *noTestScriptError) Unwrap() error { return e.cause }

// testWorktree is where a test run or a measurement runs: the workspace's
// worktree, its log, and the test script its deploy config names.
type testWorktree struct {
	log    dlog.Logger
	dir    string
	script string
}

// testWorktreeOf resolves ref to the worktree its test run runs in, recording
// each refusal under op: errUnknownTestWorkspace for a workspace this daemon
// does not serve, a *noTestScriptError for a worktree with no test script, or
// the resolution's own failure (already answered through failResolution).
func (s *server) testWorktreeOf(ctx context.Context, rpc, op string, ref *workspacev1.WorkspaceRef) (testWorktree, error) {
	subject, refused, err := s.resolveRef(ctx, rpc, ref)
	if err != nil {
		return testWorktree{}, failResolution(s.log, rpc, err)
	}
	if refused != nil {
		s.log.Info(op, "refused a test run for a workspace this daemon does not serve", dlog.Context{
			"workspace_id": ref.GetId(), "arm": refused.Arm, "reason": refused.Reason})
		return testWorktree{}, errUnknownTestWorkspace
	}
	w := testWorktree{log: subject.Log, dir: subject.Record.Dir}
	if w.script, err = testScriptOf(w.dir); err != nil {
		w.log.Error(op, "a test run has no test script to run", dlog.Context{"dir": w.dir, "cause": err.Error()})
		return testWorktree{}, &noTestScriptError{cause: err}
	}
	return w, nil
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

// writeRequest writes a request where its runner reads it, in a temp file
// named by pattern.
func writeRequest(m proto.Message, pattern string) (string, error) {
	data, err := proto.Marshal(m)
	if err != nil {
		return "", fmt.Errorf("encode the request: %w", err)
	}
	f, err := os.CreateTemp("", pattern)
	if err != nil {
		return "", fmt.Errorf("create the request file: %w", err)
	}
	_, werr := f.Write(data)
	cerr := f.Close()
	if err := errors.Join(werr, cerr); err != nil {
		os.Remove(f.Name()) //nolint:errcheck // the write already failed
		return "", fmt.Errorf("write the request file %s: %w", f.Name(), err)
	}
	return f.Name(), nil
}

func sendRunTestsError(stream *connect.ServerStream[agentreplv1.RunTestsResponse], run uint32, e *agentreplv1.RunTestsError) error {
	return stream.Send(&agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Error{Error: e}, Run: run})
}

func sendRunnerFailed(stream *connect.ServerStream[agentreplv1.RunTestsResponse], run uint32, exit int32, detail string) error {
	return sendRunTestsError(stream, run, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_RunnerFailed{
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
