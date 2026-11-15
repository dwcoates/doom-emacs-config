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

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitlayout"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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
	if refused := userOnlyOptions(msg); len(refused) > 0 && msg.GetRequester().GetUser() == nil {
		// MEASURING THE TESTS IS THE USER'S ALONE (owner ruling, 2026-11-13):
		// a fresh or ungoverned run is at the user's explicit request, never
		// an agent's ordinary run.
		s.log.Info(opRunTests, "refused an option only the user may ask for", dlog.Context{"workspace_id": msg.GetWorkspace().GetId(),
			"requester": msg.GetRequester().GetWorkspace().GetWorkspace().GetId(), "options": refused})
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_UserOnlyRefused{UserOnlyRefused: &agentreplv1.RunTestsUserOnlyRefused{Options: refused}}})
	}
	w, err := s.testWorktreeOf(ctx, rpc, opRunTests, msg.GetWorkspace())
	var noScript *noTestScriptError
	var transferred *transferredTestWorkspace
	switch {
	case errors.As(err, &transferred):
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_TransferringAway{TransferringAway: &agentreplv1.RunTestsTransferringAway{Address: transferred.address}}})
	case errors.Is(err, errUnknownTestWorkspace):
		return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.RunTestsUnknownWorkspace{}}})
	case errors.As(err, &noScript):
		return sendRunnerFailed(stream, 1, -1, noScript.Error())
	case err != nil:
		return err
	}
	if named := msg.GetWorktree(); named != nil {
		// A MAIN WORKSPACE'S HAND-CUT WORKTREE runs in place of its own.
		cut, detail, err := s.handCutWorktreeOf(ctx, w, named.GetDir())
		if err != nil {
			return err
		}
		if detail != "" {
			w.log.Info(opRunTests, "refused a test run in a worktree its workspace may not run tests in", dlog.Context{"dir": w.dir, "worktree": named.GetDir(), "detail": detail})
			return sendRunTestsError(stream, 1, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_WorktreeRefused{WorktreeRefused: &agentreplv1.RunTestsWorktreeRefused{Detail: detail}}})
		}
		if errors.As(cut.err, &noScript) {
			return sendRunnerFailed(stream, 1, -1, noScript.Error())
		}
		w = cut.worktree
	}
	log, dir, script := w.log, w.dir, w.script
	selection, err := writeRequest(msg.GetRun(), "agent-repl-run-tests-*.pb")
	if err != nil {
		log.Error(opRunTests, "a test run's selection could not be handed to its runner", dlog.Context{"dir": dir, "cause": err.Error()})
		return sendRunnerFailed(stream, 1, -1, err.Error())
	}
	defer os.Remove(selection) //nolint:errcheck // a temp file the run is done with
	argv := runTestsArgv(merge.TestCommandFor(dir, script, w.name), msg.GetCpuLoad().GetLoops(), selection)
	// EVERY RUN IS GOVERNED unless the user asks for an ungoverned one
	// (owner ruling, 2026-11-13, replacing the 2026-11-12 ruling that a queue
	// bypass holder tests ungoverned: its full run starved the machine).
	if msg.GetUngoverned() {
		argv = merge.UngovernedTestArgv(argv)
		log.Info(opRunTests, "the user asked for an ungoverned run; it takes every core at the test runs' best priority", dlog.Context{"dir": dir, "workspace": string(w.id)})
	}
	// THE DAEMON'S EXIT STOPS THE RUN AND HANDS IT ON. A run is this daemon's
	// child, so a deploy's handover cannot carry it; until 2026-11-14 the exit
	// cut every running RunTests stream at its deadline ("the exit's deadline
	// passed with standing streams still open ... RunTests=2"), the caller
	// read "the run's stream broke: incomplete envelope", and an agent's full
	// run before a landing was lost to any deploy. The exit's end of the
	// surface (Close) now stops the run, and its stream ends with
	// transferring_away naming the successor, which the caller sends the run
	// to (streamTestRun), or runner_failed when no successor serves.
	runCtx, stopRun := context.WithCancel(ctx)
	defer stopRun()
	defer context.AfterFunc(s.life, stopRun)()
	runs := max(msg.GetRuns(), 1)
	for run := uint32(1); run <= runs; run++ {
		outcome, err := s.relayTestRun(runCtx, log, stream, dir, argv, run)
		if outcome == runCutByExit {
			return s.answerRunCutByExit(log, stream, dir, run)
		}
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
		// command is `env MARKER... bash SCRIPT`: the load wraps `bash SCRIPT`.
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
	// runAborted is a run the runner stopped before it planned, on its own
	// failure (testrun.v1.TestRunAborted).
	runAborted
	// runCutByExit is a run this daemon stopped before its end because it is
	// exiting (answerRunCutByExit); nothing about its end was sent.
	runCutByExit
)

func (o runOutcome) String() string {
	return [...]string{"unended", "passed", "failed", "refused", "aborted", "cut_by_exit"}[o]
}

// answerRunCutByExit ends the stream of run RUN, which this daemon's exit
// stopped: transferring_away naming the successor the caller sends the run
// to, or runner_failed when no successor serves (a plain stop).
func (s *server) answerRunCutByExit(log dlog.Logger, stream *connect.ServerStream[agentreplv1.RunTestsResponse], dir string, run uint32) error {
	if successor := s.deps.SuccessorAddress(); successor != "" {
		log.Info(opRunTests, "this daemon is exiting for a handover; it stopped a test run and hands it to its successor", dlog.Context{"dir": dir, "run": run, "successor": successor})
		return sendRunTestsError(stream, run, &agentreplv1.RunTestsError{Cause: &agentreplv1.RunTestsError_TransferringAway{TransferringAway: &agentreplv1.RunTestsTransferringAway{Address: successor}}})
	}
	log.Info(opRunTests, "this daemon is stopping; it stopped a test run before its end", dlog.Context{"dir": dir, "run": run})
	return sendRunnerFailed(stream, run, -1, "the daemon stopped before the run ended, and no successor serves; run it again once the daemon is back")
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
	// inflight: joined: the function that starts it waits for it.
	go func() {
		result, err := s.deps.Tests.RunLinesWith(runCtx, dir, argv, scriptrunner.Options{ExtraFiles: []*os.File{pipe}}, tail.add)
		// THE WRITE END CLOSES WITH THE RUN, so the read below ends once
		// every event the run wrote has been read.
		pipe.Close() //nolint:errcheck // the reader sees its end either way
		done <- ran{result, err}
	}()

	// THE EVENTS ARE READ OFF THIS GOROUTINE, so the relay ends when the run is
	// stopped, not when the last holder of the pipe's write end lets go. The
	// script's group dies with the stop, but testrun's runner leads a session
	// of its own and winds its units down after it; a relay that read to EOF
	// waited for that wind-down, and a daemon's exit spent its whole deadline
	// on the RunTests stream (2026-11-15, `daemon.server.await_streams_ended`
	// "RunTests=1", and every GOAWAY after it left unsent).
	frames := readTestRunEvents(events, runCtx.Done())
	outcome, sendErr := runUnended, error(nil)
	// THE RUN'S END IS HELD UNTIL ITS SCRIPT EXITS, so the end relayed agrees
	// with the exit status: the script still runs steps after the runner has
	// ended (the git-state net, the timing record), and a script that exits
	// non-zero after an ended passed is relayed as ended failed, naming the
	// exit (2026-11-14: gates exited 1 while the stream said passed).
	var ended *testrunv1.TestRunEvent
relay:
	for {
		var read testRunRead
		select {
		case read = <-frames:
		case <-runCtx.Done():
			// THE RUN WAS STOPPED (the daemon's exit, the caller gone, an
			// unreadable stream): what it still writes is no longer relayed.
			break relay
		}
		ev, err := read.event, read.err
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			// A stream that cannot be read cannot be relayed: the run is
			// stopped, and its end is said as the runner's failure.
			log.Error(opRunTests, "a test run's events could not be read; the run is stopped", dlog.Context{"dir": dir, "run": run, "cause": err.Error()})
			cancel()
			break
		}
		switch {
		case ev.GetEnded().GetPassed() != nil, ev.GetEnded().GetPlanned() != nil:
			outcome = runPassed
		case ev.GetEnded() != nil:
			outcome = runFailed
		case ev.GetRefused() != nil:
			outcome = runRefused
		case ev.GetAborted() != nil:
			outcome = runAborted
			log.Error(opRunTests, "a test run stopped before it planned", dlog.Context{"dir": dir, "run": run, "selection": ev.GetAborted().GetSelection(), "cause": ev.GetAborted().GetDetail()})
		}
		if ev.GetEnded() != nil {
			ended = ev
			continue
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
	// CLOSING THE READ END ENDS THE READER, which a write end still held
	// elsewhere would otherwise keep blocked; a writer still running is
	// refused its writes rather than held on a full pipe.
	events.Close() //nolint:errcheck // the reader ends on the close
	result := <-done
	if ended == nil && sendErr == nil && s.life.Err() != nil {
		// The daemon's exit stopped the run (RunTests); its caller is told
		// where to send it, not that the run failed.
		return runCutByExit, nil
	}
	if ended != nil {
		if passed := ended.GetEnded().GetPassed(); passed != nil && result.err == nil && result.run.ExitCode != 0 {
			failure := fmt.Sprintf("the run's script exited %d after its runner said every unit passed; its last output:\n%s", result.run.ExitCode, tail.String())
			log.Error(opRunTests, "a test run's script failed after its runner ended passed; the run is relayed as failed", dlog.Context{
				"dir": dir, "run": run, "exit_code": result.run.ExitCode, "tail": tail.String(),
			})
			ended = &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Ended{Ended: &testrunv1.TestRunEnded{
				Outcome: &testrunv1.TestRunEnded_Failed{Failed: &testrunv1.TestRunEndedFailed{
					PassedUnits: passed.GetPassedUnits(), RunFailures: []string{failure},
				}},
			}}}
			outcome = runFailed
		}
		if sendErr == nil {
			sendErr = stream.Send(&agentreplv1.RunTestsResponse{Frame: &agentreplv1.RunTestsResponse_Event{Event: ended}, Run: run})
			if sendErr != nil {
				log.Info(opRunTests, "a test run's caller went away before its end was relayed", dlog.Context{"dir": dir, "run": run, "cause": sendErr.Error()})
			}
		}
	}
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

// testRunRead is one read of a test run's events stream: the event, or the
// error that ended the stream (io.EOF at its orderly end).
type testRunRead struct {
	event *testrunv1.TestRunEvent
	err   error
}

// readTestRunEvents reads EVENTS on a goroutine of its own, answering each
// event, and then the error that ended the stream (io.EOF at its orderly end),
// on the channel it returns. It ends at the stream's end, at a read that
// fails, or once STOP is closed: a relay that stopped reading never holds it
// on a send, and the relay's close of EVENTS ends a read still blocked.
func readTestRunEvents(events *os.File, stop <-chan struct{}) <-chan testRunRead {
	frames := make(chan testRunRead)
	reader := bufio.NewReader(events)
	// STOP or the stream's end ends it, and the relay closes EVENTS before it
	// returns, so the relay that starts it outlives it.
	// inflight: joined: the relay that starts it ends it.
	go func() {
		for {
			ev := &testrunv1.TestRunEvent{}
			read := testRunRead{event: ev, err: protodelim.UnmarshalFrom(reader, ev)}
			if read.err != nil {
				read.event = nil
			}
			select {
			case frames <- read:
			case <-stop:
				return
			}
			if read.err != nil {
				return
			}
		}
	}()
	return frames
}

// errUnknownTestWorkspace is a test run's workspace this daemon does not
// serve (testWorktreeOf).
var errUnknownTestWorkspace = errors.New("no workspace this daemon serves is the one named")

// transferredTestWorkspace is a test run's workspace this daemon handed to the
// successor at address, mid-handover.
type transferredTestWorkspace struct{ address string }

func (e *transferredTestWorkspace) Error() string {
	return "the workspace was handed to the successor at " + e.address
}

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
	// name is the workspace's name, which its run names itself by in the RAM
	// disk seat it holds (merge.TestCommandFor).
	name string
	// id is the workspace whose run this is.
	id ids.WorkspaceID
}

// testWorktreeOf resolves ref to the worktree its test run runs in, recording
// each refusal under op: errUnknownTestWorkspace for a workspace this daemon
// does not serve, a *noTestScriptError for a worktree with no test script, or
// the resolution's own failure (already answered through failResolution).
func (s *server) testWorktreeOf(ctx context.Context, rpc, op string, ref *workspacev1.WorkspaceRef) (testWorktree, error) {
	// THE STANDING IS ANSWERED HERE: a handover's
	// transfer is a typed arm of both test endpoints, so it is no unlanded one.
	subject, refused, err := s.resolveRef(ctx, rpc, ref)
	if err != nil {
		return testWorktree{}, failResolution(s.log, rpc, err)
	}
	if refused != nil && refused.Arm == workspace.ArmTransferringAway {
		address, _ := refused.Fields["address"].(string)
		s.log.Info(op, "refused a test run for a workspace this daemon handed to its successor; the caller sends it there", dlog.Context{
			"workspace_id": ref.GetId(), "successor": address})
		return testWorktree{}, &transferredTestWorkspace{address: address}
	}
	if refused != nil {
		s.log.Warn(opUnlandedArm+".standing", "refused a workspace this daemon does not serve",
			dlog.Context{"rpc": rpc, "arm": refused.Arm, "workspace": ref.GetId()})
		s.log.Info(op, "refused a test run for a workspace this daemon does not serve", dlog.Context{
			"workspace_id": ref.GetId(), "arm": refused.Arm, "reason": refused.Reason})
		return testWorktree{}, errUnknownTestWorkspace
	}
	w := testWorktree{log: subject.Log, dir: subject.Record.Dir, name: subject.Record.Name, id: subject.Record.ID}
	if w.script, err = s.testScriptOf(w.dir); err != nil {
		w.log.Error(op, "a test run has no test script to run", dlog.Context{"dir": w.dir, "cause": err.Error()})
		return testWorktree{}, &noTestScriptError{cause: err}
	}
	return w, nil
}

// handCutWorktree is a main workspace's hand-cut worktree resolved for a test
// run: where the run runs, or the *noTestScriptError of a worktree with no
// test script.
type handCutWorktree struct {
	worktree testWorktree
	err      error
}

// handCutWorktreeOf resolves DIR, the worktree the workspace resolved as OWN
// asked to run tests in instead of its own. It answers a refusal's detail
// when that workspace is not its repository's main workspace or DIR is no git
// worktree of that repository, and an error only when the registry cannot be
// read.
func (s *server) handCutWorktreeOf(ctx context.Context, own testWorktree, dir string) (handCutWorktree, string, error) {
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		own.log.Error(opRunTests, "could not read the repository registry to judge a test run's worktree", dlog.WithCause(dlog.Context{"worktree": dir}, err))
		return handCutWorktree{}, "", connect.NewError(connect.CodeInternal, fmt.Errorf("run tests in %s: read the repository registry: %w", dir, err))
	}
	main := false
	for _, repository := range repositories {
		main = main || wsm.IsMainWorktree(own.dir, repository.Dir)
	}
	if !main {
		return handCutWorktree{}, fmt.Sprintf("the workspace at %s is not its repository's main workspace; only a main workspace runs tests in a worktree it cut by hand", own.dir), nil
	}
	if !filepath.IsAbs(dir) {
		return handCutWorktree{}, fmt.Sprintf("the worktree %q is not an absolute directory", dir), nil
	}
	dir = filepath.Clean(dir)
	if err := gitlayout.LinkedWorktreeOf(dir, own.dir); err != nil {
		return handCutWorktree{}, err.Error(), nil
	}
	// The run is still the main workspace's: its bypass standing decides the
	// run's governance, so the cut worktree carries its id.
	cut := testWorktree{log: own.log.With(dlog.Context{"worktree": dir}), dir: dir, name: filepath.Base(dir), id: own.id}
	if cut.script, err = s.testScriptOf(dir); err != nil {
		own.log.Error(opRunTests, "a test run's hand-cut worktree has no test script to run", dlog.WithCause(dlog.Context{"worktree": dir}, err))
		return handCutWorktree{err: &noTestScriptError{cause: err}}, "", nil
	}
	return handCutWorktree{worktree: cut}, "", nil
}

// testScriptOf is the test script the worktree's deploy config names, read
// as every deploy config is (Deps.DeployConfig).
func (s *server) testScriptOf(dir string) (string, error) {
	cfg, err := s.deps.DeployConfig.Load(dir)
	if err != nil {
		return "", fmt.Errorf("read the worktree's deploy config: %w", err)
	}
	script := lifecycleconfig.TestScript(cfg)
	if script == "" {
		return "", fmt.Errorf("the worktree's deploy config %s names no agent_repl_suites test script, so this repository's tests are not the daemon's to run", lifecycleconfig.Path(dir))
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

// userOnlyOptions names the options of a run that only the user may ask for,
// as the command line spells them: a fresh run and an ungoverned run.
func userOnlyOptions(msg *agentreplv1.RunTestsRequest) []string {
	var out []string
	if msg.GetRun().GetNoReuse() {
		out = append(out, "-no-reuse")
	}
	if msg.GetUngoverned() {
		out = append(out, "-ungoverned")
	}
	return out
}
