package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/testcases"
)

// testcases.go — the workspace's record of its test cases, which `agent-repld
// test run -rerun` runs (agentrepl.v1.RunTestsRerun). EVERY RUN A WORKSPACE
// STARTS IS RECORDED, as the cases it named (testcases.Of), before it runs;
// a dry run runs nothing and records nothing. A RERUN RUNS THE RECORD, each
// case its own selection, with the runner told to drop every case the tree no
// longer holds (testrun.v1.TestRunRequest.drop_missing); the cases it drops
// are forgotten, so a rename is said once. Every failure to read or write the
// record fails the call loudly: a rerun never falls back to running nothing
// or everything.

const opTestCases = "daemon.server.test_cases"

// recordTestCases records the cases RUN names for W's workspace, unless the
// run only plans. A run naming no case (the whole roster) records nothing.
func (s *server) recordTestCases(ctx context.Context, w testWorktree, run *testrunv1.TestRunRequest) error {
	cases := testcases.Of(run.GetSelections())
	if run.GetPlanOnly() || len(cases) == 0 {
		return nil
	}
	if err := s.deps.DB.RecordTestCases(ctx, w.id, cases, s.now().UnixMilli()); err != nil {
		w.log.Error(opTestCases, "a test run's cases could not be recorded, so the run does not start", dlog.WithCause(dlog.Context{
			"workspace": string(w.id), "dir": w.dir, "cases": len(cases)}, err))
		return connect.NewError(connect.CodeInternal, fmt.Errorf("record the run's test cases: %w", err))
	}
	w.log.Info(opTestCases, "recorded a test run's cases", dlog.Context{"workspace": string(w.id), "dir": w.dir, "cases": len(cases)})
	return nil
}

// rerunOf answers the rerun of W's recorded cases with OPTIONS' options, or
// nil when the record names no case.
func (s *server) rerunOf(ctx context.Context, w testWorktree, options *testrunv1.TestRunRequest) (*testrunv1.TestRunRequest, error) {
	cases, err := s.deps.DB.TestCasesOf(ctx, w.id)
	if err != nil {
		w.log.Error(opTestCases, "a rerun's recorded cases could not be read, so it does not start", dlog.WithCause(dlog.Context{
			"workspace": string(w.id), "dir": w.dir}, err))
		return nil, connect.NewError(connect.CodeInternal, fmt.Errorf("read the workspace's recorded test cases: %w", err))
	}
	if len(cases) == 0 {
		w.log.Info(opTestCases, "refused a rerun: the workspace's record names no case", dlog.Context{"workspace": string(w.id), "dir": w.dir})
		return nil, nil
	}
	w.log.Info(opTestCases, "rerunning the workspace's recorded cases", dlog.Context{"workspace": string(w.id), "dir": w.dir, "cases": len(cases)})
	// Every option the caller set is carried, an option added later
	// included.
	rerun := proto.Clone(options).(*testrunv1.TestRunRequest)
	rerun.Selections, rerun.DropMissing = cases, true
	return rerun, nil
}

// forgetTestCases forgets the cases a rerun of W's record dropped. It runs as
// the call ends, its caller's context perhaps gone, so it writes under the
// daemon's own.
func (s *server) forgetTestCases(w testWorktree, dropped []*testrunv1.TestSelection) error {
	if len(dropped) == 0 {
		return nil
	}
	forgot, err := s.deps.DB.ForgetTestCases(context.WithoutCancel(s.life), w.id, dropped)
	if err != nil {
		w.log.Error(opTestCases, "the cases a rerun dropped could not be forgotten, so the next rerun drops them again", dlog.WithCause(dlog.Context{
			"workspace": string(w.id), "dir": w.dir, "dropped": len(dropped)}, err))
		return connect.NewError(connect.CodeInternal, fmt.Errorf("forget the cases the rerun dropped: %w", err))
	}
	w.log.Info(opTestCases, "forgot the cases a rerun dropped because the tree no longer holds them", dlog.Context{
		"workspace": string(w.id), "dir": w.dir, "dropped": len(dropped), "forgotten": forgot})
	return nil
}
