package merge

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"strings"
	"time"

	"google.golang.org/protobuf/encoding/protodelim"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE QUEUE'S RACES ARE FOUND BY THE DAEMON (owner ruling, 2026-11-25). A
// repair reran a gate's failing tests until they passed and ended its turn,
// so a race that failed only under the gate's load passed on its rerun and
// was never fixed: the same race failed merge after merge. A test that
// failed in a gate and then passed on the SAME inputs digest
// (testrun.v1.TestRunUnitEnded.inputs_digest) failed for no change of the
// code it ran, whichever merge it ran in, so the daemon records it as a race
// finding (wsm racefindings.go), and the deploy mediator is handed every open
// one at its next check (intakeBrief), and dispatches its fix as it does a
// deploy's problem.
//
// Every gate's failures are kept as candidates, and every pass a gate or a
// workspace's own run reports promotes the candidates its digest matches.
// The decision rests on the digest alone: an agent's reading of a failure
// decides nothing.

// opRaceFinding is the operation every race finding record is filed under.
const opRaceFinding = "daemon.merge.race_finding"

// The events CountRaceFinding records.
const (
	// RaceFindingNew is a test the repository held no finding of.
	RaceFindingNew = "new"
	// RaceFindingRecurred is another failure of a test already found.
	RaceFindingRecurred = "recurred"
	// RaceFindingReopened is a failure of a test whose fix had deployed.
	RaceFindingReopened = "reopened"
)

// raceEvent is how PROMOTION counts.
func raceEvent(p wsm.Promotion) string {
	switch {
	case p.New:
		return RaceFindingNew
	case p.Reopened:
		return RaceFindingReopened
	}
	return RaceFindingRecurred
}

// failedTest is one test a run failed, under the digest of its unit.
type failedTest struct {
	ref     wsm.RaceFindingRef
	digest  string
	failure string
}

// TestOutcomes reads a run's unit verdicts into the tests it failed and the
// tests it passed, each under its unit's inputs digest. A unit with no digest
// was not confined to its inputs and proves nothing, so it is left out. A Go
// test is named by its top-level name: a subtest's failure fails its test,
// and a test passed only when none of its subtests failed in the run.
func TestOutcomes(units []*testrunv1.TestRunUnitEnded) (failed []failedTest, passed []wsm.TestPass) {
	type key struct{ suite, test, digest string }
	failures := map[key]string{}
	var failedOrder []key
	passes := map[key]bool{}
	var passOrder []key
	for _, u := range units {
		if u.InputsDigest == nil || u.GetInputsDigest() == "" {
			continue
		}
		for _, t := range u.GetTests() {
			k := key{suite: u.GetSuite(), test: topLevelTest(t.GetName()), digest: u.GetInputsDigest()}
			switch {
			case t.GetFailed() != nil:
				if _, seen := failures[k]; !seen {
					failedOrder = append(failedOrder, k)
				}
				failures[k] = u.GetFailed().GetOutputTail()
			case t.GetPassed() != nil:
				if !passes[k] {
					passOrder = append(passOrder, k)
				}
				passes[k] = true
			}
		}
	}
	for _, k := range failedOrder {
		failed = append(failed, failedTest{ref: wsm.RaceFindingRef{Suite: k.suite, Test: k.test}, digest: k.digest, failure: failures[k]})
	}
	for _, k := range passOrder {
		if _, failedToo := failures[k]; failedToo {
			continue
		}
		passed = append(passed, wsm.TestPass{Ref: wsm.RaceFindingRef{Suite: k.suite, Test: k.test}, InputsDigest: k.digest})
	}
	return failed, passed
}

// topLevelTest is NAME's test: a Go subtest's top-level test, any other
// name itself.
func topLevelTest(name string) string {
	if strings.HasPrefix(name, "Test") {
		if top, _, sub := strings.Cut(name, "/"); sub {
			return top
		}
	}
	return name
}

// ReadRunEvents reads the unit verdicts of the testrun.v1.TestRunEvent stream
// a run wrote to PATH (bin/test-all.sh --events-file).
func ReadRunEvents(path string) ([]*testrunv1.TestRunUnitEnded, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("merge: open the run's events: %w", err)
	}
	defer file.Close()
	in := bufio.NewReader(file)
	var out []*testrunv1.TestRunUnitEnded
	for {
		ev := &testrunv1.TestRunEvent{}
		err := protodelim.UnmarshalFrom(in, ev)
		if errors.Is(err, io.EOF) {
			return out, nil
		}
		if err != nil {
			return nil, fmt.Errorf("merge: read the run's events at %s: %w", path, err)
		}
		if u := ev.GetUnitEnded(); u != nil {
			out = append(out, u)
		}
	}
}

// observeGateRaces reads the gate run's events at EVENTS, keeps every test it
// failed as a candidate of REPO's race findings, failed in MERGE at AT with
// the run archived at ARCHIVE, and promotes every candidate a test it passed
// matches. A failure is recorded at ERROR and passed over: the gate's verdict
// stands, and only this run's evidence is lost.
func (o *orchestrator) observeGateRaces(ctx context.Context, repo wsm.RepoKey, merge, events, archive string, at time.Time) {
	fields := dlog.Context{"repo": string(repo), "merge": merge, "events": events}
	units, err := ReadRunEvents(events)
	// A GATE WHOSE SCRIPT WROTE NO RESULTS PROVES NOTHING: its script is one
	// from before the results file (bin/test-all.sh opens it before its
	// runner starts), as a merge into a parent's branch may still run.
	if errors.Is(err, fs.ErrNotExist) {
		o.deps.Log.Global().Info(opRaceFinding, "the gate's script wrote no test results; its failures are no race candidates and its passes promote none", fields)
		return
	}
	if err != nil {
		o.deps.Log.Global().Error(opRaceFinding, "could not read the gate's test results; its failures are no race candidates and its passes promote none", withField(fields, "error", err.Error()))
		return
	}
	failed, passed := TestOutcomes(units)
	if len(failed) > 0 {
		failures := make([]wsm.GateTestFailure, 0, len(failed))
		for _, f := range failed {
			failures = append(failures, wsm.GateTestFailure{Ref: f.ref, InputsDigest: f.digest, Merge: merge, FailedAt: at, Failure: f.failure, Artifacts: archive})
		}
		if err := o.deps.DB.RecordGateFailures(ctx, repo, failures); err != nil {
			o.deps.Log.Global().Error(opRaceFinding, "could not keep the gate's failed tests as race candidates; a later pass of them finds no race", withField(withField(fields, "failed", len(failed)), "error", err.Error()))
		} else {
			o.deps.Log.Global().Info(opRaceFinding, "kept the gate's failed tests as race candidates until a pass on their inputs", withField(fields, "failed", len(failed)))
		}
	}
	o.promoteRaces(ctx, repo, passed, fields)
}

// eventsOf is the events file beside the tests log ARCHIVE (testsLogOf).
func eventsOf(archive string) string {
	return strings.TrimSuffix(archive, ".log") + "-events.log"
}

// racesOnly reports whether every test the run's last failed gate failed
// is a race finding its failure was promoted to: the gate's events name each
// failed test, each under its digest, and none is still a candidate. A gate
// with a failed unit that names no test, or carries no digest, failed for
// something no race explains. Reading the events or the store failing is an
// error the merge fails on.
func (r *run) racesOnly(ctx context.Context) (bool, error) {
	if r.failedArchive == "" {
		return false, nil
	}
	units, err := ReadRunEvents(eventsOf(r.failedArchive))
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	for _, u := range units {
		if u.GetFailed() == nil {
			continue
		}
		named := false
		for _, t := range u.GetTests() {
			named = named || t.GetFailed() != nil
		}
		if !named || u.GetInputsDigest() == "" {
			return false, nil
		}
	}
	failed, _ := TestOutcomes(units)
	if len(failed) == 0 {
		return false, nil
	}
	name, err := r.o.workspaceName(ctx, opRaceFinding, r.ws)
	if err != nil {
		return false, err
	}
	asked := make([]wsm.TestPass, 0, len(failed))
	for _, f := range failed {
		asked = append(asked, wsm.TestPass{Ref: f.ref, InputsDigest: f.digest})
	}
	pending, err := r.o.deps.DB.UnpromotedGateFailures(ctx, r.repo, name, asked)
	if err != nil {
		return false, err
	}
	return len(pending) == 0, nil
}

// ObserveTestRun implements Orchestrator: a workspace's own run in the
// worktree DIR reported UNITS, and every test it passed promotes the gate
// failures of DIR's repository its digest matches. Only a gate's failures are
// candidates: a workspace's own failure is work in progress. A repository
// that cannot be resolved is recorded at ERROR and its passes promote none.
func (o *orchestrator) ObserveTestRun(ctx context.Context, dir string, workspace ids.WorkspaceID, units []*testrunv1.TestRunUnitEnded) {
	_, passed := TestOutcomes(units)
	if len(passed) == 0 {
		return
	}
	fields := dlog.Context{"dir": dir, "run_workspace": string(workspace)}
	repo, err := o.repoKeyFor(ctx, dir)
	if err != nil {
		o.deps.Log.Global().Error(opRaceFinding, "could not resolve the repository of a workspace's test run; its passes promote no race candidate", withField(fields, "error", err.Error()))
		return
	}
	o.promoteRaces(ctx, repo, passed, withField(fields, "repo", string(repo)))
}

// promoteRaces promotes REPO's candidates PASSED matches, records each
// finding made or updated, and counts it. A failure is recorded at ERROR:
// the candidates stay for the next pass.
func (o *orchestrator) promoteRaces(ctx context.Context, repo wsm.RepoKey, passed []wsm.TestPass, fields dlog.Context) {
	if len(passed) == 0 {
		return
	}
	promoted, err := o.deps.DB.PromoteRaces(ctx, repo, passed)
	if err != nil {
		o.deps.Log.Global().Error(opRaceFinding, "could not promote the gate failures the run's passes match; they wait for the next pass", withField(withField(fields, "passed", len(passed)), "error", err.Error()))
		return
	}
	for _, p := range promoted {
		event := raceEvent(p)
		o.deps.Meter.CountRaceFinding(event)
		o.deps.Log.Global().Info(opRaceFinding, "a test that failed in a merge gate passed on the same inputs: its failure is a race, handed to the deploy mediator's next check", dlog.Context{
			"repo": string(repo), "finding": p.Finding.Ref.String(), "event": event,
			"recurrences": p.Finding.Recurrences, "merges": strings.Join(p.Finding.Merges, ","),
			"artifacts": p.Finding.Artifacts, "taken_on": !p.Finding.Open()})
	}
}
