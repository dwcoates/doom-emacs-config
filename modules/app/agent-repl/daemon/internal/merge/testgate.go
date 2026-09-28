package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// This file is the merge's TEST GATE: the run, its archive, and the parse that
// turns the script's own lines into per-suite state.
//
// THERE IS NO FLAKE RE-RUN. A failing suite is an error to remediate, period —
// a deliberate reversal of the old daemon, which re-ran once and let a second
// green paper over the first red. Re-running hides exactly the failures worth
// seeing, and the fixes loop is the remediation path a failure deserves.
//
// THE ARCHIVE IS NOT OPTIONAL EITHER. A multi-suite runner keeps going after a
// suite fails, so by the time it exits the failure's own output is thousands of
// lines back and a retained tail carries whatever the LAST suites printed. The
// verdict survives a tail; the diagnosis does not. Archiving the whole run and
// naming the file is what makes a failure reconstructible rather than only
// re-runnable.

// tailBytes bounds how much of a failing run's output travels into the fixes
// brief. The archive holds the whole thing; the brief holds what an agent can
// read.
const tailBytes = 4000

// suitePassed matches bin/test-all.sh's own per-suite pass line, whatever log
// prefix the script decorates it with.
var suitePassed = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): passed in ([0-9]+)s\s*$`)

// suiteFailed matches the script's per-suite failure line, which carries the
// suite's exit code.
var suiteFailed = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+) failed after ([0-9]+)s with exit code ([0-9]+)\s*$`)

// suiteSkipped matches the line the script prints for a suite `--suites` left
// out. A skipped suite is not a state the tab draws: it was never selected.
var suiteSkipped = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): not selected by --suites, skipping\s*$`)

// GateResult is one run of the target repository's test gate.
type GateResult struct {
	// Passed is the run's verdict, which is the script's exit status. A suite
	// that ran and failed is a verdict, never an error.
	Passed bool
	// Suites are the per-suite rows the tests tab draws, in the order the run
	// selected them.
	Suites []*frontendv1.FeedMergeTestSuite
	// Tail is the clamped tail of the run's output — what the fixes brief
	// carries, and the only part of a run a user may ever read.
	Tail string
	// ArchivePath names the file holding the run's COMPLETE output.
	ArchivePath string
	// ExitCode is the script's exit status, kept as evidence on a failure.
	ExitCode int
}

// runGate runs the selected suites in the queue's tree and archives the run.
//
// An error means the run could not be CLASSIFIED: the script could not be
// spawned, or its output could not be archived. A gate whose archive failed is
// an unrunnable gate rather than a lost archive, because the file is the only
// account of the failure that survives the run.
func (o *orchestrator) runGate(ctx context.Context, lease ids.LeaseID, round int, tree string, command []string, sel SuiteSelection) (GateResult, error) {
	if err := validateSuites(sel.Suites); err != nil {
		return GateResult{}, err
	}
	argv := append([]string(nil), command...)
	if len(argv) == 0 {
		return GateResult{}, fmt.Errorf("merge: no test command is configured for the gate")
	}
	// The FULL selection passes no narrowing: running everything is the
	// script's own default, and a repository whose script knows no --suites
	// flag is never handed one.
	if !sel.Full {
		argv = append(argv, "--suites", strings.Join(sel.Suites, ","))
	}
	output, code, err := o.deps.TestRunner.Run(ctx, tree, argv)
	if err != nil {
		return GateResult{}, &gateUnstartedError{err: err}
	}
	archive, err := o.archiveGate(lease, round, output)
	if err != nil {
		return GateResult{}, err
	}
	suites, perr := o.paintSuites(sel.Suites, output)
	if perr != nil {
		return GateResult{}, perr
	}
	return GateResult{
		Passed:      code == 0,
		Suites:      suites,
		Tail:        clampTail(output, tailBytes),
		ArchivePath: archive,
		ExitCode:    code,
	}, nil
}

// gateUnstartedError is a gate the runner could not start at all: the script
// was never spawned, so there is no verdict and no output to archive.
type gateUnstartedError struct{ err error }

func (e *gateUnstartedError) Error() string {
	return fmt.Sprintf("merge: the test gate could not run: %v", e.err)
}

func (e *gateUnstartedError) Unwrap() error { return e.err }

// archiveGate writes one run's combined output under the state root's
// merge-logs/, named by the lease and the round so a run is findable from the
// ledger alone.
func (o *orchestrator) archiveGate(lease ids.LeaseID, round int, output string) (string, error) {
	dir := filepath.Join(o.deps.StateDir, "merge-logs")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("merge: creating the gate archive directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, fmt.Sprintf("%s-tests-%d.log", lease, round))
	if err := os.WriteFile(path, []byte(output), 0o644); err != nil {
		return "", fmt.Errorf("merge: archiving the gate run to %s: %w", path, err)
	}
	return path, nil
}

// paintSuites turns the run's output into one row per SELECTED suite, in the
// selection's order, with the output painted into spans.
//
// A selected suite the output says nothing about is left RUNNING rather than
// guessed at: the script was killed, or it never reached that suite, and
// neither of those is a pass.
func (o *orchestrator) paintSuites(selected []string, output string) ([]*frontendv1.FeedMergeTestSuite, error) {
	states := parseSuiteStates(output)
	sections := splitSuiteOutput(output, selected)
	rows := make([]*frontendv1.FeedMergeTestSuite, 0, len(selected))
	for _, name := range selected {
		if states[name] == suiteStateSkipped {
			continue
		}
		spans, err := o.paintSpans(sections[name])
		if err != nil {
			return nil, err
		}
		row := &frontendv1.FeedMergeTestSuite{Name: name, Output: spans}
		switch states[name] {
		case suiteStatePassed:
			row.State = &frontendv1.FeedMergeTestSuite_Passed{Passed: &frontendv1.FeedMergeTestSuitePassed{}}
		case suiteStateFailed:
			row.State = &frontendv1.FeedMergeTestSuite_Failed{Failed: &frontendv1.FeedMergeTestSuiteFailed{}}
		default:
			row.State = &frontendv1.FeedMergeTestSuite_Running{Running: &frontendv1.FeedMergeTestSuiteRunning{}}
		}
		rows = append(rows, row)
	}
	return rows, nil
}

// paintSpans parses one section's ANSI into paint spans. THE CLIENT NEVER
// PARSES AN ESCAPE: the daemon does it once and the client paints classes.
func (o *orchestrator) paintSpans(text string) ([]*frontendv1.FeedMergeTestSpan, error) {
	if text == "" {
		return nil, nil
	}
	spans, err := o.deps.Painter.ParseANSI(text)
	if err != nil {
		return nil, fmt.Errorf("merge: painting the gate's output: %w", err)
	}
	out := make([]*frontendv1.FeedMergeTestSpan, 0, len(spans))
	for _, span := range spans {
		out = append(out, &frontendv1.FeedMergeTestSpan{Text: span.Text, PaintClass: span.Class})
	}
	return out, nil
}

// suiteState is one suite's standing as the run's own lines report it. The
// zero value is a selected suite the output never settled (paintSuites'
// switch default draws it running) and carries no name of its own: nothing
// ever needs to name "still running" directly, only fall through to it.
type suiteState int

const (
	_ suiteState = iota // the zero value: still running, named nowhere
	// suiteStatePassed is a suite the script reported passing.
	suiteStatePassed
	// suiteStateFailed is a suite the script reported failing.
	suiteStateFailed
	// suiteStateSkipped is a suite --suites left out; it draws no row.
	suiteStateSkipped
)

// parseSuiteStates reads the script's per-suite lines. A LATER line wins, so a
// suite that was reported skipped and then run reads as run.
func parseSuiteStates(output string) map[string]suiteState {
	states := map[string]suiteState{}
	for _, m := range suiteSkipped.FindAllStringSubmatch(output, -1) {
		states[m[1]] = suiteStateSkipped
	}
	for _, m := range suitePassed.FindAllStringSubmatch(output, -1) {
		states[m[1]] = suiteStatePassed
	}
	for _, m := range suiteFailed.FindAllStringSubmatch(output, -1) {
		states[m[1]] = suiteStateFailed
	}
	return states
}

// splitSuiteOutput attributes the run's lines to the suites they belong to.
//
// The script interleaves nothing: it runs suites one at a time and announces
// each terminal, so a line belongs to the suite whose terminal comes NEXT. What
// trails the last terminal belongs to no suite and is dropped from the tab —
// the archive keeps it.
func splitSuiteOutput(output string, selected []string) map[string]string {
	wanted := map[string]bool{}
	for _, name := range selected {
		wanted[name] = true
	}
	sections := map[string]string{}
	var pending []string
	for _, line := range strings.Split(output, "\n") {
		pending = append(pending, line)
		name, settled := settledSuite(line)
		if !settled || !wanted[name] {
			continue
		}
		sections[name] = strings.Join(pending, "\n")
		pending = nil
	}
	return sections
}

// settledSuite reports the suite one line settles, if it settles one.
func settledSuite(line string) (string, bool) {
	if m := suitePassed.FindStringSubmatch(line); m != nil {
		return m[1], true
	}
	if m := suiteFailed.FindStringSubmatch(line); m != nil {
		return m[1], true
	}
	return "", false
}

// clampTail keeps the LAST n bytes of the output, on a line boundary, so the
// brief's excerpt starts at a line rather than mid-word.
func clampTail(output string, n int) string {
	if len(output) <= n {
		return output
	}
	tail := output[len(output)-n:]
	if cut := strings.IndexByte(tail, '\n'); cut >= 0 && cut+1 < len(tail) {
		tail = tail[cut+1:]
	}
	return tail
}
