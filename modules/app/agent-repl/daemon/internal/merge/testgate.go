package merge

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// This file is the merge's TEST GATE's pieces: the result, and the parse that
// turns the script's own lines into per-suite state. The run itself, which
// streams those lines to the footer and the log as they are written, is
// gate.go's.
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
var suitePassed = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): passed in ([0-9]+(?:\.[0-9]+)?)s\s*$`)

// suiteFailed matches the script's per-suite failure line, which carries the
// suite's exit code.
var suiteFailed = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+) failed after ([0-9]+(?:\.[0-9]+)?)s with exit code (-?[0-9]+)\s*$`)

// suiteDeclined matches a selected suite whose precondition was unmet. A
// decline is terminal and non-failing: the gate continues and may pass.
var suiteDeclined = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): DECLINED after ([0-9]+(?:\.[0-9]+)?)s\b.*$`)

// suiteSkipped matches the line the script prints for a suite `--suites` left
// out. A skipped suite is not a state the tab draws: it was never selected.
var suiteSkipped = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): not selected by --suites, skipping\s*$`)

// suiteStarting matches the script's line as a suite starts.
var suiteStarting = regexp.MustCompile(`^.*?([A-Za-z0-9_-]+): starting\s*$`)

// suitePlanned matches the runner's line naming how many units a suite runs,
// printed as the suite starts: the total its unit verdicts are counted against.
var suitePlanned = regexp.MustCompile(`^.*?([A-Za-z0-9_-]+): ([0-9]+) units planned\s*$`)

// GateResult is one run of the target repository's test gate.
type GateResult struct {
	// Passed is the run's verdict, which is the script's exit status. A suite
	// that ran and failed is a verdict, never an error.
	Passed bool
	// Suites are the per-suite rows the tests tab draws, in the order the run
	// selected them.
	Suites []*frontendv1.FeedMergeTestSuite
	// Tail is the clamped tail of the run's output — what the fixing brief
	// carries.
	Tail string
	// ArchivePath names the file holding the run's COMPLETE output: the test
	// log the tests tab links.
	ArchivePath string
	// ExitCode is the script's exit status, kept as evidence on a failure.
	ExitCode int
}

// gateUnstartedError is a gate the runner could not start at all: the script
// was never spawned, so there is no verdict and no output to archive.
type gateUnstartedError struct{ err error }

func (e *gateUnstartedError) Error() string {
	return fmt.Sprintf("merge: the test gate could not run: %v", e.err)
}

func (e *gateUnstartedError) Unwrap() error { return e.err }

// paintSuites turns the run's output into one row per SELECTED suite, in the
// selection's order, with the output painted into spans.
//
// A selected suite the output says nothing about is left RUNNING rather than
// guessed at: the script was killed, or it never reached that suite, and
// neither of those is a pass.
func (o *orchestrator) paintSuites(selected []string, output string) ([]*frontendv1.FeedMergeTestSuite, error) {
	states := parseSuiteStates(output)
	sections := splitSuiteOutput(output, selected)
	counts := newSuiteCounts()
	for _, line := range strings.Split(output, "\n") {
		if _, _, err := counts.take(line); err != nil {
			return nil, err
		}
	}
	rows := make([]*frontendv1.FeedMergeTestSuite, 0, len(selected))
	for _, name := range selected {
		if states[name] == suiteStateSkipped {
			continue
		}
		spans, err := o.paintSpans(sections[name])
		if err != nil {
			return nil, err
		}
		row := tabSuite(name, states[name], counts.of(name))
		row.Output = spans
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
// zero value is a suite that started and has not settled -- or a selected
// suite the output never settled, which paintSuites draws running.
type suiteState int

const (
	// suiteStateRunning is a suite started and not settled.
	suiteStateRunning suiteState = iota
	// suiteStatePassed is a suite the script reported passing.
	suiteStatePassed
	// suiteStateFailed is a suite the script reported failing.
	suiteStateFailed
	// suiteStateDeclined is a selected suite whose precondition was unmet.
	suiteStateDeclined
	// suiteStateSkipped is a suite --suites left out; it draws no row.
	suiteStateSkipped
)

// parseSuiteStates reads the script's per-suite lines. A LATER line wins, so a
// suite that was reported skipped and then run reads as run.
func parseSuiteStates(output string) map[string]suiteState {
	states := map[string]suiteState{}
	for _, line := range strings.Split(output, "\n") {
		if m := suiteSkipped.FindStringSubmatch(line); m != nil {
			states[m[1]] = suiteStateSkipped
			continue
		}
		if name, state, settled := settledSuite(line); settled {
			states[name] = state
		}
	}
	return states
}

// splitSuiteOutput attributes the run's lines to the suites they belong to.
//
// The runner (testrun) runs many suites' units at once and prints each unit's
// output whole, bracketed by a line naming its unit and suite ("unit ID
// [SUITE] output:") and by the unit's own verdict line ("unit ID [SUITE]
// ok|FAILED|declined ..."). So a line inside a bracket belongs to that
// bracket's suite, and a line that names a suite -- its starting and verdict
// lines, a unit cancelled before it ran -- belongs to the suite it names.
//
// Any other line belongs to the suite whose verdict comes NEXT, which is the
// whole rule for a run that prints no brackets (one suite at a time). What
// trails the last verdict belongs to no suite and is dropped from the tab --
// the archive keeps it.
func splitSuiteOutput(output string, selected []string) map[string]string {
	wanted := map[string]bool{}
	for _, name := range selected {
		wanted[name] = true
	}
	lines := map[string][]string{}
	var pending []string
	blockUnit, blockSuite := "", ""
	for _, line := range strings.Split(output, "\n") {
		if blockUnit != "" {
			lines[blockSuite] = append(lines[blockSuite], line)
			if m := unitVerdict.FindStringSubmatch(line); m != nil && m[1] == blockUnit {
				blockUnit, blockSuite = "", ""
			}
			continue
		}
		if m := unitBegin.FindStringSubmatch(line); m != nil {
			blockUnit, blockSuite = m[1], m[2]
			lines[blockSuite] = append(lines[blockSuite], line)
			continue
		}
		if m := unitVerdict.FindStringSubmatch(line); m != nil {
			lines[m[2]] = append(lines[m[2]], line)
			continue
		}
		if m := suiteStarting.FindStringSubmatch(line); m != nil {
			lines[m[1]] = append(lines[m[1]], line)
			continue
		}
		if m := suitePlanned.FindStringSubmatch(line); m != nil {
			lines[m[1]] = append(lines[m[1]], line)
			continue
		}
		name, _, settled := settledSuite(line)
		if !settled {
			pending = append(pending, line)
			continue
		}
		lines[name] = append(append(lines[name], pending...), line)
		pending = nil
	}
	sections := map[string]string{}
	for name, ls := range lines {
		if wanted[name] {
			sections[name] = strings.Join(ls, "\n")
		}
	}
	return sections
}

// unitBegin is the runner's line opening one unit's output block.
var unitBegin = regexp.MustCompile(`^.*?unit (\S+) \[([A-Za-z0-9_-]+)\] output:\s*$`)

// unitVerdict is the runner's line settling one unit: it closes the unit's
// block, or names a unit cancelled before it ran. The third group is the
// verdict.
var unitVerdict = regexp.MustCompile(`^.*?unit (\S+) \[([A-Za-z0-9_-]+)\] (ok|FAILED|declined|NOT RUN)\b`)

// tabSuite is one suite's tab row at a state, with its counts when known.
//
// A RUNNING SUITE SAYS WHAT ITS TESTS HAVE SAID SO FAR, which its dot is drawn
// from: failing once any unit failed, passing while every verdict is a pass,
// unreported before the first verdict or with no counts at all.
func tabSuite(name string, state suiteState, counts *frontendv1.FeedMergeTestCounts) *frontendv1.FeedMergeTestSuite {
	suite := &frontendv1.FeedMergeTestSuite{Name: name, Counts: counts}
	switch state {
	case suiteStatePassed, suiteStateDeclined:
		// The wire has no declined arm. Preserve the explicit DECLINED line
		// in Output and use the existing non-failure terminal glyph.
		suite.State = &frontendv1.FeedMergeTestSuite_Passed{Passed: &frontendv1.FeedMergeTestSuitePassed{}}
	case suiteStateFailed:
		suite.State = &frontendv1.FeedMergeTestSuite_Failed{Failed: &frontendv1.FeedMergeTestSuiteFailed{}}
	default:
		running := &frontendv1.FeedMergeTestSuiteRunning{}
		switch {
		case counts.GetFailed() > 0:
			running.SoFar = &frontendv1.FeedMergeTestSuiteRunning_Failing{Failing: &frontendv1.FeedMergeTestSuiteRunningFailing{}}
		case counts.GetPassed() > 0:
			running.SoFar = &frontendv1.FeedMergeTestSuiteRunning_Passing{Passing: &frontendv1.FeedMergeTestSuiteRunningPassing{}}
		default:
			running.SoFar = &frontendv1.FeedMergeTestSuiteRunning_Unreported{Unreported: &frontendv1.FeedMergeTestSuiteRunningUnreported{}}
		}
		suite.State = &frontendv1.FeedMergeTestSuite_Running{Running: running}
	}
	return suite
}

// suiteCounts are each suite's unit counts, read off the runner's lines: the
// planned total from its "N units planned" line, a pass for each "ok" unit
// verdict, a failure for each "FAILED" or "NOT RUN" one. A declined unit is
// neither. A suite whose planned line has not been read has no counts.
type suiteCounts struct {
	planned map[string]int
	passed  map[string]int
	failed  map[string]int
}

func newSuiteCounts() *suiteCounts {
	return &suiteCounts{planned: map[string]int{}, passed: map[string]int{}, failed: map[string]int{}}
}

// take reads one line, answering the suite whose counts it moved. A planned
// line whose total does not read as a number is an error: the line is the
// runner's, and a total this gate cannot read is a broken contract.
func (c *suiteCounts) take(line string) (string, bool, error) {
	if m := suitePlanned.FindStringSubmatch(line); m != nil {
		n, err := strconv.Atoi(m[2])
		if err != nil {
			return "", false, fmt.Errorf("merge: reading suite %s's planned unit total %q: %w", m[1], m[2], err)
		}
		c.planned[m[1]] = n
		return m[1], true, nil
	}
	m := unitVerdict.FindStringSubmatch(line)
	if m == nil {
		return "", false, nil
	}
	switch m[3] {
	case "ok":
		c.passed[m[2]]++
	case "FAILED", "NOT RUN":
		c.failed[m[2]]++
	default:
		return "", false, nil
	}
	return m[2], true, nil
}

// of answers a suite's counts, nil while its planned total is unknown.
func (c *suiteCounts) of(name string) *frontendv1.FeedMergeTestCounts {
	total, known := c.planned[name]
	if !known {
		return nil
	}
	return &frontendv1.FeedMergeTestCounts{
		Passed: uint32(c.passed[name]), Failed: uint32(c.failed[name]), Total: uint32(total),
	}
}

// settledSuite reports the suite one line settles, if it settles one.
func settledSuite(line string) (string, suiteState, bool) {
	if m := suitePassed.FindStringSubmatch(line); m != nil {
		return m[1], suiteStatePassed, true
	}
	if m := suiteFailed.FindStringSubmatch(line); m != nil {
		return m[1], suiteStateFailed, true
	}
	if m := suiteDeclined.FindStringSubmatch(line); m != nil {
		return m[1], suiteStateDeclined, true
	}
	return "", suiteStateRunning, false
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
