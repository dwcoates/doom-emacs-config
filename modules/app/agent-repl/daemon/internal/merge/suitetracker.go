package merge

import (
	"errors"
	"sync"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"

	"agentrepl/testrun/seatwait"
)

// suiteTracker follows one test run's lines into the tests view the client
// draws (frontend.v1.FeedMergeTestsRunOutput): each suite's standing and
// counts, the earlier runs its reused suites name, and the runner's wait for
// a RAM disk seat. THE ONE READER OF A RUN'S LINES for every gate that draws
// its tests: a queue merge's (gateRun, its bubble's tests tab) and a deploy
// mediator's fix (fixGateRun, its FeedFixGate card), so the two never read a
// line differently.
type suiteTracker struct {
	// nowMS stamps the start of a seat wait.
	nowMS func() int64

	mu sync.Mutex
	// suites are the view's rows, in the order the suites started.
	suites []*frontendv1.FeedMergeTestSuite
	// states are each started suite's standing, by name.
	states map[string]suiteState
	// sources are the earlier runs the reused suites name, by suite.
	sources map[string]string
	// counts are each suite's unit counts as the lines have reported them.
	counts *suiteCounts
	// seatWait is the runner's wait for a RAM disk seat while it waits, nil
	// otherwise.
	seatWait *frontendv1.FeedMergeTestsRunSeatWait
}

// newSuiteTracker answers a tracker that has seen no line, stamping seat
// waits by NOWMS.
func newSuiteTracker(nowMS func() int64) *suiteTracker {
	t := &suiteTracker{nowMS: nowMS}
	t.reset()
	return t
}

// reset puts the tracker back to its start: no suite seen, no seat wait.
func (t *suiteTracker) reset() {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.suites, t.states, t.sources, t.counts, t.seatWait = nil, map[string]suiteState{}, map[string]string{}, newSuiteCounts(), nil
}

// trackedLine is what one line moved.
type trackedLine struct {
	// moved reports that the view changed, so it is published again.
	moved bool
	// settled names the suite whose edge the line was (started, passed,
	// failed or reused), with its state; empty when the line was no edge.
	suite string
	state suiteState
}

// Why a line moved nothing. Each is the caller's to record: an unreadable
// seat line at ERROR (the runner writes it in the one spelling seatwait
// reads), an uncountable line at WARN (the run goes on, and the gate's final
// parse of the whole output fails it loudly).
var (
	errSeatLine  = errors.New("a test runner's RAM disk seat line could not be read")
	errCountLine = errors.New("a test runner line could not be counted")
)

// recordUnreadLine records ERR, a line LINE the tracker could not read,
// under OP on LOG: an unreadable seat line at ERROR, an uncountable line at
// WARN, with FIELDS. The one record of an unread line for every gate.
func recordUnreadLine(log dlog.Logger, op, line string, err error, fields dlog.Context) {
	if err == nil {
		return
	}
	record := dlog.Context{"line": line, "error": err.Error()}
	for k, v := range fields {
		record[k] = v
	}
	if errors.Is(err, errSeatLine) {
		log.Error(op, errSeatLine.Error(), record)
		return
	}
	log.Warn(op, errCountLine.Error(), record)
}

// take follows LINE: a seat line stands or clears the seat wait, a unit's
// verdict or a suite's planned total moves a started suite's counts, and a
// suite's edge stands its row. An error wraps errSeatLine or errCountLine.
func (t *suiteTracker) take(line string) (trackedLine, error) {
	if moved, seat, err := t.seatLine(line); seat || err != nil {
		return trackedLine{moved: moved}, err
	}
	t.mu.Lock()
	name, counted, countErr := t.counts.take(line)
	t.mu.Unlock()
	if countErr != nil {
		countErr = errors.Join(errCountLine, countErr)
	}
	if counted {
		t.mu.Lock()
		state, started := t.states[name]
		if started {
			t.stand(name, state)
		}
		t.mu.Unlock()
		return trackedLine{moved: started}, nil
	}
	name, state, ok := suiteEdge(line)
	if !ok {
		return trackedLine{}, countErr
	}
	t.mu.Lock()
	t.states[name] = state
	if _, source, reused := reusedSuite(line); reused {
		t.sources[name] = source
	}
	t.stand(name, state)
	t.mu.Unlock()
	return trackedLine{moved: true, suite: name, state: state}, countErr
}

// seatLine follows the runner's RAM disk seat lines (seatwait), answering
// whether LINE was one and whether the view moved: a wait stands the seat
// wait, keeping the start of a wait already drawn, and the seat taken (or
// none) clears it.
func (t *suiteTracker) seatLine(line string) (moved, seat bool, err error) {
	w, waiting, parseErr := seatwait.Parse(line)
	if parseErr != nil {
		return false, true, errors.Join(errSeatLine, parseErr)
	}
	if !waiting && !seatwait.Ended(line) {
		return false, false, nil
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	was := t.seatWait
	switch {
	case waiting && was != nil:
		t.seatWait = seatWaitView(w, was.GetStartedAtMs())
	case waiting:
		t.seatWait = seatWaitView(w, t.nowMS())
	default:
		t.seatWait = nil
	}
	return waiting || was != nil, true, nil
}

// stand stands a suite's row at its edge, with its counts so far. Called
// with t.mu held.
func (t *suiteTracker) stand(name string, state suiteState) {
	suite := tabSuite(name, state, t.counts.of(name), t.sources[name])
	for i, existing := range t.suites {
		if existing.GetName() == name {
			t.suites[i] = suite
			return
		}
	}
	t.suites = append(t.suites, suite)
}

// view answers the tests view as it stands, with LOG (nil until the run has
// written it).
func (t *suiteTracker) view(log *frontendv1.FeedMergeTestLog) *frontendv1.FeedMergeTestsRunOutput {
	t.mu.Lock()
	defer t.mu.Unlock()
	return &frontendv1.FeedMergeTestsRunOutput{
		Suites:   append([]*frontendv1.FeedMergeTestSuite(nil), t.suites...),
		Log:      log,
		SeatWait: t.seatWait,
	}
}
