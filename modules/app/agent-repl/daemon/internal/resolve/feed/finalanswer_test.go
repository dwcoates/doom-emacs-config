package feed

import (
	"context"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// ---- the fakes ----

// fakeFaults is the fault record, remembered rather than written. It is the
// resolver's whole fault dependency, so a test asserts what the footer would be
// told by asserting what was opened and closed.
type fakeFaults struct {
	mu     sync.Mutex
	seq    int
	opened []wsm.Fault
	ids    []ids.FaultID
	closed []ids.FaultID
	// openErr fails the next open, so the unrecordable path is provable.
	openErr error
}

func (f *fakeFaults) OpenFault(_ context.Context, fault wsm.Fault) (ids.FaultID, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.openErr != nil {
		return "", f.openErr
	}
	f.seq++
	id := ids.FaultID(string(rune('a'+f.seq-1)) + "-fault")
	fault.ID = id
	f.opened = append(f.opened, fault)
	f.ids = append(f.ids, id)
	return id, nil
}

func (f *fakeFaults) CloseFault(_ context.Context, id ids.FaultID, _ time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.closed = append(f.closed, id)
	return nil
}

// OpenFaults answers the standing faults in scope, as the state client does.
func (f *fakeFaults) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	var out []wsm.Fault
	for _, fault := range f.standing() {
		if scope.Workspace != nil && (fault.Workspace == nil || *fault.Workspace != *scope.Workspace) {
			continue
		}
		out = append(out, fault)
	}
	return out, nil
}

// standing answers the faults opened and not since closed, in open order.
func (f *fakeFaults) standing() []wsm.Fault {
	f.mu.Lock()
	defer f.mu.Unlock()
	gone := map[ids.FaultID]bool{}
	for _, id := range f.closed {
		gone[id] = true
	}
	var out []wsm.Fault
	for i, fault := range f.opened {
		if !gone[f.ids[i]] {
			out = append(out, fault)
		}
	}
	return out
}

// fakeStallTimer is one armed window, with the virtual instant it is due.
type fakeStallTimer struct {
	d       time.Duration
	due     time.Duration
	f       func()
	stopped bool
}

func (t *fakeStallTimer) Stop() bool {
	was := t.stopped
	t.stopped = true
	return !was
}

// fakeStallClock is a VIRTUAL clock: windows are armed against it and a test
// ADVANCES it rather than waiting. A window fires only once the advance has
// carried the clock to its due instant — which is what lets a test say the
// stall fires at ninety seconds and not at eighty-nine. Firing happens on the
// test's own goroutine, from outside any resolver call, exactly as the real
// clock's goroutine reaches the resolver.
type fakeStallClock struct {
	mu    sync.Mutex
	now   time.Duration
	armed []*fakeStallTimer
}

func (c *fakeStallClock) AfterFunc(d time.Duration, f func()) Timer {
	c.mu.Lock()
	defer c.mu.Unlock()
	timer := &fakeStallTimer{d: d, due: c.now + d, f: f}
	c.armed = append(c.armed, timer)
	return timer
}

// live is the one window still armed, or nil.
func (c *fakeStallClock) live() *fakeStallTimer {
	c.mu.Lock()
	defer c.mu.Unlock()
	for i := len(c.armed) - 1; i >= 0; i-- {
		if !c.armed[i].stopped {
			return c.armed[i]
		}
	}
	return nil
}

// advance carries the virtual clock forward and fires every window that came
// due, oldest first.
func (c *fakeStallClock) advance(d time.Duration) {
	c.mu.Lock()
	c.now += d
	now := c.now
	var due []*fakeStallTimer
	for _, timer := range c.armed {
		if !timer.stopped && timer.due <= now {
			timer.stopped = true
			due = append(due, timer)
		}
	}
	c.mu.Unlock()
	for _, timer := range due {
		timer.f()
	}
}

// elapse carries the clock exactly to the live window's due instant.
func (c *fakeStallClock) elapse() {
	timer := c.live()
	if timer == nil {
		return
	}
	c.mu.Lock()
	remaining := timer.due - c.now
	c.mu.Unlock()
	c.advance(remaining)
}

// ---- harness helpers ----

// standingAnswerFault is the one standing final-answer fault, or nil.
func (h *harness) standingAnswerFault() *wsm.Fault {
	h.t.Helper()
	var out *wsm.Fault
	for _, fault := range h.faults.standing() {
		if fault.Kind != health.KindFinalAnswerUnresolved {
			continue
		}
		if out != nil {
			h.t.Fatalf("two final-answer faults stand at once: %v and %v", out.Evidence, fault.Evidence)
		}
		copied := fault
		out = &copied
	}
	return out
}

// errorRecords answers the ERROR records logged under one operation.
func (h *harness) errorRecords(operation string) []dlog.Record {
	h.t.Helper()
	var out []dlog.Record
	for _, rec := range h.log.Records() {
		if rec.Level == "error" && rec.Operation == operation {
			out = append(out, rec)
		}
	}
	return out
}

// concludeWithoutAnswer ends a turn on a success that names no answering unit.
func (h *harness) concludeWithoutAnswer(turn string) {
	h.t.Helper()
	id := ids.TurnID(turn)
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil, nil)
}

// ---- LANDED ----

// TestALandedAnswerRaisesNoFault pins the silent case: the terminal names an
// answer, the row is drawn, the border goes on, and nothing reaches the footer.
func TestALandedAnswerRaisesNoFault(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act.
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Assert.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a landed answer raised a fault: %v", fault.Evidence)
	}
	if got := h.errorRecords("daemon.feed.final_answer_unresolved"); len(got) != 0 {
		t.Fatalf("a landed answer logged %d unresolved records, want none", len(got))
	}
}

// TestALandedAnswerStillGreensOnReplay pins that the replay path — the same
// terminal arriving a second time, as it does when history is walked after a
// reconnect — keeps the row green and raises nothing.
func TestALandedAnswerStillGreensOnReplay(t *testing.T) {
	// Arrange: the answer landed once.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Act: the terminal replays.
	turn := ids.TurnID("turn-1")
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &turn, completedWith("unit-1"), nil, nil)

	// Assert: still green, still no fault.
	rows := h.responseRows()
	if len(rows) != 1 || !rows[0].GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("the replayed terminal did not leave the answer row green")
	}
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a replayed landed answer raised a fault: %v", fault.Evidence)
	}
}

// ---- NOT LANDED ----

// TestATurnThatDidNotLandItsAnswerRaisesTheFault covers both NOT LANDED cases:
// a terminal naming no answer over drawn prose, and a terminal naming an answer
// no drawn row resolves.
func TestATurnThatDidNotLandItsAnswerRaisesTheFault(t *testing.T) {
	tests := []struct {
		name     string
		conclude func(h *harness)
		wantWhy  string
		wantUnit string
	}{
		{
			name:     "the terminal names no answer while the turn drew prose",
			conclude: func(h *harness) { h.concludeWithoutAnswer("turn-1") },
			wantWhy:  whyNoAnswerNamed,
			wantUnit: "unit-1",
		},
		{
			name: "the terminal names an answer no drawn row resolves",
			conclude: func(h *harness) {
				turn := ids.TurnID("turn-1")
				h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &turn,
					completedWith("unit-never-drawn"), nil, nil)
			},
			wantWhy:  whyAnswerRowUnresolved,
			wantUnit: "unit-never-drawn",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a turn that drew settled response prose.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.resolver.OnActivity(testWorkspace, mainAgent(),
				responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
					Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
				}, nil), nil, nil)

			// Act.
			tt.conclude(h)

			// Assert: the fault stands, saying which way it failed.
			fault := h.standingAnswerFault()
			if fault == nil {
				t.Fatal("no final-answer fault stands")
			}
			if fault.Evidence["why"] != tt.wantWhy {
				t.Fatalf("why = %q, want %q", fault.Evidence["why"], tt.wantWhy)
			}
			if fault.Evidence["unit"] != tt.wantUnit {
				t.Fatalf("unit = %q, want %q", fault.Evidence["unit"], tt.wantUnit)
			}
			if fault.Evidence["turn"] != "turn-1" {
				t.Fatalf("turn = %q, want turn-1", fault.Evidence["turn"])
			}
		})
	}
}

// TestATurnThatDidNotLandItsAnswerIsRecordedAtError pins the LOUDNESS: the
// condition is an ERROR record under its own operation, not a debug line.
func TestATurnThatDidNotLandItsAnswerIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	records := h.errorRecords("daemon.feed.final_answer_unresolved")
	if len(records) != 1 {
		t.Fatalf("unresolved ERROR records = %d, want exactly one", len(records))
	}
	ctx := records[0].Context
	if ctx["turn"] != "turn-1" || ctx["unit"] != "unit-1" || ctx["why"] != whyNoAnswerNamed {
		t.Fatalf("record context = %v, want the turn, the unit and the why", ctx)
	}
}

// TestATurnThatDrewNoProseAndNamedNoAnswerRaisesNothing pins the edge the rule
// is scoped by: a turn with nothing to answer with lost no answer.
func TestATurnThatDrewNoProseAndNamedNoAnswerRaisesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a turn that drew no prose raised a fault: %v", fault.Evidence)
	}
}

// apiErrorNotice is the vendor's synthesized "API Error" notice as a producer
// marks it: prose in the shape of an answer that no model wrote.
func apiErrorNotice() *conversationv1.AgentResponseSuccess {
	return &conversationv1.AgentResponseSuccess{
		Prose: &conversationv1.AgentResponseProse{Markdown: "API Error: Can't reach the API server"},
		Authorship: &conversationv1.AgentResponseSuccess_SynthesizedNotice{
			SynthesizedNotice: &conversationv1.AgentResponseSynthesizedNotice{
				Subject: &conversationv1.AgentResponseSynthesizedNotice_Unclassified{
					Unclassified: &conversationv1.AgentNoticeUnclassified{},
				},
			},
		},
	}
}

// TestATurnWhoseOnlyProseIsAVendorNoticeRaisesNothing pins the notice exclusion
// (the 2026-09-24 replay of turn c9d7014b): a turn the vendor answered with its
// own "API Error" notice and nothing else never had an answer to lose, so a
// terminal naming no answer over it is not a defect.
func TestATurnWhoseOnlyProseIsAVendorNoticeRaisesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", apiErrorNotice(), nil), nil, nil)

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	if got := h.errorRecords("daemon.feed.final_answer_unresolved"); len(got) != 0 {
		t.Fatalf("a notice-only turn logged %d unresolved records, want none", len(got))
	}
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a notice-only turn raised a fault: %v", fault.Evidence)
	}
}

// TestAVendorNoticeBesideModelProseStillRaisesTheFault pins that the exclusion covers the
// notice alone: model prose drawn in the same turn is still an answer the
// terminal owed a name.
func TestAVendorNoticeBesideModelProseStillRaisesTheFault(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", apiErrorNotice(), nil), nil, nil)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-2", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	fault := h.standingAnswerFault()
	if fault == nil {
		t.Fatal("no final-answer fault stands over the model's prose")
	}
	if fault.Evidence["unit"] != "unit-2" || fault.Evidence["why"] != whyNoAnswerNamed {
		t.Fatalf("fault evidence = %v, want unit-2 and %q", fault.Evidence, whyNoAnswerNamed)
	}
}

// TestAModelSettleOverANoticeMakesTheBlockAnswerProseAgain pins that the SETTLED WHOLE decides
// authorship: a block first settled as a notice and re-settled as the model's
// prose (the other store plane restating it) is answer prose again.
func TestAModelSettleOverANoticeMakesTheBlockAnswerProseAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", apiErrorNotice(), nil), nil, nil)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	if fault := h.standingAnswerFault(); fault == nil || fault.Evidence["unit"] != "unit-1" {
		t.Fatalf("standing fault = %v, want one about unit-1", fault)
	}
}

// TestAContextCutDirectiveTurnRaisesNothing pins the other exclusion: /clear
// draws no answering bubble at all, so it never had an answer to lose.
func TestAContextCutDirectiveTurnRaisesNothing(t *testing.T) {
	// Arrange: a /clear turn whose response frame is suppressed.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "/clear")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "(no content)"},
		}, nil), nil, nil)

	// Act.
	h.concludeWithoutAnswer("turn-1")

	// Assert.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a /clear turn raised a final-answer fault: %v", fault.Evidence)
	}
}

// TestAStandingFinalAnswerFaultIsRetractedWhenTheNextTurnStarts pins the
// lifetime the owner ruled: it stands until the next turn STARTS.
func TestAStandingFinalAnswerFaultIsRetractedWhenTheNextTurnStarts(t *testing.T) {
	// Arrange: a turn that did not land its answer.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)
	h.concludeWithoutAnswer("turn-1")
	if h.standingAnswerFault() == nil {
		t.Fatal("arrange: no fault stands to retract")
	}

	// Act: the next turn opens.
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Assert.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("the fault survived the next turn starting: %v", fault.Evidence)
	}
}

// AN OPENED TURN IS THE TURN-STARTED RECOVERY EDGE of every fault whose
// lifetime ends there (health/lifetime.go), not only the final-answer fault
// this resolver tracks.
func TestAnOpenedTurnClosesTheFaultsWhoseLifetimeEndsAtTheNextTurn(t *testing.T) {
	tests := []struct {
		name   string
		kind   string
		closes bool
	}{
		{"an abandoned conversation", health.KindConversationAbandoned, true},
		{"a failed classifier run", health.KindClassifierFailed, true},
		{"a dead shim waits for a healthy attach", health.KindShimDied, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ws := testWorkspace
			if _, err := h.faults.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: tt.kind}); err != nil {
				t.Fatalf("OpenFault: %v", err)
			}

			// Act.
			h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-1"))

			// Assert.
			if closed := len(h.faults.standing()) == 0; closed != tt.closes {
				t.Fatalf("%s closed = %v, want %v", tt.kind, closed, tt.closes)
			}
		})
	}
}

// A TURN REPLAYED FROM HISTORY PROVES NOTHING ABOUT THE CONVERSATION NOW, so
// it retires only the final-answer fault this resolver tracks.
func TestAReplayedTurnLeavesAnAbandonedConversationStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := testWorkspace
	if _, err := h.faults.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: health.KindConversationAbandoned}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act.
	h.deliverPrompt("turn-1", "do the thing")

	// Assert.
	if got := h.faults.standing(); len(got) != 1 || got[0].Kind != health.KindConversationAbandoned {
		t.Fatalf("standing = %+v, want the abandoned conversation still standing", got)
	}
}

// ---- NOT TIMELY ----

// TestAnOpenResponseStallsAtTheWindowNotBefore pins the window itself: it is
// armed for the configured stall and fires only when that window elapses.
func TestAnOpenResponseStallsAtTheWindowNotBefore(t *testing.T) {
	// Arrange: an open response fold, one frame in.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "half an ans"}, nil), nil, nil)

	// Assert: armed for the owner's window.
	armed := h.clock.live()
	if armed == nil {
		t.Fatal("an open response fold armed no stall window")
	}
	if armed.d != DefaultAnswerStall {
		t.Fatalf("stall window = %v, want %v", armed.d, DefaultAnswerStall)
	}

	// Act: the clock reaches one second short of the window.
	h.clock.advance(DefaultAnswerStall - time.Second)

	// Assert: nothing stands at eighty-nine seconds.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a fault stands a second before the window: %v", fault.Evidence)
	}

	// Act: the last second of the window passes.
	h.clock.advance(time.Second)

	// Assert: the stall stands.
	fault := h.standingAnswerFault()
	if fault == nil {
		t.Fatal("the elapsed stall window raised no fault")
	}
	if fault.Evidence["why"] != whyStalled {
		t.Fatalf("why = %q, want %q", fault.Evidence["why"], whyStalled)
	}
	if fault.Evidence["unit"] != "unit-1" || fault.Evidence["turn"] != "turn-1" {
		t.Fatalf("evidence = %v, want the stalled unit and its turn", fault.Evidence)
	}
}

// TestAStallIsClearedByWhateverFinallyArrives pins both clearings the owner
// ruled: a frame, and the turn's terminal.
func TestAStallIsClearedByWhateverFinallyArrives(t *testing.T) {
	tests := []struct {
		name   string
		arrive func(h *harness)
	}{
		{
			name: "a further frame arrives",
			arrive: func(h *harness) {
				h.resolver.OnActivity(testWorkspace, mainAgent(),
					responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "wer"}, nil), nil, nil)
			},
		},
		{
			name: "the turn's terminal arrives",
			arrive: func(h *harness) {
				turn := ids.TurnID("turn-1")
				h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &turn,
					completedWith("unit-1"), nil, nil)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a fold that went silent long enough to stall.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.resolver.OnActivity(testWorkspace, mainAgent(),
				responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "half an ans"}, nil), nil, nil)
			h.clock.elapse()
			if h.standingAnswerFault() == nil {
				t.Fatal("arrange: the stall did not stand")
			}

			// Act.
			tt.arrive(h)

			// Assert.
			if fault := h.standingAnswerFault(); fault != nil {
				t.Fatalf("the stall survived what finally arrived: %v", fault.Evidence)
			}
		})
	}
}

// TestASettledResponseArmsNoStallWindow pins that a fold owed nothing more is
// not watched: only an OPEN fold can go silent.
func TestASettledResponseArmsNoStallWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), nil, nil)

	// Assert.
	if armed := h.clock.live(); armed != nil {
		t.Fatal("a settled response fold left a stall window armed")
	}
}

// TestAStallWindowThatLostItsRaceRaisesNothing pins the guard on the clock's
// own goroutine: a window that fired while a frame disarmed it must not report
// a fold that is moving as silent.
func TestAStallWindowThatLostItsRaceRaisesNothing(t *testing.T) {
	// Arrange: a window armed, then captured before a frame disarms it.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "half an ans"}, nil), nil, nil)
	stale := h.clock.live()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "wer"}, nil), nil, nil)

	// Act: the superseded window fires anyway.
	stale.f()

	// Assert.
	if fault := h.standingAnswerFault(); fault != nil {
		t.Fatalf("a superseded stall window raised a fault: %v", fault.Evidence)
	}
}

// TestTheFooterLineIsTerseAndSaysWhichWayTheAnswerWasLost pins the chip's line:
// one short phrase per `why`, so the strip's one elastic cell can carry it.
func TestTheFooterLineIsTerseAndSaysWhichWayTheAnswerWasLost(t *testing.T) {
	tests := []struct {
		name string
		why  string
		want string
	}{
		{"the terminal named nothing", whyNoAnswerNamed, "the turn named no answering response"},
		{"the named answer resolves to nothing", whyAnswerRowUnresolved, "the named answer has no drawn row"},
		{"an open response went silent", whyStalled, "no response frame for 1m30s"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			got := h.resolver.answerFaultLine(tt.why)

			// Assert.
			if got != tt.want {
				t.Fatalf("answerFaultLine(%q) = %q, want %q", tt.why, got, tt.want)
			}
		})
	}
}

// TestASiblingFoldMovingDoesNotAnswerAnotherFoldsStall pins the scoping: the
// stall is about ONE fold, and a different response block of the same turn
// paying out says nothing about the one that went silent.
func TestASiblingFoldMovingDoesNotAnswerAnotherFoldsStall(t *testing.T) {
	// Arrange: unit-1 stalls.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "half an ans"}, nil), nil, nil)
	h.clock.elapse()
	if h.standingAnswerFault() == nil {
		t.Fatal("arrange: the stall did not stand")
	}

	// Act: a DIFFERENT block of the same turn pays out.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-2", &conversationv1.AgentResponseUpdate{NewMarkdown: "a second block"}, nil), nil, nil)

	// Assert: unit-1's stall still stands.
	fault := h.standingAnswerFault()
	if fault == nil {
		t.Fatal("a sibling fold's frame retracted another fold's stall")
	}
	if fault.Evidence["unit"] != "unit-1" {
		t.Fatalf("unit = %q, want the stalled fold's", fault.Evidence["unit"])
	}
}
