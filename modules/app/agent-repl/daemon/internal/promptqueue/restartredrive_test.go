package promptqueue

import (
	"context"
	"errors"
	"slices"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// sentOrigins answers every origin the fake shim was asked to start a turn with.
func (s *fakeSender) sentOrigins() []conversationv1.PromptOrigin {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]conversationv1.PromptOrigin(nil), s.origins...)
}

// sentText answers the text of the nth started turn.
func (s *fakeSender) sentText(n int) string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return saidText(s.said[n])
}

// owe records a resumption the workspace is owed, as a stand-down would.
func owe(t *testing.T, h *harness) {
	t.Helper()
	if err := h.db.OweResumption(context.Background(), theWorkspace, "cut-turn", instant); err != nil {
		t.Fatalf("OweResumption: %v", err)
	}
}

func TestATurnTheStandDownCutIsOwedAResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 1 {
		t.Fatalf("owed = %d, want the cut turn owed once", got)
	}
}

func TestAUserInterruptOwesNoResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d, want nothing owed for an interrupt with no stand-down", got)
	}
}

func TestATurnThatEndedOnItsOwnDuringAStandDownOwesNoResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d, want nothing owed for a turn that completed", got)
	}
}

func TestAStandDownDeliversNothingHeldBehindTheCutTurn(t *testing.T) {
	// Arrange: a prompt queued behind the running turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("queued", "the next question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	for _, turn := range h.sender.started() {
		if turn == "queued" {
			t.Fatal("the queued prompt was started into a shim the stand-down is taking down")
		}
	}
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 1 || standing[0].Turn != "queued" {
		t.Fatalf("held = %+v, %v; want the queued prompt still held", standing, err)
	}
}

func TestTheServingSessionResumesWhatTheStandDownOwed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	origins := h.sender.sentOrigins()
	if len(origins) != 1 || origins[0] != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("started origins = %v, want one resumption after restart", origins)
	}
	if got := h.sender.sentText(0); got != resumptionInstruction {
		t.Fatalf("resumption text = %q, want the established instruction", got)
	}
}

func TestTheResumptionRunsAsTheTurnItsClaimNamed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	started := h.sender.started()
	if len(h.db.resumedAs) != 1 || len(started) != 1 || started[0] != h.db.resumedAs[0] {
		t.Fatalf("claimed as %v, started %v; want the resumption run as the turn its claim recorded the cut turns moving to", h.db.resumedAs, started)
	}
}

func TestTheResumptionPaysTheDebt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d after the resumption, want 0", got)
	}
}

func TestASecondSessionUpResumesNothingTwice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)
	h.q.ReleaseReconnectHolds(theWorkspace)
	h.watcher.idle()

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := len(h.sender.started()); got != 1 {
		t.Fatalf("started = %d turns, want the one resumption", got)
	}
}

func TestTheResumptionDrawsNoPromptRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if rows := h.feed.mirrored(); len(rows) != 0 {
		t.Fatalf("mirrored rows = %d, want none: the resumption is under the hood", len(rows))
	}
}

func TestAServingSessionWithATurnRunningLeavesTheDebtOwed(t *testing.T) {
	// Arrange: an adopted shim whose turn survived.
	h := newHarness(t)
	owe(t, h)
	h.watcher.running("adopted-turn")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 1 || len(h.sender.started()) != 0 {
		t.Fatalf("owed = %d, started = %v; want the debt left for a free session", got, h.sender.started())
	}
}

// cutByARestart runs TURN, cuts it under a stand-down as a restart does, and
// brings the restarted session up, which starts the resumption it owed.
func cutByARestart(t *testing.T, h *harness, turn string) {
	t.Helper()
	h.standingDown.Store(true)
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, idsTurn(turn), wsm.CloseKilled)
	h.standingDown.Store(false)
	h.q.ReleaseReconnectHolds(theWorkspace)
}

func TestARestartContinuesTheCutTurnWithoutResubmittingIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "cut-turn", "long running work")

	// Act
	cutByARestart(t, h, "cut-turn")

	// Assert
	origins := h.sender.sentOrigins()
	if len(origins) != 1 || origins[0] != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("started origins = %v, want the one resumption after restart", origins)
	}
	if got := h.sender.sentText(0); got == "long running work" {
		t.Fatalf("the restarted session was sent %q, want the resumption, never the cut prompt again", got)
	}
}

func TestTheResumptionsOwnEndStartsNoOtherTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "cut-turn", "long running work")
	cutByARestart(t, h, "cut-turn")
	resumed := h.sender.started()

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, resumed[0], wsm.CloseCompleted)

	// Assert
	if started := h.sender.started(); len(started) != 1 || h.sender.attempts != 1 {
		t.Fatalf("started = %v over %d attempts, want the one resumption", started, h.sender.attempts)
	}
}

func TestADeferredPromptRunsAsItsOwnTurnAfterTheRestartsResumption(t *testing.T) {
	// Arrange: a prompt deferred behind the turn the restart cuts.
	h := newHarness(t)
	running(t, h, "cut-turn", "start the work")
	if _, err := h.q.Submit(context.Background(), deferredSubmission("t1", "run the tests afterwards")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	cutByARestart(t, h, "cut-turn")
	atRelease := h.sender.started()
	originsAtRelease := h.sender.sentOrigins()

	// Act: the resumption ends.
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, atRelease[0], wsm.CloseCompleted)

	// Assert
	if len(originsAtRelease) != 1 || originsAtRelease[0] != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("origins at the release = %v, want only the resumption", originsAtRelease)
	}
	if started := h.sender.started(); !slices.Equal(started, []ids.TurnID{atRelease[0], "t1"}) {
		t.Fatalf("started = %v, want the resumption then the deferred prompt once, under its own id", started)
	}
}

// resumeReports collects every account the queue gives of an owed resumption.
func resumeReports(h *harness) func() []ResumeReport {
	var mu sync.Mutex
	var got []ResumeReport
	h.q.deps.Resumptions = func(ws ids.WorkspaceID, report ResumeReport) {
		mu.Lock()
		defer mu.Unlock()
		got = append(got, report)
	}
	return func() []ResumeReport {
		mu.Lock()
		defer mu.Unlock()
		return append([]ResumeReport(nil), got...)
	}
}

// resumeKinds names the kinds of REPORTS in order.
func resumeKinds(reports []ResumeReport) []ResumeKind {
	out := make([]ResumeKind, 0, len(reports))
	for _, r := range reports {
		out = append(out, r.Kind)
	}
	return out
}

func TestAResumedTurnIsReportedStartingThenStarted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeStarting, ResumeStarted}) {
		t.Fatalf("reports = %v, want starting then started", got)
	}
}

func TestAReleaseWithNothingOwedIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestADebtLeftForARunningTurnIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.watcher.running("adopted-turn")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestAResumptionThatCannotBeClaimedIsReportedFailed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.db.resumeClaimErr = errors.New("database is locked")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	got := reports()
	if len(got) != 1 || got[0].Kind != ResumeFailed || !strings.Contains(got[0].Text, "database is locked") {
		t.Fatalf("reports = %+v, want one failure naming the cause", got)
	}
}

func TestAReleaseTheVendorStillRefusesIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.footer.standVendorBlock("rate_limited")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestAReleaseIntoASessionThatDoesNotServeIsReportedNotNow(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	reports := resumeReports(h)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

// owingRetry makes the harness's shim state a vendor-error retry still to be
// made.
func owingRetry(h *harness) {
	h.watcher.mu.Lock()
	defer h.watcher.mu.Unlock()
	h.watcher.retryOwed = true
}

const (
	retryOwedMessage    = "the departure cut the shim's owed vendor-error retry; the next session that serves the workspace resumes the failed turn"
	retryEndedMessage   = "the session the user ended owed a vendor-error retry; it ends with the session and is not resumed"
	retryOweFailMessage = "could not record the resumption a cut vendor-error retry owes; the failed turn will not be retried"
)

func TestADepartureThatCutsAnOwedRetryOwesTheWorkspaceItsResumption(t *testing.T) {
	tests := []struct {
		name         string
		departure    sessionwatcher.Departure
		standingDown bool
		bounced      bool
		owing        bool
		wantOwed     int
		wantMessage  string
	}{
		{
			name:        "a shim that died owing a retry owes its resumption",
			departure:   departedUnasked,
			owing:       true,
			wantOwed:    1,
			wantMessage: retryOwedMessage,
		},
		{
			name:         "the daemon's own stand-down owing a retry owes its resumption",
			departure:    departedOrdered,
			standingDown: true,
			owing:        true,
			wantOwed:     1,
			wantMessage:  retryOwedMessage,
		},
		{
			name:        "a bounce forced over an owed retry owes its resumption",
			departure:   departedOrdered,
			bounced:     true,
			owing:       true,
			wantOwed:    1,
			wantMessage: retryOwedMessage,
		},
		{
			name:        "a session the user ended owes nothing for its retry",
			departure:   departedOrdered,
			owing:       true,
			wantOwed:    0,
			wantMessage: retryEndedMessage,
		},
		{
			name:      "a shim that died owing no retry owes nothing",
			departure: departedUnasked,
			wantOwed:  0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tc.bounced {
				registerBehindWork(t, h, newGate().request("a forced deploy", false))
			}
			h.standingDown.Store(tc.standingDown)
			if tc.owing {
				owingRetry(h)
			}

			// Act
			depart(h, h.watcher, tc.departure)

			// Assert
			if got := h.db.oweCalls(); got != tc.wantOwed {
				t.Fatalf("debts recorded = %d, want %d", got, tc.wantOwed)
			}
			if tc.wantMessage != "" && !logged(h.log.Records(), "info", opResume, tc.wantMessage) {
				t.Fatalf("records = %+v, want %q at info", h.log.Records(), tc.wantMessage)
			}
		})
	}
}

func TestAnOwedRetryWhoseDebtCannotBeRecordedIsLoggedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owingRetry(h)
	h.db.mu.Lock()
	h.db.oweErr = errors.New("disk full")
	h.db.mu.Unlock()

	// Act
	depart(h, h.watcher, departedUnasked)

	// Assert
	if !logged(h.log.Records(), "error", opResume, retryOweFailMessage) {
		t.Fatalf("records = %+v, want %q at error", h.log.Records(), retryOweFailMessage)
	}
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d, want no debt left by the failed write", got)
	}
}

// standCall plants a StartTurn standing for TURN on the queue, as a delivery
// in its shim call has, and answers the channel that settles it.
func standCall(h *harness, turn ids.TurnID) chan struct{} {
	settled := make(chan struct{})
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).call = &shimCall{what: "start_turn", turn: turn, opensTurn: true, settled: settled}
	h.q.mu.Unlock()
	return settled
}

// settleCall closes SETTLED, as the call's return does. The planted call is
// left standing, so a reader that looks for it finds it whenever it looks.
func settleCall(settled chan struct{}) {
	close(settled)
}

// holdForReconnectAs puts TURN's prompt under the reconnect hold, as a
// delivery the shim died under leaves it.
func holdForReconnectAs(t *testing.T, h *harness, turn ids.TurnID) {
	t.Helper()
	kind := wsm.HoldReconnect
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{Turn: turn, Workspace: theWorkspace, Hold: &kind}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
}

func TestRedeliversCutTurn(t *testing.T) {
	tests := []struct {
		name string
		call ids.TurnID
		held bool
		want bool
	}{
		{name: "a turn whose own StartTurn stands and whose prompt is held is redelivered", call: "cut-turn", held: true, want: true},
		{name: "a turn whose own StartTurn stands but whose prompt is not held is not", call: "cut-turn", held: false, want: false},
		{name: "a turn whose StartTurn does not stand is not", call: "", held: true, want: false},
		{name: "a turn another turn's StartTurn stands beside is not", call: "other-turn", held: true, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tc.held {
				holdForReconnectAs(t, h, "cut-turn")
			}
			var settled chan struct{}
			if tc.call != "" {
				settled = standCall(h, tc.call)
			}
			got := make(chan bool, 1)

			// Act
			go func() {
				redelivered, err := h.q.redeliversCutTurn(context.Background(), theWorkspace, "cut-turn")
				if err != nil {
					t.Errorf("redeliversCutTurn: %v", err)
				}
				got <- redelivered
			}()
			if settled != nil {
				settleCall(settled)
			}

			// Assert
			if g := <-got; g != tc.want {
				t.Fatalf("redeliversCutTurn = %v, want %v", g, tc.want)
			}
		})
	}
}

func TestRedeliversCutTurnFailsWhenItsCallOutlivesTheContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	standCall(h, "cut-turn")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := h.q.redeliversCutTurn(ctx, theWorkspace, "cut-turn")

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("redeliversCutTurn = %v, want the context's cancellation", err)
	}
}

// A SHIM THAT DIES UNDER THE TURN'S OWN StartTurn cut a turn it never
// accepted: its prompt is held and delivered again as itself, so the revived
// session is owed no "continue" prompt as well.
func TestOnTurnEndedForADeathUnderTheTurnsOwnStartTurnOwesNoResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "cut-turn", "the prompt")
	holdForReconnectAs(t, h, "cut-turn")
	// The call stands as the death's close arrives, and has already settled:
	// the wait for it returns at once.
	settled := make(chan struct{})
	close(settled)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).call = &shimCall{what: "start_turn", turn: "cut-turn", opensTurn: true, settled: settled}
	h.q.mu.Unlock()
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "cut-turn", wsm.CloseAgentDied)

	// Assert
	if got := h.db.owed[theWorkspace]["cut-turn"]; got {
		t.Fatal("the cut turn was owed a resumption beside its held prompt")
	}
	if got := h.db.closedTurns["cut-turn"]; got != wsm.CloseKilled {
		t.Fatalf("durable close = %s, want %s", got, wsm.CloseKilled)
	}
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opResume && strings.Contains(r.Message, "held and delivered again as itself") {
			return
		}
	}
	t.Fatalf("the decision was not recorded: %v", h.log.Records())
}

func TestOnTurnEndedForADeathWhoseHoldCannotBeReadKeepsTheFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "cut-turn", "the prompt")
	settled := make(chan struct{})
	close(settled)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).call = &shimCall{what: "start_turn", turn: "cut-turn", opensTurn: true, settled: settled}
	h.q.mu.Unlock()
	h.db.mu.Lock()
	h.db.byTurnErr = errors.New("the store is gone")
	h.db.mu.Unlock()
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "cut-turn", wsm.CloseAgentDied)

	// Assert
	if got := h.db.owed[theWorkspace]["cut-turn"]; got {
		t.Fatal("a turn whose hold could not be read was owed a resumption")
	}
	if got := h.db.closedTurns["cut-turn"]; got != wsm.CloseAgentDied {
		t.Fatalf("durable close = %s, want its failure %s", got, wsm.CloseAgentDied)
	}
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == opResume && r.Context["cause"] != nil &&
			strings.Contains(r.Context["cause"].(string), "the store is gone") {
			return
		}
	}
	t.Fatalf("the unreadable hold was not recorded at error with its cause: %v", h.log.Records())
}
