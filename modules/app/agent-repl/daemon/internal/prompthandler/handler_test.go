package prompthandler

import (
	"context"
	"errors"
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

func TestNewRefusesEachMissingCollaborator(t *testing.T) {
	full := func() Deps {
		return Deps{
			DB: newFakeDB(), Queue: &fakeQueue{}, Feed: &fakeFeed{},
			Panels: func(context.Context, ids.WorkspaceID, conversationv1.SessionCommand) (*agentreplv1.SubmitPromptCommandPanel, error) {
				return nil, nil
			},
			Log: dlog.NewTestSurfaces(),
		}
	}
	tests := []struct {
		name  string
		blank func(*Deps)
	}{
		{"log surfaces", func(d *Deps) { d.Log = nil }},
		{"state client", func(d *Deps) { d.DB = nil }},
		{"prompt queue", func(d *Deps) { d.Queue = nil }},
		{"feed resolver", func(d *Deps) { d.Feed = nil }},
		{"panel source", func(d *Deps) { d.Panels = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.blank(&deps)
			// Act
			_, err := New(deps)
			// Assert
			if err == nil {
				t.Fatalf("New with no %s must refuse", tc.name)
			}
		})
	}
}

func TestSubmitRefusesAnUnspecifiedOrigin(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := h.h.Submit(context.Background(), theWorkspace, userSaid("hello"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED, nil)
	// Assert
	if !errors.Is(err, ErrOriginRequired) {
		t.Fatalf("err = %v, want ErrOriginRequired", err)
	}
}

func TestSubmitMintsNothingWhenTheOriginIsMissing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.h.Submit(context.Background(), theWorkspace, userSaid("hello"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED, nil); err == nil {
		t.Fatal("the submission must be refused")
	}
	// Assert
	if len(h.queue.forwarded()) != 0 || len(h.db.claimed) != 0 {
		t.Fatal("nothing may be minted, claimed or forwarded before the origin is checked")
	}
}

func TestSubmitRefusesAFeedFromAnotherWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := h.h.Submit(context.Background(), theWorkspace, userSaid("keep going"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, bubbleRef("another-workspace"))
	// Assert
	if !errors.Is(err, ErrFeedNotInWorkspace) {
		t.Fatalf("err = %v, want ErrFeedNotInWorkspace", err)
	}
}

func TestSubmitForwardsABubbleAddressedPromptFromItsOwnWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.h.Submit(context.Background(), theWorkspace, userSaid("keep going"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, bubbleRef(theWorkspace)); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	forwarded := h.queue.forwarded()
	if len(forwarded) != 1 || forwarded[0].Target == nil {
		t.Fatalf("forwarded = %v, want the addressed target carried through", forwarded)
	}
}

func TestSubmitForwardsAnOrdinaryPromptWithTheMintedTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("please fix the failing test")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Recognition != RecognizedNone || got.Turn != h.minted {
		t.Fatalf("outcome = %+v, want the minted turn on an ordinary prompt", got)
	}
	if forwarded := h.queue.forwarded(); len(forwarded) != 1 || forwarded[0].Turn != h.minted {
		t.Fatalf("forwarded = %v, want the minted turn", forwarded)
	}
}

func TestSubmitCarriesTheOriginIntoTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.h.Submit(context.Background(), theWorkspace, userSaid("hello"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, nil); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if got := h.queue.forwarded()[0].Origin; got != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR {
		t.Fatalf("origin = %s, want the submission's own", got)
	}
}

func TestSubmitAnswersWithTheQueuesDisposition(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.queue.disposition = promptqueue.Disposition{Delivered: true}
	// Act
	got, err := h.submit("hello")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Disposition.Delivered {
		t.Fatalf("disposition = %+v, want the queue's answer", got.Disposition)
	}
}

func TestSubmitRefusesADuplicateIdempotencyKey(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if _, err := h.submit("hello"); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act: the same key again, as a retried request would.
	_, err := h.submit("hello")
	// Assert
	if !errors.Is(err, ErrDuplicateSubmission) {
		t.Fatalf("err = %v, want ErrDuplicateSubmission", err)
	}
	if len(h.queue.forwarded()) != 1 {
		t.Fatal("a retried request must not become a second turn")
	}
}

func TestADuplicateIsRecordedAtItsLevelByWhetherTheSubmissionIsARedrive(t *testing.T) {
	tests := []struct {
		name      string
		ctx       func(context.Context) context.Context
		wantLevel string
	}{
		{name: "an ordinary submission", ctx: func(ctx context.Context) context.Context { return ctx }, wantLevel: "warn"},
		{name: "a re-drive of an earlier attempt", ctx: WithRedrive, wantLevel: "info"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if _, err := h.submit("hello"); err != nil {
				t.Fatalf("Submit: %v", err)
			}

			// Act
			_, err := h.h.Submit(tc.ctx(context.Background()), theWorkspace, userSaid("hello"), "key-1",
				conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil)

			// Assert
			if !errors.Is(err, ErrDuplicateSubmission) {
				t.Fatalf("err = %v, want ErrDuplicateSubmission", err)
			}
			var levels []string
			for _, r := range h.log.Records() {
				if r.Operation == opSubmit && (r.Level == "warn" || r.Level == "info") && r.Context["existing_turn"] != nil {
					levels = append(levels, r.Level)
				}
			}
			if len(levels) != 1 || levels[0] != tc.wantLevel {
				t.Fatalf("duplicate records = %v, want exactly one at %s", levels, tc.wantLevel)
			}
		})
	}
}

func TestSubmitForwardsWithoutAKeyWhenNoneWasSupplied(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := h.h.Submit(context.Background(), theWorkspace, userSaid("hello"), "",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil)
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if len(h.queue.forwarded()) != 1 {
		t.Fatal("a keyless submission is still forwarded")
	}
}

func TestSubmitSurfacesAFailedIdempotencyClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.claimErr = errors.New("the database is read-only")
	// Act
	_, err := h.submit("hello")
	// Assert
	if err == nil {
		t.Fatal("a failed claim must be surfaced, never swallowed")
	}
	if len(h.queue.forwarded()) != 0 {
		t.Fatal("nothing may be forwarded when the key could not be claimed")
	}
}

func TestSubmitAnswersAPanelCommandInline(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("/status")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Recognition != RecognizedPanel || got.Panel == nil {
		t.Fatalf("outcome = %+v, want an inline panel", got)
	}
	if len(h.queue.forwarded()) != 0 {
		t.Fatal("a recognized panel is never forwarded")
	}
}

func TestSubmitMirrorsAPanelIntoTheRootFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.submit("/status"); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	panels := h.feed.mirroredPanels()
	if len(panels) != 1 {
		t.Fatalf("mirrored panels = %d, want 1", len(panels))
	}
	if _, ok := panels[0].GetPanel().(*frontendv1.FeedCommandPanel_Status); !ok {
		t.Fatalf("panel arm = %T, want the status arm", panels[0].GetPanel())
	}
}

func TestSubmitAsksThePanelSourceForTheRecognizedCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.submit("/todos"); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	asked := h.panelsAsked()
	if len(asked) != 1 || asked[0] != conversationv1.SessionCommand_SESSION_COMMAND_TODOS {
		t.Fatalf("asked = %v, want the recognized command", asked)
	}
}

func TestSubmitSurfacesAPanelSourceFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.panelErr = errPanel
	// Act
	_, err := h.submit("/status")
	// Assert
	if !errors.Is(err, errPanel) {
		t.Fatalf("err = %v, want the panel source's failure", err)
	}
}

func TestSubmitRefusesWhenThePanelSourceAnswersNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.panel = nil
	// Act
	_, err := h.submit("/mcp")
	// Assert
	if err == nil {
		t.Fatal("an absent panel must be surfaced rather than answered as empty")
	}
}

func TestSubmitRefusesARecognizedButUnsupportedCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("/agents")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Recognition != RecognizedRefused || got.RefusedCommand != "/agents" {
		t.Fatalf("outcome = %+v, want the refusal and the literal", got)
	}
	if len(h.queue.forwarded()) != 0 {
		t.Fatal("/agents is never forwarded to the shim")
	}
}

func TestSubmitMirrorsARefusalWithTheAddSupportOffer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.submit("/help"); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	refusals := h.feed.mirroredRefusals()
	if len(refusals) != 1 {
		t.Fatalf("mirrored refusals = %d, want 1", len(refusals))
	}
	if !refusals[0].AddSupport {
		t.Fatal("a refusal card carries the add-support offer")
	}
	if refusals[0].Reason != RefusalReason("/help") {
		t.Fatalf("reason = %q, want the daemon's composed sentence", refusals[0].Reason)
	}
}

func TestSubmitMintsNoTurnForARefusedCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("/agents")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Turn != "" {
		t.Fatalf("turn = %q, want none: the caller learns only that no turn was minted", got.Turn)
	}
}

func TestSubmitSendsAContextCutDownTheQueuesPath(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("/clear")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Recognition != RecognizedAct || got.Turn != h.minted {
		t.Fatalf("outcome = %+v, want a session act carrying the minted turn", got)
	}
	acts := h.queue.sessionActs()
	if len(acts) != 1 || acts[0].Kind != promptqueue.ActClear || acts[0].Turn != h.minted {
		t.Fatalf("acts = %v, want the clear act with its turn", acts)
	}
}

// TestSubmitNeverForwardsAContextCutAsAPrompt pins the handler's half of the
// owner's ruling: /compact, /compact <text> and /clear never reach the queue's
// Submit, whose running-turn path is the one that asks the routing classifier.
func TestSubmitNeverForwardsAContextCutAsAPrompt(t *testing.T) {
	tests := []struct {
		name string
		text string
		kind string
	}{
		{name: "a bare /compact", text: "/compact", kind: promptqueue.ActCompact},
		{name: "/compact with instructions", text: "/compact foo bar", kind: promptqueue.ActCompact},
		{name: "a bare /clear", text: "/clear", kind: promptqueue.ActClear},
		{name: "/clear with trailing text", text: "/clear foo", kind: promptqueue.ActClear},
		{name: "the /reset alias", text: "/reset", kind: promptqueue.ActClear},
		{name: "the /new alias", text: "/new", kind: promptqueue.ActClear},
		{name: "the /reset alias with trailing text", text: "/reset foo", kind: promptqueue.ActClear},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			if _, err := h.submit(tt.text); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			// Assert
			if got := h.queue.forwarded(); len(got) != 0 {
				t.Fatalf("forwarded = %v, want nothing on the prompt path", got)
			}
			if acts := h.queue.sessionActs(); len(acts) != 1 || acts[0].Kind != tt.kind {
				t.Fatalf("acts = %v, want the %s act", acts, tt.kind)
			}
		})
	}
}

func TestSubmitSendsAModelChangeDownTheQueuesPathWithNoTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	got, err := h.submit("/model opus")
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Turn != "" {
		t.Fatalf("turn = %q, want none: a model change is not a turn", got.Turn)
	}
	acts := h.queue.sessionActs()
	if len(acts) != 1 || acts[0].Kind != promptqueue.ActSetModel || acts[0].Value != "opus" {
		t.Fatalf("acts = %v, want the model act with its argument", acts)
	}
}

func TestSubmitCarriesTheOriginOntoAContextCut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.h.Submit(context.Background(), theWorkspace, userSaid("/compact"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT, nil); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if got := h.queue.sessionActs()[0].Origin; got != conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT {
		t.Fatalf("origin = %s, want the submission's own", got)
	}
}

func TestSubmitSurfacesARefusedSessionAct(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.queue.actErr = errors.New("the workspace has no session")
	// Act
	_, err := h.submit("/clear")
	// Assert
	if err == nil {
		t.Fatal("a refused session act must be surfaced")
	}
}

func TestSubmitSurfacesTheQueuesRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.queue.submitErr = promptqueue.ErrMerging
	// Act
	_, err := h.submit("hello")
	// Assert
	if !errors.Is(err, promptqueue.ErrMerging) {
		t.Fatalf("err = %v, want the queue's refusal carried through", err)
	}
}

func TestSubmitSurfacesAnUnresolvableWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.workspaceErr = errors.New("no such workspace")
	// Act
	_, err := h.submit("hello")
	// Assert
	if err == nil {
		t.Fatal("failing to resolve the workspace is an invariant violation, never a global write")
	}
}

// unroutableSurfaces is dlog.Surfaces whose per-workspace sink never opens, so
// a test can drive the one condition production hits when a workspace's
// directory is a scratch path or a deleted worktree.
type unroutableSurfaces struct {
	*dlog.TestSurfaces
}

func (unroutableSurfaces) Workspace(string) (dlog.Logger, error) {
	return nil, errors.New("the workspace owns no durable log sink")
}

func (s unroutableSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	return s.TestSurfaces.Global().With(dlog.Context{dlog.KeyUnroutableWorkspace: dir})
}

// TestSubmitSurvivesAWorkspaceThatOwnsNoLogSink pins that A PROMPT IS NEVER
// LOST OVER ITS OWN LOGGING. Resolving a named workspace's sink is a TOTAL
// function, so the submission goes through and its records go centrally.
func TestSubmitSurvivesAWorkspaceThatOwnsNoLogSink(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.h.deps.Log = unroutableSurfaces{dlog.NewTestSurfaces()}

	// Act.
	_, err := h.submit("hello")

	// Assert.
	if err != nil {
		t.Fatalf("Submit() = %v, want the prompt delivered despite the unroutable sink", err)
	}
}

// firstTurn is the turn the harness mints for a key's first submission.
const firstTurn ids.TurnID = "minted-turn"

// retryTurn is the turn a retry mints, distinct from the first submission's.
const retryTurn ids.TurnID = "retry-turn"

// deliveries counts everything the handler forwarded down the queue's path.
func (h *harness) deliveries() int {
	return len(h.queue.forwarded()) + len(h.queue.sessionActs())
}

// deliveredTurns names the turn of everything the handler forwarded down the
// queue's path, prompts first.
func (h *harness) deliveredTurns() []ids.TurnID {
	var out []ids.TurnID
	for _, sub := range h.queue.forwarded() {
		out = append(out, sub.Turn)
	}
	for _, act := range h.queue.sessionActs() {
		out = append(out, act.Turn)
	}
	return out
}

// TestSubmitDeliversARetryOfASubmissionTheQueueNeverAccepted reproduces the
// 2026-09-27 prompt loss: the key was claimed, the queue never accepted the
// submission, and the retry under the SAME key must be delivered -- once --
// rather than refused as a duplicate of a turn nobody delivered. It is driven
// under the FIRST submission's turn, never the one the retry minted, so a
// shim that did accept that turn answers the repeat as a no-op.
func TestSubmitDeliversARetryOfASubmissionTheQueueNeverAccepted(t *testing.T) {
	tests := []struct {
		name    string
		text    string
		arrange func(t *testing.T, h *harness)
	}{
		{
			name: "the process died between the claim and the acceptance",
			text: "hello",
			arrange: func(t *testing.T, h *harness) {
				h.db.claimed["key-1"] = &fakeClaim{turn: firstTurn}
				h.respawn(t)
			},
		},
		{
			name: "the queue refused the first submission",
			text: "hello",
			arrange: func(t *testing.T, h *harness) {
				h.queue.submitErr = errors.New("the queue is wedged")
				if _, err := h.submit("hello"); err == nil {
					t.Fatal("arrange: the refused submission succeeded")
				}
				h.queue.submitErr = nil
			},
		},
		{
			name: "the first submission's caller gave up mid-submit",
			text: "hello",
			arrange: func(t *testing.T, h *harness) {
				h.queue.submitErr = context.DeadlineExceeded
				if _, err := h.submit("hello"); err == nil {
					t.Fatal("arrange: the abandoned submission succeeded")
				}
				h.queue.submitErr = nil
			},
		},
		{
			name: "the queue refused the first context cut",
			text: "/clear",
			arrange: func(t *testing.T, h *harness) {
				h.queue.actErr = errors.New("the workspace has no session")
				if _, err := h.submit("/clear"); err == nil {
					t.Fatal("arrange: the refused context cut succeeded")
				}
				h.queue.actErr = nil
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)
			h.minted = retryTurn

			// Act
			got, err := h.submit(tc.text)

			// Assert
			if err != nil {
				t.Fatalf("Submit: %v, want the retry delivered", err)
			}
			if got.Turn != firstTurn {
				t.Fatalf("turn = %q, want the first submission's %q", got.Turn, firstTurn)
			}
			if turns := h.deliveredTurns(); len(turns) != 1 || turns[0] != firstTurn {
				t.Fatalf("delivered turns = %v, want exactly one, under %q", turns, firstTurn)
			}
			if claim := h.db.claimOn("key-1"); !claim.accepted || claim.turn != firstTurn {
				t.Fatalf("claim = %+v, want the first submission's turn stamped accepted", claim)
			}
		})
	}
}

// TestSubmitRefusesARetryOfAnAcceptedContextCut pins the act path's duplicate:
// a context cut the queue accepted refuses its retry.
func TestSubmitRefusesARetryOfAnAcceptedContextCut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if _, err := h.submit("/clear"); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	_, err := h.submit("/clear")

	// Assert
	if !errors.Is(err, ErrDuplicateSubmission) {
		t.Fatalf("err = %v, want ErrDuplicateSubmission", err)
	}
	if n := h.deliveries(); n != 1 {
		t.Fatalf("%d deliveries, want exactly one", n)
	}
}

// TestSubmitKeepsARetryOutWhileItsOriginalIsInFlight pins the no-double-
// delivery half: while the original is still blocked inside the queue, its
// retry is neither re-driven (a second delivery once the original unblocks)
// nor refused as a duplicate (the original has not been accepted). It waits,
// and here its caller gives up first.
func TestSubmitKeepsARetryOutWhileItsOriginalIsInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.queue.entered = make(chan struct{}, 1)
	h.queue.gate = make(chan struct{})
	var original sync.WaitGroup
	original.Add(1)
	go func() {
		defer original.Done()
		if _, err := h.submit("hello"); err != nil {
			t.Errorf("the original Submit: %v", err)
		}
	}()
	<-h.queue.entered
	h.queue.entered = nil
	gaveUp, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := h.h.Submit(gaveUp, theWorkspace, userSaid("hello"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil)

	// Assert
	close(h.queue.gate)
	original.Wait()
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("err = %v, want the retry's own cancellation", err)
	}
	if n := h.deliveries(); n != 1 {
		t.Fatalf("%d deliveries, want the original's alone", n)
	}
}

// TestSubmitSurfacesAFailedAcceptanceStamp pins that a claim the queue accepted
// but that could not be recorded accepted is surfaced, never swallowed.
func TestSubmitSurfacesAFailedAcceptanceStamp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.acceptErr = errors.New("the database is read-only")

	// Act
	_, err := h.submit("hello")

	// Assert
	if !errors.Is(err, h.db.acceptErr) {
		t.Fatalf("err = %v, want the failed stamp surfaced", err)
	}
	if !loggedAt(h, opSubmit, "error") {
		t.Fatalf("the failed stamp was not recorded at error")
	}
}

// TestSubmitRefusesAClaimStandingItDoesNotKnow pins the handler's backstop
// against a standing wsm adds without the handler learning it.
func TestSubmitRefusesAClaimStandingItDoesNotKnow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	unknown := wsm.ClaimStanding(99)
	h.db.standing = &unknown

	// Act
	_, err := h.submit("hello")

	// Assert
	if err == nil {
		t.Fatal("an unknown claim standing must be refused")
	}
	if n := h.deliveries(); n != 0 {
		t.Fatalf("%d deliveries, want none", n)
	}
}

// loggedAt reports whether the handler recorded operation at level.
func loggedAt(h *harness, operation, level string) bool {
	for _, record := range h.h.deps.Log.(*dlog.TestSurfaces).Records() {
		if record.Operation == operation && record.Level == level {
			return true
		}
	}
	return false
}
