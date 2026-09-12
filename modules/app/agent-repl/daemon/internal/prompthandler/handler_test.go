package prompthandler

import (
	"context"
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
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
