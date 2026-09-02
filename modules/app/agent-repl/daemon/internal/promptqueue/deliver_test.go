package promptqueue

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

func TestDeliverRecordsTheTurnBeforeItReachesTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	record, ok := h.db.startedTurn("t1")
	if !ok {
		t.Fatal("the turn must be recorded")
	}
	if record.Text != "hello" {
		t.Fatalf("text = %q, want the submission's text", record.Text)
	}
}

func TestDeliverPersistsTheOriginOntoTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sub := submission("t1", "hello")
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR
	// Act
	if _, err := h.q.Submit(context.Background(), sub); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	record, _ := h.db.startedTurn("t1")
	if record.Origin != sub.Origin.String() {
		t.Fatalf("origin = %q, want %q", record.Origin, sub.Origin.String())
	}
}

func TestDeliverMirrorsTheAcceptedPromptIntoTheFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	rows := h.feed.mirrored()
	if len(rows) != 1 {
		t.Fatalf("mirrored rows = %d, want 1", len(rows))
	}
	if _, ok := rows[0].GetRow().(*frontendv1.FeedRow_UserPrompt); !ok {
		t.Fatalf("row = %T, want a user_prompt row", rows[0].GetRow())
	}
}

func TestDeliverStampsTheMirrorWithTheMintedTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if got := h.feed.mirrored()[0].GetTurn().GetValue(); got != "t1" {
		t.Fatalf("turn = %q, want the minted turn", got)
	}
}

func TestDeliverStripsTheSentinelSpansFromTheMirror(t *testing.T) {
	// Arrange: the strip is the mirror's, so the drawn row loses the injected
	// span while the shim still receives the whole composition.
	h := newHarness(t)
	h.q.deps.StripSentinels = func(s string) string { return strings.ReplaceAll(s, "INJECTED", "") }
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "userINJECTED")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	drawn := h.feed.mirrored()[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0].GetText().GetText()
	if drawn != "user" {
		t.Fatalf("drawn = %q, want the sentinel span stripped", drawn)
	}
	sent := saidText(h.sender.said[0])
	if sent != "userINJECTED" {
		t.Fatalf("sent = %q, want the whole composition", sent)
	}
}

func TestDeliverNamesTheMainAgentFromTheShimsAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if len(h.watcher.mainAgents) != 1 || h.watcher.mainAgents[0] != "main-agent" {
		t.Fatalf("main agents = %v, want the shim's answer", h.watcher.mainAgents)
	}
}

func TestDeliverHandsTheOpenedTurnToTheWatcher(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if h.watcher.handovers() != 1 {
		t.Fatalf("handovers = %d, want 1", h.watcher.handovers())
	}
}

func TestDeliverSurfacesAShimRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("the vendor query is dead")
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err == nil {
		t.Fatal("a shim refusal must be surfaced, never swallowed")
	}
	// The mirror fires the moment the prompt is ACCEPTED FOR DELIVERY, before
	// the shim answers, so a refused turn still leaves the user's own words
	// drawn rather than swallowing them.
	if len(h.feed.mirrored()) != 1 {
		t.Fatalf("mirrored rows = %d, want the accepted prompt still drawn", len(h.feed.mirrored()))
	}
}

func TestDeliverSendsABubbleAddressedPromptToThatAgent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sub := submission("t1", "keep going")
	sub.Target = &feedid.Ref{
		WS:   theWorkspace,
		Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-agent"}},
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1", Sub: "sub-agent"},
	}
	// Act
	got, err := h.q.Submit(context.Background(), sub)
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want delivered", got)
	}
	if len(h.sender.agents) != 1 || h.sender.agents[0].GetValue() != "sub-agent" {
		t.Fatalf("agents = %v, want the addressed agent", h.sender.agents)
	}
}

func TestDeliverResolvesTheBubbleAgentFromTheRowsSecondaryKey(t *testing.T) {
	// Arrange: a bubble row whose feed names no agent still addresses one,
	// through the row key's created-agent half.
	h := newHarness(t)
	sub := submission("t1", "keep going")
	sub.Target = &feedid.Ref{
		WS:  theWorkspace,
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1", Sub: "sub-agent"},
	}
	// Act
	if _, err := h.q.Submit(context.Background(), sub); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if len(h.sender.agents) != 1 || h.sender.agents[0].GetValue() != "sub-agent" {
		t.Fatalf("agents = %v, want the row key's created agent", h.sender.agents)
	}
}

func TestDeliverRefusesABubbleRowThatNamesNoAgent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sub := submission("t1", "keep going")
	sub.Target = &feedid.Ref{WS: theWorkspace, Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1"}}
	// Act
	_, err := h.q.Submit(context.Background(), sub)
	// Assert
	if err == nil {
		t.Fatal("an addressed row that names no agent must be refused")
	}
}

func TestDeliverDoesNotHoldABubbleAddressedPromptBehindTheMainTurn(t *testing.T) {
	// Arrange: the main turn runs; a subagent's own composer is not behind it.
	h := newHarness(t)
	h.watcher.running("running-turn")
	sub := submission("t1", "keep going")
	sub.Target = &feedid.Ref{WS: theWorkspace, Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-agent"}}}
	// Act
	got, err := h.q.Submit(context.Background(), sub)
	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want delivered", got)
	}
	if len(h.db.hold("t1").Turn) != 0 {
		t.Fatal("a bubble-addressed prompt is never held")
	}
}

func TestDeliverSurfacesARefusedAgentPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.promptErr = errors.New("not deliverable")
	sub := submission("t1", "keep going")
	sub.Target = &feedid.Ref{WS: theWorkspace, Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-agent"}}}
	// Act
	_, err := h.q.Submit(context.Background(), sub)
	// Assert
	if err == nil {
		t.Fatal("a refused agent prompt must be surfaced")
	}
}

func TestDeliverSurfacesAFailedTurnRecord(t *testing.T) {
	// Arrange: nothing may reach the shim when the durable origin cannot be
	// written first — a delivered turn with no record loses its origin.
	h := newHarness(t)
	h.db.workspaces[theWorkspace] = wsm.Workspace{ID: theWorkspace, Dir: "/tmp/ws-1"}
	h.q.deps.DB = &failingPutTurn{fakeDB: h.db}
	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	// Assert
	if err == nil {
		t.Fatal("a failed turn record must refuse the delivery")
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("the shim must not be called when the turn was not recorded")
	}
}

// failingPutTurn fails only the turn record, leaving every other read intact.
type failingPutTurn struct{ *fakeDB }

func (f *failingPutTurn) PutTurn(context.Context, wsm.Turn) error {
	return errors.New("the database is read-only")
}

// TestDeliveringAPromptStampsTheSessionsEngagement pins the engagement stamp:
// the idle sweep measures hibernation eligibility from it, so a session nothing
// stamps is hibernated out from under an active user — and a session revived by
// a prompt is hibernated again before that prompt's turn has run.
func TestDeliveringAPromptStampsTheSessionsEngagement(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if h.db.engagements != 1 {
		t.Fatalf("engagement stamps = %d, want exactly one for the delivered prompt", h.db.engagements)
	}
}

// TestDeliveringAPromptTellsTheRosterATurnIsRunning pins the roster's turn
// fact: nothing on the shim's streams says a turn was accepted, so a roster
// left to infer it reads `ready` for a workspace whose turn is running.
func TestDeliveringAPromptTellsTheRosterATurnIsRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	turns := h.sidebar.rosterTurns()
	if len(turns) != 1 || turns[0] == nil {
		t.Fatalf("roster turn facts = %v, want exactly one accepted turn", turns)
	}
	if turns[0].Act != footer.ActPrompt {
		t.Fatalf("roster act = %v, want the ordinary prompt", turns[0].Act)
	}
}
