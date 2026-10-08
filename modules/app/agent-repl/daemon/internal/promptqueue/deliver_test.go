package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/replyquote"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
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

func TestDeliverTellsTheFooterTheShimTookTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert: the strip leaves `submitting` on the same ack the roster does.
	if got := h.footer.turnAcks(); got != 1 {
		t.Fatalf("footer acks = %d, want 1", got)
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

// THE MIRROR IS THE ONLY DRAW A LIVE SESSION GETS. Nothing brings a delivered
// user prompt back on the watch, so a mirror that drops the image block leaves
// an attached image invisible until some later page replays history -- which
// is exactly what it did.
func TestDeliverMirrorsAnAttachedImageBesideTheWords(t *testing.T) {
	// Arrange
	h := newHarness(t)
	said := userSaidWithImage("what is in this picture?", "/w/.claude/emacs/images/clip.png")

	// Act
	if _, err := h.q.Submit(context.Background(), Submission{
		WS: theWorkspace, Turn: "t1", Said: said,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	blocks := h.feed.mirrored()[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 2 {
		t.Fatalf("the mirrored row carries %d blocks, want the words and the image: %v", len(blocks), blocks)
	}
	if got := blocks[1].GetImage().GetSrc(); got != "src:/w/.claude/emacs/images/clip.png" {
		t.Fatalf("the mirrored image src = %q, want the resolver's own answer", got)
	}
}

// A REPLY'S QUOTE IS MIRRORED AS ITS OWN BLOCK: the live session's only draw
// keeps the quote apart from the words, as the replayed row does, so the
// collapsed bubble shows the words alone from the moment it lands.
func TestDeliverMirrorsAReplysQuoteAsItsOwnBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	said := replyquote.Quote(userSaid("and its population?"), "Paris.", false)

	// Act
	if _, err := h.q.Submit(context.Background(), Submission{
		WS: theWorkspace, Turn: "t1", Said: said,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	blocks := h.feed.mirrored()[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 2 {
		t.Fatalf("the mirrored row carries %d blocks, want the quote and the words: %v", len(blocks), blocks)
	}
	if got, want := blocks[0].GetQuote().GetText(), said.GetContent().GetBlocks()[0].GetQuote().GetText(); got != want {
		t.Fatalf("the mirrored quote = %q, want %q", got, want)
	}
	if got := blocks[1].GetText().GetText(); got != "and its population?" {
		t.Fatalf("the mirrored words = %q, want the person's own", got)
	}
}

// An image the resolver cannot place is NAMED in the mirror, never dropped:
// the person is told something they attached could not be drawn.
func TestDeliverMirrorsAnUnresolvableImageAsUnsupported(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.q.deps.ResolveImage = func(*conversationv1.ImageBlock) (string, string, error) {
		return "", "", errors.New("no producer resolves this reference")
	}
	said := userSaidWithImage("look", "/w/.claude/emacs/images/clip.png")

	// Act
	if _, err := h.q.Submit(context.Background(), Submission{
		WS: theWorkspace, Turn: "t1", Said: said,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	blocks := h.feed.mirrored()[0].GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 2 {
		t.Fatalf("the mirrored row carries %d blocks, want the words and the named refusal: %v", len(blocks), blocks)
	}
	if got := blocks[1].GetUnsupported().GetKind(); got != "image" {
		t.Fatalf("the mirrored refusal names %q, want \"image\"", got)
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

// THE FOOTER TAKES `submitting` ON RECEIPT, before the shim call. The StartTurn
// round-trip can be slow, and a footer left idle through it is the stall the
// owner saw; the submitting phase must be published before StartTurn blocks.
func TestDeliverPublishesSubmittingToTheFooterBeforeTheShim(t *testing.T) {
	// Arrange: capture what the footer holds AT the moment StartTurn is entered.
	h := newHarness(t)
	var footerAtShim []*footer.TurnStarted
	h.sender.startHook = func() { footerAtShim = h.footer.startedTurns() }

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert: the footer already carried the submitting turn when the shim was
	// asked.
	if len(footerAtShim) == 0 || footerAtShim[len(footerAtShim)-1] == nil {
		t.Fatalf("footer at StartTurn = %+v, want a submitting turn already set", footerAtShim)
	}
}

// A SHIM REFUSAL NEVER LEAVES THE FOOTER STUCK ON `submitting`. The failure is
// surfaced to the caller, and the footer drops the turn rather than showing a
// submitting phase for a turn that never ran.
func TestDeliverClearsTheFooterWhenTheShimRefuses(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("the vendor query is dead")

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err == nil {
		t.Fatal("a shim refusal must be surfaced, never swallowed")
	}

	// Assert: the last thing the footer was told is that no turn is in flight.
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[len(turns)-1] != nil {
		t.Fatalf("footer turns = %+v, want the submitting turn cleared after the refusal", turns)
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

func TestDeliverDrawsAMergeTurnsAcceptedPromptAtTheMergeAddress(t *testing.T) {
	// Arrange: a merge stands an address at one of its tabs, and submits.
	h := newHarness(t)
	lease := ids.LeaseID("lease-1")
	h.feed.SetOutputAddress("ws-1", &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})
	sub := submission("t1", "resolve the conflict")
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR
	// Act
	if _, err := h.q.Submit(context.Background(), sub); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert: the row carries the ADDRESSED feed's identity, not the root's.
	want := feedid.Encode(feedid.Ref{
		WS:   "ws-1",
		Feed: feedid.Feed{Merge: &lease},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "t1"},
	})
	if got := h.feed.mirrored()[0].GetId().GetValue(); got != want.GetValue() {
		t.Fatalf("mirror id = %q, want the addressed feed's %q", got, want.GetValue())
	}
}

func TestDeliverDrawsAUserPromptSentMidMergeOnTheRootFeed(t *testing.T) {
	// Arrange: a merge stands an address, and the user sends a prompt.
	h := newHarness(t)
	lease := ids.LeaseID("lease-1")
	h.feed.SetOutputAddress("ws-1", &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	want := feedid.Encode(feedid.Ref{
		WS:   "ws-1",
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "t1"},
	})
	if got := h.feed.mirrored()[0].GetId().GetValue(); got != want.GetValue() {
		t.Fatalf("row id = %q, want the root feed's %q", got, want.GetValue())
	}
}

func TestDeliverMirrorsOntoTheRootFeedWhenNoOutputAddressStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	want := feedid.Encode(feedid.Ref{
		WS:   "ws-1",
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "t1"},
	})
	if got := h.feed.mirrored()[0].GetId().GetValue(); got != want.GetValue() {
		t.Fatalf("mirror id = %q, want the root feed's %q", got, want.GetValue())
	}
}

// A turn_already_open is the daemon's own double-submit and is surfaced once,
// never retried, and the footer's submitting turn clears.
func TestDeliverSurfacesATurnAlreadyOpenWithoutRetrying(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("shim StartTurn refused: turn_already_open: turn t0 is already in flight")

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err == nil {
		t.Fatal("a daemon double-submit must be surfaced")
	}
	if got := h.sender.startAttempts(); got != 1 {
		t.Fatalf("StartTurn attempts = %d, want the refusal surfaced without a retry", got)
	}
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[len(turns)-1] != nil {
		t.Fatalf("footer turns = %+v, want the submitting turn cleared after the refusal", turns)
	}
}

// --- a context cut's text is delivered as a context cut --------------------

func TestAHeldCompactIsDeliveredAsTheRunningSessionAct(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "/compact keep the plan")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	cut, ok := h.q.runningCut(theWorkspace)
	if !ok || cut.turn != "t1" || cut.command != conversationv1.SessionCommand_SESSION_COMMAND_COMPACT {
		t.Fatalf("running cut = (%+v, %v), want t1 recorded as the running /compact", cut, ok)
	}
}

func TestAHeldCompactIsSentWithItsInstructions(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "/compact\nkeep the plan")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if got := saidText(h.sender.said[0]); got != "/compact keep the plan" {
		t.Fatalf("text = %q, want the literal and its instructions", got)
	}
}

func TestAHeldCompactIsRetiredFromTheTrayOnceDelivered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "/compact")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if tomb := h.db.retired("t1"); tomb == nil || tomb.Kind != tombstoneDelivered {
		t.Fatalf("tombstone = %+v, want the hold retired as delivered", tomb)
	}
}

func TestAClearSubmittedWithNothingRunningIsRunAsTheSessionAct(t *testing.T) {
	// Arrange: a caller that never went through the handler's recognition.
	h := newHarness(t)
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "/clear")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	cut, ok := h.q.runningCut(theWorkspace)
	if !ok || cut.turn != "t1" || cut.command != conversationv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("running cut = (%+v, %v), want t1 recorded as the running /clear", cut, ok)
	}
}

// clearSpellings are the texts the vendor's CLI reads as /clear, each of which
// is delivered as the /clear cut (owner ruling, 2026-09-27).
var clearSpellings = []struct {
	name string
	text string
	sent string
}{
	{name: "/reset", text: "/reset", sent: "/clear"},
	{name: "/new", text: "/new", sent: "/clear"},
	{name: "/clear with trailing text", text: "/clear foo", sent: "/clear foo"},
	{name: "/reset with trailing text", text: "/reset foo", sent: "/clear foo"},
}

func TestEverySpellingOfClearIsDeliveredAsTheClearCut(t *testing.T) {
	for _, tt := range clearSpellings {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", tt.text)); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			// Assert
			cut, ok := h.q.runningCut(theWorkspace)
			if !ok || cut.command != conversationv1.SessionCommand_SESSION_COMMAND_CLEAR {
				t.Fatalf("running cut = (%+v, %v), want the running /clear", cut, ok)
			}
		})
	}
}

func TestEverySpellingOfClearIsSentToTheVendorAsClear(t *testing.T) {
	for _, tt := range clearSpellings {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", tt.text)); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			// Assert
			if got := saidText(h.sender.said[0]); got != tt.sent {
				t.Fatalf("text = %q, want %q", got, tt.sent)
			}
		})
	}
}

func TestAPromptHeldDuringAnySpellingOfClearDoesNotInterruptIt(t *testing.T) {
	for _, tt := range clearSpellings {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if _, err := h.q.Submit(context.Background(), submission("cut-1", tt.text)); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.watcher.running("cut-1")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert
			if killed := h.sender.killed(); len(killed) != 0 {
				t.Fatalf("killed = %v, want the running /clear left to run", killed)
			}
		})
	}
}

func TestAPromptThatInterruptedTheTurnIsDeliveredWithTheInterruptionNote(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if started, notes := h.sender.started(), h.sender.notes; len(started) != 1 || started[0] != "t1" || len(notes) != 1 || notes[0] != interruptionNote {
		t.Fatalf("started = %v, notes = %q; want t1 delivered with the interruption note", started, notes)
	}
}

func TestAPromptThatWaitedForTheTurnIsDeliveredWithNoNote(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if started, notes := h.sender.started(), h.sender.notes; len(started) != 1 || len(notes) != 0 {
		t.Fatalf("started = %v, notes = %q; want t1 delivered with no note", started, notes)
	}
}

func TestStartedByMergeNamesExactlyTheMergesOwnOrigins(t *testing.T) {
	cases := []struct {
		origin conversationv1.PromptOrigin
		want   bool
	}{
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, true},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, true},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION, true},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION, true},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME, false},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, false},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED, false},
		{conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED, false},
	}
	for _, tc := range cases {
		t.Run(tc.origin.String(), func(t *testing.T) {
			// Arrange / Act
			got := startedByMerge(tc.origin.String())

			// Assert
			if got != tc.want {
				t.Fatalf("startedByMerge(%s) = %v, want %v", tc.origin, got, tc.want)
			}
		})
	}
}

// errShimNoSession is the sender's refusal of a StartTurn by a shim holding
// no session, as the workspace sender's typed refusal answers it.
var errShimNoSession = fmt.Errorf("shim StartTurn refused: %w", ErrShimHasNoSession)

// A DELIVERY THE SHIM REFUSES no_session IS HELD, never lost: the daemon
// believed the session up and it was not.
func TestANoSessionRefusalHoldsThePromptForTheReconnect(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errShimNoSession

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err != nil || got.Held == nil || *got.Held != wsm.HoldReconnect {
		t.Fatalf("Submit = (%+v, %v), want the prompt held under the reconnect hold", got, err)
	}
}

func TestANoSessionRefusalTakesTheMirroredRowDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errShimNoSession

	// Act
	_, _ = h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if got := h.feed.retiredPrompts(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("retired prompt rows = %v, want t1's mirrored row taken down", got)
	}
}

// A HELD PROMPT THE SHIM REFUSES no_session STAYS HELD: re-stamped to wait for
// the reconnect, never retired.
func TestANoSessionRefusalOfAHeldPromptKeepsItHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.notStarted = true
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.notStarted = false
	h.sender.startErr = errShimNoSession

	// Act
	h.q.ReleaseReconnectHolds("ws-1")

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), "ws-1")
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(standing) != 1 || standing[0].Tombstone != nil || standing[0].Hold == nil || *standing[0].Hold != wsm.HoldReconnect {
		t.Fatalf("standing = %+v, want t1 held under the reconnect hold", standing)
	}
}

func TestAnOrdinaryStartTurnRefusalIsStillSurfaced(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("shim StartTurn refused: query_dead")

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err == nil {
		t.Fatal("Submit = nil error, want a refusal other than no_session surfaced")
	}
}
