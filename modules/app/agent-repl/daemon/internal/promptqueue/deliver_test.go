package promptqueue

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
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

func TestDeliverMirrorsTheAcceptedPromptAtTheSessionsOutputAddress(t *testing.T) {
	// Arrange: a lease holder has addressed the session at a merge sub-feed.
	h := newHarness(t)
	lease := ids.LeaseID("lease-1")
	h.feed.SetOutputAddress("ws-1", &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert: the mirror carries the ADDRESSED feed's identity, not the root's.
	want := feedid.Encode(feedid.Ref{
		WS:   "ws-1",
		Feed: feedid.Feed{Merge: &lease},
		Row:  feedid.RowKey{Kind: feedid.KindPrompt, ID: "t1"},
	})
	if got := h.feed.mirrored()[0].GetId().GetValue(); got != want.GetValue() {
		t.Fatalf("mirror id = %q, want the addressed feed's %q", got, want.GetValue())
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

// A KEEP-ALIVE COLLISION IS TRANSIENT, NOT TERMINAL. A StartTurn refused
// because one of the shim's own keep-alive pings was momentarily in flight is
// re-driven until the ping closes, then succeeds — never surfaced as an error.
func TestDeliverReDrivesAKeepaliveCollisionThenSucceeds(t *testing.T) {
	// Arrange: two collisions, then the keep-alive closes and the turn starts.
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startScript = []error{
		keepaliveCollisionErr{keepalive: true},
		keepaliveCollisionErr{keepalive: true},
		nil,
	}

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))
	h.waitRedrives()

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v, want a keep-alive collision to be re-driven, not surfaced", err)
	}
	if !got.Delivered {
		t.Fatalf("disposition = %+v, want the prompt accepted while it re-drives", got)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started turns = %v, want the one turn started once it frees", started)
	}
	if h.watcher.handovers() != 1 {
		t.Fatalf("handovers = %d, want the turn handed over once it started", h.watcher.handovers())
	}
}

// THE RE-DRIVE IS A RETRY, NOT A SECOND TURN. Every re-drive uses the same
// minted turn id (the idempotency key), so exactly one durable turn record
// stands and the shim opens the one turn.
func TestDeliverReDrivesWithTheSameIdempotencyKey(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startScript = []error{keepaliveCollisionErr{keepalive: true}, nil}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRedrives()

	// Assert: the successful StartTurn carried the original turn id.
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started turns = %v, want the same minted turn re-driven", started)
	}
	// And exactly one durable turn record was written — no second turn.
	if _, ok := h.db.startedTurn("t1"); !ok {
		t.Fatal("the one turn must be recorded")
	}
}

// THE SUBMITTING STATUS STAYS UP ACROSS A TRANSIENT RE-DRIVE. The footer is
// told the turn once, at acceptance, and is never cleared while the prompt
// re-drives behind the keep-alive.
func TestDeliverKeepsSubmittingUpAcrossAKeepaliveReDrive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startScript = []error{keepaliveCollisionErr{keepalive: true}, nil}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRedrives()

	// Assert: the footer carries exactly the one submitting turn, never cleared.
	turns := h.footer.startedTurns()
	if len(turns) != 1 || turns[0] == nil {
		t.Fatalf("footer turns = %+v, want the submitting turn raised once and never cleared", turns)
	}
}

// A NON-TRANSIENT turn_already_open (a genuine daemon double-submit, keepalive
// false) is NOT re-driven: it stays a surfaced error, and the footer clears.
func TestDeliverSurfacesANonKeepaliveTurnAlreadyOpen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startErr = keepaliveCollisionErr{keepalive: false}

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err == nil {
		t.Fatal("a genuine daemon double-submit must be surfaced, never re-driven")
	}
	if got := h.sender.startAttempts(); got != 1 {
		t.Fatalf("StartTurn attempts = %d, want the terminal refusal not re-driven", got)
	}
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[len(turns)-1] != nil {
		t.Fatalf("footer turns = %+v, want the submitting turn cleared after the terminal refusal", turns)
	}
}

// THE RE-DRIVE IS BOUNDED. A keep-alive that never closes does not loop
// forever: the re-drive stops at its cap and surfaces a real error rather than
// spinning.
func TestDeliverBoundsTheKeepaliveReDrive(t *testing.T) {
	// Arrange: the keep-alive never closes.
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startErr = keepaliveCollisionErr{keepalive: true}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v, want the prompt accepted while it re-drives", err)
	}
	h.waitRedrives()

	// Assert: the initial delivery plus a bounded number of re-drives, no more.
	if got, want := h.sender.startAttempts(), 1+keepaliveRedriveMaxAttempts; got != want {
		t.Fatalf("StartTurn attempts = %d, want exactly %d (initial + bounded re-drives)", got, want)
	}
}

// A RE-DRIVE THAT EXHAUSTS ITS BOUND SURFACES A REAL ERROR — the durable turn
// is stamped FAILED and the submitting status clears, never a silent drop.
func TestDeliverFailsTheTurnWhenTheReDriveIsExhausted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startErr = keepaliveCollisionErr{keepalive: true}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRedrives()

	// Assert: the turn is closed failed, the footer is cleared, and the roster
	// is told the turn ended failed.
	if how, ok := h.db.closedTurn("t1"); !ok || how != wsm.CloseFailed {
		t.Fatalf("closed turn = (%v, %v), want it stamped CloseFailed", how, ok)
	}
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[0] == nil || turns[len(turns)-1] != nil {
		t.Fatalf("footer turns = %+v, want submitting raised then cleared on exhaustion", turns)
	}
	if ends := h.sidebar.rosterEnds(); len(ends) != 1 || ends[0] != wsm.CloseFailed {
		t.Fatalf("roster ends = %v, want exactly one failed close", ends)
	}
}

// A NON-TRANSIENT refusal ARRIVING DURING a re-drive stops the re-drive and
// surfaces the failure rather than re-driving into a wall.
func TestDeliverStopsReDrivingOnANonTransientRefusal(t *testing.T) {
	// Arrange: a keep-alive collision, then the query dies.
	h := newHarness(t)
	h.instantRedrive()
	h.sender.startScript = []error{
		keepaliveCollisionErr{keepalive: true},
		errors.New("the vendor query is dead"),
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRedrives()

	// Assert: it stopped at the terminal refusal, not the bound.
	if got := h.sender.startAttempts(); got != 2 {
		t.Fatalf("StartTurn attempts = %d, want it to stop at the terminal refusal", got)
	}
	if how, ok := h.db.closedTurn("t1"); !ok || how != wsm.CloseFailed {
		t.Fatalf("closed turn = (%v, %v), want it stamped CloseFailed", how, ok)
	}
}

// AN INTERRUPT CANCELS A TURN RE-DRIVING BEHIND A KEEP-ALIVE. The user asked to
// stop a turn that never started on the shim — a keep-alive held the slot — so
// the re-drive is removed and the turn is closed KILLED rather than starting
// later or being surfaced as an error.
func TestCancelKeepaliveRedriveCancelsAQueuedTurn(t *testing.T) {
	// Arrange: the backoff never fires, so the re-drive is parked in its wait,
	// and every StartTurn collides with the keep-alive.
	h := newHarness(t)
	blocked := make(chan time.Time)
	h.q.deps.After = func(time.Duration) <-chan time.Time { return blocked }
	h.sender.startErr = keepaliveCollisionErr{keepalive: true}
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	cancelled := h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t1")
	h.waitRedrives()

	// Assert
	if !cancelled {
		t.Fatal("CancelKeepaliveRedrive = false, want the queued turn cancelled")
	}
	if how, ok := h.db.closedTurn("t1"); !ok || how != wsm.CloseKilled {
		t.Fatalf("closed turn = (%v, %v), want it stamped CloseKilled", how, ok)
	}
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started turns = %v, want the cancelled turn never started", got)
	}
}

// THE CANCEL CLEARS THE SUBMITTING STATUS. The rpc answered `submitting` when
// the prompt was accepted; a cancelled re-drive clears that footer status and
// tells the roster the turn ended killed, never leaving a dangling submitting.
func TestCancelKeepaliveRedriveClearsTheSubmittingStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	blocked := make(chan time.Time)
	h.q.deps.After = func(time.Duration) <-chan time.Time { return blocked }
	h.sender.startErr = keepaliveCollisionErr{keepalive: true}
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t1")
	h.waitRedrives()

	// Assert: the footer's submitting turn was raised then cleared, and the
	// roster was told the turn ended killed.
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[0] == nil || turns[len(turns)-1] != nil {
		t.Fatalf("footer turns = %+v, want submitting raised then cleared on the cancel", turns)
	}
	ends := h.sidebar.rosterEnds()
	if len(ends) != 1 || ends[0] != wsm.CloseKilled {
		t.Fatalf("roster ends = %v, want one CloseKilled end", ends)
	}
}

// A CANCEL FOR A TURN THAT IS NOT RE-DRIVING IS A NO-OP. There is nothing of
// the user's queued behind the keep-alive, so the cancel reports false and the
// caller interrupts the genuinely open turn instead.
func TestCancelKeepaliveRedriveReportsFalseWhenNoReDriveStands(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act & Assert
	if h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t1") {
		t.Fatal("CancelKeepaliveRedrive = true, want false when no turn is re-driving")
	}
}

// A CANCEL NAMING A DIFFERENT TURN LEAVES THE RE-DRIVE ALONE. A stale interrupt
// for a turn that is not the one re-driving must not cancel the one that is.
func TestCancelKeepaliveRedriveIgnoresAMismatchedTurn(t *testing.T) {
	// Arrange: t1 is re-driving behind the keep-alive.
	h := newHarness(t)
	blocked := make(chan time.Time)
	h.q.deps.After = func(time.Duration) <-chan time.Time { return blocked }
	h.sender.startErr = keepaliveCollisionErr{keepalive: true}
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act: an interrupt for a different turn.
	mismatched := h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t2")

	// Assert: it reported false and t1's re-drive still stands (cancel it to
	// drain the goroutine cleanly).
	if mismatched {
		t.Fatal("CancelKeepaliveRedrive(t2) = true, want false for a turn that is not re-driving")
	}
	if !h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t1") {
		t.Fatal("CancelKeepaliveRedrive(t1) = false, want the still-standing re-drive cancellable")
	}
	h.waitRedrives()
}

// AN INTERRUPT THAT RACES A RE-DRIVE'S OPENING ENDS ONLY THE TURN. The turn
// opened in the same instant the user's interrupt cancelled it, so the queue
// kills it, and the kill is UNFORCED: an interrupt never stops the turn's
// detached work. A kill the shim refuses is recorded at ERROR with its cause.
func TestARedriveThatOpensAsAnInterruptCancelsItIsKilledUnforced(t *testing.T) {
	tests := []struct {
		name       string
		killErr    error
		wantKills  []ids.TurnID
		wantForces []bool
		wantLevel  string
		wantMsg    string
		wantCause  any
	}{
		{
			name:       "the kill lands unforced",
			wantKills:  []ids.TurnID{"t1"},
			wantForces: []bool{false},
			wantLevel:  "info",
			wantMsg:    "the re-driven turn opened as an interrupt cancelled it; stopping it",
		},
		{
			name:      "a refused kill is recorded at error",
			killErr:   errors.New("the shim refused the kill"),
			wantLevel: "error",
			wantMsg:   "could not stop the interrupt-cancelled turn that had just opened",
			wantCause: "the shim refused the kill",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the first StartTurn collides with a keep-alive, and the
			// re-drive's StartTurn is interrupted while it is opening the turn.
			h := newHarness(t)
			h.instantRedrive()
			h.sender.killErr = tt.killErr
			h.sender.startScript = []error{keepaliveCollisionErr{keepalive: true}, nil}
			h.sender.startHook = func() {
				if h.sender.attempts == 2 {
					h.q.CancelKeepaliveRedrive(context.Background(), theWorkspace, "t1")
				}
			}

			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.waitRedrives()

			// Assert
			if got := h.sender.killed(); !slices.Equal(got, tt.wantKills) {
				t.Fatalf("killed turns = %v, want %v", got, tt.wantKills)
			}
			if got := h.sender.killedForces(); !slices.Equal(got, tt.wantForces) {
				t.Fatalf("kill forces = %v, want %v: an interrupt's kill is never forced", got, tt.wantForces)
			}
			for _, r := range h.log.Records() {
				if r.Level == tt.wantLevel && r.Operation == opDeliver && r.Message == tt.wantMsg &&
					(tt.wantCause == nil || r.Context["cause"] == tt.wantCause) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s %q", h.log.Records(), tt.wantLevel, tt.wantMsg)
		})
	}
}
