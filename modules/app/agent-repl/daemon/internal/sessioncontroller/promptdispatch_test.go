package sessioncontroller

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

// The daemon's single reading of a submitted prompt, and the three consequences
// it decides together: the receipt, the metaprompt fold, and the clearing axis.
// Splitting them is what let a `/clear` be both cut AND drawn as a bubble.

// --- the reading -------------------------------------------------------------

func TestClassifyPromptRecognizesTheBareClear(t *testing.T) {
	tests := []struct {
		name      string
		text      string
		wantClear bool
	}{
		{
			name:      "the bare command",
			text:      "/clear",
			wantClear: true,
		},
		{
			// Whitespace around the command is still the command.
			name:      "surrounded by whitespace",
			text:      "  /clear\n",
			wantClear: true,
		},
		{
			// An argument means the user asked for something else entirely, and
			// the CLI's own expansion is just as literal about it.
			name:      "with an argument",
			text:      "/clear the build cache",
			wantClear: false,
		},
		{
			name:      "an ordinary prompt",
			text:      "hello there",
			wantClear: false,
		},
		{
			name:      "a different slash command",
			text:      "/compact",
			wantClear: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := classifyPrompt(tc.text)

			// Assert.
			if got.clear != tc.wantClear {
				t.Fatalf("classifyPrompt(%q).clear = %t, want %t", tc.text, got.clear, tc.wantClear)
			}
		})
	}
}

func TestARecognizedCommandEarnsNoReceiptAndNoDirective(t *testing.T) {
	// Arrange — the coupling itself: one reading decides both, so the two can
	// never disagree about whether this string is prompt text.
	cmd := classifyPrompt("/clear")

	// Act / Assert.
	if cmd.echoes() {
		t.Error("a recognized command echoes; the cut already draws its own divider")
	}
	if cmd.foldsMetaprompt() {
		t.Error("a recognized command folds the directive; that would destroy the command")
	}
}

func TestAnOrdinaryPromptEarnsBoth(t *testing.T) {
	// Arrange.
	cmd := classifyPrompt("hello there")

	// Act / Assert.
	if !cmd.echoes() {
		t.Error("an ordinary prompt must echo — it is what the user said")
	}
	if !cmd.foldsMetaprompt() {
		t.Error("an ordinary prompt must be eligible for the directive")
	}
}

// --- the receipt -------------------------------------------------------------

func TestSubmittingAClearPushesNoReceipt(t *testing.T) {
	// Arrange — an idle session, so the prompt goes straight to the shim.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: a bubble reading "/clear" beside the red divider reporting the very
	// same cut is the machinery narrating itself — and it would sit ABOVE the
	// divider, in the region the clear exists to discard.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a /clear, want none — only the divider", len(turns))
	}
}

func TestSubmittingAClearStillForwardsItToTheShim(t *testing.T) {
	// Arrange — withholding the BUBBLE must not withhold the COMMAND.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	got := h.client.promptTexts()
	if len(got) != 1 || got[0] != "/clear" {
		t.Fatalf("forwarded %q, want the /clear verbatim", got)
	}
}

func TestSubmittingAClearOpensTheClearingAxis(t *testing.T) {
	// Arrange — the daemon is the only thing that knows a clear has BEGUN, and
	// suppressing its receipt must not cost it that knowledge.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	cuts := h.applier.cutsApplied()
	if len(cuts) != 1 || cuts[0].axis != "clearing" || !cuts[0].open {
		t.Fatalf("cut edges = %+v, want the clearing axis opened", cuts)
	}
}

func TestAClearWithAnArgumentIsAnOrdinaryPromptAndEchoes(t *testing.T) {
	// Arrange — "/clear the build cache" is something the user SAID, and the
	// conversation is not being cut, so the bubble is the honest report.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "/clear the build cache"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	turns := h.userTurns()
	if len(turns) != 1 || turns[0].item.GetUserMessage().GetContentString() != "/clear the build cache" {
		t.Fatalf("pushed %d user turn(s), want the ordinary prompt's receipt", len(turns))
	}
}

func TestAFailedClearSubmitOpensNoClearingAxis(t *testing.T) {
	// Arrange — the prompt never reached the shim, so the cut is not coming and
	// an axis opened for it would hold the phase word until the watchdog fired.
	h := newQueueHarness(t, nil)
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "/clear"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert.
	if cuts := h.applier.cutsApplied(); len(cuts) != 0 {
		t.Fatalf("cut edges = %+v, want none for a submit that never landed", cuts)
	}
}

// --- the queued path ---------------------------------------------------------

func TestDeliveringAHeldClearPushesNoReceipt(t *testing.T) {
	// Arrange — a /clear held behind a running turn.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the turn ends and the drain delivers it.
	h.turn(false)

	// Assert: the delivery funnel gets the same reading the immediate one does.
	waitFor(t, "the held /clear delivered", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a delivered /clear, want none", len(turns))
	}
}

func TestDeliveringAHeldClearOpensTheClearingAxis(t *testing.T) {
	// Arrange — this path used to recognize nothing at all, so a /clear queued
	// behind a turn was cut with no axis open and the footer said `thinking`
	// straight through it.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the clearing axis opened by the delivery", func() bool {
		cuts := h.applier.cutsApplied()
		return len(cuts) == 1 && cuts[0].axis == "clearing" && cuts[0].open
	})
}

// --- the metaprompt re-fire --------------------------------------------------

// clearedHarness is a harness whose session has a cwd holding a metaprompt file,
// which is what the post-/clear re-fire resolves its directive from.
func clearedHarness(t *testing.T) (*queueHarness, string) {
	t.Helper()
	h := newQueueHarness(t, nil)
	cwd := t.TempDir()
	writeMetaprompt(t, cwd)
	d := h.controller()
	h.m.mu.Lock()
	d.cwd = cwd
	h.m.mu.Unlock()
	return h, cwd
}

func TestTheDirectiveIsNotFoldedIntoTheClearItself(t *testing.T) {
	// Arrange — the CLI expands the command only when it is the WHOLE prompt, so
	// a directive prepended to one means nothing is cleared at all.
	h, _ := clearedHarness(t)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	got := h.client.promptTexts()
	if len(got) == 0 || got[0] != "/clear" {
		t.Fatalf("first forwarded prompt = %q, want the bare /clear", got)
	}
}

func TestAClearFiresTheDirectiveAsItsOwnFollowUpPrompt(t *testing.T) {
	// Arrange — the cut discards the guidelines the session was operating under,
	// so they are re-sent behind it as a SEPARATE prompt.
	h, cwd := clearedHarness(t)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	got := h.client.promptTexts()
	if len(got) != 2 {
		t.Fatalf("forwarded %d prompt(s), want the /clear and the directive behind it", len(got))
	}
	if !strings.Contains(got[1], filepath.Join(cwd, metapromptRelPath)) {
		t.Errorf("follow-up = %q, want the read-directive naming the metaprompt file", got[1])
	}
}

func TestTheFollowUpDirectiveIsSentUnderItsOwnOrigin(t *testing.T) {
	// Arrange — a transcript reader must be able to tell the daemon's own
	// follow-up from anything a human sent.
	h, _ := clearedHarness(t)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	h.client.mu.Lock()
	origins := append([]string(nil), h.client.origins...)
	h.client.mu.Unlock()
	if len(origins) != 2 || origins[1] != metapromptRefireOrigin {
		t.Fatalf("origins = %q, want the follow-up under %q", origins, metapromptRefireOrigin)
	}
}

func TestTheFollowUpDirectivePushesNoReceipt(t *testing.T) {
	// Arrange — it is under the hood: the user typed a /clear and nothing else,
	// so the feed shows a divider and no bubbles at all.
	h, _ := clearedHarness(t)

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s), want none for a /clear and its follow-up", len(turns))
	}
}

func TestAClearWithNoMetapromptFileFiresNoFollowUp(t *testing.T) {
	// Arrange — most checkouts are not this repo, and a session with no
	// metaprompt file has no guidelines to restore.
	h := newQueueHarness(t, nil)
	d := h.controller()
	h.m.mu.Lock()
	d.cwd = t.TempDir()
	h.m.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if got := h.client.promptTexts(); len(got) != 1 {
		t.Fatalf("forwarded %q, want the /clear alone", got)
	}
}

func TestTheFollowUpSatisfiesAnArmedFold(t *testing.T) {
	// Arrange — an ARMED session (a RESUME start) that then clears. The re-fire
	// has already told it to read the file, so folding the same directive into
	// the next prompt would say all of it a second time.
	h, _ := clearedHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.metaArmed = true
	h.m.mu.Unlock()

	// Act: the /clear (which re-fires), then an ordinary prompt.
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit /clear: %v", err)
	}
	if err := h.submitAs("r2", "now do the thing"); err != nil {
		t.Fatalf("submit prompt: %v", err)
	}

	// Assert.
	got := h.client.promptTexts()
	if len(got) != 3 {
		t.Fatalf("forwarded %d prompt(s), want the /clear, the directive, and the prompt", len(got))
	}
	if got[2] != "now do the thing" {
		t.Errorf("ordinary prompt = %q, want it verbatim — the re-fire already delivered the directive", got[2])
	}
}

// --- withholding the follow-up's durable line --------------------------------

func TestTheStandaloneDirectiveIsWithheldFromTheFeed(t *testing.T) {
	// Arrange — the CLI records the daemon's follow-up as an ordinary "user"
	// line, so suppressing the receipt alone would still leave a purple bubble
	// full of text the user never typed.
	h := newQueueHarness(t, nil)
	directive, ok := metapromptDirective(metapromptCwd(t))
	if !ok {
		t.Fatal("metapromptDirective: want a directive")
	}

	// Act.
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u-directive", directive))

	// Assert.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s), want the daemon's own directive withheld", len(turns))
	}
}

func TestAFoldedDirectiveStillRendersItsPrompt(t *testing.T) {
	// Arrange — the fold puts the user's REAL prompt after the directive, and
	// withholding that would swallow something they actually typed.
	h := newQueueHarness(t, nil)
	directive, ok := metapromptDirective(metapromptCwd(t))
	if !ok {
		t.Fatal("metapromptDirective: want a directive")
	}

	// Act.
	h.controller().consumer.Consume(
		transcriptUserEvent(t, 12, "u-folded", prependMetaprompt(directive, "the real prompt")))

	// Assert.
	turns := h.userTurns()
	if len(turns) != 1 || turns[0].item.GetUuid() != "u-folded" {
		t.Fatalf("pushed %d user turn(s), want the folded prompt kept", len(turns))
	}
}

// --- the accepted edge, and the price of publishing it early ------------------
//
// `thinking` is published on the daemon's OWN decision to submit, ahead of the
// shim round-trip that used to gate it, so the workspace stops reading green the
// moment the user presses send. The daemon pays for that claim by withdrawing it
// itself when the submit then fails.

func TestThinkingIsPublishedBeforeTheShimIsAsked(t *testing.T) {
	// Arrange — capture the frontend push trace as the shim round-trip BEGINS.
	// That vantage point is the only one that can tell "published before the
	// Ack" from "published after it"; asserting afterwards proves neither.
	h := newQueueHarness(t, nil)
	h.push.mu.Lock()
	h.push.trace = nil
	h.push.mu.Unlock()
	var atSubmit []string
	h.client.mu.Lock()
	h.client.onSubmit = func() {
		h.push.mu.Lock()
		atSubmit = append([]string(nil), h.push.trace...)
		h.push.mu.Unlock()
	}
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	want := "workspace:RENDER_STATE_THINKING"
	if len(atSubmit) != 1 || atSubmit[0] != want {
		t.Fatalf("frontend push trace as the submit began = %v, want [%s] already published", atSubmit, want)
	}
}

func TestTheAcceptedStateEdgeIsAppliedBeforeTheShimIsAsked(t *testing.T) {
	// Arrange — the same vantage point, watching the SSM rather than the push.
	h := newQueueHarness(t, nil)
	var atSubmit int
	h.client.mu.Lock()
	h.client.onSubmit = func() {
		h.applier.reconcMutex.Lock()
		atSubmit = len(h.applier.promptAccepts)
		h.applier.reconcMutex.Unlock()
	}
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if atSubmit != 1 {
		t.Fatalf("prompt-accept edges applied as the submit began = %d, want the edge already taken", atSubmit)
	}
}

func TestAFailedSubmitRetractsTheAcceptedPrompt(t *testing.T) {
	// Arrange — the daemon has published `thinking` for a prompt the shim then
	// refuses. Nothing else can ever close that row: the lifecycle retires
	// `thinking` on a TurnEnded, and no turn that never began reports an end.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	err := h.submitAs("r1", "hello there")

	// Assert.
	if err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}
	rejects := h.applier.promptRejectCalls()
	if len(rejects) != 1 {
		t.Fatalf("prompt-reject edges = %d, want the one retraction", len(rejects))
	}
	if rejects[0].workspace != "ws" || rejects[0].requestID != "r1" {
		t.Errorf("retraction = %+v, want the refused submit's own workspace and request id", rejects[0])
	}
}

func TestAFailedSubmitRestoresTheTurnLatch(t *testing.T) {
	// Arrange — a latch left set would queue every later prompt behind a turn
	// end that can never arrive.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert.
	active, err := h.m.TurnActive("ws")
	if err != nil || active {
		t.Fatalf("TurnActive after a refused submit = (%v, %v), want false/nil", active, err)
	}
}

func TestAFailedSubmitPushesNoReceipt(t *testing.T) {
	// Arrange — the receipt stays BEHIND the submit even though the state edge
	// moved ahead of it, because a state edge can be retracted and a
	// conversation item the frontend has already drawn cannot.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a prompt no session received, want none", len(turns))
	}
}

func TestARetractedPromptClosesTheFooterClock(t *testing.T) {
	// Arrange — the accept started the footer clock, and a footer counting up
	// against a turn that never began is a worse report than no footer.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert.
	notes := h.prog.turnRejectionNotes()
	if len(notes) != 1 || notes[0].workspace != "ws" {
		t.Fatalf("progress turn rejections = %+v, want the one closure for ws", notes)
	}
}

func TestASupersededStateAxisLeavesTheFooterClockAlone(t *testing.T) {
	// Arrange — the submit fails, but the SSM reports it retracted NOTHING: a
	// durable TurnStarted (or a permission, or a cut) took the axis in the
	// window between the accept and the failure.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = false
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert: closing the clock would report an idle footer over whatever now
	// genuinely owns the turn.
	if notes := h.prog.turnRejectionNotes(); len(notes) != 0 {
		t.Fatalf("progress turn rejections = %+v, want none when the state axis was preserved", notes)
	}
}

func TestARetractionFailureStillReportsTheSubmitError(t *testing.T) {
	// Arrange — both the submit and the retraction of its state edge fail.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectErr = errors.New("state database unavailable")
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	err := h.submitAs("r1", "hello there")

	// Assert: the news is why the prompt did not go, never the bookkeeping that
	// failed to tidy up after it.
	if err == nil || err.Error() != "shim gone" {
		t.Fatalf("submit error = %v, want the shim failure itself", err)
	}
}

func TestAFailedClearRetractsNothing(t *testing.T) {
	// Arrange — `/clear` is a session command, not a turn, so it published no
	// accepted edge that could need withdrawing.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act.
	if err := h.submitAs("r1", "/clear"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert: retracting a claim never made would close a turn on no evidence.
	if rejects := h.applier.promptRejectCalls(); len(rejects) != 0 {
		t.Fatalf("prompt-reject edges for a failed /clear = %+v, want none", rejects)
	}
}

// --- the delivered edge -------------------------------------------------------
//
// The accepted edge publishes on the daemon's own intent, so the workspace reads
// `submitting` until the shim actually takes the prompt. These cover the second
// edge that turns it into `thinking`.

func TestASuccessfulSubmitAdvancesTheStateToThinking(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.applier.promptDeliverDid = true

	// Act
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert
	delivers := h.applier.promptDeliverCalls()
	if len(delivers) != 1 {
		t.Fatalf("prompt-deliver edges = %d, want the one advance", len(delivers))
	}
	if delivers[0].workspace != "ws" || delivers[0].requestID != "r1" {
		t.Errorf("delivered edge = %+v, want the submit's own workspace and request id", delivers[0])
	}
}

func TestTheDeliveredEdgeFollowsTheShimSubmit(t *testing.T) {
	// Arrange — observe the SSM as the shim round-trip BEGINS, which is the
	// only vantage point that can prove the advance did not happen too early.
	h := newQueueHarness(t, nil)
	h.applier.promptDeliverDid = true
	var atSubmit int
	h.client.mu.Lock()
	h.client.onSubmit = func() { atSubmit = len(h.applier.promptDeliverCalls()) }
	h.client.mu.Unlock()

	// Act
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert — `thinking` cannot be claimed before the shim has answered.
	if atSubmit != 0 {
		t.Fatalf("prompt-deliver edges as the submit began = %d, want none until it acks", atSubmit)
	}
}

func TestAFailedSubmitAdvancesNothing(t *testing.T) {
	// Arrange — the shim refuses, so the turn never reaches `thinking`.
	h := newQueueHarness(t, nil)
	h.applier.promptRejectDid = true
	h.controller().client = &failingClient{err: errors.New("shim gone")}

	// Act
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the injected failure")
	}

	// Assert
	if delivers := h.applier.promptDeliverCalls(); len(delivers) != 0 {
		t.Fatalf("prompt-deliver edges for a refused submit = %+v, want none", delivers)
	}
}

func TestAFailedDeliveredEdgeStillReportsTheSubmitAsAccepted(t *testing.T) {
	// Arrange — the shim HAS the prompt, so a bookkeeping failure on the phase
	// word must not report the prompt as lost.
	h := newQueueHarness(t, nil)
	h.applier.promptDeliverErr = errors.New("state database unavailable")

	// Act
	err := h.submitAs("r1", "hello there")

	// Assert
	if err != nil {
		t.Fatalf("submit = %v, want nil — the prompt reached the agent", err)
	}
	if got := h.client.promptTexts(); len(got) != 1 || got[0] != "hello there" {
		t.Fatalf("shim submissions = %q, want the accepted prompt", got)
	}
}

func TestAFailedClearAdvancesNothing(t *testing.T) {
	// Arrange — `/clear` is a session command, so it opened no turn to advance.
	h := newQueueHarness(t, nil)
	h.applier.promptDeliverDid = true

	// Act
	if err := h.submitAs("r1", "/clear"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert
	if delivers := h.applier.promptDeliverCalls(); len(delivers) != 0 {
		t.Fatalf("prompt-deliver edges for a /clear = %+v, want none", delivers)
	}
}

// metapromptCwd is a temp cwd holding a metaprompt file.
func metapromptCwd(t *testing.T) string {
	t.Helper()
	cwd := t.TempDir()
	writeMetaprompt(t, cwd)
	return cwd
}
