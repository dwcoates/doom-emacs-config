//go:build integration

package integration

import (
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// origin is the closed-attribution value every test in this suite submits
// with, unless the bullet under test is specifically about attribution.
const origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT

// ---------------------------------------------------------------------------
// SubmitPrompt on an idle session
// ---------------------------------------------------------------------------

func TestSubmitPromptOnAnIdleSessionMintsATurnIdAndStartsTheTurn(t *testing.T) {
	// Arrange: a metaprompt-wrapped span the drawn row must strip, while the
	// full text still travels to the shim unchanged.
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	visible := "fix the flaky reconnect test"
	full := visible + "\n<!--agent-repl:meta-->extra injected context<!--/agent-repl:meta-->"

	// Act
	resp := f.submit(full, "k-idle", origin)

	// Assert
	turn := resp.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	row := awaitRow(t, f, feed, "the mirrored user_prompt row", func(r *frontendv1.FeedRow) bool {
		return r.GetUserPrompt() != nil
	})
	if row.GetTurn().GetValue() != turn.GetValue() {
		t.Fatalf("mirrored row turn = %q, want the minted turn %q", row.GetTurn().GetValue(), turn.GetValue())
	}
	if got := promptText(row); got != visible {
		t.Fatalf("drawn row text = %q, want the metaprompt sentinel span stripped to %q", got, visible)
	}
	req := f.shim.ExpectStartTurn()
	if req.GetTurn().GetValue() != turn.GetValue() {
		t.Fatalf("StartTurn.turn = %q, want the minted turn %q", req.GetTurn().GetValue(), turn.GetValue())
	}
	if text(req.GetSaid()) != full {
		t.Fatalf("StartTurn.said = %q, want the FULL text (record keeps sentinels) %q", text(req.GetSaid()), full)
	}
	if req.GetOrigin() != origin {
		t.Fatalf("StartTurn.origin = %v, want %v", req.GetOrigin(), origin)
	}
}

func TestDuplicateIdempotencyKeyIsRefusedAndSendsNoSecondStartTurn(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	first := f.submit("do the thing", "dup-key", origin)
	second := f.submit("do the thing", "dup-key", origin)

	// Assert: the contract's arm is `duplicate_submission` -- "the earlier
	// submission stands (its turn, hold or panel is already visible); nothing
	// is submitted twice" -- not a second echo of the first turn.
	if first.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("the first submission = %v, want a minted turn", first)
	}
	if second.GetError().GetDuplicateSubmission() == nil {
		t.Fatalf("the duplicate submission = %v, want error.duplicate_submission", second)
	}
	if got := f.shim.Count(harness.RPCStartTurn); got != 1 {
		t.Fatalf("StartTurn count = %d, want exactly 1 (the duplicate sends no second StartTurn)", got)
	}
}

func TestSubmitPromptWithOriginUnspecifiedIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act: internal/server/validate.go refuses PROMPT_ORIGIN_UNSPECIFIED
	// before the request ever resolves a workspace, so this is a pure
	// InvalidArgument -- never an SubmitPromptError arm.
	err := f.submitExpectingError(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           said("no origin"),
		IdempotencyKey: "k-no-origin",
		Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
	})

	// Assert
	if connectCode(err) != connect.CodeInvalidArgument {
		t.Fatalf("SubmitPrompt with origin UNSPECIFIED = %v (code %v), want CodeInvalidArgument", err, connectCode(err))
	}
	if !containsField(err, "origin") {
		t.Fatalf("SubmitPrompt refusal = %v, want it to name the unset field \"origin\"", err)
	}
	// Validation refuses before any component ever logs.
	f.d.ExpectWarnings()
}

// ---------------------------------------------------------------------------
// SubmitPrompt while a turn is in flight: HELD
// ---------------------------------------------------------------------------

// promptHeldEntry finds a tray item's HeldPrompt by its turn, or nil.
func promptHeldEntry(tray *frontendv1.DaemonHoldTray, turn *conversationv1.TurnId) *frontendv1.HeldPrompt {
	for _, item := range tray.GetItems() {
		if p := item.GetPrompt(); p != nil && p.GetTurn().GetValue() == turn.GetValue() {
			return p
		}
	}
	return nil
}

func TestAHeldPromptShowsClassifyingThenAVerdictFromTheFakeHeuristic(t *testing.T) {
	// Arrange: a turn in flight (StartTurn accepted, no terminal frame yet).
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	holds := f.d.WatchHolds(f.ws)
	// Drain the fresh subscriber's initial (empty) tray push, so the two
	// reads below land on the classifying push and the verdict push in
	// order. `harness.Stream` delivers EVERY distinct push in order, never
	// coalesced, and the classifying record is written into the hold
	// (internal/promptqueue/classify.go's `hold`) and pushed SYNCHRONOUSLY,
	// before the classifier's goroutine is even started -- so it is on the
	// channel before SubmitPrompt's own response returns, deterministically.
	awaitView(t, f, holds, "the initial empty tray", func(tray *frontendv1.DaemonHoldTray) bool {
		return len(tray.GetItems()) == 0
	})

	// Act: an ordinary follow-up prompt, no explicit-interrupt keyword.
	resp := f.submit("please also check the other file", "k-held", origin)
	turn := resp.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt while a turn runs = %v, want a minted TurnId (it is HELD, not refused)", resp)
	}

	// Assert: the classifying push comes FIRST.
	classifying := harness.AwaitNext(t, f.d.Ctx(), holds, "the classifying push")
	if p := promptHeldEntry(classifying, turn); p == nil || p.GetClassifying() == nil {
		t.Fatalf("the first tray push for the held prompt = %v, want the transient classifying arm", p)
	}

	// Assert: the -fake heuristic's real verdict for an ordinary follow-up
	// (no "stop" prefix, no `[interject]` marker) is hold_for_turn_end
	// (internal/classifier/fake.go).
	verdict := harness.AwaitNext(t, f.d.Ctx(), holds, "the verdict push")
	if p := promptHeldEntry(verdict, turn); p == nil || p.GetHoldForTurnEnd() == nil {
		t.Fatalf("the verdict for the held prompt = %v, want hold_for_turn_end from the -fake heuristic", p)
	}
}

func TestAPromptBeginningWithStopTakesTheFastPathToInterjectAndInterruptsTheRunningTurn(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	resp1 := f.submit("start the long task", "k-running", origin)
	turn1 := resp1.GetSuccess().GetTurn().GetTurn()
	f.shim.ExpectStartTurn()
	footer := f.d.WatchFooter(f.ws)
	holds := f.d.WatchHolds(f.ws)

	// Act
	resp2 := f.submit("stop and rebase instead", "k-stop", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()

	// Assert: the footer shows waiting.interrupting immediately.
	awaitFooter(t, f, footer, "waiting.interrupting the moment the interrupt registers", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetInterrupting() != nil
	})
	killReq := f.shim.ExpectKillTurn()
	_ = killReq
	tray := awaitView(t, f, holds, "the interject verdict", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, turn2)
		return p != nil && p.GetInterject() != nil
	})
	if p := promptHeldEntry(tray, turn2); p == nil || p.GetInterject() == nil {
		t.Fatalf("held entry for the stop prompt = %v, want the interject verdict", p)
	}

	// Act: the fake reports the interrupted turn's real end.
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))

	// Assert: the held prompt delivers only now, as turn2.
	st2 := f.shim.ExpectStartTurn()
	if st2.GetTurn().GetValue() != turn2.GetValue() {
		t.Fatalf("StartTurn after the interrupted turn ended = turn %q, want the interjected turn %q", st2.GetTurn().GetValue(), turn2.GetValue())
	}
	_ = turn1
}

func TestHeldForTurnEndPromptsDeliverFifoAfterTheTurnEnds(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()

	resp2 := f.submit("first follow-up", "k-2", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	resp3 := f.submit("second follow-up", "k-3", origin)
	turn3 := resp3.GetSuccess().GetTurn().GetTurn()

	// Act: the running turn ends.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: turn2 delivers first, and turn3 has not delivered yet.
	st2 := f.shim.ExpectStartTurn()
	if st2.GetTurn().GetValue() != turn2.GetValue() {
		t.Fatalf("first delivered turn = %q, want the FIFO head %q", st2.GetTurn().GetValue(), turn2.GetValue())
	}
	if got := f.shim.Count(harness.RPCStartTurn); got != 2 {
		t.Fatalf("StartTurn count = %d, want 2 (turn3 must not deliver before turn2 ends)", got)
	}

	// Act: turn2 ends too.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: turn3 delivers now.
	st3 := f.shim.ExpectStartTurn()
	if st3.GetTurn().GetValue() != turn3.GetValue() {
		t.Fatalf("second delivered turn = %q, want the FIFO tail %q", st3.GetTurn().GetValue(), turn3.GetValue())
	}
}

// ---------------------------------------------------------------------------
// UpdateHeldPrompt: drop and release
// ---------------------------------------------------------------------------

func TestUpdateHeldPromptDropRemovesTheEntryDurablyAcrossADaemonRestart(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("a prompt to drop", "k-drop", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()

	// Act
	dropResp, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws,
		Turn:      turn2,
		Action:    &agentreplv1.UpdateHeldPromptRequest_Drop{Drop: &agentreplv1.UpdateHeldPromptDrop{}},
	}))
	if err != nil {
		t.Fatalf("UpdateHeldPrompt{drop} = error %v, want a success", err)
	}
	if dropResp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{drop} = %v, want a success", dropResp.Msg)
	}

	// Assert: gone before the restart too.
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the dropped entry gone", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn2) == nil
	})

	// Act: restart the daemon on the same state root.
	nd := promptRestartDaemon(t, f)

	// Assert: still gone.
	newHolds := nd.WatchHolds(f.ws)
	got := harness.AwaitNext(t, nd.Ctx(), newHolds, "the tray after restart")
	if promptHeldEntry(got, turn2) != nil {
		t.Fatalf("the dropped entry reappeared after a restart, want it gone durably")
	}
}

func TestUpdateHeldPromptReleaseDeliversNow(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("a prompt to release", "k-release", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()

	// Act
	relResp, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws,
		Turn:      turn2,
		Action:    &agentreplv1.UpdateHeldPromptRequest_Release{Release: &agentreplv1.UpdateHeldPromptRelease{}},
	}))
	if err != nil {
		t.Fatalf("UpdateHeldPrompt{release} = error %v, want a success", err)
	}
	if relResp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{release} = %v, want a success", relResp.Msg)
	}

	// Assert: release forces delivery, interrupting the running turn.
	f.shim.ExpectKillTurn()
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))
	st2 := f.shim.ExpectStartTurn()
	if st2.GetTurn().GetValue() != turn2.GetValue() {
		t.Fatalf("StartTurn after release = turn %q, want the released turn %q", st2.GetTurn().GetValue(), turn2.GetValue())
	}
}

// ---------------------------------------------------------------------------
// Held prompts across a daemon restart
// ---------------------------------------------------------------------------

func TestAHeldPromptSurvivesADaemonRestart(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("a prompt that must survive", "k-survive", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()

	// Act
	nd := promptRestartDaemon(t, f)

	// Assert
	holds := nd.WatchHolds(f.ws)
	got := awaitView(t, f, holds, "the survived held entry", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn2) != nil
	})
	if promptHeldEntry(got, turn2) == nil {
		t.Fatalf("the held prompt did not survive the restart")
	}
}

// TestACorruptedHeldPromptRowFailsBootLoudlyWithExactlyOneRestoreError is the
// all-or-nothing hold restore: two held prompts stand on one workspace, one
// held_prompts row is corrupted directly in wsm.db (harness.CorruptRow), and
// the daemon is restarted.
//
// internal/promptqueue/lifecycle.go's RestoreHolds reads EVERY held prompt in
// ONE all-or-nothing decode (internal/wsm/heldprompts.go's scanHeldPrompt): a
// single corrupt row fails the WHOLE read, so ZERO holds are loaded rather
// than the one good entry surviving. And internal/boot/sequence.go's Run is
// documented "EVERY STEP FAILS THE BOOT" -- restoreHolds failing fails the
// boot outright, so there is no live daemon left to show an "empty tray": the
// all-or-nothing loss is total, not partial. This test asserts the actual,
// deliberately documented contract (a loud non-zero refusal, matching
// TestBootRefusesAnUnwritableStateRoot's pattern) rather than the softer
// "daemon boots with an empty tray" some report of this critique assumed; see
// the report for that divergence.
func TestACorruptedHeldPromptRowFailsBootLoudlyWithExactlyOneRestoreError(t *testing.T) {
	// Arrange: two held prompts on one workspace.
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("first held prompt", "k-corrupt-1", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	f.submit("second held prompt", "k-corrupt-2", origin)
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "both held entries standing", func(tray *frontendv1.DaemonHoldTray) bool {
		return len(tray.GetItems()) == 2
	})
	if got := f.d.CountRows("held_prompts"); got != 2 {
		t.Fatalf("held_prompts row count before corruption = %d, want 2", got)
	}
	f.d.Stop()

	// Act: corrupt ONE row's `said` column with bytes that cannot decode as a
	// UserSaid, and restart on the same state root.
	f.d.CorruptRow("held_prompts", "said", "turn_id", turn2.GetValue(), []byte("not a protobuf blob"))
	nd := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExpectEarlyExit: true})
	code := nd.AwaitExit()

	// Assert: the boot refuses loudly.
	if code == 0 {
		t.Fatalf("boot with a corrupted held_prompts row exited 0, want a loud non-zero refusal")
	}
	stderr := nd.Stderr()
	if got := strings.Count(stderr, "daemon.promptqueue.restore_holds"); got != 1 {
		t.Fatalf("daemon.promptqueue.restore_holds records in stderr = %d, want exactly 1\nstderr:\n%s", got, stderr)
	}
	if !strings.Contains(stderr, `"level":"error"`) {
		t.Fatalf("stderr = %q, want an ERROR-level record for the failed restore", stderr)
	}
}

// ---------------------------------------------------------------------------
// A held prompt mirrors to the TRAY ONLY
// ---------------------------------------------------------------------------

func TestAHeldPromptMirrorsToTheTrayOnlyUntilDelivery(t *testing.T) {
	// Arrange: a turn in flight, then an ordinary follow-up that is held.
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	holds := f.d.WatchHolds(f.ws)
	resp2 := f.submit("a held prompt", "k-held-mirror", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	awaitView(t, f, holds, "the held entry standing", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn2) != nil
	})

	// Assert: NO user_prompt row anywhere on the feed carries the held turn --
	// neither in the page a fresh open serves...
	feed := f.watchRootFeed()
	page, _ := f.openFeed(nil)
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == turn2.GetValue() {
			t.Fatalf("a held prompt's user_prompt row was already on the feed's page before delivery")
		}
	}
	// ...nor on the tail while it stays held.
	harness.ExpectNoPush(t, feed, harness.ProbeWindow, "a held prompt must not push a user_prompt row before delivery")

	// Act: the running turn ends, delivering the held prompt.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	f.shim.ExpectStartTurn()

	// Assert: EXACTLY one user_prompt row now carries the delivered turn.
	awaitRow(t, f, feed, "the delivered prompt's mirrored row", func(r *frontendv1.FeedRow) bool {
		return r.GetUserPrompt() != nil && r.GetTurn().GetValue() == turn2.GetValue()
	})
	page2, _ := f.openFeed(nil)
	count := 0
	for _, row := range page2.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == turn2.GetValue() {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("user_prompt rows carrying the delivered turn = %d, want exactly 1", count)
	}
}

// ---------------------------------------------------------------------------
// UpdateHeldPrompt.accept
// ---------------------------------------------------------------------------

func TestAcceptOnAHoldForTurnEndVerdictFlipsAcceptedAndRePushesTheTray(t *testing.T) {
	// Arrange: an ordinary follow-up prompt classifies hold_for_turn_end.
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("please also check the other file", "k-accept", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the hold_for_turn_end verdict", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, turn2)
		return p != nil && p.GetHoldForTurnEnd() != nil
	})

	// Act
	resp, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws,
		Turn:      turn2,
		Action:    &agentreplv1.UpdateHeldPromptRequest_Accept{Accept: &agentreplv1.UpdateHeldPromptAccept{}},
	}))
	if err != nil {
		t.Fatalf("UpdateHeldPrompt{accept} on a hold_for_turn_end verdict = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{accept} = %v, want a success", resp.Msg)
	}

	// Assert: the entry flips to accepted and the tray is re-pushed
	// (internal/promptqueue/holdactions.go's Accept).
	got := awaitView(t, f, holds, "the accepted flag on the re-pushed tray", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, turn2)
		return p != nil && p.GetHoldForTurnEnd().GetAccepted().GetAccepted()
	})
	if p := promptHeldEntry(got, turn2); !p.GetHoldForTurnEnd().GetAccepted().GetAccepted() {
		t.Fatalf("held entry after accept = %v, want hold_for_turn_end.accepted.accepted = true", p)
	}
}

func TestAcceptOnAnInterjectVerdictAnswersAcceptNotApplicable(t *testing.T) {
	// Arrange: the "stop" fast path classifies interject.
	f := newOpened(t, harness.Opts{})
	f.submit("start the long task", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("stop and rebase instead", "k-interject-accept", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the interject verdict", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, turn2)
		return p != nil && p.GetInterject() != nil
	})
	f.shim.ExpectKillTurn()

	// Act: accept is legal only on hold_for_turn_end
	// (internal/promptqueue/holdactions.go's Accept).
	resp, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws,
		Turn:      turn2,
		Action:    &agentreplv1.UpdateHeldPromptRequest_Accept{Accept: &agentreplv1.UpdateHeldPromptAccept{}},
	}))

	// Assert: `accept_not_applicable` is a LANDED arm, so the refusal is
	// typed.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt{accept} on an interject verdict = error %v, want the typed accept_not_applicable answer", err)
	}
	if resp.Msg.GetError().GetAcceptNotApplicable() == nil {
		t.Fatalf("UpdateHeldPrompt{accept} on an interject verdict = %v, want error.accept_not_applicable", resp.Msg)
	}
	// The queue's own Accept logs this refusal at WARNING regardless of the
	// arm being landed at the wire (internal/promptqueue/holdactions.go).
	f.d.ExpectWarnings("daemon.promptqueue.accept")
}

// ---------------------------------------------------------------------------
// A failed interject reverts to classification_error and FIFO order
// ---------------------------------------------------------------------------

func TestAFailedInterjectRevertsToClassificationErrorAndFifoOrder(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the long task", "k-running", origin)
	f.shim.ExpectStartTurn()
	footer := f.d.WatchFooter(f.ws)
	holds := f.d.WatchHolds(f.ws)
	// internal/promptqueue/classify.go's stripJump logs the failed-interject
	// ERROR under opInterject; nothing else warns on this path.
	f.d.ExpectWarnings("daemon.promptqueue.interject")

	// Act: the interjecting prompt's KillTurn is refused by the shim.
	f.shim.AnswerFailure(harness.RPCKillTurn, "the vendor refused the kill")
	resp2 := f.submit("stop and rebase instead", "k-stop-fail", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()

	// Assert: the entry reverts to classification_error and the footer's
	// interrupting status clears.
	tray := awaitView(t, f, holds, "the classification_error verdict", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, turn2)
		return p != nil && p.GetClassificationError() != nil
	})
	if p := promptHeldEntry(tray, turn2); p == nil || p.GetClassificationError() == nil {
		t.Fatalf("held entry for the failed interject = %v, want classification_error", p)
	}
	awaitFooter(t, f, footer, "the footer clears waiting.interrupting after the failed interject", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetInterrupting() == nil
	})

	// Act: submit a second, ordinary follow-up AFTER the failed interject, so
	// FIFO order (by queued_at) puts turn2 ahead of it -- the queue jump the
	// interject would have taken is stripped, and turn2 goes back into the
	// ordinary FIFO pool it was already the head of by submission order.
	resp3 := f.submit("second follow-up", "k-fifo-2", origin)
	turn3 := resp3.GetSuccess().GetTurn().GetTurn()

	// Assert: the original turn ends naturally (never interrupted, since the
	// interject failed), and delivery is FIFO: turn2 first, turn3 next.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	st2 := f.shim.ExpectStartTurn()
	if st2.GetTurn().GetValue() != turn2.GetValue() {
		t.Fatalf("first delivered turn after the failed interject = %q, want the FIFO head %q", st2.GetTurn().GetValue(), turn2.GetValue())
	}
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	st3 := f.shim.ExpectStartTurn()
	if st3.GetTurn().GetValue() != turn3.GetValue() {
		t.Fatalf("second delivered turn = %q, want the FIFO tail %q", st3.GetTurn().GetValue(), turn3.GetValue())
	}
}

// ---------------------------------------------------------------------------
// A merge lease's effect on SubmitPrompt
// ---------------------------------------------------------------------------

// promptMergeFixture opens a workspace on the daemon's own (self) repo, so a
// merge takes the full pre-prompt/conflict/parked path rather than the fast
// pre-prompt/post-prompt path every other repo takes — giving the test a
// merge lease that stays open for the assertions.
func promptMergeFixture(t *testing.T) (*fixture, *harness.Repo, string) {
	t.Helper()
	selfRepo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
	// The workspace is CREATED, never merely registered: a merge runs off the
	// creation job's recorded geometry and a registered worktree has none, so
	// a registered one is refused before any lease is ever taken.
	repoRef := mergeRepositoryRef(t, d, selfRepo)
	f := mergeCreateChild(t, d, repoRef, "feature", "feature work", nil)
	f.repo = selfRepo
	source := f.ws.GetDir()
	writeCommit(t, selfRepo, source, "feature.txt", "work\n")
	// The conflict is scripted where the merge RUNS -- the target worktree --
	// which for a top-level workspace is the repository's main worktree.
	selfRepo.ScriptConflict(selfRepo.Dir, "feature", "feature.txt")
	return f, selfRepo, source
}

// promptAwaitMergeLease blocks until the workspace's merge actually holds its
// lease, which is what a submission is refused against. Enqueuing is not
// holding: admission is the queue pump's own step.
func promptAwaitMergeLease(t *testing.T, f *fixture) {
	t.Helper()
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer's merging status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging() != nil
	})
}

func TestSubmitPromptDuringAMergeLeaseAnswersMergingRefusal(t *testing.T) {
	// Arrange
	f, _, _ := promptMergeFixture(t)
	if _, err := f.d.Client().MergeWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	promptAwaitMergeLease(t, f)

	// Act
	resp := f.submitRaw(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           said("work while merging"),
		IdempotencyKey: "k-merging",
		Origin:         origin,
	})

	// Assert: `merging` is a LANDED arm of SubmitPromptError, so the refusal
	// is a typed answer rather than a transport error.
	if resp.GetError().GetMerging() == nil {
		t.Fatalf("SubmitPrompt during a merge lease = %v, want error.merging", resp)
	}
	// The refusal IS the subject, and the queue records it at WARNING
	// (internal/promptqueue/submit.go). `merging` is a LANDED arm (refuse.go
	// maps promptqueue.ErrMerging to it), so the server's own refusal path
	// answers it at DEBUG, not WARN -- only the queue's own warning fires.
	f.d.ExpectWarnings("daemon.promptqueue.submit")
}

func TestPromptsHeldBeforeAMergeLeaseStayHeld(t *testing.T) {
	// Arrange: a prompt is held (turn in flight) BEFORE the merge begins.
	f, _, _ := promptMergeFixture(t)
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	resp2 := f.submit("a prompt held before the merge", "k-preheld", origin)
	turn2 := resp2.GetSuccess().GetTurn().GetTurn()
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the pre-merge held entry", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn2) != nil
	})

	// Act
	if _, err := f.d.Client().MergeWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the held entry is unaffected.
	got := awaitView(t, f, holds, "the held entry surviving the merge's start", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn2) != nil
	})
	if promptHeldEntry(got, turn2) == nil {
		t.Fatalf("a prompt held before the merge began was dropped once the merge started")
	}
}

// ---------------------------------------------------------------------------
// SubmitPrompt addressed to a subagent bubble
// ---------------------------------------------------------------------------

func TestSubmitPromptToASubagentBubbleDeliversViaUpdateAgentPrompt(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-1", "sub-1", "reviewing the diff")))
	row := awaitRow(t, f, feed, "the subagent bubble row", func(r *frontendv1.FeedRow) bool {
		return r.GetDetachedSubagent() != nil
	})

	// Act
	resp := f.submitRaw(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           said("please summarize your findings"),
		IdempotencyKey: "k-bubble",
		Origin:         origin,
		Feed:           row.GetId(),
	})

	// Assert
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt to a subagent bubble = %v, want a success", resp)
	}
	req := f.shim.ExpectUpdateAgent()
	if req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("UpdateAgent.target = %q, want the addressed subagent %q", req.GetTarget().GetValue(), "sub-1")
	}
	if text(req.GetInput().GetPrompt()) != "please summarize your findings" {
		t.Fatalf("UpdateAgent.input.prompt = %q, want the submitted text", text(req.GetInput().GetPrompt()))
	}
}

func TestASecondSubmitWhileATurnRunsOnTheSameAgentThroughTheBubblePathAnswersTheDaemonFaultRefusal(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-1", "sub-1", "reviewing the diff")))
	row := awaitRow(t, f, feed, "the subagent bubble row", func(r *frontendv1.FeedRow) bool {
		return r.GetDetachedSubagent() != nil
	})
	first := f.submitRaw(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said("first"), IdempotencyKey: "k-bubble-1", Origin: origin, Feed: row.GetId(),
	})
	if first.GetError() != nil {
		t.Fatalf("the first bubble submit = %v, want a success", first)
	}
	f.shim.ExpectUpdateAgent()

	// Act
	err := f.submitExpectingError(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said("second"), IdempotencyKey: "k-bubble-2", Origin: origin, Feed: row.GetId(),
	})

	// Assert
	// Project-lead ruling: the shim's turn_already_open on a bubble-addressed
	// submit has no SubmitPromptError home, and answers under the landing-7
	// candidate arm bubble_refused with kind agent_busy in the reason.
	if !namesIntendedArm(err, "SubmitPromptError.bubble_refused") ||
		!strings.Contains(err.Error(), "kind agent_busy") {
		t.Fatalf("a second bubble submit while the agent's turn runs = %v, want bubble_refused{kind agent_busy}", err)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

// ---------------------------------------------------------------------------
// Slash commands
// ---------------------------------------------------------------------------

func TestStatusAnswersAStatusPanelViewInlineAndMirrorsANonDurableCommandPanelRow(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()

	// Act
	resp := f.submit("/status", "k-status", origin)

	// Assert: answered inline, no StartTurn.
	status := resp.GetSuccess().GetCommandPanel().GetStatus()
	if status == nil {
		t.Fatalf("SubmitPrompt(/status) = %v, want a command_panel.status", resp)
	}
	if got := f.shim.Count(harness.RPCStartTurn); got != 0 {
		t.Fatalf("StartTurn count after /status = %d, want 0", got)
	}
	// The rows are asserted BY CONTENT (internal/server/panels.go's statusPanel
	// splices Account/Model/Permission mode from the topbar resolver's facts,
	// ahead of Version), not merely checked non-empty: the fake's default
	// config root states account "a@x" (SPEC.md's fake .claude.json), the fake
	// shim's StartSession answers effective_model "opus"
	// (integration/fakeshim/defaults.go's DefaultModel), and permission mode
	// "default" (integration/fakeshim/server.go's default StartSession
	// answer).
	wantLabels := []string{"Version", "Account", "Model", "Permission mode"}
	if len(status.GetRows()) != len(wantLabels) {
		t.Fatalf("status panel rows = %v, want exactly the %d rows %v", status.GetRows(), len(wantLabels), wantLabels)
	}
	for i, label := range wantLabels {
		if got := status.GetRows()[i].GetLabel(); got != label {
			t.Fatalf("status panel row %d label = %q, want %q (row order matters: Version first, then the spliced session facts)", i, got, label)
		}
	}
	if got := status.GetRows()[0].GetValue(); got == "" {
		t.Fatalf("status panel Version row value is empty, want the daemon's build stamp")
	}
	wantValues := map[string]string{"Account": "a@x", "Model": "opus", "Permission mode": "default"}
	for _, row := range status.GetRows()[1:] {
		if want := wantValues[row.GetLabel()]; row.GetValue() != want {
			t.Fatalf("status panel row %q value = %q, want %q", row.GetLabel(), row.GetValue(), want)
		}
	}
	awaitRow(t, f, feed, "the mirrored command_panel row", func(r *frontendv1.FeedRow) bool {
		return r.GetCommandPanel().GetStatus() != nil
	})

	// Act: restart, and the mirrored row must be gone from the first page.
	nd := promptRestartDaemon(t, f)
	page, _ := f.openFeedOn(nd)

	// Assert
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetCommandPanel() != nil {
			t.Fatalf("the command_panel row survived a daemon restart, want it non-durable")
		}
	}
}

func TestAgentsAndHelpAnswerCommandRefusedAndNeverReachTheShim(t *testing.T) {
	for _, cmd := range []string{"/agents", "/help"} {
		t.Run(cmd, func(t *testing.T) {
			// Arrange
			f := newOpened(t, harness.Opts{})
			feed := f.watchRootFeed()

			// Act
			resp := f.submit(cmd, "k-refused", origin)

			// Assert
			if got := resp.GetSuccess().GetCommandRefused().GetCommand(); got != cmd {
				t.Fatalf("SubmitPrompt(%s) = %v, want command_refused{command: %q}", cmd, resp, cmd)
			}
			if got := f.shim.Count(harness.RPCStartTurn); got != 0 {
				t.Fatalf("StartTurn count after %s = %d, want 0 (never reaches the shim)", cmd, got)
			}
			awaitRow(t, f, feed, "the mirrored command_refused row", func(r *frontendv1.FeedRow) bool {
				return r.GetCommandRefused().GetCommand().GetText() == cmd
			})
		})
	}
}

func TestAnUnknownSlashCommandFallsThroughAsAPrompt(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	text_ := "/frobnicate the whole build"

	// Act
	resp := f.submit(text_, "k-unknown", origin)

	// Assert
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt(%q) = %v, want an ordinary minted turn", text_, resp)
	}
	req := f.shim.ExpectStartTurn()
	if got := text(req.GetSaid()); got != text_ {
		t.Fatalf("StartTurn.said = %q, want the unrecognized command's literal text %q", got, text_)
	}
}

func TestClearAndCompactGoThroughTheQueueAsSessionActsAndProduceASeparationRow(t *testing.T) {
	for _, cmd := range []string{"/clear", "/compact"} {
		t.Run(cmd, func(t *testing.T) {
			// Arrange
			f := newOpened(t, harness.Opts{})
			feed := f.watchRootFeed()

			// Act
			resp := f.submit(cmd, "k-cut", origin)

			// Assert: the cut goes down the queue's ONE delivery path and
			// reaches the shim as a StartTurn carrying the literal -- shim.v1
			// has no other verb for a context cut, and the vendor's own CLI is
			// what answers /clear and /compact. It is answered with the TURN
			// it runs as, not with command_acted: that arm is for an act that
			// "mints no turn (/model <arg>)", and a cut mints and records one.
			if resp.GetError() != nil {
				t.Fatalf("SubmitPrompt(%s) = %v, want a success", cmd, resp)
			}
			if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
				t.Fatalf("SubmitPrompt(%s) = %v, want the turn the cut runs as", cmd, resp)
			}
			req := f.shim.ExpectStartTurn()
			if got := text(req.GetSaid()); got != cmd {
				t.Fatalf("StartTurn.said after %s = %q, want the command's literal", cmd, got)
			}

			// Act: the resulting cut arrives as a page line.
			f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
					Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
				}},
			}))

			// Assert
			awaitRow(t, f, feed, "the separation divider row", func(r *frontendv1.FeedRow) bool {
				return r.GetSeparation().GetCleared() != nil
			})
		})
	}
}

func TestModelWithAnArgumentSubmitsTheModelChange(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp := f.submit("/model sonnet", "k-model", origin)

	// Assert: a session-acting command that mints no turn answers
	// command_acted -- never a turn, and never command_panel or
	// command_refused (endpoint_submit_prompt.proto's own distinction).
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt(/model sonnet) = %v, want a success", resp)
	}
	if resp.GetSuccess().GetCommandActed() == nil {
		t.Fatalf("SubmitPrompt(/model sonnet) = %v, want success.command_acted", resp)
	}
	req := f.shim.ExpectSetSessionModel()
	if req.GetModel().GetName() != "sonnet" {
		t.Fatalf("SetSessionModel.model = %q, want %q", req.GetModel().GetName(), "sonnet")
	}
}

func TestBareModelIsRefusedOrAbsorbedWithoutChangingTheModel(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp := f.submit("/model", "k-bare-model", origin)

	// Assert: whichever of refused/absorbed the daemon picks, the model never
	// reaches the shim.
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt(/model) = %v, want a success (refused or absorbed, never a transport error)", resp)
	}
	if got := f.shim.Count(harness.RPCSetSessionModel); got != 0 {
		t.Fatalf("SetSessionModel count after bare /model = %d, want 0", got)
	}
}

func TestAModelChangeSubmittedWhileATurnRunsResolvesAtTheTurnBoundary(t *testing.T) {
	// Arrange: a turn in flight.
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()

	// Act: SetModel goes through the ONE delivery path
	// (internal/workspace/session_acts.go), so it cannot overtake the running
	// turn.
	resp, err := f.d.Client().SetModel(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetModelRequest{
		Workspace: f.ws,
		Model:     &conversationv1.AgentModel{Name: "sonnet"},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetModel while a turn runs = (%v, %v), want a success (the change is HELD, not refused)", resp, err)
	}

	// Assert: HELD, not sent mid-turn.
	if got := f.shim.Count(harness.RPCSetSessionModel); got != 0 {
		t.Fatalf("SetSessionModel count while the turn runs = %d, want 0 (queued for the turn boundary)", got)
	}

	// Act: the running turn ends.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the queued act is drained AT the boundary
	// (internal/promptqueue/lifecycle.go's OnTurnEnded calls drainActs before
	// popping the next prompt).
	req := f.shim.ExpectSetSessionModel()
	if req.GetModel().GetName() != "sonnet" {
		t.Fatalf("SetSessionModel.model = %q, want %q", req.GetModel().GetName(), "sonnet")
	}
}

// ---------------------------------------------------------------------------
// SetModel / SetPermissionMode
// ---------------------------------------------------------------------------

func TestSetModelWithACatalogTokenSendsSetSessionModelAndUpdatesTheTopbarOnlyOnModelChanged(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	awaitTopbar(t, f, topbar, "the initial model", func(v *frontendv1.TopbarView) bool {
		return v.GetModelSelector().GetSelected().GetModel().GetName() != ""
	})

	// Act
	resp, err := f.d.Client().SetModel(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetModelRequest{
		Workspace: f.ws,
		Model:     &conversationv1.AgentModel{Name: "sonnet"},
	}))
	if err != nil {
		t.Fatalf("SetModel(sonnet) = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetModel(sonnet) = %v, want a success", resp.Msg)
	}
	f.shim.ExpectSetSessionModel()

	// Assert: no push yet — the unary answer alone must not move the topbar.
	harness.ExpectNoPush(t, topbar, harness.ProbeWindow, "the topbar must not update before the shim pushes model_changed")

	// Act: the shim confirms it on the standing stream.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ModelChanged{ModelChanged: &conversationv1.SessionModelChanged{
			EffectiveModel: &conversationv1.AgentModel{Name: "sonnet"},
		}},
	})

	// Assert
	awaitTopbar(t, f, topbar, "the topbar updated to sonnet", func(v *frontendv1.TopbarView) bool {
		return v.GetModelSelector().GetSelected().GetModel().GetName() == "sonnet"
	})
}

func TestSetModelWithATokenNotInTheCatalogIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().SetModel(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetModelRequest{
		Workspace: f.ws,
		Model:     &conversationv1.AgentModel{Name: "not-a-real-model"},
	}))

	// Assert: `not_in_catalog` is a LANDED arm of SetModelError, so the
	// refusal is a typed answer rather than a transport error.
	if err != nil {
		t.Fatalf("SetModel(not-a-real-model) = error %v, want the typed not_in_catalog answer", err)
	}
	if resp.Msg.GetError().GetNotInCatalog() == nil {
		t.Fatalf("SetModel(not-a-real-model) = %v, want error.not_in_catalog", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestSetPermissionModeWithAServedModeSendsSetSessionPermissionModeAndUpdatesOnlyOnThePush(t *testing.T) {
	// Arrange: the served set is topbar.SwitchableModes, a FIXED six-mode set
	// the resolver installs regardless of what the fake session states, so
	// there is always an alternate to switch to -- the earlier t.Skip here
	// was dead code that could never fire.
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	first := awaitTopbar(t, f, topbar, "the initial permission-mode picker", func(v *frontendv1.TopbarView) bool {
		return len(v.GetPermissionModePicker().GetOptions()) > 0
	})
	wantModes := []string{"default", "accept_edits", "plan", "bypass", "dont_ask", "auto"}
	var gotModes []string
	for _, opt := range first.GetPermissionModePicker().GetOptions() {
		gotModes = append(gotModes, opt.GetMode())
	}
	if !sameOrder(gotModes, wantModes) {
		t.Fatalf("permission_mode_picker.options = %v, want exactly the six switchable modes in order %v", gotModes, wantModes)
	}
	if got := first.GetPermissionModePicker().GetCurrent().GetMode(); got != "default" {
		t.Fatalf("permission_mode_picker.current = %q, want the fake session's default mode %q", got, "default")
	}
	target := "accept_edits"

	// Act
	resp, err := f.d.Client().SetPermissionMode(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetPermissionModeRequest{
		Workspace: f.ws,
		Mode:      target,
	}))
	if err != nil {
		t.Fatalf("SetPermissionMode(%s) = error %v, want a success", target, err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetPermissionMode(%s) = %v, want a success", target, resp.Msg)
	}
	req := f.shim.ExpectSetSessionPermissionMode()

	// Assert: no push yet.
	harness.ExpectNoPush(t, topbar, harness.ProbeWindow, "the topbar must not update before the shim pushes permission_mode_changed")

	// Act: the shim echoes it as the standing fact.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_PermissionModeChanged{PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{
			PermissionMode: req.GetPermissionMode(),
		}},
	})

	// Assert
	awaitTopbar(t, f, topbar, "the topbar updated to the new mode", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent().GetMode() == target
	})
}

func TestSetPermissionModeWithAModeNotServedIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().SetPermissionMode(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetPermissionModeRequest{
		Workspace: f.ws,
		Mode:      "not-a-real-mode",
	}))

	// Assert: `mode_not_served` is a LANDED arm of SetPermissionModeError, so
	// the refusal is a typed answer rather than a transport error.
	if err != nil {
		t.Fatalf("SetPermissionMode(not-a-real-mode) = error %v, want the typed mode_not_served answer", err)
	}
	if resp.Msg.GetError().GetModeNotServed() == nil {
		t.Fatalf("SetPermissionMode(not-a-real-mode) = %v, want error.mode_not_served", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestSetPermissionModeUngatedWithoutConsentIsRefused(t *testing.T) {
	// Arrange: a registered (not created-with-consent) workspace, so an
	// ungated mode ("bypass" — the wire spelling this suite assumes mirrors
	// AgentPermissionMode's own oneof field name, per the report) is refused.
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().SetPermissionMode(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetPermissionModeRequest{
		Workspace: f.ws,
		Mode:      "bypass",
	}))

	// Assert: `ungated_without_consent` is a LANDED arm, so the refusal is a
	// typed answer. `bypass` IS in the served set (topbar.SwitchableModes), so
	// `mode_not_served` is impossible here -- it is asserted EXACTLY, not as
	// an OR with mode_not_served.
	if err != nil {
		t.Fatalf("SetPermissionMode(bypass) = error %v, want a typed refusal", err)
	}
	if resp.Msg.GetError().GetUngatedWithoutConsent() == nil {
		t.Fatalf("SetPermissionMode(bypass) without creation consent = %v, want exactly error.ungated_without_consent", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

// ---------------------------------------------------------------------------
// Interrupt
// ---------------------------------------------------------------------------

func TestInterruptTurnWithLiveDetachedAgentsAnswersConfirmRequiredWithTheCount(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-1", "sub-1", "reviewing the diff")))
	// The LIVE-WORK SET is what the interrupt's challenge counts, and the
	// watcher's own record of it is the edge that says it changed. The footer's
	// background chip cannot serve as the signal: a turn is in flight here, and
	// thinking outranks background in the status tree.
	awaitLiveWork(t, f, 1)

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert: confirm_required IS a landed arm, so it answers as one.
	if err != nil {
		t.Fatalf("Interrupt{turn} with live detached work = transport error %v, want the confirm_required arm", err)
	}
	challenge := resp.Msg.GetError().GetConfirmRequired()
	if challenge == nil {
		t.Fatalf("Interrupt{turn} with live detached work = %v, want InterruptError.confirm_required", resp.Msg)
	}
	if challenge.GetLiveAgentCount() != 1 {
		t.Fatalf("confirm_required.live_agent_count = %d, want 1", challenge.GetLiveAgentCount())
	}
	// `confirm_required` is a LANDED arm (server/refuse.go's asRefusal special-
	// cases *workspace.ConfirmRequired), so the server's own answer is DEBUG;
	// the WARN comes from internal/workspace/interrupt.go's own
	// log.Warn(opInterrupt, ...) before it constructs the challenge.
	f.d.ExpectWarnings("daemon.workspace.interrupt")
}

func TestResendingInterruptWithConfirmAgentsStopsThem(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-1", "sub-1", "reviewing the diff")))
	// The LIVE-WORK SET is what the interrupt's challenge counts, and the
	// watcher's own record of it is the edge that says it changed. The footer's
	// background chip cannot serve as the signal: a turn is in flight here, and
	// thinking outranks background in the status tree.
	awaitLiveWork(t, f, 1)
	first, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil || first.Msg.GetError().GetConfirmRequired() == nil {
		t.Fatalf("the first Interrupt{turn} = (%v, %v), want confirm_required to set up the challenge", first.Msg, err)
	}
	f.d.ExpectWarnings("daemon.workspace.interrupt")

	// Act
	_, err = f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace:     f.ws,
		Target:        &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		ConfirmAgents: true,
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{turn, confirm_agents} = error %v, want a success", err)
	}
	f.shim.ExpectKillTurn()
	if got := f.shim.ExpectUpdateAgent(); got.GetInput().GetStop() == nil {
		t.Fatalf("the confirmed interrupt's UpdateAgent = %v, want the stop arm on the detached subagent", got)
	}
}

// TestInterruptTurnWithOnlyADetachedShellNeedsNoConfirmation is the other side
// of the challenge's contract: `live_agent_count` counts AGENTS, and a
// detached shell is not one. It dies with the query like anything else, but
// the user is not challenged over it.
func TestInterruptTurnWithOnlyADetachedShellNeedsNoConfirmation(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-1", "sleep 5")))
	awaitLiveWork(t, f, 1)

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{turn} with only a detached shell = error %v, want a success", err)
	}
	if challenge := resp.Msg.GetError().GetConfirmRequired(); challenge != nil {
		t.Fatalf("Interrupt{turn} with only a detached shell = confirm_required(%d), want no challenge", challenge.GetLiveAgentCount())
	}
	f.shim.ExpectKillTurn()
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestInterruptWithNothingRunningAnswersNothingRunning(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt with nothing running = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("Interrupt with nothing running = %v, want the nothing_running answer", resp.Msg)
	}
}

func TestInterruptDetachedStopsTheNamedWorkAndAnUnknownFeedIdAnswersNotDetachedWork(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-1", "sleep 5")))
	row := awaitRow(t, f, feed, "the detached shell bubble row", func(r *frontendv1.FeedRow) bool {
		return r.GetDetachedShell() != nil
	})
	// Neither branch below logs a WARN: the success path is Info
	// (internal/workspace/interrupt.go's interruptDetached), and
	// not_detached_work is answered directly at the server layer
	// (internal/server/answers.go's askIDFrom caller), a LANDED arm logged
	// at DEBUG.
	f.d.ExpectWarnings()

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Detached{Detached: row.GetId()},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{detached} on a live shell = error %v, want a success", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedDetached().GetCount(); got != 1 {
		t.Fatalf("InterruptedDetached.count = %d, want 1", got)
	}
	f.shim.ExpectStopBash()

	// Act: an unknown FeedId names no detached work.
	resp2, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Detached{Detached: &frontendv1.FeedId{Value: "not-a-real-feed-id"}},
	}))

	// Assert: `not_detached_work` is a LANDED arm, so the refusal is typed.
	if err != nil {
		t.Fatalf("Interrupt{detached} on an unknown feed id = error %v, want the typed not_detached_work answer", err)
	}
	if resp2.Msg.GetError().GetNotDetachedWork() == nil {
		t.Fatalf("Interrupt{detached} on an unknown feed id = %v, want error.not_detached_work", resp2.Msg)
	}
}

func TestInterruptAllAgentsStopsEveryLiveDetachedAgent(t *testing.T) {
	// Arrange: interruptAllAgents sweeps AGENTS only, so a lone detached
	// subagent is the fixture (internal/workspace/interrupt.go).
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-1", "sub-1", "reviewing the diff")))
	awaitRow(t, f, feed, "the subagent bubble row", func(r *frontendv1.FeedRow) bool {
		return r.GetDetachedSubagent() != nil
	})
	awaitLiveWork(t, f, 1)
	f.d.ExpectWarnings()

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{all_agents} = error %v, want a success", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedDetached().GetCount(); got != 1 {
		t.Fatalf("InterruptedDetached.count = %d, want 1 (the one live detached agent)", got)
	}
	req := f.shim.ExpectUpdateAgent()
	if req.GetTarget().GetValue() != "sub-1" {
		t.Fatalf("UpdateAgent.target = %q, want the live subagent %q", req.GetTarget().GetValue(), "sub-1")
	}
	if req.GetInput().GetStop() == nil {
		t.Fatalf("UpdateAgent.input = %v, want a stop", req.GetInput())
	}
}

// ---------------------------------------------------------------------------
// AnswerPermission
// ---------------------------------------------------------------------------

func TestAnswerPermissionForwardsTheCorrectDecision(t *testing.T) {
	t.Run("allow_once", func(t *testing.T) {
		f := newOpened(t, harness.Opts{})
		feed := f.watchRootFeed()
		f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-once", "act-once")},
		}))
		row := awaitRow(t, f, feed, "the permission card", func(r *frontendv1.FeedRow) bool { return r.GetPermission() != nil })

		resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace: f.ws, Permission: row.GetId(),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowOnce{AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{}},
		}))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Fatalf("AnswerPermission{allow_once} = %v, %v, want a success", resp, err)
		}
		req := f.shim.ExpectUpdateAgent()
		dec := req.GetInput().GetAnswer().GetPermissionDecision()
		if dec.GetAsk().GetValue() != "perm-once" {
			t.Fatalf("UpdateAgent decision.ask = %q, want %q", dec.GetAsk().GetValue(), "perm-once")
		}
		if dec.GetAllowed().GetOnce() == nil {
			t.Fatalf("UpdateAgent decision = %v, want allowed.once", dec)
		}
	})

	t.Run("allow_standing echoes the daemon-held offer", func(t *testing.T) {
		f := newOpened(t, harness.Opts{})
		feed := f.watchRootFeed()
		offer := standingPermission("perm-standing", "act-standing")
		f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: offer},
		}))
		row := awaitRow(t, f, feed, "the standing-offer permission card", func(r *frontendv1.FeedRow) bool { return r.GetPermission() != nil })

		resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace: f.ws, Permission: row.GetId(),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowStanding{AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{}},
		}))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Fatalf("AnswerPermission{allow_standing} = %v, %v, want a success", resp, err)
		}
		req := f.shim.ExpectUpdateAgent()
		dec := req.GetInput().GetAnswer().GetPermissionDecision()
		got := dec.GetAllowed().GetStanding().GetStanding()
		want := offer.GetStart().GetOfferedStanding()
		if !proto.Equal(got, want) {
			t.Fatalf("UpdateAgent standing = %v, want the daemon-held offer echoed unchanged %v", got, want)
		}
	})

	t.Run("deny with a reason", func(t *testing.T) {
		f := newOpened(t, harness.Opts{})
		feed := f.watchRootFeed()
		f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-deny", "act-deny")},
		}))
		row := awaitRow(t, f, feed, "the permission card", func(r *frontendv1.FeedRow) bool { return r.GetPermission() != nil })

		resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace: f.ws, Permission: row.GetId(),
			Answer: &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{
				Reason: &agentreplv1.AnswerPermissionDenyReason{Text: "not needed"},
			}},
		}))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Fatalf("AnswerPermission{deny} = %v, %v, want a success", resp, err)
		}
		req := f.shim.ExpectUpdateAgent()
		dec := req.GetInput().GetAnswer().GetPermissionDecision()
		if dec.GetDenied() == nil || dec.GetDenied().GetMessage() != "not needed" {
			t.Fatalf("UpdateAgent decision = %v, want denied{message: %q}", dec, "not needed")
		}
	})
}

func TestAllowStandingOnACardWithoutStandingOfferedIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-no-standing", "act-1")},
	}))
	row := awaitRow(t, f, feed, "the permission card with no standing offer", func(r *frontendv1.FeedRow) bool { return r.GetPermission() != nil })

	// Act
	resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace: f.ws, Permission: row.GetId(),
		Answer: &agentreplv1.AnswerPermissionRequest_AllowStanding{AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{}},
	}))

	// Assert: `no_standing_offer` is a LANDED arm, so the refusal is typed.
	if err != nil {
		t.Fatalf("AnswerPermission = error %v, want the typed no_standing_offer answer", err)
	}
	if resp.Msg.GetError().GetNoStandingOffer() == nil {
		t.Fatalf("AnswerPermission{allow_standing} without standing_offered = %v, want error.no_standing_offer", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

// ---------------------------------------------------------------------------
// AnswerQuestion
// ---------------------------------------------------------------------------

// promptOpenQuestion builds a single-select question batch of one.
func promptOpenQuestion(id, question string, options ...string) *conversationv1.AgentQuestion {
	var opts []*conversationv1.AgentQuestionOption
	for _, o := range options {
		opts = append(opts, &conversationv1.AgentQuestionOption{Label: &conversationv1.AgentQuestionOptionLabel{Label: o}})
	}
	return &conversationv1.AgentQuestion{
		Id: &conversationv1.AgentQuestionId{Value: id},
		Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
			Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
				Question: &conversationv1.AgentQuestionText{Text: question},
				Header:   "Choice",
				Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
					Options: opts,
				}},
			}}},
			StartedAt: startedAt(1_700_000_000_000),
		}},
	}
}

func TestAnswerQuestionEchoesServedTextsAndLabels(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: promptOpenQuestion("q-1", "Pick one", "A", "B")},
	}))
	row := awaitRow(t, f, feed, "the question card", func(r *frontendv1.FeedRow) bool { return r.GetQuestion() != nil })

	// Act
	resp, err := f.d.Client().AnswerQuestion(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: f.ws, Question: row.GetId(),
		Answers: []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"A"}}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerQuestion = %v, %v, want a success", resp, err)
	}
	req := f.shim.ExpectUpdateAgent()
	ans := req.GetInput().GetAnswer().GetQuestionAnswer()
	if ans.GetAsk().GetValue() != "q-1" {
		t.Fatalf("UpdateAgent answer.ask = %q, want %q", ans.GetAsk().GetValue(), "q-1")
	}
	sels := ans.GetAnswers().GetAnswers()
	if len(sels) != 1 || sels[0].GetQuestion().GetText() != "Pick one" {
		t.Fatalf("UpdateAgent answer selections = %v, want one echoing %q", sels, "Pick one")
	}
	if len(sels[0].GetChosen()) != 1 || sels[0].GetChosen()[0].GetLabel().GetLabel() != "A" {
		t.Fatalf("UpdateAgent chosen = %v, want [%q]", sels[0].GetChosen(), "A")
	}
}

func TestAnswerQuestionWithAnUnservedLabelIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: promptOpenQuestion("q-2", "Pick one", "A", "B")},
	}))
	row := awaitRow(t, f, feed, "the question card", func(r *frontendv1.FeedRow) bool { return r.GetQuestion() != nil })

	// Act
	resp, err := f.d.Client().AnswerQuestion(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: f.ws, Question: row.GetId(),
		Answers: []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"never served"}}},
	}))

	// Assert: `unserved_value` is a LANDED arm, so the refusal is typed.
	if err != nil {
		t.Fatalf("AnswerQuestion = error %v, want the typed unserved_value answer", err)
	}
	if resp.Msg.GetError().GetUnservedValue() == nil {
		t.Fatalf("AnswerQuestion with an unserved label = %v, want error.unserved_value", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestMultiPickOnSingleSelectIsRefused(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: promptOpenQuestion("q-3", "Pick one", "A", "B")},
	}))
	row := awaitRow(t, f, feed, "the question card", func(r *frontendv1.FeedRow) bool { return r.GetQuestion() != nil })

	// Act
	resp, err := f.d.Client().AnswerQuestion(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: f.ws, Question: row.GetId(),
		Answers: []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"A", "B"}}},
	}))

	// Assert: `multi_pick_on_single_select` is a LANDED arm, so it is typed.
	if err != nil {
		t.Fatalf("AnswerQuestion = error %v, want the typed refusal", err)
	}
	if resp.Msg.GetError().GetMultiPickOnSingleSelect() == nil {
		t.Fatalf("AnswerQuestion multi-pick on a single_select = %v, want error.multi_pick_on_single_select", resp.Msg)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestAnswerQuestionWhenNoAskIsStandingAnswersAskNotStanding(t *testing.T) {
	// Arrange: no question was ever posed, so the named feed row addresses
	// nothing standing.
	f := newOpened(t, harness.Opts{})

	// Act: an undecodable FeedId takes the server's own askIDFrom path
	// (internal/server/answers.go), never reaching workspace.refuse -- no
	// component here logs a WARN.
	f.d.ExpectWarnings()
	resp, err := f.d.Client().AnswerQuestion(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: f.ws,
		Question:  &frontendv1.FeedId{Value: "not-a-real-feed-id"},
		Answers:   []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"A"}}},
	}))

	// Assert: `ask_not_standing` is a LANDED arm, so the refusal is typed.
	if err != nil {
		t.Fatalf("AnswerQuestion on a card that never stood = error %v, want the typed ask_not_standing answer", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("AnswerQuestion on a card that never stood = %v, want error.ask_not_standing", resp.Msg)
	}
}

func TestAQuestionThatExpiresIsDrawnExpired(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: promptOpenQuestion("q-expire", "Pick one", "A", "B")},
	}))
	awaitRow(t, f, feed, "the open question card", func(r *frontendv1.FeedRow) bool { return r.GetQuestion() != nil })

	// Act: the ask goes unanswered and the producer's idle timeout closes it
	// (conversation.v1.AgentQuestionSuccess_Unanswered).
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-expire"},
			Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
				Outcome: &conversationv1.AgentQuestionSuccess_Unanswered{Unanswered: &conversationv1.AgentQuestionUnanswered{}},
			}},
		}},
	}))

	// Assert: drawn as expired (internal/resolve/feed/question.go's
	// drawQuestion), never as pending forever.
	awaitRow(t, f, feed, "the expired question card", func(r *frontendv1.FeedRow) bool {
		return r.GetQuestion().GetExpired() != nil
	})
}

// ---------------------------------------------------------------------------
// prompt_test.go helpers
// ---------------------------------------------------------------------------

// promptRestartDaemon stops the fixture's daemon and starts a fresh one on
// the same state root (so wsm.db, and this test's fake-git world, persist),
// updating the fixture's own daemon in place.
func promptRestartDaemon(t *testing.T, f *fixture) *harness.Daemon {
	t.Helper()
	f.d.Stop()
	nd := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	f.d = nd
	return nd
}

// openFeedOn opens the root feed on an explicit daemon (for the tests that
// reopen it against a restarted one).
func (f *fixture) openFeedOn(d *harness.Daemon) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken) {
	f.t.Helper()
	resp, err := d.Client().OpenFeed(d.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: f.ws}))
	if err != nil {
		f.t.Fatalf("OpenFeed = error %v, want a page and a token", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		f.t.Fatalf("OpenFeed = %v, want a success", resp.Msg)
	}
	return success.GetPage(), success.GetWatch()
}

// awaitView is AwaitView bound to the fixture's daemon context.
func awaitView[T any](t *testing.T, f *fixture, s *harness.Stream[T], what string, pred func(T) bool) T {
	t.Helper()
	return harness.AwaitView(t, f.d.Ctx(), s, what, pred)
}
