// producerfaults_e2e_test.go — the PRODUCER-FAULT area: the vendor query
// dying under a turn, the shim's converter refusing one malformed vendor
// message, and the bookkeeping records that are deliberately drawn as
// NOTHING.
//
// Every test here drives a real fake-SDK scenario through the real
// shim/daemon/store/sidecar stack (SPEC.md section B) and asserts the ONE
// contract arm that scenario's own `arms:` line names. No test writes a
// store row, a transcript line or a wire frame by hand.
//
// The families, and the contract each is written against:
//
//   - `!query-eof` / `!query-fail` — conversation/v1/session.proto's
//     SessionQueryDied ("The query ended on its own. THE ARM IS HOW"), which
//     the daemon's sessionwatcher routes (route.go's routeQueryDiedLocked:
//     "an open turn will never get a terminal now") into
//     frontend/v1/feed.proto's FeedTurnEndedErrored.query_died ("The query
//     process died out from under the turn") and footer.proto's
//     FooterSubStatusBlockedQueryDied ("The vendor query died while the shim
//     stayed healthy and nothing has restarted it").
//   - `!query-eof-mid-ask` — the same death with a `canUseTool` ask still
//     open. The scenario's own arms line: "SessionQueryDied.cause=
//     unexpected_eof with an AgentPermission settling denied", so the ask
//     must not be left open forever.
//   - `!fault-converter` / `!fault-recover` — session.proto's
//     SessionFaultConverterDefect carried on SessionDiagnostics, which the
//     daemon projects into topbar.proto's TopbarDegradedWindowWarningDetail
//     (open, then closed). The malformed message itself reaches NO
//     conversation.v1 arm, which is the point.
//   - `!keepalive`, `!residue`, `!away-summary`, `!context-tip`,
//     `!tokens-reminder` — turns whose extra records are vendor bookkeeping:
//     store.proto's StoreUnservedItem.vendor_specific residue, drawn by no
//     feed row at all. Each test asserts the SPECIFIC negative (the turn's
//     row set is exactly prompt + response + terminal) the way
//     TestHookSucceeded asserts "the feed drew NO hook card".
//   - `!context-window` — the exception in that group: it is not
//     bookkeeping but a terminal, AgentFailure.prompt_too_long, which
//     turnended.go draws as FeedTurnEndedErrored.request_too_large.
//
// NAMING: every unexported helper here is prefixed `pf` (area tag) — all
// area files compile into one Go package and a bare generic name has
// collided across parallel writers before.
package e2e

import (
	"context"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
)

// ===========================================================================
// Shared helpers (area-local).
// ===========================================================================

// pfNewWorkspace registers a fresh scripted-fake-git repository and opens it
// on the daemon, answering the ref and the account root it routes through.
// Same fixture shape as the compaction area's cpNewWorkspace; duplicated
// rather than shared because SPEC.md section E forbids an area file from
// touching another area's file.
func pfNewWorkspace(t *testing.T, w *World) (*workspacev1.WorkspaceRef, string) {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return ws, w.DefaultConfigDir
}

// pfAwaitView bounds any view-stream wait in this file at DefaultTimeout —
// every wait here chains at most one real turn through the real stack, so
// the ordinary per-test budget is the right one and no wait in this file
// gets a hand-picked longer window.
func pfAwaitView[V any](t *testing.T, w *World, s *harness.Stream[V], what string, pred func(V) bool) V {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// pfRowsOfTurn answers the rows of a feed snapshot that belong to the given
// turn, in page order. Separation dividers (whose FeedRow.turn is UNSET per
// feed.proto) are excluded by construction, which is correct: they belong to
// no turn and none of these scenarios draws one.
func pfRowsOfTurn(rows []*frontendv1.FeedRow, turn *conversationv1.TurnId) []*frontendv1.FeedRow {
	var out []*frontendv1.FeedRow
	for _, row := range rows {
		if row.GetTurn().GetValue() == turn.GetValue() {
			out = append(out, row)
		}
	}
	return out
}

// pfAssertOnlyProseRows is the SPECIFIC NEGATIVE the bookkeeping scenarios
// are about: the turn drew its prompt, its prose response and its terminal —
// and NOTHING ELSE. A residue record that had wrongly reached a
// conversation.v1 arm would land here as an extra row (a tool call, a hook
// card, a permission card, an artifact), so this is a real assertion rather
// than a restatement of the turn's success.
//
// store.proto: residue is "material the producer could not convert at all",
// and `!residue`'s own arms line reads "NONE — ... dropped from every page
// by both planes".
func pfAssertOnlyProseRows(t *testing.T, rows []*frontendv1.FeedRow, turn *conversationv1.TurnId) {
	t.Helper()
	mine := pfRowsOfTurn(rows, turn)
	if len(mine) == 0 {
		t.Fatalf("feed drew no rows at all for turn %s", turn.GetValue())
	}
	var sawPrompt, sawResponse, sawEnded bool
	for _, row := range mine {
		switch {
		case row.GetUserPrompt() != nil:
			sawPrompt = true
		case row.GetActivity().GetResponse() != nil:
			sawResponse = true
		case row.GetTurnEnded() != nil:
			sawEnded = true
		default:
			t.Errorf("feed drew an extra row %v for a bookkeeping-only turn, want prompt/response/terminal only", row)
		}
	}
	if !sawPrompt {
		t.Errorf("feed drew no user_prompt row for turn %s, want the prompt bubble", turn.GetValue())
	}
	if !sawResponse {
		t.Errorf("feed drew no response row for turn %s, want the scenario's prose", turn.GetValue())
	}
	if !sawEnded {
		t.Errorf("feed drew no turn_ended row for turn %s, want the terminal", turn.GetValue())
	}
}

// pfOpenDegradedWindow answers the first OPEN degraded-window warning on a
// topbar view, or nil.
func pfOpenDegradedWindow(v *frontendv1.TopbarView) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, warn := range v.GetWarnings().GetWarnings() {
		if dw := warn.GetDegradedWindow(); dw != nil && dw.GetOpen() != nil {
			return dw
		}
	}
	return nil
}

// pfClosedDegradedWindow answers the CLOSED degraded-window warning naming
// the SAME component and began_at_ms as `open` — that window, recovered.
func pfClosedDegradedWindow(v *frontendv1.TopbarView, open *frontendv1.TopbarDegradedWindowWarningDetail) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, warn := range v.GetWarnings().GetWarnings() {
		dw := warn.GetDegradedWindow()
		if dw == nil || dw.GetClosed() == nil {
			continue
		}
		if dw.GetComponent().GetText() == open.GetComponent().GetText() && dw.GetBeganAtMs() == open.GetBeganAtMs() {
			return dw
		}
	}
	return nil
}

// pfQueryDiedWarnings names the daemon operations a dying vendor query
// legitimately logs above INFO, so the harness's unconditional warning sweep
// does not fail a test for the very fault it provoked: the session watcher's
// own routing (route.go: `w.log.Error("daemon.sessionwatcher.query_died"...)`),
// the feed resolver's terminal draw, and the workspace health fault the
// death opens.
func pfQueryDiedWarnings(w *World) {
	w.ExpectWarnings(
		"daemon.sessionwatcher.query_died",
		"daemon.feed.query_died",
		"daemon.health.open_fault",
	)
}

// ===========================================================================
// The vendor query dying: `!query-eof`, `!query-fail`, `!query-eof-mid-ask`.
// ===========================================================================

// TestQueryEofEndsTheTurnAsQueryDied drives `!query-eof` — the fake vendor's
// iterable ENDING with no result, "the CLI going away cleanly mid-turn"
// (lifecycle.ts), whose arm is SessionQueryDied.cause=unexpected_eof.
//
// The turn it kills never produced a vendor result, so its terminal is not
// the vendor's: it is the daemon's, drawn from the session's death
// (route.go's routeQueryDiedLocked, "an open turn will never get a terminal
// now"). feed.proto's arm for exactly that is
// FeedTurnEndedErrored.query_died.
func TestQueryEofEndsTheTurnAsQueryDied(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	pfQueryDiedWarnings(w)
	ws, _ := pfNewWorkspace(t, w)

	// Act
	turn := SubmitPrompt(t, w, ws, "!query-eof")

	// Assert: the terminal is specifically the query-died arm, not a plain
	// conclusion and not a generic vendor failure.
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetConcluded() != nil {
		t.Fatalf("turn ended = %v, want an errored terminal (the query died mid-turn)", ended)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("turn ended = %v, want the errored arm", ended)
	}
	if errored.GetQueryDied() == nil {
		t.Fatalf("turn error = %v, want feed.proto's query_died arm", errored)
	}
	// Landing 10 gave the arm a cause; an EOF is the agent binary vanishing.
	if errored.GetQueryDied().GetUnexpectedEof() == nil {
		t.Errorf("query_died cause = %v, want unexpected_eof", errored.GetQueryDied())
	}
	if errored.GetHeadline().GetText() == "" {
		t.Errorf("turn error headline = %q, want the daemon's composed sentence (FeedTurnErrorHeadline)", errored.GetHeadline().GetText())
	}
}

// TestQueryDiedRaisesTheFootersTurnDiedFault is the same death seen on the
// FOOTER. A dead query is agent-repl's fault, not a block (owner rulings,
// 2026-09-28 and 2026-10-06): the strip draws `agent_repl_fault · turn_died`,
// the same fault the roster draws `turn_died`, and its activity line is the
// daemon's per-cause sentence (FooterStatusActivityTurnEnded), the one the
// feed's turn-end row carries as its headline.
func TestQueryDiedRaisesTheFootersTurnDiedFault(t *testing.T) {
	t.Parallel()
	// Arrange: the footer stream is opened BEFORE the prompt so the failed
	// turn's push is queued in order rather than possibly missed.
	w := NewWorld(t, WorldOpts{})
	pfQueryDiedWarnings(w)
	ws, _ := pfNewWorkspace(t, w)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	SubmitPrompt(t, w, ws, "!query-eof")

	// Assert
	view := pfAwaitView(t, w, footer.Stream, "the footer's turn_died fault after the query died", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetTurnDied() != nil
	})
	fault := view.GetStrip().GetStatus().GetAgentReplFault()
	if got := fault.GetActivity().GetSalient().GetTurnEnded().GetText(); got == "" {
		t.Errorf("footer turn_died activity turn_ended text = %q, want the daemon's per-cause sentence", got)
	}
}

// TestQueryFailEndsTheTurnAsQueryDied drives `!query-fail` — the iterable
// REJECTING rather than ending, "the producer died rather than finished"
// (lifecycle.ts), whose arm is SessionQueryDied.cause=iterator_failure.
//
// The CAUSE was once observable on conversation/v1's wire alone; landing 10
// gave FeedTurnErrorQueryDied its own `cause` oneof mirroring
// SessionQueryDied's, so the distinction the writer noted as unobservable now
// is one — and this test pins the iterator-failure half of it, separately
// from TestQueryEofEndsTheTurnAsQueryDied's EOF.
func TestQueryFailEndsTheTurnAsQueryDied(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	pfQueryDiedWarnings(w)
	ws, _ := pfNewWorkspace(t, w)

	// Act
	turn := SubmitPrompt(t, w, ws, "!query-fail")

	// Assert
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("turn ended = %v, want the errored arm (the query's iterator threw)", ended)
	}
	if errored.GetQueryDied() == nil {
		t.Fatalf("turn error = %v, want feed.proto's query_died arm", errored)
	}
	if errored.GetQueryDied().GetIteratorFailure() == nil {
		t.Errorf("query_died cause = %v, want iterator_failure (the SDK's iterator threw)", errored.GetQueryDied())
	}
}

// TestQueryEofMidAskDeniesTheOpenAsk drives `!query-eof-mid-ask`: the query
// dies with a `canUseTool` ask STILL OPEN. The scenario's whole point, in
// its own words, is that "an unresolved `canUseTool` promise wedges the
// vendor process, so the query-death path owes every pending callback a
// denial" — its arms line is "SessionQueryDied.cause=unexpected_eof with an
// AgentPermission settling denied".
//
// The DENIER is the gate's stand-down, which settles every pending ask as
// AgentPermissionDenied.by=user carrying the teardown's reason
// (permission-gate.ts's standDown), so the frontend arm this asserts is
// FeedPermissionAnswered.denied_by_user. Reported, not weakened: nothing in
// permission.proto reserves a separate "denied because the producer died"
// arm, so the user arm is what the contract offers.
func TestQueryEofMidAskDeniesTheOpenAsk(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	pfQueryDiedWarnings(w)
	ws, _ := pfNewWorkspace(t, w)

	// Act
	turn := SubmitPrompt(t, w, ws, "!query-eof-mid-ask")

	// Assert: the ask genuinely OPENED before the death — the scenario
	// insists on that ordering, and an ask that never opened would make the
	// denial below vacuous.
	askRow := awaitFeedRow(t, w, ws, "the permission ask opened before the query died", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetPermission() != nil
	})
	if askRow.GetPermission().GetOpen() == nil && askRow.GetPermission().GetAnswered() == nil {
		t.Fatalf("permission row = %v, want either the open ask or its settled answer", askRow.GetPermission())
	}

	// Assert: it does not stay open — the death owes it a denial.
	settled := awaitFeedRow(t, w, ws, "the open ask settling denied when the query died", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == askRow.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	answer := settled.GetPermission().GetAnswered()
	if answer.GetDeniedByUser() == nil {
		t.Fatalf("settled permission = %v, want denied_by_user (the gate's stand-down denies every pending ask)", answer)
	}

	// Assert: and the turn itself still ends on the query-died arm.
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetErrored().GetQueryDied() == nil {
		t.Fatalf("turn ended = %v, want the query_died arm", ended)
	}
}

// ===========================================================================
// The converter fault window: `!fault-converter`, `!fault-recover`.
// ===========================================================================

// TestConverterDefectOpensADegradedWindow drives `!fault-converter`: ONE
// malformed vendor message (a `hook_started` whose `hook_id` is the empty
// string — "an identity the converter requires and refuses to invent") in an
// otherwise ordinary, SUCCEEDING turn.
//
// Two facts, both from the scenario's own arms line ("SessionFault.
// converter_defect with an OPEN SessionDegradedWindow — the diagnostics arm,
// reached without any rpc failing. The malformed message itself reaches NO
// conversation.v1 arm, which is the point"):
//
//  1. the turn still CONCLUDES — a defective vendor message does not stop a
//     turn (failures.ts's own header);
//  2. the malformed hook draws NO hook card, and the shim's diagnostics
//     carry an OPEN degraded window, which the daemon projects onto the
//     topbar as a TopbarDegradedWindowWarningDetail with `open` set.
//
// As in the store-outage area, the component/reason strings are NOT pinned:
// topbar.proto commits only to "a component degraded, for a stated reason".
func TestConverterDefectOpensADegradedWindow(t *testing.T) {
	t.Parallel()
	// Arrange: a converter defect is exactly a workspace health fault.
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.health.open_fault")
	ws, configDir := pfNewWorkspace(t, w)
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "fault-converter")

	// Assert: the turn ended ordinarily.
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (an unconvertible message never stops a turn)", ended)
	}
	// Assert: the malformed hook announcement reached NO arm — no hook card.
	if h := hookRow(rows); h != nil {
		t.Fatalf("feed drew a hook card %v from the MALFORMED hook_started, want none (it reaches no conversation.v1 arm)", h)
	}
	// Assert: the diagnostics arm — an OPEN degraded window on the topbar.
	view := pfAwaitView(t, w, topbar, "an open degraded window from the converter defect", func(v *frontendv1.TopbarView) bool {
		return pfOpenDegradedWindow(v) != nil
	})
	open := pfOpenDegradedWindow(view)
	if open == nil {
		t.Fatalf("topbar push matched an open degraded window but re-scanning it found none")
	}
	if open.GetComponent().GetText() == "" {
		t.Errorf("degraded window component = %q, want a stated component (TopbarWarningComponent)", open.GetComponent().GetText())
	}
	if open.GetReason().GetText() == "" {
		t.Errorf("degraded window reason = %q, want a stated reason (TopbarWarningDetailLine)", open.GetReason().GetText())
	}
	if open.GetBeganAtMs() <= 0 {
		t.Errorf("degraded window began_at_ms = %d, want a positive epoch-ms timestamp", open.GetBeganAtMs())
	}
}

// TestConverterRecoveryClosesTheDegradedWindow is the pair's second half:
// after the defect, `!fault-recover` sends the SAME hook announcement
// WELL-FORMED, and `fault-recover`'s arms line is "the diagnostics returning
// to HEALTHY with the degraded window CLOSED carrying the dropped count the
// fault left behind".
//
// The window is identified across the transition exactly as the store-outage
// area identifies its own: by component plus began_at_ms, so this asserts
// THAT window closed rather than merely that some closed window exists.
func TestConverterRecoveryClosesTheDegradedWindow(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.health.open_fault")
	ws, configDir := pfNewWorkspace(t, w)
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()

	driveScenarioToCompletion(t, w, ws, configDir, "fault-converter")
	openedView := pfAwaitView(t, w, topbar, "an open degraded window from the converter defect", func(v *frontendv1.TopbarView) bool {
		return pfOpenDegradedWindow(v) != nil
	})
	opened := pfOpenDegradedWindow(openedView)
	if opened == nil {
		t.Fatalf("topbar push matched an open degraded window but re-scanning it found none")
	}

	// Act: an ordinary turn in which every message converts.
	recoverTurn := driveScenarioToCompletion(t, w, ws, configDir, "fault-recover")

	// Assert: the recovery turn concluded, and its WELL-FORMED hook drew no
	// card either — bubbles.go's drawHook: "A succeeded hook draws NOTHING"
	// (the same fact TestHookSucceeded pins).
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, recoverTurn); ended.GetConcluded() == nil {
		t.Fatalf("recovery turn ended = %v, want a plain conclusion", ended)
	}

	// Assert: THAT window closed, carrying its dropped count.
	closedView := pfAwaitView(t, w, topbar, "the same degraded window closing after the clean turn", func(v *frontendv1.TopbarView) bool {
		return pfClosedDegradedWindow(v, opened) != nil
	})
	closed := pfClosedDegradedWindow(closedView, opened)
	if closed == nil {
		t.Fatalf("topbar push matched the closed window but re-scanning it found none")
	}
	if got, want := closed.GetClosed().GetEndedAtMs(), opened.GetBeganAtMs(); got < want {
		t.Errorf("closed degraded window ended_at_ms = %d, want >= began_at_ms %d", got, want)
	}
	if got := closed.GetClosed().GetDroppedCount(); got < 1 {
		t.Errorf("closed degraded window dropped_count = %d, want >= 1 (the one message the converter refused)", got)
	}
}

// ===========================================================================
// Bookkeeping turns that draw NOTHING extra.
// ===========================================================================

// TestKeepaliveTurnIsOrdinaryAndUnmarked drives `!keepalive`. The scenario
// exists "so a test can drive a keep-alive-shaped turn deterministically;
// the `<!--agent-repl:keepalive-->` marker is the SHIM's, and the mock never
// adds or removes it" — its arms are AgentResponse.from_model and
// AgentSuccess.completed, i.e. an ORDINARY turn.
//
// So the assertion is the specific negative that follows from that sentence:
// the prompt bubble carries the submitted text VERBATIM, with no keep-alive
// marker minted by the producer, and the turn's row set is the ordinary one.
// prompt_origin.proto is explicit that the cache keep-alive "is not a prompt
// the daemon" mints here — this submission is a user prompt that merely
// looks like one.
func TestKeepaliveTurnIsOrdinaryAndUnmarked(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := pfNewWorkspace(t, w)
	const keepaliveMarker = "<!--agent-repl:keepalive-->"

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "keepalive")

	// Assert
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (AgentSuccess.completed)", ended)
	}
	pfAssertOnlyProseRows(t, rows, turn)

	var prompt *frontendv1.FeedRow
	for _, row := range pfRowsOfTurn(rows, turn) {
		if row.GetUserPrompt() != nil {
			prompt = row
		}
	}
	if prompt == nil {
		t.Fatalf("feed drew no user_prompt row for the keep-alive-shaped turn")
	}
	drawn := prompt.String()
	if !strings.Contains(drawn, "!keepalive") {
		t.Errorf("prompt row = %v, want the submitted text drawn verbatim", prompt)
	}
	if strings.Contains(drawn, keepaliveMarker) {
		t.Errorf("prompt row = %v, want NO %q marker — the mock never adds one", prompt, keepaliveMarker)
	}
}

// TestResidueAttachmentsDrawNoRow drives `!residue`, whose two attachments
// (`deferred_tools_delta` and `agent_listing_delta`) are the records "BOTH
// planes agree are vendor bookkeeping, not context". Its arms line is
// literally "NONE — these are StoreUnservedItem.vendor_specific ... dropped
// from every page by both planes", so the whole fact under test is a
// negative: the turn draws its prompt, its prose and its terminal, and not
// one row more.
func TestResidueAttachmentsDrawNoRow(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := pfNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "residue")

	// Assert
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	pfAssertOnlyProseRows(t, rows, turn)
}

// TestAwaySummaryDrawsNoRow drives `!away-summary`, whose recap is a
// `system:away_summary` record — arms line: "vendor_specific residue —
// `system/away_summary`, which no conversation.v1 arm models". The recap
// therefore appears NOWHERE on the feed; only the turn's own prose does.
func TestAwaySummaryDrawsNoRow(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := pfNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "away-summary")

	// Assert
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	pfAssertOnlyProseRows(t, rows, turn)
}

// TestContextTipDrawsNoRow drives `!context-tip` — the vendor's GENERIC CLI
// TIP attachment. Its arms line: "residue `attachment/context_tip` — the tip
// is recorded as itself, unconverted, and reaches no arm", with the standing
// ruling that "IT IS NOT THE CONTEXT-BUDGET WARNING ... mapping the tip to
// it would draw an unrelated tip as \"your context is filling\"".
//
// Two negatives, then: no extra feed row, and no footer salient line minted
// from the tip (no footer line warns that the context is nearly full, owner
// ruling 2026-10-06).
func TestContextTipDrawsNoRow(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := pfNewWorkspace(t, w)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "context-tip")

	// Assert
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	pfAssertOnlyProseRows(t, rows, turn)

	// A generic tip must stand no salient line, under any status.
	view := pfAwaitView(t, w, footer.Stream, "the footer after the tip turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus() != nil
	})
	if salient := footerTier(view, "salient"); salient != nil {
		t.Errorf("footer drew a salient line %v from a GENERIC context tip, want none", salient.Interface())
	}
}

// TestTokensReminderDrawsNoRow drives `!tokens-reminder` — the
// `total_tokens_reminder` attachment, "the ONE token-budget carrier any real
// capture holds ... IT IS NOT the context-budget warning either". Arms line:
// "residue `attachment/total_tokens_reminder` — recorded as itself,
// unconverted, and reaching no arm".
func TestTokensReminderDrawsNoRow(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := pfNewWorkspace(t, w)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, configDir, "tokens-reminder")

	// Assert
	rows := openFeedPage(t, w, ws)
	if ended := turnEndedRow(t, rows, turn); ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	pfAssertOnlyProseRows(t, rows, turn)

	view := pfAwaitView(t, w, footer.Stream, "the footer after the token-reminder turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus() != nil
	})
	if salient := footerTier(view, "salient"); salient != nil {
		t.Errorf("footer drew a salient line %v from a TOKEN-COUNT reminder, want none", salient.Interface())
	}
}

// TestContextWindowExceededIsDrawnAsTurnFailed drives `!context-window`. It
// is the odd one in this group: not bookkeeping at all, but a TERMINAL — its
// arms line is "AgentResponseFailure.reason=context_window_exceeded and
// AgentFailure.prompt_too_long".
//
// THE PROTO RULES THE ARM, and this test formerly asserted request_too_large
// against a daemon that no longer draws it. feed.proto confines
// FeedTurnErrorRequestTooLarge to "413 — the request exceeded the size
// limit", an API status, while AgentFailure.prompt_too_long is a PRODUCER
// terminal that never carried an API status. Its home is therefore
// FailureVendorTurnFailed — "every other unclassified abnormal end;
// `stop_reason` names the vendor's own word" — with the stop reason
// `prompt_too_long`. Ruled and implemented in 3617b4aaa; the test is
// corrected to the contract rather than the daemon to the test.
//
// The headline is asserted as a stated sentence rather than a pinned string:
// FeedTurnErrorHeadline is contracted only as "The sentence, drawn
// verbatim".
func TestContextWindowExceededIsDrawnAsTurnFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, _ := pfNewWorkspace(t, w)

	// Act
	turn := SubmitPrompt(t, w, ws, "!context-window")

	// Assert
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetConcluded() != nil {
		t.Fatalf("turn ended = %v, want an errored terminal", ended)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("turn ended = %v, want the errored arm", ended)
	}
	failed := errored.GetTurnFailed()
	if failed == nil {
		t.Fatalf("turn error = %v, want turn_failed (feed.proto's arm for a producer terminal with no drawn counterpart)", errored)
	}
	if got := failed.GetStopReason(); got != "prompt_too_long" {
		t.Errorf("turn_failed stop_reason = %q, want %q (the producer's own word)", got, "prompt_too_long")
	}
	if errored.GetRequestTooLarge() != nil {
		t.Errorf("turn error drew request_too_large, which feed.proto confines to the vendor's 413")
	}
	if errored.GetHeadline().GetText() == "" {
		t.Errorf("turn error headline = %q, want the daemon's composed sentence", errored.GetHeadline().GetText())
	}
}
