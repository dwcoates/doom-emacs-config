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
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
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
	ws := w.NewWorkspace(t)
	return ws, w.DefaultConfigDir
}

// pfAwaitView bounds any view-stream wait in this file at DefaultTimeout —
// every wait here chains at most one real turn through the real stack, so
// the ordinary per-test budget is the right one and no wait in this file
// gets a hand-picked longer window.
func pfAwaitView[V any](t *testing.T, w *World, s *harness.Stream[V], what string, pred func(V) bool) V {
	t.Helper()
	ctx, cancel := grantedclock.WithBound(w.Ctx(), DefaultTimeout)
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

// ===========================================================================
// Bookkeeping turns that draw NOTHING extra.
// ===========================================================================

// testResidueAttachmentsDrawNoRow drives `!residue`, whose two attachments
// (`deferred_tools_delta` and `agent_listing_delta`) are the records "BOTH
// planes agree are vendor bookkeeping, not context". Its arms line is
// literally "NONE — these are StoreUnservedItem.vendor_specific ... dropped
// from every page by both planes", so the whole fact under test is a
// negative: the turn draws its prompt, its prose and its terminal, and not
// one row more.
//
// Seam: two-plane-episode
func testResidueAttachmentsDrawNoRow(t *testing.T) {
	// Arrange
	w := SharedWorld(t)
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

// testContextTipDrawsNoRow drives `!context-tip` — the vendor's GENERIC CLI
// TIP attachment. Its arms line: "residue `attachment/context_tip` — the tip
// is recorded as itself, unconverted, and reaches no arm", with the standing
// ruling that "IT IS NOT THE CONTEXT-BUDGET WARNING ... mapping the tip to
// it would draw an unrelated tip as \"your context is filling\"".
//
// Two negatives, then: no extra feed row, and no footer salient line minted
// from the tip (no footer line warns that the context is nearly full, owner
// ruling 2026-10-06).
func testContextTipDrawsNoRow(t *testing.T) {
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

// testTokensReminderDrawsNoRow drives `!tokens-reminder` — the
// `total_tokens_reminder` attachment, "the ONE token-budget carrier any real
// capture holds ... IT IS NOT the context-budget warning either". Arms line:
// "residue `attachment/total_tokens_reminder` — recorded as itself,
// unconverted, and reaching no arm".
func testTokensReminderDrawsNoRow(t *testing.T) {
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
