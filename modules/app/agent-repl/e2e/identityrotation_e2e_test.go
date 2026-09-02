// identityrotation_e2e_test.go — SPEC.md §C "/clear + identity rotation"
// (#20-21), driving the real `identity-rotation-clear` scenario through the
// real shim (never hand-written events).
//
// Contract points:
//   - docs/overhaul/daemon.md §"Identity, as the daemon lives it": "Typed
//     identity spaces ... STOP at the daemon"; "vendor identities (uuid,
//     message.id, session id) never cross the daemon<->client contract"
//     — a typed identity, that is; a vendor id can still appear inside an
//     opaque, daemon-composed display STRING (the topbar's session line —
//     see below), which is not a typed identity field.
//   - docs/overhaul/shim.md §"WatchSession (standing stream)": "session-
//     scoped facts as they manifest — identity_rotated, ...".
//   - proto/src/conversation/v1/session.proto: SessionIdentityRotated
//     (previous_vendor_session_id, vendor_session_id).
//   - proto/src/conversation/v1/slash_command.proto: ContextCut/ContextCleared
//     — "History was discarded with nothing left in its place."
//   - proto/src/conversation/v1/agent.proto: AgentUpdate.context_cut — "A
//     page line of the main agent's book; the feed draws it as the
//     separation divider."
//
// Scenario driven: `!rotate` (golden `identity-rotation-clear`),
// agent-shim/claude/shim/src/fake/scenarios/session.ts's ROTATE scenario —
// "arms: SessionIdentityRotated + AgentUpdate.context_cut(ContextCleared)".
// The MANIFEST row for this golden
// (agent-shim/claude/shim/testdata/captures/MANIFEST.md) says "3 turn
// terminals" — that describes the underlying VENDOR CAPTURE this scenario
// was grounded from, not a claim that one submitted `!rotate` prompt itself
// ends in three turns. The scenario's own `run(ctx)` (read before writing
// this file) does the reset-and-conclude inside ONE turn, matching the
// existing shim-level integration coverage
// (agent-shim/claude/shim/test/integration/session.test.ts's "a /clear
// rotates the vendor id and pushes identity_rotated", which drives `!rotate`
// as a single StartTurn per submission, and drives a SECOND `!rotate` as a
// distinct later turn on the same session for the "under the rotated
// identity" case) — this file drives one real "!rotate" submission per
// rotation, exactly that shape.
//
// Which wire surface states SessionIdentityRotated: daemon/internal/resolve
// has three resolvers subscribed to SessionUpdate. Reading their dispatch
// (necessary to pick a real, already-landed observable surface for a
// documented WatchSession arm — not a hunt for a defect) shows feed/footer/
// sidebar treat SessionUpdate_IdentityRotated as a no-op, and only the
// topbar resolver folds it into its session line
// (daemon/internal/resolve/topbar/resolver.go: `s.vendorSessionID =
// u.IdentityRotated.GetVendorSessionId()`, joined into
// TopbarView.SessionLine.Text alongside the account root and model). This
// file therefore observes the rotation by watching the topbar's session
// line change to a new, distinct non-empty value — the only client-facing
// fact SessionIdentityRotated currently produces.
//
// ContextCleared is observed the documented way every other context-cut test
// in this repo does (daemon/integration/feed_test.go's
// TestContextCutClearedDrawsASeparation, mirrored here against the real
// wire): a FeedRow whose separation carries a non-nil Cleared arm.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// awaitClearedSeparations opens (or re-opens) the workspace's root feed and
// waits until at least n distinct FeedRows carry a non-nil
// separation.Cleared arm, checking the page first (already-durable history)
// and falling back to the watch stream — the same page-then-watch shape
// world_test.go's AwaitTurnEnded already uses for a turn's own terminal.
// Dedups by FeedId, since a row's upsert key can in principle be re-pushed.
func awaitClearedSeparations(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, n int) []*frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}

	seen := map[string]bool{}
	var found []*frontendv1.FeedRow
	take := func(row *frontendv1.FeedRow) {
		if row.GetSeparation().GetCleared() == nil {
			return
		}
		id := row.GetId().GetValue()
		if seen[id] {
			return
		}
		seen[id] = true
		found = append(found, row)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		take(row)
	}
	if len(found) >= n {
		return found
	}

	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	for len(found) < n {
		take(harness.AwaitNext(t, w.Ctx(), stream, "a cleared-context separation row"))
	}
	return found
}

// awaitTopbarSessionLineChange watches the workspace's topbar until its
// session-line text differs from `from` (and is non-empty), and answers the
// new text. Used to observe a SessionIdentityRotated fact the only way it
// currently reaches a client — see the file header.
func awaitTopbarSessionLineChange(t *testing.T, w *World, topbar *harness.Stream[*frontendv1.TopbarView], from string) string {
	t.Helper()
	view := harness.AwaitView(t, w.Ctx(), topbar, "the topbar's session line to change from "+from, func(v *frontendv1.TopbarView) bool {
		text := v.GetSessionLine().GetText()
		return text != "" && text != from
	})
	return view.GetSessionLine().GetText()
}

// awaitTopbarSessionLineNonEmpty watches the workspace's topbar until its
// session-line text is first non-empty (the session-started vendor id, before
// any rotation), and answers it.
func awaitTopbarSessionLineNonEmpty(t *testing.T, w *World, topbar *harness.Stream[*frontendv1.TopbarView]) string {
	t.Helper()
	view := harness.AwaitView(t, w.Ctx(), topbar, "the topbar's session line to become non-empty", func(v *frontendv1.TopbarView) bool {
		return v.GetSessionLine().GetText() != ""
	})
	return view.GetSessionLine().GetText()
}

// TestClearRotatesIdentity drives SPEC.md §C #20: the real `!rotate`
// scenario, asserting both arms the scenario states it produces —
// SessionIdentityRotated (observed via the topbar's session line, see the
// file header) and AgentUpdate.context_cut(ContextCleared) (observed as a
// feed separation row, matching every other context-cut test in this repo).
func TestClearRotatesIdentity(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := NewRealRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	topbar := w.WatchTopbar(ws)

	// Act: the first (and, for this session, only) prompt is "!rotate"
	// itself — the scenario starts the session fresh and rotates its
	// identity within that same turn, exactly as
	// session.test.ts's "a /clear rotates the vendor id" case does.
	turn := SubmitPrompt(t, w, ws, "!rotate")

	// Assert: the session states its vendor identity, then rotates it.
	before := awaitTopbarSessionLineNonEmpty(t, w, topbar)
	after := awaitTopbarSessionLineChange(t, w, topbar, before)
	if after == before {
		t.Fatalf("topbar session line = %q both before and after !rotate, want a change", before)
	}

	// Assert: the turn concluded normally (MANIFEST: `... -> success.completed`).
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("TurnEnded = %v, want a concluded (success.completed) outcome", row.GetTurnEnded())
	}

	// Assert: the cut left a cleared-context divider on the feed, with a
	// composed label (matching TestContextCutClearedDrawsASeparation's
	// same assertion against the real wire).
	cleared := awaitClearedSeparations(t, w, ws, 1)
	if cleared[0].GetSeparation().GetLabel().GetText() == "" {
		t.Fatal("the cleared divider carries no label, want a composed one")
	}
}

// TestSecondRotateUnderRotatedIdentity drives SPEC.md §C #21: a SECOND real
// `!rotate` submitted on the SAME session, once the first rotation has
// already landed — E2E-EVENT-INVENTORY.md remediation item 3 ("drive a
// SECOND `!rotate` ... instead of fabricating"), proving the daemon composes
// the second rotation onto its already-rotated state rather than this test
// fabricating an intermediate identity.
func TestSecondRotateUnderRotatedIdentity(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := NewRealRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	topbar := w.WatchTopbar(ws)

	// Act: first rotation, same as TestClearRotatesIdentity.
	firstTurn := SubmitPrompt(t, w, ws, "!rotate")
	initial := awaitTopbarSessionLineNonEmpty(t, w, topbar)
	afterFirst := awaitTopbarSessionLineChange(t, w, topbar, initial)
	if firstRow := AwaitTurnEnded(t, w, ws, firstTurn); firstRow.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("first !rotate TurnEnded = %v, want concluded", firstRow.GetTurnEnded())
	}
	firstCleared := awaitClearedSeparations(t, w, ws, 1)

	// Act: SECOND rotation, submitted on the SAME workspace/session — the
	// daemon must accept a rotate while it is already running under a
	// rotated identity, not merely a fresh one.
	secondTurn := SubmitPrompt(t, w, ws, "!rotate")

	// Assert: identity composes — the session line changes AGAIN, to a
	// THIRD distinct value, never back to the pre-rotation identity.
	afterSecond := awaitTopbarSessionLineChange(t, w, topbar, afterFirst)
	if afterSecond == initial {
		t.Fatalf("second rotation's session line = %q, same as the pre-rotation identity %q, want a new one", afterSecond, initial)
	}
	if afterSecond == afterFirst {
		t.Fatalf("second rotation's session line did not change from the first rotation's %q", afterFirst)
	}

	// Assert: the second turn also concluded normally.
	if secondRow := AwaitTurnEnded(t, w, ws, secondTurn); secondRow.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("second !rotate TurnEnded = %v, want concluded", secondRow.GetTurnEnded())
	}

	// Assert: a SECOND, distinct cleared-context divider landed on the
	// feed — the first rotation's divider is untouched.
	bothCleared := awaitClearedSeparations(t, w, ws, 2)
	if len(bothCleared) < 2 {
		t.Fatalf("cleared-context separations = %d, want at least 2", len(bothCleared))
	}
	if bothCleared[0].GetId().GetValue() != firstCleared[0].GetId().GetValue() {
		t.Fatalf("the first cleared divider's id changed across the second rotation: %q -> %q",
			firstCleared[0].GetId().GetValue(), bothCleared[0].GetId().GetValue())
	}
}
