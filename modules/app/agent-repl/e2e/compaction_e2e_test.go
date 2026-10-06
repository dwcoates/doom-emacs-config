// compaction_e2e_test.go — SPEC.md section C, "Compaction + rotation" (§C
// #22-25; #26 retired 2026-10-06). Drives the real `!compact`, `!compact-auto` and `!compact-failed`
// fake-SDK scenarios (agent-shim/claude/shim/src/fake/scenarios/session.ts)
// through the real shim, and asserts the resulting facts on the daemon's
// real Connect API — never a hand-written store row.
//
// This family was flagged as the HIGHEST false-confidence risk in the
// coverage audit: the deleted suite hand-wrote compaction events, and one of
// its files carried a stale comment claiming "the fake has no compaction"
// when it does (session.ts's COMPACT/COMPACT_AUTO/COMPACT_FAILED scenarios).
// Every assertion here is read back from a real turn driven through the real
// shim.
//
// Contract grounding, read before writing this file:
//   - docs/overhaul/shim.md: "A `compacting` arm is owed... vendor-initiated
//     auto-compaction still happens, and its start signal (the system
//     status:compacting message — the ContextCut record is the end) is
//     forwarded so a surface can draw the in-progress state."
//   - docs/overhaul/daemon.md ("PROTO-CHANGES.md landing ledger" section):
//     "AgentUpdate gains the page-line arms `context_cut` (drawn as the
//     separation divider) and `api_error`..."
//   - proto/src/conversation/v1/agent.proto: AgentUpdate.context_cut (oneof
//     cleared/compacted/compaction_failed).
//   - proto/src/frontend/v1/feed.proto: FeedSessionSeparation is the drawn
//     divider row; its `kind` oneof carries FeedContextCutCleared,
//     FeedContextCutCompacted, FeedWorktreeEntered, FeedWorktreeLeft, and
//     (Landing 8, docs/overhaul/PROTO-CHANGES.md) FeedContextCutCompactionFailed
//     — see TestCompactionFailed's header for the exact shape.
//   - proto/src/frontend/v1/footer.proto: FooterSubStatusWorkingCompacting
//     (the in-progress signal). No footer line warns that the context is
//     nearly full (owner ruling, 2026-10-06), so a failed compaction raises
//     no salient line.
//
// Scenario shapes were resolved by reading
// agent-shim/claude/shim/src/fake/scenarios/session.ts's COMPACT, COMPACT_AUTO
// and COMPACT_FAILED scenario bodies directly (fake-SDK test tooling, not
// production — reading it is expected, per this file's dispatch brief), not
// by guessing from SPEC.md's own text.
package e2e

import (
	"context"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Shared arrange/assert helpers, local to this file.
// ---------------------------------------------------------------------------

// cpNewWorkspace registers and opens ONE workspace against a fresh
// fake repository (harness.NewRepo — the daemon's scripted fake git; this
// suite does NOT use real git and must stay fast). Answers the workspace ref
// and the account config root it routes through (always DefaultConfigDir: a
// fresh harness.NewRepo directory is never under MultiRepoRoot).
func cpNewWorkspace(t *testing.T, w *World) (*workspacev1.WorkspaceRef, string) {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace(%s): %v", repo.Dir, err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace(%s) = %v, want success", repo.Dir, resp.Msg)
	}
	return ws, w.DefaultConfigDir
}

// cpAwaitFooterView bounds a footer-stream wait at DefaultTimeout (this file's
// own budget: every wait here chains at most one real turn through the real
// shim, the same shape DefaultTimeout was sized for — see world_test.go's own
// doc comment on that constant).
func cpAwaitFooterView(t *testing.T, w *World, s *harness.Stream[*frontendv1.FooterView], what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// cpOpenFeedRows re-opens the workspace's root feed and answers its full
// history page — the same read path AwaitTurnEnded itself uses to find a
// historical row, used here to scan for the separation divider a compaction
// leaves behind (a row whose own `turn` field is UNSET, per feed.proto's
// FeedRow.turn doc: "Unset for a row that belongs to no turn (a separation
// divider)" — so it cannot be found by matching the turn id).
func cpOpenFeedRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", resp.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}

// cpFindCompactedSeparation answers the first row whose separation carries a
// FeedContextCutCompacted, or nil if none does.
func cpFindCompactedSeparation(rows []*frontendv1.FeedRow) *frontendv1.FeedRow {
	for _, row := range rows {
		if row.GetSeparation().GetCompacted() != nil {
			return row
		}
	}
	return nil
}

// cpFindCompactionFailedSeparation answers the first row whose separation
// carries a FeedContextCutCompactionFailed (Landing 8), or nil if none does.
func cpFindCompactionFailedSeparation(rows []*frontendv1.FeedRow) *frontendv1.FeedRow {
	for _, row := range rows {
		if row.GetSeparation().GetCompactionFailed() != nil {
			return row
		}
	}
	return nil
}

// cpDriveObservingInProgress submits prompt, awaits the footer's
// `compacting` sub-status WHILE the turn is still running (SPEC.md #22-25 all
// name the `status{compacting}` signal explicitly — driveScenarioToCompletion
// alone would race past it, since it does not return until the turn is fully
// concluded and durable), then waits for the turn's terminal and the
// sidecar's durable cursor advance exactly as driveScenarioToCompletion does.
// The footer stream is opened BEFORE SubmitPrompt so the transient push is
// queued in order rather than possibly missed.
func cpDriveObservingInProgress(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, prompt string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	before := w.Store.Cursors(t, ctx)
	cancel()
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	footer := w.WatchFooter(ws)
	defer footer.Close()

	turn := SubmitPrompt(t, w, ws, prompt)
	cpAwaitFooterView(t, w, footer.Stream, "compacting sub-status for "+prompt, func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetCompacting() != nil
	})

	AwaitTurnEnded(t, w, ws, turn)
	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)
	return turn
}

// ---------------------------------------------------------------------------
// #22 CompactionDirected — `compaction-directed` (`!compact`, no summary
// override, the scenario's own default: "Compacted the conversation.").
// docs/overhaul/shim.md's owed `compacting` arm; the resulting
// FeedContextCutCompacted divider (daemon.md's landing-ledger note that
// AgentUpdate.context_cut is "drawn as the separation divider").
// ---------------------------------------------------------------------------

func TestCompactionDirected(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := cpNewWorkspace(t, w)
	const wantSummary = "Compacted the conversation."

	// Act: drive the real `!compact` scenario, observing the in-progress
	// compacting signal before the turn concludes.
	turn := cpDriveObservingInProgress(t, w, ws, configDir, "!compact")

	// Assert: the feed carries a separation divider compacting the context,
	// whose summary is the one the vendor's summary record states (session.ts
	// COMPACT: `ctx.compactSummary(...)`, read by the shim's settleCompaction
	// and by the sidecar off the transcript's isCompactSummary line alike).
	rows := cpOpenFeedRows(t, w, ws)
	compacted := cpFindCompactedSeparation(rows)
	if compacted == nil {
		t.Fatalf("no FeedContextCutCompacted separation row found among %d rows", len(rows))
	}
	sep := compacted.GetSeparation()
	if got := sep.GetCompacted().GetSummary().GetMarkdown(); got != wantSummary {
		t.Errorf("compacted summary = %q, want %q", got, wantSummary)
	}
	if sep.GetTokens() == nil {
		t.Error("separation.tokens = nil, want the before/after token-size fact every cut carries (feed.proto: \"Every cut has one\")")
	}
	// THE LABEL NAMES THE TRIGGER, and that is the only thing on the glass
	// telling a compaction the user ASKED FOR apart from one that happened to
	// them (`daemon/internal/resolve/feed/separation.go` compactionLabel:
	// "drawing the two identically is the most misleading thing this divider
	// can do"). It was pinned only in that package's own unit test, so nothing
	// said the wording survived the whole stack; a screenshot of two dividers
	// in one feed is what raised the question.
	if got := sep.GetLabel().GetText(); !strings.HasPrefix(got, "context compacted on request") {
		t.Errorf("separation.label.text = %q, want it to open with %q -- a compaction the user asked for",
			got, "context compacted on request")
	}

	// Assert: the turn itself concluded normally.
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome", ended)
	}
}

// ---------------------------------------------------------------------------
// #23 CompactionDirectedWithSummaryOverride — `!compact [summary]`, the
// landed summary-override option (session.ts COMPACT: `ctx.args === "" ?
// "Compacted the conversation." : ctx.args`). PROTO-CHANGES.md's landing
// ledger has no field for this override — it is a shim-internal
// parameterization of the SAME scenario, not a wire change — so this test
// passes its own distinctive summary rather than asserting against the fixed
// default #22 already covers.
// ---------------------------------------------------------------------------

func TestCompactionDirectedWithSummaryOverride(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := cpNewWorkspace(t, w)
	const wantSummary = "e2e-distinctive-compaction-summary-23"

	// Act
	turn := cpDriveObservingInProgress(t, w, ws, configDir, "!compact "+wantSummary)

	// Assert: ContextCompacted.Summary carries the test's OWN string, not the
	// fixed default — proving the override argument actually reaches the
	// converter rather than a fabricated store row standing in for it.
	rows := cpOpenFeedRows(t, w, ws)
	compacted := cpFindCompactedSeparation(rows)
	if compacted == nil {
		t.Fatalf("no FeedContextCutCompacted separation row found among %d rows", len(rows))
	}
	if got := compacted.GetSeparation().GetCompacted().GetSummary().GetMarkdown(); got != wantSummary {
		t.Errorf("compacted summary = %q, want the override %q", got, wantSummary)
	}

	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome", ended)
	}
}

// ---------------------------------------------------------------------------
// #24 CompactionAuto — `!compact-auto`. Same shapes as #22 with
// `trigger: "auto"` the only scenario-side discriminator (session.ts
// COMPACT_AUTO's own doc comment). frontend/v1's FeedContextCutCompacted
// carries no trigger field of its own (only summary/fold/cold_read), which is
// why this test once asserted nothing that told an automatic compaction from a
// directed one. IT DOES NOW, and it is not a guess at an unformatted field:
// the divider's `label.text` is a modeled field the daemon composes FROM the
// trigger (`resolve/feed/separation.go` compactionLabel) and the client draws
// verbatim, so the wording is the wire's own answer to "which kind of cut was
// this".
// ---------------------------------------------------------------------------

func TestCompactionAuto(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := cpNewWorkspace(t, w)
	const wantSummary = "The conversation was compacted automatically."

	// Act
	turn := cpDriveObservingInProgress(t, w, ws, configDir, "!compact-auto")

	// Assert
	rows := cpOpenFeedRows(t, w, ws)
	compacted := cpFindCompactedSeparation(rows)
	if compacted == nil {
		t.Fatalf("no FeedContextCutCompacted separation row found among %d rows", len(rows))
	}
	sep := compacted.GetSeparation()
	if got := sep.GetCompacted().GetSummary().GetMarkdown(); got != wantSummary {
		t.Errorf("compacted summary = %q, want %q", got, wantSummary)
	}
	if sep.GetTokens() == nil {
		t.Error("separation.tokens = nil, want the before/after token-size fact every cut carries")
	}
	// THE AUTOMATIC TRIGGER IS ON THE GLASS, which this file's own header used
	// to deny: FeedContextCutCompacted carries no trigger field, but the daemon
	// composes the divider's LABEL from the trigger and the client draws that
	// label verbatim, so the distinction reaches a reader through a modeled
	// field rather than through a guess. This is the negative of
	// TestCompactionDirected's "on request".
	if got := sep.GetLabel().GetText(); !strings.HasPrefix(got, "context compacted automatically") {
		t.Errorf("separation.label.text = %q, want it to open with %q -- a compaction that happened on its own",
			got, "context compacted automatically")
	}

	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome", ended)
	}
}

// ---------------------------------------------------------------------------
// #25 CompactionFailed — `!compact-failed`. Terminal shape resolved by
// reading session.ts's COMPACT_FAILED scenario body directly, per this
// file's dispatch brief, rather than guessing:
//
//	run(ctx) {
//	  ctx.systemMessage("status", { status: "compacting" });
//	  ctx.systemMessage("status", { status: null, compact_result: "failed",
//	    compact_error: "the summarizing request was rejected" });
//	  conclude(ctx, "The compaction failed and nothing was cut.");
//	}
//
// `conclude` is the SAME ordinary success-turn helper every plain-prose
// scenario uses (assistant prose + `result{subtype:"success"}`) — there is NO
// HibernateError and no turn-level failure arm here; the compaction failure
// rides AgentUpdate.context_cut(ContextCompactionFailed), a page-line fact on
// an otherwise normally-concluding turn.
//
// docs/overhaul/PROTO-CHANGES.md "Landing 8" (2026-09-02, user-approved)
// settled the FeedRow-level gap this test originally flagged as an open
// question: a compaction that was offered (`/compact`, the cold gate's
// compact remedy) and did not happen now DOES draw a divider —
// frontend.v1 FeedSessionSeparation.kind.compaction_failed (tag 7) =
// FeedContextCutCompactionFailed{error}, drawn in the slot the compacted
// divider would have taken, with `tokens` UNSET (nothing was cut, so there is
// no before/after size to show). It relays
// conversation.v1.ContextCut.compaction_failed verbatim. Previously — the
// state this test used to pin — nothing was drawn on the client wire at all.
// ---------------------------------------------------------------------------

func TestCompactionFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	// THE FAILED COMPACTION IS THE SUBJECT. `!compact-failed` asks the vendor
	// to reject the summarizing request, and both records below are the
	// daemon stating that outcome: the feed's marker says nothing was cut,
	// and the footer records the failure without raising a line.
	w.ExpectWarnings("daemon.feed.compaction_failed", "daemon.footer.on_context_cut")
	ws, configDir := cpNewWorkspace(t, w)
	footer := w.WatchFooter(ws)
	defer footer.Close()
	const wantError = "the summarizing request was rejected"

	// Act
	turn := cpDriveObservingInProgress(t, w, ws, configDir, "!compact-failed")

	// Assert: the turn concluded normally (conclude()'s ordinary success
	// path, not a hibernate-error or a turn-level failure arm).
	rows := cpOpenFeedRows(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome (compact-failed still ends the turn normally)", ended)
	}

	// Assert: the Landing-8 compaction_failed divider was drawn, carrying the
	// vendor's own rejection wording, with tokens UNSET (nothing was cut).
	failed := cpFindCompactionFailedSeparation(rows)
	if failed == nil {
		t.Fatalf("no FeedContextCutCompactionFailed separation row found among %d rows", len(rows))
	}
	sep := failed.GetSeparation()
	if got := sep.GetCompactionFailed().GetError(); got != wantError {
		t.Errorf("compaction_failed.error = %q, want %q", got, wantError)
	}
	if sep.GetTokens() != nil {
		t.Errorf("separation.tokens = %v, want UNSET on the compaction_failed arm (Landing 8: nothing was cut)", sep.GetTokens())
	}

	// Assert: no compacted (successful) divider was ALSO drawn for this turn.
	if compacted := cpFindCompactedSeparation(rows); compacted != nil {
		t.Errorf("found a FeedContextCutCompacted separation row after !compact-failed, want only compaction_failed: %v", compacted)
	}

	// Assert: the footer raises NO salient line for it (owner ruling,
	// 2026-10-06): the feed's outcome marker is the failure's whole account.
	// The first idle view follows the cut, since the cut and the terminal
	// after it are the only edges that end the compacting turn.
	idle := cpAwaitFooterView(t, w, footer.Stream, "the idle footer after !compact-failed", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	if salient := footerTier(idle, "salient"); salient != nil {
		t.Errorf("footer raised a salient line %v after a failed compaction, want none", salient.Interface())
	}
}
