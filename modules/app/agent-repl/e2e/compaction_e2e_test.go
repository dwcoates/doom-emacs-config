// compaction_e2e_test.go — SPEC.md section C, "Compaction + rotation" (§C
// #22-26). Drives the real `!compact`, `!compact-auto` and `!compact-failed`
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
//     FeedContextCutCompacted, FeedWorktreeEntered, FeedWorktreeLeft — NO
//     compaction_failed arm (see TestCompactionFailed's header for the
//     resulting open question).
//   - proto/src/frontend/v1/footer.proto: FooterSubStatusThinkingCompacting
//     (the in-progress signal) and FooterStatusActivityContextBudget (the
//     context-budget-warning carrier).
//
// Scenario shapes were resolved by reading
// agent-shim/claude/shim/src/fake/scenarios/session.ts's COMPACT, COMPACT_AUTO
// and COMPACT_FAILED scenario bodies directly (fake-SDK test tooling, not
// production — reading it is expected, per this file's dispatch brief), not
// by guessing from SPEC.md's own text.
package e2e

import (
	"context"
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

// newCompactionWorkspace registers and opens ONE workspace against a fresh
// fake repository (harness.NewRepo — the daemon's scripted fake git; this
// suite does NOT use real git and must stay fast). Answers the workspace ref
// and the account config root it routes through (always DefaultConfigDir: a
// fresh harness.NewRepo directory is never under MultiRepoRoot).
func newCompactionWorkspace(t *testing.T, w *World) (*workspacev1.WorkspaceRef, string) {
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

// awaitFooterView bounds a footer-stream wait at DefaultTimeout (this file's
// own budget: every wait here chains at most one real turn through the real
// shim, the same shape DefaultTimeout was sized for — see world_test.go's own
// doc comment on that constant).
func awaitFooterView(t *testing.T, w *World, s *harness.Stream[*frontendv1.FooterView], what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// openFeedRows re-opens the workspace's root feed and answers its full
// history page — the same read path AwaitTurnEnded itself uses to find a
// historical row, used here to scan for the separation divider a compaction
// leaves behind (a row whose own `turn` field is UNSET, per feed.proto's
// FeedRow.turn doc: "Unset for a row that belongs to no turn (a separation
// divider)" — so it cannot be found by matching the turn id).
func openFeedRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
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

// findCompactedSeparation answers the first row whose separation carries a
// FeedContextCutCompacted, or nil if none does.
func findCompactedSeparation(rows []*frontendv1.FeedRow) *frontendv1.FeedRow {
	for _, row := range rows {
		if row.GetSeparation().GetCompacted() != nil {
			return row
		}
	}
	return nil
}

// findTurnEnded answers the row carrying turn's FeedTurnEnded terminal.
func findTurnEnded(t *testing.T, rows []*frontendv1.FeedRow, turn *conversationv1.TurnId) *frontendv1.FeedTurnEnded {
	t.Helper()
	for _, row := range rows {
		if row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil {
			return row.GetTurnEnded()
		}
	}
	t.Fatalf("no FeedTurnEnded row found for turn %s among %d rows", turn.GetValue(), len(rows))
	return nil
}

// contextBudgetText answers the footer's standing context-budget activity
// text, whichever status arm it currently stands under (idle or thinking —
// footer.proto legalizes FooterStatusActivityContextBudget under both), or ""
// if neither carries one.
func contextBudgetText(v *frontendv1.FooterView) string {
	status := v.GetStrip().GetStatus()
	if cb := status.GetIdle().GetActivity().GetContextBudget(); cb != nil {
		return cb.GetText()
	}
	if cb := status.GetThinking().GetActivity().GetContextBudget(); cb != nil {
		return cb.GetText()
	}
	return ""
}

// driveCompactionObservingInProgress submits prompt, awaits the footer's
// `compacting` sub-status WHILE the turn is still running (SPEC.md #22-25 all
// name the `status{compacting}` signal explicitly — driveScenarioToCompletion
// alone would race past it, since it does not return until the turn is fully
// concluded and durable), then waits for the turn's terminal and the
// sidecar's durable cursor advance exactly as driveScenarioToCompletion does.
// The footer stream is opened BEFORE SubmitPrompt so the transient push is
// queued in order rather than possibly missed.
func driveCompactionObservingInProgress(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, prompt string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	before := w.Store.Cursors(t, ctx)
	cancel()
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	footer := w.WatchFooter(ws)
	defer footer.Close()

	turn := SubmitPrompt(t, w, ws, prompt)
	awaitFooterView(t, w, footer, "compacting sub-status for "+prompt, func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking().GetCompacting() != nil
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
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := newCompactionWorkspace(t, w)
	const wantSummary = "Compacted the conversation."

	// Act: drive the real `!compact` scenario, observing the in-progress
	// compacting signal before the turn concludes.
	turn := driveCompactionObservingInProgress(t, w, ws, configDir, "!compact")

	// Assert: the feed carries a separation divider compacting the context,
	// whose summary is the assistant prose settleCompaction derived from
	// (session.ts COMPACT: `conclude(ctx, summary)` — the same string).
	rows := openFeedRows(t, w, ws)
	compacted := findCompactedSeparation(rows)
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
	if sep.GetLabel().GetText() == "" {
		t.Error("separation.label.text = \"\", want a composed divider label")
	}

	// Assert: the turn itself concluded normally.
	ended := findTurnEnded(t, rows, turn)
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
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := newCompactionWorkspace(t, w)
	const wantSummary = "e2e-distinctive-compaction-summary-23"

	// Act
	turn := driveCompactionObservingInProgress(t, w, ws, configDir, "!compact "+wantSummary)

	// Assert: ContextCompacted.Summary carries the test's OWN string, not the
	// fixed default — proving the override argument actually reaches the
	// converter rather than a fabricated store row standing in for it.
	rows := openFeedRows(t, w, ws)
	compacted := findCompactedSeparation(rows)
	if compacted == nil {
		t.Fatalf("no FeedContextCutCompacted separation row found among %d rows", len(rows))
	}
	if got := compacted.GetSeparation().GetCompacted().GetSummary().GetMarkdown(); got != wantSummary {
		t.Errorf("compacted summary = %q, want the override %q", got, wantSummary)
	}

	ended := findTurnEnded(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome", ended)
	}
}

// ---------------------------------------------------------------------------
// #24 CompactionAuto — `!compact-auto`. Same shapes as #22 with
// `trigger: "auto"` the only scenario-side discriminator (session.ts
// COMPACT_AUTO's own doc comment). frontend/v1's FeedContextCutCompacted
// carries no trigger field of its own (only summary/fold/cold_read) and the
// daemon's composed divider label/token text is UNSPECIFIED by the contract
// docs beyond "composed by the daemon" — this test therefore does not assert
// a distinguishing manual-vs-automatic marker on the wire beyond the
// scenario's own distinctive concluding summary, to avoid guessing an
// unmodeled or unformatted field.
// ---------------------------------------------------------------------------

func TestCompactionAuto(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := newCompactionWorkspace(t, w)
	const wantSummary = "The conversation was compacted automatically."

	// Act
	turn := driveCompactionObservingInProgress(t, w, ws, configDir, "!compact-auto")

	// Assert
	rows := openFeedRows(t, w, ws)
	compacted := findCompactedSeparation(rows)
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

	ended := findTurnEnded(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome", ended)
	}
}

// ---------------------------------------------------------------------------
// #25 CompactionFailed — `!compact-failed`. Terminal shape resolved by
// reading session.ts's COMPACT_FAILED scenario body directly, per this
// file's dispatch brief, rather than guessing SPEC.md's own open question:
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
// rides ONLY AgentUpdate.context_cut(ContextCompactionFailed), a page-line
// fact on an otherwise normally-concluding turn. This resolves SPEC.md
// F(original)#4's open question: the plain-turn path, not a hibernate path.
//
// OPEN QUESTION (contract gap, not a guess this test papers over):
// proto/src/frontend/v1/feed.proto's FeedSessionSeparation.kind oneof models
// exactly four arms — cleared, compacted, worktree_entered, worktree_left —
// and has NO compaction_failed arm, even though
// proto/src/conversation/v1/slash_command.proto's ContextCut DOES model
// `compaction_failed` as a first-class (non-residue) arm of AgentUpdate. This
// suite cannot discover what, if anything, the daemon draws on the client
// wire for a failed compaction from the contract docs alone — no WatchFeed,
// WatchFooter or WatchTopbar shape in any of the six planning docs or
// PROTO-CHANGES.md's landing ledger is named for it. This test therefore
// asserts only the two facts the contract DOES settle: the turn concludes
// normally, and no compacted-context divider appears (consistent with
// session.ts's own "writes: nothing but the prompt line and the turn
// record — a failed compaction cut nothing"). Whether a further,
// undiscovered client-visible fact should exist for this arm is left to the
// project lead rather than guessed here.
// ---------------------------------------------------------------------------

func TestCompactionFailed(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := newCompactionWorkspace(t, w)

	// Act
	turn := driveCompactionObservingInProgress(t, w, ws, configDir, "!compact-failed")

	// Assert: the turn concluded normally (conclude()'s ordinary success
	// path, not a hibernate-error or a turn-level failure arm).
	rows := openFeedRows(t, w, ws)
	ended := findTurnEnded(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Errorf("FeedTurnEnded = %v, want a concluded outcome (compact-failed still ends the turn normally)", ended)
	}

	// Assert: nothing was cut — no compacted-context divider was drawn for
	// this turn (a failed compaction leaves the context exactly as it was).
	if compacted := findCompactedSeparation(rows); compacted != nil {
		t.Errorf("found a FeedContextCutCompacted separation row after !compact-failed, want none: %v", compacted)
	}
}

// ---------------------------------------------------------------------------
// #26 ContextBudgetWarning — `!context-budget-warning`. PROTO-CHANGES.md
// Landing 4/5: AgentUpdate.context_budget_warning = 7. Marked
// UNGROUNDED/INVENTED in the shim's own manifest (no vendor capture — not
// even the one literally named `context-budget-warning` — carries a record
// of this spelling; session.ts's own doc comment on the scenario says so
// outright, "pending a grounding capture... ruling 6... LANDING 5 IS NOT
// OVERTURNED"). This test asserts the WIRE SHAPE reaches a client-visible
// surface, not that it matches a real vendor recording.
//
// The surface used is FooterStatusActivityContextBudget
// (proto/src/frontend/v1/footer.proto), legal under BOTH the thinking and
// idle status arms ("standing while it holds") — the only client-visible
// carrier of this fact any of the six contract docs or PROTO-CHANGES.md
// names; frontend/v1/feed.proto's FeedRow has no arm for it at all.
// ---------------------------------------------------------------------------

func TestContextBudgetWarning(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws, configDir := newCompactionWorkspace(t, w)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act: drive the (UNGROUNDED, invented per the shim manifest — see
	// header) scenario to completion.
	driveScenarioToCompletion(t, w, ws, configDir, "context-budget-warning")

	// Assert: the footer's standing activity line carries the warning text.
	view := awaitFooterView(t, w, footer, "context-budget activity line", func(v *frontendv1.FooterView) bool {
		return contextBudgetText(v) != ""
	})
	if got := contextBudgetText(view); got == "" {
		t.Errorf("footer context-budget activity text = %q, want non-empty", got)
	}
}
