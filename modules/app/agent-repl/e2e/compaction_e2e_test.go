// compaction_e2e_test.go — SPEC.md section C, "Compaction + rotation" (§C
// #22-25; #26 retired 2026-10-06). Drives the real `!compact` fake-SDK
// scenario (agent-shim/claude/shim/src/fake/scenarios/session.ts) through the
// real shim; `!compact-auto` and `!compact-failed` are pinned by their two
// halves instead (testdata/scenario-goldens, the owner's ruling (a) of
// 2026-10-14). It asserts the resulting facts on the daemon's
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

	"agent-repld/integration/harness"
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

// cpDriveObservingInProgress submits prompt, waits for the turn's terminal and
// the sidecar's durable cursor advance exactly as driveScenarioToCompletion
// does, and then requires the footer's OWN RECORD that it published the
// `working · compacting` step during the turn (SPEC.md #22-25 all name the
// `status{compacting}` signal explicitly).
//
// THE RECORD, NOT THE STREAM. The footer's topic is LATEST-ONLY
// (daemon/internal/resolve/footer resolver.go topicLocked): a reader that has
// not yet taken a queued view has it replaced by the newer one. A compaction
// the fake vendor runs stands for a few milliseconds, so a client stream can
// legitimately never see it, and watching for it raced the reader against the
// cut. `daemon.footer.substatus_changed` is written for every published step
// change, so it is a durable statement that the step WAS published whatever
// any reader took. Measured alone, the step stands 2-12ms before the cut
// publishes idle.
func cpDriveObservingInProgress(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, prompt string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	before := w.Store.Cursors(t, ctx)
	cancel()
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	// The footer's participant pair must stand for the turn: without a host
	// and a web stream open the strip reads `agent_repl_fault · severed`.
	// Its views are drained, not read: the record below is the observation.
	footer := w.WatchFooter(ws)
	defer footer.Close()
	footer.Drain()

	turn := SubmitPrompt(t, w, ws, prompt)
	AwaitTurnEnded(t, w, ws, turn)
	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)

	// THE SIGNAL REACHING THE FOOTER is the observation, not the compacting
	// step being drawn. The shim's stream-plane `compacting` and the sidecar's
	// file-plane cut race, and when the cut lands first the footer draws
	// nothing for a compaction already over (daemon.footer.compacting_after_its_cut,
	// pinned with the other order in resolve/footer/compaction_test.go). Under
	// a full gate the cut won and this wait for the drawn step timed out
	// (2026-10-14). The footer takes the signal in either order.
	w.AwaitLogRecord(harness.WorkspaceLogPath(ws.GetDir(), "daemon"), "the footer taking the compacting signal for "+prompt, func(r harness.LogRecord) bool {
		return r.Operation == "daemon.footer.on_session_update" && r.Context["arm"] == "compacting"
	})
	return turn
}

// ---------------------------------------------------------------------------
// #22 CompactionDirected — `compaction-directed` (`!compact`, no summary
// override, the scenario's own default: "Compacted the conversation.").
// docs/overhaul/shim.md's owed `compacting` arm; the resulting
// FeedContextCutCompacted divider (daemon.md's landing-ledger note that
// AgentUpdate.context_cut is "drawn as the separation divider").
// ---------------------------------------------------------------------------

// Seam: compaction
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
