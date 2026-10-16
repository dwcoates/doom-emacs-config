// remainder_e2e_test.go — the fourth split of "Everything else" (SPEC.md §C,
// project-lead ruling 5): golden entries #72-76, #85-87, #93-99. Hooks,
// questions and MCP/monitors are OTHER writers' files (hooks_e2e_test.go,
// questions_e2e_test.go, mcpmonitors_e2e_test.go) — nothing here touches
// those goldens.
//
// CONTRACT GROUNDING:
//   - agent-shim/claude/shim/src/fake/scenarios/automation.ts (read directly
//     in this worktree): plan, findings, worktree-keep/-remove, cron, the
//     four push-notification arms, wakeup-schedule/-stop, artifact-publish/
//     -list.
//   - agent-shim/claude/shim/src/fake/scenarios/skills.ts: memory,
//     skills-injected (the two AgentContextInjected arms).
//   - agent-shim/claude/shim/src/fake/scenarios/tasks.ts: task-create/
//     -change/-reject, send-message/-resumed.
//   - agent-shim/claude/shim/src/fake/scenarios/web.ts: web-fetch, web-search.
//   - agent-shim/claude/shim/testdata/captures/MANIFEST.md: the "diagnostics"
//     golden's own row (`hook`, `thinking`, `response` -> `success.completed`,
//     NO tool at all) — see TestDiagnostics's own comment for why no
//     dedicated scenario exists for it.
//   - proto/src/frontend/v1/feed.proto: FeedPlan/FeedPlanPlanned,
//     FeedFindings, FeedArtifact/FeedArtifactPublished,
//     FeedSessionSeparation.worktree_entered/worktree_left,
//     FeedSimpleToolCall/FeedToolCallReturned (the generic tool-card shell
//     every other tool-kind here renders through — cron, push notification,
//     task acts, send-message, web-fetch, web-search all have no dedicated
//     bubble of their own).
//   - proto/src/frontend/v1/topbar.proto: TopbarWarningStrip.warnings — used
//     by TestDiagnostics to assert a HEALTHY diagnostics push by absence,
//     mirroring world_test.go's KeepAliveNeverAppearsOnWire-style
//     assert-by-absence precedent named in SPEC.md §C #45.
//   - proto/src/store/v1/endpoint_open_agent_session.proto,
//     proto/src/store/v1/store.proto (StoreAgentItem/AgentFrame/AgentUpdate/
//     AgentActivity), proto/src/conversation/v1/agent_activity.proto
//     (AgentContextInjected) — the STORE-level surface TestContextInjectedMemory
//     and TestContextInjectedSkills read, per the project-lead ruling below.
//   - daemon/internal/sessionwatcher/watcher.go's adoptMainAgentLocked,
//     operation "daemon.sessionwatcher.main_agent" — the daemon's own
//     structured-log record naming a workspace's main-agent identity, which
//     is how this file obtains a conversation.v1.AgentId at all (see the
//     ruling below: the value "never crosses the wire" to any frontend rpc).
//
// PROJECT-LEAD RULINGS (2026-09-02, on this file's four originally-open
// questions — superseding the wording below that used to present them as
// open):
//
//  1. diagnostics — CONFIRMED. A healthy shim's diagnostics push produces no
//     topbar warning, so asserting the healthy state BY ABSENCE (no
//     TopbarWarningStrip entries) is correct. Strengthened per the ruling to
//     also assert the topbar stream delivered at least one view, so "no
//     warning" cannot silently pass as "no stream ever arrived".
//  2. context-injected-memory / context-injected-skills — CHANGED. The
//     footer's momentary FooterStatusLoading arm is NOT used: nothing in the
//     contract ties that push to AgentContextInjected specifically, so that
//     assertion would have been coincidental. These are FILE-PLANE facts
//     (daemon.md's own list) whose only end-to-end observable is the STORE.
//     Both tests now open the main agent's book via the store's
//     OpenAgentSession (a read-only verb, like the GetSidecarCursors read
//     path driveScenarioToCompletion already relies on for durability) and
//     assert the injected attachment landed as a page line carrying
//     AgentActivity.context_injected with the right kind. The main agent's
//     conversation.v1.AgentId is obtained from the daemon's own structured
//     log (see CONTRACT GROUNDING above) since AgentId itself "never crosses
//     the wire" to any frontend rpc (subagents_e2e_test.go's own header
//     comment, independently confirmed by grep of every frontend .proto
//     under proto/src/frontend and proto/src/agentrepl).
//  3. push-notification-not-sent — ACCEPTABLE AS WRITTEN. The generic
//     FeedSimpleToolCall shell exposes no per-reason (config_off/
//     user_present/no_transport) field BY CONTRACT (feed.proto declares no
//     such arm), so per-arm reachability — driving all three registered
//     scenarios and asserting each settles — is the strongest available
//     assertion at this surface; kept as three sub-tests.
//  4. send-message-resumed — SAME RULING. The `resumedAgentId` discriminator
//     is not exposed on the generic tool-card surface BY CONTRACT either;
//     reachability (the resumed scenario settles and its composed text
//     output carries the vendor's resumed-from-transcript wording) is the
//     strongest available assertion, kept as written.
//
// SCENARIO-NAME MISMATCHES FOUND (per the sibling-writer precedent SPEC.md's
// dispatch note describes — golden manifest name vs. registered `!name`).
// Recorded by the project lead in E2E-SCENARIO-COVERAGE.md as a naming-drift
// section for a later shim-side cleanup; not a blocker for this file:
//   - "context-injected-memory" golden -> registered name "memory" (`!memory`).
//   - "context-injected-skills" golden -> registered name "skills-injected"
//     (`!skills-injected`).
//   - "push-notification-not-sent" golden -> THREE registered arms exist
//     (push-config-off, push-user-present, push-no-transport); this file
//     drives all three as sub-tests of one Go test, matching golden #45's
//     "every declared arm reachable" design intent stated in automation.ts's
//     own file doc comment.
//   - "artifact-publish-and-list" golden -> two registered names
//     ("artifact-publish", "artifact-list"), driven as one combined test.
//   - "schedule-wakeup-schedule-and-stop" golden -> two registered names
//     ("wakeup-schedule", "wakeup-stop"), driven as one combined test.
//   - "send-message-queued-and-resumed" golden -> two registered names
//     ("send-message", "send-message-resumed"), driven as one combined test.
//   - "task-acts-create-change-reject" golden -> three registered names
//     ("task-create", "task-change", "task-reject"), driven as one combined
//     test.
//   - "worktree-enter-exit-kept-and-removed" golden -> two registered names
//     ("worktree-keep", "worktree-remove"), driven as one combined test.
//   - "diagnostics" golden -> NO registered `!name` scenario exists anywhere
//     in fake/scenarios/*.ts (confirmed by grep of every `name:` field in
//     that directory). The MANIFEST's own row for it names no tool at all
//     (`hook, thinking, response -> success.completed`), which is exactly
//     the DEFAULT prose scenario's shape (fake/scenarios/prose.ts's PROSE,
//     `name: ""`) — an ordinary prompt with no `!scenario` prefix. CONFIRMED
//     by project-lead ruling 1 above.
//
// This suite mocks every external dependency (user ruling): the vendor is
// the fake SDK riding the real shim's --fake mode, and git is the SCRIPTED
// FAKE git the daemon harness installs (harness.NewRepo/harness.Register) —
// never harness.NewRealRepo/RealRepo (being deleted from the harness) and
// this file never sets Opts.SkipFakeGit. Every transcript fact asserted here
// comes from a named fake-SDK scenario driven through the real shim, per
// this package's own grep gate. The two store reads this file makes
// (OpenAgentSession) are read-only verbs, not the forbidden WriteBatch, and
// read facts the real shim itself wrote via --fake — never a hand-authored
// row.
package e2e

import (
	"context"
	"fmt"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Shared helpers for this file only. Every top-level declaration in this
// file is prefixed rm* to avoid colliding with an identically-named helper
// in a sibling area file — all twenty area files build into one Go package,
// and an unprefixed name like `awaitFeedRow` has already collided across
// writers once.
// ---------------------------------------------------------------------------

// rmNewWorkspace builds one World and registers a fresh fake repository
// (harness.NewRepo — the scripted fake git, never the real binary) as its
// workspace.
func rmNewWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, ws
}

// rmAwaitFeedRow opens ws's root feed and answers the first row satisfying
// pred, checking the already-materialized page first (a scenario driven to
// completion via driveScenarioToCompletion has already waited for the
// turn's own terminal row by the time this is called) and falling back to
// the live tail otherwise. Duplicated from the same shape world_test.go's
// AwaitTurnEnded uses, rather than shared, because this file may not edit
// world_test.go.
func rmAwaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if pred(row) {
			return row
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}

// rmOpenAgentBook opens the given agent's book at the store directly (a
// read-only verb, OpenAgentSession — never the forbidden WriteBatch) and
// answers its first page. Used only by TestContextInjectedMemory and
// TestContextInjectedSkills, per project-lead ruling 2: AgentContextInjected
// is a FILE-PLANE-ONLY fact with no frontend-observable surface, so this is
// the suite's one legitimate direct store read outside the harness's own
// GetSidecarCursors durability wait.
func rmOpenAgentBook(t *testing.T, w *World, agent *conversationv1.AgentId) *storev1.AgentSessionPage {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	page, err := readAgentBook(ctx, w.Store.Client, agent)
	if err != nil {
		t.Fatal(err)
	}
	return page
}

// TestCronCreateSetsTheFootersCronsChip drives `!cron-create`, which creates a
// job and leaves it standing, so the footer's ⏱ chip stays set once the turn
// has ended: a state every footer reader is handed, however far behind.
func TestCronCreateSetsTheFootersCronsChip(t *testing.T) {
	t.Parallel()
	// Arrange.
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "cron-create")
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetConcluded() == nil {
		t.Fatalf("the cron-create turn ended = %v, want a concluded outcome", ended)
	}
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Assert: the standing job reached the ONE surface cron is contracted to
	// reach -- the footer's ⏱ chip.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the footer's crons chip for the standing job", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetCrons() != nil
	})
	if got := view.GetStrip().GetLiveWork().GetCrons().GetCount(); got != 1 {
		t.Fatalf("the footer's crons chip counts %d jobs, want the 1 standing job", got)
	}
}

// ===========================================================================
// #85 PlanModeEnterExit — golden "plan-mode-enter-exit", registered scenario
// name "plan" (`!plan`, automation.ts PLAN_MODE). ONE FeedPlan bubble
// coalesces the enter and exit calls onto one FeedId (feed.proto's own doc
// comment); this asserts the FINAL "planned" state, which carries the
// document and, when the vendor named a plan file, an edit target.
// ===========================================================================

// TestPlanModeCoalescesOntoOneBubble is the SAME turn as #85 asked the other
// way: feed.proto's FeedPlan says the daemon keys the enter and the exit onto
// ONE FeedId, so a plan episode is ONE row in the feed no matter how many
// plan-mode calls it took or how many planes reported them. A screenshot of
// `!plan` showed the plan card drawn TWICE -- once where the enter landed and
// once after the turn concluded -- which is invisible to #85,
// since a duplicate satisfies "a planned row exists" perfectly.
//
// The count is read AFTER driveScenarioToCompletion, which waits for the
// sidecar's file-plane cursor to advance past this turn's transcript lines as
// well as for the turn's own terminal row: both planes have delivered
// everything they are going to deliver for this turn before the page is read.
//
// Seam: two-plane-episode
func TestPlanModeCoalescesOntoOneBubble(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "plan")

	// Assert
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	var plans []string
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if row.GetActivity().GetPlan() != nil {
			plans = append(plans, fmt.Sprintf("%s(%T)", row.GetId().GetValue(), row.GetActivity().GetPlan().GetState()))
		}
	}
	if len(plans) != 1 {
		t.Fatalf("the feed holds %d plan rows %v, want exactly 1: feed.proto's FeedPlan coalesces the enter and the exit onto ONE FeedId", len(plans), plans)
	}
}

// TestPlanModeSettlesPlanned reads the plan bubble's state off the page AFTER
// both planes have delivered, rather than waiting for a `planned` row to
// appear at any moment. The difference is the whole defect: the sidecar's copy
// of the turn arrives after the shim's, its `EnterPlanMode` last, and a bubble
// that took that enter would go back to its PLANNING treatment with the plan
// already presented. #85's wait passes either way.
func TestPlanModeSettlesPlanned(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "plan")

	// Assert
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		plan := row.GetActivity().GetPlan()
		if plan == nil {
			continue
		}
		if plan.GetPlanned() == nil {
			t.Fatalf("the plan bubble settled as %T, want planned", plan.GetState())
		}
		return
	}
	t.Fatal("the feed holds no plan bubble at all")
}

// ===========================================================================
// ChessBoard — "chess-board" (automation.ts CHESS_BOARD): the agent calls
// agent-repl's own show_chess_board tool, and the daemon draws the board.
// A test daemon has no explanation-engine checkout (the harness states the
// host's empty), so the board settles as unavailable for exactly that reason.
// ===========================================================================

func TestChessBoard(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "chess-board")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the chess board, unavailable for want of a checkout", func(r *frontendv1.FeedRow) bool {
		board := r.GetActivity().GetChessBoard()
		return r.GetTurn().GetValue() == turn.GetValue() &&
			strings.HasPrefix(board.GetUnavailable().GetReason().GetText(), "The explanation-engine checkout was not found")
	})
	if got := row.GetActivity().GetChessBoard().GetHeading().GetText(); got != "Chess board · CEE session fake-cee-session" {
		t.Fatalf("heading = %q, want the scenario's session", got)
	}
}
