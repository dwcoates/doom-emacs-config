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
	"slices"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
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

// rmToolCallSettled matches a SimpleToolCall row for the given turn and tool
// name whose outcome has reached Returned (succeeded or failed).
func rmToolCallSettled(turn *conversationv1.TurnId, toolName string) func(*frontendv1.FeedRow) bool {
	return func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return row.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == toolName &&
			call.GetReturned() != nil
	}
}

// rmRequireSucceeded fails the test if the row's tool call did not settle
// with the succeeded verdict.
func rmRequireSucceeded(t *testing.T, row *frontendv1.FeedRow, toolName string) *frontendv1.FeedToolCallReturned {
	t.Helper()
	returned := row.GetActivity().GetSimpleToolCall().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("%s tool call returned = %v, want the succeeded verdict", toolName, returned)
	}
	if returned.GetFailed() != nil {
		t.Fatalf("%s tool call returned = %v, want no failed verdict alongside succeeded", toolName, returned)
	}
	return returned
}

// rmRequireFailed fails the test if the row's tool call did not settle with
// the failed verdict.
func rmRequireFailed(t *testing.T, row *frontendv1.FeedRow, toolName string) *frontendv1.FeedToolCallReturned {
	t.Helper()
	returned := row.GetActivity().GetSimpleToolCall().GetReturned()
	if returned.GetFailed() == nil {
		t.Fatalf("%s tool call returned = %v, want the failed verdict", toolName, returned)
	}
	if returned.GetSucceeded() != nil {
		t.Fatalf("%s tool call returned = %v, want no succeeded verdict alongside failed", toolName, returned)
	}
	return returned
}

// rmMainAgentID answers ws's main-agent conversation.v1.AgentId, per
// project-lead ruling 2 (this file's header comment): AgentId "never
// crosses the wire" to any frontend rpc, so the ONLY way this suite obtains
// one is the daemon's own structured log — operation
// "daemon.sessionwatcher.main_agent" (daemon/internal/sessionwatcher/
// watcher.go's adoptMainAgentLocked), logged the first time the session's
// main agent is learned, carrying "agent_id" in its Context. This waits on
// a LogRecord, one of SPEC.md §B's three named synchronization sources
// (a watch-stream frame, a LogRecord, or a bounded store read) — never a
// sleep.
func rmMainAgentID(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) *conversationv1.AgentId {
	t.Helper()
	rec := w.AwaitWorkspaceLogOperation(ws.GetDir(), "daemon.sessionwatcher.main_agent")
	id, _ := rec.Context["agent_id"].(string)
	if id == "" {
		t.Fatalf("daemon.sessionwatcher.main_agent log record carries no non-empty agent_id: %+v", rec)
	}
	return &conversationv1.AgentId{Value: id}
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
	resp, err := w.Store.Client.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent:    agent,
		PageSize: 200,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%s): %v", agent.GetValue(), err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenAgentSession(%s) = %v, want success", agent.GetValue(), resp.Msg)
	}
	return success.GetPage()
}

// rmFindContextInjected scans an agent book's page for a line carrying
// AgentActivity.context_injected, and answers the first one found (or nil).
func rmFindContextInjected(page *storev1.AgentSessionPage) *conversationv1.AgentContextInjected {
	for _, line := range page.GetLines() {
		injected := line.GetLine().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity().GetContextInjected()
		if injected != nil {
			return injected
		}
	}
	return nil
}

// ===========================================================================
// #72 ArtifactPublishAndList — golden "artifact-publish-and-list".
//
// feed.proto's FeedArtifact doc comment: "Only a PUBLISH draws; a list act
// produces no row." So the publish half asserts the FeedArtifact bubble's
// published state, and the list half — genuinely producing no feed row per
// the contract — asserts only that its own turn completes successfully
// (driveScenarioToCompletion already does this: it fails the test if the
// turn does not reach its terminal row).
// ===========================================================================

func TestArtifactPublishAndList(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act: publish.
	publishTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "artifact-publish")

	// Assert: the published bubble.
	row := rmAwaitFeedRow(t, w, ws, "the artifact-publish bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == publishTurn.GetValue() && r.GetActivity().GetArtifact().GetPublished() != nil
	})
	artifact := row.GetActivity().GetArtifact()
	if artifact.GetHeading().GetText() == "" {
		t.Fatal("artifact bubble carries no heading, want the composed favicon+title")
	}
	if artifact.GetPublished().GetUrl().GetUrl() == "" {
		t.Fatal("published artifact bubble carries no URL")
	}

	// Act: list (no row expected per feed.proto's own doc comment).
	listTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "artifact-list")
	if listTurn.GetValue() == "" {
		t.Fatal("artifact-list turn minted no id")
	}
}

// TestArtifactHeadingKeepsTheFavicon reads the published heading exactly.
// #72 only asks that it is non-empty, which a heading that lost its glyph
// satisfies — and a screenshot of the real page caught exactly that card,
// titled with no favicon, against feed.proto's "favicon emoji + title". The
// fake's `!artifact-publish` announces 📊 on the call and restates only the
// title on the outcome, which is the shape every real publish has.
func TestArtifactHeadingKeepsTheFavicon(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	publishTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "artifact-publish")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the artifact-publish bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == publishTurn.GetValue() && r.GetActivity().GetArtifact().GetPublished() != nil
	})
	if got := row.GetActivity().GetArtifact().GetHeading().GetText(); got != "📊 Offline Report" {
		t.Fatalf("artifact heading = %q, want %q", got, "📊 Offline Report")
	}
}

// ===========================================================================
// #73 ContextInjectedMemory — golden "context-injected-memory", registered
// scenario name "memory" (`!memory`, skills.ts MEMORY_INJECTED).
//
// PROJECT-LEAD RULING 2 (this file's header comment): the footer's momentary
// FooterStatusLoading arm is NOT the observable here — nothing in the
// contract ties that push to AgentContextInjected, so it would have been
// coincidental. This is a FILE-PLANE-ONLY fact (skills.ts's own file doc
// comment: "the vendor writes them and streams nothing"; daemon.md lists
// AgentContextInjected as absent from the shim's live WatchSession) whose
// only end-to-end observable is the STORE. This test drives the scenario to
// completion (which already waits, via driveScenarioToCompletion, for the
// sidecar to durably advance a cursor under the project directory — the
// same read-only-verb durability wait every other test in this suite
// relies on), then opens the main agent's book directly at the store
// (OpenAgentSession, itself a read-only verb) and asserts a page line
// carries AgentActivity.context_injected with the memory arm set.
// ===========================================================================

func TestContextInjectedMemory(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "memory")

	// Assert: a page line in the main agent's book carries the injected
	// memory fact.
	agent := rmMainAgentID(t, w, ws)
	page := rmOpenAgentBook(t, w, agent)
	injected := rmFindContextInjected(page)
	if injected == nil {
		t.Fatalf("agent %s's book carries no AgentContextInjected line", agent.GetValue())
	}
	memory := injected.GetMemory()
	if memory == nil {
		t.Fatalf("AgentContextInjected = %v, want the memory arm set", injected)
	}
	if memory.GetPath() == "" {
		t.Fatal("AgentInjectedMemory carries no path")
	}
	if memory.GetContent() == "" {
		t.Fatal("AgentInjectedMemory carries no content")
	}
}

// ===========================================================================
// #74 ContextInjectedSkills — golden "context-injected-skills", registered
// scenario name "skills-injected" (`!skills-injected`, skills.ts
// SKILLS_INJECTED). Emits an `invoked_skills` attachment among others; per
// project-lead ruling 2 (see TestContextInjectedMemory), asserted the same
// way: through the store's AgentContextInjected line, never the footer.
// ===========================================================================

func TestContextInjectedSkills(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "skills-injected")

	// Assert: a page line in the main agent's book carries the injected
	// skills fact.
	agent := rmMainAgentID(t, w, ws)
	page := rmOpenAgentBook(t, w, agent)
	injected := rmFindContextInjected(page)
	if injected == nil {
		t.Fatalf("agent %s's book carries no AgentContextInjected line", agent.GetValue())
	}
	skills := injected.GetSkills()
	if skills == nil {
		t.Fatalf("AgentContextInjected = %v, want the skills arm set", injected)
	}
	if len(skills.GetSkills()) == 0 {
		t.Fatal("AgentInjectedSkills carries no entries, want at least the fake-skill fixture")
	}
}

// ===========================================================================
// #75 CronCreateListDelete — golden "cron-create-list-delete", registered
// scenario name "cron" (automation.ts CRON). Three tool_use/tool_result
// pairs in one turn (CronCreate, CronList, CronDelete).
//
// CRON DRAWS NO TOOL CARD. feed.proto's FeedTurnActivity oneof has no cron
// arm, and the daemon's own resolver says so in its default arm
// (daemon/internal/resolve/feed/sink.go: "Every other kind that draws
// nowhere (task acts, wakeups, cron, notifications, injected context)
// answers the same way" — errNotARow).
// docs/overhaul/webapp.md:282 states the same from the client's side: "NOT
// in the feed: ... crons (footer only)". The contracted surface is the
// FOOTER's ⏱ chip (footer.proto FooterChipCrons, "Set iff at least one"),
// which the daemon's footer resolver maintains from the very AgentCron
// facts this scenario produces (internal/resolve/footer/chips.go
// applyCron). So this asserts the turn's own conclusion plus that chip —
// never a tool card.
// ===========================================================================

func TestCronCreateListDelete(t *testing.T) {
	t.Parallel()
	// Arrange. The footer is watched BEFORE the turn is driven: the
	// scenario CREATES and then DELETES the job inside one turn, so the
	// non-empty job set the chip is set from exists only in the middle of
	// the turn. A stream opened afterwards could legitimately never carry
	// it.
	w, ws := rmNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "cron")

	// Assert: the turn concluded (this golden's own terminal).
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetConcluded() == nil {
		t.Fatalf("the cron turn ended = %v, want a concluded outcome", ended)
	}

	// Assert: the cron acts reached the ONE surface they are contracted to
	// reach — the footer's ⏱ chip, set while the created job stands.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the footer's crons chip while the created job stands", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetCrons() != nil
	})
	if got := view.GetStrip().GetLiveWork().GetCrons().GetCount(); got == 0 {
		t.Fatalf("the footer's crons chip is set with count %d, want a positive job count", got)
	}
}

// ===========================================================================
// #76 Diagnostics — golden "diagnostics".
//
// OPEN QUESTION, reported rather than guessed past: no scenario named
// `diagnostics` (or anything resembling it) exists anywhere in
// agent-shim/claude/shim/src/fake/scenarios/*.ts — confirmed by grepping
// every scenario's own `name:` field in that directory. The MANIFEST row for
// this golden names NO tool at all (`hook, thinking, response ->
// success.completed`), which is exactly fake/scenarios/prose.ts's DEFAULT
// scenario shape (`name: ""`, selected by any prompt carrying no `!scenario`
// prefix). The daemon-visible fact a "diagnostics" capture golden most
// plausibly exists to pin is the shim's own SessionDiagnostics push
// (session.proto: "PUSHED at the shim's cadence and on change ... the
// pulled GetSessionDiagnostics verb is superseded by this arm"), which this
// suite can only observe indirectly through the topbar's warning strip
// (topbar.proto: TopbarWarningStrip.warnings, "Empty = nothing is wrong").
// This test therefore drives an ordinary plain prompt and asserts, BY
// ABSENCE (mirroring world_test.go's own KeepAliveNeverAppearsOnWire-style
// precedent named in SPEC.md §C #45), that a healthy session produces no
// topbar warning — the daemon-visible face of a healthy diagnostics push.
// If the project lead determines this golden names a different, real
// scenario this pass missed, that supersedes this test's approach.
// ===========================================================================

func TestDiagnostics(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act: an ordinary prompt, no `!scenario` prefix — the default prose
	// scenario, matching this golden's own tool-free capture shape.
	turn := SubmitPrompt(t, w, ws, "plain diagnostics smoke check, no tool involved")
	AwaitTurnEnded(t, w, ws, turn)

	// Assert: the topbar stream actually delivered a view (per project-lead
	// ruling 1: "no warning" must not silently pass as "no stream ever
	// arrived") AND that view carries no warning — the healthy diagnostics
	// push, by absence.
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitNext(t, ctx, topbar, "the topbar view after an ordinary turn")
	if view == nil {
		t.Fatal("the topbar stream delivered no view at all, want at least one")
	}
	if got := view.GetWarnings().GetWarnings(); len(got) != 0 {
		t.Fatalf("topbar warnings = %v, want none for a healthy session", got)
	}
}

// ===========================================================================
// #85 PlanModeEnterExit — golden "plan-mode-enter-exit", registered scenario
// name "plan" (`!plan`, automation.ts PLAN_MODE). ONE FeedPlan bubble
// coalesces the enter and exit calls onto one FeedId (feed.proto's own doc
// comment); this asserts the FINAL "planned" state, which carries the
// document and, when the vendor named a plan file, an edit target.
// ===========================================================================

func TestPlanModeEnterExit(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "plan")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the plan bubble's planned state", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetActivity().GetPlan().GetPlanned() != nil
	})
	planned := row.GetActivity().GetPlan().GetPlanned()
	if planned.GetProse().GetMarkdown() == "" {
		t.Fatal("plan bubble's planned state carries no document")
	}
	if planned.GetEdit().GetPath() == "" {
		t.Fatal("plan bubble's planned state carries no edit target, want one (PLAN_MODE names a plan file)")
	}
}

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
// #86 PushNotificationSent — golden "push-notification-sent", registered
// scenario name "push-sent" (automation.ts pushScenario, outcome=sent).
// ===========================================================================

func TestPushNotificationSent(t *testing.T) {
	t.Parallel()
	// Arrange. The footer is watched BEFORE the turn is driven: the
	// notification is raised mid-turn and a later activity can replace the
	// standing line, so a stream opened afterwards could legitimately miss it.
	w, ws := rmNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "push-sent")

	// Assert: the turn concluded. A push notification draws NO feed row —
	// feed.proto's FeedTurnActivity oneof has no notification arm, and the
	// daemon's resolver names notifications in the default arm that answers
	// errNotARow (daemon/internal/resolve/feed/sink.go). The daemon-visible
	// fact this golden pins is that the scenario reaches the daemon and its
	// turn settles the documented way.
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetConcluded() == nil {
		t.Fatalf("the push-sent turn ended = %v, want a concluded outcome", ended)
	}

	// Assert the SPECIFIC negative rather than merely declining to look: not
	// one row of the whole feed is an agent-notification row of any kind.
	// FeedTurnActivity's oneof (feed.proto) has no notification arm at all, so
	// the honest statement of "no feed row" is that the turn's rows are the
	// tool call, the response and the terminal — and nothing else appears
	// carrying the pushed message.
	rmAssertNoRowCarriesText(t, w, ws, rmPushMessage)

	// Assert the surface the push DOES reach, which the proto names:
	// FooterStatusActivityNotification is "an agent notification, shown until
	// the next activity replaces it" (footer.proto:176-177, :219-220), its
	// `text` "the composed line, drawn verbatim" (footer.proto:658-661). The
	// daemon raises it from the push's START state, message verbatim
	// (daemon/internal/resolve/footer/chips.go applyNotification), and the fake
	// pushes one fixed message for every push arm (automation.ts pushScenario).
	rmAwaitFooter(t, w, footer, "the footer's standing notification line from the sent push", func(v *frontendv1.FooterView) bool {
		return rmNotificationText(v) == rmPushMessage
	})
}

// rmPushMessage is the one message every push arm of automation.ts's
// pushScenario sends ("The offline run finished."), asserted verbatim because
// the whole path from the tool result to the footer copies it unchanged.
const rmPushMessage = "The offline run finished."

// rmNotificationText answers the footer's standing notification line whichever
// status arm is in effect, since the notification outlives the turn that raised
// it and the status underneath it therefore changes (footer.proto declares the
// same FooterStatusActivityNotification under idle, thinking and waiting).
func rmNotificationText(v *frontendv1.FooterView) string {
	status := v.GetStrip().GetStatus()
	switch {
	case status.GetIdle().GetActivity().GetNotification() != nil:
		return status.GetIdle().GetActivity().GetNotification().GetText()
	case status.GetThinking().GetActivity().GetNotification() != nil:
		return status.GetThinking().GetActivity().GetNotification().GetText()
	case status.GetWaiting().GetActivity().GetNotification() != nil:
		return status.GetWaiting().GetActivity().GetNotification().GetText()
	}
	return ""
}

// rmAssertNoRowCarriesText fails if any row of ws's materialized root feed
// draws the given text as a prompt body, a response, or a tool-call name — the
// specific negative behind "a push notification draws no feed row".
func rmAssertNoRowCarriesText(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, text string) {
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
		if row.GetAgentPrompt().GetAddress().GetText() == text {
			t.Errorf("feed row %s draws the pushed message as an agent prompt, want no row for a notification", row.GetId().GetValue())
		}
		if row.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == text {
			t.Errorf("feed row %s draws the pushed message as a response, want no row for a notification", row.GetId().GetValue())
		}
	}
}

// ===========================================================================
// #87 PushNotificationNotSent — golden "push-notification-not-sent". THREE
// registered arms exist for the "not sent" family (automation.ts
// pushScenario: push-config-off, push-user-present, push-no-transport, one
// per AgentPushNotificationNotSent disabled-reason arm) — driven as
// sub-tests of this one golden, per automation.ts's own file doc comment
// ("every arm is a separate rendering ... a mock that only ever sent one
// would leave two of them unreachable"). A notification reaches NO frontend
// surface at all (see the sent arm above), let alone a per-reason one, so
// each sub-test asserts only that its own named scenario reaches the daemon
// and its turn settles the documented way — proving the arm is reachable end
// to end, which is this golden's own point.
// ===========================================================================

func TestPushNotificationNotSent(t *testing.T) {
	t.Parallel()
	// Arrange. As in the sent arm, the footer is watched before any turn runs.
	w, ws := rmNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	cases := []string{"push-config-off", "push-user-present", "push-no-transport"}
	for _, scenario := range cases {
		t.Run(scenario, func(t *testing.T) {
			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, scenario)

			// Assert: the turn concluded. As in the sent arm above, a push
			// notification draws no feed row at all, so the reachability
			// this sub-test exists to prove is stated at the turn's own
			// terminal.
			ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
			if ended.GetConcluded() == nil {
				t.Fatalf("the %s turn ended = %v, want a concluded outcome", scenario, ended)
			}

			// The specific negative, stated rather than merely implied.
			rmAssertNoRowCarriesText(t, w, ws, rmPushMessage)

			// The surface a push DOES reach is raised from the push's START
			// state, which every arm of pushScenario emits — the
			// disabled-reason lives on the vendor's RESULT, so the standing
			// notification line stands here exactly as it does for push-sent
			// (daemon/internal/resolve/footer/chips.go applyNotification takes
			// only AgentPushNotification_Start).
			rmAwaitFooter(t, w, footer, "the footer's standing notification line from the "+scenario+" push", func(v *frontendv1.FooterView) bool {
				return rmNotificationText(v) == rmPushMessage
			})
		})
	}
}

// ===========================================================================
// #93 ReportFindings — golden "report-findings", registered scenario name
// "findings" (`!findings`, automation.ts REPORT_FINDINGS). Three findings —
// one CONFIRMED/fixed, one PLAUSIBLE/skipped, one unverdicted/no_change —
// so every verdict and outcome arm named in feed.proto's FeedFindingsRow is
// reachable from one call.
// ===========================================================================

func TestReportFindings(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "findings")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the findings bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetActivity().GetFindings() != nil
	})
	findings := row.GetActivity().GetFindings()
	rows := findings.GetRows()
	if len(rows) != 3 {
		t.Fatalf("findings bubble has %d rows, want 3", len(rows))
	}
	if rows[0].GetVerdict() == nil {
		t.Fatal("findings row 0 carries no verdict, want CONFIRMED")
	}
	if rows[0].GetOutcome() == nil {
		t.Fatal("findings row 0 carries no outcome, want fixed")
	}
	if rows[1].GetVerdict() == nil {
		t.Fatal("findings row 1 carries no verdict, want PLAUSIBLE")
	}
	if rows[2].GetVerdict() != nil {
		t.Fatal("findings row 2 carries a verdict, want none (unverified)")
	}
}

// ===========================================================================
// #94 ScheduleWakeupScheduleAndStop — golden
// "schedule-wakeup-schedule-and-stop", registered as TWO scenario names
// (automation.ts WAKEUP_SCHEDULE "wakeup-schedule", WAKEUP_STOP
// "wakeup-stop") — driven as one combined test.
// ===========================================================================

func TestScheduleWakeupScheduleAndStop(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// A wakeup draws NO feed row: feed.proto's FeedTurnActivity oneof has no
	// wakeup arm, and "wakeups" is named outright in the daemon resolver's
	// draws-nothing default arm (daemon/internal/resolve/feed/sink.go). The
	// FOOTER is where the contract puts it, so both arms are pinned there as
	// well as at their own turn terminals.
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act + Assert: schedule.
	scheduleTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "wakeup-schedule")
	if ended := AwaitTurnEnded(t, w, ws, scheduleTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the wakeup-schedule turn ended = %v, want a concluded outcome", ended)
	}
	// The pending wakeup owns the footer's status once the turn is done:
	// footer.proto:241-243 declares FooterSubStatusWaitingWakeup as "the
	// self-scheduled wakeup fallback: shown only when the footer would
	// otherwise read idle/done", and the resolver publishes exactly that arm
	// while a scheduled wakeup stands
	// (daemon/internal/resolve/footer/status.go's wakeup()).
	//
	// The ⏱ chip is pinned on the SAME view rather than by a second wait. Both
	// facts are rendered from the one `s.wakeup` field — the waiting arm by
	// footer/status.go's wakeup(), the chip by chips.go:547-551, which counts
	// the pending wakeup alongside `s.crons` because footer.proto:849-851
	// words that chip as "live scheduled jobs (cron/wakeup schedules)". So a
	// single publish carries both, and nothing obliges the daemon to publish
	// again afterwards: the schedule turn has concluded and the session is
	// idle. A second AwaitView on the same stream would consume only pushes
	// AFTER the view the first wait returned, and so hangs out its whole
	// budget whenever no unrelated update happens to follow — the observed
	// flake under -parallel 8, where the daemon log falls silent between the
	// wakeup publish and teardown.
	view := rmAwaitFooter(t, w, footer, "the footer's waiting-on-wakeup status while the scheduled wakeup stands", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetWakeup() != nil
	})
	if got := view.GetStrip().GetLiveWork().GetCrons().GetCount(); got == 0 {
		t.Fatalf("the footer's ⏱ chip counts %d scheduled jobs while a wakeup stands, want the wakeup counted: %v", got, view.GetStrip().GetLiveWork())
	}

	// Act + Assert: stop.
	stopTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "wakeup-stop")
	if ended := AwaitTurnEnded(t, w, ws, stopTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the wakeup-stop turn ended = %v, want a concluded outcome", ended)
	}
	// The stop RETIRES both: applyWakeup's Stopped outcome clears `s.wakeup`,
	// so the waiting arm goes away and the footer falls back to the idle/done
	// status the fallback was only ever standing in front of.
	rmAwaitFooter(t, w, footer, "the footer's waiting-on-wakeup status to be retired by the stop", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetWakeup() == nil &&
			v.GetStrip().GetLiveWork().GetCrons() == nil
	})
}

// ===========================================================================
// #95 SendMessageQueuedAndResumed — golden
// "send-message-queued-and-resumed", registered as TWO scenario names
// (tasks.ts SEND_MESSAGE_QUEUED "send-message", SEND_MESSAGE_RESUMED
// "send-message-resumed") — driven as one combined test.
//
// CORRECTED: an earlier version of this header said a send "reaches no
// frontend surface of the sender's at all". It does. sink.go routes
// AgentActivity_SendMessage to drawSendMessage, which draws a FeedAgentPrompt
// on the SENDER's own feed — address "→ <recipient label>", body the caller's
// one-line summary (daemon/internal/resolve/feed/sendmessage.go, landed by "a
// SendMessage draws agent_prompt on the sender's feed with a composed address
// line and the caller's summary"). Each delivery is pinned on that row.
//
// The corpus-noted discriminator between the two deliveries is
// `resumedAgentId`, present only on the resumed arm; that half genuinely has
// no drawn shape — see the dispute noted in the body.
// ===========================================================================

func TestSendMessageQueuedAndResumed(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// A send DOES draw on the sender's own feed, as an agent_prompt row:
	// feed.proto:1421-1433's FeedAgentPrompt is "ONE component, both ends: on
	// the SENDER's feed it is the outgoing send, on the recipient's the
	// delivered prompt; only the address line differs", its address "'→
	// Explore' on the sender's feed", its body the prompt's content. The
	// daemon composes exactly that — `"→ " + sendRecipientLabel(...)` with the
	// caller's ONE-LINE SUMMARY as the sole body block, never the relayed body
	// (daemon/internal/resolve/feed/sendmessage.go, landed as "a SendMessage
	// draws agent_prompt on the sender's feed with a composed address line and
	// the caller's summary").

	// Act + Assert: queued (to a live agent). tasks.ts SEND_MESSAGE_QUEUED
	// addresses the literal id "a1234567890abcde" with the summary "check the
	// branch"; nothing in this workspace has a feed for that id, so
	// sendRecipientLabel falls to rule 2 — the addressed string exactly as the
	// caller wrote it — and both halves of the row are exact.
	queuedTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "send-message")
	if ended := AwaitTurnEnded(t, w, ws, queuedTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the send-message (queued) turn ended = %v, want a concluded outcome", ended)
	}
	// THE WAIT IS ON THE DELIVERED ROW, NOT MERELY ON A ROW. Every frame of the
	// send upserts the SAME row (drawSendMessage's own contract): the start
	// draws the address and the summary with `delivery` still unset, and only
	// the success carries the arm. The two reach the daemon by DIFFERENT
	// paths — the turn's own lifecycle comes off the shim's control plane
	// while the agent activity is tailed out of the store — so the turn's
	// terminal row is routinely published before the send's success is drawn,
	// and OpenFeed's page then legitimately answers the start-version of the
	// row. Waiting on `GetAgentPrompt() != nil` therefore captured a row the
	// daemon had not finished (observed: the start upsert at 12:09:50.711, the
	// page read at .713, the success upsert at .722). Waiting on the arm this
	// test is ABOUT loses no coverage — an arm that never arrives times the
	// wait out and fails — and it is the only version of the row the
	// assertions below are about.
	queuedRow := rmAwaitFeedRow(t, w, ws, "the sender's outgoing-send agent_prompt row, delivered", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == queuedTurn.GetValue() && r.GetAgentPrompt().GetQueuedToLive() != nil
	}).GetAgentPrompt()
	if got, want := queuedRow.GetAddress().GetText(), "→ a1234567890abcde"; got != want {
		t.Errorf("outgoing send address = %q, want %q", got, want)
	}
	if got, want := rmAgentPromptText(t, queuedRow), "check the branch"; got != want {
		t.Errorf("outgoing send body = %q, want the caller's summary %q", got, want)
	}
	// The delivery arm (landing 10) — the recipient was already live, so the
	// message queued for it and nothing was started — is what the wait above
	// is predicated on, so reaching this line IS that assertion: an arm that
	// never arrived would have timed the wait out and failed the test.

	// Act + Assert: resumed (idle agent, resumed from transcript). The
	// recipient id is MINTED by the fake (ctx.mintAgentTaskId()), so the
	// address is pinned by its composed shape rather than a literal, while the
	// summary — the half the contract says a surface draws — is exact.
	//
	// The RESUMPTION has its own drawn shape as of landing 10:
	// FeedAgentPrompt.resumed_recipient mirrors
	// AgentSendMessageResumedRecipient, so a reader can tell the delivery that
	// "RESTARTED A DORMANT AGENT, which begins consuming tokens again" from the
	// one that cost nothing beyond the message.
	resumedTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "send-message-resumed")
	if ended := AwaitTurnEnded(t, w, ws, resumedTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the send-message-resumed turn ended = %v, want a concluded outcome", ended)
	}
	// Waited on the resumed arm for the same reason the queued half waits on
	// its own: the start-version of this row carries no delivery either, and
	// its address is not yet resolved to the minted recipient.
	resumedRow := rmAwaitFeedRow(t, w, ws, "the resumed send's agent_prompt row, delivered", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == resumedTurn.GetValue() && r.GetAgentPrompt().GetResumedRecipient() != nil
	}).GetAgentPrompt()
	address := resumedRow.GetAddress().GetText()
	if !strings.HasPrefix(address, "→ ") {
		t.Errorf("resumed send address = %q, want the composed \"→ <recipient>\" form", address)
	}
	if name := strings.TrimPrefix(address, "→ "); name == "" || name == sendNotNamedLabel {
		t.Errorf("resumed send address = %q, want it to name the resumed recipient", address)
	}
	if got, want := rmAgentPromptText(t, resumedRow), "resume the sweep"; got != want {
		t.Errorf("resumed send body = %q, want the caller's summary %q", got, want)
	}
	// The resumed_recipient arm, like the queued arm above, is the fact the
	// wait for this row is predicated on; reaching here is that assertion.
}

// sendNotNamedLabel is the daemon's honest stand-in when nothing on the wire
// names a send's recipient (daemon/internal/resolve/feed/sendmessage.go's
// sendNotNamed) — the one label the resumed arm's address must NOT be.
const sendNotNamedLabel = "an agent the send did not name"

// rmAgentPromptText answers the single text block an outgoing send's body
// carries, failing loudly on any other shape: the contract is that the drawn
// body is the caller's one-line summary and nothing else.
func rmAgentPromptText(t *testing.T, prompt *frontendv1.FeedAgentPrompt) string {
	t.Helper()
	blocks := prompt.GetBody().GetBlocks()
	if len(blocks) != 1 {
		t.Fatalf("outgoing send body has %d blocks, want exactly 1 (the caller's summary)", len(blocks))
	}
	text := blocks[0].GetText()
	if text == nil {
		t.Fatalf("outgoing send body block = %v, want a text block", blocks[0])
	}
	return text.GetText()
}

// ===========================================================================
// #96 TaskActsCreateChangeReject — golden "task-acts-create-change-reject",
// registered as THREE scenario names (tasks.ts TASK_CREATE "task-create",
// TASK_CHANGE "task-change", TASK_REJECT "task-reject") — driven as one
// combined test.
//
// A TASK ACT DRAWS NO TOOL CARD. feed.proto:279 retires the feed's task arm
// outright — "Tag 4 is RETIRED: the task bubble left the feed — tracker tasks
// draw in the FOOTER's checklist only" — and the feed resolver's own sink
// answers every task act with errNotARow, recording
// `daemon.feed.activity_draws_nothing` ("task acts" are named in that
// default arm's comment, internal/resolve/feed/sink.go:87-97). The earlier
// assertion here awaited a settled `TaskUpdate` tool card for each of the
// three turns, which the contract says can never exist; the contracted
// surfaces are the turn's own terminal and the footer's ☑ checklist, which
// footer/chips.go applyTaskAct maintains from these very acts. This mirrors
// TestCronCreateListDelete above, whose acts are footer-only for the same
// reason.
//
// task-reject stays the negative: the board REFUSES the update
// (`success: false`, `isError: true`), and applyTaskAct's own contract is
// that "a REJECTED act still carries the task as it stands, so the state is
// applied either way" — so the checklist survives the rejection rather than
// being torn down by it.
// ===========================================================================

func TestTaskActsCreateChangeReject(t *testing.T) {
	t.Parallel()
	// Arrange. The footer is watched before the first turn is driven, so
	// every checklist state each turn publishes is queued in order.
	w, ws := rmNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act + Assert: create (two TaskCreate calls plus a linking TaskUpdate).
	createTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "task-create")
	if ended := AwaitTurnEnded(t, w, ws, createTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the task-create turn ended = %v, want a concluded outcome", ended)
	}
	// Both created tasks stand, neither done: the ☑ chip's fraction is the
	// checklist's summary (footer.proto FooterChipTasks).
	rmAwaitFooter(t, w, footer, "the ☑ chip carrying both created tasks", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetTasks().GetTotal() == 2
	})

	// Act + Assert: change (status pending -> in_progress). applyTaskAct
	// projects `running` onto the checklist row, which the panel draws as
	// FooterTaskRowRunning.
	changeTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "task-change")
	if ended := AwaitTurnEnded(t, w, ws, changeTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the task-change turn ended = %v, want a concluded outcome", ended)
	}
	rmAwaitFooter(t, w, footer, "the checklist's running row after task-change", func(v *frontendv1.FooterView) bool {
		for _, row := range v.GetExpanded().GetTasks().GetRows() {
			if row.GetStatus().GetRunning() != nil {
				return true
			}
		}
		return false
	})

	// Act + Assert: reject (the board refuses the update). The turn still
	// concludes, and the checklist still stands.
	rejectTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "task-reject")
	if ended := AwaitTurnEnded(t, w, ws, rejectTurn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the task-reject turn ended = %v, want a concluded outcome", ended)
	}
	view := rmAwaitFooter(t, w, footer, "the checklist standing after the rejected act", func(v *frontendv1.FooterView) bool {
		return len(v.GetExpanded().GetTasks().GetRows()) > 0
	})
	if got := view.GetStrip().GetLiveWork().GetTasks(); got == nil {
		t.Fatalf("the footer's ☑ chip is unset after the rejected act, want the checklist still summarized: %v", view.GetStrip().GetLiveWork())
	}
}

// rmAwaitFooter waits for a footer view satisfying pred, on this area's
// ordinary per-wait budget.
func rmAwaitFooter(t *testing.T, w *World, footer *FooterWatch, what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, footer.Stream, what, pred)
}

// ===========================================================================
// #97 WebFetch — golden "web-fetch", registered scenario name "web-fetch"
// (web.ts WEB_FETCH). Name matches exactly; no mismatch to flag here.
// ===========================================================================

func TestWebFetch(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "web-fetch")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the WebFetch settled tool card", rmToolCallSettled(turn, "WebFetch"))
	returned := rmRequireSucceeded(t, row, "WebFetch")
	if returned.GetText().GetText() == "" {
		t.Fatal("WebFetch tool call carries no text output, want the fetched page's summarized result")
	}
}

// ===========================================================================
// #98 WebSearch — golden "web-search", registered scenario name "web-search"
// (web.ts WEB_SEARCH). Name matches exactly. The results array is
// deliberately mixed (a hit list plus a bare commentary string, per web.ts's
// own file doc comment), which AgentWebSearchSuccess splits into `link` and
// `note` entries — this test only asserts the tool settled and rendered a
// non-empty links output; it does not pin the note arm, which the generic
// FeedSimpleToolCall links form may or may not carry as a distinct element
// (lower depth than the focused areas, per this file's own dispatch scope).
// ===========================================================================

func TestWebSearch(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "web-search")

	// Assert
	row := rmAwaitFeedRow(t, w, ws, "the WebSearch settled tool card", rmToolCallSettled(turn, "WebSearch"))
	returned := rmRequireSucceeded(t, row, "WebSearch")
	links := returned.GetLinks()
	if links == nil || len(links.GetLinks()) == 0 {
		t.Fatalf("WebSearch tool call returned = %v, want a non-empty links output form", returned)
	}
	// THE GROUP'S PAGES, EACH ITS OWN ROW. web.ts's fixture answers one hit
	// group of two pages plus one bare narration string, so the drawn answer
	// is THREE rows in the engine's order. Asserting only "non-empty" passed
	// while the transcript plane read `title`/`url` off the GROUP rather than
	// its `content` array: it minted one row with an empty title and an empty
	// href and lost both pages, and the card drew an invisible dead row where
	// two clickable results belonged.
	type linkRow struct{ text, url string }
	var got []linkRow
	for _, link := range links.GetLinks() {
		got = append(got, linkRow{text: link.GetText(), url: link.GetUrl().GetUrl()})
	}
	want := []linkRow{
		{text: "Example API reference", url: "https://docs.example.com/reference/"},
		{text: "Example changelog", url: "https://docs.example.com/changelog/"},
		{text: "The reference page covers every method; the changelog lists recent additions.", url: ""},
	}
	if !slices.Equal(got, want) {
		t.Fatalf("WebSearch drawn link rows = %+v, want %+v", got, want)
	}
}

// ===========================================================================
// #99 WorktreeEnterExitKeptAndRemoved — golden
// "worktree-enter-exit-kept-and-removed", registered as TWO scenario names
// (automation.ts WORKTREE_KEEP "worktree-keep", WORKTREE_REMOVE
// "worktree-remove") — driven as one combined test. A worktree move renders
// as a FeedSessionSeparation divider (feed.proto: "the session moved INTO an
// isolated git worktree" / "the session RETURNED from the worktree"), not a
// FeedSimpleToolCall — this is the one automation-family tool whose calls
// do NOT ride the generic tool-card shell.
//
// A SEPARATION ROW CARRIES NO TURN. feed.proto's FeedRow.turn is documented
// "Unset for a row that belongs to no turn (a separation divider)"
// (feed.proto:92-95), so matching a divider by turn id matches nothing. The
// predicates below therefore key on the separation arm alone, and the two
// scenarios are driven one at a time so each pair of dividers is
// unambiguous.
// ===========================================================================

func TestWorktreeEnterExitKeptAndRemoved(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act + Assert: keep.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "worktree-keep")
	entered := rmAwaitFeedRow(t, w, ws, "the worktree-entered divider (keep)", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeEntered() != nil
	})
	if entered.GetSeparation().GetWorktreeEntered().GetPath().GetText() == "" {
		t.Fatal("worktree-entered divider carries no path")
	}
	left := rmAwaitFeedRow(t, w, ws, "the worktree-left divider (kept)", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeLeft().GetKept() != nil
	})
	if left.GetSeparation().GetWorktreeLeft().GetKept().GetPath().GetText() == "" {
		t.Fatal("worktree-left (kept) divider carries no path")
	}

	// Act + Assert: remove.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "worktree-remove")
	left = rmAwaitFeedRow(t, w, ws, "the worktree-left divider (removed)", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeLeft().GetRemoved() != nil
	})
	if left.GetSeparation().GetWorktreeLeft().GetRemoved().GetDiscarded() == nil {
		t.Fatal("worktree-left (removed) divider carries no discarded-files/commits line, want one composed (the fixture discards 3 files, 1 commit)")
	}
}

// TestWorktreeDiscardLineIsGrammatical reads the loud discard line the removal
// composes. The fake's `!worktree-remove` discards THREE files and ONE commit,
// and a screenshot caught that line as "3 files, 1 commits discarded" —
// the one figure a reader is most likely to be alarmed by, misspelled.
func TestWorktreeDiscardLineIsGrammatical(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "worktree-remove")

	// Assert
	left := rmAwaitFeedRow(t, w, ws, "the worktree-left divider (removed)", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeLeft().GetRemoved() != nil
	})
	got := left.GetSeparation().GetWorktreeLeft().GetRemoved().GetDiscarded().GetText()
	if got != "3 files, 1 commit discarded" {
		t.Fatalf("discard line = %q, want %q", got, "3 files, 1 commit discarded")
	}
}

// ===========================================================================
// Coverage extension — `!send-message-refused`: an undeliverable send.
// ===========================================================================

// TestSendMessageRefused drives "!send-message-refused" (tasks.ts's
// SEND_MESSAGE_REFUSED): a `SendMessage` addressed to an agent the user
// stopped, answered `success: false` with the vendor's refusal prose. The
// scenario's declared arm is conversation/v1's AgentSendMessageFailure.
//
// # WHAT THE FRONTEND CONTRACT SAYS ABOUT A REFUSED SEND
//
// A send is NOT drawn as a tool card. feed.proto's FeedTurnActivity oneof has
// no send arm; a send is drawn with FeedAgentPrompt instead, "ONE component,
// both ends: on the SENDER's feed it is the outgoing send, on the
// recipient's the delivered prompt; only the address line differs", and
// daemon/internal/resolve/feed/sendmessage.go's drawSendMessage is the
// composer for the sender's end. Its failure arm draws the row against what
// the START said, with this comment: "A send that could not be delivered
// still HAPPENED, and its row is what explains the attempt."
//
// THE GAP THIS TEST RECORDED IS CLOSED (landing 14). FeedAgentPrompt's
// `delivery` oneof once carried only the two arms that say how a send LANDED,
// so a refused send was drawn exactly as one whose producer merely stated
// nothing — the refusal was indistinguishable from an absence. The oneof now
// carries a third arm, `refused`, whose only field is the producer's own
// refusal words: a reason and no kind, because a reason is all the vendor
// gives.
//
// So this asserts the whole path end to end — the attempt is drawn AND
// SURVIVES the refusal (a resolver that dropped the row on the failure arm,
// or that invented a recipient identity the refusal never resolved, would
// fail here), the delivery states the REFUSAL BY NAME rather than the silence
// that reads as "not yet delivered" (a resolver that left it unset, or that
// reported a landing the refusal never achieved, would fail here), the reason
// is the vendor's own prose carried verbatim from the shim's converter
// through the daemon (a producer that dropped the tool result's content, or a
// resolver that synthesized wording of its own, would fail here), and the
// turn still concludes.
func TestSendMessageRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := rmNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "send-message-refused")

	// Assert: the attempt is drawn on the SENDER's feed as an agent_prompt,
	// addressed with the outgoing "→ " prefix drawSendMessage composes and
	// naming the recipient EXACTLY AS THE CALLER WROTE IT — the refusal
	// resolved no identity, and sendRecipientLabel must not invent one.
	const recipient = "a85a6434719755df1"
	row := rmAwaitFeedRow(t, w, ws, "the refused send's own agent_prompt row", func(r *frontendv1.FeedRow) bool {
		p := r.GetAgentPrompt()
		return p != nil && strings.HasPrefix(p.GetAddress().GetText(), "→ ")
	})
	prompt := row.GetAgentPrompt()
	if got, want := prompt.GetAddress().GetText(), "→ "+recipient; got != want {
		t.Errorf("refused send address = %q, want %q — the addressed string verbatim, never a synthesized "+
			"identity and never the \"an agent the send did not name\" fallback", got, want)
	}

	// Assert: the body is the caller's SUMMARY, never the message itself —
	// AgentSendMessage's contract forbids drawing the body, and a refusal is
	// no license to start.
	var bodies []string
	for _, block := range prompt.GetBody().GetBlocks() {
		bodies = append(bodies, block.GetText().GetText())
	}
	if len(bodies) != 1 || bodies[0] != "continue" {
		t.Errorf("refused send body blocks = %q, want exactly the caller's summary [%q]", bodies, "continue")
	}

	// Assert: NOTHING was delivered, and the row SAYS SO BY NAME. An unset
	// delivery is what a producer that stated nothing leaves behind, and a
	// reader cannot tell that apart from a message still on its way.
	refused := prompt.GetRefused()
	if refused == nil {
		t.Fatalf("refused send delivery = %T, want the refused arm — an unset delivery is "+
			"indistinguishable from a send whose producer simply stated no delivery", prompt.GetDelivery())
	}

	// Assert: the refusal carries the VENDOR's own words, verbatim. The vendor
	// declares no refusal code, so this prose is the only thing it ever says
	// about why the send was refused.
	const prose = "The agent was stopped by the user."
	if got := refused.GetReason().GetText(); got != prose {
		t.Errorf("refusal reason = %q, want the vendor's own prose %q — never wording the "+
			"daemon or the client invented", got, prose)
	}

	// Assert: the refusal is the VENDOR's, not the turn's — a tool that
	// answered `success: false` still lets the agent conclude.
	if ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded(); ended.GetConcluded() == nil {
		t.Fatalf("the send-message-refused turn ended = %v, want a concluded outcome", ended)
	}
}
