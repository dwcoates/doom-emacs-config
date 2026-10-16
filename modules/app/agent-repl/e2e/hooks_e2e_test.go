// hooks_e2e_test.go — SPEC.md section C, "Everything else" split (project-lead
// ruling 5): #77-80, the hook family. Every test drives the REAL shim's
// `--fake` vendor through a real daemon/store/sidecar (World, world_test.go)
// and asserts only on what crossed the daemon's Connect API (OpenFeed) or
// landed in a real process's own durable structured log.
//
// Contract grounding:
//   - proto/src/conversation/v1/agent_activity.proto: message AgentHook, its
//     five result arms (start/succeeded/blocking_error/non_blocking_error/
//     cancelled).
//   - proto/src/frontend/v1/feed.proto: message FeedHook has ONLY two oneof
//     outcome arms, `blocked` and `failed` — there is no succeeded or
//     cancelled arm on the wire the daemon serves to a client at all.
//   - daemon/internal/resolve/feed/bubbles.go's drawHook: "A succeeded hook
//     draws NOTHING: quiet automation stays quiet, and only a refusal or a
//     failure is the user's business" — its `default:` case (which the
//     Cancelled arm also falls into) is commented "Succeeded and cancelled
//     draw nothing."
//   - agent-shim/claude/shim/src/convert/hooks.ts: converts the vendor's
//     `hook_started`/`hook_response` stream pair into AgentHook, and is the
//     source of the exact wire text asserted below (see HookBlocked's own
//     comment for one place this text differs from the fake scenario's
//     ATTACHMENT record, which this suite never reads — attachments are a
//     durable-transcript concern the sidecar/store own, not the daemon's
//     live stream).
//   - agent-shim/claude/shim/src/fake/scenarios/hooks.ts: the four scenarios
//     this file drives (HOOK_SUCCESS, HOOK_BLOCKED, HOOK_FAILED,
//     HOOK_CANCELLED). This is fake-SDK test tooling, not production, per
//     the task's own instruction to read it line by line — SPEC.md never
//     walked it (its own F.2, "no ruling needed... a writer finding the
//     landed shape differs... reports it; it is never fixed by changing
//     production").
//
// Every scenario here has a REAL golden capture per
// agent-shim/claude/shim/testdata/captures/MANIFEST.md ("hook-succeeded",
// "hook-blocked", "hook-failed", "hook-cancelled" — 2026-09-01, single-turn
// captures with a hook attachment). None is UNGROUNDED/INVENTED/
// DECLARED-ONLY, so no such flag is needed on any test below.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// newHookWorld builds one test's World plus one repository (fake-git backed)
// registered as its workspace. Every hook test needs exactly this and
// nothing more — no fixture data, no pre-seeded transcripts (the grep gate
// forbids hand-authoring the latter anyway).
func newHookWorld(t *testing.T) (*World, *harness.Repo, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{DurableState: true}})
	repo, ws := w.NewRepoWorkspace(t)
	return w, repo, ws
}

// openFeedPage answers a fresh snapshot of the workspace's root feed. Called
// AFTER driveScenarioToCompletion/driveDocumentedPrompt has already
// synchronized on the turn's end and the sidecar's durable cursor advance,
// so this snapshot is a stable read of history, never a race against the
// turn still running.
func openFeedPage(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	return openFeedPageOn(t, w.Daemon, ws)
}

// openFeedPageOn is openFeedPage against one named daemon — a relaunched
// successor as well as the World's own.
func openFeedPageOn(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	resp, err := d.Client().OpenFeed(d.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", resp.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}

// driveHookScenarioWatched drives one hook scenario to completion in a session
// whose feed watch already stands, and answers its turn.
//
// A FAILED OR BLOCKED HOOK'S CARD IS DRAWN LIVE ONLY (owner rulings
// 2026-10-06): the store delivers the hook line to the standing watches and
// keeps none of it. A fresh session's first hook can land before the feed's
// watch subscribes, and such a card is simply not drawn, which the owner
// accepted (shim-store's TestAHookLineWrittenBeforeTheWatchSubscribesIsNotReplayed
// pins it). So the card is asserted only where the watch stood first: an
// ordinary turn runs to its end — which reached the feed through that very
// watch — before the hook scenario is prompted.
func driveHookScenarioWatched(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, scenario string) *conversationv1.TurnId {
	t.Helper()
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	return driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, scenario)
}

// awaitHookCard waits for the hook card WHAT names on ws's root feed: in the
// page, else on its watch. A failed or blocked hook's card is live only, and
// it reaches the daemon on the store's watch while the turn's end reaches it on
// the session's own stream, so nothing orders the card before the turn's end:
// a snapshot taken once the turn has ended can predate it.
func awaitHookCard(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string) *frontendv1.FeedHook {
	t.Helper()
	return awaitFeedRow(t, w, ws, what, func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetHook() != nil
	}).GetActivity().GetHook()
}

// hookRow answers the one FeedHook card in a feed snapshot, or nil if the
// feed drew none — which, per bubbles.go's drawHook, is the CORRECT outcome
// for a succeeded or a cancelled hook, not a gap in this test.
func hookRow(rows []*frontendv1.FeedRow) *frontendv1.FeedHook {
	if hooks := hookRows(rows); len(hooks) > 0 {
		return hooks[0]
	}
	return nil
}

// hookRows answers every FeedHook card in a feed snapshot, in feed order.
func hookRows(rows []*frontendv1.FeedRow) []*frontendv1.FeedHook {
	var hooks []*frontendv1.FeedHook
	for _, row := range rows {
		if hook := row.GetActivity().GetHook(); hook != nil {
			hooks = append(hooks, hook)
		}
	}
	return hooks
}

// assertOneHookCard fails unless a feed snapshot holds exactly one hook card.
//
// ONE FIRING, ONE ROW, ACROSS BOTH PLANES. The shim's stream and the sidecar's
// transcript each carry the firing, and they must key it as one unit; two
// cards for one firing is the two planes disagreeing, which no single
// system's own suite can see.
func assertOneHookCard(t *testing.T, rows []*frontendv1.FeedRow) {
	t.Helper()
	if got := hookRows(rows); len(got) != 1 {
		t.Fatalf("hook cards = %d (%v), want exactly one for the one firing", len(got), got)
	}
}

// TestHookBlocked is #78 (golden hook-blocked): the fake SDK's blocking
// PostToolUse hook around an Edit call.
//
// FINDING — reported, not fixed: the fake scenario's own ATTACHMENT record
// (hooks.ts, `type: "hook_blocking_error"`) carries a DIFFERENT, richer
// blocking text — "the suite failed after editing /w/s/example.ts" — than
// what reaches the daemon's live feed. convert/hooks.ts derives
// FeedHookBlocked.reason from the STREAM's `hook_response` message alone:
// `blockingText = message.output !== "" ? message.output : message.stderr`,
// which for this scenario is "the suite failed after the edit" (the
// scenario's `hook_response.output`/`stderr`). The attachment's fuller
// sentence is a durable-transcript fact the sidecar/store own; it never
// reaches this wire. This test asserts the wire text, not the attachment's.
func TestHookBlocked(t *testing.T) {
	t.Parallel()
	// Arrange
	w, _, ws := newHookWorld(t)

	// Act
	turn := driveHookScenarioWatched(t, w, ws, "hook-blocked")

	// Assert: the turn still concludes ordinarily — a blocked TOOL is not a
	// stopped turn (hooks.ts's own header comment; shim.md).
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (a blocked tool never stops the turn)", ended)
	}
	h := awaitHookCard(t, w, ws, "the blocked hook's card")
	assertOneHookCard(t, openFeedPage(t, w, ws))
	blocked := h.GetBlocked()
	if blocked == nil {
		t.Fatalf("hook card = %v, want the blocked outcome arm", h)
	}
	const wantReason = "the suite failed after the edit"
	if got := blocked.GetReason(); got != wantReason {
		t.Fatalf("blocked hook reason = %q, want %q", got, wantReason)
	}
}

// TestHookFailed is #79 (golden hook-failed): the fake SDK's non-blocking
// failing SessionStart hook (exit 1 on stderr, no gated tool call at all).
//
// THE CARD IS LIVE ONLY: the shim hands the failed firing's start and outcome
// to the store, which delivers them to the standing watches and keeps only
// each row's identity (shim-store `kindHookDropped`). So the daemon that
// watched the firing draws the card, and a relaunched daemon, whose history
// pages carry no hook line, draws the turn and no card (owner rulings
// 2026-10-06; the shim's convert/hooks.test.ts "the hook records handed to the
// record plane").
func TestHookFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	w, repo, ws := newHookWorld(t)
	w.ExpectWarnings("daemon.rollout.reconcile")

	// Act
	turn := driveHookScenarioWatched(t, w, ws, "hook-failed")

	// Assert: a failed startup hook blocks nothing, so the turn still
	// concludes ordinarily.
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (a failed hook blocks nothing)", ended)
	}
	h := awaitHookCard(t, w, ws, "the failed hook's card")
	assertOneHookCard(t, openFeedPage(t, w, ws))
	failed := h.GetFailed()
	if failed == nil {
		t.Fatalf("hook card = %v, want the failed outcome arm", h)
	}
	if got := failed.GetExitCode(); got != 1 {
		t.Fatalf("failed hook exit code = %d, want 1", got)
	}
	const wantOutput = "Failed to run: no interpreter on PATH."
	if got := failed.GetOutput().GetText(); got != wantOutput {
		t.Fatalf("failed hook output = %q, want %q", got, wantOutput)
	}

	// Act: the host's stop and a fresh daemon over the same store.
	successor := rlStopAndRelaunch(t, w)
	again := harness.Register(t, successor, repo.Dir)

	// Assert: the turn is replayed from the store, and the card is not.
	adAwaitReplayedFeedRow(t, successor, again, "the hook turn's terminal, replayed after the relaunch", endsTurn(turn))
	if h := hookRow(openFeedPageOn(t, successor, again)); h != nil {
		t.Fatalf("the relaunched daemon drew hook card %v from history, want none: the store keeps no hook line a page can serve", h)
	}
}
