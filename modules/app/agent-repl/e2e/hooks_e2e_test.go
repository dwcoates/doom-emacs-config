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
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// newHookWorld builds one test's World plus one repository (fake-git backed)
// registered as its workspace. Every hook test needs exactly this and
// nothing more — no fixture data, no pre-seeded transcripts (the grep gate
// forbids hand-authoring the latter anyway).
func newHookWorld(t *testing.T) (*World, *harness.Repo, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, repo, ws
}

// openFeedPage answers a fresh snapshot of the workspace's root feed. Called
// AFTER driveScenarioToCompletion/driveDocumentedPrompt has already
// synchronized on the turn's end and the sidecar's durable cursor advance,
// so this snapshot is a stable read of history, never a race against the
// turn still running.
func openFeedPage(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
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

// hookRow answers the one FeedHook card in a feed snapshot, or nil if the
// feed drew none — which, per bubbles.go's drawHook, is the CORRECT outcome
// for a succeeded or a cancelled hook, not a gap in this test.
func hookRow(rows []*frontendv1.FeedRow) *frontendv1.FeedHook {
	for _, row := range rows {
		if hook := row.GetActivity().GetHook(); hook != nil {
			return hook
		}
	}
	return nil
}

// TestHookSucceeded is #77 (golden hook-succeeded): the fake SDK's
// succeeding PreToolUse hook around a Read call.
//
// NAMING MISMATCH — reported, not fixed (SPEC.md F.2 "no ruling needed... a
// writer finding the landed shape differs... reports it; it is never fixed
// by changing production"): the golden is named "hook-succeeded" in
// MANIFEST.md, but the scenario `fake/scenarios/hooks.ts` actually
// registers is named "hook-success" (`prompt: "!hook-success"`).
// `fake/registry.ts` selects a scenario by an EXACT `!<name>` match on the
// scenario's own `name` field, not the golden's name, so this test drives
// "!hook-success" — the prompt that actually reaches HOOK_SUCCESS. Driving
// "!hook-succeeded" instead would silently fall through to the default
// prose scenario and never touch hook code at all.
func TestHookSucceeded(t *testing.T) {
	t.Parallel()
	// Arrange
	w, repo, ws := newHookWorld(t)

	// Act
	turn := driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, "!hook-success")

	// Assert: the turn ran to an ordinary conclusion — a succeeded hook never
	// touches how a turn ends.
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	// The feed drew NO hook card at all — bubbles.go's drawHook: "A succeeded
	// hook draws NOTHING."
	if h := hookRow(rows); h != nil {
		t.Fatalf("feed drew a hook card %v for a SUCCEEDED hook, want none", h)
	}
	// The only remaining proof the hook actually fired and succeeded is the
	// shim's own durable structured log, captured at this workspace's "shim"
	// sink (convert/hooks.ts: `LOGGER.logVerbose(..., "a hook succeeded")`,
	// under the fixed operation "shim.convert.hooks" bindLog binds it to).
	record := w.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "shim"), "the succeeded hook's own log record",
		func(r harness.LogRecord) bool {
			return r.Operation == "shim.convert.hooks" && r.Message == "a hook succeeded"
		})
	if got, want := record.Context["hook"], "PreToolUse:Read"; got != want {
		t.Fatalf("succeeded-hook log context[hook] = %v, want %q", got, want)
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
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "hook-blocked")

	// Assert: the turn still concludes ordinarily — a blocked TOOL is not a
	// stopped turn (hooks.ts's own header comment; shim.md).
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (a blocked tool never stops the turn)", ended)
	}
	h := hookRow(rows)
	if h == nil {
		t.Fatalf("feed drew no hook card, want the blocked hook's card")
	}
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
func TestHookFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	w, _, ws := newHookWorld(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "hook-failed")

	// Assert: a failed startup hook blocks nothing, so the turn still
	// concludes ordinarily.
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion (a failed hook blocks nothing)", ended)
	}
	h := hookRow(rows)
	if h == nil {
		t.Fatalf("feed drew no hook card, want the failed hook's card")
	}
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
}

// TestHookCancelled is #80 (golden hook-cancelled): the fake SDK's cancelled
// PostToolUse hook around an Edit call the hook did NOT block — the edit
// stands, and the turn ends ordinarily.
func TestHookCancelled(t *testing.T) {
	t.Parallel()
	// Arrange
	w, repo, ws := newHookWorld(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "hook-cancelled")

	// Assert: the edit stood and the turn concluded ordinarily.
	rows := openFeedPage(t, w, ws)
	ended := turnEndedRow(t, rows, turn)
	if ended.GetConcluded() == nil {
		t.Fatalf("turn ended = %v, want a plain conclusion", ended)
	}
	// The feed drew NO hook card — bubbles.go's drawHook default case:
	// "Succeeded and cancelled draw nothing."
	if h := hookRow(rows); h != nil {
		t.Fatalf("feed drew a hook card %v for a CANCELLED hook, want none", h)
	}
	// The only remaining proof the hook fired and was cancelled is the
	// shim's own durable structured log. Cancellation is an ordinary vendor
	// outcome, so convert/hooks.ts records it at INFO rather than WARN.
	record := w.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "shim"), "the cancelled hook's own log record",
		func(r harness.LogRecord) bool {
			return r.Operation == "shim.convert.hooks" && r.Message == "a hook was cancelled before it finished"
		})
	if got, want := record.Level, "info"; got != want {
		t.Fatalf("cancelled-hook log level = %q, want %q", got, want)
	}
}
