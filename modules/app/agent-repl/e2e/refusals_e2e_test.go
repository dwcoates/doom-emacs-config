// refusals_e2e_test.go — SPEC.md section C, "Refusal arms" (#51-54).
//
// Every test here proves a REFUSAL crossed the real wire, driven either by a
// named fake-SDK scenario through the real shim, or — for the one arm the
// contract rules cannot be reached by any scenario prompt — by the
// documented whole-process env lever. No test in this file writes a store
// fact, a transcript file, or a wire frame by hand; every fact comes from a
// real prompt through the real daemon+store+sidecar+shim stack (SPEC.md
// section B).
//
// Contract citations, per test, are in each test's own header comment.
//
// NAMING: every unexported helper in this file is prefixed `rf` (area tag).
// All 20 area files compile into one Go package; a bare, generic name like
// `awaitFeedRow` has already collided across parallel writers once, so every
// area's own helpers get their own short tag (project-lead instruction).
package e2e

import (
	"errors"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ===========================================================================
// Shared helpers. Local to this file — nothing here is exported for another
// area file to import (SPEC.md section E: area files touch no shared file
// but harness_e2e.go/world_test.go/main_test.go).
// ===========================================================================

// rfConnectCode reads a Connect error's code, or CodeUnknown for anything
// else — this file's own copy of the same small helper
// daemon/integration/support_test.go keeps (a different Go module; nothing
// there is importable from here).
func rfConnectCode(err error) connect.Code {
	var cerr *connect.Error
	if errors.As(err, &cerr) {
		return cerr.Code()
	}
	return connect.CodeUnknown
}

// rfNamesIntendedArm reports whether a refusal names the exact unlanded
// error arm daemon/ERROR-ARMS.md documents, in its contracted
// `intended arm: <Rpc>Error.<arm>: <reason>` spelling.
func rfNamesIntendedArm(err error, arm string) bool {
	if err == nil {
		return false
	}
	msg := err.Error()
	return strings.Contains(msg, "intended arm: ") && strings.Contains(msg, arm)
}

// rfUserSaid builds the one-block plain-text UserSaid every prompt in this
// file sends — the same shape world_test.go's own SubmitPrompt helper
// builds internally, duplicated here because rfSubmitBubblePrompt needs the
// raw (non-success-asserting) response that helper deliberately does not
// offer (see its own doc comment).
func rfUserSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
	}}}
}

// rfOpenFeed opens the workspace's root feed (feed == nil) or a subagent
// bubble's own sub-feed (feed == that bubble row's FeedId, per
// endpoint_open_feed.proto: "a subagent bubble row's own FeedId ... addresses
// the feed within it"), failing the test loudly on anything but success.
func rfOpenFeed(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId) *agentreplv1.OpenFeedSuccess {
	t.Helper()
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws, Feed: feed}))
	if err != nil {
		t.Fatalf("OpenFeed(feed=%v): %v", feed, err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed(feed=%v) = %v, want success", feed, resp.Msg)
	}
	return success
}

// rfAwaitFeedRow opens the given feed (root when feed is nil, a subagent
// bubble's sub-feed otherwise) and waits until some row satisfies pred,
// checking the already-served history page first and then the live tail —
// the same two-step AwaitTurnEnded already uses in world_test.go,
// generalized to an arbitrary predicate so this file can locate a subagent
// bubble's own FeedId and a permission ask raised under it.
func rfAwaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	success := rfOpenFeed(t, w, ws, feed)
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if pred(row) {
			return row
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}

// rfSubagentOf answers a feed row's FeedSubagent, whether it arrived as a
// SYNC subagent's nested activity unit (FeedTurnActivity.subagent) or as a
// detached subagent's own top-level placement (FeedRow.detached_subagent) —
// feed.proto: "the same drawn component the detached wrapper carries; a
// bubble is a sub-feed either way." Nil if the row is neither.
func rfSubagentOf(row *frontendv1.FeedRow) *frontendv1.FeedSubagent {
	if s := row.GetActivity().GetSubagent(); s != nil {
		return s
	}
	return row.GetDetachedSubagent().GetSubagent()
}

// rfSubmitBubblePrompt submits a real prompt addressed at a subagent
// bubble's own FeedId (SubmitPromptRequest.feed set —
// endpoint_submit_prompt.proto: "SET = a subagent bubble's feed ... the
// prompt is addressed to THAT agent"), returning the raw response/error
// instead of asserting success — world_test.go's own SubmitPrompt helper
// does the opposite (asserts success), so a refusal-expecting test cannot
// use it (its own doc comment says as much).
func rfSubmitBubblePrompt(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId, text string) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	t.Helper()
	return w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace:      ws,
		Feed:           feed,
		Said:           rfUserSaid(text),
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))
}

// ===========================================================================
// #51 — BubbleRefusedNotDeliverable
//
// PROTO-CHANGES.md Landing 7 / endpoint_submit_prompt.proto:
// SubmitPromptError.bubble_refused{kind: not_deliverable}. shim.md's landing
// 3 relay: "UpdateAgent to a subagent with a prompt refuses `not_deliverable`"
// — landing 7 (shim.md's own landing 7 relay) narrowed this rule to carve out
// ONLY the busy case (#52, `agent_busy`); an IDLE (already-settled) subagent
// still answers not_deliverable, unchanged. Driven by the `subagent`
// scenario (golden name `subagent-sync-nested-activity` in the manifest;
// `!subagent` is its documented prompt — confirmed by reading
// agent-shim/claude/shim/src/fake/scenarios/subagents.ts directly, since the
// manifest's golden name and the scenario's own `name`/`prompt` fields
// differ), whose subagent completes SYNCHRONOUSLY within the driving turn —
// by the time the turn ends it is settled, never busy.
// ===========================================================================

func TestBubbleRefusedNotDeliverable(t *testing.T) {
	t.Parallel()
	// Arrange: drive a synchronous subagent to completion, then find its
	// settled bubble row.
	w := NewWorld(t, WorldOpts{})
	// THE SHIM'S REFUSAL IS THE ASSERTION BELOW. A prompt addressed to a
	// subagent has no SDK route at all, the shim answers `not_deliverable`,
	// and the delivery path records that refusal on its way to the caller.
	w.ExpectWarnings("daemon.promptqueue.deliver")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	turn := SubmitPrompt(t, w, ws, "!subagent")
	AwaitTurnEnded(t, w, ws, turn)

	bubble := rfAwaitFeedRow(t, w, ws, nil, "the settled sync-subagent bubble", func(r *frontendv1.FeedRow) bool {
		s := rfSubagentOf(r)
		return s != nil && s.GetSettled() != nil
	})

	// Act: address a fresh prompt at the now-idle subagent's own bubble.
	resp, err := rfSubmitBubblePrompt(t, w, ws, bubble.GetId(), "are you still there?")

	// Assert: SubmitPromptError.bubble_refused{not_deliverable}.
	if err != nil {
		t.Fatalf("SubmitPrompt(bubble) = error %v, want a typed SubmitPromptError", err)
	}
	refused := resp.Msg.GetError().GetBubbleRefused()
	if refused == nil {
		t.Fatalf("SubmitPrompt(bubble) = %v, want error.bubble_refused", resp.Msg)
	}
	if refused.GetNotDeliverable() == nil {
		t.Fatalf("bubble_refused.kind = %v, want not_deliverable (settled subagent)", refused)
	}
}

// ===========================================================================
// #52 — BubbleRefusedAgentBusy
//
// shim.md landing 7 relay: "UpdateAgentFailure.agent_busy: an
// UpdateAgent{prompt} to a subagent whose own turn is running is refused
// with this arm"; endpoint_submit_prompt.proto:
// SubmitPromptError.bubble_refused{kind: agent_busy}. Driven by the
// `subagent-detached-live` scenario (agent-shim/claude/shim/src/fake/
// scenarios/subagents.ts), whose subagent is "left LIVE after the turn ends"
// and "Nothing here ever finishes the agent" (its own doc comment) — a
// deterministic, never-settles busy target, avoiding any race against a
// subagent that might complete on its own.
// ===========================================================================

func TestBubbleRefusedAgentBusy(t *testing.T) {
	t.Parallel()
	// Arrange: drive the never-settling detached subagent, then find its
	// still-live bubble row.
	w := NewWorld(t, WorldOpts{})
	// THE SHIM'S REFUSAL IS THE ASSERTION BELOW. The prompt is addressed at a
	// subagent whose own turn is open, the shim answers `agent_busy`, and the
	// delivery path records that refusal on its way to the caller.
	w.ExpectWarnings("daemon.promptqueue.deliver")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	turn := SubmitPrompt(t, w, ws, "!subagent-detached-live")
	AwaitTurnEnded(t, w, ws, turn)

	bubble := rfAwaitFeedRow(t, w, ws, nil, "the live detached-subagent bubble", func(r *frontendv1.FeedRow) bool {
		s := rfSubagentOf(r)
		return s != nil && s.GetLive() != nil
	})

	// Act: address a fresh prompt at the still-running subagent's bubble.
	resp, err := rfSubmitBubblePrompt(t, w, ws, bubble.GetId(), "status update please")

	// Assert: SubmitPromptError.bubble_refused{agent_busy}.
	if err != nil {
		t.Fatalf("SubmitPrompt(bubble) = error %v, want a typed SubmitPromptError", err)
	}
	refused := resp.Msg.GetError().GetBubbleRefused()
	if refused == nil {
		t.Fatalf("SubmitPrompt(bubble) = %v, want error.bubble_refused", resp.Msg)
	}
	if refused.GetAgentBusy() == nil {
		t.Fatalf("bubble_refused.kind = %v, want agent_busy (live subagent)", refused)
	}
}

// ===========================================================================
// #53 — UnknownAgentOnUpdateAgent
//
// shim.v1 UpdateAgentFailure.kind == unknown_agent (Landing 1, "OWN ACCORD"
// arm list, PROTO-CHANGES.md). daemon/ERROR-ARMS.md ("Interrupt /
// AnswerPermission / AnswerQuestion") records `unknown_agent` (among others)
// as an arm relayed BY NAME but with NO landed `<Rpc>Error` field yet: the
// daemon answers a raw Connect error, CodeFailedPrecondition, whose message
// is exactly `intended arm: <RpcName>Error.<arm_name>: <reason>`
// (daemon/ERROR-ARMS.md's own header spells this format; `server.UnlandedArm`
// is the one helper that produces it). This row is unmodified by landing 7
// (which touched only the bubble_refused/agent_busy rows above it), so it is
// still the current, binding shape.
//
// No client-facing rpc lets a caller name an AgentId directly (Interrupt
// addresses a TURN or a detached bubble's FeedId; AnswerPermission/
// AnswerQuestion address a card's FeedId) — the daemon resolves the target
// AgentId itself from the addressed card/bubble. So "an agent id the session
// does not recognize" is reached by making the daemon resolve to an agent
// the SHIM has already forgotten: raise a gated permission ask under a
// detached subagent (`subagent-detached-live`, whose own doc comment
// promises "an AgentPermission raised UNDER the subagent"), STOP that
// subagent (its own doc comment: "AgentSubagentFailure.cause=stopped_by_user
// when the stop lands"), then answer the now-orphaned permission card.
//
// OPEN QUESTION (flagged, not guessed around): whether an ended subagent's
// own still-open ask is answered `unknown_agent` (this test's assertion) or
// is itself auto-concluded by the stop (which would answer the ALREADY-
// TYPED, ALREADY-LANDED `ask_not_standing` arm instead — a normal
// AnswerPermissionError, not a Connect error at all). Neither daemon.md nor
// ERROR-ARMS.md states which; this test asserts the ERROR-ARMS.md-documented
// shape as the more specific, more recently recorded contract fact. If the
// project lead's run shows `ask_not_standing` instead, that is exactly the
// kind of "contract point turned out ambiguous" finding this suite exists to
// surface, not a defect in this test's authorship.
// ===========================================================================

func TestUnknownAgentOnUpdateAgent(t *testing.T) {
	t.Parallel()
	// Arrange: raise a permission ask under a detached subagent, then stop
	// that subagent out from under its own still-open ask.
	w := NewWorld(t, WorldOpts{})
	// THE UNLANDED ARM IS THIS TEST'S ASSERTION. AnswerPermissionError carries
	// no `unknown_agent` arm (endpoint_answer_permission.proto), the gap is a
	// ledgered row in daemon/ERROR-ARMS.md, and server.UnlandedArm records
	// every such refusal under this operation so the ledger reconciles against
	// the log. The assertion below pins that exact spelling.
	w.ExpectWarnings("daemon.refusal.unlanded_arm")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	turn := SubmitPrompt(t, w, ws, "!subagent-detached-live")
	AwaitTurnEnded(t, w, ws, turn)

	bubble := rfAwaitFeedRow(t, w, ws, nil, "the live detached-subagent bubble", func(r *frontendv1.FeedRow) bool {
		s := rfSubagentOf(r)
		return s != nil && s.GetLive() != nil
	})
	permission := rfAwaitFeedRow(t, w, ws, bubble.GetId(), "a permission ask raised under the subagent", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission().GetOpen() != nil
	})

	interruptResp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Detached{Detached: bubble.GetId()},
	}))
	if err != nil {
		t.Fatalf("Interrupt(detached) = error %v, want success (stopping the live subagent)", err)
	}
	if interruptResp.Msg.GetSuccess().GetInterruptedDetached() == nil {
		t.Fatalf("Interrupt(detached) = %v, want success.interrupted_detached", interruptResp.Msg)
	}

	// Act: answer the now-orphaned permission ask.
	_, err = w.Client().AnswerPermission(w.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace:  ws,
		Permission: permission.GetId(),
		Answer:     &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{}},
	}))

	// Assert: the daemon/ERROR-ARMS.md documented unlanded-arm shape.
	if err == nil {
		t.Fatalf("AnswerPermission(orphaned ask) = success, want a refusal (the addressed agent is gone)")
	}
	if code := rfConnectCode(err); code != connect.CodeFailedPrecondition {
		t.Fatalf("AnswerPermission(orphaned ask) code = %v, want CodeFailedPrecondition (%s)", code, err)
	}
	if !rfNamesIntendedArm(err, "AnswerPermissionError.unknown_agent") {
		t.Fatalf("AnswerPermission(orphaned ask) = %v, want the intended-arm spelling for AnswerPermissionError.unknown_agent", err)
	}
}

// ===========================================================================
// #54 — StartSessionVendorStartFailed (+ its retry-recovery half)
//
// shim.v1 StartSessionFailure.cause.vendor_start_failed
// (proto/src/shim/v1/endpoint_start_session.proto). SPEC.md section F item 3
// (RULED): StartSessionRequest carries NO prompt text, so no scenario prompt
// can ever select this arm — it is reached only through the fake SDK's
// whole-process env lever, AGENT_REPL_FAKE_REFUSE=start(-once)
// (src/fake/index.ts lines 124-142, quoted in SPEC.md), which makes
// createFakeQuery throw synchronously on StartSession, which the shim turns
// into vendor_start_failed (confirmed directly by that file's own comment at
// the throw site: "StartSession turns this into `vendor_start_failed`").
// This lever is a whole-PROCESS knob, safe here only because NewWorld gives
// each test its own daemon+store+sidecar+shim world.
//
// LANDING 9 SETTLED THE ARM: OpenWorkspaceError.vendor_start_failed{detail}
// (docs/overhaul/PROTO-CHANGES.md "Landing 9"). The vendor failing to start
// INSIDE an already-running shim is a layer of its own — the shim process is
// up and serving, so neither OpenWorkspaceError.spawn_failed nor
// SessionFault.shim_start_failed, both of which name the SHIM PROCESS failing
// to start, describes it. The daemon relays the shim's own verdict on the
// TYPED arm, carrying the shim's account as `detail`, so these tests pin that
// arm rather than settling for "not silently healthy". The recovery half is
// unchanged: start-once's own doc comment ("a failed start leaves the engine
// as it found it ... the same warm shim serves the conversation") means the
// SAME workspace recovers and serves a real turn once the refusal clears.
// ===========================================================================

// rfAwaitVendorStartFailed drives OpenWorkspace and asserts the typed
// vendor_start_failed arm carrying the shim's own detail.
//
// The neighbouring arm is named and failed EXPLICITLY rather than lumped in
// with "some other error": OpenWorkspaceError.spawn_failed says the shim
// PROCESS never came up, which is a broken harness (a mis-staged bundle,
// missing vendor deps), not the refusal this lever provokes. Accepting it
// would let this test pass green on a world where the shim never ran at all.
func rfAwaitVendorStartFailed(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a typed OpenWorkspaceResponse", err)
	}
	oerr := resp.Msg.GetError()
	if oerr == nil {
		t.Fatalf("OpenWorkspace = %v, want error.vendor_start_failed: a refused vendor start must never "+
			"present as a silently healthy, usable session", resp.Msg)
	}
	if oerr.GetSpawnFailed() != nil {
		t.Fatalf("OpenWorkspace = spawn_failed (%v), want vendor_start_failed: the shim PROCESS "+
			"never came up, so this test never reached the arm it exists to cover — the harness or "+
			"the shim's own bring-up is broken, not the vendor", oerr)
	}
	failed := oerr.GetVendorStartFailed()
	if failed == nil {
		t.Fatalf("OpenWorkspace error = %v, want the typed vendor_start_failed arm", oerr)
	}
	if failed.GetDetail() == "" {
		t.Fatal("vendor_start_failed.detail is empty, want the shim's own account of the refused start")
	}
}

// rfVendorStartFaultWarnings are the daemon warning records a REFUSED vendor
// start legitimately produces, and they are these two tests' own subject: the
// arrangement asks the fake SDK to refuse StartSession, the daemon relays the
// typed vendor_start_failed arm, and it opens a health fault over the session
// it could not bring up. Declared for the same reason the cold-gate area
// declares `daemon.health.open_fault` (coldgate_e2e_test.go's
// coldGateWarnings): a bring-up the test deliberately breaks is a fault the
// daemon is RIGHT to record.
//
// It is declared rather than left to chance because the record lands
// ASYNCHRONOUSLY, a moment behind the rpc that caused it. Whether the sweep at
// test cleanup sees it therefore depends on how much the test does afterwards
// and how loaded the machine is — observed directly: the one-shot test, which
// runs on for another turn after the refusal, failed the sweep under a
// saturated parallel run while its every-start sibling, which ends
// immediately, did not. An undeclared fault whose observation is a race is a
// flake, and the cure is to state the fault the arrangement causes, not to
// hope the sweep runs first.
// `daemon.workspace.open` is the same refusal on the verb's own side: the
// session did not come up, which is precisely what the lever asked for.
var rfVendorStartFaultWarnings = []string{"daemon.health.open_fault", "daemon.workspace.open"}

func TestStartSessionVendorStartFailed(t *testing.T) {
	t.Parallel()
	// Arrange: a world whose every StartSession is refused by the vendor.
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{ExtraEnv: []string{"AGENT_REPL_FAKE_REFUSE=start"}}})
	// The bring-up's own record of the rejection ("nothing retries until a
	// restart") is the refusal this arrangement asks for, as in the recovering
	// sibling below.
	w.ExpectWarnings(append(rfVendorStartFaultWarnings, "daemon.workspace.bring_up")...)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act + Assert: opening the workspace answers the typed arm carrying the
	// shim's own account, never a silently healthy session.
	rfAwaitVendorStartFailed(t, w, ws)
}

func TestStartSessionVendorStartFailedRecovers(t *testing.T) {
	t.Parallel()
	// Arrange: a world whose FIRST StartSession only is refused.
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{ExtraEnv: []string{"AGENT_REPL_FAKE_REFUSE=start-once"}}})
	// The retry's own bring-up path is the RECOVERY this test is named for:
	// the refused start left the shim process up and inert, so the second
	// open attaches to that survivor rather than spawning a second one.
	w.ExpectWarnings(append(rfVendorStartFaultWarnings, "daemon.workspace.bring_up")...)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the first open attempt consumes the one scripted refusal.
	rfAwaitVendorStartFailed(t, w, ws)

	// Act: retry the same verb — start-once's own doc comment: "something
	// was wrong, it was fixed, the same warm shim serves the conversation."
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace (retry) = error %v, want success (the refusal was one-shot)", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace (retry) = %v, want success", resp.Msg)
	}

	// Assert: the recovered session serves a real turn end to end.
	turn := SubmitPrompt(t, w, ws, "!prose-streamed")
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded() == nil {
		t.Fatalf("the recovered session's turn ended row = %v, want a terminal", row)
	}
}
