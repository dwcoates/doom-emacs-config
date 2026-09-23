// interrupt_e2e_test.go — SPEC.md §C "Interrupts" (#18-19).
//
// Both tests drive the real daemon+store+sidecar+shim stack (World, see
// world_test.go and main_test.go) against the real (`--fake`-mode) shim's
// scripted vendor. Contract: daemon.md's "Queue, holds, leases — contract
// facts" ("PERMISSION DECLINE IS DENY-AND-CONTINUE... Stopping is what
// Interrupt is for") and its "Session lifecycle" section's END act
// (`agentrepl Interrupt` as one of the narrow, single-target stops); the
// wire shape is endpoint_interrupt.proto (InterruptRequest{target: turn} ->
// InterruptResponse{success.interrupted_turn}) and frontend/v1/feed.proto's
// FeedTurnEnded.interrupted terminal arm.
//
// # #18 InterruptAfterTextDelta — an open question on the scenario's shape
//
// SPEC.md §C's own entry for this test reads: "turn terminal
// success.interrupted, streamed prose truncated exactly after the observed
// text_delta." Grounding the scenario before writing (per SPEC.md's own
// discipline: "resolve by reading the scenario body... not by guessing")
// turned up a disagreement: the golden this test drives, `interrupt`
// (agent-shim/claude/shim/src/fake/scenarios/lifecycle.ts, INTERRUPT_MID_TOOL,
// prompt "!interrupt"), is documented by its OWN scenario source as "a tool
// call the interrupt lands INSIDE: the assistant message is marked
// `aborted`... and the turn ends `error_during_execution` /
// `aborted_streaming`" — a withheld-thinking + Bash-tool_use turn. Its
// thinking block's `thinking` string is "" (withheld), so the engine's
// streaming helper (`emitBlockStream` in src/fake/index.ts) never emits a
// `thinking_delta`, and the scenario never calls `ctx.assistant` with a
// `{ type: "text", ... }` block at all — so there is structurally no
// `text_delta` for this scenario to interrupt "after." This looks like a
// genuine contract-vs-grounded-scenario disagreement rather than something
// this test should paper over by inventing a text-streaming assertion the
// scenario cannot produce; flagging it here (open question) rather than
// guessing, per the binding instruction. What IS grounded, from the same
// scenario source's own "arms" field ("AgentBashInterrupted.cause=by_user
// and AgentInterrupted.by_user") and from agent_activity.proto
// (AgentBashSuccess.outcome.interrupted is itself a SUCCESS arm, "both arms
// carry output" — completed and interrupted are siblings) and feed.proto
// (FeedToolCallReturned's only two verdicts are succeeded/failed; there is
// no third "interrupted" verdict on the wire) is: the Bash tool call reaches
// a RETURNED/succeeded state (not stuck at running, not failed) at the same
// time the turn's terminal row lands FeedTurnEndedInterrupted. This test
// asserts exactly that or ground, and no more.
//
// # #19 BashInterruptedByTimeout — a Bash-tool-own timeout, no daemon Interrupt
//
// The golden's MANIFEST name (agent-shim/claude/shim/testdata/captures/
// MANIFEST.md: "bash-interrupted-by-timeout") differs from the fake
// scenario's own registered name and prompt ("bash-timeout" / "!bash-timeout",
// src/fake/scenarios/shell.ts's BASH_TIMEOUT) — SPEC.md §D marks this golden
// "no new scenario needed", so BASH_TIMEOUT is the intended realization; this
// test drives it by its actual prompt. Per that scenario's own doc comment,
// the vendor "auto-backgrounds rather than killing" on its own timeout: the
// TURN concludes normally (`success.completed`) and the Bash unit becomes
// live detached work (AgentBashInterrupted.cause=timed_out) — the opposite
// of #18's turn-level stop, and never anything this test's own daemon client
// interrupts. The scenario's own "writes" field says the incremental spool
// carries NO `EXIT=` line, so the detached shell never reaches
// FeedShellSettled (whose outcome arms are completed/cancelled/lost only,
// per feed.proto — there is no "timed_out" wire arm to pin here); this test
// asserts the shell stays live with the scenario's own spool content, not a
// settled outcome the wire has no vocabulary for.
package e2e

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// TestInterruptAfterTextDelta drives the `interrupt` golden (#18): a real
// mid-tool interrupt, delivered through the daemon's real Interrupt rpc
// while the Bash tool call is genuinely live, must truncate the call in
// place and end the turn `success.interrupted` — not a fabricated
// AgentInterrupted the test invents by racing a sleep.
func TestInterruptAfterTextDelta(t *testing.T) {
	t.Parallel()
	// Arrange: a world with a scripted-fake-git workspace (a later user
	// ruling reversed real git: only claude-repld, the shim, shim-store, and
	// shim-sidecar run for real), and a feed watch opened BEFORE the prompt
	// is submitted, so no push in between can be missed.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	openedSuccess := opened.Msg.GetSuccess()
	if openedSuccess == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	tail := w.WatchFeedOn(w.Client(), openedSuccess.GetWatch())
	defer tail.Close()

	turn := SubmitPrompt(t, w, ws, "!interrupt")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// Act: wait until the Bash tool call is genuinely live (running, no
	// result yet) — the window INTERRUPT_MID_TOOL's own `awaitInterrupt()`
	// call is parked inside, in the same synchronous run as the tool_use
	// above it, so there is no window where the call is live and a stop has
	// nothing to resolve — then interrupt the turn for real.
	harness.AwaitView(t, ctx, tail, "the Bash tool call to start running", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() &&
			r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Bash" &&
			r.GetActivity().GetSimpleToolCall().GetRunning() != nil
	})

	resp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil {
		t.Fatalf("Interrupt(turn): %v", err)
	}
	if resp.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("Interrupt(turn) = %v, want success.interrupted_turn", resp.Msg)
	}

	// Assert: the Bash tool call settles (returned, succeeded — see the file
	// header: there is no third "interrupted" verdict on the wire), and the
	// turn's own terminal row is the interrupted arm.
	returned := harness.AwaitView(t, ctx, tail, "the Bash tool call to settle after the interrupt", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() &&
			r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Bash" &&
			r.GetActivity().GetSimpleToolCall().GetReturned() != nil
	})
	if got := returned.GetActivity().GetSimpleToolCall().GetReturned().GetSucceeded(); got == nil {
		t.Fatalf("the interrupted Bash call's returned verdict = %v, want succeeded (an interrupted run is a SUCCESS arm — see the file header)", returned.GetActivity().GetSimpleToolCall().GetReturned())
	}

	terminal := harness.AwaitView(t, ctx, tail, "the turn's interrupted terminal", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue() && r.GetTurnEnded().GetInterrupted() != nil
	})
	if terminal.GetTurnEnded().GetInterrupted() == nil {
		t.Fatalf("the turn's terminal row = %v, want turn_ended.interrupted", terminal)
	}
}

// TestBashInterruptedByTimeout drives the `bash-interrupted-by-timeout`
// golden (#19) via its registered fake scenario name, `bash-timeout` (see
// the file header). Unlike #18, nothing here calls the daemon's Interrupt
// rpc: the vendor itself reports the timeout and auto-backgrounds, so the
// turn concludes ordinarily while the Bash unit lives on as detached work.
func TestBashInterruptedByTimeout(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: drive the scenario to its own natural completion — no Interrupt
	// call anywhere in this test, deliberately, per the header note.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "bash-timeout")

	// THE SPOOL IS NOT THE TURN'S, so the turn concluding does not mean it has
	// arrived. `driveScenarioToCompletion` waits for the turn's terminal, and
	// the detached shell OUTLIVES that turn by design — its spool text is
	// appended to the spool FILE and reaches the daemon on the sidecar's own
	// pickup, after the turn is over. A page read on the line after the
	// conclusion therefore races it, and found `spool text is empty` once in
	// twenty-five in-container runs. The wait is on the rows this test is
	// about, on their feeds' own tails.
	//
	// TWO ROWS, TWO FEEDS (feed.proto, FeedRow.shell_head / detached_shell;
	// ac808eb57): the bubble's HEAD — command, clock, state — rides the ROOT
	// feed, and its spool BODY rides the bubble's own sub-feed, addressed by
	// the head's FeedId.
	initial, tail := dbOpenRootFeed(t, w, ws)
	defer tail.Close()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	head := dbAwaitShellHead(t, ctx, initial, tail, "sleep 600", "the auto-backgrounded Bash unit's shell head",
		func(*frontendv1.FeedShell) bool { return true })
	bodyInitial, body := dbOpenShellBody(t, w, ws, head)
	defer body.Close()
	if got := dbAwaitShellSpool(t, ctx, bodyInitial, body, "still going"); got == "" {
		t.Fatalf("the detached shell's spool text is empty, want the scenario's own appended output")
	}

	// Assert: fetch the settled page and find both facts durably recorded —
	// the turn's ORDINARY concluded terminal, and the Bash unit's own
	// detached-shell head, still live (no EXIT= line was ever written, per
	// the scenario's own "writes" field).
	page, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	pageSuccess := page.Msg.GetSuccess()
	if pageSuccess == nil {
		t.Fatalf("OpenFeed = %v, want success", page.Msg)
	}
	rows := pageSuccess.GetPage().GetSuccess().GetRows()

	var turnTerminal *frontendv1.FeedRow
	var shell *frontendv1.FeedShell
	for _, row := range rows {
		if row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil {
			turnTerminal = row
		}
		if row.GetDetachedShell() != nil {
			t.Errorf("the root feed carries a detached_shell BODY row %v: the spool body rides only the bubble's own sub-feed", row)
		}
		if h := dbShellHead(row, "sleep 600"); h != nil {
			shell = h
		}
	}

	if turnTerminal == nil {
		t.Fatalf("no turn_ended row found for turn %s in %d rows", turn.GetValue(), len(rows))
	}
	if turnTerminal.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("the bash-timeout turn's terminal = %v, want turn_ended.concluded (the vendor's own timeout, not a turn-level interrupt)", turnTerminal.GetTurnEnded())
	}
	if turnTerminal.GetTurnEnded().GetInterrupted() != nil {
		t.Fatalf("the bash-timeout turn's terminal carries turn_ended.interrupted, want none: this scenario is the Bash tool's OWN timeout, distinct from a turn-level interrupt (see #18)")
	}

	if shell == nil {
		t.Fatalf("no shell_head row found in %d rows, want the auto-backgrounded Bash unit's own bubble", len(rows))
	}
	if shell.GetSettled() != nil {
		t.Fatalf("the detached shell's state = settled %v, want live: the scenario's spool carries no EXIT= line, so the shell never settles", shell.GetSettled())
	}
	if shell.GetLive() == nil {
		t.Fatalf("the detached shell's state = %v, want live", shell.GetState())
	}
}
