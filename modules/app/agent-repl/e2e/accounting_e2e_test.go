// accounting_e2e_test.go — SPEC.md §C "Usage / accounting" (#58-59). See
// SPEC.md (read-only) for the harness this file builds on: NewWorld,
// SubmitPrompt/AwaitTurnEnded, driveScenarioToCompletion, harness.Register.
//
// Contract citations:
//
//   - #58 AccountUsage drives `!usage-full`
//     (agent-shim/claude/shim/src/fake/scenarios/session.ts). USAGE_FULL's
//     own doc comment names it as "the SAME available shape under the name
//     the e2e roster reaches for" — i.e. this scenario is the one this
//     suite is meant to drive for this golden, distinct from
//     `!usage-available` (same shape, older name) and from
//     `!usage-subagent`, which the SPEC's own §C entry says is "a
//     DIFFERENT, narrower fixture [that] does not cover this golden."
//     `usage-full` sets every window (`five_hour`, `seven_day`,
//     `seven_day_oauth_apps`, `seven_day_opus`, `seven_day_sonnet`,
//     `model_scoped`, `extra_usage`) via the same `accountInfo`/
//     `usage_EXPERIMENTAL` controls docs/overhaul/shim.md §"Session facts
//     with no message behind them" describes ("account usage →
//     query.usage_EXPERIMENTAL…()"), landing as
//     conversation.v1 SessionUpdate.account_usage
//     (SessionAccountUsage/SessionAccountUsageAvailable). The captured
//     golden `account-usage`
//     (agent-shim/claude/shim/testdata/captures/MANIFEST.md) grounds the
//     same shape from a real vendor recording of the identical controls;
//     this suite drives the scripted `--fake` shim, never a replayed
//     capture, so what is asserted here is the scripted push landing and
//     being filed by the daemon (daemon/internal/resolve/footer/
//     resolver.go's `observeAccountUsage`), not the capture itself.
//     The account-usage figures this scenario sets (five_hour 41%, weekly
//     windows single digits to 13%) sit below the footer's
//     DefaultRateLimitNewsworthyThreshold (0.8,
//     daemon/internal/resolve/footer/api.go), so the allowance never
//     surfaces as a drawn "rate limited" status line — that line is gated
//     on a SEPARATE newsworthiness fact this golden does not exercise. The
//     assertable contract fact at this scenario's own figures is that the
//     push arrived and was filed, observed via the daemon's own structured
//     log record for the arm (an explicitly sanctioned wait signal per
//     SPEC.md §B "Waits": "a LogRecord in the daemon's, store's, or
//     sidecar's own structured log").
//
//   - #59 ContextUsage drives `!context-usage-drift` (session.ts), one of
//     "the e2e mock additions" docs/overhaul/shim.md documents for exactly
//     this push contract: "context_usage is pushed at session start and at
//     every turn end regardless of scenario... CADENCE IS THE ENGINE'S...
//     a scenario can only change WHAT is sampled, never WHEN" — this
//     scenario is the one whose sampled total GROWS turn over turn, so a
//     test can observe a push happening without ever pulling. This is the
//     shim.md §"Simple reads the shim now PUSHES on the session stream
//     (context usage, diagnostics) — no pull rpcs remain" contract point
//     SPEC.md's own §C entry cites. Two turns are driven, and the
//     daemon-resolved `/context` panel (frontend.v1 ContextPanelView, via
//     SubmitPromptCommandPanel.context) is read after each: the panel
//     resolves purely from conversation.v1 SessionContextUsage state the
//     shim already pushed (daemon/internal/resolve/topbar/contextpanel.go's
//     own doc comment: "resolves... from the SAME SessionContextUsage fact
//     the context chip resolves from... never an estimate and never
//     derived from usage frames"), so reading it a second time and finding
//     it moved is the push contract in action — the TEST never calls any
//     pull verb itself.
//
// Neither `usage-full` nor `context-usage-drift` carries an
// UNGROUNDED/INVENTED/DECLARED-ONLY mark in
// agent-shim/claude/shim/testdata/captures/MANIFEST.md.
//
// OPEN QUESTION for the project lead, not resolved here: this file's own
// dispatch brief said the deleted suite's hand-written nested-subagent
// usage events are retired by `!usage-historical`
// (agent-shim/claude/shim/src/fake/scenarios/subagents.ts), which the
// manifest marks UNGROUNDED/INVENTED, and that this file should drive it.
// SPEC.md §C instead assigns `!usage-historical` to test #30
// (NestedSubagentHistoricalUsage, subagents_e2e_test.go's own row) and this
// file's assigned entries are #58-59 (AccountUsage, ContextUsage) only —
// neither citing usage-historical, and SPEC.md's §D scenario-mapping table
// lists `!usage-historical` only under §D's "extra options" list feeding
// test #30, never under account-usage or context-usage. This file therefore
// does NOT drive `usage-historical`, to stay inside SPEC.md's own
// assignment for this file; flagging the discrepancy rather than guessing
// which document should yield.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// submitCommand submits a daemon-recognized slash command (never a plain
// prompt) and answers its SubmitPromptSuccess. Unlike SubmitPrompt (this
// package's helper for prompts that mint a turn), a command answers
// SYNCHRONOUSLY with a command_panel/command_acted/command_refused arm and
// mints no turn, so it is asserted here rather than through SubmitPrompt,
// which fails a test that does not get a minted turn back.
func submitCommand(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, command string) *agentreplv1.SubmitPromptSuccess {
	t.Helper()
	resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: command}}},
		}}},
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt(%q): %v", command, err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("SubmitPrompt(%q) = %v, want a success", command, resp.Msg)
	}
	return success
}

// TestAccountUsage drives #58: `!usage-full` pushes
// conversation.v1 SessionUpdate.account_usage, and the daemon's footer
// resolver files it (daemon/internal/resolve/footer/resolver.go's
// observeAccountUsage), observed via the resolver's own structured log
// record for the arm — see this file's header comment for why the figures
// this scenario carries never cross the footer's separate newsworthiness
// threshold and so are not asserted as a drawn status line.
func TestAccountUsage(t *testing.T) {
	t.Parallel()
	// Arrange: one world, one (fake-git) repository registered as a
	// workspace.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: drive the scenario that switches the account-usage probe to the
	// full/available shape, to its own turn-ended, store-durable
	// completion.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-full")

	// Assert: the turn concluded (this scenario's own golden terminal is
	// success.completed).
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("usage-full turn ended = %v, want a concluded (success.completed) outcome", row.GetTurnEnded())
	}

	// Assert: the daemon's own footer resolver logged that it took the
	// account_usage arm off the session stream for this workspace — the
	// structured-log wait signal SPEC.md §B names for exactly this
	// situation (a fact with no user-visible surface at this scenario's own
	// figures), read from the WORKSPACE's own daemon sink.
	// The resolver files through the WORKSPACE-BOUND logger (its mutate
	// helper is keyed by workspace), so the record lands on the workspace's
	// own daemon sink, never on the restart-scoped daemon.run.log.
	w.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "daemon"), "the footer resolver to file an account_usage session update", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.footer.on_session_update" && r.Context["arm"] == "account_usage"
	})

	// Assert the FRONTEND consequence, which for these figures is a SPECIFIC
	// NEGATIVE rather than a drawn line — the one account arm frontend/v1
	// names at all.
	//
	// footer.proto:721-730's FooterStatusActivityRateLimited is "the
	// rate-limit rung… both figures are shown so a reader can tell WHICH
	// allowance the NEWSWORTHY percentage belongs to": the line's whole
	// premise is that something is newsworthy. The resolver enforces exactly
	// that ("an unremarkable allowance is not news and would crowd out the
	// lines that are" — daemon/internal/resolve/footer/activity.go rateLine),
	// and `usage-full`'s five_hour window is 41%
	// (agent-shim/claude/shim/src/fake/catalogs.ts fakeAccountUsage), well
	// under DefaultRateLimitNewsworthyThreshold. So a filed-but-unremarkable
	// sample must draw NO rate-limited activity line, which is a stronger
	// statement than the log record alone: it says the sample landed AND the
	// newsworthiness gate held.
	assertNoRateLimitedLine(t, w, ws)
}

// TestAccountUsageAvailableArm drives `!usage-available` — the OTHER
// registered name for the same available arm ("two names for one arm is
// deliberate: `!usage-available` names the ARM and `!usage-full` names what a
// reader wants from it", session.ts) — so the arm's own spelling is exercised
// end to end rather than only its alias, and asserts the same pair of facts.
func TestAccountUsageAvailableArm(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-available")

	// Assert
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("usage-available turn ended = %v, want a concluded (success.completed) outcome", row.GetTurnEnded())
	}
	w.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "daemon"), "the footer resolver to file an account_usage session update", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.footer.on_session_update" && r.Context["arm"] == "account_usage"
	})
	assertNoRateLimitedLine(t, w, ws)
}

// assertNoRateLimitedLine fails if the footer draws a rate-limited activity
// line. It opens a FRESH footer stream and reads its FIRST view: a newly
// opened stream is served the CURRENT resolved state, so — called after the
// resolver's own account_usage log record has been observed — the view it
// answers necessarily already carries the filed sample. No sleep, and no
// waiting for the absence of something.
func assertNoRateLimitedLine(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	footer := w.WatchFooter(ws)
	defer footer.Close()
	view := harness.AwaitView(t, w.Ctx(), footer.Stream, "the footer's current view after the usage sample was filed", func(*frontendv1.FooterView) bool {
		return true
	})
	status := view.GetStrip().GetStatus()
	for _, line := range []*frontendv1.FooterStatusActivityRateLimited{
		status.GetIdle().GetActivity().GetRateLimited(),
		status.GetWorking().GetActivity().GetRateLimited(),
		status.GetWaiting().GetActivity().GetRateLimited(),
	} {
		if line != nil {
			t.Errorf("footer draws a rate-limited activity line %v, want none at this sample's sub-threshold utilizations", line)
		}
	}
}

// TestContextUsage drives #59: `!context-usage-drift` pushes a GROWING
// conversation.v1 SessionUpdate.context_usage at every turn's end
// (session start and turn end, per shim.md — never on request), and the
// daemon's /context panel — which resolves purely from that already-pushed
// state, never from a pull of its own — is read after each of two turns to
// prove the second reading moved without the test ever calling a pull verb.
func TestContextUsage(t *testing.T) {
	t.Parallel()
	// Arrange: one world, one (fake-git) repository registered as a
	// workspace.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: drive the drift scenario once, then read the /context panel —
	// the daemon-resolved view of the SessionContextUsage state the shim's
	// FIRST push (session start / this turn's end) already landed.
	firstTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "context-usage-drift")
	firstRow := AwaitTurnEnded(t, w, ws, firstTurn)
	if firstRow.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("context-usage-drift turn 1 ended = %v, want a concluded (success.completed) outcome", firstRow.GetTurnEnded())
	}
	firstPanel := contextPanelOf(t, submitCommand(t, w, ws, "/context"))

	// Act: drive the SAME scenario a second time. Its own contract is that
	// the sampled total GROWS from the turn it runs on — the engine pushes
	// context_usage at every turn end regardless of scenario, so the second
	// push lands without the test asking for it.
	secondTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "context-usage-drift")
	secondRow := AwaitTurnEnded(t, w, ws, secondTurn)
	if secondRow.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("context-usage-drift turn 2 ended = %v, want a concluded (success.completed) outcome", secondRow.GetTurnEnded())
	}
	secondPanel := contextPanelOf(t, submitCommand(t, w, ws, "/context"))

	// Assert: both readings resolve a populated panel — the panel is
	// ALWAYS POPULATED once a session has started (contextpanel.go), never
	// a round trip to the vendor.
	if firstPanel.GetHeader() == nil || firstPanel.GetHeader().GetUsed() == "" || firstPanel.GetHeader().GetTotal() == "" || firstPanel.GetHeader().GetModel() == "" {
		t.Fatalf("the /context panel after turn 1 = %v, want a composed header", firstPanel)
	}
	if len(firstPanel.GetSections()) == 0 {
		t.Fatalf("the /context panel after turn 1 = %v, want at least one section", firstPanel)
	}
	if secondPanel.GetHeader() == nil || secondPanel.GetHeader().GetUsed() == "" || secondPanel.GetHeader().GetTotal() == "" || secondPanel.GetHeader().GetModel() == "" {
		t.Fatalf("the /context panel after turn 2 = %v, want a composed header", secondPanel)
	}

	// Assert: the second reading differs from the first — the daemon's
	// already-held state MOVED between the two /context reads purely
	// because the shim pushed again at the second turn's end, never
	// because /context itself asked the vendor anything (it is a read of
	// state the daemon already has; contextpanel.go's own doc comment).
	if secondPanel.GetHeader().GetUsed() == firstPanel.GetHeader().GetUsed() && secondPanel.GetHeader().GetPercent() == firstPanel.GetHeader().GetPercent() {
		t.Fatalf("the /context panel header occupancy did not move between two context-usage-drift turns: used %q/percent %d then used %q/percent %d, want the drift scenario's growing occupancy to show up as a second, different PUSH",
			firstPanel.GetHeader().GetUsed(), firstPanel.GetHeader().GetPercent(),
			secondPanel.GetHeader().GetUsed(), secondPanel.GetHeader().GetPercent())
	}
}

// contextPanelOf extracts the /context command's answer, failing the test if
// the command was not recognized as the /context command panel.
func contextPanelOf(t *testing.T, success *agentreplv1.SubmitPromptSuccess) *frontendv1.ContextPanelView {
	t.Helper()
	panel := success.GetCommandPanel().GetContext()
	if panel == nil {
		t.Fatalf("SubmitPrompt(/context) = %v, want a command_panel.context", success)
	}
	return panel
}
