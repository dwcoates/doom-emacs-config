// sessionfacts_e2e_test.go — the SESSION-SCOPED facts that ride
// conversation.v1 SessionUpdate and have no turn or unit of their own: fast
// mode, the MCP catalog narrowing, the vendor's rate-limit windows, and the
// account-usage outcome arms.
//
// CONTRACT GROUNDING (read in this worktree):
//
//   - proto/src/conversation/v1/session.proto — SessionUpdate's arms
//     `fast_mode` (SessionFastMode, "THE ARM IS THE STATE; off carries the
//     vendor's reason"), `mcp_server` (SessionMcpServer, "THE ARM IS THE
//     HEALTH"), `account_usage` (SessionAccountUsage, "THE ARM IS WHETHER A
//     FIGURE WAS READ", with SessionAccountUsageUnavailable's four reason
//     arms), and `rate_limit_status` (SessionRateLimitStatus, "a STREAM-ONLY
//     event... the footer's allowance cell draws it").
//   - proto/src/frontend/v1/footer.proto — FooterStatusActivityRateLimited
//     ("The vendor bills TWO independent allowances (rolling five-hour
//     session, seven-day weekly) reported through one event") and
//     FooterAllowance (`newsworthy`, `resets_at_s`, `utilization`, and the
//     typed status oneof allowed/allowed_warning/rejected whose comment says
//     "UNSET IS LEGAL: no rate-limit event has been observed for the window
//     yet").
//   - proto/src/frontend/v1/mcp_panel.proto — McpPanelView/McpPanelRow, the
//     surface the MCP healths are drawn on (driven here through /mcp, the
//     same synchronous command mcpmonitors_e2e_test.go reads).
//   - agent-shim/claude/shim/src/fake/scenarios/session.ts and
//     src/fake/catalogs.ts — the scenarios driven here and their exact
//     figures.
//
// THREE DISPUTES ARE RECORDED HERE RATHER THAN PAPERED OVER. Each is stated
// in full at the test it constrains; in summary:
//
//  1. FAST MODE HAS NO FRONTEND SURFACE. `frontend/v1` carries no fast-mode
//     field anywhere (grep for "fast" over proto/src/frontend/v1 answers
//     nothing), and every daemon resolver that switches on
//     SessionUpdate_FastMode does so with an EMPTY branch
//     (daemon/internal/resolve/{footer,topbar,sidebar}/resolver.go,
//     internal/resolve/feed/sink.go). So the strongest assertion available
//     is the scenario's own exact prose plus the daemon's structured record
//     that it took the typed arm — the same wait signal
//     accounting_e2e_test.go uses for account usage.
//
//  2. `!rate-limit-seven-day` CANNOT DRAW ITS ALLOWANCE. The scenario's
//     utilization is 0.61 (session.ts RATE_LIMIT_SEVEN_DAY) and the footer
//     only draws the rate line when an allowance reaches
//     DefaultRateLimitNewsworthyThreshold = 0.8
//     (daemon/internal/resolve/footer/api.go; activity.go's rateLine: "at
//     least one allowance is newsworthy: an unremarkable allowance is not
//     news"). `!rate-limit-five-hour` (0.82) is above it and IS asserted on
//     the drawn FooterAllowance below. A fake whose seven-day figure were
//     >= 0.8 would make the weekly cell assertable the same way; changing
//     the mock is outside this file's scope.
//
//  3. THE ACCOUNT-USAGE OUTCOME ARMS REACH NO DRAWN SHAPE. The only
//     consumer of SessionAccountUsage is the footer's `observeAccountUsage`,
//     which files five_hour/seven_day FIGURES and returns early for every
//     unavailable arm ("A sample that could read no figure... leaves the
//     figures on hand standing"). The fake's figures (five_hour 41,
//     seven_day 63 — catalogs.ts fakeAccountUsage) are below the same 0.8
//     newsworthiness threshold, so neither the available nor the
//     unavailable arms move any pixel: opus_absent draws exactly what
//     available draws, and the four unavailable reasons draw exactly what
//     the previous sample drew. What IS observable — and asserted — is the
//     scenario's own prose and a FRESH account_usage arm reaching the
//     daemon after the scenario switched it (the shim reprobes account
//     usage at every turn close: agent-shim/claude/shim/src/engine/
//     session.ts `reprobeSessionFacts`, "A TURN CAN CHANGE WHAT THE PROBES
//     ANSWER").
//
// Every scenario here runs against the scripted fake git (harness.NewRepo)
// and the fake-SDK vendor inside the real shim: no real git, no vendor
// binary, no network. Nothing in this file writes a store row or a vendor
// JSONL of its own.
package e2e

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
)

// sfNewWorkspace builds one World and registers a fresh scripted-fake-git
// repository as its workspace.
func sfNewWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef, string) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, ws, repo.Dir
}

// sfAwaitConclusion waits for the turn's settled response bubble and asserts
// its markdown is EXACTLY want — the one fact that tells the scenarios of a
// family apart when none of them reaches a drawn shape.
func sfAwaitConclusion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId, want string) {
	t.Helper()
	awaitFeedRow(t, w, ws, "the scenario's settled response bubble ("+want+")", func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() &&
			row.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == want
	})
}

// The two daemon records that name a SessionUpdate arm, one per resolver that
// takes session facts.
const (
	sfFooterSessionUpdate = "daemon.footer.on_session_update"
	sfTopbarSessionUpdate = "daemon.topbar.on_session_update"
)

// sfSessionArmRecords counts one resolver's own records for one SessionUpdate
// arm on the workspace's daemon sink. Both the footer and the topbar log every
// arm they take, including the ones they deliberately draw nothing from
// (each resolver.go's sessionArm: "Every arm has a branch"), which is what
// makes this countable.
//
// WHICH RESOLVER IS NOT A CHOICE. daemon/internal/sessionwatcher/route.go
// routes each arm to the views that resolve from it, and the split is stated
// there once: "session identity and health are the topbar's, accounting and
// the rate-limit status are the footer's". So an arm's records stand on
// exactly one of the two operations, and a test that greps the other one
// would wait forever on a hop that never happens.
func sfSessionArmRecords(t *testing.T, w *World, workspaceDir, operation, arm string) int {
	t.Helper()
	n := 0
	for _, r := range w.WorkspaceLog(workspaceDir, "daemon") {
		if r.Operation == operation && r.Context["arm"] == arm {
			n++
		}
	}
	return n
}

// sfAwaitSessionArmRecords waits until at least want records for the arm
// stand on the workspace's daemon sink.
func sfAwaitSessionArmRecords(t *testing.T, w *World, workspaceDir, operation, arm string, want int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if got := sfSessionArmRecords(t, w, workspaceDir, operation, arm); got >= want {
			return
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("e2e: waiting for %d %q session-update records under %q on the workspace's daemon sink, saw %d",
				want, arm, operation, sfSessionArmRecords(t, w, workspaceDir, operation, arm))
			return
		}
	}
}

// sfRateLimited answers the standing FooterStatusActivityRateLimited under
// whichever status arm is set, or nil when the standing activity is a
// different kind (or none). The kind is STATUS-INDEPENDENT — footer.proto:
// "THREE ACTIVITY KINDS ARE STATUS-INDEPENDENT and appear in EVERY status
// arm's oneof (notification, rate_limited, context_budget)" — so a reader
// that looked under one status only would miss it whenever the daemon's
// status moved on.
func sfRateLimited(v *frontendv1.FooterView) *frontendv1.FooterStatusActivityRateLimited {
	status := v.GetStrip().GetStatus()
	switch {
	case status.GetIdle() != nil:
		return status.GetIdle().GetActivity().GetRateLimited()
	case status.GetThinking() != nil:
		return status.GetThinking().GetActivity().GetRateLimited()
	case status.GetWaiting() != nil:
		return status.GetWaiting().GetActivity().GetRateLimited()
	case status.GetInterrupted() != nil:
		return status.GetInterrupted().GetActivity().GetRateLimited()
	case status.GetMerging() != nil:
		return status.GetMerging().GetActivity().GetRateLimited()
	case status.GetBackground() != nil:
		return status.GetBackground().GetActivity().GetRateLimited()
	case status.GetBlocked() != nil:
		return status.GetBlocked().GetActivity().GetRateLimited()
	case status.GetDisconnected() != nil:
		return status.GetDisconnected().GetActivity().GetRateLimited()
	case status.GetClosing() != nil:
		return status.GetClosing().GetActivity().GetRateLimited()
	case status.GetLoading() != nil:
		return status.GetLoading().GetActivity().GetRateLimited()
	default:
		return nil
	}
}

// ===========================================================================
// Fast mode — `!fast-off` and `!fast-cooldown` (session.ts fastModeScenario).
//
// SessionFastMode's arms are on / off (carrying the vendor's reason) /
// cooldown, and session.proto states why cooldown is its own arm rather than
// off: "NOT the same as off: nothing needs doing and offering the user a way
// to turn it on would offer something that cannot take effect."
//
// DISPUTE 1 (see the file header): no frontend surface exists for any of the
// three arms, so the drawn assertion this test would otherwise make cannot
// be written against the frozen contract. Asserted instead: the scenario's
// exact conclusion prose (which of the two states ran), and a FRESH
// fast_mode arm reaching the daemon's TOPBAR resolver for this workspace —
// proof the typed SessionFastMode crossed the shim's converter and the
// daemon's session-stream boundary, which is every hop that exists.
//
// The topbar is the one resolver the arm reaches: sessionwatcher/route.go
// routes fast_mode to the topbar alone, so the footer never takes it.
// ===========================================================================

func TestFastModeOffAndCooldownStatesReachTheDaemon(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, workspaceDir := sfNewWorkspace(t)

	cases := []struct {
		name       string
		scenario   string
		conclusion string
	}{
		{name: "off", scenario: "fast-off", conclusion: "Fast mode is off."},
		{name: "cooldown", scenario: "fast-cooldown", conclusion: "Fast mode is cooldown."},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: what the daemon had already taken before this
			// scenario ran, so the wait below is for a NEW record.
			before := sfSessionArmRecords(t, w, workspaceDir, sfTopbarSessionUpdate, "fast_mode")

			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert
			sfAwaitConclusion(t, w, ws, turn, tc.conclusion)
			sfAwaitSessionArmRecords(t, w, workspaceDir, sfTopbarSessionUpdate, "fast_mode", before+1)
		})
	}
}

// ===========================================================================
// The MCP catalog narrowed to the healthy server — `!mcp-healthy`
// (session.ts MCP_HEALTHY, which switches mcpServerStatus() to
// catalogs.ts FAKE_MCP_SERVERS_HEALTHY: the `echo` server alone).
//
// The contract fact this pins is the one the narrowing exposes:
// daemon/internal/resolve/topbar/mcppanel.go's putMcpServer keeps rows
// KEYED BY NAME and only ever drops a row whose health arm is UNSET ("A
// health whose arm is UNSET says the server no longer stands... and the row
// is dropped rather than drawn with a badge the producer never chose"). A
// narrowed catalog states nothing at all about the servers it omits, so
// their rows STAND with the healths last stated — including `broken`'s
// verbatim "spawn ENOENT" — and `echo` is restated connected. A daemon that
// treated "absent from the newest catalog" as "gone" would fail this.
// ===========================================================================

func TestMcpCatalogNarrowedToHealthyKeepsTheOmittedRows(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Arrange: STATE the five-server catalog on the daemon first. The shim
	// probes mcpServerStatus() at StartSession too, but those start-time
	// pushes predate this test's daemon subscription, so the only catalog
	// this test can rely on the daemon having taken is one a turn CLOSE
	// pushed (engine/session.ts reprobeSessionFacts). `!mcp-all` is that
	// turn, and it is what makes the narrowing below a NARROWING rather than
	// the first catalog the daemon ever saw.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "mcp-all")
	awaitMcpPanel(t, w, ws, func(v *frontendv1.McpPanelView) bool {
		return v != nil && len(v.GetRows()) == 5
	})

	// Act: narrow the catalog to the connected server alone.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "mcp-healthy")
	panel := awaitMcpPanel(t, w, ws, func(v *frontendv1.McpPanelView) bool {
		return v != nil && len(v.GetRows()) == 5
	})

	// Assert: every row still stands, with its health unchanged.
	wantHealth := map[string]string{
		"echo":         "connected",
		"broken":       "failed",
		"needs-login":  "needs_auth",
		"slow":         "pending",
		"switched-off": "disabled",
	}
	for _, row := range panel.GetRows() {
		want, ok := wantHealth[row.GetName()]
		if !ok {
			t.Errorf("McpPanelView row for unexpected server %q: %v", row.GetName(), row)
			continue
		}
		if got := mcpRowHealth(row); got != want {
			t.Errorf("after !mcp-healthy, McpPanelRow %q health = %q, want %q — a narrowed catalog states nothing about the servers it omits",
				row.GetName(), got, want)
		}
		delete(wantHealth, row.GetName())
	}
	for name := range wantHealth {
		t.Errorf("after !mcp-healthy, the /mcp panel dropped the row for %q; only an UNSET health arm may drop a row (mcp_panel rows are keyed by name)", name)
	}
}

// ===========================================================================
// The five-hour rate-limit window — `!rate-limit-five-hour`
// (session.ts rateLimitWindowScenario: an `allowed_warning`
// rate_limit_event naming the `five_hour` window at utilization 0.82,
// resetting in an hour).
//
// This is the one rate-limit scenario whose figure clears the footer's
// newsworthiness gate, so it is asserted on the DRAWN shape: the standing
// FooterStatusActivityRateLimited's SESSION allowance, its typed
// `allowed_warning` status arm (copied arm-for-arm from
// SessionRateLimitStatus by daemon/internal/resolve/footer/activity.go's
// `allowance`, which never defaults an arm), and the daemon's own
// percent→fraction and seconds conversions
// (state.go observeFigures: "the contract carries a 0..1 fraction and epoch
// seconds, so the conversion is the daemon's and never the client's").
//
// The footer watch is opened BEFORE the prompt on purpose. The shim reprobes
// account usage at every turn CLOSE (engine/session.ts
// reprobeSessionFacts), and that sample's five-hour figure is 41% — below
// the newsworthiness gate — so the drawn line stands only between the
// rate-limit event and the turn's own close. publish.Topic's subscription
// guarantee makes that a certainty rather than a race: "Subscribe delivers
// the latest published value first... and then every later value in
// publication order, skipping none", with an unbounded per-subscriber queue.
// ===========================================================================

func TestRateLimitFiveHourWindowDrawsTheSessionAllowance(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "rate-limit-five-hour")
	sfAwaitConclusion(t, w, ws, turn, "The five_hour window is 82% used.")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the five-hour allowance's drawn rate-limit line", func(v *frontendv1.FooterView) bool {
		return sfRateLimited(v).GetSession() != nil
	})

	// Assert
	session := sfRateLimited(view).GetSession()
	if got := session.GetUtilization(); got < 0.815 || got > 0.825 {
		t.Errorf("FooterAllowance(session).utilization = %v, want the event's 0.82 as a 0..1 fraction", got)
	}
	if !session.GetNewsworthy() {
		t.Errorf("FooterAllowance(session).newsworthy = false at utilization %v, want true (the drawn line exists only because it is)", session.GetUtilization())
	}
	if session.GetAllowedWarning() == nil {
		t.Errorf("FooterAllowance(session).status = %v, want the allowed_warning arm the event carried", session.GetStatus())
	}
	if session.GetAllowed() != nil || session.GetRejected() != nil {
		t.Errorf("FooterAllowance(session) carries a second status arm: %v", session.GetStatus())
	}
	if session.GetResetsAtS() == 0 {
		t.Errorf("FooterAllowance(session).resets_at_s = 0, want the event's reset instant in epoch SECONDS")
	}
}

// ===========================================================================
// The seven-day rate-limit window — `!rate-limit-seven-day` (the same
// scenario factory naming the `seven_day` window at utilization 0.61).
//
// DISPUTE 2 (see the file header): 0.61 never reaches
// DefaultRateLimitNewsworthyThreshold (0.8), and the footer draws no rate
// line at all unless one allowance is newsworthy, so the WEEKLY allowance
// cell footer.proto declares cannot be reached from this mock. What is
// asserted here is the hop that exists: the event's window is filed by the
// footer resolver (observeRateLimitStatus maps seven_day and its per-model
// aliases onto the weekly allowance), evidenced by a fresh
// rate_limit_status arm, plus the scenario's own exact prose.
// ===========================================================================

func TestRateLimitSevenDayWindowIsFiled(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, workspaceDir := sfNewWorkspace(t)
	before := sfSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "rate_limit_status")

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "rate-limit-seven-day")

	// Assert
	sfAwaitConclusion(t, w, ws, turn, "The seven_day window is 61% used.")
	sfAwaitSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "rate_limit_status", before+1)
}

// ===========================================================================
// The overage window — `!rate-limit` (session.ts RATE_LIMIT: an
// `allowed_warning` event on the `overage` window at utilization 0.79 with a
// surpassed threshold).
//
// footer.proto gives the strip TWO allowance cells and no third:
// FooterStatusActivityRateLimited carries `session` and `weekly` only. The
// daemon therefore refuses to draw the overage window against a cell it is
// not about, and says so LOUDLY rather than silently
// (daemon/internal/resolve/footer/resolver.go observeRateLimitStatus: "The
// overage window has no cell in the contract, so it is logged and dropped
// rather than drawn against a window it is not about"). That warning is this
// scenario's whole observable contract, so it is declared to the harness's
// warning sweep and then asserted — a declared-and-unasserted warning would
// let the drop go silent.
// ===========================================================================

func TestRateLimitOverageWindowIsDroppedLoudly(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, workspaceDir := sfNewWorkspace(t)
	w.ExpectWarnings("daemon.footer.rate_limit_overage")

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "rate-limit")

	// Assert
	sfAwaitConclusion(t, w, ws, turn, "The account is approaching its overage threshold.")
	w.AwaitLogRecord(harness.WorkspaceLogPath(workspaceDir, "daemon"),
		"the footer resolver to drop the overage window loudly",
		func(r harness.LogRecord) bool {
			return r.Operation == "daemon.footer.rate_limit_overage" && r.Level == "warn"
		})

	// Assert: nothing was drawn for it. The overage figure (0.79) is below
	// the newsworthiness gate in any case, so this is the drop and the gate
	// agreeing — a daemon that filed overage onto the weekly cell would
	// still not draw at 0.79, which is why the log record above, not this
	// negative alone, is the load-bearing assertion.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footerOf(t, w, ws), "the footer after the overage event", func(v *frontendv1.FooterView) bool {
		return v.GetStrip() != nil
	})
	if line := sfRateLimited(view); line != nil {
		t.Errorf("the footer drew a rate-limit line for the overage window: %v", line)
	}
}

// footerOf opens a footer watch bound to the test's cleanup, for the reads
// that need one view rather than a wait across a scenario.
func footerOf(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) *harness.Stream[*frontendv1.FooterView] {
	t.Helper()
	footer := w.WatchFooter(ws)
	t.Cleanup(footer.Close)
	return footer.Stream
}

// ===========================================================================
// The account-usage outcome arms — `!usage-opus-absent` (available with an
// absent optional window) and the four unavailable reasons
// (`!usage-service-unavailable`, `!usage-window-unavailable`,
// `!usage-utilization-unavailable`, `!usage-sampling-failure`).
//
// session.proto's SessionAccountUsageUnavailable is a four-arm oneof and
// SessionAccountUsageAvailable's optional windows are "each UNSET when the
// account has no such allowance" — catalogs.ts fakeAccountUsage produces one
// distinct vendor shape per arm for exactly that reason ("A single canned
// answer could produce only the first arm, so the mock keeps all five and a
// scenario picks").
//
// DISPUTE 3 (see the file header): none of the five arms reaches a drawn
// shape. observeAccountUsage reads FIGURES only and returns early on the
// unavailable arm, and the fake's figures (41 / 63) are below the footer's
// newsworthiness gate, so every arm renders the identical (empty) footer.
// Asserted instead, per arm: the scenario's own exact prose, and a FRESH
// account_usage sample reaching the daemon after the arm was switched — the
// turn-close reprobe the shim's engine performs precisely because "A TURN
// CAN CHANGE WHAT THE PROBES ANSWER".
// ===========================================================================

func TestAccountUsageOutcomeArmsAreSampledAfterTheSwitch(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, workspaceDir := sfNewWorkspace(t)

	cases := []struct {
		name     string
		scenario string
	}{
		{name: "opus_absent", scenario: "usage-opus-absent"},
		{name: "service_unavailable", scenario: "usage-service-unavailable"},
		{name: "window_unavailable", scenario: "usage-window-unavailable"},
		{name: "utilization_unavailable", scenario: "usage-utilization-unavailable"},
		{name: "sampling_failure", scenario: "usage-sampling-failure"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			before := sfSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "account_usage")

			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert: the arm the scenario switched to, named in its own
			// conclusion (session.ts usageScenario composes it verbatim).
			sfAwaitConclusion(t, w, ws, turn,
				"The account-usage probe now answers with the "+tc.name+" shape.")
			sfAwaitSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "account_usage", before+1)
		})
	}
}
