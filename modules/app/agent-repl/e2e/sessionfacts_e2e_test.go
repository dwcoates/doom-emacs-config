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
//   - proto/src/frontend/v1/footer.proto — the ENDURING usage line
//     (FooterActivityEnduringUsage, always drawn when nothing covers it)
//     with its FooterAllowance cells (`newsworthy`,
//     `resets_at_s`, `utilization`, the typed verdict), and the SALIENT
//     vendor rate-limit line (FooterStatusActivityRateLimit: a warning or a
//     refusal, standing until a later event reports the allowance allowed).
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
//  1. FAST MODE NOW HAS A FRONTEND SURFACE (Landing 13). It used to have
//     none — `frontend/v1` carried no fast-mode field anywhere and every
//     resolver's SessionUpdate_FastMode branch was empty — which is what
//     made these tests weak BY CONTRACT rather than by neglect.
//     `TopbarView.fast_mode` (frontend/v1/topbar.proto, TopbarFastMode)
//     carries the state BY NAME and the topbar resolver fills it, so the
//     assertions below are on the DRAWN ARM and no longer on a daemon log
//     record.
//
//  2. A VENDOR RATE-LIMIT EVENT THAT WARNS OR REFUSES IS SALIENT (owner
//     ruling, 2026-09-30): the vendor speaking stands its own line on every
//     status arm, whatever the figure, until a later event reports that
//     allowance allowed. `!rate-limit-five-hour`, `!rate-limit-seven-day` and
//     `!rate-limit` each send an `allowed_warning`, so each stands the line
//     naming its allowance.
//
//  3. AN UNREAD SAMPLE DRAWS NO CAVEAT (owner ruling, fc4917be4,
//     2026-09-15). An unread sample leaves the read figures standing, and
//     its outcome — its reason, and a sampling
//     failure's cause — is recorded on the daemon's `daemon.footer.usage_
//     sample_unreadable` breadcrumb instead. The enduring usage line is drawn
//     whatever the figures (owner ruling, 2026-09-28), so the tests below
//     read the sampled figures straight off it; they never open a rate-limit
//     line first, because a standing warning would cover the enduring line.
//
// Every scenario here runs against the scripted fake git (harness.NewRepo)
// and the fake-SDK vendor inside the real shim: no real git, no vendor
// binary, no network. Nothing in this file writes a store row or a vendor
// JSONL of its own.
package e2e

import (
	"context"
	"strings"
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

// ===========================================================================
// Fast mode — `!fast-off` and `!fast-cooldown` (session.ts fastModeScenario).
//
// SessionFastMode's arms are on / off (carrying the vendor's reason) /
// cooldown, and session.proto states why cooldown is its own arm rather than
// off: "NOT the same as off: nothing needs doing and offering the user a way
// to turn it on would offer something that cannot take effect."
//
// LANDING 13 STRENGTHENED THIS TEST (see the file header's dispute 1). The
// assertion is now the DRAWN ARM on TopbarView.fast_mode, not a daemon log
// record: each state reaches the strip under its own name, and `cooldown`
// specifically is asserted NOT to arrive as `off` — the whole reason the
// contract keeps it a separate arm. The scenario's exact conclusion prose is
// still pinned, because it is what says which state the vendor reported.
//
// The topbar is the one resolver the arm reaches: sessionwatcher/route.go
// routes fast_mode to the topbar alone, so the footer never takes it.
// ===========================================================================

// sfFastModeArm names the topbar's drawn fast-mode state, or "" when the view
// carries none. The NAME is what the test asserts on, so an arm the contract
// grows later fails loudly here instead of being read as one of these.
func sfFastModeArm(v *frontendv1.TopbarView) string {
	switch v.GetFastMode().GetState().(type) {
	case *frontendv1.TopbarFastMode_On:
		return "on"
	case *frontendv1.TopbarFastMode_Off:
		return "off"
	case *frontendv1.TopbarFastMode_Cooldown:
		return "cooldown"
	default:
		return ""
	}
}

// sfAwaitFastMode waits for the topbar to draw the named fast-mode arm and
// answers the view that did. A FRESH stream is served the daemon's current
// resolved state and then every later push, so this waits for an event and
// never for a bound.
func sfAwaitFastMode(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, want string) *frontendv1.TopbarView {
	t.Helper()
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()
	return harness.AwaitView(t, w.Ctx(), topbar, "the topbar to draw fast mode "+want,
		func(v *frontendv1.TopbarView) bool { return sfFastModeArm(v) == want })
}

func TestFastModeOffAndCooldownStatesReachTheStrip(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

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
			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert: the vendor said which state it is, and the strip draws
			// that state under its own name.
			sfAwaitConclusion(t, w, ws, turn, tc.conclusion)
			view := sfAwaitFastMode(t, w, ws, tc.name)
			if got := sfFastModeArm(view); got != tc.name {
				t.Fatalf("TopbarView.fast_mode arm = %q, want %q", got, tc.name)
			}
		})
	}
}

// COOLDOWN IS NOT OFF, asserted as a specific negative on the drawn shape.
// session.proto states the reason the arms are separate — "NOT the same as
// off: nothing needs doing and offering the user a way to turn it on would
// offer something that cannot take effect" — and a resolver that folded the
// two would pass every assertion above but fail this one.
func TestFastModeCooldownIsNotDrawnAsOff(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fast-cooldown")
	sfAwaitConclusion(t, w, ws, turn, "Fast mode is cooldown.")

	// Assert
	view := sfAwaitFastMode(t, w, ws, "cooldown")
	if _, off := view.GetFastMode().GetState().(*frontendv1.TopbarFastMode_Off); off {
		t.Fatal("the strip drew cooldown as off, which offers a switch that cannot take effect")
	}
}

// THE OFF ARM CARRIES THE VENDOR'S REASON VERBATIM. `!fast-off` reports
// `fast_mode_disabled_reason: "preference"` (session.ts fastModeScenario), and
// the contract keeps the string rather than a class, so the strip can say why.
func TestFastModeOffCarriesTheVendorsReason(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fast-off")
	sfAwaitConclusion(t, w, ws, turn, "Fast mode is off.")

	// Assert
	view := sfAwaitFastMode(t, w, ws, "off")
	off, ok := view.GetFastMode().GetState().(*frontendv1.TopbarFastMode_Off)
	if !ok {
		t.Fatalf("TopbarView.fast_mode = %v, want the off arm", view.GetFastMode())
	}
	if got := off.Off.GetReason(); got != "preference" {
		t.Fatalf("TopbarFastModeOff.reason = %q, want the vendor's own %q", got, "preference")
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
// THE VENDOR SPEAKING IS SALIENT (owner ruling, 2026-09-30): the event stands
// the rate-limit line naming the SESSION allowance, its `allowed_warning`
// verdict, and the daemon's percent→fraction and milliseconds→seconds
// conversions (footer salient.go observeRateLimitEvent). It stands until a
// later event reports the allowance allowed, whatever samples arrive between.
// ===========================================================================

func TestRateLimitFiveHourWindowStandsTheSessionRateLimitLine(t *testing.T) {
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
	view := harness.AwaitView(t, ctx, footer.Stream, "the session allowance's rate-limit line", func(v *frontendv1.FooterView) bool {
		return footerRateLimit(v).GetWindow().GetSession() != nil
	})

	// Assert
	line := footerRateLimit(view)
	if got := line.GetUtilization(); got < 0.815 || got > 0.825 {
		t.Errorf("FooterStatusActivityRateLimit.utilization = %v, want the event's 0.82 as a 0..1 fraction", got)
	}
	if line.GetAllowedWarning() == nil {
		t.Errorf("FooterStatusActivityRateLimit.verdict = %v, want the allowed_warning arm the event carried", line.GetVerdict())
	}
	if line.GetResetsAtS() == 0 {
		t.Errorf("FooterStatusActivityRateLimit.resets_at_s = 0, want the event's reset instant in epoch SECONDS")
	}
}

// ===========================================================================
// The seven-day rate-limit window — `!rate-limit-seven-day` (the same
// scenario factory naming the `seven_day` window at utilization 0.91).
//
// The event stands the rate-limit line naming the WEEKLY allowance. Both hops
// are asserted — the drawn line and the session-arm record behind it —
// because the line alone would not say the event's own window reached the
// store.
// ===========================================================================

func TestRateLimitSevenDayWindowStandsTheWeeklyRateLimitLine(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, workspaceDir := sfNewWorkspace(t)
	before := sfSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "rate_limit_status")
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "rate-limit-seven-day")
	sfAwaitConclusion(t, w, ws, turn, "The seven_day window is 91% used.")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the weekly allowance's rate-limit line", func(v *frontendv1.FooterView) bool {
		return footerRateLimit(v).GetWindow().GetWeekly() != nil
	})

	// Assert
	line := footerRateLimit(view)
	if got := line.GetUtilization(); got < 0.905 || got > 0.915 {
		t.Errorf("FooterStatusActivityRateLimit.utilization = %v, want the event's 0.91 as a 0..1 fraction", got)
	}
	if line.GetAllowedWarning() == nil {
		t.Errorf("FooterStatusActivityRateLimit.verdict = %v, want the allowed_warning arm the event carried", line.GetVerdict())
	}
	if line.GetResetsAtS() == 0 {
		t.Errorf("FooterStatusActivityRateLimit.resets_at_s = 0, want the event's reset instant in epoch SECONDS")
	}
	sfAwaitSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "rate_limit_status", before+1)
}

// ===========================================================================
// The overage window — `!rate-limit` (session.ts RATE_LIMIT: an
// `allowed_warning` event on the `overage` window at utilization 0.79 with a
// surpassed threshold).
//
// THE VENDOR'S WARNING STANDS WHATEVER THE FIGURE (owner ruling, 2026-09-30):
// the old newsworthiness gate decided whether a rate line was drawn at all;
// now the vendor speaking is the salient line, and 0.79 is the figure it
// carries. The harness's warning sweep — which fails on any warn record this
// test does not declare — still asserts the retired
// `daemon.footer.rate_limit_overage` warning stays gone.
// ===========================================================================

func TestRateLimitOverageWindowStandsTheOverageRateLimitLine(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "rate-limit")
	sfAwaitConclusion(t, w, ws, turn, "The account is approaching its overage threshold.")

	// Assert
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the overage allowance's rate-limit line", func(v *frontendv1.FooterView) bool {
		return footerRateLimit(v).GetWindow().GetOverage() != nil
	})
	if got := footerRateLimit(view).GetUtilization(); got < 0.785 || got > 0.795 {
		t.Errorf("FooterStatusActivityRateLimit.utilization = %v, want the event's 0.79 as a 0..1 fraction", got)
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
// distinct vendor shape per arm for exactly that reason.
//
// WHAT THE STRIP OWES AN UNREAD SAMPLE, per the owner ruling of 2026-09-15
// (fc4917be4, "drop the usage-unread line"): NOTHING DRAWN. The figures LAST
// READ stand, the age they are drawn with stays the age of that reading, and
// the unread's reason is named on the daemon's own breadcrumb rather than on
// the strip. These tests assert all three through the real stack.
// ===========================================================================

// The five-hour utilization catalogs.ts's available shape files, as the
// footer draws it (percent on the wire, fraction on the contract).
const sfFiveHourUtilization = 0.41

// sfUnreadableOperation is the daemon's breadcrumb for a sample that read no
// figure (daemon/internal/resolve/footer/resolver.go logUnreadableSample).
const sfUnreadableOperation = "daemon.footer.usage_sample_unreadable"

// sfAwaitUnreadable waits for the workspace's daemon sink to record an unread
// sample under `reason`, and answers the record.
func sfAwaitUnreadable(t *testing.T, w *World, workspaceDir, reason string) harness.LogRecord {
	t.Helper()
	return w.Daemon.AwaitWorkspaceLogRecord(workspaceDir, "the "+reason+" unread sample's breadcrumb",
		func(r harness.LogRecord) bool {
			return r.Operation == sfUnreadableOperation && r.Context["reason"] == reason
		})
}

// sfAwaitUsage waits for the enduring usage line to draw the sampled
// five-hour figure, and answers the line. The enduring line is drawn whatever
// the figures (owner ruling, 2026-09-28), and nothing here stands a salient
// line over it.
func sfAwaitUsage(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) *frontendv1.FooterActivityEnduringUsage {
	t.Helper()
	footer := w.WatchFooter(ws)
	defer footer.Close()
	view := harness.AwaitView(t, w.Ctx(), footer.Stream, "the enduring usage line with the sampled figures",
		func(v *frontendv1.FooterView) bool {
			return footerEnduringUsage(v).GetSession().GetUtilization() == sfFiveHourUtilization
		})
	return footerEnduringUsage(view)
}

// sfSwitchedToUnread waits for a driven unread-arm scenario's conclusion and
// for the daemon to have taken the reprobe that read nothing. The caller
// drives the scenario itself, with the name as a literal, so the scenario
// matrix check can read which scenario this file drives.
func sfSwitchedToUnread(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId, reason string) {
	t.Helper()
	sfAwaitConclusion(t, w, ws, turn, "The account-usage probe now answers with the "+reason+" shape.")
	sfAwaitUnreadable(t, w, ws.GetDir(), reason)
}

// sfAssertNoCaveat fails if the line carries an account-usage outcome: the
// ruling retired the unread caveat, and a daemon that still filled it would
// draw a warning the owner removed.
func sfAssertNoCaveat(t *testing.T, line *frontendv1.FooterActivityEnduringUsage) {
	t.Helper()
	if line.GetSample() != nil {
		t.Errorf("FooterActivityEnduringUsage.sample = %v, want UNSET: an unread sample draws no caveat (owner ruling, fc4917be4)", line.GetSample())
	}
}

func TestAccountUsageUnreadArmsLeaveTheReadFiguresStanding(t *testing.T) {
	t.Parallel()
	cases := []struct {
		reason   string
		scenario string
	}{
		{reason: "service_unavailable", scenario: "usage-service-unavailable"},
		{reason: "window_unavailable", scenario: "usage-window-unavailable"},
		{reason: "utilization_unavailable", scenario: "usage-utilization-unavailable"},
		{reason: "sampling_failure", scenario: "usage-sampling-failure"},
	}
	for _, tc := range cases {
		t.Run(tc.reason, func(t *testing.T) {
			t.Parallel()
			// Arrange: the session's start-time probe READ the available
			// shape, so the figures standing are figures actually read.
			w, ws, _ := sfNewWorkspace(t)

			// Act
			sfSwitchedToUnread(t, w, ws, driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario), tc.reason)
			line := sfAwaitUsage(t, w, ws)

			// Assert: ALONGSIDE, never instead of. The five-hour figure the
			// last readable sample filed is still drawn, and no caveat is
			// drawn for the unread.
			if got := line.GetSession().GetUtilization(); got != sfFiveHourUtilization {
				t.Errorf("FooterAllowance(session).utilization = %v, want the standing %v left alone by the %s sample",
					got, sfFiveHourUtilization, tc.reason)
			}
			sfAssertNoCaveat(t, line)
		})
	}
}

// A SAMPLED WINDOW RESETS IN THE FUTURE, so the drawn countdown runs.
//
// THE DEFECT THIS PINS: catalogs.ts fakeAccountUsage used to state its reset
// instants absolutely (2026-08-29T20:00Z / 2026-09-02T00:00Z). Once those fell
// into the past every countdown the footer drew off a SAMPLE read `resets in
// 0m`, while one drawn off a rate-limit EVENT (minted at `now + 3600s`)
// counted down properly — two sources of the same cell disagreeing, for no
// reason the vendor has. The fixture now states both windows as offsets from
// the fake's own clock, and this asserts the ordering the countdown needs:
// the drawn reset is AFTER the moment the footer was read.
//
// THE SESSION CELL IS THE SAMPLED ONE: 41%, filed by the start-time probe.
// The unread arm ahead keeps the turn-close reprobes from re-filing it.
func TestSampledAllowanceResetsAfterItWasRead(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)
	sfSwitchedToUnread(t, w, ws, driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-service-unavailable"), "service_unavailable")

	// Act
	line := sfAwaitUsage(t, w, ws)
	readAt := time.Now().Unix()

	// Assert
	if got := line.GetSession().GetUtilization(); got != sfFiveHourUtilization {
		t.Fatalf("FooterAllowance(session).utilization = %v, want the sampled %v: this cell must be the sample's", got, sfFiveHourUtilization)
	}
	if got := line.GetSession().GetResetsAtS(); got <= readAt {
		t.Errorf("FooterAllowance(session).resets_at_s = %d, want an instant after the read at %d: a sampled window must reset in the FUTURE, never at `resets in 0m`",
			got, readAt)
	}
}

// THE SAMPLE THE SESSION PROBES AT ITS OWN START REACHES THE FOOTER, and it
// did not before. The shim probes the account's usage once inside StartSession
// (engine/session.ts, the `void pushAccountUsage()` beside the keepalive
// cadence), while the daemon opens its standing WatchSession only AFTER
// StartSession has answered — so with `accountUsage` missing from
// SessionPushes.REPLAYED that first sample was fanned out to a stream nobody
// was reading and the footer held no allowance figure at all until a turn
// closed and reprobed.
//
// MEASURED in a sandbox run before the fix: the sample went out at 37.494 to
// the bring-up's stream, the daemon's own watch opened at 37.503, and the
// footer's first sighting of any account usage was the turn-close reprobe
// 41ms later.
//
// THE TURN HERE READS NOTHING on purpose: the probe is switched to an unread
// arm by it, so its turn-close reprobe files no figure, and the session's 41%
// on the enduring line cannot have come from anything but the start-time
// probe.
func TestAccountUsageProbedAtSessionStartReachesTheFooter(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act: the workspace's very first turn, and it reads nothing.
	sfSwitchedToUnread(t, w, ws, driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-service-unavailable"), "service_unavailable")
	line := sfAwaitUsage(t, w, ws)

	// Assert
	if got := line.GetSession().GetUtilization(); got != sfFiveHourUtilization {
		t.Errorf("FooterAllowance(session).utilization = %v, want %v from the session's own start-time probe",
			got, sfFiveHourUtilization)
	}
}

// THE SAMPLING FAILURE KEEPS THE SHIM'S OWN CAUSE. The arm exists so a reader
// learns WHY the shim could not sample. The strip no longer draws the unread
// (owner ruling, fc4917be4), so the daemon's breadcrumb is where the cause
// lands, and a cause dropped there would leave the record saying only
// "something".
func TestAccountUsageSamplingFailureCarriesACause(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-sampling-failure")
	sfAwaitConclusion(t, w, ws, turn,
		"The account-usage probe now answers with the sampling_failure shape.")

	// Assert
	record := sfAwaitUnreadable(t, w, ws.GetDir(), "sampling_failure")
	if cause, _ := record.Context["cause"].(string); !strings.Contains(cause, "transcript scan") {
		t.Errorf("the sampling failure's breadcrumb cause = %q, want the shim's own account of what failed (catalogs.ts: the local transcript scan)", cause)
	}
}

// `!usage-opus-absent` IS NOT AN UNAVAILABILITY, and that is the whole
// scenario: the service answered in full and this account simply has no opus
// window (catalogs.ts: "An ABSENT OPTIONAL WINDOW, which is NOT an
// unavailability"). The enduring line draws no opus window, so what a reader
// sees is the session and weekly allowances the sample read, both drawn.
func TestAccountUsageOpusAbsentDrawsTheSessionAndWeeklyAllowances(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-opus-absent")
	sfAwaitConclusion(t, w, ws, turn,
		"The account-usage probe now answers with the opus_absent shape.")

	// Assert
	line := sfAwaitUsage(t, w, ws)
	if line.GetWeekly() == nil {
		t.Errorf("FooterActivityEnduringUsage.weekly is UNSET, want the weekly allowance an absent opus window leaves drawn")
	}
}
