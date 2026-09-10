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
//  1. FAST MODE NOW HAS A FRONTEND SURFACE (Landing 13). It used to have
//     none — `frontend/v1` carried no fast-mode field anywhere and every
//     resolver's SessionUpdate_FastMode branch was empty — which is what
//     made these tests weak BY CONTRACT rather than by neglect.
//     `TopbarView.fast_mode` (frontend/v1/topbar.proto, TopbarFastMode)
//     carries the state BY NAME and the topbar resolver fills it, so the
//     assertions below are on the DRAWN ARM and no longer on a daemon log
//     record.
//
//  2. `!rate-limit-seven-day` NOW DRAWS ITS ALLOWANCE. The scenario's
//     utilization was 0.61 while the footer only draws the rate line when
//     an allowance reaches DefaultRateLimitNewsworthyThreshold = 0.8
//     (daemon/internal/resolve/footer/api.go; activity.go's rateLine: "at
//     least one allowance is newsworthy: an unremarkable allowance is not
//     news"), which made the WEEKLY cell unreachable from this mock. The
//     mock's figure is now 0.91 (session.ts RATE_LIMIT_SEVEN_DAY) — no
//     capture in the corpus carries a seven-day window or a utilization of
//     any kind, so there was no real figure to prefer — and the weekly cell
//     is asserted on the drawn shape below.
//
//  3. THE ACCOUNT-USAGE OUTCOME ARMS NOW REACH A DRAWN SHAPE (Landing 13).
//     `observeAccountUsage` used to file five_hour/seven_day FIGURES and
//     return early for every unavailable arm, so no arm of that oneof moved
//     a pixel: opus_absent drew exactly what available drew, and the four
//     unavailable reasons drew exactly what the previous sample drew.
//     `FooterStatusActivityRateLimited.sample` (frontend/v1/footer.proto,
//     FooterAllowanceSample) now carries the outcome BY NAME beside the
//     figures, and the rate line's newsworthiness gate opens on an unread
//     sample as well as on a newsworthy allowance — otherwise the cell would
//     stay unreachable from this mock, whose figures (five_hour 41,
//     seven_day 63 — catalogs.ts fakeAccountUsage) sit under the 0.8
//     threshold. The daemon files a fresh sample at every turn close (the
//     shim reprobes: agent-shim/claude/shim/src/engine/session.ts
//     `reprobeSessionFacts`, "A TURN CAN CHANGE WHAT THE PROBES ANSWER").
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
// This is the one rate-limit scenario whose figure clears the footer's
// newsworthiness gate, so it is asserted on the DRAWN shape: the standing
// FooterStatusActivityRateLimited's SESSION allowance, its typed
// `allowed_warning` status arm (copied arm-for-arm from
// SessionRateLimitStatus by daemon/internal/resolve/footer/activity.go's
// `allowance`, which never defaults an arm), and the daemon's own
// percent→fraction and seconds conversions
// (state.go fileFigures: "the contract carries a 0..1 fraction and epoch
// seconds, so the conversion is the daemon's and never the client's").
//
// The footer watch is opened BEFORE the prompt on purpose. The shim reprobes
// account usage at every turn CLOSE (engine/session.ts
// reprobeSessionFacts), that sample's five-hour figure is 41% — below the
// newsworthiness gate — and the sample is the LAST figure sighting to reach
// the footer, so it overwrites the event's 0.82 and the drawn line retires
// with it (footer/state.go fileFigures: "THE LAST SIGHTING TO ARRIVE WINS",
// arrival being the only valid order between a shim-stamped sample and an
// event the contract gives no instant at all). MEASURED: the line is drawn
// and then gone again, both inside one turn. So the line stands only between
// the rate-limit event and the turn's own close, and publish.Topic's
// subscription guarantee is what makes catching it a certainty rather than a
// race: "Subscribe delivers the latest published value first... and then
// every later value in publication order, skipping none", with an unbounded
// per-subscriber queue.
//
// It did NOT always retire. While the footer ordered the two sightings by
// comparing the shim's `observed_at_ms` against the daemon's own clock, the
// reprobe's sample routinely lost to the event that preceded it and the
// retired 0.82 kept being drawn after the close — the defect this window is
// now free of.
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
// scenario factory naming the `seven_day` window at utilization 0.91).
//
// The figure clears the footer's newsworthiness gate (0.8), so this window
// is asserted on the DRAWN shape: footer.proto's WEEKLY allowance cell,
// which observeRateLimitStatus fills from seven_day and its per-model
// aliases. Both hops are asserted — the drawn cell and the session-arm
// record behind it — because the cell alone would not say the event's own
// window reached the store.
//
// The footer watch opens BEFORE the prompt for the same reason as the
// five-hour test: the turn's close reprobes account usage with figures below
// the gate (seven_day is 63%), that sample is the last figure sighting to
// arrive and so overwrites the event's 0.91, and the drawn line retires with
// it. It therefore stands only between the rate-limit event and that close,
// and publish.Topic's subscription guarantee makes catching it a certainty
// rather than a race.
// ===========================================================================

func TestRateLimitSevenDayWindowDrawsTheWeeklyAllowance(t *testing.T) {
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
	view := harness.AwaitView(t, ctx, footer.Stream, "the seven-day allowance's drawn rate-limit line", func(v *frontendv1.FooterView) bool {
		return sfRateLimited(v).GetWeekly() != nil
	})

	// Assert
	weekly := sfRateLimited(view).GetWeekly()
	if got := weekly.GetUtilization(); got < 0.905 || got > 0.915 {
		t.Errorf("FooterAllowance(weekly).utilization = %v, want the event's 0.91 as a 0..1 fraction", got)
	}
	if !weekly.GetNewsworthy() {
		t.Errorf("FooterAllowance(weekly).newsworthy = false at utilization %v, want true (the drawn line exists only because it is)", weekly.GetUtilization())
	}
	if weekly.GetAllowedWarning() == nil {
		t.Errorf("FooterAllowance(weekly).status = %v, want the allowed_warning arm the event carried", weekly.GetStatus())
	}
	if weekly.GetAllowed() != nil || weekly.GetRejected() != nil {
		t.Errorf("FooterAllowance(weekly) carries a second status arm: %v", weekly.GetStatus())
	}
	if weekly.GetResetsAtS() == 0 {
		t.Errorf("FooterAllowance(weekly).resets_at_s = 0, want the event's reset instant in epoch SECONDS")
	}
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
// LANDING 13 STRENGTHENED THESE TESTS (see the file header's dispute 3). Each
// arm now reaches FooterStatusActivityRateLimited.sample by name, and the
// assertions below are on that drawn arm — plus, for every unavailable one,
// the standing contract that a failed read LEAVES THE FIGURES ON HAND
// STANDING ("A sample that could read no figure... leaves the figures on hand
// standing", daemon/internal/resolve/footer/resolver.go).
//
// THE FAKE'S FIGURES ARE THE PROOF OF THAT SECOND HALF: five_hour 41 and
// seven_day 63 (catalogs.ts fakeAccountUsage) are the figures an available
// sample files, and they are what must still be drawn after an unread.
// ===========================================================================

// The five-hour and seven-day utilizations catalogs.ts's available shape
// files, as the footer draws them (percent on the wire, fraction on the
// contract).
const (
	sfFiveHourUtilization = 0.41
	sfSevenDayUtilization = 0.63
)

// sfSampleArm names the footer's drawn account-usage outcome, or "" when the
// rate line carries none (including when no line is drawn at all).
func sfSampleArm(v *frontendv1.FooterView) string {
	switch sfRateLimited(v).GetSample().GetOutcome().(type) {
	case *frontendv1.FooterAllowanceSample_Available:
		return "available"
	case *frontendv1.FooterAllowanceSample_ServiceUnavailable:
		return "service_unavailable"
	case *frontendv1.FooterAllowanceSample_WindowUnavailable:
		return "window_unavailable"
	case *frontendv1.FooterAllowanceSample_UtilizationUnavailable:
		return "utilization_unavailable"
	case *frontendv1.FooterAllowanceSample_SamplingFailure:
		return "sampling_failure"
	default:
		return ""
	}
}

// sfAwaitSampleArm waits for the footer to draw the named account-usage
// outcome and answers the view that did.
func sfAwaitSampleArm(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, want string) *frontendv1.FooterView {
	t.Helper()
	footer := w.WatchFooter(ws)
	defer footer.Close()
	return harness.AwaitView(t, w.Ctx(), footer.Stream, "the footer to draw the "+want+" usage sample",
		func(v *frontendv1.FooterView) bool { return sfSampleArm(v) == want })
}

func TestAccountUsageUnreadArmsAreNamedOnTheFooter(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	cases := []struct {
		name     string
		scenario string
	}{
		{name: "service_unavailable", scenario: "usage-service-unavailable"},
		{name: "window_unavailable", scenario: "usage-window-unavailable"},
		{name: "utilization_unavailable", scenario: "usage-utilization-unavailable"},
		{name: "sampling_failure", scenario: "usage-sampling-failure"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a READ sample first, so the figures this arm must
			// leave standing are figures the daemon actually holds. Driven
			// rather than assumed: the session's own start-time probe is not
			// this test's to rely on.
			readable := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-available")
			sfAwaitConclusion(t, w, ws, readable,
				"The account-usage probe now answers with the available shape.")

			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert: the arm the scenario switched to, named in its own
			// conclusion (session.ts usageScenario composes it verbatim) and
			// then named again on the drawn footer.
			sfAwaitConclusion(t, w, ws, turn,
				"The account-usage probe now answers with the "+tc.name+" shape.")
			view := sfAwaitSampleArm(t, w, ws, tc.name)

			// Assert: ALONGSIDE, never instead of. The figures the last
			// readable sample filed are still drawn.
			line := sfRateLimited(view)
			if got := line.GetSession().GetUtilization(); got != sfFiveHourUtilization {
				t.Errorf("FooterAllowance(session).utilization = %v, want the standing %v left alone by an unread sample",
					got, sfFiveHourUtilization)
			}
			if got := line.GetWeekly().GetUtilization(); got != sfSevenDayUtilization {
				t.Errorf("FooterAllowance(weekly).utilization = %v, want the standing %v left alone by an unread sample",
					got, sfSevenDayUtilization)
			}
		})
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
// MEASURED in a playtest run before the fix: the sample went out at 37.494 to
// the bring-up's stream, the daemon's own watch opened at 37.503, and the
// footer's first sighting of any account usage was the turn-close reprobe
// 41ms later.
//
// The FIRST turn of the workspace is an unread one on purpose: the figures it
// draws cannot have come from its own close (that sample read nothing), so
// drawing them at all is the start-time probe having survived. It is the same
// shape the D33 playbook photographs.
func TestAccountUsageProbedAtSessionStartReachesTheFooter(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act: the workspace's very first turn, and it reads nothing.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-service-unavailable")
	sfAwaitConclusion(t, w, ws, turn,
		"The account-usage probe now answers with the service_unavailable shape.")

	// Assert
	view := sfAwaitSampleArm(t, w, ws, "service_unavailable")
	line := sfRateLimited(view)
	if got := line.GetSession().GetUtilization(); got != sfFiveHourUtilization {
		t.Errorf("FooterAllowance(session).utilization = %v, want %v from the session's own start-time probe",
			got, sfFiveHourUtilization)
	}
	if got := line.GetWeekly().GetUtilization(); got != sfSevenDayUtilization {
		t.Errorf("FooterAllowance(weekly).utilization = %v, want %v from the session's own start-time probe",
			got, sfSevenDayUtilization)
	}
}

// THE SAMPLING FAILURE KEEPS THE SHIM'S OWN CAUSE. The arm exists so a reader
// learns WHY the shim could not sample, and a cause the daemon dropped would
// leave the arm saying only "something".
func TestAccountUsageSamplingFailureCarriesACause(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, _ := sfNewWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-sampling-failure")
	sfAwaitConclusion(t, w, ws, turn,
		"The account-usage probe now answers with the sampling_failure shape.")

	// Assert
	view := sfAwaitSampleArm(t, w, ws, "sampling_failure")
	failure, ok := sfRateLimited(view).GetSample().GetOutcome().(*frontendv1.FooterAllowanceSample_SamplingFailure)
	if !ok {
		t.Fatalf("sample outcome = %v, want the sampling_failure arm", sfRateLimited(view).GetSample())
	}
	if failure.SamplingFailure.GetCause() == "" {
		t.Error("FooterAllowanceSampleSamplingFailure.cause is empty, want the shim's own account of what failed")
	}
}

// `!usage-opus-absent` IS NOT AN UNAVAILABILITY, and that is the whole
// scenario: the service answered in full and this account simply has no opus
// window (catalogs.ts: "An ABSENT OPTIONAL WINDOW, which is NOT an
// unavailability"). So the drawn fact is that it RETIRES a standing unread —
// the sample reads again, and with the fake's figures under the
// newsworthiness gate the rate line goes away entirely.
func TestAccountUsageOpusAbsentRetiresAStandingUnread(t *testing.T) {
	t.Parallel()
	// Arrange: an unread standing on the footer to be retired.
	w, ws, _ := sfNewWorkspace(t)
	unread := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-service-unavailable")
	sfAwaitConclusion(t, w, ws, unread,
		"The account-usage probe now answers with the service_unavailable shape.")
	sfAwaitSampleArm(t, w, ws, "service_unavailable")

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-opus-absent")
	sfAwaitConclusion(t, w, ws, turn,
		"The account-usage probe now answers with the opus_absent shape.")

	// Assert: the sample reads again, so the unread is gone and — the figures
	// being unremarkable — so is the line it rode on.
	footer := w.WatchFooter(ws)
	defer footer.Close()
	harness.AwaitView(t, w.Ctx(), footer.Stream, "the footer to retire the unread an absent optional window is not",
		func(v *frontendv1.FooterView) bool { return sfRateLimited(v) == nil })
}
