// sessionfacts_e2e_test.go — the SESSION-SCOPED facts that ride
// conversation.v1 SessionUpdate and have no turn or unit of their own: the
// MCP catalog narrowing, the vendor's rate-limit windows, and the
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
//     `resets_at_s`, `utilization`, the typed verdict). A vendor rate-limit
//     event stands no salient line: it feeds these figures (owner ruling,
//     2026-10-01).
//   - proto/src/frontend/v1/mcp_panel.proto — McpPanelView/McpPanelRow, the
//     surface the MCP healths are drawn on (driven here through /mcp, the
//     same synchronous command mcpmonitors_e2e_test.go reads).
//   - agent-shim/claude/shim/src/fake/scenarios/session.ts and
//     src/fake/catalogs.ts — the scenarios driven here and their exact
//     figures.
//
// FAST MODE HAS NO FRONTEND SURFACE: the topbar's fast-mode cell was dropped
// (owner ruling, 2026-10-02), so nothing here asserts on it.
//
// TWO DISPUTES ARE RECORDED HERE RATHER THAN PAPERED OVER. Each is stated
// in full at the test it constrains; in summary:
//
//  1. A VENDOR RATE-LIMIT EVENT FEEDS THE ENDURING USAGE LINE (owner
//     ruling, 2026-10-01, superseding the 2026-09-30 ruling that made it
//     salient): the event's figure and verdict land on the allowance it
//     names, and no salient line stands. `!rate-limit-five-hour`,
//     `!rate-limit-seven-day` and `!rate-limit` each send an
//     `allowed_warning`, so each draws that verdict on its allowance.
//
//  2. AN UNREAD SAMPLE DRAWS NO CAVEAT (owner ruling, fc4917be4,
//     2026-09-15). An unread sample leaves the read figures standing, and
//     its outcome — its reason, and a sampling
//     failure's cause — is recorded on the daemon's `daemon.footer.usage_
//     sample_unreadable` breadcrumb instead. The enduring usage line is drawn
//     whatever the figures (owner ruling, 2026-09-28), so the tests below
//     read the sampled figures straight off it.
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

	"agent-repld/integration/harness"
)

// sfNewWorkspace builds one World and registers a fresh scripted-fake-git
// repository as its workspace.
func sfNewWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef, string) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo, ws := w.NewRepoWorkspace(t)
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

// ===========================================================================
// The five-hour rate-limit window — `!rate-limit-five-hour`
// (session.ts rateLimitWindowScenario: an `allowed_warning`
// rate_limit_event naming the `five_hour` window at utilization 0.82,
// resetting in an hour).
//
// THE EVENT FEEDS THE ENDURING USAGE LINE (owner ruling, 2026-10-01): the
// SESSION allowance carries the event's `allowed_warning` verdict, and no
// salient line stands — the awaited view is the unpinned tier, which a
// salient line would replace.
//
// ON A SAMPLED WINDOW THE EVENT SUPPLIES THE VERDICT AND THE SAMPLE THE
// FIGURE (footer resolver.go observeRateLimitStatus): an event carries no
// instant, so its utilization never overwrites the newest sample's. The
// figure the line draws is therefore the account-usage sample that follows
// the event, which the fake reports at the event's own figure (one account,
// one figure per window: shim fake/catalogs.ts noteAnnouncedWindow), so the
// awaited view is the one carrying BOTH (`sfAwaitSampledAllowance`).
// ===========================================================================

// ===========================================================================
// The seven-day rate-limit window — `!rate-limit-seven-day` (the same
// scenario factory naming the `seven_day` window at utilization 0.91).
//
// The event feeds the WEEKLY allowance, its verdict directly and its figure
// through the sample that follows it, as the five-hour window's does. Both
// hops are asserted — the drawn
// allowance and the session-arm record behind it — because the allowance
// alone would not say the event's own window reached the store.
// ===========================================================================

func TestRateLimitSevenDayWindowFeedsTheWeeklyAllowance(t *testing.T) {
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
	view := sfAwaitSampledAllowance(t, ctx, footer.Stream, "the weekly allowance", (*frontendv1.FooterActivityEnduringUsage).GetWeekly, 0.91)

	// Assert
	sfAssertEventFeedsAllowance(t, view, footerEnduringUsage(view).GetWeekly(), 0.91)
	sfAwaitSessionArmRecords(t, w, workspaceDir, sfFooterSessionUpdate, "rate_limit_status", before+1)
}

// ===========================================================================
// The overage window — `!rate-limit` (session.ts RATE_LIMIT: an
// `allowed_warning` event on the `overage` window at utilization 0.79 with a
// surpassed threshold).
//
// The event feeds the OVERAGE allowance whatever the figure: 0.79 sits below
// the newsworthy gate, which only colors an allowance and never decides
// whether it is drawn. The harness's warning sweep — which fails on any warn
// record this test does not declare — still asserts the retired
// `daemon.footer.rate_limit_overage` warning stays gone.
// ===========================================================================

// sfAwaitSampledAllowance awaits the footer view whose enduring usage line
// draws the SAMPLED allowance PICK with a rate-limit event's `allowed_warning`
// verdict AND the account's figure WANT, within half a point. On a sampled
// window the two arrive on different facts — the verdict on the event, the
// figure on the sample after it — so the view carrying only the verdict is
// not yet the event's whole effect. Awaiting on the verdict alone raced the
// sample: the first such view still drew the figure sampled BEFORE the event.
func sfAwaitSampledAllowance(t *testing.T, ctx context.Context, footer *harness.Stream[*frontendv1.FooterView], what string, pick func(*frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance, want float64) *frontendv1.FooterView {
	t.Helper()
	return harness.AwaitView(t, ctx, footer, what+"'s warning at the account's figure on the enduring usage line", func(v *frontendv1.FooterView) bool {
		allowance := pick(footerEnduringUsage(v))
		got := allowance.GetUtilization()
		return allowance.GetAllowedWarning() != nil && got >= want-0.005 && got <= want+0.005
	})
}

// sfAssertEventFeedsAllowance asserts one rate-limit event's figures landed on
// ALLOWANCE: its utilization as a 0..1 fraction within half a point of WANT,
// and its reset instant in epoch SECONDS. It also asserts VIEW stands no
// salient line, because the event is never one (owner ruling, 2026-10-01).
func sfAssertEventFeedsAllowance(t *testing.T, view *frontendv1.FooterView, allowance *frontendv1.FooterAllowance, want float64) {
	t.Helper()
	if salient := footerTier(view, "salient"); salient != nil {
		t.Errorf("footer stands salient line %v, want none: a rate-limit event feeds the enduring usage line only", salient)
	}
	if got := allowance.GetUtilization(); got < want-0.005 || got > want+0.005 {
		t.Errorf("FooterAllowance.utilization = %v, want the event's %v as a 0..1 fraction", got, want)
	}
	if allowance.GetResetsAtS() == 0 {
		t.Errorf("FooterAllowance.resets_at_s = 0, want the event's reset instant in epoch SECONDS")
	}
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
