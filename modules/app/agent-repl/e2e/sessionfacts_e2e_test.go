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
//     HEALTH"), and `rate_limit_status` (SessionRateLimitStatus, "a STREAM-ONLY
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
// ONE DISPUTE IS RECORDED HERE RATHER THAN PAPERED OVER:
//
//  1. A VENDOR RATE-LIMIT EVENT FEEDS THE ENDURING USAGE LINE (owner
//     ruling, 2026-10-01, superseding the 2026-09-30 ruling that made it
//     salient): the event's figure and verdict land on the allowance it
//     names, and no salient line stands. `!rate-limit-seven-day` and
//     `!rate-limit` each send an `allowed_warning`, so each draws that
//     verdict on its allowance. The five-hour window is not drawn (owner
//     ruling, 2026-10-30), and the account's usage is never sampled.
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

	"agent-repld/integration/grantedclock"
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
			row.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == want
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
	ctx, cancel := grantedclock.WithBound(w.Ctx(), DefaultTimeout)
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
// The seven-day rate-limit window — `!rate-limit-seven-day` (the same
// scenario factory naming the `seven_day` window at utilization 0.91).
//
// The event feeds the WEEKLY allowance, its verdict and its figure. Both
// hops are asserted — the drawn
// allowance and the session-arm record behind it — because the allowance
// alone would not say the event's own window reached the store.
// ===========================================================================

// TestRateLimitSevenDayWindowFeedsTheWeeklyAllowance wires a session's
// rate-limit event through the daemon to the footer's weekly allowance.
//
// Seam: session-footer
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

	ctx, cancel := grantedclock.WithBound(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := sfAwaitEventAllowance(t, ctx, footer.Stream, "the weekly allowance", func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
		return u.GetWeekly().GetOverall()
	}, 0.91)

	// Assert
	sfAssertEventFeedsAllowance(t, view, footerEnduringUsage(view).GetWeekly().GetOverall(), 0.91)
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

// sfAwaitEventAllowance awaits the footer view whose enduring usage line
// draws the allowance PICK with a rate-limit event's `allowed_warning`
// verdict AND its figure WANT, within half a point.
func sfAwaitEventAllowance(t *testing.T, ctx context.Context, footer *harness.Stream[*frontendv1.FooterView], what string, pick func(*frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance, want float64) *frontendv1.FooterView {
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

