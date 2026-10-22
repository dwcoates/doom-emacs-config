// Package e2e — degraded-state area (SPEC.md section C, "Degraded state",
// entries #56-57).
//
// CONTRACT: this file provokes a REAL shim-store outage with the harness's
// Store.Stop / Store.StartSameDB control and asserts what the daemon renders
// on the topbar's warning strip — NEVER a fabricated fact. This is
// E2E-EVENT-INVENTORY.md's ruling 2 verbatim ("2. degradedStateEvent — RULED:
// provoke a REAL store outage. The degraded-state telemetry is produced by
// the REAL shim when the REAL store goes away, so the test drives that
// condition rather than fabricating its report... No fabricated telemetry
// survives."), restated at SPEC.md's "Store stop/restart control (ruling 2)".
//
// The wire path this file asserts against: the real shim keeps its own
// health report — conversation/v1/session.proto's SessionDiagnostics, "kept
// since shim start" — with one SessionDegradedWindow per component that
// degraded, each an open/closed oneof (open: still degraded; closed:
// ended_at_ms + dropped_count). shim.md's session.proto file-map entry
// states this is "PUSHED at the shim's cadence and on change... the daemon's
// sessionwatcher routes it (topbar warnings)" — i.e. SessionUpdate.diagnostics
// (tag 25) arrives on the shim's direct connection to the daemon (never
// through the store, which is a separate durability path the shim/sidecar
// write to — daemon.md/shim.md: "the daemon must never import the [store]
// package... the daemon's read path is shim.v1"), and the daemon projects it
// into frontend/v1/topbar.proto's TopbarView.warnings (TopbarWarningStrip),
// one TopbarWarning per window carrying a TopbarDegradedWindowWarningDetail
// (component, reason, began_at_ms, extent{open|closed{ended_at_ms,
// dropped_count}}). daemon.md's "Failure classification" section: "upstream
// silence is stated by the daemon as a fact (shim-degraded arms)".
//
// Because the turn's own liveness is carried over that SAME direct shim-daemon
// link, not through the store, a turn submitted during the outage still
// completes (AwaitTurnEnded reads the feed, which is daemon-held per
// daemon.md's "GetFeedPage's walk position is DAEMON-held") — the outage only
// degrades the shim's OWN store-write path, which is exactly the fact these
// tests assert on.
//
// Neither test pins the shim's own literal component/reason strings: SPEC.md's
// binding instruction is to write to the CONTRACT, never to a production
// shape inferred by reading source, and the contract only commits to "a
// component degraded, for a stated reason" (topbar.proto) — so the assertions
// below are structural (non-empty, matching identity across the open/closed
// pair) rather than string-literal.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// openWorkspace registers a fresh repository (harness.NewRepo, the scripted
// fake-git world — this suite mocks every external dependency except
// agent-repld, the shim, shim-store and shim-sidecar) and opens it on the daemon,
// spawning the real session the degraded-state tests observe. Modeled on
// daemon/integration/support_test.go's fixture.open(): SubmitPrompt refuses
// with SubmitPromptNoSession (endpoint_submit_prompt.proto) until the
// workspace has been opened, and a workspace is not CONNECTED (daemon.md
// invariant 11) until both client-hop streams are held, so this holds them
// for the test's life exactly as that convention does.
func openWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	ws := w.NewWorkspace(t)

	w.RequireOpen(t, ws)

	host := w.WatchHost(ws)
	t.Cleanup(host.Close)
	web := w.WatchWeb(ws)
	t.Cleanup(web.Close)

	return ws
}

// findOpenDegradedWindow answers the first OPEN degraded-window warning on
// this topbar view, or nil.
func findOpenDegradedWindow(v *frontendv1.TopbarView) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, warn := range v.GetWarnings().GetWarnings() {
		dw := warn.GetDegradedWindow()
		if dw != nil && dw.GetOpen() != nil {
			return dw
		}
	}
	return nil
}

// findClosedDegradedWindow answers the CLOSED degraded-window warning on this
// topbar view that names the same component and began_at_ms as `open` — the
// SAME window `open` named, now recovered — or nil.
func findClosedDegradedWindow(v *frontendv1.TopbarView, open *frontendv1.TopbarDegradedWindowWarningDetail) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, warn := range v.GetWarnings().GetWarnings() {
		dw := warn.GetDegradedWindow()
		if dw == nil || dw.GetClosed() == nil {
			continue
		}
		if dw.GetComponent().GetText() == open.GetComponent().GetText() && dw.GetBeganAtMs() == open.GetBeganAtMs() {
			return dw
		}
	}
	return nil
}

// TestRecoveryAfterStoreRestart is SPEC.md §C #57 (RecoveryAfterStoreRestart):
// same family, second half — the SAME degraded window transitions to CLOSED
// once the store is back and the shim reconnects ("the degraded fact
// clears", SPEC.md).
func TestRecoveryAfterStoreRestart(t *testing.T) {
	t.Parallel()
	// Arrange: same setup as TestDegradedDuringRealStoreOutage, through the
	// open window.
	w := NewWorld(t, WorldOpts{})
	// The store outage this test provokes is exactly a health fault. It also
	// reaches the daemon's own standing streams: the shim serves WatchAgent
	// out of the store, so a store the test killed severs that stream and
	// the link with it. Whether the break lands inside the outage window is
	// a race, and an undeclared record whose observation is a race is a
	// flake rather than a finding.
	w.ExpectWarnings("daemon.health.open_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.link_fault")
	ws := openWorkspace(t, w)
	topbar := w.WatchTopbar(ws)
	t.Cleanup(topbar.Close)

	// No terminal is awaited inside the outage — see
	// TestDegradedDuringRealStoreOutage's own note: the daemon's turn
	// terminal is store-derived (shim.md:422-425), so none exists while the
	// store is down.
	w.Store.Stop()
	SubmitPrompt(t, w, ws, "!prose-streamed")

	openCtx, openCancel := grantedclock.WithBound(w.Ctx(), StoreOutageWindow)
	defer openCancel()
	openedView := harness.AwaitView(t, openCtx, topbar, "an open degraded window from the real store outage", func(v *frontendv1.TopbarView) bool {
		return findOpenDegradedWindow(v) != nil
	})
	opened := findOpenDegradedWindow(openedView)
	if opened == nil {
		t.Fatalf("topbar push matched an open degraded window but re-scanning it found none")
	}

	// Act: restore the store on the SAME socket + database (StartSameDB).
	w.Store.StartSameDB(t)

	// Assert: the SAME window (identified by component + began_at_ms) closes.
	closeCtx, closeCancel := grantedclock.WithBound(w.Ctx(), StoreOutageWindow)
	defer closeCancel()
	closedView := harness.AwaitView(t, closeCtx, topbar, "the same degraded window closing after the store restarted", func(v *frontendv1.TopbarView) bool {
		return findClosedDegradedWindow(v, opened) != nil
	})
	closed := findClosedDegradedWindow(closedView, opened)
	if closed == nil {
		t.Fatalf("topbar push matched the closed window but re-scanning it found none")
	}
	if got, want := closed.GetClosed().GetEndedAtMs(), opened.GetBeganAtMs(); got < want {
		t.Errorf("closed degraded window ended_at_ms = %d, want >= began_at_ms %d", got, want)
	}
	if got := closed.GetClosed().GetDroppedCount(); got < 0 {
		t.Errorf("closed degraded window dropped_count = %d, want >= 0", got)
	}
}
