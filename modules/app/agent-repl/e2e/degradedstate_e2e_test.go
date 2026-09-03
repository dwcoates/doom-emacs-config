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
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// openWorkspace registers a fresh repository (harness.NewRepo, the scripted
// fake-git world — this suite mocks every external dependency except
// claude-repld, the shim, shim-store and shim-sidecar) and opens it on the daemon,
// spawning the real session the degraded-state tests observe. Modeled on
// daemon/integration/support_test.go's fixture.open(): SubmitPrompt refuses
// with SubmitPromptNoSession (endpoint_submit_prompt.proto) until the
// workspace has been opened, and a workspace is not CONNECTED (daemon.md
// invariant 11) until both client-hop streams are held, so this holds them
// for the test's life exactly as that convention does.
func openWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace = %v, want success", resp.Msg)
	}

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

// TestDegradedDuringRealStoreOutage is SPEC.md §C #56
// (DegradedDuringRealStoreOutage): a real store outage, provoked with
// Store.Stop, is stated by the daemon as an OPEN degraded window on the
// topbar's warning strip.
func TestDegradedDuringRealStoreOutage(t *testing.T) {
	// Arrange: an opened workspace with a live session, whose store link is
	// healthy at session start (GetLiveWork, shim.md, succeeds before the
	// outage begins).
	w := NewWorld(t, WorldOpts{})
	ws := openWorkspace(t, w)
	topbar := w.WatchTopbar(ws)
	t.Cleanup(topbar.Close)

	// Act: provoke a REAL store outage (never a fabricated fact — ruling 2),
	// then submit a real prompt so the shim actually attempts to persist
	// something against the now-dead store link.
	//
	// NO TERMINAL IS AWAITED INSIDE THE OUTAGE. The daemon's turn terminal
	// is STORE-DERIVED (docs/overhaul/shim.md:422-425 — the daemon's reads
	// of a turn, including its terminal, are served from the store), so
	// while Store.Stop() holds there is no terminal for AwaitTurnEnded to
	// find and the wait could only time out. The submission alone provokes
	// the degraded window this test is about.
	w.Store.Stop()
	SubmitPrompt(t, w, ws, "!prose-streamed")

	ctx, cancel := context.WithTimeout(w.Ctx(), StoreOutageWindow)
	defer cancel()

	// Assert
	dw := harness.AwaitView(t, ctx, topbar, "an open degraded window from the real store outage", func(v *frontendv1.TopbarView) bool {
		return findOpenDegradedWindow(v) != nil
	})
	got := findOpenDegradedWindow(dw)
	if got == nil {
		t.Fatalf("topbar push matched an open degraded window but re-scanning it found none")
	}
	if got.GetComponent().GetText() == "" {
		t.Errorf("degraded window component = %q, want a stated component (topbar.proto TopbarWarningComponent)", got.GetComponent().GetText())
	}
	if got.GetReason().GetText() == "" {
		t.Errorf("degraded window reason = %q, want a stated reason (topbar.proto TopbarWarningDetailLine)", got.GetReason().GetText())
	}
	if got.GetBeganAtMs() <= 0 {
		t.Errorf("degraded window began_at_ms = %d, want a positive epoch-ms timestamp", got.GetBeganAtMs())
	}
}

// TestRecoveryAfterStoreRestart is SPEC.md §C #57 (RecoveryAfterStoreRestart):
// same family, second half — the SAME degraded window transitions to CLOSED
// once the store is back and the shim reconnects ("the degraded fact
// clears", SPEC.md).
func TestRecoveryAfterStoreRestart(t *testing.T) {
	// Arrange: same setup as TestDegradedDuringRealStoreOutage, through the
	// open window.
	w := NewWorld(t, WorldOpts{})
	ws := openWorkspace(t, w)
	topbar := w.WatchTopbar(ws)
	t.Cleanup(topbar.Close)

	// No terminal is awaited inside the outage — see
	// TestDegradedDuringRealStoreOutage's own note: the daemon's turn
	// terminal is store-derived (shim.md:422-425), so none exists while the
	// store is down.
	w.Store.Stop()
	SubmitPrompt(t, w, ws, "!prose-streamed")

	openCtx, openCancel := context.WithTimeout(w.Ctx(), StoreOutageWindow)
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
	closeCtx, closeCancel := context.WithTimeout(w.Ctx(), StoreOutageWindow)
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
