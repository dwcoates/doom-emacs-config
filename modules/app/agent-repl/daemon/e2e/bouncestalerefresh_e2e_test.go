// THE STALE-SHIM REFRESH IS AN AUTOMATIC STOP, AND AUTOMATIC STOPS WAIT.
//
// THE GAP. `refreshStaleShim` fires from `onConnectedForGeneration`, which runs
// at ShimReady — the shim's handshake — and it has NO TURN GATE at all
// (sessioncontroller/buildrefresh.go, sessioncontroller.go:2998). The one
// moment a surviving shim reports its build identity is precisely the moment it
// is reattaching after a bounce, and a shim that survived a deploy is
// reattaching MID-TURN as often as not. Today that turn is destroyed by a
// process replacement the user never asked for and cannot see coming: the
// refresh calls RestartSession, which stops the live shim immediately.
//
// WHAT MUST BE TRUE INSTEAD. An AUTOMATIC stop waits for the turn boundary,
// behind a lease that is only settled when no turn is in flight. The in-flight
// turn completes untouched; the replacement happens at the boundary, where
// replacing a process costs the user nothing.
//
// AND THE OTHER HALF OF THE SAME CONTRACT: an EXPLICIT, user-commanded restart
// stays IMMEDIATE. The gate is about work the user did not ask to interrupt,
// not about making the one control that rescues a wedged session wait for the
// wedge to clear. Both halves are pinned here, because a fix that gated
// everything would be as wrong as the ungated present.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// spawnedShims reports how many shim processes THIS boot has exec'd. A stale
// refresh is visible as a spawn; its absence while a turn is in flight is the
// gate doing its job.
func (b *shutdownBoot) spawnedShims() int {
	b.shimsMu.Lock()
	defer b.shimsMu.Unlock()
	return len(b.shims)
}

// staleReattachWorld is the arrangement both gating tests share: a bundle baked
// at `sha-old`, a first daemon that agrees with it (so the session comes up
// with no refresh at all), and a successor that believes the current bundle is
// `sha-new` — a shim that outlived a deploy, exactly as a real one does.
func staleReattachWorld(t *testing.T) (*shutdownWorld, *shutdownBoot) {
	t.Helper()
	world := newShutdownWorld(t, withWorldShimBuild("sha-old"))
	return world, world.boot(t, withBootShimBuild("sha-old"))
}

// TestE2EAStaleShimReattachingMidTurnIsNotInterrupted covers THE GATE: the
// successor recognizes the stale bundle while a turn is in flight and leaves
// the turn alone.
//
// The evidence is the successor's own re-derivation of the mid-turn state: a
// WorkspaceState carrying turn_active, produced from the store replay the
// reattach opens. On today's code that replay is never opened — the refusal to
// wire and the restart happen in the same hook, BEFORE connectivity is
// published — so the absence of that frame is the failure, and it is bounded
// rather than a hang.
func TestE2EAStaleShimReattachingMidTurnIsNotInterrupted(t *testing.T) {
	// Arrange — a live turn on a shim whose bundle the successor will call stale.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world, first := staleReattachWorld(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-stale-midturn")

	// Act — the deploy happened: the daemon that comes back knows a newer bundle.
	first.bounce()
	second := world.boot(t, withBootShimBuild("sha-new"))
	_, afterConn, _ := reattached(t, second, cwd)

	// Assert — the successor re-derives the live turn instead of ending it, and
	// never paints the workspace interrupted on the way.
	awaitAll(t, afterConn,
		rejectRestartInterrupted(cwd, "a stale BUILD IDENTITY is a reason to replace the process at the next turn boundary, never a reason to destroy the turn the user is waiting on"),
		map[string]func(*frontendv1.FrontendFrame) bool{
			"a WorkspaceState from the successor still reporting the turn ACTIVE — the mid-turn reattach must wire the session and re-derive its live turn, not stop the shim at the handshake that reported the stale build": func(frame *frontendv1.FrontendFrame) bool {
				state := workspaceStateFor(frame, cwd)
				return state != nil && state.GetTurnActive()
			},
		})
	if got := second.spawnedShims(); got != 0 {
		t.Errorf("the successor exec'd %d shim(s) while a turn was in flight, want 0: the stale-build replacement is an AUTOMATIC stop and automatic stops wait for the turn boundary", got)
	}
}

// TestE2EAGatedStaleRefreshRunsAtTheTurnBoundary covers THE OTHER SIDE OF THE
// GATE: waiting is a DEFERRAL, not a cancellation. Once the turn is over the
// deferred replacement runs, and the workspace ends up on the current bundle.
func TestE2EAGatedStaleRefreshRunsAtTheTurnBoundary(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world, first := staleReattachWorld(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-stale-boundary")

	// Act — reattach mid-turn against a daemon that calls the bundle stale...
	first.bounce()
	second := world.boot(t, withBootShimBuild("sha-new"))
	_, afterConn, _ := reattached(t, second, cwd)
	// THE REATTACH IS OBSERVED BEFORE THE STOP IS SENT, exactly as the
	// non-interruption sibling above observes it. `reattached` fires the boot
	// sweep's re-check pass; the pass that CLAIMS the surviving shim runs behind
	// it, and until it has, the successor drives no session controller for this
	// workspace and a stop has nothing to travel over. Sending one into that
	// window measures the reattach's timing rather than the lease, and the
	// boundary this test is about would never be reported at all.
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState from the successor reporting the turn ACTIVE — the reattach the stop below must be delivered through": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, cwd)
			return state != nil && state.GetTurnActive()
		},
	})
	if got := second.spawnedShims(); got != 0 {
		t.Fatalf("the successor exec'd %d shim(s) BEFORE the turn boundary, want 0: the deferral this test is about never happened, so what follows would be measuring an ungated restart", got)
	}
	// ... then reach the boundary by stopping the turn the gate was protecting.
	writeCmd(t, afterConn, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState with the turn no longer active (the boundary the deferred stop was waiting for)": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, cwd)
			return state != nil && !state.GetTurnActive()
		},
	})

	// Assert — the deferred replacement runs now.
	//
	// Spinning on the count is a rendezvous with the refresh goroutine rather
	// than a timed guess, and it is BOUNDED: a deferral that is silently a
	// cancellation must fail here by the frame budget, never sit forever.
	awaitCount(t, second.spawnedShims, 1,
		"a replacement shim exec at the turn boundary — the gate DEFERS an automatic stop, it does not cancel it, so a workspace that was found running a superseded bundle must end up on the current one once its turn is over")
}

// TestE2EAnExplicitRestartOfALiveTurnIsImmediate covers THE EXPLICIT HALF: a
// user-commanded RestartSessionCmd replaces the process NOW, mid-turn, without
// waiting for any boundary.
//
// It needs no bounce: the contract is about the command, and the command is the
// same one whether or not a deploy happened. It is the standing guard that a
// fix for the gate above does not make the deliberate escape hatch wait for the
// very turn a user is trying to escape.
func TestE2EAnExplicitRestartOfALiveTurnIsImmediate(t *testing.T) {
	// Arrange — a live, parked turn on an ordinary (non-stale) session.
	// The workspace tempdir is created BEFORE the harness on purpose: cleanups
	// run LIFO, so this ordering tears the harness (and its shim processes)
	// down before the tempdir is removed.
	cwd := t.TempDir()
	h := newUDSHarness(t)
	_, conn, _, _ := liveSession(t, h, cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-explicit-restart")

	// Act — the user asks for the process to be replaced, mid-turn.
	sendRestartSession(t, conn, cwd, "r-restart")

	// Assert — a SECOND exec, without the turn having been allowed to finish.
	// Waiting on the count is a rendezvous with the restart, not a timed guess,
	// and it is bounded so a restart that started waiting for the turn boundary
	// fails here rather than hanging.
	awaitCount(t, func() int { return int(h.shimSpawns()) }, 2,
		"a second shim exec while the turn is still parked — an EXPLICIT restart is the deliberate escape hatch from a wedged session and may never be made to wait for the turn it is escaping")
}
