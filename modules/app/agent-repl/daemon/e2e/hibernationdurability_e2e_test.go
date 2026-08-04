// THE INVARIANT ACROSS A DAEMON BOUNCE: a hibernated session is structurally
// outside the keep-alive loop, and a ping that was already submitted is never
// submitted twice.
//
// WHY DURABILITY IS THE WHOLE CLAIM AND NOT A NICETY. "Hibernated but still
// pinging" is the one state the contract makes unrepresentable
// (frontend.proto HibernateWorkspaceCmd), and the cheapest way to break it is a
// restart: a daemon that came back believing nothing was asleep would spawn a
// shim, pay a full cold bring-up, and submit a model call for a workspace the
// user deliberately put down. The same restart is the cheapest way to double a
// ping, because the ping's own turn end is the only record that says the window
// was already served.
//
// A BOUNCE IS TWO DAEMONS OVER ONE DURABLE STATE DIRECTORY, which is what
// shutdownWorld.boot is for (shutdownscheduleharness_test.go). The shims are
// NOT torn down by a bounce: a shim outlives its daemon, redials the one
// well-known socket, and is parked by the next daemon's listener.
package e2e

import (
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
)

// keepAliveQuietWindow is how long a forbidden ping is watched for.
//
// It is a FAILURE bound, not a tuned delay, and follows connectivitystate's
// repaintWindow precedent: the daemon submits a due ping from inside the sweep
// it was fired by, so anything this side of a second is generous, and a passing
// test spends the whole window exactly once.
const keepAliveQuietWindow = 2 * time.Second

// assertNoPingWithin drains the tail for the quiet window and fails if a
// keep-alive ping appears.
func assertNoPingWithin(t *testing.T, tail *storeTail, why string) {
	t.Helper()
	forbidden := noKeepAlivePing(why)
	deadline := time.After(keepAliveQuietWindow)
	for {
		select {
		case ev, ok := <-tail.events:
			if !ok {
				return
			}
			if reason := forbidden(ev); reason != "" {
				t.Fatalf("forbidden durable event: %s", reason)
			}
		case <-deadline:
			return
		}
	}
}

// countKeepAlivePings drains the tail for the quiet window and reports how many
// keep-alive turns started in it.
func countKeepAlivePings(t *testing.T, tail *storeTail) int {
	t.Helper()
	count := 0
	deadline := time.After(keepAliveQuietWindow)
	for {
		select {
		case ev, ok := <-tail.events:
			if !ok {
				return count
			}
			if keepAlivePing(ev) != nil {
				count++
			}
		case <-deadline:
			return count
		}
	}
}

// spawnedShimFor reports whether this boot has exec'd a shim for workspace.
// Unlike shimFor it does not fail when there is none: "no shim was ever started
// for this workspace" is the assertion here, not a setup failure.
func spawnedShimFor(t *testing.T, b *shutdownBoot, workspace string) bool {
	t.Helper()
	// THE LOOKUP GOES THROUGH THE SAME CANONICALIZER THE SPAWN DID: the spawn
	// path records dlog.WorkspaceFromDirectory(...).Directory, and a test passes
	// the raw t.TempDir() path, which on macOS is /var/... while the canonical
	// form is /private/var/.... Comparing the two derivations would report
	// "no shim" for every workspace and pass this assertion vacuously.
	canonicalWorkspace, err := dlog.WorkspaceFromDirectory(workspace)
	if err != nil {
		t.Fatalf("canonicalize workspace %s: %v", workspace, err)
	}
	canonical := canonicalWorkspace.Directory
	b.shimsMu.Lock()
	defer b.shimsMu.Unlock()
	for _, s := range b.shims {
		if s.workspace == canonical {
			return true
		}
	}
	return false
}

// --- (5) a hibernated session is never pinged ---------------------------------

// TestE2EAHibernatedSessionIsNeverPinged covers the INVARIANT on a live daemon,
// with a positive control beside it: two workspaces are idle past the ping
// window and only the one that is awake is pinged.
//
// The control is what makes the negative mean something. Without it, a sweep
// that pinged nobody at all — because the policy was mis-wired, or the sweep
// never ran — would pass a bare "the hibernated one was not pinged".
func TestE2EAHibernatedSessionIsNeverPinged(t *testing.T) {
	// Arrange — A asleep, B awake, both on the same daemon and the same sweep.
	// Tempdirs before the harness: cleanups run LIFO, so this tears the shims
	// down before the directories are removed.
	cwdAsleep, cwdAwake := t.TempDir(), t.TempDir()
	policy := testKeepAlivePolicy()
	h := keepAliveHarness(t, policy)
	asleep := adoptKeepAliveSession(t, h, policy, cwdAsleep)
	awake := adoptKeepAliveSession(t, h, policy, cwdAwake)
	sendHibernate(t, asleep.conn, "r-hibernate")
	if ack := awaitAck(t, asleep.conn, "r-hibernate", "the forced hibernate"); !ack.GetOk() {
		t.Fatalf("hibernateWorkspace nacked: %s", ack.GetError())
	}
	awaitHibernationDetail(t, asleep.conn, asleep.sessionID)

	// Act — one sweep, both workspaces idle past the ping window.
	awake.idleFor(t, policy.pingAt())

	// Assert — the awake session is pinged...
	awake.store.await(t, "the awake session's keep-alive ping", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
	// ... and the sleeping one is not. The sweep that produced the ping above
	// is the same pass that would have produced this one, so its arrival is
	// what makes the quiet window a statement rather than a guess.
	assertNoPingWithin(t, asleep.store, "the session is hibernated, and hibernation and keep-alive-stop are ONE transition")
}

// TestE2EAHibernationSurvivesADaemonRestart covers the RECORD: the successor
// daemon knows the session is asleep, and knows why, without anyone telling it.
func TestE2EAHibernationSurvivesADaemonRestart(t *testing.T) {
	// Arrange — a hibernated session on the first daemon.
	cwd := t.TempDir()
	policy := testKeepAlivePolicy().apply(t)
	world := newShutdownWorld(t)
	first := world.boot(t, withBootIdleSweeper(policy.idleCutoff))
	id, conn, _, _ := liveSession(t, first.harness(), cwd)
	writeCmd(t, conn, `{"requestId":"r-warmup","submitPrompt":{"text":"warmup","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, conn, cwd, "the warm-up turn's result item", isResult)
	sendHibernate(t, conn, "r-hibernate")
	if ack := awaitAck(t, conn, "r-hibernate", "the forced hibernate"); !ack.GetOk() {
		t.Fatalf("hibernateWorkspace nacked: %s", ack.GetError())
	}
	awaitHibernationDetail(t, conn, id)

	// Act
	first.bounce()
	second := world.boot(t, withBootIdleSweeper(policy.idleCutoff))

	// Assert — the connect snapshot, which is what a webapp mounting after the
	// restart renders its revival gate from.
	view := snapshotSessionView(connectSnapshot(t, second), id)
	if view == nil {
		t.Fatalf("the successor's connect StateSnapshot carries no SessionView for %s at all", id)
	}
	if !view.GetHibernated() {
		t.Fatal("the successor reports the session awake: a hibernation the restart forgot is a session the daemon will bring up and bill the user for")
	}
	if view.GetHibernation().GetForced() == nil {
		t.Errorf("the successor reports hibernation cause %T, want the forced arm the predecessor recorded", view.GetHibernation().GetCause())
	}
}

// TestE2EARehydratedHibernatedSessionIsStillNeverPinged covers the invariant on
// the OTHER side of the restart: the successor's own keep-alive sweep leaves the
// sleeping session alone, and — the fact underneath that — never starts a shim
// for it at all.
//
// The spawn assertion is the structural one. A ping requires a shim to submit
// it through, so "this daemon exec'd no shim for that workspace" forecloses the
// whole class of ways a rehydrated session could be pinged, not merely the one
// this sweep took.
func TestE2EARehydratedHibernatedSessionIsStillNeverPinged(t *testing.T) {
	// Arrange — a hibernated session, and a successor daemon over its state.
	cwd := t.TempDir()
	policy := testKeepAlivePolicy().apply(t)
	world := newShutdownWorld(t)
	first := world.boot(t, withBootIdleSweeper(policy.idleCutoff))
	id, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	writeCmd(t, conn, `{"requestId":"r-warmup","submitPrompt":{"text":"warmup","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, conn, cwd, "the warm-up turn's result item", isResult)
	sendHibernate(t, conn, "r-hibernate")
	if ack := awaitAck(t, conn, "r-hibernate", "the forced hibernate"); !ack.GetOk() {
		t.Fatalf("hibernateWorkspace nacked: %s", ack.GetError())
	}
	awaitHibernationDetail(t, conn, id)
	first.bounce()
	second := world.boot(t, withBootIdleSweeper(policy.idleCutoff))
	tail := tailStore(t, vendorID)

	// Act — the successor's own keep-alive check, well past the ping window.
	second.clock.advance(policy.pingAt())
	second.sweepIdle <- time.Now()

	// Assert
	assertNoPingWithin(t, tail, "the session was hibernated before the restart and rehydrated hibernated")
	if spawnedShimFor(t, second, cwd) {
		t.Errorf("the successor daemon started a shim for the hibernated workspace %s: a sleeping session costs nothing until the user revives it", cwd)
	}
}

// --- (6) a restart does not double a ping ---------------------------------------

// TestE2EAPingAlreadySubmittedIsNotSubmittedAgainAfterARestart covers THE
// DOUBLE PING. The ping's own turn end is the durable record that the window
// was served; a successor that ignored it would re-serve the same window and
// pay for a second model call the moment it booted.
//
// WHAT "MID-PING" MEANS HERE. The offline engine completes a turn
// synchronously, so a daemon cannot be caught with a ping genuinely in flight
// without stubbing the engine — which would be testing the stub. What IS
// reproducible, and is the failure the invariant exists against, is a restart
// arriving immediately after the submission: the successor comes up with the
// same idle history and must read the ping already in it.
func TestE2EAPingAlreadySubmittedIsNotSubmittedAgainAfterARestart(t *testing.T) {
	// Arrange — the first daemon serves the window.
	cwd := t.TempDir()
	policy := testKeepAlivePolicy().apply(t)
	world := newShutdownWorld(t)
	first := world.boot(t, withBootIdleSweeper(policy.idleCutoff))
	id, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	writeCmd(t, conn, `{"requestId":"r-warmup","submitPrompt":{"text":"warmup","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, conn, cwd, "the warm-up turn's result item", isResult)
	tail := tailStore(t, vendorID)
	first.clock.advance(policy.pingAt())
	first.sweepIdle <- time.Now()
	tail.await(t, "the first daemon's keep-alive ping", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})

	// Act — the daemon dies with the window just served, and its replacement
	// runs its own check on a clock that has NOT been moved: measured from the
	// ping's own end, no time has passed and nothing is due.
	first.bounce()
	second := world.boot(t, withBootIdleSweeper(policy.idleCutoff))
	second.sweepRecheckWhenParked(t, id)
	second.sweepIdle <- time.Now()

	// Assert
	if got := countKeepAlivePings(t, tail); got != 0 {
		t.Fatalf("the successor daemon submitted %d further keep-alive ping(s) for a window its predecessor had already served: the ping's own turn end is the durable record that says the cache is warm", got)
	}
}

// TestE2ETheKeepAlivePingIsNeverRenderedAsConversation covers the exclusion the
// whole attribution exists for, on the frontend side: the ping is plumbing, so
// a connected client sees nothing of it — not the daemon's prompt, and not the
// vendor's one-token reply.
func TestE2ETheKeepAlivePingIsNeverRenderedAsConversation(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act — a ping, then a real turn behind it as the terminator.
	s.idleFor(t, policy.pingAt())
	s.store.await(t, "the keep-alive ping", func(ev *corev1.Event) bool { return keepAlivePing(ev) != nil })
	writeCmd(t, s.conn, `{"requestId":"r-after","submitPrompt":{"text":"after-the-ping","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — everything the frontend was shown between the ping and the real
	// turn's reply is free of the ping's text and of its answer.
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if text := assistantText(item); text != "" && strings.Contains(text, keepAlivePingText) {
				return "a ConversationDelta carried the keep-alive ping's turn: " + text
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the following real turn's reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf("after-the-ping")) {
					return true
				}
			}
			return false
		},
	})
}
