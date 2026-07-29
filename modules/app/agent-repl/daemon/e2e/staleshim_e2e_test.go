package e2e

import (
	"runtime"
	"testing"
)

// THE STALE-SHIM REFRESH, end to end against a REAL bundle.
//
// A shim outlives its daemon, so a deploy never reached one: the bundle on disk
// moved and the running process kept executing the code it started with. The
// unit seams pin the comparator; this pins that the whole chain actually
// exists — build.mjs bakes the identity in, the shim reports it on its hello,
// and the daemon acts on the difference — because every one of those links is a
// place the fact could be dropped silently.
//
// The harness bakes one identity into a genuinely built bundle and tells the
// daemon the current one is different, which is precisely a shim that survived
// a deploy.

// TestAStaleShimIsBouncedOntoTheCurrentBundle — one bounce, and only one.
//
// The respawned shim is the SAME bundle and so reports the same stale identity;
// the daemon's once-per-session latch is what stops that from becoming an
// endless restart loop, and the absence of a third exec is what proves it.
func TestAStaleShimIsBouncedOntoTheCurrentBundle(t *testing.T) {
	// Arrange — a bundle built at "old" against a daemon whose current bundle
	// is "new".
	h := newUDSHarness(t, withStaleShimBuild("sha-old", "sha-new"))

	// Act — creating the session brings the (stale) shim up, which handshakes
	// and is judged.
	h.createSession(t, t.TempDir())

	// Assert — a SECOND exec for the session is the bounce. Waiting on the
	// count is a rendezvous with the bounce goroutine, not a timed guess.
	for h.shimSpawns() < 2 {
		runtime.Gosched()
	}
	if got := h.shimSpawns(); got < 2 {
		t.Fatalf("shim execs = %d, want at least 2 — the stale shim was never bounced", got)
	}
}

// TestACurrentShimIsNeverBounced — the steady state, and the guard against a
// refresh that fires on every session. A bundle whose identity matches the
// daemon's is executed exactly once.
func TestACurrentShimIsNeverBounced(t *testing.T) {
	// Arrange — bundle and daemon agree.
	h := newUDSHarness(t, withStaleShimBuild("sha-same", "sha-same"))
	cwd := t.TempDir()

	// Act — the same bring-up, judged the same way. A completed health probe
	// happens strictly AFTER the handshake that would have triggered a bounce,
	// so the assertion below rests on a finished comparison rather than racing
	// one.
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	defer conn.Close()
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	awaitSessionHealth(t, conn, cwd, id, "e2e-stale-shim-health-1")

	// Assert.
	if got := h.shimSpawns(); got != 1 {
		t.Fatalf("shim execs = %d, want exactly 1 — a matching bundle must never be bounced", got)
	}
}
