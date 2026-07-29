package ssm

import (
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// The gate: an agent state is visible ONLY while the workspace is wired.
// ---------------------------------------------------------------------------

// The law, stated once per agent state. Every one of these is a claim the
// vocabulary makes about a LIVE session, so an unwired workspace may report
// none of them — it reports the absence itself.
func TestAgentStatesAreVisibleOnlyWhileWired(t *testing.T) {
	tests := []struct {
		name  string
		token string
		cause string
		wired frontendv1.RenderState
	}{
		{"thinking", sigThinking, causeTurnStarted, frontendv1.RenderState_RENDER_STATE_THINKING},
		{"ready", sigReady, causeSessionStarted, frontendv1.RenderState_RENDER_STATE_READY},
		{"done", sigDone, causeTurnEnded, frontendv1.RenderState_RENDER_STATE_DONE},
		{"interrupted", sigInterrupted, causeInterrupted, frontendv1.RenderState_RENDER_STATE_INTERRUPTED},
		{"permission", sigPermission, causePermission, frontendv1.RenderState_RENDER_STATE_PERMISSION},
		{"idle", sigIdle, causeSessionStarted, frontendv1.RenderState_RENDER_STATE_IDLE},
		{"vendor_blocked", sigVendorBlocked, causeVendorBlocked, frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED},
	}
	for _, tc := range tests {
		t.Run(tc.name+" shows through a wired workspace", func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			seedSignal(t, db, "ws", "s1", tc.token, tc.cause, 1, 5)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != tc.wired {
				t.Fatalf("state = %s, want %s", renderName(got.state), renderName(tc.wired))
			}
		})
		t.Run(tc.name+" is hidden by an unwired workspace", func(t *testing.T) {
			// Arrange — the wired row is OLDER than the agent row, so only RANK
			// can make it win.
			db := newTestDB(t)
			seedSignal(t, db, "ws", "", sigDormant, causeWired, -1, 1)
			seedSignal(t, db, "ws", "s1", tc.token, tc.cause, 1, 5)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != frontendv1.RenderState_RENDER_STATE_DORMANT {
				t.Fatalf("state = %s, want DORMANT — an unwired workspace may report nothing about the agent", renderName(got.state))
			}
		})
	}
}

// The yellow promotion is derived from a GREEN winner, so it must not survive
// the gate either: background tasks counted for a workspace nobody is connected
// to are not something to advertise as running.
func TestBackgroundWorkDoesNotPromoteAnUnwiredWorkspace(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "", sigDormant, causeWired, -1, 1)
	seedSignal(t, db, "ws", "s1", sigIdle, causeSessionStarted, 1, 2)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 3, "a1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_DORMANT {
		t.Fatalf("state = %s, want DORMANT", renderName(got.state))
	}
}

// ---------------------------------------------------------------------------
// The three positions of the axis
// ---------------------------------------------------------------------------

// A bring-up in flight is INIT: blue, and the one blue that legitimately spins.
func TestStartingResolvesInit(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.ApplyWired("ws1", WiringStarting, "bring_up"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT for a bring-up in flight", renderName(got))
	}
}

// Not wired and not starting is DORMANT, its own word rather than INIT's.
func TestDormantResolvesItsOwnState(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.ApplyWired("ws1", WiringDormant, "hibernated"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DORMANT {
		t.Fatalf("state = %s, want DORMANT", renderName(got))
	}
}

// A workspace with agent history and NO wired row at all is dormant, not
// "unknown": the log carries no evidence of a live session behind it.
func TestAnAbsentWiredAxisResolvesDormant(t *testing.T) {
	// Arrange — a workspace whose log predates the axis entirely.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s1", sigDone, causeTurnEnded, 1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_DORMANT {
		t.Fatalf("state = %s, want DORMANT for a workspace with no wired row", renderName(got.state))
	}
}

// The synthesized dormant candidate still names the conversation it is about,
// so a frontend is not handed a state with no session id.
func TestTheSynthesizedDormantCandidateKeepsTheSessionID(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s7", sigDone, causeTurnEnded, 1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.sessionID != "s7" {
		t.Fatalf("sessionID = %q, want s7", got.sessionID)
	}
}

// A workspace with ONLY task counters still resolves found=false. The
// synthesized dormant candidate rides the agent axis, so a workspace that has
// never had one stays unborn rather than becoming a dormant tab.
func TestATaskOnlyWorkspaceIsStillUnborn(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 1, 1, "a1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.found {
		t.Fatalf("found=true (state=%s), want an unborn workspace", renderName(got.state))
	}
}

// ---------------------------------------------------------------------------
// The blue family beneath it is unchanged
// ---------------------------------------------------------------------------

// dead and degraded outrank dormant: each is a more specific true statement
// than "nothing is wired", and all three are blue so the color never moves.
func TestTheMoreSpecificBluesOutrankDormant(t *testing.T) {
	tests := []struct {
		name  string
		token string
		cause string
		want  frontendv1.RenderState
	}{
		{"the shim is gone", sigDead, causeSessionEnded, frontendv1.RenderState_RENDER_STATE_DEAD},
		{"the transport went quiet", sigDegraded, "connection_degraded", frontendv1.RenderState_RENDER_STATE_DEGRADED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — dormant is seeded LATER, so only rank can make it lose.
			db := newTestDB(t)
			seedSignal(t, db, "ws", "s1", tc.token, tc.cause, 1, 1)
			seedSignal(t, db, "ws", "", sigDormant, causeWired, -1, 2)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != tc.want {
				t.Fatalf("state = %s, want %s", renderName(got.state), renderName(tc.want))
			}
		})
	}
}

// ---------------------------------------------------------------------------
// The axis's own edges
// ---------------------------------------------------------------------------

// The wiring OPENS and the agent axis becomes visible in the same act.
func TestWiringOpensTheAgentAxis(t *testing.T) {
	// Arrange — a turn running behind a workspace nothing is wired to.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DORMANT {
		t.Fatalf("pre-wiring state = %s, want DORMANT", renderName(got))
	}
	// Act.
	if err := m.ApplyWired("ws1", WiringWired, "shim_ready"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING once the substrate is proven", renderName(got))
	}
}

// HIBERNATION closes it, and the turn the workspace was hibernated during stops
// being reported — which is the point: there is nobody to report it to.
func TestHibernationDropsAWiredWorkspaceToDormant(t *testing.T) {
	// Arrange — a wired, thinking workspace.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.ApplyWired("ws1", WiringDormant, "hibernated"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DORMANT {
		t.Fatalf("state = %s, want DORMANT after hibernation", renderName(got))
	}
}

// A ROTATION BOUNCE reports the gap honestly, and the re-handshake closes it.
func TestARotationBounceReopensOnTheReHandshake(t *testing.T) {
	// Arrange — a wired workspace mid-turn.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — the bounce, then the new ShimReady.
	if err := m.ApplyWired("ws1", WiringStarting, "session_rotating"); err != nil {
		t.Fatalf("bounce: %v", err)
	}
	bounced := mustCurrent(t, m, "ws1").GetState()
	if err := m.ApplyWired("ws1", WiringWired, "shim_ready"); err != nil {
		t.Fatalf("re-handshake: %v", err)
	}
	// Assert — blue across the window, and the agent axis back afterwards.
	if bounced != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("mid-bounce state = %s, want INIT (a re-handshake is a bring-up)", renderName(bounced))
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("post-bounce state = %s, want THINKING", renderName(got))
	}
}

// An edge that moves nothing appends nothing — and says so, because a bring-up
// that believes it is starting an already-starting workspace is worth seeing.
func TestARepeatedWiringIsALoudNoOp(t *testing.T) {
	// Arrange.
	m, cl, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyWired("ws1", WiringStarting, "bring_up"); err != nil {
		t.Fatalf("first: %v", err)
	}
	// Act.
	if err := m.ApplyWired("ws1", WiringStarting, "bring_up again"); err != nil {
		t.Fatalf("second: %v", err)
	}
	// Assert.
	if !cl.contains("wired axis unchanged") {
		t.Fatalf("a no-op wiring edge was not logged: %v", cl.lines)
	}
}

func TestApplyWiredRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	// Act.
	err := m.ApplyWired("", WiringWired, "shim_ready")
	// Assert.
	if err == nil {
		t.Fatal("ApplyWired with no workspace must fail loudly")
	}
}

// ---------------------------------------------------------------------------
// Boot
// ---------------------------------------------------------------------------

// A daemon that has just started has no shim connections at all, so every
// workspace its log restores is dormant however green the log reads.
func TestOpenMarksEveryRestoredWorkspaceDormant(t *testing.T) {
	// Arrange — two wired workspaces, one green and one mid-turn, persisted.
	path := filepath.Join(t.TempDir(), "state.db")
	res := fakeResolver{"s1": "wsA", "s2": "wsB"}
	first, err := Open(Options{DBPath: path, Resolver: res, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open 1: %v", err)
	}
	wireAll(t, first, res)
	if err := first.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := first.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act — the restart.
	cl := &capLog{}
	second, err := Open(Options{DBPath: path, Resolver: res, Logf: cl.logf})
	if err != nil {
		t.Fatalf("Open 2: %v", err)
	}
	t.Cleanup(func() { second.Close() })

	// Assert — both dormant, and the sweep named itself.
	for _, ws := range []string{"wsA", "wsB"} {
		if got := mustCurrent(t, second, ws).GetState(); got != frontendv1.RenderState_RENDER_STATE_DORMANT {
			t.Fatalf("%s = %s, want DORMANT after a daemon restart", ws, renderName(got))
		}
	}
	if !cl.contains("marked dormant at open") {
		t.Fatalf("the boot sweep was not logged: %v", cl.lines)
	}
}

// The sweep does not grow the log for a workspace already dormant: a daemon
// restarted twenty times must not accumulate twenty rows per workspace.
func TestTheBootSweepAppendsNothingForAnAlreadyDormantWorkspace(t *testing.T) {
	// Arrange — one restart has already made it dormant.
	path := filepath.Join(t.TempDir(), "state.db")
	res := fakeResolver{"s1": "ws1"}
	first, err := Open(Options{DBPath: path, Resolver: res, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open 1: %v", err)
	}
	wireAll(t, first, res)
	if err := first.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close 1: %v", err)
	}
	second, err := Open(Options{DBPath: path, Resolver: res, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open 2: %v", err)
	}
	before := wiredRowCount(t, second, "ws1")
	if err := second.Close(); err != nil {
		t.Fatalf("Close 2: %v", err)
	}

	// Act — a second restart over an already-dormant log.
	third, err := Open(Options{DBPath: path, Resolver: res, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open 3: %v", err)
	}
	t.Cleanup(func() { third.Close() })

	// Assert.
	if after := wiredRowCount(t, third, "ws1"); after != before {
		t.Fatalf("wired rows = %d, want %d — a restart must not grow the log for an already-dormant workspace", after, before)
	}
}

// wiredRowCount counts the workspace's rows on the WIRED axis.
func wiredRowCount(t *testing.T, m *Manager, ws string) int {
	t.Helper()
	var n int
	if err := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE workspace = ? AND state IN ('wired','starting','dormant')`,
		ws).Scan(&n); err != nil {
		t.Fatalf("count wired rows: %v", err)
	}
	return n
}
