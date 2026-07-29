package ssm

import (
	"database/sql"
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
			seedSignal(t, db, "ws", "", sigSevered, causeWired, -1, 1)
			seedSignal(t, db, "ws", "s1", tc.token, tc.cause, 1, 5)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != frontendv1.RenderState_RENDER_STATE_SEVERED {
				t.Fatalf("state = %s, want SEVERED — an unwired workspace may report nothing about the agent", renderName(got.state))
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
	seedSignal(t, db, "ws", "", sigSevered, causeWired, -1, 1)
	seedSignal(t, db, "ws", "s1", sigIdle, causeSessionStarted, 1, 2)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 3, "a1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("state = %s, want SEVERED", renderName(got.state))
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

// Not wired, not starting, and BROKEN is SEVERED, its own word rather than
// INIT's.
func TestSeveredResolvesItsOwnState(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.ApplyWired("ws1", WiringSevered, "bring_up_failed"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("state = %s, want SEVERED", renderName(got))
	}
}

// A workspace with agent history and NO wired row at all is HIBERNATED, not
// "unknown" and not severed: the log carries no evidence of a live session
// behind it, and equally none of anything having broken. Nothing was ever wired
// here, so the honest answer is teal — before the split this workspace stood
// there in blue accusing the local substrate of a fault it never had.
func TestAnAbsentWiredAxisResolvesHibernated(t *testing.T) {
	// Arrange — a workspace whose log predates the axis entirely.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s1", sigDone, causeTurnEnded, 1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("state = %s, want HIBERNATED for a workspace with no wired row", renderName(got.state))
	}
}

// The synthesized hibernated candidate still names the conversation it is about,
// so a frontend is not handed a state with no session id.
func TestTheSynthesizedHibernatedCandidateKeepsTheSessionID(t *testing.T) {
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
// synthesized hibernated candidate rides the agent axis, so a workspace that has
// never had one stays unborn rather than becoming a sleeping tab.
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

// dead and degraded outrank severed: each is a more specific true statement
// than "nothing is wired", and all three are blue so the color never moves.
func TestTheMoreSpecificBluesOutrankSevered(t *testing.T) {
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
			// Arrange — severed is seeded LATER, so only rank can make it lose.
			db := newTestDB(t)
			seedSignal(t, db, "ws", "s1", tc.token, tc.cause, 1, 1)
			seedSignal(t, db, "ws", "", sigSevered, causeWired, -1, 2)
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
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("pre-wiring state = %s, want HIBERNATED", renderName(got))
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
func TestHibernationDropsAWiredWorkspaceToHibernated(t *testing.T) {
	// Arrange — a wired, thinking workspace.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.ApplyWired("ws1", WiringHibernated, "hibernated"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("state = %s, want HIBERNATED after hibernation", renderName(got))
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
// workspace its log restores is asleep however green the log reads.
//
// It is HIBERNATED and not severed: a daemon that has just booted is not a
// broken one, and the bootsweep is about to reattach every shim that survived
// the bounce. Marking the whole tab-bar blue on every restart is what spent
// blue's meaning in the first place.
func TestOpenMarksEveryRestoredWorkspaceHibernated(t *testing.T) {
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

	// Assert — both asleep, and the sweep named itself.
	for _, ws := range []string{"wsA", "wsB"} {
		if got := mustCurrent(t, second, ws).GetState(); got != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
			t.Fatalf("%s = %s, want HIBERNATED after a daemon restart", ws, renderName(got))
		}
	}
	if !cl.contains("marked hibernated at open") {
		t.Fatalf("the boot sweep was not logged: %v", cl.lines)
	}
}

// The sweep does not grow the log for a workspace already asleep: a daemon
// restarted twenty times must not accumulate twenty rows per workspace.
func TestTheBootSweepAppendsNothingForAnAlreadyAsleepWorkspace(t *testing.T) {
	// Arrange — one restart has already put it to sleep.
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

	// Act — a second restart over an already-asleep log.
	third, err := Open(Options{DBPath: path, Resolver: res, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open 3: %v", err)
	}
	t.Cleanup(func() { third.Close() })

	// Assert.
	if after := wiredRowCount(t, third, "ws1"); after != before {
		t.Fatalf("wired rows = %d, want %d — a restart must not grow the log for an already-asleep workspace", after, before)
	}
}

// wiredRowCount counts the workspace's rows on the WIRED axis.
func wiredRowCount(t *testing.T, m *Manager, ws string) int {
	t.Helper()
	var n int
	if err := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE workspace = ? AND state IN ('wired','starting','severed','hibernated','dormant')`,
		ws).Scan(&n); err != nil {
		t.Fatalf("count wired rows: %v", err)
	}
	return n
}

// ---------------------------------------------------------------------------
// The closed half is TWO states, and the legacy spelling of it is a third
// ---------------------------------------------------------------------------

// A hibernation and a severance both close the axis, and they must never render
// alike. This is the whole reason the split exists: one token used to mean both
// "we put this session to sleep on purpose to reclaim its ~500MB" and "the
// backend substrate is broken", so the most ordinary event in the system was
// indistinguishable from a dead shim.
func TestTheTwoClosedHalvesResolveToDifferentStates(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedSignal(t, db, "sleeping", "", sigHibernated, causeWired, -1, 1)
	seedSignal(t, db, "broken", "", sigSevered, causeWired, -1, 1)
	// Act.
	sleeping, err := resolve(db, "sleeping", nil)
	if err != nil {
		t.Fatalf("resolve sleeping: %v", err)
	}
	broken, err := resolve(db, "broken", nil)
	if err != nil {
		t.Fatalf("resolve broken: %v", err)
	}
	// Assert.
	if sleeping.state == broken.state {
		t.Fatalf("both closed halves resolve %s; the split exists precisely so they differ", renderName(sleeping.state))
	}
	if sleeping.state != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("hibernated resolves %s, want HIBERNATED", renderName(sleeping.state))
	}
	if broken.state != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("severed resolves %s, want SEVERED", renderName(broken.state))
	}
}

// `workspace_state` is APPEND-ONLY and rows written before the split literally
// contain the text `dormant`. That spelling must resolve FOREVER, and it must
// resolve to SEVERED: a pre-split row says only that the axis was closed, and
// reading it as benign would claim more than the row knows.
func TestALegacyDormantRowStillResolvesSevered(t *testing.T) {
	// Arrange — a log written by a daemon that predates the split.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "", "dormant", causeWired, -1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("state = %s, want SEVERED for a pre-split 'dormant' row", renderName(got.state))
	}
}

// The legacy row must keep OUTRANKING the agent axis too. If it were dropped
// from the rank table it would stop being a candidate at all, and the workspace
// would resolve off whatever its last turn reported — a green or red tab for a
// session nothing is connected to, which is the one thing the connection-truth
// law forbids.
func TestALegacyDormantRowStillHidesTheAgentAxis(t *testing.T) {
	// Arrange — the legacy row is OLDER than the agent row, so only RANK can
	// make it win.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "", "dormant", causeWired, -1, 1)
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 5)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("state = %s, want SEVERED — a legacy closed axis may report nothing about the agent", renderName(got.state))
	}
}

// A legacy `dormant` row is still the TOP of the axis, so a hibernation landing
// on top of it must actually append. Were the legacy spelling missing from
// wiredAxisTop's IN-list, that read would answer "" and applyWiredLocked would
// believe the axis had never moved.
func TestALegacyDormantRowIsVisibleToTheAxisTopRead(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := appendRow(m.db, "ws1", "", "dormant", causeWired, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		t.Fatalf("seed a legacy row: %v", err)
	}
	// Act.
	top, err := wiredAxisTop(m.db, "ws1")
	// Assert.
	if err != nil {
		t.Fatalf("wiredAxisTop: %v", err)
	}
	if top != "dormant" {
		t.Fatalf("wiredAxisTop = %q, want %q — the pre-split spelling must stay visible to the axis read", top, "dormant")
	}
}

// TEAL'S RANK IS THE LOAD-BEARING PART OF THE SPLIT. `hibernated` sits at 15,
// which is ABOVE green, so a stale `thinking` row from the turn a workspace was
// hibernated after cannot mask a workspace that is genuinely asleep. Ranking
// teal below green — the tempting reading of "benign" — would do exactly that.
func TestHibernatedOutranksAStaleAgentRow(t *testing.T) {
	// Arrange — the hibernation is OLDER than the thinking row, so only rank can
	// make it win.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "", sigHibernated, causeWired, -1, 1)
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 5)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("state = %s, want HIBERNATED — an asleep workspace may report nothing about the agent", renderName(got.state))
	}
}

// And it ranks BELOW the blue band, which is what keeps a real fault visible: a
// workspace that is both asleep and degraded has something to act on, and teal
// says the opposite.
func TestTheBlueBandOutranksHibernated(t *testing.T) {
	// Arrange — the hibernation is seeded LATER, so only rank can make it lose.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s1", sigDegraded, "connection_degraded", 1, 1)
	seedSignal(t, db, "ws", "", sigHibernated, causeWired, -1, 2)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_DEGRADED {
		t.Fatalf("state = %s, want DEGRADED — a real fault outranks a benign sleep", renderName(got.state))
	}
}
