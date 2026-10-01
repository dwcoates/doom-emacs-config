package footer

import (
	"context"
	"slices"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

// instant is the fixed clock every test starts from, so no assertion depends
// on wall-clock time.
var instant = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// testWS is the workspace every test resolves for.
const testWS = ids.WorkspaceID("ws-1")

// fakeClock is the injected clock: it never advances on its own, and a dwell
// fires only when a test advances past it. Nothing in these tests sleeps.
type fakeClock struct {
	mu      sync.Mutex
	now     time.Time
	pending []*fakeTimer
}

// fakeTimer is one scheduled dwell.
type fakeTimer struct {
	at      time.Time
	f       func()
	stopped bool
}

// Stop cancels the dwell.
func (t *fakeTimer) Stop() bool {
	t.stopped = true
	return true
}

// newFakeClock builds a clock pinned at instant.
func newFakeClock() *fakeClock { return &fakeClock{now: instant} }

// Now is the pinned instant.
func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

// AfterFunc records a dwell rather than running one.
func (c *fakeClock) AfterFunc(d time.Duration, f func()) Timer {
	c.mu.Lock()
	defer c.mu.Unlock()
	t := &fakeTimer{at: c.now.Add(d), f: f}
	c.pending = append(c.pending, t)
	return t
}

// Advance moves the clock and runs every dwell that came due, which is how a
// test observes the R1 successor push without waiting on anything.
func (c *fakeClock) Advance(d time.Duration) {
	c.mu.Lock()
	c.now = c.now.Add(d)
	due := make([]*fakeTimer, 0, len(c.pending))
	keep := c.pending[:0]
	for _, t := range c.pending {
		switch {
		case t.stopped:
		case !t.at.After(c.now):
			due = append(due, t)
		default:
			keep = append(keep, t)
		}
	}
	c.pending = keep
	c.mu.Unlock()
	for _, t := range due {
		t.f()
	}
}

// harness is one resolver under test with its clock and its log records.
type harness struct {
	r     *resolver
	clock *fakeClock
	log   *dlog.TestSurfaces
}

// testColors is the render-colors double: every footer_status arm painted,
// which is exactly what the resolver asserts. The values are irrelevant — the
// resolver reads the table's KEYS.
func testColors() vocab.RenderColors {
	status := map[string]string{}
	for _, arm := range statusArms {
		status[arm] = "grey"
	}
	return vocab.RenderColors{FooterStatus: status}
}

// newHarness builds a bound resolver on the fake clock.
func newHarness(t *testing.T, opts ...Option) *harness {
	t.Helper()
	clock := newFakeClock()
	log := dlog.NewTestSurfaces()
	all := append([]Option{
		WithClock(clock),
	}, opts...)
	r, err := newResolver(testColors(), log, all...)
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	if err := r.SetWorkspaceDir(testWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	return &harness{r: r, clock: clock, log: log}
}

// view is the workspace's last published view, failing when none exists.
func (h *harness) view(t *testing.T) *frontendv1.FooterView {
	t.Helper()
	got, ok := h.r.Topic(testWS).Latest()
	if !ok {
		t.Fatalf("no footer view was published")
	}
	return got
}

// status is the last published view's status arm name.
func (h *harness) status(t *testing.T) string {
	t.Helper()
	return statusName(h.view(t).GetStrip().GetStatus())
}

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange, Act
	_, err := New(testColors(), nil)

	// Assert
	if err == nil {
		t.Fatalf("New(nil) returned no error; a resolver that cannot log must not be built")
	}
}

func TestNothingIsPublishedBeforeTheFirstFact(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, ok := h.r.Topic(testWS).Latest()

	// Assert
	if ok {
		t.Fatalf("a view was published before any fact arrived; absence is the legal not-yet-resolved state")
	}
}

func TestTheFirstFactPublishesACompleteView(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	view := h.view(t)
	strip := view.GetStrip()
	if strip.GetStatus().GetStatus() == nil {
		t.Fatalf("the published status carries no arm")
	}
	if strip.GetTokens().GetInput() == nil {
		t.Fatalf("the tokens cell's figure is unset; the cell is always populated")
	}
	if view.GetExpanded().GetTokens() == nil || view.GetExpanded().GetCrons() == nil {
		t.Fatalf("a panel is missing; every panel ships on every push")
	}
}

func TestEveryTokensPanelLineIsAlwaysSet(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	panel := h.view(t).GetExpanded().GetTokens()
	switch {
	case panel.GetInput() == nil, panel.GetCacheRead() == nil, panel.GetCacheWrite() == nil,
		panel.GetOutput() == nil, panel.GetThinking() == nil, panel.GetFirstToken() == nil:
		t.Fatalf("panel = %+v, want all six lines present with empty slots", panel)
	}
}

func TestAnIdenticalRepublishIsDeduplicated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	first := h.view(t)

	// Act
	connected(h)

	// Assert
	if h.view(t) != first {
		t.Fatalf("an identical view was republished; the topic deduplicates by proto.Equal")
	}
}

func TestTheClockCarriesTheTurnStartInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	got := h.view(t).GetStrip().GetClock()
	if got.TurnStartedAtMs == nil || *got.TurnStartedAtMs != instant.UnixMilli() {
		t.Fatalf("clock = %+v, want the turn's start instant", got)
	}
}

func TestTheClockIsUnsetWithNoTurnInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.SetTurn(testWS, nil)

	// Assert
	if h.view(t).GetStrip().GetClock().TurnStartedAtMs != nil {
		t.Fatalf("the clock carries an instant with no turn in flight")
	}
}

func TestAFrameForAnUnboundWorkspaceIsRecordedLoudly(t *testing.T) {
	// Arrange
	log := dlog.NewTestSurfaces()
	r, err := newResolver(testColors(), log, WithClock(newFakeClock()))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act
	r.OnLink("ws-unbound", shimclient.LinkConnected)

	// Assert
	records := log.Records()
	if len(records) == 0 {
		t.Fatalf("no record was written for a frame on an unbound workspace")
	}
	if records[len(records)-1].Context["invariant_violation"] == nil {
		t.Fatalf("record = %+v, want the invariant violation named", records[len(records)-1])
	}
}

func TestAContextCutEndsTheCompaction(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
	})

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: the cut record is the compaction's only end signal", got)
	}
}

func TestAContextCutEndsTheClear(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActClear})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestAFailedCompactionDrawsItsAccountAsEvidence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "summary model refused"},
		},
	})

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	text := idle.GetActivity().GetSalient().GetContextBudget().GetText()
	if text == "" || !contains(text, "summary model refused") {
		t.Fatalf("activity text = %q, want the producer's account", text)
	}
}

func TestAFailedCompactionIsRecordedAtWarn(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "boom"},
		},
	})

	// Assert
	if !hasLevel(h.log.Records(), dlog.LevelWarn, "daemon.footer.on_context_cut") {
		t.Fatalf("records = %+v, want a WARN for the failed compaction", h.log.Records())
	}
}

// contains reports whether needle appears in haystack.
func contains(haystack, needle string) bool {
	if len(needle) > len(haystack) {
		return false
	}
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return true
		}
	}
	return false
}

// hasLevel reports whether any record matches the level and operation.
// TestTheStatusArmIsRecordedWhenItChanges — a later disagreement between the
// strip and the roster is diagnosed from the log, so every MOVE of the
// published arm is recorded once, with the arm it replaced.
func TestTheStatusArmIsRecordedWhenItChanges(t *testing.T) {
	// Arrange: the first published view settles on idle.
	h := newHarness(t)
	connected(h)

	// Act: detached work raises background, and the authoritative set retires it.
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", "agent-2", "Explore"))
	h.r.OnLiveWorkChanged(testWS, LiveWorkSet{})

	// Assert
	if got := armChanges(h.log.Records()); !slices.Equal(got, []string{"idle", "background", "idle"}) {
		t.Fatalf("recorded arms = %v, want idle, background, idle", got)
	}
}

// TestAContextCutRecordsTheArmItMoves — the cut ends the turn, so it moves the
// published arm, and that move is recorded like every other one.
func TestAContextCutRecordsTheArmItMoves(t *testing.T) {
	// Arrange: a turn is in flight.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
	})

	// Assert
	if got := armChanges(h.log.Records()); !slices.Equal(got, []string{"idle", "working", "idle"}) {
		t.Fatalf("recorded arms = %v, want idle, working, idle", got)
	}
}

// TestADaemonScopedFaultRecordsTheArmItMoves — a daemon-scoped fault moves
// every strip's arm through the resolver-wide path, and that move is recorded
// like one made through a workspace's own.
func TestADaemonScopedFaultRecordsTheArmItMoves(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindPromptsDirMissing, true))

	// Assert
	if got := armChanges(h.log.Records()); !slices.Equal(got, []string{"idle", "blocked"}) {
		t.Fatalf("recorded arms = %v, want idle, blocked", got)
	}
}

// TestAnUnchangedStatusArmIsNotRecordedAgain keeps the record a record of
// CHANGES: a push that leaves the arm where it stands writes nothing.
func TestAnUnchangedStatusArmIsNotRecordedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a second fact that leaves the strip idle.
	h.r.SetParked(testWS, false)

	// Assert
	if got := countOf(h.log.Records(), dlog.LevelInfo, "daemon.footer.status_arm_changed"); got != 1 {
		t.Fatalf("arm-change records = %d, want the one for the first published arm", got)
	}
}

// armChanges is every recorded arm, in the order the footer published them.
func armChanges(records []dlog.Record) []string {
	var out []string
	for _, rec := range records {
		if rec.Operation != "daemon.footer.status_arm_changed" {
			continue
		}
		arm, _ := rec.Context["arm"].(string)
		out = append(out, arm)
	}
	return out
}

func hasLevel(records []dlog.Record, level, operation string) bool {
	for _, rec := range records {
		if rec.Level == level && rec.Operation == operation {
			return true
		}
	}
	return false
}

// ---- shared frame builders ------------------------------------------------
// The whole package's tests build frames from these, so a contract change
// reaches every test through one place.

// mainAgent is the main-thread agent every frame is attributed to.
var mainAgent = &conversationv1.AgentId{Value: "agent-main"}

// testTurnID is the turn every terminal names.
const testTurnID = ids.TurnID("turn-1")

// completed is an ordinary agent completion.
func completed() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}
}

// interruptedByUserStop is a user-commanded stop the agent acknowledged.
func interruptedByUserStop() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{
			Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{
					ByUser: &conversationv1.AgentInterruptedByUser{},
				},
			},
		},
	}
}

// interruptedByHostDown is a stop nobody chose: the host went down under the
// agent.
func interruptedByHostDown() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{
			Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_HostShutdown{
					HostShutdown: &conversationv1.AgentInterruptedByHostShutdown{},
				},
			},
		},
	}
}

// thinkingActivity is one reasoning frame, the cheapest way to make a turn
// look like it started producing.
func thinkingActivity(unit string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Thinking{
			Thinking: &conversationv1.AgentThinking{
				Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}},
			},
		},
	}
}

// wakeupScheduled is a pending self-scheduled wakeup.
func wakeupScheduled(at time.Time) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "wake-1"},
		Item: &conversationv1.AgentActivity_ScheduleWakeup{
			ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
				Result: &conversationv1.AgentScheduleWakeup_Success{
					Success: &conversationv1.AgentScheduleWakeupSuccess{
						Outcome: &conversationv1.AgentScheduleWakeupSuccess_Scheduled{
							Scheduled: &conversationv1.AgentScheduleWakeupScheduled{WakeAtMs: at.UnixMilli()},
						},
					},
				},
			},
		},
	}
}

// createdShell is a shell announced already detached.
func createdShell(work, command string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Owner: mainAgent,
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{
			Created: &conversationv1.DetachedWorkCreated{
				WorkCreated: &conversationv1.DetachableWork{
					Work: &conversationv1.DetachableWork_Bash{
						Bash: &conversationv1.AgentBash{
							Result: &conversationv1.AgentBash_Start{
								Start: &conversationv1.AgentBashStart{
									Command:   &conversationv1.AgentBashCommand{Line: command},
									StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: instant.UnixMilli()},
								},
							},
						},
					},
				},
			},
		},
	}
}

// ---- the R1 dwell ---------------------------------------------------------

func TestTheMomentaryInterruptedStatusIsRetiredByTheDwell(t *testing.T) {
	// Arrange
	h := newHarness(t, WithMomentaryDwell(time.Second))
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)
	if got := h.status(t); got != "interrupted" {
		t.Fatalf("status = %q, want interrupted before the dwell elapses", got)
	}

	// Act
	h.clock.Advance(time.Second)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want the successor once the dwell elapsed", got)
	}
}

func TestTheDwellDoesNotFireEarly(t *testing.T) {
	// Arrange
	h := newHarness(t, WithMomentaryDwell(time.Second))
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)

	// Act
	h.clock.Advance(999 * time.Millisecond)

	// Assert
	if got := h.status(t); got != "interrupted" {
		t.Fatalf("status = %q, want interrupted until the dwell fully elapses", got)
	}
}

func TestAHostShutdownIsDrawnAsItsOwnInterruptedStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByHostDown(), nil)

	// Assert
	interrupted := h.view(t).GetStrip().GetStatus().GetInterrupted()
	if interrupted.GetHostShutdown() == nil {
		t.Fatalf("substatus = %+v, want host_shutdown", interrupted.GetSubstatus())
	}
}

func TestAnUnstatedInterruptCauseReadsAsTheUserStop(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{
			Interrupted: &conversationv1.AgentInterrupted{},
		},
	}, nil)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetInterrupted().GetByUser() == nil {
		t.Fatalf("want the ordinary user stop when the producer stated no cause")
	}
}

func TestANewTurnSupersedesAStandingDwell(t *testing.T) {
	// Arrange
	h := newHarness(t, WithMomentaryDwell(time.Second))
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.clock.Advance(time.Second)

	// Assert
	if got := h.status(t); got != "working" {
		t.Fatalf("status = %q, want the new turn to survive the cancelled dwell", got)
	}
}

func TestTheDwellRetiresTheMomentaryLoadingStatus(t *testing.T) {
	// Arrange
	h := newHarness(t, WithMomentaryDwell(time.Second))
	connected(h)
	h.r.OnActivity(testWS, mainAgent, memoryInjection("CLAUDE.md"))
	if got := h.status(t); got != "loading" {
		t.Fatalf("status = %q, want loading before the dwell elapses", got)
	}

	// Act
	h.clock.Advance(time.Second)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want the successor once the dwell elapsed", got)
	}
}

func TestTheTurnOpenEdgeRaisesSubmittingBeforeAnyFrame(t *testing.T) {
	// Arrange: the edge comes from the session watcher, which only exists
	// over a route that has been seen; a turn on a route NEVER seen is the
	// bring-up window instead (ladder.AwaitingBringUp).
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnTurnOpened(testWS, "turn-1")

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetWorking()
	if got.GetSubmitting() == nil {
		t.Fatalf("status = %v, want working.submitting on the turn-open edge", got)
	}
}

func TestTheTurnOpenEdgeStartsTheStripsClock(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnTurnOpened(testWS, "turn-1")

	// Assert
	clock := h.view(t).GetStrip().GetClock()
	if clock.TurnStartedAtMs == nil || *clock.TurnStartedAtMs != h.clock.Now().UnixMilli() {
		t.Fatalf("clock = %+v, want the turn-open instant", clock)
	}
}

func TestTheTurnOpenEdgeKeepsAnAlreadyInstalledAct(t *testing.T) {
	// Arrange: the daemon named the act (a compaction) before the shim
	// answered StartTurn, over a route that has been seen.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnTurnOpened(testWS, "turn-1")

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetWorking()
	if got.GetCompacting() == nil {
		t.Fatalf("status = %v, want working.compacting: the edge must not demote a named act", got)
	}
}

// TestNewRefusesAFooterStatusTableMissingAnArm pins that an arm the resolver
// emits with no color refuses the build rather than drawing unpainted.
func TestNewRefusesAFooterStatusTableMissingAnArm(t *testing.T) {
	// Arrange.
	colors := testColors()
	delete(colors.FooterStatus, "working")

	// Act.
	_, err := New(colors, dlog.NewTestSurfaces())

	// Assert.
	if err == nil {
		t.Fatal("New accepted a footer_status table with no color for the working arm")
	}
}

// TestNewRefusesASurplusFooterStatusRow pins the other direction: a
// footer_status row naming no arm the resolver can emit is a state the
// vocabulary paints and the footer can never reach.
func TestNewRefusesASurplusFooterStatusRow(t *testing.T) {
	// Arrange.
	colors := testColors()
	colors.FooterStatus["daydreaming"] = "grey"

	// Act.
	_, err := New(colors, dlog.NewTestSurfaces())

	// Assert.
	if err == nil {
		t.Fatal("New accepted a footer_status row naming no FooterStatus.status arm")
	}
}

// ---- Prime: the per-workspace footer topic re-primes on reconnect ----------

// TestPrimePublishesAViewWithoutAnyLiveFact is the idle-session-after-restart
// case: a daemon restart rebuilds the resolver empty, and an idle session
// produces no fresh live edge, so without a register-time prime the footer
// topic would hold nothing. Prime alone must publish a current view.
func TestPrimePublishesAViewWithoutAnyLiveFact(t *testing.T) {
	// Arrange: a workspace bound at registration but with no session fact yet.
	h := newHarness(t)
	if _, ok := h.r.Topic(testWS).Latest(); ok {
		t.Fatalf("a view stood before Prime; the arrange assumed an empty topic")
	}

	// Act.
	h.r.Prime(testWS)

	// Assert: the topic now holds a current, idle view for a subscriber to replay.
	if h.status(t) != "idle" {
		t.Fatalf("primed status = %q, want idle for a workspace with no session fact", h.status(t))
	}
}

// TestPrimedViewIsAReconnectingSubscribersFirstDelivery proves the reconnect
// case end to end: a subscriber that arrives after the prime — the client that
// reconnected to the daemon — receives the primed view as its first delivery
// rather than staying quiet.
func TestPrimedViewIsAReconnectingSubscribersFirstDelivery(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.Prime(testWS)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	got := <-h.r.Topic(testWS).Subscribe(ctx)

	// Assert.
	if got.GetStrip().GetStatus().GetStatus() == nil {
		t.Fatalf("a reconnecting subscriber's first delivery carried no status arm")
	}
}

// TestPrimedViewIsComplete pins the completeness contract for a view primed
// from an empty accumulation: nothing partial is shipped even when no session
// fact has been observed.
func TestPrimedViewIsComplete(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.Prime(testWS)

	// Assert.
	view := h.view(t)
	strip := view.GetStrip()
	switch {
	case strip.GetStatus().GetStatus() == nil:
		t.Fatalf("the primed status carries no arm")
	case strip.GetTokens().GetInput() == nil:
		t.Fatalf("the primed tokens cell is unpopulated")
	case view.GetExpanded().GetTokens() == nil || view.GetExpanded().GetCrons() == nil:
		t.Fatalf("the primed view is missing a panel")
	}
}

// TestPrimeReflectsAccumulatedFactsRatherThanIdle is the idle-session-WITH-its-
// facts case: when the resolver already holds live facts, a prime republishes
// the CURRENT view built from them, never a blank or default one.
func TestPrimeReflectsAccumulatedFactsRatherThanIdle(t *testing.T) {
	// Arrange: a turn in flight is a fact idle would erase if Prime rebuilt
	// from scratch.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: h.clock.Now(), Act: ActPrompt})
	if h.status(t) != "working" {
		t.Fatalf("arrange status = %q, want working", h.status(t))
	}

	// Act.
	h.r.Prime(testWS)

	// Assert: the prime kept the accumulated status rather than resetting it.
	if h.status(t) != "working" {
		t.Fatalf("primed status = %q, want the accumulated thinking view", h.status(t))
	}
}

// TestPrimeOnALiveFooterIsDeduplicated guards against a spurious repaint: a
// prime that renders the same view the topic already holds must not reach a
// subscriber a second time (the topic's value dedup drops it).
func TestPrimeOnALiveFooterIsDeduplicated(t *testing.T) {
	// Arrange: a standing view, then a subscriber caught up to it.
	h := newHarness(t)
	connected(h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	sub := h.r.Topic(testWS).Subscribe(ctx)
	<-sub // the latest replayed on subscribe

	// Act: prime with no state change.
	h.r.Prime(testWS)

	// Assert: nothing new is delivered — a second, identical view would be a
	// spurious repaint.
	select {
	case extra := <-sub:
		t.Fatalf("Prime delivered a duplicate view %+v; an identical re-render must dedup", extra)
	default:
	}
}

// lineChanges is every recorded activity-line change, in publication order.
func lineChanges(records []dlog.Record) []dlog.Record {
	var out []dlog.Record
	for _, rec := range records {
		if rec.Operation == "daemon.footer.activity_line_changed" {
			out = append(out, rec)
		}
	}
	return out
}

// vendorCompacting is the vendor's own auto-compaction start signal.
func vendorCompacting() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	}
}

func TestTheActivityLineIsRecordedWhenItChanges(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
		want map[string]any
	}{
		{
			name: "a line set",
			act:  func(h *harness) { h.r.OnSessionUpdate(testWS, vendorCompacting()) },
			want: map[string]any{
				"arm": "working", "kind": "salient.compaction", "text": "compacting the context…",
				"previous_kind": "enduring", "previous_text": "",
				"cause": "daemon.footer.on_session_update",
			},
		},
		{
			name: "a line cleared",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, vendorCompacting())
				h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
					Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
				})
			},
			want: map[string]any{
				"arm": "idle", "kind": "enduring", "text": "",
				"previous_kind": "salient.compaction", "previous_text": "compacting the context…",
				"cause": "daemon.footer.on_context_cut",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

			// Act
			tt.act(h)

			// Assert
			changes := lineChanges(h.log.Records())
			if len(changes) == 0 {
				t.Fatalf("no activity-line change was recorded")
			}
			last := changes[len(changes)-1]
			if last.Level != dlog.LevelInfo {
				t.Fatalf("level = %q, want INFO", last.Level)
			}
			for key, want := range tt.want {
				if got := last.Context[key]; got != want {
					t.Fatalf("%s = %v, want %v (record %+v)", key, got, want, last.Context)
				}
			}
		})
	}
}

func TestAnUnchangedActivityLineIsNotRecordedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())
	before := len(lineChanges(h.log.Records()))

	// Act: a fact that leaves the line where it stands.
	h.r.SetParked(testWS, false)

	// Assert
	if got := len(lineChanges(h.log.Records())); got != before {
		t.Fatalf("activity-line records = %d, want %d: nothing changed", got, before)
	}
}

func TestADaemonScopedFaultRecordsTheActivityLineItStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a daemon-scoped fact reaches the strip through the resolver-wide path.
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindPromptsDirMissing, true))

	// Assert
	changes := lineChanges(h.log.Records())
	if len(changes) == 0 {
		t.Fatalf("no activity-line change was recorded for the daemon fault")
	}
	last := changes[len(changes)-1]
	if last.Context["cause"] != "daemon.footer.open_fault" || last.Context["kind"] == "none" {
		t.Fatalf("record = %+v, want the fault's line caused by open_fault", last.Context)
	}
}

// TestTheTurnOpenEdgeKeepsTheUsageOfFramesThatBeatIt is the forced
// interleaving the daemon integration suite lost at random: the shim streams
// the turn's first API response before StartTurn's answer is back, so its
// usage-carrying unit is folded in between SetTurn and the turn-open edge.
func TestTheTurnOpenEdgeKeepsTheUsageOfFramesThatBeatIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, thinkingFrame("unit-0", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnTurnOpened(testWS, turn)
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", nil))
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("verdict = %+v, want complete: the edge wiped the usage a frame that beat it had carried",
			h.view(t).GetStrip().GetTokens().GetVerdict())
	}
}

// TestTheTurnOpenEdgeKeepsTheActivityAFrameAlreadyShowed: the status does not
// fall back to `submitting` for a turn whose first frame has already landed.
func TestTheTurnOpenEdgeKeepsTheActivityAFrameAlreadyShowed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, thinkingFrame("unit-0", nil))

	// Act
	h.r.OnTurnOpened(testWS, testTurnID)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetWorking().GetSubmitting() != nil {
		t.Fatalf("status = %v, want the activity the frame showed kept, not thinking.submitting", h.view(t).GetStrip().GetStatus())
	}
}

// unboundErrors counts the ERROR records stating an unbound-workspace
// invariant violation.
func unboundErrors(log *dlog.TestSurfaces, operation string) int {
	n := 0
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == operation {
			n++
		}
	}
	return n
}

// TestAFrameForAnUnboundWorkspaceStatesTheViolationAtError pins that the
// record's level matches what it claims: the violation used to ride as context
// on INFO and DEBUG records only, where no level sweep could see it.
func TestAFrameForAnUnboundWorkspaceStatesTheViolationAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestSurfaces()
	r, err := newResolver(testColors(), log, WithClock(newFakeClock()))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act
	r.OnLink("ws-unbound", shimclient.LinkConnected)

	// Assert
	if got := unboundErrors(log, "daemon.footer.unbound_workspace"); got != 1 {
		t.Fatalf("unbound-workspace ERROR records = %d, want 1", got)
	}
}

func TestTheUnboundViolationIsStatedOncePerWorkspace(t *testing.T) {
	// Arrange
	log := dlog.NewTestSurfaces()
	r, err := newResolver(testColors(), log, WithClock(newFakeClock()))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	r.OnLink("ws-unbound", shimclient.LinkConnected)

	// Act
	r.OnLink("ws-unbound", shimclient.LinkDialing)

	// Assert
	if got := unboundErrors(log, "daemon.footer.unbound_workspace"); got != 1 {
		t.Fatalf("unbound-workspace ERROR records = %d, want 1 across two frames", got)
	}
}

func TestRetryStandingWhileTheVendorRetriesTheTurnsCall(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "ENOTFOUND"})

	// Act
	got := h.r.RetryStanding(testWS)

	// Assert
	if !got {
		t.Fatalf("RetryStanding = false, want true while the vendor retries")
	}
}

func TestRetryStandingIsFalseForAnUnseenWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := h.r.RetryStanding(ids.WorkspaceID("never-seen"))

	// Assert
	if got {
		t.Fatalf("RetryStanding = true for a workspace the footer never saw")
	}
}

func TestRetryStandingEndsWhenTheRetriedCallIsAnswered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "ENOTFOUND"})
	h.r.OnActivity(testWS, mainAgent, thinkingActivity("th-1"))

	// Act
	got := h.r.RetryStanding(testWS)

	// Assert
	if got {
		t.Fatalf("RetryStanding = true after the retried call was answered")
	}
}
