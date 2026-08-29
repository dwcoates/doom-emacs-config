package footer

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
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

// fakeEncode is the injected FeedId encoder: feedid.Encode is a peer leaf and
// its landing is not this package's to wait on, so the tests assert the ROW
// KEY the resolver asked for rather than the bytes the encoder produces.
func fakeEncode(ref feedid.Ref) *frontendv1.FeedId {
	return &frontendv1.FeedId{Value: string(ref.Row.Kind) + "|" + ref.Row.ID + "|" + ref.Row.Sub}
}

// harness is one resolver under test with its clock and its log records.
type harness struct {
	r     *resolver
	clock *fakeClock
	log   *dlog.TestSurfaces
}

// newHarness builds a bound resolver on the fake clock.
func newHarness(t *testing.T, opts ...Option) *harness {
	t.Helper()
	clock := newFakeClock()
	log := dlog.NewTestSurfaces()
	all := append([]Option{
		WithClock(clock),
		WithFeedIDEncoder(fakeEncode),
	}, opts...)
	r, err := newResolver(log, all...)
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
	_, err := New(nil)

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
	h.r.OnLink(testWS, shimclient.LinkConnected)

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
	h.r.OnLink(testWS, shimclient.LinkConnected)

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
	h.r.OnLink(testWS, shimclient.LinkConnected)
	first := h.view(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

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
	r, err := newResolver(log, WithClock(newFakeClock()))
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
	h.r.OnContextCut(testWS, &conversationv1.ContextCut{
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
	h.r.OnContextCut(testWS, &conversationv1.ContextCut{
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
	h.r.OnContextCut(testWS, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "summary model refused"},
		},
	})

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	text := idle.GetActivity().GetContextBudget().GetText()
	if text == "" || !contains(text, "summary model refused") {
		t.Fatalf("activity text = %q, want the producer's account", text)
	}
}

func TestAFailedCompactionIsRecordedAtWarn(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnContextCut(testWS, &conversationv1.ContextCut{
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
		Work: &conversationv1.DetachedWorkId{Value: work},
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
