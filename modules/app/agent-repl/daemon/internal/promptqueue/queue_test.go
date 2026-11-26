package promptqueue

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lockwatch"
	"agent-repld/internal/wsm"
)

func TestNewRefusesEachMissingCollaborator(t *testing.T) {
	// Arrange: one complete set of dependencies, then each one blanked.
	full := func() Deps {
		h := &harness{db: newFakeDB(), sender: newFakeSender(), watcher: &fakeWatcher{},
			feed: &fakeFeed{}, footer: &fakeFooter{}, sidebar: &fakeSidebar{}, holds: &fakeHolds{}, judge: &scriptedJudge{}}
		return Deps{
			DB: h.db, Judge: h.judge, Network: &fakeNetwork{}, Feed: h.feed, Footer: h.footer, Sidebar: h.sidebar, Holds: h.holds,
			Edges:       &fakeEdges{},
			Leases:      &fakeLeases{},
			Merges:      &fakeLeases{},
			Submissions: &fakeSubmissions{},
			TurnBanners: &fakeTurnBanners{},
			Commits:     &fakeRecounter{},
			CacheClocks: &fakeCacheClocks{},
			Client:      func(ids.WorkspaceID) (Sender, bool) { return h.sender, true },
			Watcher:     func(ids.WorkspaceID) (Watcher, bool) { return h.watcher, true },
			ResolveImage: func(*conversationv1.ImageBlock) (string, string, error) {
				return "src", "alt", nil
			},
			Log: dlog.NewTestSurfaces(),
		}
	}
	tests := []struct {
		name  string
		blank func(*Deps)
	}{
		{"log surfaces", func(d *Deps) { d.Log = nil }},
		{"state client", func(d *Deps) { d.DB = nil }},
		{"classifier", func(d *Deps) { d.Judge = nil }},
		{"feed resolver", func(d *Deps) { d.Feed = nil }},
		{"turn banners", func(d *Deps) { d.TurnBanners = nil }},
		{"commit recounter", func(d *Deps) { d.Commits = nil }},
		{"keep-alive keeper's cache clocks", func(d *Deps) { d.CacheClocks = nil }},
		{"footer resolver", func(d *Deps) { d.Footer = nil }},
		{"network's state", func(d *Deps) { d.Network = nil }},
		{"roster resolver", func(d *Deps) { d.Sidebar = nil }},
		{"turn edges' observer", func(d *Deps) { d.Edges = nil }},
		{"lease watcher", func(d *Deps) { d.Leases = nil }},
		{"merge requests' record", func(d *Deps) { d.Merges = nil }},
		{"submissions' telemetry", func(d *Deps) { d.Submissions = nil }},
		{"holds resolver", func(d *Deps) { d.Holds = nil }},
		{"client resolver", func(d *Deps) { d.Client = nil }},
		{"watcher resolver", func(d *Deps) { d.Watcher = nil }},
		{"image resolver", func(d *Deps) { d.ResolveImage = nil }},
		{"stand-down count", func(d *Deps) { d.StandDowns = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.blank(&deps)
			// Act
			_, err := New(deps)
			// Assert
			if err == nil {
				t.Fatalf("New with no %s must refuse", tc.name)
			}
		})
	}
}

func TestNewAcceptsACompleteWiring(t *testing.T) {
	// Arrange / Act
	h := newHarness(t)
	// Assert
	if h.q == nil {
		t.Fatal("a complete wiring must build a queue")
	}
}

func TestSaidTextJoinsEveryTextBlock(t *testing.T) {
	// Arrange
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "first"}}},
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "second"}}},
		},
	}}
	// Act
	got := saidText(said)
	// Assert
	if got != "first\nsecond" {
		t.Fatalf("saidText = %q, want both blocks joined", got)
	}
}

func TestDispositionParkedReadsTheThreeWayAnswer(t *testing.T) {
	tests := []struct {
		name string
		d    Disposition
		want bool
	}{
		{"delivered", Disposition{Delivered: true}, false},
		{"refused", Disposition{RefusedArm: ArmMerging}, false},
		{"held", Disposition{}, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.d.Parked(); got != tc.want {
				t.Fatalf("Parked() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestStandingHoldTellsWhyNoHoldStands(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID)
		want    error
	}{
		{
			name: "a turn nothing was held under",
			arrange: func(*testing.T, *harness) (ids.WorkspaceID, ids.TurnID) {
				return theWorkspace, "never-held"
			},
			want: ErrNoSuchHold,
		},
		{
			name: "a hold of another workspace",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				return "another-workspace", "t1"
			},
			want: ErrNoSuchHold,
		},
		{
			name: "a hold the session took",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				turnEnds(h)
				return theWorkspace, "t1"
			},
			want: ErrAlreadyDelivered,
		},
		{
			name: "a hold dropped",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
				return theWorkspace, "t1"
			},
			want: ErrNoSuchHold,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, turn := tt.arrange(t, h)
			// Act
			_, err := h.q.standingHold(context.Background(), ws, turn)
			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("err = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestStandingHoldAnswersAStandingHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	// Act
	held, err := h.q.standingHold(context.Background(), theWorkspace, "t1")
	// Assert
	if err != nil || held.Turn != "t1" || held.Tombstone != nil {
		t.Fatalf("standingHold = (%+v, %v), want t1 standing", held, err)
	}
}

func TestStandingHoldSurfacesAFailedRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	h.db.byTurnErr = errors.New("database is locked")
	// Act
	_, err := h.q.standingHold(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, h.db.byTurnErr) {
		t.Fatalf("err = %v, want the read failure", err)
	}
}

func TestHoldUnderTellsEveryFateOfANamedHold(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID)
		want     error
		wantTomb string
	}{
		{
			name: "a standing hold",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				return theWorkspace, "t1"
			},
		},
		{
			name:    "a turn nothing was held under",
			arrange: func(*testing.T, *harness) (ids.WorkspaceID, ids.TurnID) { return theWorkspace, "never-held" },
			want:    ErrNoSuchHold,
		},
		{
			name: "a hold of another workspace",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				return "another-workspace", "t1"
			},
			want: ErrNoSuchHold,
		},
		{
			name: "a hold the session took",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				turnEnds(h)
				return theWorkspace, "t1"
			},
			want: ErrAlreadyDelivered, wantTomb: tombstoneDelivered,
		},
		{
			name: "a hold dropped",
			arrange: func(t *testing.T, h *harness) (ids.WorkspaceID, ids.TurnID) {
				queuedBehind(t, h, "t1")
				if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
					t.Fatalf("Drop: %v", err)
				}
				return theWorkspace, "t1"
			},
			want: ErrNotHeld, wantTomb: tombstoneDropped,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, turn := tt.arrange(t, h)
			// Act
			held, err := h.q.holdUnder(context.Background(), ws, turn)
			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("err = %v, want %v", err, tt.want)
			}
			gotTomb := ""
			if held.Tombstone != nil {
				gotTomb = held.Tombstone.Kind
			}
			if gotTomb != tt.wantTomb {
				t.Fatalf("tombstone = %q, want %q: a retired hold's row rides beside its error", gotTomb, tt.wantTomb)
			}
		})
	}
}

func TestHoldUnderSurfacesAFailedReadWithTheHoldItNames(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.byTurnErr = errors.New("database is locked")
	// Act
	_, err := h.q.holdUnder(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, h.db.byTurnErr) || !strings.Contains(err.Error(), `"t1"`) {
		t.Fatalf("err = %v, want the read failure naming t1", err)
	}
}

// TestEveryNamedHoldResolverReadsThroughHoldUnder pins that the resolvers of
// a named hold share holdUnder's shape: each refuses another workspace's hold
// as never held, and each surfaces the one-row read's failure. A resolver
// hand-rolling its own read (the workspace's standing list, say) fails here.
func TestEveryNamedHoldResolverReadsThroughHoldUnder(t *testing.T) {
	resolvers := []struct {
		name    string
		resolve func(h *harness, ws ids.WorkspaceID) error
	}{
		{"standingHold", func(h *harness, ws ids.WorkspaceID) error {
			_, err := h.q.standingHold(context.Background(), ws, "t1")
			return err
		}},
		{"heldForEdit", func(h *harness, ws ids.WorkspaceID) error {
			_, err := h.q.heldForEdit(context.Background(), ws, "t1", h.q.deps.Log.Global(), opEditBegin)
			return err
		}},
		{"heldForFold", func(h *harness, ws ids.WorkspaceID) error {
			_, err := h.q.heldForFold(context.Background(), ws, "t1", h.q.deps.Log.Global())
			return err
		}},
	}
	for _, r := range resolvers {
		t.Run(r.name+" refuses another workspace's hold", func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			queuedBehind(t, h, "t1")
			// Act
			err := r.resolve(h, "another-workspace")
			// Assert
			if !errors.Is(err, ErrNoSuchHold) {
				t.Fatalf("err = %v, want ErrNoSuchHold", err)
			}
		})
		t.Run(r.name+" surfaces the one-row read's failure", func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			queuedBehind(t, h, "t1")
			h.db.byTurnErr = errors.New("database is locked")
			// Act
			err := r.resolve(h, theWorkspace)
			// Assert
			if !errors.Is(err, h.db.byTurnErr) {
				t.Fatalf("err = %v, want the one-row read's failure", err)
			}
		})
	}
}

// fakeStalls is a lockwatch.Registry that records what it was asked.
type fakeStalls struct {
	mu      sync.Mutex
	watched []watchedLock
}

type watchedLock struct {
	m    *lockwatch.Mutex
	lock string
	ws   ids.WorkspaceID
}

func (f *fakeStalls) Watch(m *lockwatch.Mutex, lock string, ws ids.WorkspaceID, _ dlog.Logger) func() {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.watched = append(f.watched, watchedLock{m: m, lock: lock, ws: ws})
	return func() {}
}

// newWatchedHarness is the harness with its queue rebuilt on stalls.
func newWatchedHarness(t *testing.T, stalls *fakeStalls) *harness {
	t.Helper()
	h := newHarness(t)
	deps := h.q.deps
	deps.Stalls = stalls
	q, err := newQueue(deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = q
	return h
}

// TestNewWatchesTheQueueMutex pins that the daemon-wide mutex every Submit
// takes is watched, under no workspace.
func TestNewWatchesTheQueueMutex(t *testing.T) {
	// Arrange.
	stalls := &fakeStalls{}

	// Act.
	h := newWatchedHarness(t, stalls)

	// Assert.
	want := []watchedLock{{m: &h.q.mu, lock: "promptqueue.queue"}}
	if !reflect.DeepEqual(stalls.watched, want) {
		t.Fatalf("watched = %+v, want %+v", stalls.watched, want)
	}
}

// TestResolvingAWorkspaceWatchesItsLocks pins that a workspace's delivery and
// verdict locks are watched, under that workspace, once its logger is known.
func TestResolvingAWorkspaceWatchesItsLocks(t *testing.T) {
	// Arrange.
	stalls := &fakeStalls{}
	h := newWatchedHarness(t, stalls)

	// Act.
	if _, err := h.q.logger(context.Background(), theWorkspace); err != nil {
		t.Fatalf("logger: %v", err)
	}

	// Assert.
	s := h.q.state(theWorkspace)
	want := []watchedLock{
		{m: &h.q.mu, lock: "promptqueue.queue"},
		{m: &s.drain, lock: "promptqueue.drain", ws: theWorkspace},
		{m: &s.verdicts, lock: "promptqueue.verdicts", ws: theWorkspace},
	}
	if !reflect.DeepEqual(stalls.watched, want) {
		t.Fatalf("watched = %+v, want %+v", stalls.watched, want)
	}
}

// TestAWorkspacesLocksAreWatchedOnce pins that every later resolution of the
// same workspace registers nothing more.
func TestAWorkspacesLocksAreWatchedOnce(t *testing.T) {
	// Arrange.
	stalls := &fakeStalls{}
	h := newWatchedHarness(t, stalls)
	if _, err := h.q.logger(context.Background(), theWorkspace); err != nil {
		t.Fatalf("logger: %v", err)
	}

	// Act.
	if _, err := h.q.logger(context.Background(), theWorkspace); err != nil {
		t.Fatalf("logger: %v", err)
	}

	// Assert.
	if n := len(stalls.watched); n != 3 {
		t.Fatalf("registrations = %d (%+v), want 3", n, stalls.watched)
	}
}

// TestAnUnresolvedWorkspaceIsNotWatched pins that a workspace whose logger
// cannot be resolved registers nothing: its record would have nowhere to go.
func TestAnUnresolvedWorkspaceIsNotWatched(t *testing.T) {
	// Arrange.
	stalls := &fakeStalls{}
	h := newWatchedHarness(t, stalls)

	// Act.
	_, err := h.q.logger(context.Background(), "no-such-workspace")

	// Assert.
	if err == nil {
		t.Fatal("logger resolved a workspace the state client does not hold")
	}
	if n := len(stalls.watched); n != 1 {
		t.Fatalf("registrations = %d (%+v), want only the queue's own mutex", n, stalls.watched)
	}
}

func TestStandingSessionHold(t *testing.T) {
	reconnect, coldGate, merge := wsm.HoldReconnect, wsm.HoldColdGate, wsm.HoldMerge
	retired := &wsm.Tombstone{Kind: tombstoneDropped}
	tests := []struct {
		name string
		held wsm.HeldPrompt
		want bool
	}{
		{name: "a standing reconnect hold", held: wsm.HeldPrompt{Hold: &reconnect}, want: true},
		{name: "a retired reconnect hold", held: wsm.HeldPrompt{Hold: &reconnect, Tombstone: retired}, want: false},
		{name: "a standing cold-gate hold", held: wsm.HeldPrompt{Hold: &coldGate}, want: true},
		{name: "a retired cold-gate hold", held: wsm.HeldPrompt{Hold: &coldGate, Tombstone: retired}, want: false},
		{name: "a standing hold of another kind", held: wsm.HeldPrompt{Hold: &merge}, want: false},
		{name: "a standing prompt with no hold", held: wsm.HeldPrompt{}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := standingSessionHold(tt.held)
			// Assert
			if got != tt.want {
				t.Fatalf("standingSessionHold = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestInQueueOrderSortsOldestFirstKeepingTies(t *testing.T) {
	// Arrange
	later, earlier := instant.Add(time.Second), instant
	held := []wsm.HeldPrompt{
		{Turn: "late", QueuedAt: later},
		{Turn: "tie-a", QueuedAt: earlier},
		{Turn: "tie-b", QueuedAt: earlier},
	}
	// Act
	got := inQueueOrder(held)
	// Assert
	var turns []ids.TurnID
	for _, h := range got {
		turns = append(turns, h.Turn)
	}
	if want := []ids.TurnID{"tie-a", "tie-b", "late"}; !slices.Equal(turns, want) {
		t.Fatalf("order = %v, want %v", turns, want)
	}
	if &got[0] != &held[0] {
		t.Fatal("inQueueOrder sorted a copy, want HELD sorted in place")
	}
}

func TestInQueueOrderLeavesAnEmptyQueueEmpty(t *testing.T) {
	// Arrange / Act
	got := inQueueOrder(nil)
	// Assert
	if len(got) != 0 {
		t.Fatalf("order = %v, want none", got)
	}
}

func TestASubmissionToAForgottenWorkspaceIsRefusedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.workspaceErr = wsm.ErrNotFound
	h.db.mu.Unlock()

	// Act
	_, err := h.q.Submit(context.Background(), submission("t1", "after the forget"))

	// Assert
	if !errors.Is(err, wsm.ErrNotFound) {
		t.Fatalf("Submit = %v, want the forgotten workspace refused", err)
	}
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing delivered", started)
	}
	if !logged(h.log.Records(), "info", opSubmit, "the workspace is no longer registered; its submission is refused") {
		t.Fatalf("records = %+v, want the refusal at info", h.log.Records())
	}
	if logged(h.log.Records(), "error", opSubmit, "could not resolve the workspace") {
		t.Fatalf("records = %+v, want no ERROR for a forgotten workspace", h.log.Records())
	}
}
