package promptqueue

import (
	"context"
	"reflect"
	"regexp"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lockwatch"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

func TestNewRefusesEachMissingCollaborator(t *testing.T) {
	// Arrange: one complete set of dependencies, then each one blanked.
	full := func() Deps {
		h := &harness{db: newFakeDB(), sender: newFakeSender(), watcher: &fakeWatcher{},
			feed: &fakeFeed{}, footer: &fakeFooter{}, sidebar: &fakeSidebar{}, holds: &fakeHolds{}, judge: &scriptedJudge{}}
		return Deps{
			DB: h.db, Judge: h.judge, Feed: h.feed, Footer: h.footer, Sidebar: h.sidebar, Holds: h.holds,
			Edges:       &fakeEdges{},
			Leases:      &fakeLeases{},
			TurnBanners: &fakeTurnBanners{},
			Commits:     &fakeRecounter{},
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
		{"footer resolver", func(d *Deps) { d.Footer = nil }},
		{"roster resolver", func(d *Deps) { d.Sidebar = nil }},
		{"turn edges' observer", func(d *Deps) { d.Edges = nil }},
		{"lease watcher", func(d *Deps) { d.Leases = nil }},
		{"holds resolver", func(d *Deps) { d.Holds = nil }},
		{"client resolver", func(d *Deps) { d.Client = nil }},
		{"watcher resolver", func(d *Deps) { d.Watcher = nil }},
		{"image resolver", func(d *Deps) { d.ResolveImage = nil }},
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

func TestStandingHoldReportsAnUnknownTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := h.q.standingHold(context.Background(), theWorkspace, "never-held")
	// Assert
	if err != ErrNoSuchHold {
		t.Fatalf("err = %v, want ErrNoSuchHold", err)
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

// TestNoSiteHandRollsTheStandingSessionHoldTest pins the call sites to the
// one predicate: a hand-rolled copy of its test fails here rather than
// drifting from it silently.
func TestNoSiteHandRollsTheStandingSessionHoldTest(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`Tombstone\s*[!=]=\s*nil\s*(&&|\|\|)\s*\w+\.Hold\s*[!=]=\s*nil\s*(&&|\|\|)\s*\*\w+\.Hold\s*[!=]=\s*wsm\.HoldReconnect`)
	const definition = "return h.Tombstone == nil && h.Hold != nil && waitsOnSession(*h.Hold)"
	definitions := 0
	for _, file := range sourcescan.Production(t) {
		// Act
		for i, line := range strings.Split(string(file.Source), "\n") {
			// Assert
			if strings.TrimSpace(line) == definition {
				definitions++
				continue
			}
			if handRolled.MatchString(line) {
				t.Errorf("%s:%d hand-rolls the standing session hold test; call standingSessionHold: %s", file.Name, i+1, strings.TrimSpace(line))
			}
		}
	}
	if definitions != 1 {
		t.Fatalf("found %d definitions of standingSessionHold's test, want exactly 1", definitions)
	}
}
