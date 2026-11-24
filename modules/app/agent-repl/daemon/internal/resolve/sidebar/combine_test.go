package sidebar_test

import (
	"fmt"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/vocab"
)

// gatedFooter is the real footer with one gate in its status read: once
// armed, the first read of the gated workspace's status says so on entered
// and waits for release. A render reads every row's status, so arming it
// holds the next render mid-way, under the roster's lock.
type gatedFooter struct {
	footer.Resolver
	gated    ids.WorkspaceID
	mu       sync.Mutex
	armed    bool
	entered  chan struct{}
	released chan struct{}
}

func (g *gatedFooter) Status(ws ids.WorkspaceID) *frontendv1.FooterStatus {
	g.mu.Lock()
	hold := g.armed && ws == g.gated
	if hold {
		g.armed = false
	}
	g.mu.Unlock()
	if hold {
		close(g.entered)
		<-g.released
	}
	return g.Resolver.Status(ws)
}

// arm holds the next render at the gated workspace's row.
func (g *gatedFooter) arm() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.armed = true
}

// combineFixture is a roster over three registered workspaces whose next
// render can be held, with the queue's depth reported on queued.
type combineFixture struct {
	r        sidebar.Resolver
	gate     *gatedFooter
	surfaces *dlog.TestSurfaces
	queued   chan int
}

const (
	wsA = ids.WorkspaceID("combine-a")
	wsB = ids.WorkspaceID("combine-b")
	wsC = ids.WorkspaceID("combine-c")
)

func newCombineFixture(t *testing.T, opts ...sidebar.Option) *combineFixture {
	t.Helper()
	colors, err := vocab.LoadRenderColors(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadRenderColors: %v", err)
	}
	f, err := footer.New(colors, dlog.NewTestSurfaces(), footer.WithMomentaryDwell(time.Hour))
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	gate := &gatedFooter{Resolver: f, gated: wsA, entered: make(chan struct{}), released: make(chan struct{})}
	surfaces := dlog.NewTestSurfaces()
	clock := sidebar.WithClock(func() time.Time { return epoch })
	r, err := sidebar.New(testColors(), surfaces, gate, append([]sidebar.Option{clock}, opts...)...)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	r.SetRegistry(registry(workspace(string(wsA), "a"), workspace(string(wsB), "b"), workspace(string(wsC), "c")))
	queued := make(chan int, 16)
	sidebar.SetOnQueued(r, func(depth int) { queued <- depth })
	return &combineFixture{r: r, gate: gate, surfaces: surfaces, queued: queued}
}

// holdRender starts change on its own goroutine with the next render held at
// its first row, and returns once that render is under way. The returned
// channel closes when change returns.
func (fx *combineFixture) holdRender(t *testing.T, change func()) <-chan struct{} {
	t.Helper()
	fx.gate.arm()
	done := make(chan struct{})
	go func() {
		defer close(done)
		change()
	}()
	if depth := <-fx.queued; depth != 1 {
		t.Fatalf("the held change joined a queue of depth %d, want 1", depth)
	}
	<-fx.gate.entered
	return done
}

// queueBehind starts each change on its own goroutine and returns once every
// one of them is queued behind the held render.
func (fx *combineFixture) queueBehind(t *testing.T, changes ...func()) []<-chan struct{} {
	t.Helper()
	var dones []<-chan struct{}
	for i, change := range changes {
		done := make(chan struct{})
		go func() {
			defer close(done)
			change()
		}()
		if depth := <-fx.queued; depth != i+1 {
			t.Fatalf("queued change %d joined a queue of depth %d, want %d", i, depth, i+1)
		}
		dones = append(dones, done)
	}
	return dones
}

func TestTheCombinedCommit(t *testing.T) {
	t.Run("changes queued while a render runs are published by one further render", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		before := latestPush(t, fx.r).GetRevision()
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })
		waiters := fx.queueBehind(t,
			func() { fx.r.SetBringingUp(wsB, true) },
			func() { fx.r.SetBringingUp(wsC, true) })

		// Act
		close(fx.gate.released)
		<-leader
		for _, done := range waiters {
			<-done
		}

		// Assert
		if got, want := latestPush(t, fx.r).GetRevision(), before+2; got != want {
			t.Fatalf("revision = %d after three changes, two of them queued together, want %d", got, want)
		}
	})

	t.Run("every queued caller returns with its change published", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })
		waiters := fx.queueBehind(t,
			func() { fx.r.SetBringingUp(wsB, true) },
			func() { fx.r.SetBringingUp(wsC, true) })

		// Act
		close(fx.gate.released)
		<-leader
		<-waiters[0]

		// Assert: the first waiter has returned; its change, and its batch
		// mate's, are on the roster already.
		for _, ws := range []ids.WorkspaceID{wsB, wsC} {
			row, _, ok := fx.r.Row(ws)
			if !ok {
				t.Fatalf("no published row for %s", ws)
			}
			if got := availabilityName(row); got != "pending" {
				t.Fatalf("%s availability = %q once its caller returned, want pending", ws, got)
			}
		}
		<-waiters[1]
	})

	t.Run("each row of a combined render names the change that moved it", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })
		waiters := fx.queueBehind(t,
			func() { fx.r.OnLink(wsB, shimclient.LinkDead) },
			func() { fx.r.SetOpening(wsC, true) })

		// Act
		close(fx.gate.released)
		<-leader
		for _, done := range waiters {
			<-done
		}

		// Assert
		causes := map[string]any{}
		for _, rec := range armChanges(fx.surfaces) {
			causes[fmt.Sprint(rec.Context[dlog.KeyWorkspaceDir])] = rec.Context["cause"]
		}
		want := map[string]any{
			"/repos/one/" + string(wsB): "daemon.sidebar.on_link",
			"/repos/one/" + string(wsC): "daemon.sidebar.set_opening",
		}
		for dir, cause := range want {
			if causes[dir] != cause {
				t.Fatalf("arm change of %s named cause %v, want %v (all: %v)", dir, causes[dir], cause, causes)
			}
		}
	})

	t.Run("a sink that changes the roster again is served by the next leader", func(t *testing.T) {
		// Arrange: the status-change sink re-projects the footer once, as the
		// daemon's durable sink may cause a further change.
		var r sidebar.Resolver
		var once sync.Once
		fx := newCombineFixture(t, sidebar.WithStatusChangeSink(func(ws ids.WorkspaceID, _ time.Time) {
			if r != nil {
				once.Do(func() { r.FooterStatusChanged(ws) })
			}
		}))
		r = fx.r

		// Act: returns only if the nested change was not left waiting on
		// the goroutine running the sink.
		r.SetBringingUp(wsB, true)

		// Assert
		if row, _, ok := r.Row(wsB); !ok || availabilityName(row) != "pending" {
			t.Fatalf("the change was not published")
		}
	})
}

func TestThePostingView(t *testing.T) {
	t.Run("a posted change returns while a render holds the roster's lock", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })

		// Act: returns only because it waits for nothing.
		fx.r.Posting().SetBringingUp(wsB, true)

		// Assert
		if depth := <-fx.queued; depth != 1 {
			t.Fatalf("the posted change joined a queue of depth %d, want 1", depth)
		}
		close(fx.gate.released)
		<-leader
		fx.r.Settle()
		if row, _, ok := fx.r.Row(wsB); !ok || availabilityName(row) != "pending" {
			t.Fatalf("the posted change was not published once the held render ended")
		}
	})

	t.Run("a posted change with no batch under way is led on its own", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)

		// Act
		fx.r.Posting().SetBringingUp(wsB, true)
		fx.r.Settle()

		// Assert
		if row, _, ok := fx.r.Row(wsB); !ok || availabilityName(row) != "pending" {
			t.Fatalf("the posted change was not published")
		}
	})

	t.Run("posted and committed changes are applied in the order they were queued", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })
		fx.r.Posting().SetOpening(wsC, true)
		if depth := <-fx.queued; depth != 1 {
			t.Fatalf("the posted change joined a queue of depth %d, want 1", depth)
		}
		committed := make(chan struct{})
		go func() {
			defer close(committed)
			fx.r.SetOpening(wsC, false)
		}()
		if depth := <-fx.queued; depth != 2 {
			t.Fatalf("the committed change joined a queue of depth %d, want 2", depth)
		}

		// Act
		close(fx.gate.released)
		<-leader
		<-committed

		// Assert
		if row, _, ok := fx.r.Row(wsC); !ok || row.GetOpening() != nil {
			t.Fatalf("the row draws the posted change over the committed one queued after it")
		}
	})

	t.Run("a read waits for every change posted before it", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })
		fx.r.Posting().SetBringingUp(wsC, true)
		if depth := <-fx.queued; depth != 1 {
			t.Fatalf("the posted change joined a queue of depth %d, want 1", depth)
		}
		read := make(chan *frontendv1.RosterRow, 1)
		go func() {
			row, _, _ := fx.r.Row(wsC)
			read <- row
		}()
		if depth := <-fx.queued; depth != 2 {
			t.Fatalf("the read joined a queue of depth %d, want 2", depth)
		}

		// Act
		close(fx.gate.released)
		<-leader

		// Assert
		if row := <-read; availabilityName(row) != "pending" {
			t.Fatalf("the read answered availability %q, want the posted change's pending", availabilityName(row))
		}
	})

	t.Run("the footer's status edge is posted", func(t *testing.T) {
		// Arrange
		fx := newCombineFixture(t)
		leader := fx.holdRender(t, func() { fx.r.SetBringingUp(wsA, true) })

		// Act: returns only because it waits for nothing.
		fx.r.FooterStatusChanged(wsB)

		// Assert
		if depth := <-fx.queued; depth != 1 {
			t.Fatalf("the footer's status edge joined a queue of depth %d, want 1", depth)
		}
		close(fx.gate.released)
		<-leader
	})
}

func TestSettleWaitsForTheReportOfEveryBatchBeforeIt(t *testing.T) {
	// Arrange: the status-change sink holds the first report it is handed.
	entered, release := make(chan struct{}), make(chan struct{})
	var once sync.Once
	var mu sync.Mutex
	reported := false
	fx := newCombineFixture(t, sidebar.WithStatusChangeSink(func(ids.WorkspaceID, time.Time) {
		held := false
		once.Do(func() { held = true })
		if !held {
			return
		}
		close(entered)
		<-release
		mu.Lock()
		reported = true
		mu.Unlock()
	}))
	fx.r.Posting().SetOpening(wsB, true)
	<-entered

	// Act
	settled := make(chan struct{})
	go func() {
		defer close(settled)
		fx.r.Settle()
	}()
	close(release)
	<-settled

	// Assert
	mu.Lock()
	defer mu.Unlock()
	if !reported {
		t.Fatal("Settle returned before the batch before it was reported")
	}
}
