package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

const theWorkspace ids.WorkspaceID = "ws-hidden"

var (
	now     = time.Unix(1_800_000_000, 0).UTC()
	lapseAt = now.Add(30 * time.Minute)
)

type fakeDB struct {
	mu        sync.Mutex
	records   map[ids.WorkspaceID]wsm.Workspace
	listErr   error
	readErr   error
	setErr    error
	setLapses []wsm.CacheLapse
}

func (f *fakeDB) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.listErr != nil {
		return nil, f.listErr
	}
	out := make([]wsm.Workspace, 0, len(f.records))
	for _, rec := range f.records {
		out = append(out, rec)
	}
	// THE REGISTRY'S ORDER, made deterministic: the real store lists by
	// creation, and these records are created in ID order.
	slices.SortFunc(out, func(a, b wsm.Workspace) int { return strings.Compare(string(a.ID), string(b.ID)) })
	return out, nil
}

func (f *fakeDB) Workspace(_ context.Context, id wsm.WorkspaceID) (wsm.Workspace, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.readErr != nil {
		return wsm.Workspace{}, f.readErr
	}
	rec, ok := f.records[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return rec, nil
}

func (f *fakeDB) SetCacheLapse(_ context.Context, id wsm.WorkspaceID, lapse wsm.CacheLapse) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.setErr != nil {
		return false, f.setErr
	}
	rec, ok := f.records[id]
	if !ok {
		return false, wsm.ErrNotFound
	}
	if !rec.Hidden && !rec.Closed {
		return false, nil
	}
	f.setLapses = append(f.setLapses, lapse)
	rec.CacheLapse = &lapse
	f.records[id] = rec
	return true, nil
}

type fakeShim struct {
	mu      sync.Mutex
	cadence []string
	err     error
	set     chan string

	// answer and beatErr are what Beat answers; beats counts its calls.
	answer  *shimv1.SendKeepaliveSuccess
	beatErr error
	beats   int
}

func (f *fakeShim) Hold(context.Context) error { return f.note("hold") }
func (f *fakeShim) Run(context.Context) error  { return f.note("run") }
func (f *fakeShim) LastRound(context.Context) error {
	return f.note("last_round")
}

func (f *fakeShim) Beat(context.Context) (*shimv1.SendKeepaliveSuccess, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.beats++
	return f.answer, f.beatErr
}

func (f *fakeShim) note(cadence string) error {
	f.mu.Lock()
	f.cadence = append(f.cadence, cadence)
	f.mu.Unlock()
	if f.set != nil {
		f.set <- cadence
	}
	return f.err
}

type fakeStandDowns struct {
	stood chan ids.WorkspaceID
}

func (f *fakeStandDowns) StandDownWithheld(ws ids.WorkspaceID) { f.stood <- ws }

type harness struct {
	keeper *Keeper
	db     *fakeDB
	shim   *fakeShim
	// shims, when set, is the shim serving each workspace; a workspace absent
	// from it is served by none.
	shims      map[ids.WorkspaceID]*fakeShim
	serving    bool
	standDowns *fakeStandDowns
	publishes  int
	clock      *clocktest.Scripted
	log        *dlog.TestSurfaces
}

func hidden() wsm.Workspace {
	at := now.Add(-time.Hour)
	return wsm.Workspace{ID: theWorkspace, Hidden: true, HiddenAt: &at}
}

func visible() wsm.Workspace {
	return wsm.Workspace{ID: theWorkspace}
}

func closed() wsm.Workspace {
	return wsm.Workspace{ID: theWorkspace, Closed: true}
}

func newHarness(t *testing.T, rec wsm.Workspace) *harness {
	t.Helper()
	h := &harness{
		db:         &fakeDB{records: map[ids.WorkspaceID]wsm.Workspace{rec.ID: rec}},
		shim:       &fakeShim{},
		serving:    true,
		standDowns: &fakeStandDowns{stood: make(chan ids.WorkspaceID, 4)},
		clock:      clocktest.New(now),
		log:        dlog.NewTestSurfaces(),
	}
	keeper, err := New(Deps{
		DB: h.db,
		Shim: func(ws ids.WorkspaceID) (Keepalives, bool) {
			if h.shims != nil {
				shim, ok := h.shims[ws]
				return shim, ok
			}
			if !h.serving {
				return nil, false
			}
			return h.shim, true
		},
		StandDowns:      h.standDowns,
		PublishRegistry: func(context.Context) error { h.publishes++; return nil },
		Clock:           h.clock,
		Log:             h.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.keeper = keeper
	return h
}

func (h *harness) found(q dlog.RecordQuery) bool {
	_, ok := dlog.FindRecord(h.log.Records(), q)
	return ok
}

func (h *harness) stoodDown() bool {
	select {
	case <-h.standDowns.stood:
		return true
	default:
		return false
	}
}

func stated(at time.Time, gated *uint64) *conversationv1.SessionCacheLapse {
	lapse := &conversationv1.SessionCacheLapse{LapsesAtMs: at.UnixMilli()}
	if gated != nil {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Gated{Gated: &conversationv1.SessionCacheLapseGated{ContextTokens: *gated}}
	} else {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Ungated{Ungated: &conversationv1.SessionCacheLapseUngated{}}
	}
	return lapse
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := func() Deps {
		return Deps{
			DB:              &fakeDB{},
			Shim:            func(ids.WorkspaceID) (Keepalives, bool) { return nil, false },
			StandDowns:      &fakeStandDowns{},
			PublishRegistry: func(context.Context) error { return nil },
			Clock:           clocktest.New(now),
			Log:             dlog.NewTestSurfaces(),
		}
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "state client", strip: func(d *Deps) { d.DB = nil }},
		{name: "shim fleet", strip: func(d *Deps) { d.Shim = nil }},
		{name: "stand-downs", strip: func(d *Deps) { d.StandDowns = nil }},
		{name: "republish", strip: func(d *Deps) { d.PublishRegistry = nil }},
		{name: "clock", strip: func(d *Deps) { d.Clock = nil }},
		{name: "log", strip: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tt.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatalf("New without a %s succeeded", tt.name)
			}
		})
	}
}

func TestRecordStoresALapsingWorkspacesProjection(t *testing.T) {
	tokens := uint64(120_000)
	tests := []struct {
		name  string
		rec   func() wsm.Workspace
		gated *uint64
	}{
		{name: "a hidden workspace's gated lapse", rec: hidden, gated: &tokens},
		{name: "a hidden workspace's ungated lapse", rec: hidden, gated: nil},
		{name: "a closed workspace's gated lapse", rec: closed, gated: &tokens},
		{name: "a closed workspace's ungated lapse", rec: closed, gated: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tt.rec())

			// Act
			h.keeper.record(context.Background(), theWorkspace, stated(lapseAt, tt.gated))

			// Assert
			got := h.db.records[theWorkspace].CacheLapse
			if got == nil || !got.LapsesAt.Equal(lapseAt) || (got.GatedTokens == nil) != (tt.gated == nil) {
				t.Fatalf("CacheLapse = %+v, want lapsing at %v gated %v", got, lapseAt, tt.gated)
			}
			if !h.keeper.deadlines[theWorkspace].Equal(lapseAt) || h.publishes != 1 {
				t.Fatalf("deadline %v and %d publishes, want %v and 1", h.keeper.deadlines[theWorkspace], h.publishes, lapseAt)
			}
		})
	}
}

func TestRecordDropsAProjectionForAVisibleWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())
	h.keeper.deadlines[theWorkspace] = lapseAt

	// Act
	h.keeper.record(context.Background(), theWorkspace, stated(lapseAt, nil))

	// Assert
	if _, armed := h.keeper.deadlines[theWorkspace]; armed || h.publishes != 0 {
		t.Fatalf("armed %v with %d publishes, want no deadline and no publish", armed, h.publishes)
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opProjection, Message: "a projected cache lapse arrived for a workspace that is neither hidden nor closed; it is dropped"}) {
		t.Fatalf("no record of the dropped projection: %v", h.log.Records())
	}
}

func TestRecordLogsAWriteThatFails(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.db.setErr = errors.New("disk full")

	// Act
	h.keeper.record(context.Background(), theWorkspace, stated(lapseAt, nil))

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opProjection})
	if !ok || rec.Context["cause"] != "disk full" || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("error record = %+v, %v; want one naming the cause and the workspace", rec, ok)
	}
	if _, armed := h.keeper.deadlines[theWorkspace]; armed {
		t.Fatal("a projection that was not recorded was armed")
	}
}

func TestRecordReadsAnUnnamedVerdictAsUngated(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	lapse := &conversationv1.SessionCacheLapse{LapsesAtMs: lapseAt.UnixMilli()}

	// Act
	h.keeper.record(context.Background(), theWorkspace, lapse)

	// Assert
	got := h.db.records[theWorkspace].CacheLapse
	if got == nil || got.GatedTokens != nil {
		t.Fatalf("CacheLapse = %+v, want an ungated lapse", got)
	}
	if !h.found(dlog.RecordQuery{Level: "info", Message: "the projected lapse states a verdict this daemon does not name; it is read as ungated"}) {
		t.Fatalf("no record of the unnamed verdict: %v", h.log.Records())
	}
}

func TestReconcileSetsTheCadenceToMatchTheVisibility(t *testing.T) {
	merged := hidden()
	mergedAt := now
	merged.MergedAt = &mergedAt
	closedHidden := hidden()
	closedHidden.Closed = true
	reopened := visible()
	tests := []struct {
		name string
		rec  wsm.Workspace
		want string
	}{
		{name: "a hidden workspace holds", rec: hidden(), want: "hold"},
		{name: "a visible workspace runs", rec: visible(), want: "run"},
		{name: "a merged workspace runs", rec: merged, want: "run"},
		{name: "a closed workspace gets its last round", rec: closed(), want: "last_round"},
		{name: "a closed hidden workspace holds", rec: closedHidden, want: "hold"},
		{name: "a reopened workspace runs", rec: reopened, want: "run"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tt.rec)

			// Act
			h.keeper.reconcile(context.Background(), theWorkspace, "test")

			// Assert
			if len(h.shim.cadence) != 1 || h.shim.cadence[0] != tt.want {
				t.Fatalf("cadence = %v, want [%s]", h.shim.cadence, tt.want)
			}
		})
	}
}

func TestReconcileOfAClosedWorkspaceKeepsItsLapse(t *testing.T) {
	// Arrange
	h := newHarness(t, closed())
	h.keeper.deadlines[theWorkspace] = lapseAt

	// Act
	h.keeper.reconcile(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.deadlines[theWorkspace]; !armed {
		t.Fatal("a closed workspace's lapse was disarmed")
	}
}

func TestReconcileOfAVisibleWorkspaceDisarmsItsLapse(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())
	h.keeper.deadlines[theWorkspace] = lapseAt

	// Act
	h.keeper.reconcile(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.deadlines[theWorkspace]; armed {
		t.Fatal("a visible workspace kept its lapse armed")
	}
}

func TestReconcileWithNoServedSessionSetsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.serving = false

	// Act
	h.keeper.reconcile(context.Background(), theWorkspace, "test")

	// Assert
	if len(h.shim.cadence) != 0 || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("cadence %v with records %v, want nothing set and no error", h.shim.cadence, h.log.Records())
	}
}

func TestReconcileAnswersAShimFailure(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		level string
	}{
		{name: "a shim with no session yet", err: ErrNoSession, level: "debug"},
		{name: "a shim that died under the call", err: fmt.Errorf("%w: unavailable: unexpected EOF", shimclient.ErrShimDied), level: "info"},
		{name: "a shim that cannot be reached", err: errors.New("connection refused"), level: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, hidden())
			h.shim.err = tt.err

			// Act
			h.keeper.reconcile(context.Background(), theWorkspace, "test")

			// Assert
			rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: tt.level, Operation: opCadence})
			if !ok || rec.Context["cadence"] != "hold" {
				t.Fatalf("%s record = %+v, %v; want one naming the cadence", tt.level, rec, ok)
			}
		})
	}
}

func TestReconcileLogsAVisibilityThatCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.db.readErr = errors.New("database is locked")

	// Act
	h.keeper.reconcile(context.Background(), theWorkspace, "test")

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opCadence})
	if !ok || rec.Context["cause"] != "database is locked" || len(h.shim.cadence) != 0 {
		t.Fatalf("error record = %+v, %v with cadence %v; want the cause and nothing set", rec, ok, h.shim.cadence)
	}
}

func TestLapseDueStandsDownALapsedWorkspace(t *testing.T) {
	tests := []struct {
		name string
		rec  func() wsm.Workspace
	}{
		{name: "a hidden workspace", rec: hidden},
		{name: "a closed workspace", rec: closed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := tt.rec()
			rec.CacheLapse = &wsm.CacheLapse{LapsesAt: lapseAt}
			h := newHarness(t, rec)
			h.keeper.deadlines[theWorkspace] = lapseAt
			h.clock.Set(lapseAt)

			// Act
			h.keeper.lapseDue(context.Background())

			// Assert
			if !h.stoodDown() || h.publishes != 1 {
				t.Fatalf("stood down %v with %d publishes, want a stand-down and one publish", h.stoodDown(), h.publishes)
			}
		})
	}
}

func TestLapseDueLeavesAWorkspaceItShouldNotStandDown(t *testing.T) {
	later := lapseAt.Add(time.Hour)
	tests := []struct {
		name     string
		rec      func() wsm.Workspace
		rearmed  bool
		deadline time.Time
	}{
		{name: "shown or reopened again", rec: visible},
		{name: "a lapse that moved later", rec: func() wsm.Workspace {
			rec := hidden()
			rec.CacheLapse = &wsm.CacheLapse{LapsesAt: later}
			return rec
		}, rearmed: true, deadline: later},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tt.rec())
			h.keeper.deadlines[theWorkspace] = lapseAt
			h.clock.Set(lapseAt)

			// Act
			h.keeper.lapseDue(context.Background())

			// Assert
			deadline, armed := h.keeper.deadlines[theWorkspace]
			if h.stoodDown() || armed != tt.rearmed || armed && !deadline.Equal(tt.deadline) || h.publishes != 0 {
				t.Fatalf("armed %v at %v with %d publishes, want armed %v at %v, no stand-down and no publish", armed, deadline, h.publishes, tt.rearmed, tt.deadline)
			}
		})
	}
}

func TestLapseDueKeepsALapseNotYetDue(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.keeper.deadlines[theWorkspace] = lapseAt

	// Act
	h.keeper.lapseDue(context.Background())

	// Assert
	if _, armed := h.keeper.deadlines[theWorkspace]; !armed || h.stoodDown() {
		t.Fatalf("armed %v, want the lapse still armed and nothing stood down", armed)
	}
}

func TestLapseDueLogsARecordThatCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.keeper.deadlines[theWorkspace] = lapseAt
	h.clock.Set(lapseAt)
	h.db.readErr = errors.New("database is locked")

	// Act
	h.keeper.lapseDue(context.Background())

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opLapse})
	if !ok || rec.Context["cause"] != "database is locked" || h.stoodDown() {
		t.Fatalf("error record = %+v, %v; want the cause and no stand-down", rec, ok)
	}
}

func TestRunTakesARecordedLapseWhenItComes(t *testing.T) {
	// Arrange
	rec := hidden()
	rec.CacheLapse = &wsm.CacheLapse{LapsesAt: lapseAt}
	h := newHarness(t, rec)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- h.keeper.Run(ctx) }()

	// Act
	if waited := <-h.clock.Asked; waited != 30*time.Minute {
		t.Fatalf("the keeper waited %v, want the 30 minutes to the recorded lapse", waited)
	}
	h.clock.Set(lapseAt)
	h.clock.Fire <- lapseAt

	// Assert
	if got := <-h.standDowns.stood; got != theWorkspace {
		t.Fatalf("stood down %q, want %q", got, theWorkspace)
	}
	cancel()
	if err := <-done; err != nil {
		t.Fatalf("Run = %v, want nil", err)
	}
}

func TestRunAppliesAQueuedEdge(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.shim.set = make(chan string, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go func() { _ = h.keeper.Run(ctx) }()

	// Act
	h.keeper.VisibilityChanged(theWorkspace)

	// Assert
	if got := <-h.shim.set; got != "hold" {
		t.Fatalf("cadence = %q, want hold", got)
	}
}

func TestRunAppliesAQueuedClosure(t *testing.T) {
	// Arrange
	h := newHarness(t, closed())
	h.shim.set = make(chan string, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go func() { _ = h.keeper.Run(ctx) }()

	// Act
	h.keeper.ClosureChanged(theWorkspace)

	// Assert
	if got := <-h.shim.set; got != "last_round" {
		t.Fatalf("cadence = %q, want last_round", got)
	}
}

func TestRunRefusesAListThatFails(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.db.listErr = errors.New("database is locked")

	// Act
	err := h.keeper.Run(context.Background())

	// Assert
	if err == nil || !h.found(dlog.RecordQuery{Level: "error", Operation: opLapse}) {
		t.Fatalf("Run = %v with records %v, want an error and its record", err, h.log.Records())
	}
}

func TestAnOperationTheDaemonsExitCancelsIsRecordedAsLeaving(t *testing.T) {
	tests := []struct {
		name string
		act  func(ctx context.Context, h *harness)
	}{
		{name: "re-arming at the start", act: func(ctx context.Context, h *harness) {
			h.db.listErr = context.Canceled
			if err := h.keeper.Run(ctx); err != nil {
				t.Errorf("Run = %v, want nil when the daemon is leaving", err)
			}
		}},
		{name: "recording a projection", act: func(ctx context.Context, h *harness) {
			h.db.setErr = context.Canceled
			h.keeper.record(ctx, theWorkspace, stated(lapseAt, nil))
		}},
		{name: "reading a visibility", act: func(ctx context.Context, h *harness) {
			h.db.readErr = context.Canceled
			h.keeper.reconcile(ctx, theWorkspace, "test")
		}},
		{name: "setting a cadence", act: func(ctx context.Context, h *harness) {
			h.shim.err = context.Canceled
			h.keeper.reconcile(ctx, theWorkspace, "test")
		}},
		{name: "reading a lapsed workspace", act: func(ctx context.Context, h *harness) {
			h.db.readErr = context.Canceled
			h.keeper.deadlines[theWorkspace] = lapseAt
			h.clock.Set(lapseAt)
			h.keeper.lapseDue(ctx)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, hidden())
			ctx, cancel := context.WithCancel(context.Background())
			cancel()

			// Act
			tt.act(ctx, h)

			// Assert
			if h.found(dlog.RecordQuery{Level: "error"}) || !h.found(dlog.RecordQuery{Level: "info"}) {
				t.Fatalf("records = %+v, want the leaving at INFO and no ERROR", h.log.Records())
			}
		})
	}
}
