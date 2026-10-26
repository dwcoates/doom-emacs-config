package coldbacklog

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// newYork is a location whose day boundaries differ from UTC's, so a test
// that judged a weekend in UTC instead would fail.
var newYork = mustLoad("America/New_York")

func mustLoad(name string) *time.Location {
	loc, err := time.LoadLocation(name)
	if err != nil {
		panic(err)
	}
	return loc
}

// at is an instant on a 2026-10 day in New York (Monday is the 26th).
func at(day, hour int) time.Time {
	return time.Date(2026, time.October, day, hour, 0, 0, 0, newYork)
}

func TestStaleAt(t *testing.T) {
	tests := []struct {
		name     string
		lapsedAt time.Time
		want     time.Time
	}{
		{name: "a weekday gate is stale a day later", lapsedAt: at(26, 10), want: at(27, 10)},
		{name: "a gate from the start of a weekday is stale at its end", lapsedAt: at(29, 0), want: at(30, 0)},
		{name: "a Friday gate skips the weekend", lapsedAt: at(30, 15), want: at(33, 15)},
		{name: "a gate from the start of a Friday is stale at its end", lapsedAt: at(30, 0), want: at(31, 0)},
		{name: "a Saturday gate counts from Monday", lapsedAt: at(31, 10), want: at(34, 0)},
		{name: "a Sunday gate counts from Monday", lapsedAt: at(32, 23), want: at(34, 0)},
		{name: "the weekend is judged in the location, not in UTC", lapsedAt: at(30, 22).UTC(), want: at(33, 22)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := StaleAt(tt.lapsedAt, newYork)

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("StaleAt(%v) = %v, want %v", tt.lapsedAt, got.In(newYork), tt.want)
			}
		})
	}
}

// coldAt is a lapsed gate's cold facts, going cold at WENT (no lifetime).
func coldAt(went time.Time) *conversationv1.SessionCold {
	return &conversationv1.SessionCold{
		ContextTokens:   100_000,
		LastRequestAtMs: went.Add(-time.Hour).UnixMilli(),
		Reason:          &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: time.Hour.Milliseconds()}},
	}
}

type fakeDB struct {
	records map[ids.WorkspaceID]wsm.Workspace
	err     error
}

func (d *fakeDB) Workspace(_ context.Context, id wsm.WorkspaceID) (wsm.Workspace, error) {
	if d.err != nil {
		return wsm.Workspace{}, d.err
	}
	rec, ok := d.records[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return rec, nil
}

type backlogCall struct {
	ws       ids.WorkspaceID
	lapsedAt time.Time
}

type harness struct {
	db       *fakeDB
	gates    map[ids.WorkspaceID]*conversationv1.SessionCold
	backlogs []backlogCall
	failWith error
	clock    *clocktest.Scripted
	log      *dlog.TestSurfaces
	sweeper  *Sweeper
}

func newHarness(t *testing.T, now time.Time) *harness {
	t.Helper()
	h := &harness{
		db:    &fakeDB{records: map[ids.WorkspaceID]wsm.Workspace{}},
		gates: map[ids.WorkspaceID]*conversationv1.SessionCold{},
		clock: clocktest.New(now),
		log:   dlog.NewTestSurfaces(),
	}
	sweeper, err := New(h.deps())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.sweeper = sweeper
	return h
}

func (h *harness) deps() Deps {
	return Deps{
		DB:    h.db,
		Gates: func() map[ids.WorkspaceID]*conversationv1.SessionCold { return h.gates },
		Backlog: func(_ context.Context, ws ids.WorkspaceID, lapsedAt time.Time) error {
			h.backlogs = append(h.backlogs, backlogCall{ws: ws, lapsedAt: lapsedAt})
			return h.failWith
		},
		Clock:    h.clock,
		Location: newYork,
		Log:      h.log,
	}
}

// gate stands a gate that went cold at WENT on a workspace whose row is REC.
func (h *harness) gate(rec wsm.Workspace, went time.Time) {
	h.db.records[rec.ID] = rec
	h.gates[rec.ID] = coldAt(went)
}

func (h *harness) logged(level, message string) bool {
	_, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: level, Operation: opSweep, Message: message})
	return ok
}

func TestSweepMovesAShownWorkspaceWhoseGateIsStale(t *testing.T) {
	// Arrange
	h := newHarness(t, at(27, 11))
	h.gate(wsm.Workspace{ID: "w1"}, at(26, 10))

	// Act
	h.sweeper.Sweep(context.Background())

	// Assert
	want := []backlogCall{{ws: "w1", lapsedAt: at(26, 10).UTC()}}
	if !slices.Equal(h.backlogs, want) {
		t.Fatalf("backlogs = %v, want %v", h.backlogs, want)
	}
}

func TestSweepLeavesAWorkspace(t *testing.T) {
	tests := []struct {
		name string
		rec  wsm.Workspace
		went time.Time
		now  time.Time
	}{
		{name: "whose gate has stood under a day", rec: wsm.Workspace{ID: "w1"}, went: at(26, 10), now: at(27, 9)},
		{name: "whose gate has stood exactly a day", rec: wsm.Workspace{ID: "w1"}, went: at(26, 10), now: at(27, 10)},
		{name: "whose gate stood over a day only by counting the weekend", rec: wsm.Workspace{ID: "w1"}, went: at(30, 15), now: at(32, 20)},
		{name: "the user is looking at", rec: wsm.Workspace{ID: "w1", Current: true}, went: at(26, 10), now: at(28, 10)},
		{name: "already hidden", rec: wsm.Workspace{ID: "w1", Hidden: true}, went: at(26, 10), now: at(28, 10)},
		{name: "already moved for this gate", rec: wsm.Workspace{ID: "w1", BackloggedGateAt: ptr(at(26, 10).UTC())}, went: at(26, 10), now: at(28, 10)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tt.now)
			h.gate(tt.rec, tt.went)

			// Act
			h.sweeper.Sweep(context.Background())

			// Assert
			if len(h.backlogs) != 0 {
				t.Fatalf("backlogs = %v, want none", h.backlogs)
			}
		})
	}
}

func TestSweepMovesAShownWorkspaceForANewerGate(t *testing.T) {
	// Arrange: moved once for an older gate, shown again, cold again since.
	h := newHarness(t, at(29, 11))
	h.gate(wsm.Workspace{ID: "w1", BackloggedGateAt: ptr(at(26, 10).UTC())}, at(28, 10))

	// Act
	h.sweeper.Sweep(context.Background())

	// Assert
	want := []backlogCall{{ws: "w1", lapsedAt: at(28, 10).UTC()}}
	if !slices.Equal(h.backlogs, want) {
		t.Fatalf("backlogs = %v, want %v", h.backlogs, want)
	}
}

func TestSweepSkipsAWorkspaceNoLongerRegistered(t *testing.T) {
	// Arrange
	h := newHarness(t, at(28, 10))
	h.gates["w-gone"] = coldAt(at(26, 10))

	// Act
	h.sweeper.Sweep(context.Background())

	// Assert
	if len(h.backlogs) != 0 {
		t.Fatalf("backlogs = %v, want none", h.backlogs)
	}
	if !h.logged("debug", "the workspace behind a stale cold gate is no longer registered; nothing is moved") {
		t.Fatalf("the skip was not recorded: %v", h.log.Records())
	}
}

func TestSweepRecordsAFailedReadAtErrorAndMovesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, at(28, 10))
	h.gate(wsm.Workspace{ID: "w1"}, at(26, 10))
	h.db.err = errors.New("disk I/O error")

	// Act
	h.sweeper.Sweep(context.Background())

	// Assert
	if len(h.backlogs) != 0 {
		t.Fatalf("backlogs = %v, want none", h.backlogs)
	}
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opSweep})
	if !ok || rec.Context["workspace"] != "w1" || rec.Context["cause"] != "disk I/O error" {
		t.Fatalf("the failed read was not recorded at error with its workspace and cause: %v", h.log.Records())
	}
}

func TestSweepGoesOnPastAFailedMove(t *testing.T) {
	// Arrange
	h := newHarness(t, at(28, 10))
	h.gate(wsm.Workspace{ID: "w1"}, at(26, 10))
	h.gate(wsm.Workspace{ID: "w2"}, at(26, 10))
	h.failWith = errors.New("workspace not served here")

	// Act
	h.sweeper.Sweep(context.Background())

	// Assert
	if len(h.backlogs) != 2 {
		t.Fatalf("backlogs = %v, want both stale gates tried", h.backlogs)
	}
	if !h.logged("debug", "the backlog verb did not move the workspace; it is tried again next sweep") {
		t.Fatalf("the failed move was not recorded: %v", h.log.Records())
	}
}

func TestRunSweepsOnEveryBeat(t *testing.T) {
	// Arrange
	h := newHarness(t, at(28, 10))
	h.gate(wsm.Workspace{ID: "w1"}, at(26, 10))
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act
	go func() { done <- h.sweeper.Run(ctx, DefaultEvery) }()
	if got := <-h.clock.Asked; got != DefaultEvery {
		t.Fatalf("first wait = %v, want %v", got, DefaultEvery)
	}
	h.clock.Fire <- at(28, 10)
	<-h.clock.Asked
	cancel()

	// Assert
	if err := <-done; err != nil {
		t.Fatalf("Run = %v, want nil", err)
	}
	if len(h.backlogs) != 1 {
		t.Fatalf("backlogs = %v, want one sweep's move", h.backlogs)
	}
}

func TestTheScheduleIsClockRepeat(t *testing.T) {
	// Act
	repeats := sourcescan.Count(t, "clock.Repeat(")
	waits := sourcescan.Count(t, "Clock.After(") + sourcescan.Count(t, "clock.After(")

	// Assert
	if repeats != 1 || waits != 0 {
		t.Fatalf("clock.Repeat( appears %d times and a hand-rolled .After( wait %d times; want one Repeat and no own wait", repeats, waits)
	}
}

func TestAGateIsDatedByGateLapsedAt(t *testing.T) {
	// Act
	shared := sourcescan.Count(t, "cachelapse.GateLapsedAt(")
	own := sourcescan.Count(t, "GetLastRequestAtMs()")

	// Assert
	if shared != 1 || own != 0 {
		t.Fatalf("cachelapse.GateLapsedAt( appears %d times and an own reading of the last request %d times; want the shared reading alone", shared, own)
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "the state client", strip: func(d *Deps) { d.DB = nil }},
		{name: "the standing gates", strip: func(d *Deps) { d.Gates = nil }},
		{name: "the backlog verb", strip: func(d *Deps) { d.Backlog = nil }},
		{name: "the clock", strip: func(d *Deps) { d.Clock = nil }},
		{name: "the location", strip: func(d *Deps) { d.Location = nil }},
		{name: "the log surfaces", strip: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := newHarness(t, at(26, 10)).deps()
			tt.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatal("New built a sweeper with a collaborator missing")
			}
		})
	}
}

func ptr(t time.Time) *time.Time { return &t }
