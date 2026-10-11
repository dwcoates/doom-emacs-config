package branchcommits

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

const theWorkspace ids.WorkspaceID = "ws-1"

type fakeDB struct {
	mu       sync.Mutex
	records  map[ids.WorkspaceID]wsm.Workspace
	readErr  error
	writeErr error
	// mergedOnWrite answers the live write as refused, as the store does for
	// a workspace that merged while it was counted.
	mergedOnWrite bool
	writes        []uint32
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

func (f *fakeDB) SetLiveCommitCount(_ context.Context, _ wsm.WorkspaceID, count uint32) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.writeErr != nil {
		return false, f.writeErr
	}
	if f.mergedOnWrite {
		return false, nil
	}
	f.writes = append(f.writes, count)
	return true, nil
}

func (f *fakeDB) written() []uint32 {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]uint32(nil), f.writes...)
}

type fakeGit struct {
	mu    sync.Mutex
	count uint32
	err   error
	asked [][3]string
	// hold, when set, blocks each count until a value arrives on it; entered
	// is told as each count starts.
	hold    chan struct{}
	entered chan struct{}
}

func (g *fakeGit) CountCommits(_ context.Context, dir, base, tip string) (uint32, error) {
	g.mu.Lock()
	g.asked = append(g.asked, [3]string{dir, base, tip})
	hold, entered := g.hold, g.entered
	g.mu.Unlock()
	if entered != nil {
		entered <- struct{}{}
	}
	if hold != nil {
		<-hold
	}
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.count, g.err
}

type harness struct {
	db        *fakeDB
	git       *fakeGit
	log       *dlog.TestSurfaces
	published chan struct{}
	publishOK error
	r         *Recounter
}

func live() wsm.Workspace {
	return wsm.Workspace{ID: theWorkspace, Dir: "/work/ws-1", Branch: "feature", ParentBranch: "master"}
}

func newHarness(t *testing.T, rec wsm.Workspace) *harness {
	t.Helper()
	h := &harness{
		db:        &fakeDB{records: map[ids.WorkspaceID]wsm.Workspace{rec.ID: rec}},
		git:       &fakeGit{count: 4},
		log:       dlog.NewTestSurfaces(),
		published: make(chan struct{}, 16),
	}
	r, err := New(Deps{
		DB:  h.db,
		Git: h.git,
		PublishRegistry: func(context.Context) error {
			h.published <- struct{}{}
			return h.publishOK
		},
		Log: h.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.r = r
	return h
}

func (h *harness) record(level, message string) (dlog.Record, bool) {
	for _, rec := range h.log.Records() {
		if rec.Level == level && rec.Operation == opRecount && rec.Message == message {
			return rec, true
		}
	}
	return dlog.Record{}, false
}

func countPtr(n uint32) *uint32 { return &n }

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := func() Deps {
		return Deps{DB: &fakeDB{}, Git: &fakeGit{}, PublishRegistry: func(context.Context) error { return nil }, Log: dlog.NewTestSurfaces()}
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "state client", strip: func(d *Deps) { d.DB = nil }},
		{name: "git", strip: func(d *Deps) { d.Git = nil }},
		{name: "republish", strip: func(d *Deps) { d.PublishRegistry = nil }},
		{name: "log", strip: func(d *Deps) { d.Log = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatalf("New without a %s succeeded", tc.name)
			}
		})
	}
}

func TestRecountRecordsAndRepublishesAMovedCount(t *testing.T) {
	tests := []struct {
		name   string
		stored *uint32
		count  uint32
	}{
		{name: "a first count", stored: nil, count: 4},
		{name: "a count that moved", stored: countPtr(2), count: 4},
		{name: "a count that fell to zero", stored: countPtr(2), count: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			rec := live()
			rec.CommitCount = tc.stored
			h := newHarness(t, rec)
			h.git.count = tc.count

			// Act
			h.r.recount(context.Background(), theWorkspace)

			// Assert
			if got := h.db.written(); len(got) != 1 || got[0] != tc.count {
				t.Fatalf("writes = %v, want [%d]", got, tc.count)
			}
			if len(h.published) != 1 {
				t.Fatalf("the roster was republished %d times, want once", len(h.published))
			}
		})
	}
}

func TestRecountCountsTheBranchAgainstItsParent(t *testing.T) {
	// Arrange
	h := newHarness(t, live())

	// Act
	h.r.recount(context.Background(), theWorkspace)

	// Assert
	if len(h.git.asked) != 1 || h.git.asked[0] != [3]string{"/work/ws-1", "master", "feature"} {
		t.Fatalf("git was asked %v, want master..feature in the workspace's dir", h.git.asked)
	}
}

func TestRecountSkips(t *testing.T) {
	merged := time.Unix(1_700_000_000, 0)
	tests := []struct {
		name    string
		rec     func() wsm.Workspace
		message string
		asksGit bool
	}{
		{name: "a merged workspace", rec: func() wsm.Workspace { r := live(); r.MergedAt = &merged; return r }, message: "the workspace has merged; it keeps the count its merge landed"},
		{name: "a workspace with no branch", rec: func() wsm.Workspace { r := live(); r.Branch = ""; return r }, message: "the workspace names no branch pair to count between"},
		{name: "a workspace with no parent branch", rec: func() wsm.Workspace { r := live(); r.ParentBranch = ""; return r }, message: "the workspace names no branch pair to count between"},
		{name: "an unchanged count", rec: func() wsm.Workspace { r := live(); r.CommitCount = countPtr(4); return r }, message: "the branch's commit count is unchanged", asksGit: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, tc.rec())

			// Act
			h.r.recount(context.Background(), theWorkspace)

			// Assert
			if got := h.db.written(); len(got) != 0 {
				t.Fatalf("writes = %v, want none", got)
			}
			if len(h.published) != 0 {
				t.Fatal("the roster was republished for a skipped recount")
			}
			if (len(h.git.asked) > 0) != tc.asksGit {
				t.Fatalf("git asked = %v, want asked %v", h.git.asked, tc.asksGit)
			}
			if _, ok := h.record("debug", tc.message); !ok {
				t.Fatalf("no debug %q record: %v", tc.message, h.log.Records())
			}
		})
	}
}

func TestRecountOfAForgottenWorkspaceWritesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, live())

	// Act
	h.r.recount(context.Background(), "gone")

	// Assert
	if len(h.git.asked) != 0 || len(h.db.written()) != 0 {
		t.Fatalf("a forgotten workspace was counted: git %v, writes %v", h.git.asked, h.db.written())
	}
	if rec, ok := h.record("debug", "a workspace owed a recount is no longer registered"); !ok || rec.Context["workspace"] != "gone" {
		t.Fatalf("no debug record naming the forgotten workspace: %v", h.log.Records())
	}
}

func TestRecountWhoseWriteIsRefusedForAMergeDoesNotRepublish(t *testing.T) {
	// Arrange
	h := newHarness(t, live())
	h.db.mergedOnWrite = true

	// Act
	h.r.recount(context.Background(), theWorkspace)

	// Assert
	if len(h.published) != 0 {
		t.Fatal("the roster was republished for a refused write")
	}
	if _, ok := h.record("debug", "the workspace merged while its branch was counted; the landed count stands"); !ok {
		t.Fatalf("no record of the refused write: %v", h.log.Records())
	}
}

func TestRecountErrors(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*harness)
		message string
		writes  int
		publish int
	}{
		{name: "the record will not read", arrange: func(h *harness) { h.db.readErr = errors.New("disk I/O error") }, message: "could not read the workspace owed a recount"},
		{name: "git fails", arrange: func(h *harness) { h.git.err = errors.New("fatal: bad revision") }, message: "could not count the branch's commits; the panel keeps the last count"},
		{name: "the write fails", arrange: func(h *harness) { h.db.writeErr = errors.New("database is locked") }, message: "could not record the branch's commit count"},
		{name: "the republish fails", arrange: func(h *harness) { h.publishOK = errors.New("verbs not bound") }, message: "could not republish the roster after a recount", writes: 1, publish: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, live())
			tc.arrange(h)

			// Act
			h.r.recount(context.Background(), theWorkspace)

			// Assert
			rec, ok := h.record("error", tc.message)
			if !ok {
				t.Fatalf("no error %q record: %v", tc.message, h.log.Records())
			}
			if rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
				t.Fatalf("error record context = %v, want the workspace and the cause", rec.Context)
			}
			if got := len(h.db.written()); got != tc.writes {
				t.Fatalf("writes = %d, want %d", got, tc.writes)
			}
			if got := len(h.published); got != tc.publish {
				t.Fatalf("republishes = %d, want %d", got, tc.publish)
			}
		})
	}
}

func TestTurnClosedOwesEachWorkspaceOnceInOrder(t *testing.T) {
	// Arrange
	h := newHarness(t, live())

	// Act
	h.r.TurnClosed("a")
	h.r.TurnClosed("b")
	h.r.TurnClosed("a")

	// Assert
	var got []ids.WorkspaceID
	for {
		ws, ok := h.r.next()
		if !ok {
			break
		}
		got = append(got, ws)
	}
	if len(got) != 2 || got[0] != "a" || got[1] != "b" {
		t.Fatalf("owed = %v, want [a b]", got)
	}
}

func TestRunRecountsAfterATurnCloses(t *testing.T) {
	// Arrange
	h := newHarness(t, live())
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- h.r.Run(ctx) }()

	// Act
	h.r.TurnClosed(theWorkspace)
	<-h.published
	cancel()

	// Assert
	if err := <-done; err != nil {
		t.Fatalf("Run = %v, want nil once its context ends", err)
	}
	if got := h.db.written(); len(got) != 1 || got[0] != 4 {
		t.Fatalf("writes = %v, want [4]", got)
	}
}

func TestRunRecountsAgainForACloseDuringACount(t *testing.T) {
	// Arrange: the first count is held in flight.
	h := newHarness(t, live())
	h.git.hold, h.git.entered = make(chan struct{}), make(chan struct{}, 2)
	go h.r.Run(t.Context())
	h.r.TurnClosed(theWorkspace)
	<-h.git.entered

	// Act: a second close lands while the first count runs.
	h.r.TurnClosed(theWorkspace)
	h.git.hold <- struct{}{}
	<-h.published
	<-h.git.entered
	h.git.hold <- struct{}{}

	// Assert: the second close was counted after it, not folded into the first.
	h.git.mu.Lock()
	asked := len(h.git.asked)
	h.git.mu.Unlock()
	if asked != 2 {
		t.Fatalf("git was asked %d times, want twice", asked)
	}
}
