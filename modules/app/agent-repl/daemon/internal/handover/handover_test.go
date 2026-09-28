package handover

import (
	"context"
	"path/filepath"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// fakeQueue records the lease re-evaluations the intake asked for.
type fakeQueue struct {
	promptqueue.Queue
	mu       sync.Mutex
	notified []ids.WorkspaceID
}

func (q *fakeQueue) OnLeaseChanged(ws ids.WorkspaceID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.notified = append(q.notified, ws)
}

func (q *fakeQueue) calls() []ids.WorkspaceID {
	q.mu.Lock()
	defer q.mu.Unlock()
	return append([]ids.WorkspaceID(nil), q.notified...)
}

// newIntake arranges an intake over a real state client with one registered
// workspace: the lease row is what the behavior is made of.
func newIntake(t *testing.T) (*Intake, *fakeQueue, wsm.DB, ids.WorkspaceID) {
	t.Helper()
	log := dlog.NewTestSurfaces()
	db, err := wsm.Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), wsm.WithLogger(log.Global()))
	if err != nil {
		t.Fatalf("wsm.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	dir := t.TempDir()
	ws, _, err := db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{
		Name: filepath.Base(dir), Branch: "feature", ParentBranch: "master", RepoDir: dir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	queue := &fakeQueue{}
	intake, err := NewIntake(db, queue, log.Global())
	if err != nil {
		t.Fatalf("NewIntake: %v", err)
	}
	return intake, queue, db, ws.ID
}

// TestQuiesceTakesTheHold covers the outgoing half: from the transfer notice
// on, every arrival is held rather than served.
func TestQuiesceTakesTheHold(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)

	// Act
	if _, err := intake.Quiesce(context.Background(), ws); err != nil {
		t.Fatalf("Quiesce: %v", err)
	}

	// Assert
	lease, held, err := db.Lease(context.Background(), ws)
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if !held || lease.Policy != wsm.PolicyHold {
		t.Fatalf("lease = %+v, held %v, want a hold policy lease", lease, held)
	}
}

// TestQuiesceTellsTheQueue covers the step the hold row alone does not
// accomplish: standing submissions are only re-evaluated when the queue is
// told.
func TestQuiesceTellsTheQueue(t *testing.T) {
	// Arrange
	intake, queue, _, ws := newIntake(t)

	// Act
	if _, err := intake.Quiesce(context.Background(), ws); err != nil {
		t.Fatalf("Quiesce: %v", err)
	}

	// Assert
	if got := queue.calls(); len(got) != 1 || got[0] != ws {
		t.Fatalf("the queue was told %v, want exactly one re-evaluation of %q", got, ws)
	}
}

// TestQuiesceOnAnAlreadyHeldWorkspaceSucceeds covers the state the caller
// asked for already holding: a merge that got there first holds the intake for
// its own reason, and that is quiet enough.
func TestQuiesceOnAnAlreadyHeldWorkspaceSucceeds(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)
	if _, err := db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	_, err := intake.Quiesce(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Quiesce over an existing lease = %v, want success", err)
	}
}

func TestQuiesceAnswersTheLeaseItTook(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)

	// Act
	taken, err := intake.Quiesce(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Quiesce: %v", err)
	}
	lease, held, err := db.Lease(context.Background(), ws)
	if err != nil || !held || lease.ID != taken {
		t.Fatalf("Quiesce answered %q, want the held lease %+v (held %v, %v)", taken, lease, held, err)
	}
}

// TestQuiesceOverAnotherHoldersLeaseAnswersNoLease pins that the caller is
// never handed another holder's lease to release.
func TestQuiesceOverAnotherHoldersLeaseAnswersNoLease(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)
	if _, err := db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	taken, err := intake.Quiesce(context.Background(), ws)

	// Assert
	if err != nil || taken != "" {
		t.Fatalf("Quiesce = (%q, %v), want no lease of its own", taken, err)
	}
}

// TestDrainIntakeReleasesTheHandoverHold covers the incoming half: releasing
// the hold is what drains the held intake.
func TestDrainIntakeReleasesTheHandoverHold(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)
	if _, err := intake.Quiesce(context.Background(), ws); err != nil {
		t.Fatalf("Quiesce: %v", err)
	}

	// Act
	if err := intake.DrainIntake(context.Background(), ws); err != nil {
		t.Fatalf("DrainIntake: %v", err)
	}

	// Assert
	if _, held, err := db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("lease held = %v (err %v) after the drain, want it released", held, err)
	}
}

// TestDrainIntakeLeavesAnotherHoldersLeaseAlone covers the lease this half
// does not own: a merge that was running when the handover began still owns
// the workspace on the successor.
func TestDrainIntakeLeavesAnotherHoldersLeaseAlone(t *testing.T) {
	// Arrange
	intake, _, db, ws := newIntake(t)
	if _, err := db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	if err := intake.DrainIntake(context.Background(), ws); err != nil {
		t.Fatalf("DrainIntake: %v", err)
	}

	// Assert
	lease, held, err := db.Lease(context.Background(), ws)
	if err != nil || !held || lease.Holder != wsm.HolderMerge {
		t.Fatalf("lease = %+v, held %v (err %v), want the merge's lease untouched", lease, held, err)
	}
}

// TestDrainIntakeOnAnUnheldWorkspaceSucceeds covers the adoption of a
// workspace nothing was holding.
func TestDrainIntakeOnAnUnheldWorkspaceSucceeds(t *testing.T) {
	// Arrange
	intake, _, _, ws := newIntake(t)

	// Act
	err := intake.DrainIntake(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("DrainIntake with nothing held = %v, want success", err)
	}
}

// viewFixture is the four real resolvers plus one subscription per topic, so
// a republication is OBSERVED on the topic rather than inferred.
type viewFixture struct {
	topbar  topbar.Resolver
	footer  footer.Resolver
	holds   holds.Resolver
	sidebar sidebar.Resolver

	topbarRows  <-chan *frontendv1.TopbarView
	footerRows  <-chan *frontendv1.FooterView
	holdsRows   <-chan *frontendv1.DaemonHoldTray
	sidebarRows <-chan *frontendv1.WorkspaceRoster
}

// testColors is a fully painted render-colors table: every resolver here
// refuses to serve an unpainted state, which is the guarantee they exist to
// keep.
func testColors() vocab.RenderColors {
	status := map[string]string{}
	for _, arm := range []string{
		"submitting", "thinking", "clearing", "compacting", "permission", "done",
		"interrupted", "turn_failed", "ready", "idle_async", "vendor_blocked", "init", "severed",
		"start_failed", "degraded", "dead", "merge_enqueuing", "merging",
		"merge_queued", "merge_conflict", "merge_failed", "merged", "none",
		"inactive",
	} {
		status[arm] = "grey"
	}
	glyphs := map[string]string{}
	for _, arm := range []string{
		"merge_enqueuing", "merging", "merge_queued", "merge_conflict",
		"merge_failed", "merged",
	} {
		glyphs[arm] = "recycle"
	}
	return vocab.RenderColors{
		RosterStatus: status,
		MergeGlyphs:  glyphs,
		TopbarConnectivity: map[string]string{
			"connected": "green", "connecting": "blue", "severed": "blue",
			"dead": "blue", "no_session": "none",
		},
		TopbarTones: []string{"none", "blue", "purple", "red", "yellow", "green"},
		FooterStatus: map[string]string{
			"disconnected": "grey", "closing": "grey", "interrupted": "grey",
			"loading": "grey", "blocked": "grey", "merging": "grey",
			"waiting": "grey", "thinking": "grey", "background": "grey",
			"idle": "grey", "merge_conflict": "grey", "merge_failed": "grey",
			"merged": "grey",
		},
		FooterAllowance: map[string]string{
			"allowed": "grey", "allowed_warning": "grey", "rejected": "grey",
		},
	}
}

// waitDeadline bounds every wait below. It is a FAILURE deadline, never a
// synchronization device: each wait returns the moment its value arrives.
const waitDeadline = 5 * time.Second

// newViewFixture arranges the four resolvers with the workspace bound and one
// value published on each, then subscribes and consumes that first delivery,
// so every later delivery is a republication.
func newViewFixture(t *testing.T, ws ids.WorkspaceID) *viewFixture {
	t.Helper()
	log := dlog.NewTestSurfaces()
	dir := t.TempDir()

	top, err := topbar.New(testColors(), log)
	if err != nil {
		t.Fatalf("topbar.New: %v", err)
	}
	foot, err := footer.New(testColors(), log)
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	tray, err := holds.New(log)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}
	roster, err := sidebar.New(testColors(), log)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	for _, bind := range []func(ids.WorkspaceID, string) error{top.SetWorkspaceDir, foot.SetWorkspaceDir, tray.SetWorkspaceDir} {
		if err := bind(ws, dir); err != nil {
			t.Fatalf("SetWorkspaceDir: %v", err)
		}
	}

	// One publication on each topic, so every one holds a latest value. They
	// are published DIRECTLY: what a resolver needs before its own view is
	// complete is that resolver's contract, and this fixture is about the
	// republication rather than about any of them.
	top.Topic(ws).Publish(&frontendv1.TopbarView{})
	foot.Topic(ws).Publish(&frontendv1.FooterView{})
	tray.Topic(ws).Publish(&frontendv1.DaemonHoldTray{})
	roster.Topic().Publish(&frontendv1.WorkspaceRoster{})

	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	f := &viewFixture{
		topbar: top, footer: foot, holds: tray, sidebar: roster,
		topbarRows:  top.Topic(ws).Subscribe(ctx),
		footerRows:  foot.Topic(ws).Subscribe(ctx),
		holdsRows:   tray.Topic(ws).Subscribe(ctx),
		sidebarRows: roster.Topic().Subscribe(ctx),
	}
	// Subscribe delivers the latest value first; consuming it leaves every
	// later delivery a republication.
	awaitTopbar(t, f.topbarRows)
	awaitFooter(t, f.footerRows)
	awaitHolds(t, f.holdsRows)
	awaitRoster(t, f.sidebarRows)
	return f
}

func awaitTopbar(t *testing.T, ch <-chan *frontendv1.TopbarView) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(waitDeadline):
		t.Fatal("the topbar topic delivered nothing")
	}
}

func awaitFooter(t *testing.T, ch <-chan *frontendv1.FooterView) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(waitDeadline):
		t.Fatal("the footer topic delivered nothing")
	}
}

func awaitHolds(t *testing.T, ch <-chan *frontendv1.DaemonHoldTray) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(waitDeadline):
		t.Fatal("the hold tray topic delivered nothing")
	}
}

func awaitRoster(t *testing.T, ch <-chan *frontendv1.WorkspaceRoster) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(waitDeadline):
		t.Fatal("the roster topic delivered nothing")
	}
}

// TestPublishViewsRepublishesEveryStandingView covers the repaint an adoption
// owes a re-attached client: all four standing views are pushed again.
func TestPublishViewsRepublishesEveryStandingView(t *testing.T) {
	// Arrange
	ws := ids.WorkspaceID("ws-1")
	f := newViewFixture(t, ws)
	views, err := NewViews(f.topbar, f.footer, f.holds, f.sidebar, dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("NewViews: %v", err)
	}

	// Act
	if err := views.PublishViews(context.Background(), ws); err != nil {
		t.Fatalf("PublishViews: %v", err)
	}

	// Assert
	awaitTopbar(t, f.topbarRows)
	awaitFooter(t, f.footerRows)
	awaitHolds(t, f.holdsRows)
	awaitRoster(t, f.sidebarRows)
}

// TestPublishViewsSkipsAViewNothingHasPublished covers the view with no value:
// an empty repaint drawn over a client's real one is worse than leaving it
// alone, so nothing is published and the record says so.
func TestPublishViewsSkipsAViewNothingHasPublished(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	surfaces := dlog.NewTestSurfaces()
	top, err := topbar.New(testColors(), surfaces)
	if err != nil {
		t.Fatalf("topbar.New: %v", err)
	}
	foot, err := footer.New(testColors(), surfaces)
	if err != nil {
		t.Fatalf("footer.New: %v", err)
	}
	tray, err := holds.New(surfaces)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}
	roster, err := sidebar.New(testColors(), surfaces)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	views, err := NewViews(top, foot, tray, roster, log)
	if err != nil {
		t.Fatalf("NewViews: %v", err)
	}

	// Act
	if err := views.PublishViews(context.Background(), ids.WorkspaceID("ws-1")); err != nil {
		t.Fatalf("PublishViews: %v", err)
	}

	// Assert
	var found bool
	for _, r := range log.Records() {
		if r.Operation == opPublish && r.Context["views"] == 0 {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want a republish record naming zero views", log.Records())
	}
}

// TestRepublishReportsAnEmptyTopic covers the helper's own answer, which is
// what makes the skip above legible in the record.
func TestRepublishReportsAnEmptyTopic(t *testing.T) {
	// Arrange
	var topic publish.Topic[int]

	// Act
	got := republish(&topic)

	// Assert
	if got {
		t.Fatal("republish reported a publication on a topic that never had one")
	}
}
