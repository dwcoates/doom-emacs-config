package boot

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// instant is the fixed instant every test stamps with: no test in this package
// reads the wall clock.
var instant = time.Date(2026, 9, 1, 9, 0, 0, 0, time.UTC)

// fakeSupervisor answers Adopt from a script. Spawn is deliberately absent: a
// boot that spawned a shim would have killed and restarted a conversation the
// whole sequence exists to preserve, so a call to it fails the test.
type fakeSupervisor struct {
	shimclient.Supervisor
	mu sync.Mutex
	// adopted records every workspace Adopt was called for, in order.
	adopted []ids.WorkspaceID
	// dialed records the socket path each Adopt was handed, which is what the
	// generation resolution's own test reads back.
	dialed []string
	// err, when set, is what Adopt answers instead of a client.
	err error
	// hang makes Adopt wait out its context instead of answering, which is
	// what the real supervisor does for a survivor whose lock reads HELD and
	// whose socket path is gone: shimclient.bringUp redials that forever.
	hang bool
	// standingDown is the supervisor's stand-down latch.
	standingDown bool
}

func (s *fakeSupervisor) Adopt(ctx context.Context, ws ids.WorkspaceID, _, udsPath string) (shimclient.Client, error) {
	s.mu.Lock()
	s.adopted = append(s.adopted, ws)
	s.dialed = append(s.dialed, udsPath)
	hang, err := s.hang, s.err
	s.mu.Unlock()
	if hang {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	if err != nil {
		return nil, err
	}
	return &fakeShimClient{pid: adoptedShimPID}, nil
}

// adoptedShimPID is the pid every adopted fake shim answers, so the bounce
// accounting's record of WHICH process survived has something real to name.
const adoptedShimPID = 4242

// fakeShimClient is an adopted shim client. Only PID is answered: the boot
// sequence reads nothing else off an adopted client, and a verb it does not
// call has no honest fake.
type fakeShimClient struct {
	shimclient.Client
	pid int
}

func (c *fakeShimClient) PID() int { return c.pid }

// hasRecord reports whether the boot logged a record at level under operation.
func (h *harness) hasRecord(level, operation string) bool {
	for _, r := range h.log.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

func (s *fakeSupervisor) paths() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.dialed...)
}

func (s *fakeSupervisor) calls() []ids.WorkspaceID {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]ids.WorkspaceID(nil), s.adopted...)
}

// fakeQueue is the prompt queue's restore half and its orphan door. Every
// other verb is absent: the boot sequence calls exactly these two.
type fakeQueue struct {
	promptqueue.Queue
	mu  sync.Mutex
	err error
	// db answers the state client the boot was built with, so the door closes
	// orphans on the very store (or failing store) the test arranged.
	db func() wsm.DB
	// leaseChanged records every OnLeaseChanged, in order.
	leaseChanged []wsm.WorkspaceID
	// reconnects records every ReleaseReconnectHolds, in order.
	reconnects []wsm.WorkspaceID
}

// ReleaseReconnectHolds records the release the boot asked for.
func (q *fakeQueue) ReleaseReconnectHolds(ws wsm.WorkspaceID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.reconnects = append(q.reconnects, ws)
}

// CloseOrphans is the queue's door, closing on the boot's own state client.
func (q *fakeQueue) CloseOrphans(ctx context.Context, ws wsm.WorkspaceID, at time.Time) (wsm.OrphanReport, error) {
	return q.db().CloseOrphans(ctx, ws, at)
}

func (q *fakeQueue) RestoreHolds(context.Context) error {
	q.mu.Lock()
	defer q.mu.Unlock()
	return q.err
}

// OnLeaseChanged records the workspaces whose lease set the boot changed.
func (q *fakeQueue) OnLeaseChanged(ws wsm.WorkspaceID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.leaseChanged = append(q.leaseChanged, ws)
}

// leaseChanges answers the workspaces the queue was told about, in order.
func (q *fakeQueue) leaseChanges() []wsm.WorkspaceID {
	q.mu.Lock()
	defer q.mu.Unlock()
	return append([]wsm.WorkspaceID(nil), q.leaseChanged...)
}

// fakeMerge is the merge orchestrator's recovery half.
type fakeMerge struct {
	merge.Orchestrator
	mu        sync.Mutex
	recovered int
	err       error
}

func (m *fakeMerge) Recover(context.Context) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.recovered++
	return m.err
}

func (m *fakeMerge) recoveries() int {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.recovered
}

// fakeRollout answers the two halves the boot uses: the manifest
// reconciliation every boot runs, and the join a successor runs.
type fakeRollout struct {
	rollout.Controller
	mu sync.Mutex
	// bound reports whether the boot had bound the views when Reconcile ran,
	// read through boundNow.
	boundNow         func() bool
	boundAtReconcile bool
	// dispositions is what Reconcile answers.
	dispositions []rollout.Disposition
	reconcileErr error
	joins        int
	joinErr      error
	// adopted and unadopted are the survivor sets the boot handed Reconcile,
	// which is what the no-manifest accounting keys on.
	adopted   []rollout.AdoptedSession
	unadopted []rollout.UnadoptedSession
	// onReconcile runs inside Reconcile, where the real controller records
	// the bounce dispositions as faults.
	onReconcile func()
	// takenUp is the adopted set TakeUpCarries was handed, takeUpErr what it
	// answers, and onTakeUp / onFinish run inside TakeUpCarries and
	// FinishCarries, so a test reads what the boot had done by then.
	takenUp   []wsm.WorkspaceID
	takeUpErr error
	onTakeUp  func()
	finishes  int
	onFinish  func()
}

func (r *fakeRollout) TakeUpCarries(_ context.Context, adopted []wsm.WorkspaceID) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.takenUp = append([]wsm.WorkspaceID(nil), adopted...)
	if r.onTakeUp != nil {
		r.onTakeUp()
	}
	return r.takeUpErr
}

func (r *fakeRollout) FinishCarries(context.Context) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.finishes++
	if r.onFinish != nil {
		r.onFinish()
	}
}

func (r *fakeRollout) Reconcile(_ context.Context, survivors rollout.Survivors) ([]rollout.Disposition, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.adopted = append([]rollout.AdoptedSession(nil), survivors.Adopted...)
	r.unadopted = append([]rollout.UnadoptedSession(nil), survivors.Unadopted...)
	if r.onReconcile != nil {
		r.onReconcile()
	}
	if r.boundNow != nil {
		r.boundAtReconcile = r.boundNow()
	}
	if r.reconcileErr != nil {
		return nil, r.reconcileErr
	}
	return append([]rollout.Disposition(nil), r.dispositions...), nil
}

func (r *fakeRollout) Join(context.Context) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.joins++
	return r.joinErr
}

func (r *fakeRollout) joinCalls() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.joins
}

// failingHolds is a state client whose held-prompt read fails, which is how a
// CORRUPT durable record manifests: the whole load refuses and nothing is
// restored.
type failingHolds struct {
	wsm.DB
	err error
}

func (d failingHolds) AllHeldPrompts(context.Context) ([]wsm.HeldPrompt, error) {
	return nil, d.err
}

// bringingEdge is one BringingUp call the boot made.
type bringingEdge struct {
	ws       ids.WorkspaceID
	underWay bool
}

// harness is one boot sequence under test with every fake reachable.
type harness struct {
	// binds counts the boot's BindViews calls; bindErr is what they answer.
	binds   atomic.Int32
	bindErr error
	// restore answers the boot's missing-worktree restorer; nil answers
	// "nothing to restore from", which is what every arrangement that does
	// not name a branch means. restoreAsked records every workspace it was
	// asked about, in order.
	restore      func(wsm.Workspace) (bool, error)
	restoreAsked []ids.WorkspaceID
	seq          Sequence
	deps         Deps
	db           wsm.DB
	supervisor   *fakeSupervisor
	queue        *fakeQueue
	merge        *fakeMerge
	rollout      *fakeRollout
	log          *dlog.TestSurfaces
	// installed records every workspace whose adopted client was installed.
	installed []ids.WorkspaceID
	// sessionsAdopted records every adopted survivor stated to hold a session.
	sessionsAdopted []ids.WorkspaceID
	// probes is the scripted lock state per workspace directory.
	probes map[string]sessionlock.State
	// probeErrs is the scripted probe error per workspace directory.
	probeErrs map[string]error
	// socketProbes is the scripted listener state per socket path. An unset
	// path answers ABSENT, which is the ordinary "nothing ever bound here".
	socketProbes map[string]shimsocket.State
	// socketProbeErrs is the scripted socket-probe error per socket path.
	socketProbeErrs map[string]error
	// bringingMu guards bringing: the bring-up lowers it from concurrent
	// goroutines.
	bringingMu sync.Mutex
	// bringing records every BringingUp edge, in order.
	bringing []bringingEdge
	// unserved records every workspace the reconciliation marked unserved.
	unserved []ids.WorkspaceID
	// ensures counts the bring-up's EnsureServices calls; ensureErr is what
	// they answer.
	ensures   atomic.Int32
	ensureErr error
	// startedMu guards started: the bring-up starts its workspaces on
	// concurrent goroutines.
	startedMu sync.Mutex
	// started records every workspace the bring-up started, in the order the
	// starts happened to begin.
	started []ids.WorkspaceID
	// startErrs is the scripted start failure per workspace.
	startErrs map[ids.WorkspaceID]error
}

// newHarness builds a boot sequence over a REAL WSM store in the test's temp
// dir: the registry, the leases and the held prompts are what the sequence's
// behavior is made of, so they are exercised for real rather than faked.
func newHarness(t *testing.T, adjust ...func(*Deps, *harness)) *harness {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")

	log := dlog.NewTestSurfaces()
	// A SHORT state root, not t.TempDir(): the layout's shim socket paths live
	// under it, t.TempDir() embeds the test's name, and an AF_UNIX path over
	// the kernel's 104-byte sun_path limit cannot be bound or dialed at all —
	// which would make a socket probe answer "undetermined" for a reason that
	// has nothing to do with the behavior under test.
	root, err := os.MkdirTemp("", "boot")
	if err != nil {
		t.Fatalf("temp state root: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	db, err := wsm.Open(context.Background(), filepath.Join(root, "wsm.db"), wsm.WithLogger(log.Global()))
	if err != nil {
		t.Fatalf("wsm.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })

	layout, err := stateroot.Root(root, "")
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	h := &harness{
		db:         db,
		supervisor: &fakeSupervisor{},
		queue:      &fakeQueue{},
		merge:      &fakeMerge{},
		rollout:    &fakeRollout{},
		log:        log,
		probes:     map[string]sessionlock.State{},
		probeErrs:  map[string]error{},

		socketProbes:    map[string]shimsocket.State{},
		socketProbeErrs: map[string]error{},
		startErrs:       map[ids.WorkspaceID]error{},
	}
	h.rollout.boundNow = func() bool { return h.binds.Load() > 0 }
	h.queue.db = func() wsm.DB { return h.deps.DB }
	h.deps = Deps{
		BindViews: func(context.Context) error {
			h.binds.Add(1)
			return h.bindErr
		},
		RestoreMissingWorktree: func(_ context.Context, ws wsm.Workspace) (bool, error) {
			h.restoreAsked = append(h.restoreAsked, ws.ID)
			if h.restore == nil {
				return false, nil
			}
			return h.restore(ws)
		},
		Layout:     layout,
		DB:         db,
		Supervisor: h.supervisor,
		Queue:      h.queue,
		Merge:      h.merge,
		Rollout:    h.rollout,
		RunDir:     filepath.Join(root, "run"),
		Probe: func(_, dir string) (sessionlock.State, error) {
			if err, ok := h.probeErrs[dir]; ok {
				return sessionlock.StateUnknown, err
			}
			state, ok := h.probes[dir]
			if !ok {
				return sessionlock.StateFree, nil
			}
			return state, nil
		},
		SocketProbe: func(path string) (shimsocket.State, error) {
			if err, ok := h.socketProbeErrs[path]; ok {
				return shimsocket.StateUndetermined, err
			}
			state, ok := h.socketProbes[path]
			if !ok {
				return shimsocket.StateAbsent, nil
			}
			return state, nil
		},
		Adopted: func(_ context.Context, ws ids.WorkspaceID, _ shimclient.Client) error {
			h.installed = append(h.installed, ws)
			return nil
		},
		SessionAdopted: func(ws ids.WorkspaceID) { h.sessionsAdopted = append(h.sessionsAdopted, ws) },
		StartSession: func(_ context.Context, ws ids.WorkspaceID) error {
			h.noteStarted(ws)
			return h.startErrs[ws]
		},
		BringingUp: func(ws ids.WorkspaceID, underWay bool) {
			h.bringingMu.Lock()
			defer h.bringingMu.Unlock()
			h.bringing = append(h.bringing, bringingEdge{ws, underWay})
		},
		Unserved: func(ws ids.WorkspaceID) {
			h.unserved = append(h.unserved, ws)
		},
		EnsureServices: func(context.Context) error {
			h.ensures.Add(1)
			return h.ensureErr
		},
		Now: func() time.Time { return instant },
		Log: log,
	}
	for _, a := range adjust {
		a(&h.deps, h)
	}
	seq, err := New(h.deps)
	if err != nil {
		t.Fatalf("boot.New: %v", err)
	}
	h.seq = seq
	return h
}

// register puts one workspace in the registry and scripts its lock state.
func (h *harness) register(t *testing.T, dir string, state sessionlock.State) wsm.Workspace {
	t.Helper()
	ws, _, err := h.db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{Branch: "feature", ParentBranch: "master", RepoDir: dir, DefaultBranch: "master"})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", dir, err)
	}
	h.probes[ws.Dir] = state
	return ws
}

// hibernate records the idle sweep's own terminal on a workspace's session,
// which is what makes it read as asleep to everything that asks.
func (h *harness) hibernate(t *testing.T, ws wsm.Workspace) {
	t.Helper()
	ctx := context.Background()
	if err := h.db.PutSession(ctx, wsm.Session{
		Workspace: ws.ID, HostSessionID: "host-" + string(ws.ID), StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutSession(%s): %v", ws.ID, err)
	}
	if err := h.db.SetSessionTerminal(ctx, ws.ID, wsm.SessionTerminal{
		Kind: wsm.TerminalHibernated, Detail: "idle past the cutoff", At: instant,
	}); err != nil {
		t.Fatalf("SetSessionTerminal(%s): %v", ws.ID, err)
	}
}

// userSaid composes a one-block text submission.
func userSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// errBoom is the failure every scripted collaborator answers with.
var errBoom = errors.New("boom")

// minimalDeps is one harness's Deps, for the constructor's own tests: they
// vary a single optional collaborator and never run the sequence.
func minimalDeps(t *testing.T) Deps {
	t.Helper()
	return newHarness(t).deps
}

// failingList is a state client whose workspace registry cannot be read, which
// is the first thing a boot does and the one failure that stops it before any
// reconciliation runs.
type failingList struct {
	wsm.DB
	err error
}

func (d failingList) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	return nil, d.err
}

// failingCloseOrphans is a state client whose orphan-closing transaction
// fails: a client-less workspace's turns would be left without a terminal.
type failingCloseOrphans struct {
	wsm.DB
	err error
}

func (d failingCloseOrphans) CloseOrphans(context.Context, wsm.WorkspaceID, time.Time) (wsm.OrphanReport, error) {
	return wsm.OrphanReport{}, d.err
}

// failingLease is a state client whose occupancy lease cannot be read, which
// is how a boot fails to tell whether a merge was in flight across the crash.
type failingLease struct {
	wsm.DB
	err error
}

func (d failingLease) Lease(context.Context, wsm.WorkspaceID) (wsm.Lease, bool, error) {
	return wsm.Lease{}, false, d.err
}

// previousProcessLease takes a lease on ws through a SECOND state handle on the
// boot's database file, standing for a previous daemon process that died
// without its orderly close: the handle is never closed through Close, which
// would release what it holds.
func (h *harness) previousProcessLease(t *testing.T, ws wsm.WorkspaceID, holder wsm.LeaseHolder) wsm.Lease {
	t.Helper()
	previous, err := wsm.Open(context.Background(), h.deps.Layout.DB())
	if err != nil {
		t.Fatalf("wsm.Open for the previous process: %v", err)
	}
	t.Cleanup(func() { previous.Close() })
	lease, err := previous.AcquireLease(context.Background(), ws, holder, wsm.PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease by the previous process: %v", err)
	}
	return lease
}

// failingRelease is a state client whose lease release fails, which is how a
// boot fails to clear a hold nobody owns.
type failingRelease struct {
	wsm.DB
	err error
}

func (d failingRelease) ReleaseLease(context.Context, wsm.LeaseID) error { return d.err }

// failingForeign is a state client whose lease table cannot be read.
type failingForeign struct {
	wsm.DB
	err error
}

func (d failingForeign) ForeignLeases(context.Context) ([]wsm.Lease, error) { return nil, d.err }

// StandDownEverySpawn is the supervisor's own sweep of processes it started
// and still owns. These fakes spawn no process, so there is never one to
// sweep.
func (s *fakeSupervisor) StandDownEverySpawn(context.Context, string) error { return nil }

// BeginStandDown latches the fake supervisor's stand-down, answering whether
// this call was the one that latched it.
func (s *fakeSupervisor) BeginStandDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.standingDown {
		return false
	}
	s.standingDown = true
	return true
}

// SpawnedFor answers the supervisor's live spawn registry. These fakes spawn
// no process, so this daemon owns no spawn for any workspace.
func (s *fakeSupervisor) SpawnedFor(ids.WorkspaceID) (int, bool) { return 0, false }

// StandingDown answers the fake supervisor's latch.
func (s *fakeSupervisor) StandingDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.standingDown
}

// noteStarted records that the bring-up started ws.
func (h *harness) noteStarted(ws ids.WorkspaceID) {
	h.startedMu.Lock()
	defer h.startedMu.Unlock()
	h.started = append(h.started, ws)
}

// runAndBringUp runs the reconciliation and then the bring-up step over the
// set it named, which is the order the daemon runs them in: the
// reconciliation, then the listener, then the bring-up. It is a helper because
// the two halves are one boot as far as every bring-up test is concerned; the
// tests whose subject is the SPLIT itself call the two directly.
func (h *harness) runAndBringUp(t *testing.T) (Report, BringUpReport) {
	t.Helper()
	report, err := h.seq.Run(context.Background())
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	return report, h.seq.BringUp(context.Background(), report.PendingBringUp)
}
