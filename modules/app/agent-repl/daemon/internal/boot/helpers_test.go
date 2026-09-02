package boot

import (
	"context"
	"errors"
	"path/filepath"
	"sync"
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
	// err, when set, is what Adopt answers instead of a client.
	err error
}

func (s *fakeSupervisor) Adopt(_ context.Context, ws ids.WorkspaceID, _, _ string) (shimclient.Client, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.adopted = append(s.adopted, ws)
	if s.err != nil {
		return nil, s.err
	}
	return nil, nil
}

func (s *fakeSupervisor) calls() []ids.WorkspaceID {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]ids.WorkspaceID(nil), s.adopted...)
}

// fakeQueue is the prompt queue's restore half. Every other verb is absent:
// the boot sequence calls exactly one of them.
type fakeQueue struct {
	promptqueue.Queue
	mu       sync.Mutex
	restored int
	err      error
}

func (q *fakeQueue) RestoreHolds(context.Context) error {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.restored++
	return q.err
}

func (q *fakeQueue) restores() int {
	q.mu.Lock()
	defer q.mu.Unlock()
	return q.restored
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
	// dispositions is what Reconcile answers.
	dispositions []rollout.Disposition
	reconcileErr error
	joins        int
	joinErr      error
	// adopted is the adopted set the boot handed Reconcile, which is what the
	// no-manifest accounting keys on.
	adopted []ids.WorkspaceID
}

func (r *fakeRollout) Reconcile(_ context.Context, adopted []ids.WorkspaceID) ([]rollout.Disposition, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.adopted = append([]ids.WorkspaceID(nil), adopted...)
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

// harness is one boot sequence under test with every fake reachable.
type harness struct {
	seq        Sequence
	deps       Deps
	db         wsm.DB
	supervisor *fakeSupervisor
	queue      *fakeQueue
	merge      *fakeMerge
	rollout    *fakeRollout
	log        *dlog.TestSurfaces
	// installed records every workspace whose adopted client was installed.
	installed []ids.WorkspaceID
	// probes is the scripted lock state per workspace directory.
	probes map[string]sessionlock.State
	// probeErrs is the scripted probe error per workspace directory.
	probeErrs map[string]error
}

// newHarness builds a boot sequence over a REAL WSM store in the test's temp
// dir: the registry, the leases and the held prompts are what the sequence's
// behavior is made of, so they are exercised for real rather than faked.
func newHarness(t *testing.T, adjust ...func(*Deps, *harness)) *harness {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")

	log := dlog.NewTestSurfaces()
	root := t.TempDir()
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
	}
	h.deps = Deps{
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
		Adopted: func(_ context.Context, ws ids.WorkspaceID, _ shimclient.Client) error {
			h.installed = append(h.installed, ws)
			return nil
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
