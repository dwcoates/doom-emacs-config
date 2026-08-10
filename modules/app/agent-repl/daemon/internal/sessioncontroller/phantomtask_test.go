package sessioncontroller

import (
	"errors"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
)

// phantomHarness is one wired workspace whose clock the test moves, which is
// what makes the grace observable without waiting for it.
type phantomHarness struct {
	t      *testing.T
	m      *Manager
	push   *fakePusher
	client *fakeClient
	mu     sync.Mutex
	nowMs  int64
}

func newPhantomHarness(t *testing.T) *phantomHarness {
	t.Helper()
	h := &phantomHarness{t: t, push: &fakePusher{}, nowMs: 1_000_000}
	var mu sync.Mutex
	var last *fakeClient
	m, err := New(Config{
		Push:              h.push,
		Progress:          &fakeProgress{},
		SSM:               &fakeApplier{},
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		Now:               h.now,
		newClient: func(c shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: c}
			mu.Lock()
			last = fc
			mu.Unlock()
			return fc
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	h.m = m
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	mu.Lock()
	h.client = last
	mu.Unlock()
	return h
}

func (h *phantomHarness) now() int64 {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.nowMs
}

func (h *phantomHarness) advance(ms int64) {
	h.mu.Lock()
	h.nowMs += ms
	h.mu.Unlock()
}

func (h *phantomHarness) consumer() *consumer {
	h.t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		h.t.Fatalf("existing: %v", err)
	}
	return d.consumer
}

// startTask feeds a TaskStarted, exactly as the shim stream would.
func (h *phantomHarness) startTask(seq uint64, id string) {
	h.t.Helper()
	if err := h.consumer().Apply(&corev1.Event{
		SessionId: "s1", Seq: seq, ProducedAtMs: h.now(),
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
			TaskId: id, Kind: corev1.TaskKind_TASK_KIND_AGENT, Description: id,
		}},
	}); err != nil {
		h.t.Fatalf("Apply(task_started %s): %v", id, err)
	}
}

// endTurn feeds an accepted turn boundary, the edge a turn's tasks are retired
// on.
func (h *phantomHarness) endTurn(seq uint64, turnID string) {
	h.t.Helper()
	if err := h.consumer().Apply(&corev1.Event{
		SessionId: "s1", Seq: seq, ProducedAtMs: h.now(), Plane: corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: turnID}},
	}); err != nil {
		h.t.Fatalf("Apply(turn_started): %v", err)
	}
	if err := h.consumer().Apply(&corev1.Event{
		SessionId: "s1", Seq: seq + 1, ProducedAtMs: h.now(), Plane: corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: turnID}},
	}); err != nil {
		h.t.Fatalf("Apply(turn_ended): %v", err)
	}
}

// catalogStatus is the status arm the newest pushed catalog carries for id, and
// whether the catalog names it at all.
func (h *phantomHarness) catalogStatus(id string) (any, bool) {
	h.t.Helper()
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	for i := len(h.push.catalog) - 1; i >= 0; i-- {
		for _, task := range h.push.catalog[i].GetTasks() {
			if task.GetTaskId() == id {
				return task.GetStatus(), true
			}
		}
	}
	return nil, false
}

func TestPhantomSweepClosesATaskTheShimIsNotRunning(t *testing.T) {
	// Arrange — the incident's shape: a task_started with no task_ended ever,
	// and a shim reporting an empty live set long afterwards.
	h := newPhantomHarness(t)
	h.startTask(1, "phantom")
	h.client.liveTasks = nil
	h.advance(phantomTaskGraceMs)

	// Act.
	closed := h.m.SweepPhantomTasks()

	// Assert — the entry is retired, and it is retired as LOST rather than as a
	// success nobody reported.
	if closed != 1 {
		t.Fatalf("closed = %d, want 1", closed)
	}
	status, found := h.catalogStatus("phantom")
	if !found {
		t.Fatalf("the catalog no longer names the task at all; it must name it as finished")
	}
	if _, lost := status.(*frontendv1.TaskEntry_Lost); !lost {
		t.Fatalf("status = %T, want the lost arm", status)
	}
}

func TestPhantomSweepLeavesATaskTheShimStillLists(t *testing.T) {
	// Arrange — a subagent grinding through a long tool call. Silence about it
	// is not evidence, and the shim's list is what says so.
	h := newPhantomHarness(t)
	h.startTask(1, "slow")
	h.client.liveTasks = []string{"slow"}
	h.advance(phantomTaskGraceMs * 100)

	// Act.
	closed := h.m.SweepPhantomTasks()

	// Assert.
	if closed != 0 {
		t.Fatalf("closed = %d, want 0 — a task the shim still lists is running, however long it runs", closed)
	}
	if ids := h.consumer().openTaskIDs(); len(ids) != 1 || ids[0] != "slow" {
		t.Fatalf("open tasks = %v, want [slow]", ids)
	}
}

func TestTurnEndMakesOpenTasksSweepableWithoutTheGrace(t *testing.T) {
	// Arrange — a task open at a turn end, with no time passed at all. Before
	// the boundary it is too young to ask about.
	h := newPhantomHarness(t)
	h.startTask(1, "orphan")
	h.client.liveTasks = nil
	if closed := h.m.SweepPhantomTasks(); closed != 0 {
		t.Fatalf("closed = %d before the turn end, want 0 — the grace has not been served", closed)
	}

	// Act — the turn that drove it ends, and the sweep runs on the same clock.
	h.endTurn(2, "t1")
	closed := h.m.SweepPhantomTasks()

	// Assert.
	if closed != 1 {
		t.Fatalf("closed = %d after the turn end, want 1", closed)
	}
}

func TestPhantomCloseIsIdempotentAcrossSweeps(t *testing.T) {
	// Arrange — the first sweep has already retired the entry.
	h := newPhantomHarness(t)
	h.startTask(1, "phantom")
	h.client.liveTasks = nil
	h.advance(phantomTaskGraceMs)
	h.m.SweepPhantomTasks()
	h.push.mu.Lock()
	pushesAfterFirst := len(h.push.catalog)
	h.push.mu.Unlock()

	// Act — the sweeper runs again, as it does every tick.
	closed := h.m.SweepPhantomTasks()

	// Assert — nothing is closed twice, and nothing is asked about either: the
	// open set is the gate.
	if closed != 0 {
		t.Fatalf("second sweep closed = %d, want 0", closed)
	}
	if got := h.client.liveTaskQueryCount(); got != 1 {
		t.Fatalf("live-task queries = %d, want 1 — a session with no open entry has nothing to ask about", got)
	}
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.catalog) != pushesAfterFirst {
		t.Fatalf("catalog pushes = %d, want %d — a repeat sweep must push nothing", len(h.push.catalog), pushesAfterFirst)
	}
}

func TestDeduplicatedTaskEndStillLeavesTheCatalogClosed(t *testing.T) {
	// Arrange — the end the daemon never saw as live: the shim wrote it, the
	// backlog replayed it, and the SSM dropped the repeat, so the catalog was
	// left holding the task open. The shim, meanwhile, has long since dropped
	// it from its live set.
	h := newPhantomHarness(t)
	h.startTask(1, "deduped")
	h.client.liveTasks = nil
	h.advance(phantomTaskGraceMs)

	// Act.
	h.m.SweepPhantomTasks()

	// Assert — the entry ends up closed despite no TaskEnded ever reaching the
	// fold.
	status, found := h.catalogStatus("deduped")
	if !found {
		t.Fatalf("the catalog no longer names the task at all; it must name it as finished")
	}
	if _, running := status.(*frontendv1.TaskEntry_Running); running {
		t.Fatalf("status = running, want a terminal arm")
	}
}

func TestPhantomSweepClosesNothingWhenTheShimCannotAnswer(t *testing.T) {
	// Arrange — the discriminator is the shim's list, so a shim that did not
	// answer is not a session with no tasks.
	h := newPhantomHarness(t)
	h.startTask(1, "unknown")
	h.client.liveTasksErr = errors.New("shim gone")
	h.advance(phantomTaskGraceMs)

	// Act.
	closed := h.m.SweepPhantomTasks()

	// Assert.
	if closed != 0 {
		t.Fatalf("closed = %d, want 0 — silence may never close a task", closed)
	}
	if ids := h.consumer().openTaskIDs(); len(ids) != 1 {
		t.Fatalf("open tasks = %v, want the entry left standing", ids)
	}
}
