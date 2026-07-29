package sessiondrv

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// THE ESCAPE LADDER. Every bring-up ends on `wired` or on a resolved failure;
// "wedged on starting" has no path left. See bringupescape.go.
// ---------------------------------------------------------------------------

// escapeHarness drives ensure() against a queue of fake clients, so a first
// bring-up can fail and the ladder's FRESH retry can succeed (or fail again)
// under the test's control rather than a timer's.
type escapeHarness struct {
	m       *Manager
	spawner *fakeSpawner
	pusher  *fakePusher
	applier *fakeApplier
	log     *logCapture

	mu      sync.Mutex
	clients []*fakeClient
}

// newEscapeHarness builds a manager whose Nth bring-up gets clients[N]. A
// client with a non-nil notReady never finishes handshaking, which is what a
// shim dying mid-bring-up looks like from here.
func newEscapeHarness(t *testing.T, clients ...*fakeClient) *escapeHarness {
	t.Helper()
	h := &escapeHarness{
		spawner: &fakeSpawner{resume: map[string]string{}},
		pusher:  &fakePusher{},
		applier: &fakeApplier{},
		log:     &logCapture{},
		clients: clients,
	}
	m, err := New(Config{
		Push:              h.pusher,
		SSM:               h.applier,
		Spawner:           h.spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		Logf:              h.log.logf,
		newClient: func(c shimclient.Config) sessionClient {
			h.mu.Lock()
			defer h.mu.Unlock()
			if len(h.clients) == 0 {
				return &fakeClient{cfg: c}
			}
			next := h.clients[0]
			h.clients = h.clients[1:]
			next.cfg = c
			return next
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	h.m = m
	return h
}

// sdkDied fires the shim's own account of a dead SDK stream at the workspace's
// live driver — the DegradedState the shim sends immediately before it shuts
// down with `sdk_error`, and the only detail the daemon ever gets.
func (h *escapeHarness) sdkDied(t *testing.T, reason string) {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	d.consumer.Degraded("", &corev1.DegradedState{Component: shimSDKComponent, Reason: reason})
}

// failureCards returns every SystemFailureItem pushed into the feed.
func (h *escapeHarness) failureCards() []*frontendv1.SystemFailureItem {
	var out []*frontendv1.SystemFailureItem
	h.pusher.mu.Lock()
	deltas := append([]*frontendv1.ConversationDelta(nil), h.pusher.convo...)
	h.pusher.mu.Unlock()
	for _, delta := range deltas {
		for _, item := range delta.GetItems() {
			if f := item.GetSystemFailure(); f != nil {
				out = append(out, f)
			}
		}
	}
	return out
}

func (h *escapeHarness) hasCard(errType errclass.Type) bool {
	for _, c := range h.failureCards() {
		if c.GetErrorType() == string(errType) {
			return true
		}
	}
	return false
}

// blocked is a client that never finishes its handshake.
func blocked() *fakeClient { return &fakeClient{notReady: make(chan struct{})} }

// errBringUpDead is a handshake that fails immediately — the retry's failure,
// delivered by the client rather than by a timer, so the ladder's second rung
// is exercised without waiting out bringUpTimeout.
var errBringUpDead = errors.New("the shim connection died")

func TestResumeDeathDropsThePointerAndRetriesFresh(t *testing.T) {
	// Arrange — the first bring-up resumes a conversation and its shim dies;
	// the second is fresh and comes up.
	h := newEscapeHarness(t, blocked(), &fakeClient{})
	h.spawner.resume["s1"] = "uuid-gone"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: process exited with code 1")

	// Act.
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("ensure after a resume death = %v, want the fresh retry to succeed", err)
	}
	if d == nil {
		t.Fatal("ensure returned no driver")
	}
	if got := h.spawner.dropCalls(); len(got) != 1 {
		t.Fatalf("DropResume calls = %v, want exactly one", got)
	}
}

func TestResumeDeathNotesTheMissingHistoryInTheFeed(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t, blocked(), &fakeClient{})
	h.spawner.resume["s1"] = "uuid-gone"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: process exited with code 1")

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Assert — a silently emptied workspace is indistinguishable from lost data.
	if !h.hasCard(errclass.TypeSessionHistoryMissing) {
		t.Fatalf("no history-missing note in the feed; cards=%v", h.failureCards())
	}
}

func TestTheFreshRetryHappensExactlyOnce(t *testing.T) {
	// Arrange — both bring-ups die. A ladder that retried on the fresh failure
	// too would spin forever against a genuinely broken environment.
	h := newEscapeHarness(t, blocked(), &fakeClient{awaitErr: errBringUpDead})
	h.spawner.resume["s1"] = "uuid-gone"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: first")

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("ensure succeeded with both bring-ups dead")
	}
	if got := h.spawner.dropCalls(); len(got) != 1 {
		t.Fatalf("DropResume calls = %v, want exactly one — the fresh failure has nothing left to drop", got)
	}
}

func TestAFreshBringUpFailureResolvesStartFailed(t *testing.T) {
	// Arrange — no resume pointer at all, so there is nothing to retry.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: no such model")

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("a fresh bring-up that never wired returned no error")
	}
	if !h.hasCard(errclass.TypeSessionStartFailed) {
		t.Fatalf("no start-failed card; cards=%v", h.failureCards())
	}
}

func TestStartFailedNamesTheError(t *testing.T) {
	// Arrange — a card that only announces a failure tells the user nothing
	// they did not already see in the tab color.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: no such model")

	// Act.
	h.m.ensure(context.Background(), "ws")

	// Assert.
	for _, c := range h.failureCards() {
		if c.GetErrorType() == string(errclass.TypeSessionStartFailed) {
			if !strings.Contains(c.GetSourceDetail(), "no such model") {
				t.Fatalf("start-failed detail = %q, want the shim's own reason", c.GetSourceDetail())
			}
			return
		}
	}
	t.Fatal("no start-failed card")
}

func TestStartFailedClosesTheWiredAxis(t *testing.T) {
	// Arrange — this is the whole point: `starting` may never be terminal.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: no such model")

	// Act.
	h.m.ensure(context.Background(), "ws")

	// Assert.
	for _, w := range h.applier.wiringsApplied() {
		if w.workspace == "ws" && w.wiring == ssm.WiringDormant && w.reason == "bring_up_failed" {
			return
		}
	}
	t.Fatalf("no bring-up-failed close on the wired axis; calls=%v", h.applier.wiringsApplied())
}

func TestABringUpTimeoutResolvesStartFailed(t *testing.T) {
	// Arrange — the shim never speaks at all, so no fault ever arrives and the
	// only bound is the wait's own deadline.
	h := newEscapeHarness(t, blocked())
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act — a cancelled context stands in for the elapsed bringUpTimeout: both
	// end the wait with AwaitReady's error rather than a fault.
	_, err := h.m.ensure(ctx, "ws")

	// Assert.
	if err == nil {
		t.Fatal("a bring-up that never wired returned no error")
	}
	if !h.hasCard(errclass.TypeSessionStartFailed) {
		t.Fatalf("no start-failed card; cards=%v", h.failureCards())
	}
}

func TestAStaleResumeDroppedAtSpawnNotesTheMissingHistory(t *testing.T) {
	// Arrange — the spawner caught the bad pointer BEFORE handing it to the
	// CLI, so nothing dies and the note is the only thing the user sees.
	h := newEscapeHarness(t, &fakeClient{})
	h.spawner.staleDropped = "uuid-gone"

	// Act.
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Assert.
	if !h.hasCard(errclass.TypeSessionHistoryMissing) {
		t.Fatalf("no history-missing note; cards=%v", h.failureCards())
	}
}

func TestAHealthyBringUpNotesNothing(t *testing.T) {
	// Arrange — the ordinary case must stay silent.
	h := newEscapeHarness(t, &fakeClient{})

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Assert.
	if cards := h.failureCards(); len(cards) != 0 {
		t.Fatalf("a healthy bring-up pushed %v", cards)
	}
}
