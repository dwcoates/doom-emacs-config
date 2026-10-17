package feed

import (
	"context"
	"errors"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// A MESSAGE FROM AN AGENT OF THIS SESSION is headed by its sender: by the
// requested name its spawn gave it, or by its identifier, asked of the record by
// the task id it names its sender with, and drawn again in place once known.

// fakeVendorTasks is the record behind Deps.VendorTasks: it answers each task
// id from `agents` (or `err`) and records every lookup.
type fakeVendorTasks struct {
	mu      sync.Mutex
	agents  map[string]VendorTaskAgent
	err     error
	lookups []string
}

func (f *fakeVendorTasks) ResolveVendorTask(_ context.Context, ws ids.WorkspaceID, session, vendorTaskID string) (VendorTaskAgent, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.lookups = append(f.lookups, string(ws)+"/"+session+"/"+vendorTaskID)
	return f.agents[vendorTaskID], f.err
}

// inSessionMessage is a message from the agent of this session TASK names,
// with id ID, to the main agent.
func (h *harness) inSessionMessage(id, task string) {
	h.t.Helper()
	h.resolver.OnPeerMessage(testWorkspace, &conversationv1.PeerMessage{
		Agent: mainAgent(),
		Body:  "the follow-ups are done",
		Id:    id,
		Kind: &conversationv1.PeerMessage_InSession{InSession: &conversationv1.PeerMessageInSession{
			SenderTask: &conversationv1.PeerMessageSenderTask{VendorTaskId: task},
		}},
	}, nil, nil)
}

// headerOfOnlyRow is the received header of the root feed's one row.
func (h *harness) headerOfOnlyRow() string {
	h.t.Helper()
	return fromLine(h.only(rootFeed()).GetPeerMessage().GetFrom())
}

func TestAMessageFromASubagentIsHeadedByItsTaskIdUntilTheRecordAnswers(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.inSessionMessage("p1", "a47cd5e35c91902a0")

	// Assert.
	if got := h.headerOfOnlyRow(); got != "← agent unresolved a47cd5e35c91902a0" {
		t.Fatalf("header = %q, want the task id, unresolved", got)
	}
}

func TestAMessageFromASubagentAsksTheRecordWithinThisSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inSessionMessage("p1", "a47cd5e35c91902a0")

	// Act.
	h.runSpawned()

	// Assert.
	if got := h.taskSource.lookups; len(got) != 1 || got[0] != string(testWorkspace)+"/agent-main/a47cd5e35c91902a0" {
		t.Fatalf("lookups = %v, want one, within the main agent's lineage", got)
	}
}

func TestAMessageFromANamedSubagentIsHeadedByItsName(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.taskSource.agents = map[string]VendorTaskAgent{"a47cd": {Agent: "toolu_spawn", RequestedName: "vetter"}}
	h.inSessionMessage("p1", "a47cd")

	// Act.
	h.runSpawned()

	// Assert.
	if got := h.headerOfOnlyRow(); got != "← agent named vetter" {
		t.Fatalf("header = %q, want the subagent's requested name", got)
	}
}

func TestAMessageFromAnUnnamedSubagentIsHeadedByItsIdentifier(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.taskSource.agents = map[string]VendorTaskAgent{"a47cd": {Agent: "toolu_spawn"}}
	h.inSessionMessage("p1", "a47cd")

	// Act.
	h.runSpawned()

	// Assert.
	if got := h.headerOfOnlyRow(); got != "← agent identified toolu_spawn" {
		t.Fatalf("header = %q, want the subagent's identifier", got)
	}
}

func TestAMessageFromASubagentTakesTheNameTheDrawnSpawnGaveIt(t *testing.T) {
	// Arrange: the record's answer carries no name, the spawn this feed drew does.
	h := newHarness(t)
	h.spawnNamedSubagent("spawn-1", &conversationv1.AgentId{Value: "toolu_spawn"}, "vetter")
	h.taskSource.agents = map[string]VendorTaskAgent{"a47cd": {Agent: "toolu_spawn"}}
	h.inSessionMessage("p1", "a47cd")

	// Act.
	h.runSpawned()

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if peer := row.GetPeerMessage(); peer != nil {
			if got := fromLine(peer.GetFrom()); got != "← agent named vetter" {
				t.Fatalf("header = %q, want the drawn spawn's name", got)
			}
			return
		}
	}
	t.Fatal("no peer bubble drawn")
}

func TestATaskTheRecordDoesNotPairYetIsAskedAgainWhenDrawnAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inSessionMessage("p1", "a47cd")
	h.runSpawned()

	// Act.
	h.inSessionMessage("p1", "a47cd")
	h.runSpawned()

	// Assert.
	if got := len(h.taskSource.lookups); got != 2 {
		t.Fatalf("lookups = %d, want the unpaired task asked again", got)
	}
}

func TestATaskIsLookedUpOnceWhileItsLookupIsUnderWay(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: both planes deliver the message before the lookup answers.
	h.inSessionMessage("p1", "a47cd")
	h.inSessionMessage("p1", "a47cd")
	h.runSpawned()

	// Assert.
	if got := len(h.taskSource.lookups); got != 1 {
		t.Fatalf("lookups = %d, want one while one is under way", got)
	}
}

func TestAFailedTaskLookupIsRecordedAtErrorNamingTheTask(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.taskSource.err = errors.New("store unavailable")
	h.inSessionMessage("p1", "a47cd")

	// Act.
	h.runSpawned()

	// Assert.
	if !h.hasRecordWith("error", "daemon.feed.vendor_task_lookup_failed", "task_id", "a47cd") {
		t.Fatalf("records = %+v, want the failed lookup at ERROR naming the task", h.records())
	}
}

func TestATaskLookupWithNoShimToAskIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.taskSource.err = ErrNoHistorySource
	h.inSessionMessage("p1", "a47cd")

	// Act.
	h.runSpawned()

	// Assert.
	if !h.hasRecordWith("info", "daemon.feed.vendor_task_lookup_no_source", "task_id", "a47cd") {
		t.Fatalf("records = %+v, want no shim recorded at INFO", h.records())
	}
}

func TestAMessageNamingNoSenderTaskIsRecordedAtErrorAndDrawnUnnamed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.inSessionMessage("p1", "")

	// Assert.
	if got := h.headerOfOnlyRow(); got != "← unnamed another session" || !h.hasRecord("error", "daemon.feed.peer_in_session_no_task") {
		t.Fatalf("header = %q, records = %+v; want the unnamed stand-in and the defect at ERROR", got, h.records())
	}
}

func TestALookupWithNoMainAgentToScopeItIsRecordedAtErrorAndAsksNothing(t *testing.T) {
	// Arrange: a workspace whose main agent no watcher named.
	h := newHarness(t)
	h.resolver.mu.Lock()
	s := h.resolver.state(otherWorkspace)

	// Act.
	h.resolver.askVendorTask(s, "a47cd", s.vendorTaskOf("a47cd"))
	h.resolver.mu.Unlock()
	h.runSpawned()

	// Assert.
	if len(h.taskSource.lookups) != 0 || !h.hasRecordWith("error", "daemon.feed.vendor_task_no_lineage", "task_id", "a47cd") {
		t.Fatalf("lookups = %v, records = %+v; want none asked and the defect at ERROR", h.taskSource.lookups, h.records())
	}
}
