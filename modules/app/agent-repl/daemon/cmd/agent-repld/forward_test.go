package main

import (
	"context"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/workspace"
)

// recordingRelay is a workspace.HostRelay that records feed tail returns.
type recordingRelay struct {
	tailReturns []ids.WorkspaceID
}

func (r *recordingRelay) OpenInEditor(ids.WorkspaceID, string, *uint32) {}
func (r *recordingRelay) ReloadWebapp(ids.WorkspaceID)                  {}
func (r *recordingRelay) PublishHostWorkspace(ids.WorkspaceID)          {}
func (r *recordingRelay) ReturnFeedToTail(ws ids.WorkspaceID) {
	r.tailReturns = append(r.tailReturns, ws)
}

func TestRelayForwarderCarriesReturnFeedToTailToTheBoundRelay(t *testing.T) {
	// Arrange.
	target := &recordingRelay{}
	var f relayForwarder
	f.bind(target)

	// Act.
	f.ReturnFeedToTail("w1")

	// Assert.
	if !slices.Equal(target.tailReturns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("tail returns = %v, want [w1]", target.tailReturns)
	}
}

// fixedHistory is a feed.HistorySource answering one page.
type fixedHistory struct{ page *conversationv1.HistoryPage }

func (f fixedHistory) ReadHistory(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	return f.page, nil
}

func TestHistoryForwarderReadsThroughTheBoundFleet(t *testing.T) {
	// Arrange.
	want := &conversationv1.HistoryPage{}
	var f historyForwarder
	f.bind(fixedHistory{page: want})

	// Act.
	got, err := f.ReadHistory(context.Background(), "w1", nil, nil)

	// Assert.
	if err != nil || got != want {
		t.Fatalf("ReadHistory = %v, %v; want the fleet's page", got, err)
	}
}

func TestHistoryForwarderBeforeTheFleetIsAnError(t *testing.T) {
	// Arrange.
	var f historyForwarder

	// Act.
	_, err := f.ReadHistory(context.Background(), "w1", nil, nil)

	// Assert.
	if err == nil {
		t.Fatal("ReadHistory before the fleet was bound answered no error")
	}
}

// servedQueue records the vendor-serves edges the forwarder carried.
type servedQueue struct {
	promptqueue.Queue
	served []ids.WorkspaceID
}

func (q *servedQueue) OnVendorServes(ws ids.WorkspaceID) { q.served = append(q.served, ws) }

func TestVendorServesForwarderCarriesTheEdgeToTheBoundQueue(t *testing.T) {
	// Arrange
	queue := &servedQueue{}
	f := &vendorServesForwarder{log: dlog.NewTestLogger()}
	f.bind(queue)

	// Act
	f.VendorServes("ws-1", "usage_limit")

	// Assert
	if !slices.Equal(queue.served, []ids.WorkspaceID{"ws-1"}) {
		t.Fatalf("served = %v, want [ws-1]", queue.served)
	}
}

func TestVendorServesForwarderWithNoQueueIsAWiringDefectAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	f := &vendorServesForwarder{log: log}

	// Act
	f.VendorServes("ws-1", "usage_limit")

	// Assert
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.cmd.vendor_serves" {
			return
		}
	}
	t.Fatalf("no ERROR record of an edge before the queue was bound")
}

func TestAnUnboundMergeMoverRefusesTheMove(t *testing.T) {
	// Arrange.
	f := &mergeMoverForwarder{}

	// Act.
	err := f.SuspendForTransfer(context.Background(), "ws-1")

	// Assert.
	if !errors.Is(err, errMergeMoverUnbound) {
		t.Fatalf("SuspendForTransfer = %v, want errMergeMoverUnbound", err)
	}
}

// recordingMover records the moves a bound forwarder carries.
type recordingMover struct{ calls []string }

func (m *recordingMover) SuspendForTransfer(_ context.Context, ws ids.WorkspaceID) error {
	m.calls = append(m.calls, "suspend "+string(ws))
	return nil
}

func (m *recordingMover) AdoptWorkspace(_ context.Context, ws ids.WorkspaceID) error {
	m.calls = append(m.calls, "adopt "+string(ws))
	return nil
}

func TestABoundMergeMoverCarriesBothMoves(t *testing.T) {
	// Arrange.
	f := &mergeMoverForwarder{}
	target := &recordingMover{}
	f.bind(target)

	// Act.
	errs := errors.Join(f.SuspendForTransfer(context.Background(), "ws-1"), f.AdoptWorkspace(context.Background(), "ws-1"))

	// Assert.
	if errs != nil || len(target.calls) != 2 || target.calls[0] != "suspend ws-1" || target.calls[1] != "adopt ws-1" {
		t.Fatalf("calls = %v (%v), want both moves carried", target.calls, errs)
	}
}

// publishingVerbs is a workspace.Verbs whose only live method is the roster
// republish.
type publishingVerbs struct {
	workspace.Verbs
	calls int
	err   error
}

func (v *publishingVerbs) PublishRegistry(context.Context) error {
	v.calls++
	return v.err
}

func TestVerbsForwarderPublishRegistryReachesTheBoundVerbs(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "a republish that succeeds", err: nil},
		{name: "a republish that fails", err: errors.New("roster unreadable")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			target := &publishingVerbs{err: tc.err}
			f := &verbsForwarder{}
			publish := f.publishRegistry("a test ran")
			f.bind(target)

			// Act.
			err := publish(context.Background())

			// Assert.
			if target.calls != 1 || !errors.Is(err, tc.err) {
				t.Fatalf("calls = %d, err = %v; want one call answering %v", target.calls, err, tc.err)
			}
		})
	}
}

func TestVerbsForwarderPublishRegistryBeforeTheVerbsIsAnError(t *testing.T) {
	// Arrange.
	f := &verbsForwarder{}

	// Act.
	err := f.publishRegistry("a merge landed")(context.Background())

	// Assert.
	if err == nil || err.Error() != "agent-repld: a merge landed before the workspace verbs existed" {
		t.Fatalf("err = %v, want the wiring defect named", err)
	}
}

// TestEveryEarlyRegistryRepublishGoesThroughTheForwarder pins the shared
// shape: a component built before the verbs republishes the roster through
// verbsForwarder.publishRegistry, never a hand-rolled closure of its own.
func TestEveryEarlyRegistryRepublishGoesThroughTheForwarder(t *testing.T) {
	// Arrange.
	source, err := os.ReadFile("graph.go")
	if err != nil {
		t.Fatalf("read graph.go: %v", err)
	}

	// Act.
	handRolled := strings.Count(string(source), "verbsRef.verbs()")

	// Assert.
	if handRolled != 0 {
		t.Fatalf("graph.go reads verbsRef.verbs() %d times, want every republish through verbsRef.publishRegistry", handRolled)
	}
}
