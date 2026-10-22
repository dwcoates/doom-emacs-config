package main

import (
	"context"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/mergeworkspace"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/rollout"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// recordingRelay is a workspace.HostRelay that records feed tail returns and
// commit log opens.
type recordingRelay struct {
	tailReturns    []ids.WorkspaceID
	commitLogOpens []*agentreplv1.OpenInEditorCommitLog
}

func (r *recordingRelay) OpenInEditor(ids.WorkspaceID, string, *uint32) {}
func (r *recordingRelay) OpenCommitLog(_ ids.WorkspaceID, target *agentreplv1.OpenInEditorCommitLog) {
	r.commitLogOpens = append(r.commitLogOpens, target)
}
func (r *recordingRelay) ReloadWebapp(ids.WorkspaceID)         {}
func (r *recordingRelay) PublishHostWorkspace(ids.WorkspaceID) {}
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

func TestRelayForwarderCarriesOpenCommitLogToTheBoundRelay(t *testing.T) {
	// Arrange.
	target := &recordingRelay{}
	var f relayForwarder
	f.bind(target)
	log := &agentreplv1.OpenInEditorCommitLog{RepositoryDir: "/repo", Branch: "master", Commit: "abc123"}

	// Act.
	f.OpenCommitLog("w1", log)

	// Assert.
	if len(target.commitLogOpens) != 1 || target.commitLogOpens[0] != log {
		t.Fatalf("commit log opens = %v, want the one forwarded", target.commitLogOpens)
	}
}

// fixedHistory is a feed.HistorySource answering one page.
type fixedHistory struct{ page *conversationv1.HistoryPage }

func (f fixedHistory) ReadHistory(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	return f.page, nil
}

func (f fixedHistory) ReadInherited(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer, int64) (*conversationv1.HistoryPage, error) {
	return f.page, nil
}

func (f fixedHistory) ResolvePeerDelivery(context.Context, ids.WorkspaceID, string) (feed.PeerDeliveryEnds, error) {
	return feed.PeerDeliveryEnds{Sender: "session-a", Recipient: "session-b"}, nil
}

func (f fixedHistory) ResolveVendorTask(context.Context, ids.WorkspaceID, string, string) (feed.VendorTaskAgent, error) {
	return feed.VendorTaskAgent{Agent: "toolu_spawn", RequestedName: "vetter"}, nil
}

func TestHistoryForwarderResolvesAVendorTaskThroughTheBoundFleet(t *testing.T) {
	// Arrange.
	var f historyForwarder
	f.bind(fixedHistory{})

	// Act.
	got, err := f.ResolveVendorTask(context.Background(), "w1", "agent-main", "a47cd")

	// Assert.
	if err != nil || got != (feed.VendorTaskAgent{Agent: "toolu_spawn", RequestedName: "vetter"}) {
		t.Fatalf("ResolveVendorTask = %+v, %v; want the fleet's agent", got, err)
	}
}

func TestHistoryForwarderResolvingAVendorTaskBeforeTheFleetIsAnError(t *testing.T) {
	// Arrange.
	var f historyForwarder

	// Act.
	_, err := f.ResolveVendorTask(context.Background(), "w1", "agent-main", "a47cd")

	// Assert.
	if err == nil {
		t.Fatal("ResolveVendorTask before the fleet is bound = nil, want the boot-order error")
	}
}

func TestHistoryForwarderResolvesAPeerDeliveryThroughTheBoundFleet(t *testing.T) {
	// Arrange.
	var f historyForwarder
	f.bind(fixedHistory{})

	// Act.
	got, err := f.ResolvePeerDelivery(context.Background(), "w1", "m-1")

	// Assert.
	if err != nil || got != (feed.PeerDeliveryEnds{Sender: "session-a", Recipient: "session-b"}) {
		t.Fatalf("ResolvePeerDelivery = %+v, %v; want the fleet's ends", got, err)
	}
}

func TestHistoryForwarderResolvingAPeerDeliveryBeforeTheFleetIsAnError(t *testing.T) {
	// Arrange.
	var f historyForwarder

	// Act.
	_, err := f.ResolvePeerDelivery(context.Background(), "w1", "m-1")

	// Assert.
	if err == nil {
		t.Fatal("ResolvePeerDelivery before the fleet is bound = nil, want the boot-order error")
	}
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

func TestHistoryForwarderReadsInheritedThroughTheBoundFleet(t *testing.T) {
	// Arrange.
	want := &conversationv1.HistoryPage{}
	var f historyForwarder
	f.bind(fixedHistory{page: want})

	// Act.
	got, err := f.ReadInherited(context.Background(), "w1", &conversationv1.AgentId{Value: "book"}, nil, 1)

	// Assert.
	if err != nil || got != want {
		t.Fatalf("ReadInherited = %v, %v; want the fleet's page", got, err)
	}
}

func TestHistoryForwarderReadsNoInheritedPageBeforeTheFleet(t *testing.T) {
	// Arrange.
	var f historyForwarder

	// Act.
	_, err := f.ReadInherited(context.Background(), "w1", &conversationv1.AgentId{Value: "book"}, nil, 1)

	// Assert.
	if err == nil {
		t.Fatal("ReadInherited before the fleet was bound answered no error")
	}
}

// exclusiveQueue records the holds the delivery forwarder carried.
type exclusiveQueue struct {
	promptqueue.Queue
	held []ids.WorkspaceID
}

func (q *exclusiveQueue) Exclusively(ctx context.Context, ws ids.WorkspaceID, _ string, perform func(context.Context) error) error {
	q.held = append(q.held, ws)
	return perform(ctx)
}

func TestDeliveryForwarderHoldsThroughTheBoundQueue(t *testing.T) {
	// Arrange.
	queue := &exclusiveQueue{}
	var d deliveryForwarder
	d.bind(queue)
	ran := false

	// Act.
	err := d.Exclusively(context.Background(), "w1", "fork", func(context.Context) error { ran = true; return nil })

	// Assert.
	if err != nil || !ran || len(queue.held) != 1 || queue.held[0] != "w1" {
		t.Fatalf("Exclusively = %v, ran %v, held %v; want the work run under the queue's hold", err, ran, queue.held)
	}
}

func TestDeliveryForwarderBeforeTheQueueRunsNothing(t *testing.T) {
	// Arrange.
	var d deliveryForwarder
	ran := false

	// Act.
	err := d.Exclusively(context.Background(), "w1", "fork", func(context.Context) error { ran = true; return nil })

	// Assert.
	if err == nil || ran {
		t.Fatalf("Exclusively = %v, ran %v; want an error and nothing run", err, ran)
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
	f := &mergeForwarder{}

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

func (m *recordingMover) Drain(context.Context) {
	m.calls = append(m.calls, "drain")
}

func TestABoundMergeMoverCarriesBothMoves(t *testing.T) {
	// Arrange.
	f := &mergeForwarder{}
	target := &recordingMover{}
	f.bind(target)

	// Act.
	errs := errors.Join(f.SuspendForTransfer(context.Background(), "ws-1"), f.AdoptWorkspace(context.Background(), "ws-1"))

	// Assert.
	if errs != nil || len(target.calls) != 2 || target.calls[0] != "suspend ws-1" || target.calls[1] != "adopt ws-1" {
		t.Fatalf("calls = %v (%v), want both moves carried", target.calls, errs)
	}
}

func TestABoundMergeForwarderCarriesTheDrain(t *testing.T) {
	// Arrange.
	f := &mergeForwarder{}
	target := &recordingMover{}
	f.bind(target)

	// Act.
	f.Drain(context.Background())

	// Assert.
	if len(target.calls) != 1 || target.calls[0] != "drain" {
		t.Fatalf("calls = %v, want the drain carried", target.calls)
	}
}

func TestAnUnboundMergeForwarderPanicsOnTheDrain(t *testing.T) {
	// Arrange.
	f := &mergeForwarder{}
	defer func() {
		// Assert.
		got := recover()
		err, ok := got.(error)
		if !ok || !errors.Is(err, errMergeMoverUnbound) {
			t.Fatalf("recovered %v, want a panic carrying errMergeMoverUnbound", got)
		}
	}()

	// Act.
	f.Drain(context.Background())
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

// mergeVerbs is a workspace.Verbs whose live methods are the merge
// orchestrator's two reaches into the verbs: the name claim and the interrupt.
type mergeVerbs struct {
	workspace.Verbs
	interrupted []ids.WorkspaceID
}

func (v *mergeVerbs) ClaimMergeName(_ context.Context, _, name string) (string, func(), bool, error) {
	return "agent/" + name, func() {}, false, nil
}

func (v *mergeVerbs) Interrupt(_ context.Context, ws ids.WorkspaceID, _ workspace.InterruptTarget, _ bool) (workspace.InterruptOutcome, error) {
	v.interrupted = append(v.interrupted, ws)
	return workspace.InterruptOutcome{Turn: true}, nil
}

func TestVerbsForwarderBound(t *testing.T) {
	tests := []struct {
		name    string
		bind    bool
		wantErr string
	}{
		{name: "verbs bound", bind: true},
		{name: "verbs not yet bound", wantErr: "agent-repld: a test ran before the workspace verbs existed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := &verbsForwarder{}
			target := &mergeVerbs{}
			if tc.bind {
				f.bind(target)
			}

			// Act.
			verbs, err := f.bound("a test ran")

			// Assert.
			if tc.wantErr == "" && (err != nil || verbs != workspace.Verbs(target)) {
				t.Fatalf("bound = %v, %v; want the bound verbs", verbs, err)
			}
			if tc.wantErr != "" && (err == nil || err.Error() != tc.wantErr || verbs != nil) {
				t.Fatalf("bound = %v, %v; want %q", verbs, err, tc.wantErr)
			}
		})
	}
}

func TestVerbsForwarderMergeReachesBeforeTheVerbsAreErrors(t *testing.T) {
	tests := []struct {
		name string
		call func(f *verbsForwarder) error
		want string
	}{
		{name: "a name claim", call: func(f *verbsForwarder) error {
			_, _, _, err := f.ClaimMergeName(context.Background(), "/r", "fix")
			return err
		}, want: "agent-repld: an async merge's name was checked before the workspace verbs existed"},
		{name: "a merge tab interrupt", call: func(f *verbsForwarder) error {
			_, err := f.interruptTurn(context.Background(), "ws-1")
			return err
		}, want: "agent-repld: a merge tab's agent was interrupted before the workspace verbs existed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			err := tc.call(&verbsForwarder{})

			// Assert.
			if err == nil || err.Error() != tc.want {
				t.Fatalf("err = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestVerbsForwarderMergeReachesTheBoundVerbs(t *testing.T) {
	// Arrange.
	f := &verbsForwarder{}
	target := &mergeVerbs{}
	f.bind(target)

	// Act.
	branch, _, taken, claimErr := f.ClaimMergeName(context.Background(), "/r", "fix")
	running, interruptErr := f.interruptTurn(context.Background(), "ws-1")

	// Assert.
	if claimErr != nil || branch != "agent/fix" || taken || interruptErr != nil || !running || len(target.interrupted) != 1 {
		t.Fatalf("claim = %q, %v, %v; interrupt = %v, %v (%v); want both carried to the verbs", branch, taken, claimErr, running, interruptErr, target.interrupted)
	}
}

// TestEveryForwardedVerbsReadGoesThroughBound pins the shared shape: a method
// of the forwarder reaches the verbs through bound, so every early call is
// refused with the same named error.
func TestEveryForwardedVerbsReadGoesThroughBound(t *testing.T) {
	// Arrange.
	source, err := os.ReadFile("forward.go")
	if err != nil {
		t.Fatalf("read forward.go: %v", err)
	}

	// Act.
	reads := strings.Count(string(source), "f.verbs()")

	// Assert.
	if reads != 1 {
		t.Fatalf("forward.go reads f.verbs() %d times, want once, inside bound", reads)
	}
}

// boundDetached answers a detached forwarder bound to a service over a fresh
// in-memory state client holding one owed landing report.
func boundDetached(t *testing.T) (*detachedForwarder, wsm.DB, *dlog.TestSurfaces) {
	t.Helper()
	db, err := wsm.OpenInMemory(context.Background())
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	if err := db.OweHandBack(context.Background(), wsm.OwedHandBack{MergeWorkspace: "ws-merge", Requester: "ws-requester",
		MergeName: "m", Branch: "b", Summary: "s", IdempotencyKey: "k", OwedAt: time.Unix(1, 0)}); err != nil {
		t.Fatalf("OweHandBack: %v", err)
	}
	logs := dlog.NewTestSurfaces()
	f := &detachedForwarder{}
	f.bind(mergeworkspace.New(mergeworkspace.Deps{DB: db, Log: logs}))
	return f, db, logs
}

func TestDetachedForwarderCoversTheOwedReportsThroughTheBoundService(t *testing.T) {
	// Arrange
	f, db, _ := boundDetached(t)

	// Act
	err := f.CoverOwed(context.Background())

	// Assert
	covered, readErr := db.OwedHandBacks(context.Background(), true)
	if err != nil || readErr != nil || len(covered) != 1 {
		t.Fatalf("CoverOwed = %v; covered %+v, %v; want the report covered", err, covered, readErr)
	}
}

func TestDetachedForwarderCoversNothingBeforeTheService(t *testing.T) {
	// Arrange
	f := &detachedForwarder{}

	// Act
	err := f.CoverOwed(context.Background())

	// Assert
	if err == nil {
		t.Fatal("CoverOwed before the service = nil, want the error")
	}
}

func TestDetachedForwarderPaysThroughTheBoundService(t *testing.T) {
	// Arrange: a service whose state read fails, so the payment is visible
	// in its own record without a prompt path behind it.
	f, db, logs := boundDetached(t)
	db.Close()

	// Act
	f.PayOwed(context.Background(), false, nil)

	// Assert
	if !slices.ContainsFunc(logs.Records(), func(r dlog.Record) bool {
		return r.Level == "error" && r.Message == "could not read the owed landing reports; they stay owed"
	}) {
		t.Fatalf("records = %+v, want the service's own record of the payment", logs.Records())
	}
}

func TestDetachedForwarderPayingBeforeTheServicePanics(t *testing.T) {
	// Arrange
	f := &detachedForwarder{}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("PayOwed before the service returned, want a panic: the graph's order is broken")
		}
	}()

	// Act
	f.PayOwed(context.Background(), true, nil)
}

func TestFreshBootSettle(t *testing.T) {
	tests := []struct {
		name    string
		joining bool
		want    bool
	}{
		{name: "a fresh boot settles through the rollout", joining: false, want: true},
		{name: "a joining successor settles at its takeover instead", joining: true, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			settle := freshBootSettle(tt.joining, settlingController{})

			// Assert
			if (settle != nil) != tt.want {
				t.Fatalf("freshBootSettle(joining=%v) present = %v, want %v", tt.joining, settle != nil, tt.want)
			}
		})
	}
}

// settlingController is a rollout controller whose only method in use is
// SettleBoot.
type settlingController struct{ rollout.Controller }

func (settlingController) SettleBoot(context.Context) {}

// supersedingQueue is a prompt queue whose only verb is Supersede, answering
// retired and recording what it was asked.
type supersedingQueue struct {
	promptqueue.Service
	retired bool
	asked   []ids.TurnID
}

func (q *supersedingQueue) Supersede(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	q.asked = append(q.asked, turn)
	return q.retired, nil
}

func TestHoldsForwarderSupersedesThroughTheBoundQueue(t *testing.T) {
	// Arrange
	f := &holdsForwarder{}
	q := &supersedingQueue{retired: true}
	f.bind(q)

	// Act
	retired, err := f.Supersede(context.Background(), "ws-a", "turn-1")

	// Assert
	if err != nil || !retired || len(q.asked) != 1 || q.asked[0] != "turn-1" {
		t.Fatalf("Supersede = (%v, %v) asking %v, want the bound queue's answer for turn-1", retired, err, q.asked)
	}
}

func TestHoldsForwarderRefusesASupersessionBeforeTheQueue(t *testing.T) {
	// Arrange
	f := &holdsForwarder{}

	// Act
	retired, err := f.Supersede(context.Background(), "ws-a", "turn-1")

	// Assert
	if err == nil || retired {
		t.Fatalf("Supersede = (%v, %v), want a refusal before the queue is bound", retired, err)
	}
}
