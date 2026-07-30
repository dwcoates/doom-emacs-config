package sessioncontroller

import (
	"errors"
	"reflect"
	"runtime"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// errApplyPermission is the SSM refusing a permission-row edge.
var errApplyPermission = errors.New("the state log rejected the permission row")

// ---------------------------------------------------------------------------
// The PERMISSION-ROW PRODUCER: the daemon's permission path telling the SSM
// that the agent is parked on a question.
//
// The count it folds is permRegistry's own waiter set, so these tests drive
// REAL blocked HandlePermission round-trips and assert the edges that fall out
// of them — never a hand-fed count.
// ---------------------------------------------------------------------------

// newProducerHandler builds a permHandler wired exactly as the session controller wires it:
// onPermsChanged calls the production producer, which reads the manager's own
// permission registry and applies the edge to the SSM. It returns the handler
// and the fake applier the edges land in.
func newProducerHandler(t *testing.T) (permHandler, *fakeApplier) {
	t.Helper()
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	applier := m.cfg.SSM.(*fakeApplier)
	cons := newConsumer("ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
	ph := permHandler{reg: m.reg, cons: cons, logf: func(string, ...any) {}, onPermsChanged: func() {
		m.notePermissionState("ws")
	}}
	return ph, applier
}

// waitForPermEdges blocks until the applier has recorded at least n
// permission-row edges, yielding the scheduler between checks.
//
// It is the rendezvous these tests need and waitForPermWaiter is not.
// HandlePermission registers its waiter and only THEN fires the notification
// the producer rides, so a test that answers as soon as the waiter appears can
// race the opening edge — and a permission answered before its own open lands
// legitimately produces no open at all (see permstate.go). Waiting on the edge
// itself pins the sequence under test without pinning wall-clock time.
func waitForPermEdges(applier *fakeApplier, n int) {
	for len(applier.permissionsApplied()) < n {
		runtime.Gosched()
	}
}

// answerFn resolves a parked request the way one of the three real resolutions
// does. Abandonment is the registry's teardown path (fail), which sends a nil
// response rather than fabricating a decision.
type answerFn func(t *testing.T, reg *permRegistry, requestID string)

func allowIt(t *testing.T, reg *permRegistry, requestID string) {
	t.Helper()
	if err := reg.answer(requestID, true, "", nil); err != nil {
		t.Fatalf("answer(allow): %v", err)
	}
}

func denyIt(t *testing.T, reg *permRegistry, requestID string) {
	t.Helper()
	if err := reg.answer(requestID, false, "no", nil); err != nil {
		t.Fatalf("answer(deny): %v", err)
	}
}

func abandonIt(_ *testing.T, reg *permRegistry, _ string) { reg.fail("teardown") }

func TestPermissionRowOpensAndClosesOnEveryResolution(t *testing.T) {
	tests := []struct {
		name   string
		answer answerFn
	}{
		{name: "a granted permission closes the row", answer: allowIt},
		{name: "a denied permission closes the row", answer: denyIt},
		{name: "an abandoned permission closes the row", answer: abandonIt},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ph, applier := newProducerHandler(t)
			req := &corev1.PermissionRequest{RequestId: "r1", ToolName: "Bash"}

			// Act — park the real handler, then resolve it.
			done := make(chan *corev1.PermissionResponse, 1)
			go func() { done <- ph.HandlePermission("s1", req) }()
			waitForPermWaiter(ph.reg, "ws", "r1")
			waitForPermEdges(applier, 1)
			tc.answer(t, ph.reg, "r1")
			<-done

			// Assert — one open on the way in, one close on the way out.
			want := []permissionCall{
				{workspace: "ws", pending: true, reason: "pending=1"},
				{workspace: "ws", pending: false, reason: "pending=0"},
			}
			if got := applier.permissionsApplied(); !reflect.DeepEqual(got, want) {
				t.Fatalf("permission edges = %+v, want %+v", got, want)
			}
		})
	}
}

func TestConcurrentPermissionsOpenOnceAndCloseAtZero(t *testing.T) {
	// Arrange — two questions parked at the same time.
	ph, applier := newProducerHandler(t)
	done := map[string]chan *corev1.PermissionResponse{}
	for i, id := range []string{"r1", "r2"} {
		ch := make(chan *corev1.PermissionResponse, 1)
		done[id] = ch
		go func() { ch <- ph.HandlePermission("s1", &corev1.PermissionRequest{RequestId: id, ToolName: "Bash"}) }()
		waitForPermWaiter(ph.reg, "ws", id)
		waitForPermEdges(applier, i+1)
	}

	// Act — answer them one at a time, each handler's round-trip fully settled
	// before the next answer, so the counts these edges carry are the sequence
	// under test rather than a scheduling accident.
	allowIt(t, ph.reg, "r1")
	<-done["r1"]
	allowIt(t, ph.reg, "r2")
	<-done["r2"]

	// Assert — the SSM sees the counts, and only the first and last are edges
	// it can act on (the middle two report a still-pending set).
	want := []permissionCall{
		{workspace: "ws", pending: true, reason: "pending=1"},
		{workspace: "ws", pending: true, reason: "pending=2"},
		{workspace: "ws", pending: true, reason: "pending=1"},
		{workspace: "ws", pending: false, reason: "pending=0"},
	}
	if got := applier.permissionsApplied(); !reflect.DeepEqual(got, want) {
		t.Fatalf("permission edges = %+v, want %+v", got, want)
	}
}

func TestPermissionRowApplyFailureIsLoudAndDoesNotBlockTheRoundTrip(t *testing.T) {
	// Arrange — an SSM that refuses the edge.
	ph, applier := newProducerHandler(t)
	applier.permErr = errApplyPermission

	// Act.
	done := make(chan *corev1.PermissionResponse, 1)
	go func() { done <- ph.HandlePermission("s1", &corev1.PermissionRequest{RequestId: "r1"}) }()
	waitForPermWaiter(ph.reg, "ws", "r1")
	waitForPermEdges(applier, 1)
	allowIt(t, ph.reg, "r1")
	resp := <-done

	// Assert — the human's answer still reaches the shim; a state-write failure
	// must never swallow a decision.
	if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
		t.Fatalf("decision = %s, want ALLOW", resp.GetDecision())
	}
	if len(applier.permissionsApplied()) != 2 {
		t.Fatalf("both edges must still be attempted, got %+v", applier.permissionsApplied())
	}
}
