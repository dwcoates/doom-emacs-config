package shimclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

type recordingFileDiagnostics struct{ got chan *corev1.Event }

func (s *recordingFileDiagnostics) PersistFileDiagnostic(ev *corev1.Event, _ *corev1.FilePlaneDiagnostic) error {
	s.got <- ev
	return nil
}

type failingFileDiagnostics struct{}

func (failingFileDiagnostics) PersistFileDiagnostic(*corev1.Event, *corev1.FilePlaneDiagnostic) error {
	return errors.New("durable sink failed")
}

func TestReplayContinuationFromLastSeq(t *testing.T) {
	// Arrange: the daemon has durably seen through seq 4; on attach its
	// DaemonHello must carry from_seq=4 and the shim replays 5..9.
	h := newHarness()
	h.seq.SetLastSeq("sess-1", 4)
	gotFrom := make(chan uint64, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		dh := fakeServerHandshake(t, conn, "sess-1", "1", false)
		gotFrom <- dh.GetFromSeq()
		for seq := uint64(5); seq <= 9; seq++ {
			mustWriteMsg(t, conn, persistentTurnEnd("sess-1", seq))
		}
		_, _ = wire.ReadAny(conn)
	})
	_, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act / Assert
	select {
	case from := <-gotFrom:
		if from != 4 {
			t.Fatalf("DaemonHello from_seq: got %d want 4", from)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no DaemonHello observed")
	}
	for want := uint64(5); want <= 9; want++ {
		if got := recvEvent(t, h.state.ch).GetSeq(); got != want {
			t.Fatalf("replayed seq: got %d want %d", got, want)
		}
	}
	if last := h.seq.LastSeq("sess-1"); last != 9 {
		t.Fatalf("seq store: got %d want 9", last)
	}
}

func TestSeqRegressionIsDetected(t *testing.T) {
	// Arrange: a client (no live connection needed; exercise the demux logic
	// directly) that has advanced to seq 5.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 5)); err != nil {
		t.Fatalf("first event should be accepted: %v", err)
	}
	<-h.state.ch // drain

	// Act: a lower seq is a protocol violation.
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 3))

	// Assert
	if !errors.Is(err, ErrSeqRegression) {
		t.Fatalf("want ErrSeqRegression, got %v", err)
	}
}

func TestRejectedLifecycleEventDoesNotAdvanceHighWater(t *testing.T) {
	h := newHarness()
	sinkErr := errors.New("durable turn claim rejected")
	h.state.err = sinkErr
	c := New(h.config(t, "sess-1", "/unused.sock"))

	err := c.dispatchEvent(persistentTurnEnd("sess-1", 7))
	if !errors.Is(err, ErrLifecycleRejected) || !strings.Contains(err.Error(), sinkErr.Error()) {
		t.Fatalf("dispatch err = %v, want ErrLifecycleRejected carrying sink cause", err)
	}
	<-h.state.ch
	if got := h.seq.LastSeq("sess-1"); got != 0 {
		t.Fatalf("last sequence = %d, want 0 after rejected lifecycle event", got)
	}
	if c.lastSeen != 0 {
		t.Fatalf("client lastSeen = %d, want 0 after rejected lifecycle event", c.lastSeen)
	}
}

func TestRejectedAccountUsageObservationDoesNotAdvanceHighWater(t *testing.T) {
	h := newHarness()
	sinkErr := errors.New("account usage observation names unknown turn")
	h.state.err = sinkErr
	c := New(h.config(t, "sess-1", "/unused.sock"))
	ev := &corev1.Event{
		SessionId: "sess-1", Seq: 7, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: &corev1.AccountUsageObservation{
			TurnId: "missing", Boundary: &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}},
		}},
	}

	err := c.dispatchEvent(ev)
	if !errors.Is(err, ErrLifecycleRejected) || !strings.Contains(err.Error(), sinkErr.Error()) {
		t.Fatalf("dispatch err = %v, want ErrLifecycleRejected carrying sink cause", err)
	}
	<-h.state.ch
	if got := h.seq.LastSeq("sess-1"); got != 0 || c.lastSeen != 0 {
		t.Fatalf("rejected observation advanced sequence: store=%d client=%d", got, c.lastSeen)
	}
}

func TestRejectedFrameDoesNotAdvanceHighWater(t *testing.T) {
	h := newHarness()
	sinkErr := errors.New("response usage has no validated root-turn claim")
	h.frame.err = sinkErr
	c := New(h.config(t, "sess-1", "/unused.sock"))
	ev := &corev1.Event{SessionId: "sess-1", Seq: 7, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_Vendor{Vendor: &anypb.Any{}}}

	err := c.dispatchEvent(ev)
	if !errors.Is(err, ErrLifecycleRejected) || !errors.Is(err, sinkErr) || !strings.Contains(err.Error(), "frame sink rejected") {
		t.Fatalf("dispatch err = %v, want terminal lifecycle rejection carrying frame sink cause", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("sess-1"); got != 0 || c.lastSeen != 0 {
		t.Fatalf("rejected frame advanced sequence: store=%d client=%d", got, c.lastSeen)
	}
}

func TestResumedQueryPinsDurableCursorUntilRuntimeIdentityIsAccepted(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	created := &corev1.Event{SessionId: "vendor-session", Seq: 7, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "query", Event: &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "vendor-session"}}}},
	}}}
	if err := c.dispatchEvent(created); err != nil {
		t.Fatalf("QueryCreated: %v", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 0 {
		t.Fatalf("durable cursor after resumed QueryCreated = %d, want pinned before commitment", got)
	}

	runtime := &corev1.Event{SessionId: "vendor-session", Seq: 8, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "query", Event: &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{Identity: &corev1.QueryRuntimeIdentity{VendorSessionId: "vendor-session"}}},
	}}}
	if err := c.dispatchEvent(runtime); err != nil {
		t.Fatalf("QueryRuntimeObserved: %v", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 8 {
		t.Fatalf("durable cursor after accepted runtime identity = %d, want 8", got)
	}
}

func TestRejectedRuntimeIdentityKeepsResumeCommitmentPinnedForReplacementController(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	created := &corev1.Event{SessionId: "requested", Seq: 7, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "query", Event: &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "requested"}}}},
	}}}
	if err := c.dispatchEvent(created); err != nil {
		t.Fatalf("QueryCreated: %v", err)
	}
	assertRecv(t, h.frame.ch)
	h.frame.err = errors.New("resumed runtime identity mismatch")
	runtime := &corev1.Event{SessionId: "requested", Seq: 8, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "query", Event: &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{Identity: &corev1.QueryRuntimeIdentity{VendorSessionId: "replacement"}}},
	}}}
	if err := c.dispatchEvent(runtime); !errors.Is(err, ErrLifecycleRejected) {
		t.Fatalf("rejected runtime identity = %v, want terminal lifecycle rejection", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 0 || c.pendingResumeQuery != "query" {
		t.Fatalf("rejected runtime advanced resume commitment: durable=%d pending=%q", got, c.pendingResumeQuery)
	}
}

func TestDurableCursorPinsWholeTurnUntilTerminalSinkCommits(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	startSession := &corev1.Event{SessionId: "vendor-session", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{VendorSessionId: "vendor-session"}}}
	startTurn := &corev1.Event{SessionId: "vendor-session", Seq: 2, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn"}}}
	response := &corev1.Event{SessionId: "vendor-session", Seq: 3, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_Vendor{Vendor: &anypb.Any{}}}
	endTurn := &corev1.Event{SessionId: "vendor-session", Seq: 4, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "turn"}}}
	if err := c.dispatchEvent(startSession); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(startTurn); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(response); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 1 {
		t.Fatalf("durable cursor during turn = %d, want 1 before TurnStarted", got)
	}
	if c.lastSeen != 3 {
		t.Fatalf("volatile cursor during turn = %d, want 3", c.lastSeen)
	}
	h.state.err = errors.New("terminal accounting transaction rejected")
	if err := c.dispatchEvent(endTurn); !errors.Is(err, ErrLifecycleRejected) {
		t.Fatalf("terminal rejection = %v, want ErrLifecycleRejected", err)
	}
	assertRecv(t, h.state.ch)
	if got := h.seq.LastSeq("agent-session"); got != 1 {
		t.Fatalf("failed terminal transaction advanced durable cursor to %d", got)
	}
	h.state.err = nil
	if err := c.dispatchEvent(endTurn); err != nil {
		t.Fatalf("terminal replay: %v", err)
	}
	assertRecv(t, h.state.ch)
	if got := h.seq.LastSeq("agent-session"); got != 4 {
		t.Fatalf("committed terminal cursor = %d, want 4", got)
	}
}

func TestDurableCursorPinsRotatedTurnFromClaimBridgeUntilTerminalSinkCommits(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	baseline := &corev1.Event{SessionId: "vendor-new", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{VendorSessionId: "vendor-new"}}}
	bridge := &corev1.Event{SessionId: "vendor-new", Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{TurnId: "turn", PreviousSessionId: "vendor-old"}}}
	response := &corev1.Event{SessionId: "vendor-new", Seq: 3, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_Vendor{Vendor: &anypb.Any{}}}
	endTurn := &corev1.Event{SessionId: "vendor-new", Seq: 4, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "turn"}}}

	if err := c.dispatchEvent(baseline); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(bridge); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.claims.ch)
	if err := c.dispatchEvent(response); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 1 {
		t.Fatalf("durable cursor during bridged turn = %d, want 1 before bridge", got)
	}
	if err := c.dispatchEvent(endTurn); err != nil {
		t.Fatalf("terminal boundary after bridge: %v", err)
	}
	assertRecv(t, h.state.ch)
	if got := h.seq.LastSeq("agent-session"); got != 4 {
		t.Fatalf("committed bridged terminal cursor = %d, want 4", got)
	}
}

func TestDurableCursorPinsTypedTerminationUntilGenericCompanion(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	baseline := &corev1.Event{SessionId: "vendor-session", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{VendorSessionId: "vendor-session"}}}
	lifecycle := &corev1.Event{SessionId: "vendor-session", Seq: 2, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{QueryInstanceId: "query", ObservedAtMs: 1234, Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor-session"}, Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}}}}}
	queryID := "query"
	companion := &corev1.Event{SessionId: "vendor-session", Seq: 3, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID}}}
	if err := c.dispatchEvent(baseline); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(lifecycle); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 1 {
		t.Fatalf("typed termination advanced durable cursor to %d", got)
	}
	if err := c.dispatchEvent(companion); err != nil {
		t.Fatal(err)
	}
	<-h.deg.ds
	if got := h.seq.LastSeq("agent-session"); got != 3 {
		t.Fatalf("complete termination pair cursor = %d, want 3", got)
	}
}

func TestDurableCursorPinsStartupFailureUntilItsExactGenericCompanion(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	baseline := &corev1.Event{SessionId: "vendor-session", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{VendorSessionId: "vendor-session"}}}
	lifecycle := &corev1.Event{SessionId: "vendor-session", Seq: 2, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{QueryInstanceId: "startup-query", ObservedAtMs: 1234, Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionIdentityUnavailable{VendorSessionIdentityUnavailable: &corev1.VendorSessionIdentityUnavailable{}}, Reason: &corev1.QueryTerminated_StartupFailure{StartupFailure: &corev1.QueryStartupFailure{Cause: "daemon connection refused"}}}}}}}
	queryID := "startup-query"
	companion := &corev1.Event{SessionId: "vendor-session", Seq: 3, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID}}}
	if err := c.dispatchEvent(baseline); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(lifecycle); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 1 {
		t.Fatalf("startup failure advanced durable cursor to %d before its companion", got)
	}
	if err := c.dispatchEvent(companion); err != nil {
		t.Fatal(err)
	}
	<-h.deg.ds
	if got := h.seq.LastSeq("agent-session"); got != 3 {
		t.Fatalf("startup failure companion cursor = %d, want 3", got)
	}
}

func TestDurableCursorAdvancesPastIntentionalTerminationBeforeNextUnexpectedQuery(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "agent-session", "/unused.sock"))
	baseline := &corev1.Event{SessionId: "vendor-session", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{VendorSessionId: "vendor-session"}}}
	intentional := &corev1.Event{SessionId: "vendor-session", Seq: 2, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{QueryInstanceId: "retired-query", ObservedAtMs: 1234, Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor-session"}, Reason: &corev1.QueryTerminated_Intentional{Intentional: &corev1.IntentionalQueryTermination{Reason: "SIGTERM"}}}}}}}
	unexpected := &corev1.Event{SessionId: "vendor-session", Seq: 3, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{QueryInstanceId: "resumed-query", ObservedAtMs: 2345, Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor-session"}, Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}}}}}
	queryID := "resumed-query"
	companion := &corev1.Event{SessionId: "vendor-session", Seq: 4, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID}}}

	if err := c.dispatchEvent(baseline); err != nil {
		t.Fatal(err)
	}
	assertRecv(t, h.state.ch)
	if err := c.dispatchEvent(intentional); err != nil {
		t.Fatalf("intentional termination: %v", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 2 {
		t.Fatalf("intentional termination cursor = %d, want 2", got)
	}
	if err := c.dispatchEvent(unexpected); err != nil {
		t.Fatalf("unexpected termination after intentional shutdown: %v", err)
	}
	assertRecv(t, h.frame.ch)
	if got := h.seq.LastSeq("agent-session"); got != 2 {
		t.Fatalf("unexpected termination cursor = %d, want pin at 2 pending companion", got)
	}
	if err := c.dispatchEvent(companion); err != nil {
		t.Fatalf("unexpected termination companion: %v", err)
	}
	<-h.deg.ds
	if got := h.seq.LastSeq("agent-session"); got != 4 {
		t.Fatalf("completed resumed termination cursor = %d, want 4", got)
	}
}

func TestReplayCursorViolationFailsBeforeSinkMutationAndLogsIdentity(t *testing.T) {
	h := newHarness()
	var logs []string
	cfg := h.config(t, "agent-session", "/unused.sock")
	cfg.Logf = func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }
	c := New(cfg)
	event := &corev1.Event{SessionId: "vendor-session", Seq: 4, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "turn", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "turn"}}}
	err := c.dispatchEvent(event)
	if !errors.Is(err, ErrReplayCursorInvariant) {
		t.Fatalf("error = %v, want ErrReplayCursorInvariant", err)
	}
	select {
	case got := <-h.state.ch:
		t.Fatalf("cursor violation reached state sink: %+v", got)
	default:
	}
	if c.lastSeen != 0 || h.seq.LastSeq("agent-session") != 0 {
		t.Fatalf("cursor violation mutated cursors: volatile=%d durable=%d", c.lastSeen, h.seq.LastSeq("agent-session"))
	}
	joined := strings.Join(logs, "\n")
	if !strings.Contains(joined, "replay cursor invariant REJECTED") || !strings.Contains(joined, "session=agent-session") || !strings.Contains(joined, "seq=4") || !strings.Contains(joined, `turn "turn"`) {
		t.Fatalf("logs = %v", logs)
	}
}

func TestRejectedLifecycleEventTerminatesInsteadOfReconnectLoop(t *testing.T) {
	h := newHarness()
	h.state.err = errors.New("turn end has no durable active claim")
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		mustWriteMsg(t, conn, persistentTurnEnd("sess-1", 7))
		_, _ = wire.ReadAny(conn)
	})
	c := New(h.config(t, "sess-1", path))
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	err := c.Run(ctx)
	if !errors.Is(err, ErrLifecycleRejected) {
		t.Fatalf("Run err = %v, want terminal ErrLifecycleRejected", err)
	}
	if got := h.seq.LastSeq("sess-1"); got != 0 {
		t.Fatalf("last sequence = %d, want rejected seq uncommitted", got)
	}
}

func TestRejectedTurnClaimBridgeDoesNotAdvanceOrLeakToOtherSinks(t *testing.T) {
	h := newHarness()
	sinkErr := errors.New("bridge contradicts durable start receipt")
	h.claims.err = sinkErr
	c := New(h.config(t, "sess-1", "/unused.sock"))
	ev := &corev1.Event{
		SessionId: "vendor-new",
		Seq:       3,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: "turn-1",
		Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
			TurnId: "turn-1", PreviousSessionId: "vendor-old",
		}},
	}

	err := c.dispatchEvent(ev)
	if !errors.Is(err, ErrTurnClaimRejected) ||
		!strings.Contains(err.Error(), sinkErr.Error()) {
		t.Fatalf("dispatch err = %v, want ErrTurnClaimRejected carrying sink cause", err)
	}
	assertRecv(t, h.claims.ch)
	if got := h.seq.LastSeq("sess-1"); got != 0 || c.lastSeen != 0 {
		t.Fatalf("rejected bridge advanced sequence: store=%d client=%d", got, c.lastSeen)
	}
	select {
	case got := <-h.state.ch:
		t.Fatalf("rejected bridge reached lifecycle sink: %+v", got)
	default:
	}
	select {
	case got := <-h.frame.ch:
		t.Fatalf("rejected bridge reached frontend sink: %+v", got)
	default:
	}
}

func TestEphemeralSeqZeroDoesNotAdvanceHighWater(t *testing.T) {
	// Arrange
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 7)); err != nil {
		t.Fatalf("persistent event: %v", err)
	}
	<-h.state.ch

	// Act: an ephemeral ContentDelta (seq 0) must not regress or advance.
	ephemeral := &corev1.Event{
		SessionId: "sess-1",
		Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u1"}},
	}
	err := c.dispatchEvent(ephemeral)

	// Assert
	if err != nil {
		t.Fatalf("ephemeral event should not error: %v", err)
	}
	<-h.frame.ch // routed to frame sink
	if last := h.seq.LastSeq("sess-1"); last != 7 {
		t.Fatalf("ephemeral must not touch high-water: got %d want 7", last)
	}
}

func TestFilePlaneDiagnosticPersistsWithoutEnteringOtherSinks(t *testing.T) {
	h := newHarness()
	diagnostics := &recordingFileDiagnostics{got: make(chan *corev1.Event, 1)}
	cfg := h.config(t, "agent-session", "/unused.sock")
	cfg.FileDiagnostics = diagnostics
	c := New(cfg)
	context, err := structpb.NewStruct(map[string]any{"file": "events.jsonl"})
	if err != nil {
		t.Fatal(err)
	}
	ev := &corev1.Event{SessionId: "claude-session", Seq: 9, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Plane: corev1.Plane_PLANE_FILE, ProducedAtMs: 1234,
		Payload: &corev1.Event_FilePlaneDiagnostic{FilePlaneDiagnostic: &corev1.FilePlaneDiagnostic{
			SourceRuntime: corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR,
			Level:         "error", Verbosity: "normal", Operation: "sidecar.ingest.failed", Message: "ingest failed", Context: context, SourcePid: 42, SourcePath: "/tmp/events.jsonl",
		}}}
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatch file-plane diagnostic: %v", err)
	}
	assertRecv(t, diagnostics.got)
	if got := h.seq.LastSeq("agent-session"); got != 9 {
		t.Fatalf("last sequence=%d, want 9", got)
	}
	for name, channel := range map[string]chan *corev1.Event{"state": h.state.ch, "frame": h.frame.ch} {
		select {
		case <-channel:
			t.Fatalf("file-plane diagnostic entered %s sink", name)
		default:
		}
	}
}

func TestFilePlaneDiagnosticRejectsWrongPlane(t *testing.T) {
	h := newHarness()
	cfg := h.config(t, "agent-session", "/unused.sock")
	cfg.FileDiagnostics = &recordingFileDiagnostics{got: make(chan *corev1.Event, 1)}
	c := New(cfg)
	err := c.dispatchEvent(&corev1.Event{SessionId: "claude-session", Seq: 1, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Plane: corev1.Plane_PLANE_STREAM, ProducedAtMs: 1,
		Payload: &corev1.Event_FilePlaneDiagnostic{FilePlaneDiagnostic: &corev1.FilePlaneDiagnostic{SourceRuntime: corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR, Level: "info", Verbosity: "normal", Operation: "sidecar.x", Message: "x", Context: &structpb.Struct{}, SourcePid: 1, SourcePath: "/tmp/x"}}})
	if err == nil {
		t.Fatal("wrong-plane file diagnostic was accepted")
	}
}

func TestFilePlaneDiagnosticDoesNotAdvanceSequenceUntilPersisted(t *testing.T) {
	h := newHarness()
	context, err := structpb.NewStruct(map[string]any{})
	if err != nil {
		t.Fatal(err)
	}
	ev := &corev1.Event{SessionId: "claude-session", Seq: 7, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, Plane: corev1.Plane_PLANE_FILE, ProducedAtMs: 1,
		Payload: &corev1.Event_FilePlaneDiagnostic{FilePlaneDiagnostic: &corev1.FilePlaneDiagnostic{SourceRuntime: corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR, Level: "error", Verbosity: "normal", Operation: "sidecar.x", Message: "x", Context: context, SourcePid: 1}}}
	failingConfig := h.config(t, "agent-session", "/unused.sock")
	failingConfig.FileDiagnostics = failingFileDiagnostics{}
	failing := New(failingConfig)
	if err := failing.dispatchEvent(ev); err == nil {
		t.Fatal("failed persistence was accepted")
	}
	if got := h.seq.LastSeq("agent-session"); got != 0 || failing.lastSeen != 0 {
		t.Fatalf("failed persistence advanced sequence: store=%d client=%d", got, failing.lastSeen)
	}
	successConfig := h.config(t, "agent-session", "/unused.sock")
	successConfig.FileDiagnostics = &recordingFileDiagnostics{got: make(chan *corev1.Event, 1)}
	success := New(successConfig)
	if err := success.dispatchEvent(ev); err != nil {
		t.Fatalf("successful retry: %v", err)
	}
	if got := h.seq.LastSeq("agent-session"); got != 7 {
		t.Fatalf("successful retry sequence=%d, want 7", got)
	}
}

func TestEventRouting(t *testing.T) {
	vendorAny, err := anypb.New(&corev1.TurnEnded{StopReason: "vendor-wrapped"})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	tests := []struct {
		name string
		ev   *corev1.Event
		want string // "state" | "frame" | "degraded"
	}{
		{
			name: "session started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}}},
			want: "state",
		},
		{
			name: "turn started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}},
			want: "state",
		},
		{
			name: "turn claim bridge to dedicated ledger sink",
			ev: &corev1.Event{
				SessionId: "s",
				Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
					TurnId: "turn-1", PreviousSessionId: "s-old",
				}},
			},
			want: "claim",
		},
		{
			name: "task started to state sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1"}}},
			want: "state",
		},
		{
			name: "account usage observation to error-returning state sink",
			ev: &corev1.Event{SessionId: "s", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: &corev1.AccountUsageObservation{
				TurnId: "turn-1", Boundary: &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}},
			}}},
			want: "state",
		},
		{
			name: "content delta to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u"}}},
			want: "frame",
		},
		{
			name: "message latency to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{Uuid: "m", TtftMs: 865}}},
			want: "frame",
		},
		{
			name: "heartbeat progress to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "t"}}},
			want: "frame",
		},
		{
			name: "vendor payload to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_Vendor{Vendor: vendorAny}},
			want: "frame",
		},
		{
			// A clear is CONVERSATION content: it renders as its own bubble and
			// floors the frontend's replay. Nothing in the SSM's state axes moves
			// because a conversation's history stopped informing the agent, so it
			// belongs to the frame sink and not the lifecycle sink.
			name: "context cleared to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}}},
			want: "frame",
		},
		{
			name: "context compacted to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{}}},
			want: "frame",
		},
		{
			name: "unparsed to frame sink",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{Producer: "claude-shim"}}},
			want: "frame",
		},
		{
			name: "degraded state to degraded reporter",
			ev:   &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "store-client"}}},
			want: "degraded",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness()
			c := New(h.config(t, "s", "/unused.sock"))

			// Act
			if err := c.dispatchEvent(tt.ev); err != nil {
				t.Fatalf("dispatchEvent: %v", err)
			}

			// Assert
			switch tt.want {
			case "state":
				assertRecv(t, h.state.ch)
			case "claim":
				assertRecv(t, h.claims.ch)
			case "frame":
				assertRecv(t, h.frame.ch)
			case "degraded":
				select {
				case <-h.deg.ds:
				case <-time.After(time.Second):
					t.Fatal("degraded reporter never called")
				}
			}
		})
	}
}

func TestTurnClaimBridgeCannotReachLifecycleOrFrontendSinks(t *testing.T) {
	h := newHarness()
	c := New(h.config(t, "s", "/unused.sock"))
	ev := &corev1.Event{
		SessionId: "vendor-new",
		Seq:       2,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: "turn-1",
		Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
			TurnId: "turn-1", PreviousSessionId: "vendor-old",
		}},
	}

	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}
	assertRecv(t, h.claims.ch)
	select {
	case got := <-h.state.ch:
		t.Fatalf("bridge reached lifecycle StateSink: %+v", got)
	default:
	}
	select {
	case got := <-h.frame.ch:
		t.Fatalf("bridge reached frontend FrameSink: %+v", got)
	default:
	}
}

// splitLevelConfig wires the client's info and warn channels to separate
// sinks so a test can assert WHICH one a record took. A shim-reported
// degradation that lands on the info channel is invisible to a level filter,
// which is the defect these tests fence.
func splitLevelConfig(t *testing.T, h *harness, info, warn *[]string) Config {
	t.Helper()
	cfg := h.config(t, "s", "/unused.sock")
	cfg.Logf = func(format string, args ...any) { *info = append(*info, fmt.Sprintf(format, args...)) }
	cfg.Warnf = func(format string, args ...any) { *warn = append(*warn, fmt.Sprintf(format, args...)) }
	return cfg
}

func TestShimDegradedStateTakesTheWarnChannel(t *testing.T) {
	// Arrange.
	var info, warn []string
	c := New(splitLevelConfig(t, newHarness(), &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
		Component: "claude-shim-sdk", Reason: "unexpected_query_termination",
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(warn, "\n"), "shim reported DegradedState") {
		t.Fatalf("warn = %v, want the shim degradation at warn", warn)
	}
}

func TestHistoricalShimDegradedStateStaysOnTheInfoChannel(t *testing.T) {
	// Arrange -- the reporter classified this row as a RETIRED query's, replayed
	// off the durable sequence. The anomaly was warned about when it happened;
	// re-warning at every later bring-up would alarm forever over one durable
	// row.
	var info, warn []string
	h := newHarness()
	h.deg.disposition = DegradationHistorical
	c := New(splitLevelConfig(t, h, &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
		Component: "claude-shim-sdk", Reason: "unexpected_query_termination",
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if len(warn) != 0 {
		t.Fatalf("warn = %v, want a replayed degradation recorded at info only", warn)
	}
}

func TestHistoricalShimDegradedStateKeepsItsRecord(t *testing.T) {
	// Arrange -- the severity moved; the record did not. A silent drop would be
	// strictly worse than the noise it replaced.
	var info, warn []string
	h := newHarness()
	h.deg.disposition = DegradationHistorical
	c := New(splitLevelConfig(t, h, &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
		Component: "claude-shim-sdk", Reason: "unexpected_query_termination",
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	joined := strings.Join(info, "\n")
	if !strings.Contains(joined, "shim reported DegradedState") || !strings.Contains(joined, "disposition=historical") {
		t.Fatalf("info = %v, want the replayed degradation recorded with its disposition", info)
	}
}

func TestLiveShimDegradedStateNamesItsDispositionToo(t *testing.T) {
	// Arrange -- the live arm's severity is untouched, and its record carries
	// the same verdict field so the two are told apart by reading, not by
	// absence.
	var info, warn []string
	c := New(splitLevelConfig(t, newHarness(), &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
		Component: "claude-shim-sdk", Reason: "unexpected_query_termination",
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(warn, "\n"), "disposition=live") {
		t.Fatalf("warn = %v, want the live degradation named as such at warn", warn)
	}
}

func TestRecoveredShimDegradedStateStaysOnTheInfoChannel(t *testing.T) {
	// Arrange -- the RECOVERY of a degradation is ordinary good news and must
	// not inflate the warn channel.
	var info, warn []string
	c := New(splitLevelConfig(t, newHarness(), &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
		Component: "claude-shim-sdk", Recovered: true,
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if len(warn) != 0 {
		t.Fatalf("warn = %v, want a recovery recorded at info only", warn)
	}
}

func TestUnparsedEventTakesTheWarnChannel(t *testing.T) {
	// Arrange -- an unparsable vendor line is conversation content the user
	// will never see.
	var info, warn []string
	c := New(splitLevelConfig(t, newHarness(), &info, &warn))
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{
		Producer: "claude-shim", Error: "bad json",
	}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(warn, "\n"), "received UnparsedEvent") {
		t.Fatalf("warn = %v, want the unparsable event at warn", warn)
	}
}

func TestShimDegradedStateStillRecordsWithNoWarnChannelWired(t *testing.T) {
	// Arrange -- an unwired warn channel must lose the SEVERITY, never the
	// record.
	var info []string
	cfg := newHarness().config(t, "s", "/unused.sock")
	cfg.Logf = func(format string, args ...any) { info = append(info, fmt.Sprintf(format, args...)) }
	c := New(cfg)
	ev := &corev1.Event{SessionId: "s", Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{Component: "store-client"}}}

	// Act.
	if err := c.dispatchEvent(ev); err != nil {
		t.Fatalf("dispatchEvent: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(info, "\n"), "shim reported DegradedState") {
		t.Fatalf("info = %v, want the degradation still recorded through Logf", info)
	}
}

func assertRecv(t *testing.T, ch chan *corev1.Event) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(time.Second):
		t.Fatal("expected sink never received the event")
	}
}
