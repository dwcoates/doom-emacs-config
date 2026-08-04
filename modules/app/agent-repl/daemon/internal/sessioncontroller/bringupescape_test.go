package sessioncontroller

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
// bring-up can fail and the ladder's same-conversation retry can succeed (or fail again)
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
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
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
// live session controller — the DegradedState the shim sends immediately before it shuts
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

func TestResumeTransportFailurePreservesThePointerAndRetriesTheSameConversation(t *testing.T) {
	// Arrange — the first connection dies before readiness. The replacement
	// transport comes up against the same durable conversation.
	h := newEscapeHarness(t, &fakeClient{awaitErr: errBringUpDead}, &fakeClient{})
	h.spawner.resume["s1"] = "uuid-gone"

	// Act.
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("ensure after a transport death = %v, want the same-conversation retry to succeed", err)
	}
	if d == nil {
		t.Fatal("ensure returned no session controller")
	}
	if got := h.spawner.resume["s1"]; got != "uuid-gone" {
		t.Fatalf("resume pointer = %q, want uuid-gone preserved", got)
	}
	if !h.log.contains("retry=same_conversation_once") || !h.log.contains(`resume="uuid-gone"`) {
		t.Fatalf("transport retry decision was not self-evident in logs: %v", h.log.lines)
	}
}

func TestResumeVendorFailurePreservesHistoryAndDoesNotInventStaleEvidence(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t, blocked())
	h.spawner.resume["s1"] = "uuid-gone"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	h.sdkDied(t, "SDK stream failed: process exited with code 1")

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("an SDK failure during bring-up reported success")
	}

	// Assert — an arbitrary SDK error is not affirmative evidence that the
	// transcript is invalid. The pointer and history remain untouched.
	if got := h.spawner.resume["s1"]; got != "uuid-gone" {
		t.Fatalf("resume pointer = %q, want uuid-gone preserved", got)
	}
	if !h.log.contains("resume_decision=preserve") || !h.log.contains("failure_kind=vendor") {
		t.Fatalf("vendor failure preservation was not logged: %v", h.log.lines)
	}
}

func TestResumedQueryStartupFailureRetainsTypedTerminationThroughDriveabilityFailure(t *testing.T) {
	// Arrange — a resumed query dies before its handshake can make the session
	// driveable. The lifecycle record is the authoritative reason and identity.
	h := newEscapeHarness(t, blocked())
	h.spawner.resume["s1"] = "vendor-resume"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	if err := d.consumer.Consume(&corev1.Event{
		Seq: 17,
		Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
			QueryInstanceId: "resumed-query",
			ObservedAtMs:    1234,
			Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{
				VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor-resume"},
				Reason:         &corev1.QueryTerminated_StartupFailure{StartupFailure: &corev1.QueryStartupFailure{Cause: "resume rejected"}},
			}},
		}},
	}); err != nil {
		t.Fatalf("consume termination: %v", err)
	}

	// Act.
	_, err = h.m.ensure(context.Background(), "ws")

	// Assert — callers retain the original error chain and can obtain the exact
	// typed lifecycle evidence without re-parsing an error string.
	if err == nil {
		t.Fatal("ensure succeeded after a pre-readiness startup failure")
	}
	var detailer interface {
		QueryTerminationFailureDetail() *frontendv1.QueryTerminationFailure
	}
	if !errors.As(err, &detailer) {
		t.Fatalf("ensure error %v carries no typed query-termination detail", err)
	}
	detail := detailer.QueryTerminationFailureDetail()
	if detail == nil || detail.GetAgentReplSessionId() != "s1" || detail.GetQueryInstanceId() != "resumed-query" || detail.GetVendorSessionId() != "vendor-resume" || detail.GetStartupFailure() == nil || detail.GetStartupFailure().GetCause() != "resume rejected" {
		t.Fatalf("typed bring-up termination = %v", detail)
	}
	if !h.log.contains("BRING-UP QUERY TERMINATION retained") {
		t.Fatalf("typed bring-up retention was not loud-logged: %v", h.log.lines)
	}
}

func TestTheSameConversationTransportRetryHappensExactlyOnce(t *testing.T) {
	// Arrange — both transports die. A ladder that retried on the second failure
	// too would spin forever against a genuinely broken environment.
	h := newEscapeHarness(t, &fakeClient{awaitErr: errBringUpDead}, &fakeClient{awaitErr: errBringUpDead})
	h.spawner.resume["s1"] = "uuid-gone"

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("ensure succeeded with both bring-ups dead")
	}
	if got := h.spawner.calls; len(got) != 2 {
		t.Fatalf("EnsureShim calls = %v, want exactly two bounded attempts", got)
	}
}

func TestTransportRetrySpawnFailurePreservesResumeAndResolvesTheAxis(t *testing.T) {
	// The first transport failed after it had selected a durable conversation.
	// A replacement that cannot even spawn must surface that error without
	// clearing or rewriting the resume pointer.
	h := newEscapeHarness(t, &fakeClient{})
	h.spawner.resume["s1"] = "uuid-live"
	failed, err := h.m.bringUp("ws")
	if err != nil {
		t.Fatalf("initial bringUp: %v", err)
	}
	h.spawner.err = errors.New("shim launcher unavailable")

	_, err = h.m.escapeFailedBringUp(context.Background(), "ws", failed,
		&bringUpFailure{kind: bringUpFailureTransport, cause: errBringUpDead})

	if err == nil || !strings.Contains(err.Error(), "shim launcher unavailable") {
		t.Fatalf("ensure error = %v", err)
	}
	if got := h.spawner.resume["s1"]; got != "uuid-live" {
		t.Fatalf("resume pointer = %q, want preserved uuid-live", got)
	}
	if !h.hasCard(errclass.TypeSessionStartFailed) {
		t.Fatalf("spawn failure did not close starting with a failure card: %v", h.failureCards())
	}
}

func TestRetiredGenerationFailureCannotStopOrRewriteTheCurrentGeneration(t *testing.T) {
	// Arrange — a replacement generation is already healthy when an older
	// initiating caller finally receives its timeout.
	h := newEscapeHarness(t)
	retired := &sessionController{
		sessionID: "s1", workspace: "ws", generationID: "generation-retired",
		resumedVendorSessionID: "uuid-live", client: &fakeClient{}, faulted: make(chan struct{}),
	}
	current := &sessionController{
		sessionID: "s1", workspace: "ws", generationID: "generation-current",
		resumedVendorSessionID: "uuid-live", client: &fakeClient{}, cancel: func() {}, faulted: make(chan struct{}),
	}
	h.m.mu.Lock()
	h.m.byWS["ws"] = current
	h.m.mu.Unlock()
	h.spawner.resume["s1"] = "uuid-live"

	// Act.
	got, err := h.m.escapeFailedBringUp(context.Background(), "ws", retired,
		&bringUpFailure{kind: bringUpFailureTimeout, cause: context.DeadlineExceeded})

	// Assert — the retired waiter observes the current route and has no
	// authority to stop a shim or alter durable resume identity.
	if err != nil || got != current {
		t.Fatalf("escape returned controller=%p err=%v, want current controller %p", got, err, current)
	}
	if stopped := h.spawner.stoppedSessions(); len(stopped) != 0 {
		t.Fatalf("retired generation stopped sessions %v", stopped)
	}
	if got := h.spawner.resume["s1"]; got != "uuid-live" {
		t.Fatalf("resume pointer = %q, want uuid-live", got)
	}
	if !h.log.contains("decision=observe_current") || !h.log.contains("resume_decision=preserve") {
		t.Fatalf("retired-generation decision was not logged: %v", h.log.lines)
	}
}

func TestRetiredGenerationFailureCannotResurrectAHibernatedWorkspace(t *testing.T) {
	// Arrange — hibernation already removed the controller while its original
	// bring-up waiter was still unwinding.
	h := newEscapeHarness(t)
	canceled := false
	retired := &sessionController{
		sessionID: "s1", workspace: "ws", generationID: "generation-retired",
		resumedVendorSessionID: "uuid-live", client: &fakeClient{},
		cancel: func() { canceled = true }, faulted: make(chan struct{}),
	}
	cause := &bringUpFailure{kind: bringUpFailureCanceled, cause: context.Canceled}

	// Act.
	got, err := h.m.escapeFailedBringUp(context.Background(), "ws", retired, cause)

	// Assert — no current owner means there is nothing to observe and no
	// authority to respawn. Only the retired private client is canceled.
	if got != nil || !errors.Is(err, context.Canceled) {
		t.Fatalf("escape returned controller=%v err=%v", got, err)
	}
	if !canceled {
		t.Fatal("retired private client was not canceled")
	}
	if got := h.spawner.calls; len(got) != 0 {
		t.Fatalf("hibernated workspace was respawned: EnsureShim calls=%v", got)
	}
	if stopped := h.spawner.stoppedSessions(); len(stopped) != 0 {
		t.Fatalf("retired generation stopped sessions %v", stopped)
	}
	if !h.log.contains("decision=return_failure") || !h.log.contains("stop_shim=false") {
		t.Fatalf("hibernated retired-generation decision was not logged: %v", h.log.lines)
	}
}

func TestTransportRetryRejectsEveryChangedResumeIdentity(t *testing.T) {
	for _, actual := range []string{"", "uuid-other"} {
		t.Run("actual="+actual, func(t *testing.T) {
			// Arrange — an already-resumed conversation can only retry against
			// the exact same UUID. An empty value is a fresh conversation and is
			// therefore as invalid as a different UUID.
			h := newEscapeHarness(t, &fakeClient{})
			failed := &sessionController{
				sessionID: "s1", workspace: "ws", generationID: "generation-first",
				resumedVendorSessionID: "uuid-live", client: &fakeClient{}, cancel: func() {}, faulted: make(chan struct{}),
			}
			h.m.mu.Lock()
			h.m.byWS["ws"] = failed
			h.m.mu.Unlock()
			h.spawner.resume["s1"] = actual

			// Act.
			_, err := h.m.escapeFailedBringUp(context.Background(), "ws", failed,
				&bringUpFailure{kind: bringUpFailureTransport, cause: errBringUpDead})

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "changed resume identity") {
				t.Fatalf("escape error = %v", err)
			}
			if h.m.Live("ws") {
				t.Fatal("an identity-changing retry left a live session controller")
			}
			if !h.log.contains("resume_decision=abort") {
				t.Fatalf("identity invariant failure was not logged: %v", h.log.lines)
			}
		})
	}
}

func TestEnsureShimHardFailurePropagatesWithoutCreatingALiveController(t *testing.T) {
	// Arrange — the spawner has rejected restoration before starting a shim.
	// A session controller must not appear for a conversation that never
	// acquired its exact resume transport.
	h := newEscapeHarness(t)
	h.spawner.resume["s1"] = "uuid-live"
	h.spawner.err = errors.New("resume target uuid-live has no transcript")

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "resume target uuid-live has no transcript") {
		t.Fatalf("ensure error = %v", err)
	}
	if h.m.Live("ws") {
		t.Fatal("a rejected resume created a live session controller")
	}
	if got := h.spawner.calls; len(got) != 1 || got[0] != "s1" {
		t.Fatalf("EnsureShim calls = %v, want exactly [s1]", got)
	}
	if got := h.spawner.resume["s1"]; got != "uuid-live" {
		t.Fatalf("resume identity = %q, want uuid-live preserved", got)
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

func TestStartFailedClosesTheLegacyConnectivityProjection(t *testing.T) {
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
		if w.workspace == "ws" && w.wiring == ssm.WiringSevered && w.reason == "bring_up_failed" {
			return
		}
	}
	t.Fatalf("no bring-up-failed close on the legacy connectivity projection; calls=%v", h.applier.wiringsApplied())
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
