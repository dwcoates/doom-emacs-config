package shimclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// runConnectedClient starts c.Run and returns a channel that fires on each
// successful attach plus a stop func that cancels and joins Run.
func runConnectedClient(t *testing.T, cfg Config) (*Client, <-chan *corev1.ShimHello, func()) {
	t.Helper()
	connected := make(chan *corev1.ShimHello, 8)
	cfg.OnConnected = func(h *corev1.ShimHello) bool { connected <- h; return false }
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() { errCh <- c.Run(ctx) }()
	stop := func() {
		cancel()
		select {
		case <-errCh:
		case <-time.After(2 * time.Second):
			t.Error("Run did not return after cancel")
		}
	}
	return c, connected, stop
}

func waitConnected(t *testing.T, ch <-chan *corev1.ShimHello) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(2 * time.Second):
		t.Fatal("client never connected")
	}
}

func TestSetModelRejectsSyntheticBeforeControlSend(t *testing.T) {
	// A zero Client has no live control transport. The CLI marker must be
	// rejected before request-id allocation or any daemon-to-shim send.
	c := &Client{}
	selected, err := c.SetModel(context.Background(), "<synthetic>")
	if err == nil || err.Error() != "set model: model id is empty" {
		t.Fatalf("SetModel(<synthetic>) error = %v, want empty-model refusal", err)
	}
	if selected != "" {
		t.Fatalf("SetModel(<synthetic>) selected = %q, want empty", selected)
	}
}

func TestSubmitPromptAckSuccess(t *testing.T) {
	// Arrange: shim acks the prompt with its request_id.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read SubmitPrompt: %v", err)
			return
		}
		sp, ok := m.(*corev1.SubmitPrompt)
		if !ok {
			t.Errorf("expected SubmitPrompt, got %T", m)
			return
		}
		if sp.GetText() != "hello" {
			t.Errorf("prompt text: got %q", sp.GetText())
		}
		mustWriteMsg(t, conn, &corev1.Ack{RequestId: sp.GetRequestId()})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act
	err := c.SubmitPrompt(context.Background(), "", "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if err != nil {
		t.Fatalf("SubmitPrompt: want nil, got %v", err)
	}
}

// submitPromptRequestID runs one SubmitPrompt against a fake shim and reports
// the request_id that reached the wire — the id the shim adopts as the turn's
// own identity (core.proto §"Turn lifecycle authority").
func submitPromptRequestID(t *testing.T, requestID string) string {
	t.Helper()
	observed := make(chan string, 1)
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read SubmitPrompt: %v", err)
			return
		}
		sp, ok := m.(*corev1.SubmitPrompt)
		if !ok {
			t.Errorf("expected SubmitPrompt, got %T", m)
			return
		}
		observed <- sp.GetRequestId()
		mustWriteMsg(t, conn, &corev1.Ack{RequestId: sp.GetRequestId()})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	if err := c.SubmitPrompt(context.Background(), requestID, "hello", "human", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	select {
	case got := <-observed:
		return got
	case <-time.After(2 * time.Second):
		t.Fatal("the shim never received a SubmitPrompt")
		return ""
	}
}

// A caller that already published an identity for this turn — the keep-alive
// ping, whose claim, holds and window row are keyed by it before the submit —
// gets THAT id on the wire, so the turn comes back under the one name.
func TestSubmitPromptSendsTheCallersOwnRequestID(t *testing.T) {
	if got := submitPromptRequestID(t, "ka_deadbeef"); got != "ka_deadbeef" {
		t.Fatalf("wire request_id = %q, want the caller's own id %q", got, "ka_deadbeef")
	}
}

// A caller with no identity of its own says so with an empty id, and the client
// mints one: nothing daemon-side is keyed by this turn's name.
func TestSubmitPromptMintsARequestIDWhenTheCallerOwnsNone(t *testing.T) {
	if got := submitPromptRequestID(t, ""); !strings.HasPrefix(got, "daemon-prompt-") {
		t.Fatalf("wire request_id = %q, want a minted daemon-prompt-* id", got)
	}
}

func TestSubmitPromptRejectsInvalidOriginBeforeControlSend(t *testing.T) {
	for _, origin := range []corev1.PromptOrigin{
		corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
		corev1.PromptOrigin(999),
	} {
		t.Run(origin.String(), func(t *testing.T) {
			c := &Client{}
			err := c.SubmitPrompt(context.Background(), "", "hello", "human", "", origin)
			if err == nil || !strings.Contains(err.Error(), "prompt origin") {
				t.Fatalf("SubmitPrompt origin=%v error = %v, want prompt-origin refusal", origin, err)
			}
		})
	}
}

func TestHealthRequiresMatchingStatusFromLiveShim(t *testing.T) {
	// Arrange: a completed handshake followed by a health response carrying the
	// same request id proves this is the current shim connection, not merely a
	// socket that happened to accept a write.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read HealthCheck: %v", err)
			return
		}
		check, ok := m.(*corev1.HealthCheck)
		if !ok {
			t.Errorf("expected HealthCheck, got %T", m)
			return
		}
		mustWriteMsg(t, conn, &corev1.HealthStatus{RequestId: check.GetRequestId(), Healthy: true, Component: "claude-shim"})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act.
	status, err := c.Health(context.Background(), "frontend-health-1")

	// Assert.
	if err != nil {
		t.Fatalf("Health: %v", err)
	}
	if !status.GetHealthy() || status.GetRequestId() != "frontend-health-1" || status.GetComponent() != "claude-shim" {
		t.Fatalf("status = %+v", status)
	}
}

func TestHealthFailsWithoutLiveShim(t *testing.T) {
	// Arrange: no Run/handshake has made a connection active.
	c := New(Config{SessionID: "sess-1", Logf: t.Logf})

	// Act.
	_, err := c.Health(context.Background(), "frontend-health-1")

	// Assert.
	if !errors.Is(err, ErrNotConnected) {
		t.Fatalf("Health error = %v, want ErrNotConnected", err)
	}
}

func TestSubmitPromptNackIsLoudError(t *testing.T) {
	// Arrange: shim nacks the prompt.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read SubmitPrompt: %v", err)
			return
		}
		sp := m.(*corev1.SubmitPrompt)
		mustWriteMsg(t, conn, &corev1.Nack{RequestId: sp.GetRequestId(), Reason: "busy"})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act
	err := c.SubmitPrompt(context.Background(), "", "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if !errors.Is(err, ErrNack) {
		t.Fatalf("want ErrNack, got %v", err)
	}
}

func TestControlAckTimeout(t *testing.T) {
	// Arrange: shim reads the prompt but never acks.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		_, _ = wire.ReadAny(conn) // read the SubmitPrompt, then stay silent
		_, _ = wire.ReadAny(conn)
	})
	cfg := h.config(t, "sess-1", path)
	cfg.AckTimeout = 60 * time.Millisecond
	c, connected, stop := runConnectedClient(t, cfg)
	defer stop()
	waitConnected(t, connected)

	// Act
	err := c.SubmitPrompt(context.Background(), "", "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if !errors.Is(err, ErrAckTimeout) {
		t.Fatalf("want ErrAckTimeout, got %v", err)
	}
}

func TestInterruptAckSuccess(t *testing.T) {
	// Arrange
	h := newHarness()
	delivered := make(chan string, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read Interrupt: %v", err)
			return
		}
		iv, ok := m.(*corev1.Interrupt)
		if !ok {
			t.Errorf("expected Interrupt, got %T", m)
			return
		}
		delivered <- iv.GetRequestId()
		mustWriteMsg(t, conn, &corev1.Ack{
			RequestId:        iv.GetRequestId(),
			InterruptOutcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED,
		})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act
	outcome, err := c.Interrupt(context.Background(), "fe-1")

	// Assert
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if reqID := <-delivered; reqID == "" {
		t.Fatal("shim should have received an Interrupt with a request id")
	}
	if outcome != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Fatalf("outcome = %v, want INTERRUPTED (the shim's ack verdict, verbatim)", outcome)
	}
}

// A CONTROL REQUEST THAT WAS DELIVERED IS NOT ONE THAT NEVER LEFT. The shim
// dispatches synchronously, so an Interrupt that reached its socket has already
// run against the SDK; losing the receipt loses the answer and not the effect,
// and the caller must be able to tell that from an undeliverable stop.
func TestInterruptDeliveredThenDisconnectedIsNamedAsUnacked(t *testing.T) {
	// Arrange — the shim reads the Interrupt and then drops the connection
	// without answering it.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		if _, err := wire.ReadAny(conn); err != nil {
			t.Errorf("read Interrupt: %v", err)
			return
		}
		_ = conn.Close()
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act.
	_, err := c.Interrupt(context.Background(), "fe-1")

	// Assert.
	if !errors.Is(err, ErrDeliveredUnacked) {
		t.Fatalf("Interrupt err = %v, want ErrDeliveredUnacked — the shim received the stop and only its answer was lost", err)
	}
}

func TestInterruptWithNoConnectionIsNotReportedAsDelivered(t *testing.T) {
	// Arrange — no live connection at all, so nothing left the daemon.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/nonexistent/agent-shim/session.sock"))

	// Act.
	_, err := c.Interrupt(context.Background(), "fe-1")

	// Assert.
	if errors.Is(err, ErrDeliveredUnacked) {
		t.Fatalf("Interrupt err = %v, want a not-connected refusal rather than a delivered-unacked claim", err)
	}
}

func TestSubmitPromptNotConnected(t *testing.T) {
	// Arrange: a client that never connects (dials into the void via a path
	// that is never served).
	h := newHarness()
	c := New(h.config(t, "sess-1", "/nonexistent/agent-shim/session.sock"))

	// Act: no Run goroutine, so there is no live connection.
	err := c.SubmitPrompt(context.Background(), "", "hi", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if !errors.Is(err, ErrNotConnected) {
		t.Fatalf("want ErrNotConnected, got %v", err)
	}
}

func TestPermissionRequestRoundTrip(t *testing.T) {
	// Arrange: shim sends a canUseTool request and expects the matching answer.
	h := newHarness()
	h.perm = funcPerm(func(_ string, req *corev1.PermissionRequest) *corev1.PermissionResponse {
		return &corev1.PermissionResponse{
			RequestId: req.GetRequestId(),
			Decision:  corev1.PermissionDecision_PERMISSION_DECISION_ALLOW,
		}
	})
	gotResp := make(chan *corev1.PermissionResponse, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		mustWriteMsg(t, conn, &corev1.PermissionRequest{RequestId: "perm-7", ToolName: "Bash"})
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read PermissionResponse: %v", err)
			return
		}
		pr, ok := m.(*corev1.PermissionResponse)
		if !ok {
			t.Errorf("expected PermissionResponse, got %T", m)
			return
		}
		gotResp <- pr
		_, _ = wire.ReadAny(conn)
	})
	_, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act / Assert
	select {
	case pr := <-gotResp:
		if pr.GetRequestId() != "perm-7" {
			t.Fatalf("response request_id: got %q want perm-7", pr.GetRequestId())
		}
		if pr.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
			t.Fatalf("decision: got %v", pr.GetDecision())
		}
	case <-time.After(2 * time.Second):
		t.Fatal("never received PermissionResponse")
	}
}

// TestRequestIDsStayDistinctAcrossDaemonBootsAndRotations pins the property the
// durable turn ledger depends on: a request id names ONE turn for all time.
//
// The counter cannot supply that. It restarts at 1 in every process and in every
// fresh Client, so two boots reach `daemon-prompt-2` for two different turns.
// Only the random suffix separates them, and a turn id that repeats is not a
// cosmetic collision: the second turn lands on the first turn's ledger row, and
// if that row is already closed its bridge is refused against a claim it never
// owned.
func TestRequestIDsStayDistinctAcrossDaemonBootsAndRotations(t *testing.T) {
	tests := []struct {
		name    string
		boots   int
		perBoot int
	}{
		{name: "two boots reaching the same counter position", boots: 2, perBoot: 4},
		{name: "many rotations of one workspace's session", boots: 16, perBoot: 4},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — each boot is a fresh Client for the SAME session, so every
			// one of them walks the counter from 1 exactly as a restart does.
			seen := make(map[string]struct{})

			// Act.
			for boot := 0; boot < tc.boots; boot++ {
				c := New(Config{SessionID: "s_33107324a26398ef", Logf: func(string, ...any) {}})
				for i := 0; i < tc.perBoot; i++ {
					id, err := c.newRequestID("prompt")
					if err != nil {
						t.Fatalf("newRequestID: %v", err)
					}
					// Assert — no id may ever repeat, within a boot or across boots.
					if _, dup := seen[id]; dup {
						t.Fatalf("request id %q minted twice (boot %d, mint %d)", id, boot, i)
					}
					seen[id] = struct{}{}
				}
			}

			if want := tc.boots * tc.perBoot; len(seen) != want {
				t.Fatalf("distinct ids = %d, want %d", len(seen), want)
			}
		})
	}
}

// TestRequestIDCounterAloneIsNotAnIdentity pins WHY the suffix is load-bearing:
// two boots really do reach the same counter value, so an id built from the
// counter alone would collide by construction.
func TestRequestIDCounterAloneIsNotAnIdentity(t *testing.T) {
	// Arrange.
	first := New(Config{SessionID: "s_33107324a26398ef", Logf: func(string, ...any) {}})
	second := New(Config{SessionID: "s_33107324a26398ef", Logf: func(string, ...any) {}})

	// Act — mint the second id from each, the position the live collision named.
	var ids [2]string
	for i, c := range []*Client{first, second} {
		if _, err := c.newRequestID("prompt"); err != nil {
			t.Fatalf("newRequestID: %v", err)
		}
		id, err := c.newRequestID("prompt")
		if err != nil {
			t.Fatalf("newRequestID: %v", err)
		}
		ids[i] = id
	}

	// Assert — same counter prefix, different id.
	const prefix = "daemon-prompt-2-"
	for _, id := range ids {
		if !strings.HasPrefix(id, prefix) {
			t.Fatalf("id %q lacks prefix %q — the boots did not reach the same counter position", id, prefix)
		}
	}
	if ids[0] == ids[1] {
		t.Fatalf("both boots minted %q — the counter position alone decided the identity", ids[0])
	}
}

// ---------------------------------------------------------------------------
// THE ORIGIN CORRELATION. A stop travels under a DAEMON-MINTED control id,
// which is the only id the wire exchange can be correlated by and which appears
// nowhere in the vocabulary of whoever asked for it. A frontend interrupt was
// therefore unfindable end to end: the command arrived under `fe-276-1074`, the
// exchange went out under `daemon-interrupt-3-3b5adc2becd0`, and nothing joined
// them — so a stop that WAS delivered and answered INTERRUPTED within two
// milliseconds read exactly like one the daemon had silently swallowed.
// ---------------------------------------------------------------------------

// capturingLogf collects log lines so a test can assert what the record says.
type capturingLogf struct {
	mu    sync.Mutex
	lines []string
}

func (c *capturingLogf) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

// linesContaining returns every captured line mentioning needle.
func (c *capturingLogf) linesContaining(needle string) []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	var out []string
	for _, line := range c.lines {
		if strings.Contains(line, needle) {
			out = append(out, line)
		}
	}
	return out
}

// interruptWithCapturedLog runs one acked interrupt under a capturing logger.
func interruptWithCapturedLog(t *testing.T, originRequestID string) *capturingLogf {
	t.Helper()
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read Interrupt: %v", err)
			return
		}
		iv, ok := m.(*corev1.Interrupt)
		if !ok {
			t.Errorf("expected Interrupt, got %T", m)
			return
		}
		mustWriteMsg(t, conn, &corev1.Ack{
			RequestId:        iv.GetRequestId(),
			InterruptOutcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED,
		})
		_, _ = wire.ReadAny(conn)
	})
	log := &capturingLogf{}
	cfg := h.config(t, "sess-1", path)
	cfg.Logf = log.logf
	c, connected, stop := runConnectedClient(t, cfg)
	defer stop()
	waitConnected(t, connected)

	if _, err := c.Interrupt(context.Background(), originRequestID); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	return log
}

// THE SEND NAMES WHO ORDERED IT. Without this, a search for the frontend's own
// id finds the command arriving and nothing after it.
func TestInterruptSendLogNamesTheOriginRequest(t *testing.T) {
	// Arrange, Act.
	log := interruptWithCapturedLog(t, "fe-276-1074")

	// Assert.
	sent := log.linesContaining("sent control request")
	if len(sent) != 1 {
		t.Fatalf("send lines = %d, want exactly 1", len(sent))
	}
	if !strings.Contains(sent[0], `origin_request_id="fe-276-1074"`) {
		t.Fatalf("send line = %q, want it to name the frontend request that ordered the stop", sent[0])
	}
}

// THE ACK NAMES IT TOO, which is the half that answers "was it delivered". The
// send alone cannot distinguish a stop the shim answered from one it ignored.
func TestInterruptAckLogNamesTheOriginRequest(t *testing.T) {
	// Arrange, Act.
	log := interruptWithCapturedLog(t, "fe-276-1074")

	// Assert.
	acked := log.linesContaining("acked outcome=")
	if len(acked) != 1 {
		t.Fatalf("ack lines = %d, want exactly 1", len(acked))
	}
	if !strings.Contains(acked[0], `origin_request_id="fe-276-1074"`) {
		t.Fatalf("ack line = %q, want it to name the frontend request that ordered the stop", acked[0])
	}
}

// A CALLER WITH NO ID OF ITS OWN IS RENDERED AS ONE, rather than the field
// vanishing: an exchange nothing outside this package named is itself a fact.
func TestInterruptLogRendersAnUnnamedOrigin(t *testing.T) {
	// Arrange, Act.
	log := interruptWithCapturedLog(t, "")

	// Assert.
	sent := log.linesContaining("sent control request")
	if len(sent) != 1 || !strings.Contains(sent[0], `origin_request_id=""`) {
		t.Fatalf("send lines = %q, want one naming an empty origin rather than omitting the field", sent)
	}
}

// --- CancelDetachedAgents ---------------------------------------------------
//
// The stop an Interrupt structurally cannot make: an interrupt ends the turn,
// and detached agents have outlived their turn by definition.

func TestCancelDetachedAgentsRelaysTheShimVerdict(t *testing.T) {
	// Arrange
	h := newHarness()
	delivered := make(chan string, 1)
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read CancelDetachedAgents: %v", err)
			return
		}
		cv, ok := m.(*corev1.CancelDetachedAgents)
		if !ok {
			t.Errorf("expected CancelDetachedAgents, got %T", m)
			return
		}
		delivered <- cv.GetRequestId()
		mustWriteMsg(t, conn, &corev1.Ack{
			RequestId: cv.GetRequestId(),
			DetachedCancelOutcome: &corev1.DetachedCancelOutcome{
				Outcome: &corev1.DetachedCancelOutcome_Cancelled{
					Cancelled: &corev1.DetachedAgentsCancelled{TaskIds: []string{"t-1", "t-2"}},
				},
			},
		})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act
	outcome, err := c.CancelDetachedAgents(context.Background(), "fe-1")

	// Assert: the task ids come back verbatim — they are what the daemon
	// settles bubbles by.
	if err != nil {
		t.Fatalf("CancelDetachedAgents: %v", err)
	}
	if reqID := <-delivered; reqID == "" {
		t.Fatal("shim should have received a CancelDetachedAgents with a request id")
	}
	if got := outcome.GetCancelled().GetTaskIds(); len(got) != 2 || got[0] != "t-1" || got[1] != "t-2" {
		t.Fatalf("task ids = %v, want [t-1 t-2]", got)
	}
}

func TestCancelDetachedAgentsRefusesAnAckWithNoOutcome(t *testing.T) {
	// Arrange — the shim acks without the field the contract sets on every arm.
	h := newHarness()
	path := startFakeShim(t, func(conn net.Conn) {
		_ = fakeServerHandshake(t, conn, "sess-1", "1", false)
		m, err := wire.ReadAny(conn)
		if err != nil {
			t.Errorf("read CancelDetachedAgents: %v", err)
			return
		}
		cv, ok := m.(*corev1.CancelDetachedAgents)
		if !ok {
			t.Errorf("expected CancelDetachedAgents, got %T", m)
			return
		}
		mustWriteMsg(t, conn, &corev1.Ack{RequestId: cv.GetRequestId()})
		_, _ = wire.ReadAny(conn)
	})
	c, connected, stop := runConnectedClient(t, h.config(t, "sess-1", path))
	defer stop()
	waitConnected(t, connected)

	// Act
	outcome, err := c.CancelDetachedAgents(context.Background(), "fe-1")

	// Assert: a protocol violation, not an empty stop. Reading it as "nothing
	// was running" would settle no bubbles and report a successful cancel for
	// work still in flight.
	if err == nil {
		t.Fatal("an ack with no detached_cancel_outcome must be refused")
	}
	if outcome != nil {
		t.Fatalf("outcome = %+v, want nil", outcome)
	}
}
