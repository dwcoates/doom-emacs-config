package shimclient

import (
	"context"
	"errors"
	"net"
	"strings"
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
	err := c.SubmitPrompt(context.Background(), "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if err != nil {
		t.Fatalf("SubmitPrompt: want nil, got %v", err)
	}
}

func TestSubmitPromptRejectsInvalidOriginBeforeControlSend(t *testing.T) {
	for _, origin := range []corev1.PromptOrigin{
		corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
		corev1.PromptOrigin(999),
	} {
		t.Run(origin.String(), func(t *testing.T) {
			c := &Client{}
			err := c.SubmitPrompt(context.Background(), "hello", "human", "", origin)
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
	err := c.SubmitPrompt(context.Background(), "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

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
	err := c.SubmitPrompt(context.Background(), "hello", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

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
	outcome, err := c.Interrupt(context.Background())

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

func TestSubmitPromptNotConnected(t *testing.T) {
	// Arrange: a client that never connects (dials into the void via a path
	// that is never served).
	h := newHarness()
	c := New(h.config(t, "sess-1", "/nonexistent/agent-shim/session.sock"))

	// Act: no Run goroutine, so there is no live connection.
	err := c.SubmitPrompt(context.Background(), "hi", "human", "", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

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
