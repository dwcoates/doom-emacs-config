package shimclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"claude-repld/internal/dlog"
)

// fakeServerHandshakeWithPid is fakeServerHandshake plus the one field this
// file turns on: the shim's own pid, which is the generation identity the
// daemon compares connections by.
func fakeServerHandshakeWithPid(t *testing.T, conn net.Conn, sessionID, protoVer string, pid int32) *corev1.DaemonHello {
	t.Helper()
	mustWriteMsg(t, conn, &corev1.ShimHello{
		SessionId:       sessionID,
		Vendor:          "claude",
		ShimVersion:     "test-shim",
		ProtocolVersion: protoVer,
		Pid:             pid,
	})
	m, err := wire.ReadAny(conn)
	if err != nil {
		t.Fatalf("shim reading DaemonHello: %v", err)
	}
	dh, ok := m.(*corev1.DaemonHello)
	if !ok {
		t.Fatalf("shim expected DaemonHello, got %T", m)
	}
	mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: sessionID, FromSeq: dh.GetFromSeq()})
	return dh
}

// recordingLogf captures the client's log lines so a test can assert on the
// record a reset leaves behind.
type recordingLogf struct {
	mu    sync.Mutex
	lines []string
}

func (r *recordingLogf) logf(t *testing.T) dlog.Logf {
	t.Helper()
	inner := shimclientTestLogf(t)
	return func(format string, args ...any) {
		inner(format, args...)
		r.mu.Lock()
		defer r.mu.Unlock()
		r.lines = append(r.lines, strings.TrimSpace(fmt.Sprintf(format, args...)))
	}
}

func (r *recordingLogf) find(substr string) (string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, line := range r.lines {
		if strings.Contains(line, substr) {
			return line, true
		}
	}
	return "", false
}

func TestSeqRegressionWithinOneShimGenerationStaysFatal(t *testing.T) {
	// Arrange: the mark was advanced by generation pid=1001, and the SAME
	// generation is on the wire now.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	c.connGeneration = "pid=1001"
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 5)); err != nil {
		t.Fatalf("first event should be accepted: %v", err)
	}
	<-h.state.ch

	// Act
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 3))

	// Assert
	if !errors.Is(err, ErrSeqRegression) {
		t.Fatalf("dispatchEvent err = %v, want ErrSeqRegression", err)
	}
}

func TestSeqRegressionWithUnidentifiableGenerationStaysFatal(t *testing.T) {
	// Arrange: a connection whose hello carried no pid cannot prove a
	// generation change, so its lower seq must stay fatal.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	c.lastSeen = 4203
	c.seqGeneration = "pid=1001"
	c.connGeneration = ""

	// Act
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 90))

	// Assert
	if !errors.Is(err, ErrSeqRegression) {
		t.Fatalf("dispatchEvent err = %v, want ErrSeqRegression", err)
	}
}

func TestLowerSeqAfterShimGenerationChangeIsAccepted(t *testing.T) {
	// Arrange: the durable mark (4203) was earned by a retired shim
	// generation; the live connection belongs to a new one.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	c.lastSeen = 4203
	c.seqGeneration = "pid=1001"
	c.connGeneration = "pid=2002"

	// Act
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 90))

	// Assert
	if err != nil {
		t.Fatalf("dispatchEvent err = %v, want the new generation's seq accepted", err)
	}
	<-h.state.ch
	if c.lastSeen != 90 {
		t.Fatalf("client lastSeen = %d, want 90 rebased onto the new seq space", c.lastSeen)
	}
	if got := h.seq.LastSeq("sess-1"); got != 90 {
		t.Fatalf("durable last_seq = %d, want 90", got)
	}
}

func TestShimGenerationChangeRebasesTheMarkAgainWithinTheNewGeneration(t *testing.T) {
	// Arrange: one rebase has already happened, so the mark now belongs to the
	// new generation.
	h := newHarness()
	c := New(h.config(t, "sess-1", "/unused.sock"))
	c.lastSeen = 4203
	c.seqGeneration = "pid=1001"
	c.connGeneration = "pid=2002"
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 90)); err != nil {
		t.Fatalf("rebasing event should be accepted: %v", err)
	}
	<-h.state.ch

	// Act: a second regression, this time inside the adopted generation.
	err := c.dispatchEvent(persistentTurnEnd("sess-1", 12))

	// Assert
	if !errors.Is(err, ErrSeqRegression) {
		t.Fatalf("dispatchEvent err = %v, want ErrSeqRegression inside the adopted generation", err)
	}
}

func TestShimGenerationResetIsLogged(t *testing.T) {
	// Arrange
	h := newHarness()
	rec := &recordingLogf{}
	cfg := h.config(t, "sess-1", "/unused.sock")
	cfg.Logf = rec.logf(t)
	c := New(cfg)
	c.lastSeen = 4203
	c.seqGeneration = "pid=1001"
	c.connGeneration = "pid=2002"

	// Act
	if err := c.dispatchEvent(persistentTurnEnd("sess-1", 90)); err != nil {
		t.Fatalf("dispatchEvent err = %v, want the new generation's seq accepted", err)
	}
	<-h.state.ch

	// Assert: the record names the retired mark and both generations.
	line, ok := rec.find("SHIM GENERATION CHANGE")
	if !ok {
		t.Fatalf("no SHIM GENERATION CHANGE record; log was %v", rec.lines)
	}
	for _, want := range []string{"retired_last_seen=4203", `previous_generation="pid=1001"`, `new_generation="pid=2002"`, "first_seq=90"} {
		if !strings.Contains(line, want) {
			t.Fatalf("reset record %q is missing %q", line, want)
		}
	}
}

func TestRestartedShimRenumberingFromOneKeepsTheSessionAlive(t *testing.T) {
	// Arrange: the fake shim serves two connections. The first (pid 1001)
	// publishes seq 5 and hangs up; the second (pid 2002) is a restarted shim
	// whose store seq space begins again at 1.
	h := newHarness()
	var conns atomic.Int32
	path := startFakeShim(t, func(conn net.Conn) {
		switch conns.Add(1) {
		case 1:
			fakeServerHandshakeWithPid(t, conn, "sess-1", "1", 1001)
			mustWriteMsg(t, conn, persistentTurnEnd("sess-1", 5))
			// The generation ENDS here: a closed stream delivers the framed
			// event already written before the reader sees EOF, so the client
			// consumes seq 5 and then reconnects.
			conn.Close()
		default:
			fakeServerHandshakeWithPid(t, conn, "sess-1", "1", 2002)
			mustWriteMsg(t, conn, persistentTurnEnd("sess-1", 1))
			_, _ = wire.ReadAny(conn)
		}
	})
	cfg := h.config(t, "sess-1", path)
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	runErr := make(chan error, 1)
	go func() { runErr <- c.Run(ctx) }()

	// Act: consume the first generation's event, then the restarted shim's.
	if got := recvEvent(t, h.state.ch).GetSeq(); got != 5 {
		t.Fatalf("first generation seq = %d, want 5", got)
	}
	if got := recvEvent(t, h.state.ch).GetSeq(); got != 1 {
		t.Fatalf("second generation seq = %d, want 1", got)
	}

	// Assert: Run is still alive (no terminal seq regression) and the mark was
	// rebased onto the new generation's space.
	select {
	case err := <-runErr:
		t.Fatalf("Run returned %v, want the session to survive the shim restart", err)
	default:
	}
	if got := h.seq.LastSeq("sess-1"); got != 1 {
		t.Fatalf("durable last_seq = %d, want 1 after the rebase", got)
	}
	cancel()
	select {
	case err := <-runErr:
		if err != nil {
			t.Fatalf("Run after cancel = %v, want nil", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Run did not return after cancel")
	}
}
