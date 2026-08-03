package shimclient

import (
	"context"
	"errors"
	"net"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// fakeDeaths is a hand-driven ShimDeaths: the test decides when the process
// "dies" by closing died, so no wait in these tests depends on a duration.
type fakeDeaths struct {
	died  chan struct{}
	state ShimSpawnState
}

func (f *fakeDeaths) DiedBeforeConnect(string) <-chan struct{} { return f.died }
func (f *fakeDeaths) SpawnState(string) ShimSpawnState         { return f.state }

// deathSource is a ConnSource that also answers for the spawned process, which
// is the shape the daemon's real listener adapter has.
type deathSource struct{ *fakeDeaths }

func (deathSource) Next(ctx context.Context, _ string) (net.Conn, *corev1.ShimHello, error) {
	<-ctx.Done()
	return nil, nil, ctx.Err()
}

type observedExitSource struct {
	exits chan ShimExit
	serve func(net.Conn)
	calls atomic.Int64
}

func (s *observedExitSource) DiedAfterConnect(string) <-chan ShimExit { return s.exits }

func (s *observedExitSource) Next(ctx context.Context, _ string) (net.Conn, *corev1.ShimHello, error) {
	if s.calls.Add(1) > 1 {
		<-ctx.Done()
		return nil, nil, ctx.Err()
	}
	client, peer := net.Pipe()
	go s.serve(peer)
	return client, &corev1.ShimHello{
		SessionId: "s1", Vendor: "claude", ShimVersion: "test-shim", ProtocolVersion: "1",
	}, nil
}

func serveReadyUntilClosed(t *testing.T, conn net.Conn) {
	t.Helper()
	defer conn.Close()
	if _, err := wire.ReadAny(conn); err != nil {
		t.Errorf("read DaemonHello: %v", err)
		return
	}
	mustWriteMsg(t, conn, &corev1.ShimReady{SessionId: "s1"})
	_, _ = wire.ReadAny(conn)
}

func TestAwaitReadyFailsFastWhenTheSpawnedShimExitsBeforeConnecting(t *testing.T) {
	// Arrange: a bring-up waiting on a shim that has not connected, with a
	// deadline far longer than the test would ever wait for.
	deaths := &fakeDeaths{
		died: make(chan struct{}),
		state: ShimSpawnState{
			Spawned:    true,
			StderrTail: "node: cannot find module dist/main.js\n",
			Failure:    errors.New("server: session s1: shimclient: the daemon-spawned shim exited before it ever connected: exit code 1"),
		},
	}
	c := New(Config{SessionID: "s1", ShimDeaths: deaths, Logf: shimclientTestLogf(t)})
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- c.AwaitReady(ctx) }()

	// Act: the process is reaped without ever dialling in.
	close(deaths.died)

	// Assert: the wait ends on the exit, carrying the exit status and stderr.
	select {
	case err := <-done:
		if err == nil {
			t.Fatal("AwaitReady returned nil after the spawned shim died")
		}
		if !strings.Contains(err.Error(), "exit code 1") {
			t.Fatalf("AwaitReady error %v omits the exit status", err)
		}
		if !strings.Contains(err.Error(), "cannot find module") {
			t.Fatalf("AwaitReady error %v omits the shim stderr", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("AwaitReady did not return after the spawned shim died")
	}
}

func TestAwaitReadyReportsADeathWithNoRecordedDetailAsADeath(t *testing.T) {
	// Arrange: the death fires but no cause was recorded — the wait must still
	// end on the death rather than spin on the closed channel.
	deaths := &fakeDeaths{died: make(chan struct{}), state: ShimSpawnState{Spawned: true}}
	close(deaths.died)
	c := New(Config{SessionID: "s1", ShimDeaths: deaths, Logf: shimclientTestLogf(t)})
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()

	// Act
	err := c.AwaitReady(ctx)

	// Assert
	if !errors.Is(err, ErrShimDiedBeforeConnect) {
		t.Fatalf("AwaitReady error = %v, want %v", err, ErrShimDiedBeforeConnect)
	}
}

func TestAwaitReadyDeadlineNamesALiveProcessThatNeverDialled(t *testing.T) {
	// Arrange: the spawned process is alive and simply never connected.
	deaths := &fakeDeaths{
		died: make(chan struct{}),
		state: ShimSpawnState{
			Spawned:    true,
			Alive:      true,
			StderrTail: "waiting for the store socket\n",
		},
	}
	c := New(Config{SessionID: "s1", ShimDeaths: deaths, Logf: shimclientTestLogf(t)})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := c.AwaitReady(ctx)

	// Assert: the timeout says which failure it was and carries the evidence.
	if err == nil {
		t.Fatal("AwaitReady returned nil for an expired context")
	}
	if !strings.Contains(err.Error(), "STILL ALIVE but never dialled in") {
		t.Fatalf("AwaitReady error %v does not say the process is alive and never dialled", err)
	}
	if !strings.Contains(err.Error(), "waiting for the store socket") {
		t.Fatalf("AwaitReady error %v omits the shim stderr tail", err)
	}
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitReady error %v dropped the context cause", err)
	}
}

func TestAwaitReadyDeadlineSaysNothingAboutAProcessThisDaemonNeverSpawned(t *testing.T) {
	// Arrange: a shim that outlived a previous daemon — no local process, so
	// no process claim may be made.
	deaths := &fakeDeaths{died: make(chan struct{})}
	c := New(Config{SessionID: "s1", ShimDeaths: deaths, Logf: shimclientTestLogf(t)})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := c.AwaitReady(ctx)

	// Assert
	if err == nil || strings.Contains(err.Error(), "spawned") {
		t.Fatalf("AwaitReady error = %v, want a bare deadline with no process claim", err)
	}
}

func TestAwaitReadyDeadlineReportsAProcessAlreadyGoneWithNoRecordedFailure(t *testing.T) {
	// Arrange: the process is reaped and no cause was recorded — the timeout
	// must still say the process is gone rather than imply it is running.
	deaths := &fakeDeaths{died: make(chan struct{}), state: ShimSpawnState{Spawned: true}}
	c := New(Config{SessionID: "s1", ShimDeaths: deaths, Logf: shimclientTestLogf(t)})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := c.AwaitReady(ctx)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has exited") {
		t.Fatalf("AwaitReady error = %v, want it to report the exited process", err)
	}
}

func TestNewTakesTheSpawnWatchFromASourceThatCarriesOne(t *testing.T) {
	// Arrange
	deaths := &fakeDeaths{died: make(chan struct{})}

	// Act
	c := New(Config{SessionID: "s1", Source: deathSource{deaths}, Logf: shimclientTestLogf(t)})

	// Assert: the client picked the seam up without it being bound explicitly.
	if c.cfg.ShimDeaths == nil {
		t.Fatal("New did not adopt the ConnSource's spawn watch")
	}
}

func TestRunReturnsTheConnectedShimProcessDeathWithoutReconnect(t *testing.T) {
	// Arrange: one shim reaches readiness. Its process observer is independent
	// of the socket so the test can publish the exact reap evidence first.
	h := newHarness()
	source := &observedExitSource{exits: make(chan ShimExit, 1)}
	source.serve = func(conn net.Conn) { serveReadyUntilClosed(t, conn) }
	cfg := h.config(t, "s1", "")
	cfg.Source = source
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- c.Run(ctx) }()
	readyCtx, readyCancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer readyCancel()
	if err := c.AwaitReady(readyCtx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}

	// Act: process reap closes the transport in production. Publishing the
	// evidence before cancelling the peer makes the causal ownership explicit.
	source.exits <- ShimExit{Description: "exit status 17", ExitCode: 17, StderrTail: "SDK stream ended unexpectedly\n"}
	c.mu.Lock()
	c.active.conn.Close()
	c.mu.Unlock()
	err := <-done

	// Assert: a dead process is terminal, not another ConnSource.Next call.
	if !errors.Is(err, ErrShimDiedAfterConnect) {
		t.Fatalf("Run error = %v, want %v", err, ErrShimDiedAfterConnect)
	}
	if !strings.Contains(err.Error(), "exit status 17") || !strings.Contains(err.Error(), "SDK stream ended unexpectedly") {
		t.Fatalf("Run error %v omitted process evidence", err)
	}
	if calls := source.calls.Load(); calls != 1 {
		t.Fatalf("ConnSource.Next calls = %d, want 1 after terminal process death", calls)
	}
}

func TestIntentionalControllerCancellationWinsOverTheProcessExit(t *testing.T) {
	// Arrange: a connected shim whose controller owns the shutdown.
	h := newHarness()
	source := &observedExitSource{exits: make(chan ShimExit, 1)}
	source.serve = func(conn net.Conn) { serveReadyUntilClosed(t, conn) }
	cfg := h.config(t, "s1", "")
	cfg.Source = source
	c := New(cfg)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- c.Run(ctx) }()
	readyCtx, readyCancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer readyCancel()
	if err := c.AwaitReady(readyCtx); err != nil {
		t.Fatalf("AwaitReady: %v", err)
	}

	// Act: hibernation cancels the controller before SIGTERM reaps the shim.
	cancel()
	if err := <-done; err != nil {
		t.Fatalf("Run after intentional cancellation = %v, want clean completion", err)
	}
	source.exits <- ShimExit{Description: "terminated", ExitCode: 0}

	// Assert: the planned stop did not become a reconnect or a fatal death.
	if calls := source.calls.Load(); calls != 1 {
		t.Fatalf("ConnSource.Next calls = %d, want 1 after intentional shutdown", calls)
	}
}
