package shimclient

import (
	"context"
	"errors"
	"net"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
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
