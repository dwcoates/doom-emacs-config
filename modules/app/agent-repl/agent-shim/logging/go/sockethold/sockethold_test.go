package sockethold

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"
)

// takeBound bounds a Take against a holder already serving in the test, a
// kernel-local exchange; it is the guard against a hang, never a wait.
const takeBound = 5 * time.Second

// holder is a running holder over one data socket, named "data".
type holder struct {
	control string
	data    string
	grants  chan Grant
}

// startHolder binds a data socket and serves it under the name "data".
func startHolder(t *testing.T) *holder {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sh")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	h := &holder{control: filepath.Join(dir, "c.sock"), data: filepath.Join(dir, "d.sock"), grants: make(chan Grant, 8)}
	dataLn, err := net.ListenUnix("unix", &net.UnixAddr{Name: h.data, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	// The holder keeps the path: Go unlinks a socket it bound when the
	// listener closes, so the fixture's listener must not.
	dataLn.SetUnlinkOnClose(false)
	dataFile, err := dataLn.File()
	if err != nil {
		t.Fatal(err)
	}
	dataLn.Close()
	controlLn, err := net.ListenUnix("unix", &net.UnixAddr{Name: h.control, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	var done sync.WaitGroup
	done.Add(1)
	go func() {
		defer done.Done()
		if err := Serve(ctx, controlLn, map[string]*os.File{"data": dataFile}, func(g Grant) { h.grants <- g }); err != nil {
			t.Errorf("Serve: %v", err)
		}
	}()
	t.Cleanup(func() { cancel(); done.Wait(); dataFile.Close() })
	return h
}

func take(t *testing.T, control, name string) (net.Listener, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), takeBound)
	defer cancel()
	return Take(ctx, control, name)
}

// accepted dials the data socket and answers whether LN accepted that connection.
func accepted(t *testing.T, ln net.Listener, data string) {
	t.Helper()
	client, err := net.Dial("unix", data)
	if err != nil {
		t.Fatalf("dial the data socket: %v", err)
	}
	defer client.Close()
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept on the taken listener: %v", err)
	}
	conn.Close()
}

func TestATakenSocketAcceptsTheDataSocketsConnections(t *testing.T) {
	// Arrange
	h := startHolder(t)

	// Act
	ln, err := take(t, h.control, "data")

	// Assert
	if err != nil {
		t.Fatalf("Take: %v", err)
	}
	defer ln.Close()
	accepted(t, ln, h.data)
	if g := <-h.grants; g.Name != "data" || g.Err != nil {
		t.Fatalf("grant = %+v, want data granted", g)
	}
}

func TestTheSocketOutlivesTheServerThatTookIt(t *testing.T) {
	// Arrange: a server takes the socket, then exits.
	h := startHolder(t)
	first, err := take(t, h.control, "data")
	if err != nil {
		t.Fatalf("Take: %v", err)
	}
	first.Close()

	// Act: a client connects while no server accepts, then the next server takes it.
	client, dialErr := net.Dial("unix", h.data)
	second, takeErr := take(t, h.control, "data")

	// Assert: the connect queued, and the next server accepts it.
	if dialErr != nil {
		t.Fatalf("a connect while no server accepts = %v, want it queued", dialErr)
	}
	defer client.Close()
	if takeErr != nil {
		t.Fatalf("second Take: %v", takeErr)
	}
	defer second.Close()
	conn, err := second.Accept()
	if err != nil {
		t.Fatalf("the next server's accept: %v", err)
	}
	conn.Close()
}

func TestTakeIsRefusedASocketTheHolderDoesNotHold(t *testing.T) {
	// Arrange
	h := startHolder(t)

	// Act
	_, err := take(t, h.control, "other")

	// Assert
	if !errors.Is(err, ErrRefused) {
		t.Fatalf("Take = %v, want ErrRefused", err)
	}
	if g := <-h.grants; g.Name != "other" || g.Err == nil {
		t.Fatalf("grant = %+v, want the refusal recorded", g)
	}
}

func TestTakeRefusesAMalformedNameWithoutDialing(t *testing.T) {
	cases := []struct {
		name string
		ask  string
	}{
		{name: "an empty name", ask: ""},
		{name: "a name with a line break", ask: "a\nb"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := take(t, "/nonexistent/control.sock", tc.ask)

			// Assert
			if err == nil || errors.Is(err, ErrRefused) {
				t.Fatalf("Take = %v, want a malformed-name error", err)
			}
		})
	}
}

func TestTakeFailsWhenNoHolderServes(t *testing.T) {
	// Act
	_, err := take(t, "/tmp/sockethold-no-holder.sock", "data")

	// Assert
	if err == nil {
		t.Fatal("Take = nil, want the dial failure")
	}
}
