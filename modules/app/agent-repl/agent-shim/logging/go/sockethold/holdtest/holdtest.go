// Package holdtest stands a socket holder up for a test, in place of the
// launchd job and the sockethold program production runs: it serves the
// descriptors it is given on a control socket until the test ends.
package holdtest

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"agentrepl/logging/sockethold"
)

// Start serves FILES, by name, on a fresh control socket until the test
// ends, and answers the control socket's path. RECORD is told every grant
// (sockethold.Serve), nil when the test reads none. The control socket lives
// under /tmp, so its path stays inside a unix socket's length limit. At the
// test's end the holder stops and closes FILES; a holder that stopped
// serving on its own fails the test.
func Start(t testing.TB, files map[string]*os.File, record func(sockethold.Grant)) string {
	t.Helper()
	if record == nil {
		record = func(sockethold.Grant) {}
	}
	dir, err := os.MkdirTemp("/tmp", "hold")
	if err != nil {
		t.Fatalf("holdtest: the holder's directory: %v", err)
	}
	control := filepath.Join(dir, "control.sock")
	controlLn, err := net.ListenUnix("unix", &net.UnixAddr{Name: control, Net: "unix"})
	if err != nil {
		t.Fatalf("holdtest: the holder's control socket: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := sockethold.Serve(ctx, controlLn, files, record); err != nil {
			t.Errorf("holdtest: the holder stopped serving: %v", err)
		}
	}()
	t.Cleanup(func() {
		cancel()
		wg.Wait()
		for _, f := range files {
			f.Close()
		}
		os.RemoveAll(dir)
	})
	return control
}

// TCP is a loopback listening socket's descriptor, for a holder to keep, and
// its address. The listener itself is closed: the holder keeps the
// descriptor, never a listener that accepts.
func TCP(t testing.TB) (*os.File, string) {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("holdtest: the loopback listener: %v", err)
	}
	f, err := ln.(*net.TCPListener).File()
	if err != nil {
		t.Fatalf("holdtest: the loopback listener's descriptor: %v", err)
	}
	address := ln.Addr().String()
	ln.Close()
	return f, address
}
