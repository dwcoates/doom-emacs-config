package harness

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"agentrepl/logging/sockethold"
)

// SocketHolder is a socket holder for a test's daemons, standing in for the
// launchd job and the sockethold program production runs: it keeps the
// shared listener open for the whole test, across every daemon, and hands it
// to whichever daemon asks on its control socket.
type SocketHolder struct {
	// Control is the holder's control socket, passed to a daemon as --holder.
	Control string
	// Address is the shared listener's address, which daemon.addr advertises.
	Address string
}

// StartSocketHolder starts a holder of one shared loopback listener, named
// "daemon" as the daemon's production holder names it, until the test ends.
func StartSocketHolder(t testing.TB) *SocketHolder {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "hold")
	if err != nil {
		t.Fatalf("harness: the holder's directory: %v", err)
	}
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("harness: the shared listener: %v", err)
	}
	file, err := shared.(*net.TCPListener).File()
	if err != nil {
		t.Fatalf("harness: the shared listener's descriptor: %v", err)
	}
	address := shared.Addr().String()
	// The holder keeps the descriptor, never a listener that accepts.
	shared.Close()
	control := filepath.Join(dir, "control.sock")
	controlLn, err := net.ListenUnix("unix", &net.UnixAddr{Name: control, Net: "unix"})
	if err != nil {
		t.Fatalf("harness: the holder's control socket: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := sockethold.Serve(ctx, controlLn, map[string]*os.File{"daemon": file}, func(g sockethold.Grant) {
			if g.Err != nil {
				t.Errorf("harness: the holder refused %q: %v", g.Name, g.Err)
			}
		}); err != nil {
			t.Errorf("harness: the holder stopped serving: %v", err)
		}
	}()
	t.Cleanup(func() {
		cancel()
		wg.Wait()
		file.Close()
		os.RemoveAll(dir)
	})
	return &SocketHolder{Control: control, Address: address}
}
