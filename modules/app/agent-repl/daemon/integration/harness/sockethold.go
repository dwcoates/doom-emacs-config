package harness

import (
	"os"
	"testing"

	"agentrepl/logging/sockethold"
	"agentrepl/logging/sockethold/holdtest"
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
// A refused request fails the test: every daemon asks for "daemon".
func StartSocketHolder(t testing.TB) *SocketHolder {
	t.Helper()
	file, address := holdtest.TCP(t)
	control := holdtest.Start(t, map[string]*os.File{"daemon": file}, func(g sockethold.Grant) {
		if g.Err != nil {
			t.Errorf("harness: the holder refused %q: %v", g.Name, g.Err)
		}
	})
	return &SocketHolder{Control: control, Address: address}
}
