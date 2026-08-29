// Package daemonaddr owns the daemon's one loopback listener claim and the
// daemon.addr file that advertises it.
//
// The file is written atomically once the listener is bound and removed on
// orderly exit; a joining successor writes it only once it owns every
// workspace. See ARCHITECTURE.md "State root layout" and docs/overhaul/
// daemon.md "CROSS-SYSTEM PROCESS CONTRACTS".
package daemonaddr

import (
	"net"

	"claude-repld/internal/notimpl"
)

// Claim is a bound loopback listener together with the advertisement file it
// may publish.
type Claim interface {
	// Listener is the bound loopback listener the server serves on.
	Listener() net.Listener
	// Address is "127.0.0.1:<port>" with the actually-bound port.
	Address() string
	// Publish writes Address to the daemon.addr path atomically (write a
	// temporary file, then rename). A joining daemon calls it only once it
	// owns every workspace.
	Publish() error
	// Withdraw removes the daemon.addr file. Orderly exit calls it; it is
	// idempotent, and removing an absent file is success.
	Withdraw() error
	// Close closes the listener. It does not withdraw the advertisement.
	Close() error
}

// Bind claims a loopback listener and prepares the advertisement at addrPath.
// port 0 asks the kernel for a free port, which is what Address then reports.
// Nothing is published until Publish is called.
func Bind(addrPath string, port int) (Claim, error) {
	return nil, notimpl.Err
}

// Read reads an existing daemon.addr file, which is how a joining successor
// learns the incumbent's address when it is not given one. A missing file is
// reported as an error, never as an empty address.
func Read(addrPath string) (string, error) {
	return "", notimpl.Err
}
