// Package daemonaddr owns the daemon's one loopback listener claim and the
// daemon.addr file that advertises it.
//
// The file is written atomically once the listener is bound and removed on
// orderly exit; a joining successor writes it only once it owns every
// workspace. See ARCHITECTURE.md "State root layout" and docs/overhaul/
// daemon.md "CROSS-SYSTEM PROCESS CONTRACTS".
//
// The daemon.addr payload is the bare "host:port" address on the first line
// and an optional "pid=<n>" line naming the advertising daemon's process id.
// A reader that needs only the address takes the first line, which is exactly
// what a legacy daemon wrote; ReadAdvertisement returns both, reporting
// PIDKnown false for a legacy file that names no pid. The pid lets a reader
// tell a dead advertiser (retire it silently) from a live-but-unreachable one
// (a real transport fault) without dialing.
package daemonaddr

import (
	"net"
	"time"
)

// Claim is a bound loopback listener together with the advertisement file it
// may publish.
type Claim interface {
	// Listener is the bound loopback listener the server serves on.
	Listener() net.Listener
	// Address is "127.0.0.1:<port>" with the actually-bound port.
	Address() string
	// Publish writes the advertisement to the daemon.addr path atomically
	// (write a temporary file, then rename). The payload is the bare address
	// on the first line and "pid=<n>" -- this daemon's process id -- on the
	// second; see ReadAdvertisement for the format and its legacy fallback. A
	// joining daemon calls it only once it owns every workspace.
	Publish() error
	// Withdraw removes the daemon.addr file WHILE IT STILL NAMES THIS CLAIM'S
	// ADDRESS, and answers whether it removed anything. Orderly exit calls it;
	// it is idempotent, removing an absent file is success, and a successor's
	// advertisement is left alone.
	Withdraw() (bool, error)
	// Close closes the listener. It does not withdraw the advertisement.
	Close() error
}

// Bind claims a loopback listener and prepares the advertisement at addrPath.
// port 0 asks the kernel for a free port, which is what Address then reports.
// Nothing is published until Publish is called.
// Binding is preceded by an exclusive kernel lock on LockPath(addrPath),
// which is the actual boot-exclusivity claim: a port-0 bind hands every
// racing daemon a different free port and arbitrates nothing. A second daemon
// loses there, before it has bound or written anything, and Bind returns
// ErrClaimed.
//
// A HELD CLAIM IS WAITED ON FOR ClaimWaitBound BEFORE IT IS BELIEVED. An
// outgoing daemon holds the claim until its process ends, and it withdraws
// daemon.addr at the START of its shutdown, so a replacement is spawned into
// a window where the address is already gone and the claim is not yet free.
// Exiting on the first refusal there destroyed the daemon instead of
// replacing it. Only a claim still held after the bound is a live incumbent.
func Bind(addrPath string, port int) (Claim, error) {
	return BindWithin(addrPath, port, ClaimWaitBound)
}

// BindWithin is Bind with the claim wait named explicitly, so a caller that
// must not pay the production bound -- a test, or a boot whose caller already
// knows the incumbent is gone -- can say so. A wait of zero or less refuses a
// held claim on sight, which is the pre-2026-09-12 behavior and is correct
// only where nothing can be departing.
func BindWithin(addrPath string, port int, wait time.Duration) (Claim, error) {
	return bind(addrPath, port, wait)
}

// BindJoining binds a SUCCESSOR's listener WITHOUT the boot claim. The
// incumbent holds that claim for as long as it serves, so a successor racing
// for it would lose to its own predecessor and exit; it takes the claim at
// Publish, when it takes over.
func BindJoining(addrPath string, port int) (Claim, error) {
	return bindJoining(addrPath, port)
}
