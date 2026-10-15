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
	"context"
	"errors"
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
	// Verify reports whether what this claim OWNS ON DISK is still there: the
	// state root it was bound in (the same directory, not one recreated at
	// the same path), the daemon.lock whose kernel lock it holds (the same
	// inode), and -- while it is published and not withdrawn -- daemon.addr
	// naming this claim's address. A loss wraps ErrVanished and names what
	// vanished; any other error means the question could not be answered.
	Verify() error
	// Depart releases the serving lock (ServingName) and keeps the boot
	// claim: from this call on, a booting daemon reads this one as DEPARTING
	// and waits for its exit, where before it refused at once. The orderly
	// exit calls it first, before daemon.addr is withdrawn and before any
	// answer owed leaves, so every daemon spawned in response to a shutdown
	// finds it departed. It is idempotent.
	Depart() error
	// Close closes the listener and releases the boot claim, serving lock
	// first. It does not withdraw the advertisement.
	Close() error
	// AwaitBootClaim returns once this claim HOLDS the boot claim: at once
	// for a claim that already does, and for a successor the moment the
	// outgoing daemon's exit releases it. The wait is a blocking kernel lock,
	// never a poll, so the takeover follows the exit by nothing but the
	// kernel's wake-up. Every caller shares one wait; a caller whose ctx ends
	// first gets ctx's error while the wait goes on for the others. Publish
	// after it advertises under the held claim.
	AwaitBootClaim(ctx context.Context) error
}

// ErrVanished marks a Verify failure that is a LOSS: something this claim owns
// on disk is gone or was replaced. A daemon that reads it no longer holds an
// exclusive state root -- a second daemon booting on a recreated root takes a
// daemon.lock nobody holds -- and must stand down.
var ErrVanished = errors.New("the daemon's state on disk vanished")

// Bind claims a loopback listener and prepares the advertisement at addrPath.
// port 0 asks the kernel for a free port, which is what Address then reports.
// Nothing is published until Publish is called.
// Binding is preceded by an exclusive kernel lock on LockPath(addrPath),
// which is the actual boot-exclusivity claim: a port-0 bind hands every
// racing daemon a different free port and arbitrates nothing. A second daemon
// loses there, before it has bound or written anything, and Bind returns
// ErrClaimed.
//
// A HELD CLAIM IS REFUSED AT ONCE ONLY WHILE ITS HOLDER SERVES. The holder
// of the boot claim also holds the serving lock (ServingName) until the first
// instant of its shutdown; a held claim beside a held serving lock is a
// daemon serving and staying, and Bind returns ErrClaimed at once. A held
// claim beside a FREE serving lock is an outgoing daemon -- it withdraws
// daemon.addr at the start of its shutdown and holds the claim until its
// process ends, and its replacement is spawned into exactly that window -- so
// Bind waits up to ClaimWaitBound for it. Exiting on the first refusal there
// destroyed the daemon instead of replacing it (2026-09-12).
func Bind(addrPath string, port int) (Claim, error) {
	c, _, err := BindWithin(addrPath, port, ClaimWaitBound, BootOrdinary)
	return c, err
}

// BindWithin is Bind with the claim wait and the boot mode named explicitly,
// answering how the claim was decided (ClaimVerdict) for the boot's record. A
// caller that must not pay the production bound -- a test, or a boot whose
// caller already knows the incumbent is gone -- can say so. A wait of zero or
// less refuses a held claim on sight, which is correct only where nothing can
// be departing. BootReplacing waits without probing the serving lock: its
// incumbent spawned it while still serving, just before its own exit.
func BindWithin(addrPath string, port int, wait time.Duration, mode BootMode) (Claim, ClaimVerdict, error) {
	return bind(addrPath, port, wait, mode)
}

// BindJoining binds a SUCCESSOR's listener WITHOUT the boot claim. The
// incumbent holds that claim for as long as it serves, so a successor racing
// for it would lose to its own predecessor and exit; it takes the claim at
// Publish, when it takes over.
func BindJoining(addrPath string, port int) (Claim, error) {
	return bindJoining(addrPath, port)
}
