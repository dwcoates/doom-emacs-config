package main

import (
	"errors"
	"fmt"
	"net"
	"syscall"

	"claude-repld/internal/frontend"
	"claude-repld/internal/shimlisten"
)

// THE BOOT CLAIM IS WHY A DUPLICATE DAEMON CANNOT DESTROY THE INCUMBENT.
//
// The daemon owns two kinds of listening endpoint, and they behave in exactly
// opposite ways when a second daemon boots next to a live one:
//
//   - The TCP listen on -addr is EXCLUSIVE. The kernel arbitrates it: the
//     second bind fails with EADDRINUSE and the loser learns it lost.
//   - The unix sockets (the frontend socket Emacs dials, the shim socket every
//     shim dials) are NOT. Binding one means unlinking whatever file is at the
//     path first, because a leftover socket file from a dead daemon would
//     otherwise refuse the bind forever. That unlink does not ask whether the
//     socket it is deleting belongs to a LIVE daemon.
//
// Boot used to bind the unix sockets first and take the exclusive TCP listen
// last, which made the second daemon maximally destructive: it unlinked and
// rebound the incumbent's frontend and shim sockets, THEN died on EADDRINUSE.
// The incumbent kept serving an unlinked inode nothing could dial any more, so
// every Emacs dial got connection-refused, the link stayed down, Emacs's ensure
// spawned another daemon, and the outage sustained itself. That is a ~15 minute
// total outage produced entirely by boot ORDER.
//
// So the order is inverted and carried by the type system rather than by the
// reading order of main: winning the exclusive claim PRODUCES the value, and
// the unix binds are methods on it. A daemon that never won the claim holds no
// bootClaim and therefore has nothing to call the unlink-and-bind paths on.
type bootClaim struct{ http net.Listener }

// errIncumbentDaemon marks the one loss that is not a malfunction: another
// claude-repld is already up and serving on this address. The loser exits
// having touched nothing of the incumbent's.
var errIncumbentDaemon = errors.New("claude-repld: another daemon already holds the daemon address")

// errNoBootClaim is the refusal a unix bind gives when it is reached without
// the exclusive claim in hand. It cannot happen through main, which is the
// point: the compiler will not produce a bootClaim for a daemon that lost.
var errNoBootClaim = errors.New("claude-repld: refusing to bind a shared unix socket without the exclusive boot claim")

// claimBootExclusivity takes the daemon's exclusive listen on addr. This is the
// FIRST thing boot does that touches shared state, ahead of every socket bind,
// so the arbitration happens before anything is destroyed.
//
// A live incumbent is reported as errIncumbentDaemon; any other bind failure
// (a bad address, a denied privileged port) keeps its own cause, because the
// two call for different operator responses.
func claimBootExclusivity(addr string) (*bootClaim, error) {
	l, err := net.Listen("tcp", addr)
	if err != nil {
		if errors.Is(err, syscall.EADDRINUSE) {
			return nil, fmt.Errorf("%w (%s): %v", errIncumbentDaemon, addr, err)
		}
		return nil, fmt.Errorf("claude-repld: claim daemon address %s: %w", addr, err)
	}
	return &bootClaim{http: l}, nil
}

// HTTPListener is the claimed listener, handed to the HTTP server once the rest
// of boot is wired. The claim is taken early and served late deliberately:
// connections that arrive in between queue in the kernel's accept backlog,
// which is strictly better than the connection-refused they used to get.
func (c *bootClaim) HTTPListener() net.Listener {
	if c == nil {
		return nil
	}
	return c.http
}

// ListenFrontendUDS binds the frontend socket Emacs dials, unlinking a stale
// socket file first. Stale cleanup is unchanged and still required — a daemon
// that died without unlinking leaves a file that would refuse every future bind
// — and it is safe precisely because reaching here means no live daemon holds
// the exclusive claim, so any leftover belongs to a dead one.
func (c *bootClaim) ListenFrontendUDS(path string) (net.Listener, error) {
	if c == nil || c.http == nil {
		return nil, errNoBootClaim
	}
	return frontend.ListenUDS(path)
}

// ListenShim binds the shim socket every session shim dials, under the same
// claim discipline as the frontend socket and for the same reason: its bind
// unlinks whatever is at the path.
func (c *bootClaim) ListenShim(l *shimlisten.Server, path string) error {
	if c == nil || c.http == nil {
		return errNoBootClaim
	}
	return l.Listen(path)
}
