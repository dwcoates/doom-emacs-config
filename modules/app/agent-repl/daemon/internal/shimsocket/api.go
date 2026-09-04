// Package shimsocket PROBES the workspace socket path a shim listens on.
//
// The workspace kernel lock says whether a conversation is OWNED. It does not
// say whether the owner is REACHABLE, and the two are separate kernel facts
// that a boot has to agree on before it decides to spawn:
//
//   - a shim that survived a daemon crash is still BOUND to
//     <state>/sock/<workspace>.sock, and a daemon that reads the lock as free
//     and spawns anyway puts a second process on a path the first one still
//     owns: the newcomer's bind fails, it dies, and the daemon then dials the
//     path and reaches the SURVIVOR, which refuses StartSession with
//     `already_started`. The turn is lost to a shim nobody adopted.
//   - a dead shim leaves its socket FILE behind — an AF_UNIX path is not
//     reclaimed on process death the way a flock is — so a spawn onto a stale
//     path fails to bind for a reason that no longer exists.
//
// So the socket is probed as well, and the answer is one of four states, never
// a guess. A LIVE listener is a survivor that must be ADOPTED rather than
// spawned over; a STALE path is unlinked before the spawn; an ABSENT path is
// the ordinary spawn; and an UNDETERMINED path is a loud refusal, never a
// spawn.
package shimsocket

import (
	"errors"
	"fmt"
	"net"
	"os"
	"strings"
	"time"

	"claude-repld/internal/dlog"
)

// DialTimeout is how long a liveness probe waits for a connect. It is a
// CONNECT to a local AF_UNIX path, which either completes or is refused by the
// kernel immediately; the bound exists only so a pathological listener with a
// full backlog cannot hang a boot.
const DialTimeout = 2 * time.Second

// State is what a probe could determine about a socket path.
type State int

// The probe states. "Could not tell" is never read as absent.
const (
	// StateUndetermined means the probe failed for a reason other than the
	// path being absent or refused — a permission error, a path that exists
	// and is not a socket. Never read as free to spawn onto.
	StateUndetermined State = iota
	// StateAbsent means no file exists at the path: nothing to adopt, nothing
	// to unlink, and a spawn may bind it.
	StateAbsent
	// StateStale means a socket FILE exists whose listener is gone: the dial
	// was refused. It must be unlinked before a spawn can bind the path.
	StateStale
	// StateLive means a listener accepted a connection on this path: a shim
	// survives here and must be ADOPTED, never spawned over.
	StateLive
)

// String names the state for a log record.
func (s State) String() string {
	switch s {
	case StateAbsent:
		return "absent"
	case StateStale:
		return "stale"
	case StateLive:
		return "live"
	default:
		return "undetermined"
	}
}

// Probe answers whether a shim is listening on path.
//
// A missing path is StateAbsent. A path that exists but is not a socket is
// StateUndetermined WITH the error — it is not ours to unlink. A path that
// dials is StateLive (the probe connection is closed immediately; the shim's
// server treats a connect-and-close as any other short-lived peer). A dial
// refused by the kernel (ECONNREFUSED, or ENOENT racing an unlink) is
// StateStale: the file outlived its listener.
func Probe(path string) (State, error) {
	if strings.TrimSpace(path) == "" {
		return StateUndetermined, errors.New("shimsocket: socket path is empty")
	}
	info, err := os.Lstat(path)
	switch {
	case errors.Is(err, os.ErrNotExist):
		return StateAbsent, nil
	case err != nil:
		return StateUndetermined, fmt.Errorf("shimsocket: stat %q: %w", path, err)
	case info.Mode()&os.ModeSocket == 0:
		return StateUndetermined, fmt.Errorf("shimsocket: %q exists and is not a socket (mode %s)", path, info.Mode())
	}

	conn, err := net.DialTimeout("unix", path, DialTimeout)
	if err == nil {
		_ = conn.Close()
		return StateLive, nil
	}
	if errors.Is(err, syscallECONNREFUSED) || errors.Is(err, os.ErrNotExist) {
		return StateStale, nil
	}
	return StateUndetermined, fmt.Errorf("shimsocket: dial %q: %w", path, err)
}

// ProbeWithLog is Probe with the canonical record on every branch. Probe
// itself stays a pure primitive so a caller that already logs does not log
// twice.
func ProbeWithLog(log dlog.Logger, path string) (State, error) {
	state, err := Probe(path)
	if log == nil {
		return state, err
	}
	ctx := dlog.Context{"socket_path": path, "state": state.String()}
	if err != nil {
		ctx["error"] = err.Error()
		log.Error("daemon.shimsocket.probe", "the shim socket probe could not tell", ctx)
		return state, err
	}
	log.Debug("daemon.shimsocket.probe", "shim socket probed", ctx)
	return state, nil
}

// ClearStale unlinks a socket path that has no listener, so a spawn can bind
// it. It RE-PROBES rather than trusting a state the caller carried in: between
// the caller's probe and this call a shim may have bound the path, and
// unlinking a live shim's socket would strand the process the lock exists to
// protect.
//
// It is an error to clear anything but a stale path, and the error names what
// was found: an absent path needs no clearing, and a live one must be adopted.
func ClearStale(log dlog.Logger, path string) error {
	state, err := Probe(path)
	if err != nil {
		return fmt.Errorf("shimsocket: clear %q: %w", path, err)
	}
	switch state {
	case StateAbsent:
		return nil
	case StateStale:
		if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("shimsocket: unlink the stale socket %q: %w", path, err)
		}
		if log != nil {
			// INFO, not WARN. A dead shim always leaves its socket file
			// behind — an AF_UNIX path is not reclaimed on process death —
			// so sweeping one is the ordinary course of a bring-up after a
			// crash, not a fault. The record still lands, because a spawn
			// that had to clear a path is worth reading in a diagnosis.
			log.Info("daemon.shimsocket.clear", "unlinked a socket path whose listener is gone",
				dlog.Context{"socket_path": path})
		}
		return nil
	default:
		return fmt.Errorf("shimsocket: refusing to clear %q: it is %s", path, state)
	}
}
