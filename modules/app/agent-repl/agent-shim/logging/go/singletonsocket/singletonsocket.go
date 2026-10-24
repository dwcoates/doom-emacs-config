// Package singletonsocket binds the unix socket of a SINGLETON SERVICE — the
// store, zettel — where the socket file is the service's token: a live
// listener on the path means another instance is serving, and must never be
// stolen from; a socket nobody answers on is a corpse a crashed instance left,
// and is reclaimed.
//
// It lives in the logging module because that is the one Go module every
// agent-repl Go service already requires. It writes no log records: each
// failure is an *Error naming the step that failed, and the result says
// whether a stale socket was reclaimed, so the calling service logs every
// outcome through its own canonical logger.
package singletonsocket

import (
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"time"
)

// Step names the stage of binding a failure happened at.
type Step string

// The steps, in the order Listen takes them.
const (
	// StepResolve: the path could not be made absolute.
	StepResolve Step = "resolve"
	// StepPrepareDir: the socket's directory could not be created.
	StepPrepareDir Step = "prepare-dir"
	// StepInspect: the path could not be inspected.
	StepInspect Step = "inspect"
	// StepNotASocket: something other than a socket occupies the path.
	StepNotASocket Step = "not-a-socket"
	// StepOccupied: another instance answers on the socket.
	StepOccupied Step = "occupied"
	// StepCloseProbe: the probe connection to a live socket would not close.
	StepCloseProbe Step = "close-probe"
	// StepRemoveStale: a stale socket could not be removed.
	StepRemoveStale Step = "remove-stale"
	// StepBind: listening on the path failed.
	StepBind Step = "bind"
	// StepRestrict: the socket could not be restricted to its owner.
	StepRestrict Step = "restrict"
)

// Error is a failure of one step.
type Error struct {
	Step Step
	// Path is the absolute socket path (the given path at StepResolve).
	Path string
	// Mode is the occupying file's mode at StepNotASocket.
	Mode os.FileMode
	Err  error
}

func (e *Error) Error() string {
	return fmt.Sprintf("singleton socket %s: %s: %v", e.Path, e.Step, e.Err)
}

func (e *Error) Unwrap() error { return e.Err }

// ErrOccupied is the cause carried by a StepOccupied failure.
var ErrOccupied = errors.New("another instance is already listening on this socket")

// ErrNotASocket is the cause carried by a StepNotASocket failure.
var ErrNotASocket = errors.New("a non-socket occupies the socket path")

// Result describes a successful bind.
type Result struct {
	// Path is the absolute socket path bound.
	Path string
	// Reclaimed is true when a stale socket was removed first.
	Reclaimed bool
}

// Listen binds path for a singleton service: it creates the directory (0700),
// refuses a live listener, reclaims a stale socket, refuses a non-socket, and
// restricts the bound socket to its owner (0600). probe bounds the dial that
// decides whether an existing socket is live.
func Listen(path string, probe time.Duration) (net.Listener, Result, error) {
	absolute, err := filepath.Abs(path)
	if err != nil {
		return nil, Result{}, &Error{Step: StepResolve, Path: path, Err: err}
	}
	if err := os.MkdirAll(filepath.Dir(absolute), 0o700); err != nil {
		return nil, Result{}, &Error{Step: StepPrepareDir, Path: absolute, Err: err}
	}
	result := Result{Path: absolute}
	switch info, statErr := os.Lstat(absolute); {
	case statErr == nil && info.Mode()&os.ModeSocket != 0:
		if occupied, dialErr := net.DialTimeout("unix", absolute, probe); dialErr == nil {
			if closeErr := occupied.Close(); closeErr != nil {
				return nil, Result{}, &Error{Step: StepCloseProbe, Path: absolute, Err: closeErr}
			}
			return nil, Result{}, &Error{Step: StepOccupied, Path: absolute, Err: ErrOccupied}
		}
		if removeErr := os.Remove(absolute); removeErr != nil {
			return nil, Result{}, &Error{Step: StepRemoveStale, Path: absolute, Err: removeErr}
		}
		result.Reclaimed = true
	case statErr == nil:
		return nil, Result{}, &Error{Step: StepNotASocket, Path: absolute, Mode: info.Mode(), Err: ErrNotASocket}
	case !errors.Is(statErr, os.ErrNotExist):
		return nil, Result{}, &Error{Step: StepInspect, Path: absolute, Err: statErr}
	}
	listener, err := net.Listen("unix", absolute)
	if err != nil {
		return nil, Result{}, &Error{Step: StepBind, Path: absolute, Err: err}
	}
	if err := os.Chmod(absolute, 0o600); err != nil {
		cause := &Error{Step: StepRestrict, Path: absolute, Err: err}
		if closeErr := listener.Close(); closeErr != nil {
			return nil, Result{}, errors.Join(cause, fmt.Errorf("closing the abandoned listener on %s: %w", absolute, closeErr))
		}
		return nil, Result{}, cause
	}
	return listener, result, nil
}
