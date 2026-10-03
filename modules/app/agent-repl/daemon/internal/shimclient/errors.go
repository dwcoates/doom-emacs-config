package shimclient

import (
	"errors"
	"fmt"

	"connectrpc.com/connect"
)

// ErrNoProcess is returned by Kill on a client that supervises no child
// process of its own — an ADOPTED shim, whose pid the lock holder did not
// yield. The caller stops it through the shim's own KillSession verb instead.
var ErrNoProcess = errors.New("shimclient: no supervised process")

// ErrStandingDown refuses a spawn on a supervisor that has already begun
// standing its processes down. It is the LATCH the immediate shutdown's sweep
// sets, and it exists because a sweep alone is not enough: a bring-up that had
// not yet reached cmd.Start when the sweep took its snapshot would start a
// shim the sweep had already walked past, and this daemon is the only thing
// that would ever know that process existed. The prompt behind such a spawn is
// not lost -- it is durably HELD before the bring-up begins, and the next
// daemon restores it -- so the refusal costs a restart, never work.
var ErrStandingDown = errors.New("shimclient: the supervisor is standing down; no new shim may be spawned")

// ErrDetached is returned by a supervision verb on a client that has already
// handed its process over. A detached client supervises nothing.
var ErrDetached = errors.New("shimclient: client is detached")

// InvalidRequestError is a base-function validation failure. It NAMES the
// field, so the refusal a caller relays says which one was wrong.
type InvalidRequestError struct {
	// Message is the proto message whose validation failed.
	Message string
	// Field is the offending field's path, e.g. "StartTurnRequest.turn.value".
	Field string
	// Reason states what was wrong with it.
	Reason string
}

func (e *InvalidRequestError) Error() string {
	return fmt.Sprintf("shimclient: invalid %s: %s: %s", e.Message, e.Field, e.Reason)
}

// invalid builds an InvalidRequestError for one field.
func invalid(message, field, reason string) error {
	return &InvalidRequestError{Message: message, Field: field, Reason: reason}
}

// OccupiedError refuses a second Occupy, NAMING the current holder so the
// refusal a caller surfaces says who has the guard.
type OccupiedError struct {
	// Holder is the occupant that already has the guard.
	Holder string
	// Requested is the holder that was refused.
	Requested string
}

func (e *OccupiedError) Error() string {
	return fmt.Sprintf("shimclient: occupied by %q; %q refused", e.Holder, e.Requested)
}

// StreamOpenError is a REFUSED stream open. A Connect error on a Watch open is
// this error from the Watch call — never a stream that fails on first Recv.
type StreamOpenError struct {
	// Procedure is the rpc whose stream could not be opened.
	Procedure string
	// Err is the transport's or the shim's refusal.
	Err error
}

func (e *StreamOpenError) Error() string {
	return fmt.Sprintf("shimclient: %s stream refused: %v", e.Procedure, e.Err)
}

// Unwrap yields the refusal, so a caller can classify it.
func (e *StreamOpenError) Unwrap() error { return e.Err }

// BringUpDeathError ends a bring-up because the process DIED. It carries the
// exit decoding and the stderr ring, which is why bring-up never needs a
// timeout to explain itself.
type BringUpDeathError struct {
	// Exit is the decoded exit plus the stderr evidence.
	Exit ExitInfo
}

func (e *BringUpDeathError) Error() string {
	return fmt.Sprintf("shimclient: shim died during bring-up: pid=%d code=%d signal=%q stderr=%q",
		e.Exit.PID, e.Exit.Code, e.Exit.Signal, e.Exit.Stderr)
}

// Unwrap names a death the supervisor's own stand-down sweep caused as
// ErrStandingDown: a spawn the sweep killed mid bring-up is the same refusal a
// spawn asked for after the sweep gets, and its caller tells the daemon's
// departure from a shim that genuinely would not come up by that.
func (e *BringUpDeathError) Unwrap() error {
	if e.Exit.Attribution != nil && e.Exit.Attribution.Actor == ActorStandDown {
		return ErrStandingDown
	}
	return nil
}

// SpecError refuses a spawn whose Spec is incomplete. The spawn contract has
// no optional parts.
type SpecError struct {
	// Field is the Spec field that was missing or unusable.
	Field string
	// Reason states what was wrong with it.
	Reason string
}

func (e *SpecError) Error() string {
	return fmt.Sprintf("shimclient: invalid Spec: %s: %s", e.Field, e.Reason)
}

// errProcessDead ends a wait because the supervised process is gone. It is
// internal: callers learn of death through Exited and Connectivity.
var errProcessDead = errors.New("shimclient: process is dead")

// Detail renders a shim call failure's own words, without the transport's
// framing. A Connect error's Error() prefixes its code ("internal: ..."), and
// the code is the transport's business: a refusal relayed to a client carries
// what the SHIM said, so the caller reads the shim's account and not ours.
func Detail(err error) string {
	if err == nil {
		return ""
	}
	var cerr *connect.Error
	if errors.As(err, &cerr) {
		return cerr.Message()
	}
	return err.Error()
}
