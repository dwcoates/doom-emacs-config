// Package ctxend answers the one question every logging path in the daemon
// asks of a failed call before choosing its level: did the call fail because
// its context ENDED -- cancelled (a client that left, the daemon's own orderly
// exit) or past its deadline -- rather than because something broke?
//
// Work abandoned that way is stated at INFO or DEBUG; everything else keeps its
// ERROR. Every such classification goes through Is, so no site can draw the
// line somewhere else (a source scan in the package's tests fails a
// hand-rolled copy).
package ctxend

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agentrepl/logging/logsite"
)

// Is reports whether err is a context ending: context.Canceled or
// context.DeadlineExceeded, however deeply wrapped.
func Is(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}

// Log records a call that answered err through the one split on LOG: at
// CUTLEVEL with the CUT message when err is a context ending, and at error
// with the FAILED message otherwise. The caller picks the cut level and
// stamps the error into fields under its own key. Each message is the
// caller's own (logsite.Msg), so each record is keyed by the caller.
func Log(err error, log dlog.Logger, cutLevel, operation string, cut, failed logsite.Message, fields dlog.Context) {
	if Is(err) {
		log.Log(cutLevel, operation, cut, fields)
		return
	}
	log.Log(dlog.LevelError, operation, failed, fields)
}
