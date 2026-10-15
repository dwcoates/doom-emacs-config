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
)

// Is reports whether err is a context ending: context.Canceled or
// context.DeadlineExceeded, however deeply wrapped.
func Is(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}

// Log records a call that answered err through the one split: through cut,
// with the cut message, when err is a context ending, and through failed,
// with the failed message, otherwise. The caller picks the cut level (the
// recorder it passes) and stamps the error into fields under its own key.
func Log(err error, cut, failed func(operation, message string, ctx dlog.Context), operation, cutMessage, failedMessage string, fields dlog.Context) {
	if Is(err) {
		cut(operation, cutMessage, fields)
		return
	}
	failed(operation, failedMessage, fields)
}
