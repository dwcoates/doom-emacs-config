// Package transportcut answers whether a Connect call's or stream's failure
// was the link to its peer breaking under it, rather than anything the peer
// answered. It is the one judgement every client of a peer that can die
// (a shim, an incumbent daemon) reads a dead peer from.
package transportcut

import (
	"errors"
	"io"

	"connectrpc.com/connect"
)

// Is answers whether err is the link breaking under a call rather than
// anything the peer answered: `unavailable`; a streamed response the peer
// cut partway through an envelope, which connect reports as invalid_argument
// "protocol error: incomplete envelope" wrapping io.ErrUnexpectedEOF; or a
// unary response cut partway through its body, which connect reports as
// unknown "read message" wrapping the same.
//
// AN ANSWER THE PEER SENT CAN NEVER WRAP io.ErrUnexpectedEOF: a refusal that
// crossed the wire is rebuilt on this side from its code and message alone,
// so only this side's own read of a cut stream carries the cause.
// MEASURED, merge gate 2026-10-15:
// TestAShimThatDiesAgainBeforeAnyTurnEndsIsLeftDownUntilTheNextPrompt caught
// `daemon.shimclient.watch_agent` ERROR "shim stream refused {error=
// invalid_argument: protocol error: incomplete envelope: unexpected EOF}" for
// the shim it had SIGKILLed. MEASURED, merge gate 2026-10-16:
// TestASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt caught
// `daemon.shimclient.gather_title_digest` ERROR "shim call failed {error=
// unknown: read message: unexpected EOF}" for a shim dying under the call.
func Is(err error) bool {
	switch connect.CodeOf(err) {
	case connect.CodeUnavailable:
		return true
	case connect.CodeInvalidArgument, connect.CodeUnknown:
		return errors.Is(err, io.ErrUnexpectedEOF)
	}
	return false
}
