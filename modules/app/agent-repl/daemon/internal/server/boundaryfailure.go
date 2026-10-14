package server

import (
	"agent-repld/internal/ctxend"

	"connectrpc.com/connect"
)

// boundaryFailure is the ONE answer every rpc's boundary gives when
// beginRequest could not bind the request's scope. The two shared boundaries
// in requestlog_server.go (serveUnary, serveStream) are its only callers.
//
// A request whose own context ended is answered CANCELED: the caller left, and
// calling that an internal failure would tell it the daemon broke.
func boundaryFailure(err error) *connect.Error {
	if ctxend.Is(err) {
		return connect.NewError(connect.CodeCanceled, err)
	}
	return connect.NewError(connect.CodeInternal, err)
}
