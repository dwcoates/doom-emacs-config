package server

import "connectrpc.com/connect"

// boundaryFailure is the ONE answer every generated rpc wrapper gives when
// beginRequest could not bind the request's scope. It is spelled once so the
// fifty-nine wrappers in requestlog_server.go cannot drift apart on it.
//
// A request whose own context ended is answered CANCELED: the caller left, and
// calling that an internal failure would tell it the daemon broke.
func boundaryFailure(err error) *connect.Error {
	if endedOnCancel(err) {
		return connect.NewError(connect.CodeCanceled, err)
	}
	return connect.NewError(connect.CodeInternal, err)
}
