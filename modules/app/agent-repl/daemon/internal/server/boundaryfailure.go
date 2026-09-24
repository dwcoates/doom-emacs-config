package server

import "connectrpc.com/connect"

// boundaryFailure is the ONE answer every generated rpc wrapper gives when
// beginRequest could not bind the request's scope. It is spelled once so the
// fifty-nine wrappers in requestlog_server.go cannot drift apart on it.
func boundaryFailure(err error) *connect.Error {
	return connect.NewError(connect.CodeInternal, err)
}
