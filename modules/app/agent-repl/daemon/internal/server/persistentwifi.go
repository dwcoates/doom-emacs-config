package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/publish"
)

// PersistentWifi is the server's view of the persistent-wifi service
// (internal/wifirpc.Client): the change passes through to it, and the
// standing it publishes is pushed on.
type PersistentWifi interface {
	// Update passes one validated action through to the service and answers
	// its response whole. An error is a service the call could not reach.
	Update(ctx context.Context, req *agentreplv1.UpdatePersistentWifiModeRequest) (*agentreplv1.UpdatePersistentWifiModeResponse, error)
	// Topic is the standing, pushed on every Emacs WatchDaemon stream.
	Topic() *publish.Topic[*agentreplv1.PersistentWifiState]
}

// UpdatePersistentWifiMode turns the machine's persistent wifi mode on, off or
// over. The persistent-wifi service owns the change and records its outcome;
// this only validates and passes it through.
func (s *server) UpdatePersistentWifiMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdatePersistentWifiModeRequest],
) (*connect.Response[agentreplv1.UpdatePersistentWifiModeResponse], error) {
	if err := validateUpdatePersistentWifiModeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp, err := s.deps.PersistentWifi.Update(ctx, req.Msg)
	if err != nil {
		return nil, err
	}
	return connect.NewResponse(resp), nil
}
