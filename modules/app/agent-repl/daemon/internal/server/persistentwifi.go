package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/publish"
)

// PersistentWifi is the server's view of the persistent-wifi controller
// (internal/persistentwifi): the change, and the standing's publication.
type PersistentWifi interface {
	// Update runs one validated action and answers its response whole.
	Update(ctx context.Context, req *agentreplv1.UpdatePersistentWifiModeRequest) *agentreplv1.UpdatePersistentWifiModeResponse
	// Topic is the standing, pushed on every Emacs WatchDaemon stream.
	Topic() *publish.Topic[*agentreplv1.PersistentWifiState]
}

// UpdatePersistentWifiMode turns the machine's persistent wifi mode on, off or
// over. The controller owns the change and records its outcome; this only
// validates and delegates.
func (s *server) UpdatePersistentWifiMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdatePersistentWifiModeRequest],
) (*connect.Response[agentreplv1.UpdatePersistentWifiModeResponse], error) {
	if err := validateUpdatePersistentWifiModeRequest(req.Msg); err != nil {
		return nil, err
	}
	return connect.NewResponse(s.deps.PersistentWifi.Update(ctx, req.Msg)), nil
}
