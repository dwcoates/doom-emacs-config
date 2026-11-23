package server

import (
	"context"
	"errors"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
)

func (s *requestLoggingServer) UpdateProductionMediation(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateProductionMediationRequest],
) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	return serveUnary(s, ctx, "UpdateProductionMediation", req, s.server.UpdateProductionMediation)
}

// UpdateProductionMediation is the production mediator's decisions
// (endpoint_update_production_mediation.proto).
func (s *server) UpdateProductionMediation(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateProductionMediationRequest],
) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errProductionMediationUnbuilt)
}

var errProductionMediationUnbuilt = errors.New("the production mediator is not built yet")
