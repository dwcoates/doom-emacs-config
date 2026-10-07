package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/classifierupdate"
	"claude-repld/internal/dlog"
)

// ClassifierPrompt is the server's view of the routing brief's updater
// (internal/classifierupdate).
type ClassifierPrompt interface {
	// Update rewrites the routing brief to carry req and commits it. A
	// *classifierupdate.Refusal is the contract's answer; any other error is a
	// failure outside it.
	Update(ctx context.Context, req classifierupdate.Request) (classifierupdate.Result, error)
}

// UpdateClassifierPrompt rewrites the routing classifier's brief to carry the
// user's change and commits it. The brief is daemon-global, so the request
// names no workspace and the records go to the daemon's own log.
func (s *server) UpdateClassifierPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateClassifierPromptRequest],
) (*connect.Response[agentreplv1.UpdateClassifierPromptResponse], error) {
	const rpc = "UpdateClassifierPrompt"
	if err := validateUpdateClassifierPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateClassifierPromptResponse{}
	example := req.Msg.GetExample()
	result, err := s.deps.ClassifierPrompt.Update(ctx, classifierupdate.Request{
		Instruction: req.Msg.GetInstruction(),
		Example: classifierupdate.Example{
			Text:  example.GetText(),
			Route: routeOf(example.GetRoute()),
		},
	})
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.update_classifier_prompt", "the routing classifier's brief was rewritten and committed",
		dlog.Context{"commit": result.Commit, "path": result.Path})
	resp.Result = &agentreplv1.UpdateClassifierPromptResponse_Success{
		Success: &agentreplv1.UpdateClassifierPromptSuccess{Commit: result.Commit, Path: result.Path},
	}
	return connect.NewResponse(resp), nil
}

// routeOf reads a wire verdict as the classifier's own route. The validator
// has already refused UNSPECIFIED, so any other value here is a contract the
// server has not been taught, and it panics rather than guess one.
func routeOf(route agentreplv1.ClassifierRoute) classifier.Route {
	switch route {
	case agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_INTERRUPT:
		return classifier.RouteInterrupt
	case agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_AFTER_TOOL_CALL:
		return classifier.RouteAfterToolCall
	case agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_HOLD_FOR_TURN_END:
		return classifier.RouteQueue
	default:
		panic(fmt.Sprintf("server: classifier route %v has no classifier.Route", route))
	}
}
