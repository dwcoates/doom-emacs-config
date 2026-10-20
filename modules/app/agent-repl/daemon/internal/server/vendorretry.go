package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// RetryVendorError asks the workspace's shim for its scheduled vendor-error
// retry now (RET in an empty composer, owner ruling 2026-10-20) and answers
// the shim's judgment arm for arm. The new schedule reaches the footer on the
// session stream, never in this answer.
func (s *server) RetryVendorError(
	ctx context.Context,
	req *connect.Request[agentreplv1.RetryVendorErrorRequest],
) (*connect.Response[agentreplv1.RetryVendorErrorResponse], error) {
	const rpc = "RetryVendorError"
	if err := validateRetryVendorErrorRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RetryVendorErrorResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	answered, err := s.deps.Verbs.RetryVendorError(ctx, subject.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	success, err := retryVendorErrorSuccess(answered)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	resp.Result = &agentreplv1.RetryVendorErrorResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// retryVendorErrorSuccess carries the shim's answer onto the rpc's own arms,
// one for one. An answer naming no outcome is a shim defect the workspace
// layer already refuses, so reaching here with one is an error.
func retryVendorErrorSuccess(answered *shimv1.RetryVendorErrorNowResponse) (*agentreplv1.RetryVendorErrorSuccess, error) {
	switch outcome := answered.GetOutcome().(type) {
	case *shimv1.RetryVendorErrorNowResponse_Retried:
		return &agentreplv1.RetryVendorErrorSuccess{Outcome: &agentreplv1.RetryVendorErrorSuccess_Retried{
			Retried: &agentreplv1.RetryVendorErrorRetried{}}}, nil
	case *shimv1.RetryVendorErrorNowResponse_NothingScheduled:
		return &agentreplv1.RetryVendorErrorSuccess{Outcome: &agentreplv1.RetryVendorErrorSuccess_NothingScheduled{
			NothingScheduled: &agentreplv1.RetryVendorErrorNothingScheduled{}}}, nil
	case *shimv1.RetryVendorErrorNowResponse_Busy:
		return &agentreplv1.RetryVendorErrorSuccess{Outcome: &agentreplv1.RetryVendorErrorSuccess_Busy{
			Busy: &agentreplv1.RetryVendorErrorBusy{Detail: outcome.Busy.GetDetail()}}}, nil
	case *shimv1.RetryVendorErrorNowResponse_Ended:
		return &agentreplv1.RetryVendorErrorSuccess{Outcome: &agentreplv1.RetryVendorErrorSuccess_Ended{
			Ended: &agentreplv1.RetryVendorErrorEnded{Detail: outcome.Ended.GetDetail()}}}, nil
	default:
		return nil, fmt.Errorf("the shim answered RetryVendorErrorNow with no outcome (%T)", outcome)
	}
}
