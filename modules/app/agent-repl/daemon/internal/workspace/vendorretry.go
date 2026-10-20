package workspace

import (
	"context"
	"fmt"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE VENDOR-ERROR RETRY ASKED FOR NOW (owner ruling, 2026-10-20). The shim
// owns the retry of a turn the vendor failed and states its schedule on the
// session stream; RET in an empty composer asks the workspace's shim for that
// retry at once (shim.v1.RetryVendorErrorNow). Nothing is sent as content: the
// retry is the shim's own continue prompt. Whether one was due is the shim's
// judgment, answered whole.

const opRetryVendorError = "daemon.workspace.retry_vendor_error"

// RetryVendorError asks the workspace's shim for its scheduled vendor-error
// retry now and answers the shim's judgment.
func (v *verbs) RetryVendorError(ctx context.Context, ws ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error) {
	_, log, err := v.owned(ctx, "RetryVendorError", ws)
	if err != nil {
		return nil, err
	}
	return v.deps.Sessions.RetryVendorError(ctx, log, ws)
}

// RetryVendorError asks the workspace's shim for its scheduled retry now. A
// workspace with no live shim is the no_session refusal; an answer naming no
// outcome is a shim defect, refused loudly.
func (f *Fleet) RetryVendorError(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error) {
	client, ok := f.Client(ws)
	if !ok {
		log.Info(opRetryVendorError, "the workspace has no session to retry on", nil)
		return nil, &ShimRefusal{Verb: "RetryVendorErrorNow", Arm: ArmShimNoSession, Detail: "the workspace has no live session"}
	}
	response, err := client.RetryVendorErrorNow(ctx, &shimv1.RetryVendorErrorNowRequest{})
	if err != nil {
		log.Error(opRetryVendorError, "the shim did not answer the retry asked for now", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("retry vendor error on %q: %w", ws, err)
	}
	outcome := retryOutcomeName(response)
	if outcome == "" {
		log.Error(opRetryVendorError, "the shim answered the retry asked for now with no outcome", dlog.Context{
			"invariant_violation": "RetryVendorErrorNowResponse always names its outcome",
		})
		return nil, fmt.Errorf("retry vendor error on %q: the shim answered no outcome", ws)
	}
	log.Info(opRetryVendorError, "the shim answered the retry asked for now", dlog.Context{
		"outcome": outcome, "detail": retryOutcomeDetail(response),
	})
	return response, nil
}

// retryOutcomeName names the shim's answer for the records, empty when it
// names none.
func retryOutcomeName(response *shimv1.RetryVendorErrorNowResponse) string {
	switch response.GetOutcome().(type) {
	case *shimv1.RetryVendorErrorNowResponse_Retried:
		return "retried"
	case *shimv1.RetryVendorErrorNowResponse_NothingScheduled:
		return "nothing_scheduled"
	case *shimv1.RetryVendorErrorNowResponse_Busy:
		return "busy"
	case *shimv1.RetryVendorErrorNowResponse_Ended:
		return "ended"
	default:
		return ""
	}
}

// retryOutcomeDetail is the shim's own words on a busy or ended answer.
func retryOutcomeDetail(response *shimv1.RetryVendorErrorNowResponse) string {
	if busy := response.GetBusy(); busy != nil {
		return busy.GetDetail()
	}
	return response.GetEnded().GetDetail()
}
