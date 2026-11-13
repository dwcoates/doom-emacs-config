package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// opSummarize is the operation every record of a summary asked before a start
// is filed under.
const opSummarize = "daemon.workspace.summarize_before_start"

// summarizeBeforeStart asks CLIENT for every summary REQ's start needs, BEFORE
// the start, each under its own bound (DefaultSummarizeBound): the cold gate's
// compaction of the resumed conversation when the resume's remediation is
// `compact`, and the summary of another conversation the binding owes
// (CARRIED.SummaryOf). It answers the carried summary's text, "" when none was
// owed.
//
// THE START NEVER SUMMARIZES. A summarizer's time grows with the conversation
// it reads, and a 596,848-token summary outran the 60s start bound it once
// shared with the opening (2026-11-13), so the summaries are their own calls
// and the StartSession that follows is bounded by the opening alone.
//
// A failure answers the same labeled error a start's own vendor failure does,
// marked as the compaction (compactionFailure), so the vendor-start run's
// retry and fault rules apply unchanged; every retry of the start asks for
// its summaries again.
func (f *Fleet) summarizeBeforeStart(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, req *shimv1.StartSessionRequest, carried *wsm.OwedStartWork) (string, error) {
	resume := req.GetResume()
	if compact := resume.GetColdRemediation().GetCompact(); compact != nil {
		asked := &shimv1.SummarizeConversationRequest{
			WorkspaceName: req.GetWorkspaceName(),
			Profile:       req.GetProfile(),
			Summarizer:    compact,
			Use: &shimv1.SummarizeConversationRequest_Compact{Compact: &shimv1.SummarizeConversationCompact{
				VendorSessionId: resume.GetVendorSessionId(),
				RolledBackTurns: resume.GetRolledBackTurns(),
			}},
		}
		if _, err := f.summarize(ctx, log, ws, client, asked); err != nil {
			return "", err
		}
	}
	if carried == nil || carried.SummaryOf == "" {
		return "", nil
	}
	asked := &shimv1.SummarizeConversationRequest{
		WorkspaceName: req.GetWorkspaceName(),
		Profile:       req.GetProfile(),
		Summarizer:    defaultSummarizer(),
		Use: &shimv1.SummarizeConversationRequest_Carry{Carry: &shimv1.SummarizeConversationCarry{
			VendorSessionId: carried.SummaryOf,
		}},
	}
	success, err := f.summarize(ctx, log, ws, client, asked)
	if err != nil {
		return "", err
	}
	text := success.GetCarried().GetText()
	if text == "" {
		err := fmt.Errorf("summarize %q for workspace %q: the shim answered a carry with no summary text", carried.SummaryOf, ws)
		log.Error(opSummarize, "the shim answered the carried summary without one; the session was not started",
			dlog.WithCause(dlog.Context{"invariant_violation": "a carry's success is `carried` with its text, never empty", "summary_of": carried.SummaryOf}, err))
		return "", err
	}
	return text, nil
}

// summarize sends ONE SummarizeConversation, bounded by the fleet's
// summarizeBound, and answers its success or the labeled error its failure
// is.
func (f *Fleet) summarize(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, req *shimv1.SummarizeConversationRequest) (*shimv1.SummarizeConversationSuccess, error) {
	fields := summarizeFields(req)
	log.Info(opSummarize, "asking the shim for the summary before the start", fields)
	asked := f.now()
	summarizeCtx, end := context.WithTimeout(ctx, f.summarizeBound)
	defer end()
	response, err := client.SummarizeConversation(summarizeCtx, req)
	fields["elapsed_ms"] = f.now().Sub(asked).Milliseconds()
	if err != nil {
		// A FIRED BOUND IS NAMED with its operation, the bound and the time
		// it was sized from, and is never retried: the label is a rejection.
		if errors.Is(err, context.DeadlineExceeded) && ctx.Err() == nil {
			fields["bound_ms"] = f.summarizeBound.Milliseconds()
			fields["expected_ms"] = summarizeExpected.Milliseconds()
			fields["shim_pid"] = client.PID()
			log.Error(opSummarize, "the shim's summarizer did not answer inside its bound; the start is refused and not retried",
				dlog.WithCause(fields, err))
			detail := fmt.Sprintf("the summary of %q did not complete within %s (expected at most %s)",
				fields["vendor_session_id"], f.summarizeBound, summarizeExpected)
			refusal := refuseWith(log, "OpenWorkspace", ArmVendorStartFailed,
				fmt.Sprintf("the vendor failed to start for workspace %q: %s", ws, detail), false,
				map[string]any{"detail": detail})
			return nil, markCompaction(labeled(fmt.Errorf("%w: %w", refusal, err), false, true, detail), true)
		}
		if errors.Is(err, shimclient.ErrStandDownOrdered) {
			log.Info(opSummarize, "the SummarizeConversation call ended in a stand-down this daemon ordered", dlog.WithCause(fields, err))
			return nil, fmt.Errorf("summarize before the start for %q: %w", ws, err)
		}
		log.Error(opSummarize, "the SummarizeConversation call failed", dlog.WithCause(fields, err))
		return nil, fmt.Errorf("summarize before the start for %q: %w", ws, err)
	}
	failure := response.GetFailure()
	if failure == nil {
		success := response.GetSuccess()
		if success == nil {
			err := fmt.Errorf("summarize before the start for %q: the shim answered no result", ws)
			log.Error(opSummarize, "the shim's SummarizeConversation answer sets no result", dlog.WithCause(dlog.Context{
				"invariant_violation": "SummarizeConversationResponse.result is always set"}, err))
			return nil, err
		}
		fields["tokens_before"] = success.GetCompacted().GetTokensBefore()
		fields["tokens_after"] = success.GetCompacted().GetTokensAfter()
		log.Info(opSummarize, "the shim wrote the summary the start needs", fields)
		return success, nil
	}
	fields["detail"] = failure.GetDetail()
	switch cause := failure.GetCause().(type) {
	case *shimv1.SummarizeConversationFailure_SummarizerFailed:
		log.Error(opSummarize, "the shim's summarizer did not complete; the start is refused", fields)
		return nil, vendorStartLabel(log, ws, cause.SummarizerFailed, failure.GetDetail(), true)
	case *shimv1.SummarizeConversationFailure_UnknownSession:
		log.Error(opSummarize, "the conversation to summarize has no transcript; the start is refused", fields)
		return nil, markCompaction(labeled(refuse(log, "OpenWorkspace", ArmUnknownSession,
			fmt.Sprintf("the shim has no transcript for conversation %q: %s", fields["vendor_session_id"], failure.GetDetail()), false),
			false, false, failure.GetDetail()), true)
	case *shimv1.SummarizeConversationFailure_AlreadyStarted:
		err := fmt.Errorf("summarize before the start for %q: the shim refused already_started: %s", ws, failure.GetDetail())
		log.Error(opSummarize, "the shim refused the summary already_started; this daemon asked a started shim", dlog.WithCause(dlog.Context{
			"invariant_violation": "a summary is asked of a shim before its session starts", "detail": failure.GetDetail()}, err))
		return nil, err
	default:
		err := fmt.Errorf("summarize before the start for %q: the shim's refusal names no cause: %s", ws, failure.GetDetail())
		log.Error(opSummarize, "the shim's SummarizeConversation refusal sets no cause", dlog.WithCause(dlog.Context{
			"invariant_violation": "SummarizeConversationFailure.cause is always set", "detail": failure.GetDetail()}, err))
		return nil, err
	}
}

// summarizeFields is the record's context for REQ: which use, which
// conversation, and the summarizer the request named.
func summarizeFields(req *shimv1.SummarizeConversationRequest) dlog.Context {
	fields := dlog.Context{
		"requested_model": req.GetSummarizer().GetModel().GetName(),
		"scope":           conversationv1.SessionCompactScope_name[int32(req.GetSummarizer().GetScope())],
	}
	switch use := req.GetUse().(type) {
	case *shimv1.SummarizeConversationRequest_Compact:
		fields["use"] = "compact"
		fields["vendor_session_id"] = use.Compact.GetVendorSessionId()
	case *shimv1.SummarizeConversationRequest_Carry:
		fields["use"] = "carry"
		fields["vendor_session_id"] = use.Carry.GetVendorSessionId()
	}
	return fields
}
