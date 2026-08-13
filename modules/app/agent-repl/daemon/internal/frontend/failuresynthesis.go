// failuresynthesis.go — failure cards the DAEMON composes, from evidence no
// producer could have written as one.
//
// SYNTHESIS, NOT TRANSLATION, which is why this survives translate.go. A
// degraded store link and a query that terminated unexpectedly are things the
// daemon observed about a session; nothing wrote them into a transcript, so
// there is no record to forward and no vendor shape to neutralize. The daemon
// resolves them into the card a user reads and acts on.
//
// That line is the same one conversation.v1 draws for FailureRaised, from the
// other side: what lands THERE is the vendor's own recorded error, durable
// because the vendor made it durable. What lands here is the other kind — a
// session that never started, a store write the shim rejected — which has no
// producer and therefore cannot be an entry at all.
//
// Recovery classification is not restated on either side. `internal/errclass`
// is the one authority on whether anything can be done about a failure, and a
// card reaches a client already resolved by it.

package frontend

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"claude-repld/internal/errclass"

	"google.golang.org/protobuf/proto"
)

// ---------------------------------------------------------------------------
// DegradedState -> FailureCardView
// ---------------------------------------------------------------------------

// FailureCardFromDegradedState classifies a shim-reported DegradedState
// as a conversation card (F4), replacing the DegradedNotice banner (RETIRED,
// step 11).
//
// The window's two edges become ONE card: the opening report leaves
// resolved_at_ms zero and the recovery stamps it, under the same uuid the
// caller keys them by, so the feed reconciles in place and shows a settled
// card instead of a permanent alarm about something that ended.
//
// dropped_count finally survives. The banner discarded it, which meant the
// single most useful fact about a store outage — how much conversation was
// lost — reached no surface at all.
func FailureCardFromDegradedState(ds *protocolv1.DegradedState, atMs int64) *frontendv1.FailureCardView {
	if ds == nil {
		return nil
	}
	var item *frontendv1.FailureCardView
	if ds.GetComponent() == "claude-shim-sdk" && ds.GetReason() == "unexpected_query_termination" {
		item = errclass.UnexpectedQueryTermination(ds.GetComponent(), ds.GetReason())
	} else {
		item = errclass.Degraded(ds.GetComponent(), ds.GetReason(), int64(ds.GetDroppedCount()))
	}
	if ds.GetRecovered() {
		errclass.Resolve(item, atMs)
	}
	return item
}

// FailureCardFromQueryTermination translates the durable typed query
// lifecycle record directly into the dedicated frontend failure detail. The
// generic failure vocabulary remains populated, while every diagnostic field
// retains the lifecycle record's exact identity and typed cause.
func FailureCardFromQueryTermination(sessionID string, lifecycle *protocolv1.QueryLifecycle, observedAtMs int64) (*frontendv1.FailureCardView, error) {
	if lifecycle == nil || lifecycle.GetTerminated() == nil {
		return nil, nil
	}
	terminated := lifecycle.GetTerminated()
	if terminated.GetIntentional() != nil {
		return nil, nil
	}
	if sessionID == "" || lifecycle.GetQueryInstanceId() == "" || observedAtMs <= 0 {
		return nil, fmt.Errorf("typed query termination missing identity session=%q query_instance_id=%q observed_at_ms=%d", sessionID, lifecycle.GetQueryInstanceId(), observedAtMs)
	}
	// sessionID is still REQUIRED above and still names the record in every
	// error below; it simply no longer rides the wire. The contract retired
	// agent_repl_session_id from this evidence because a rendering frontend has
	// no session vocabulary to read it with — what it needs is the VENDOR
	// conversation, which is content, and that is carried unchanged.
	detail := &frontendv1.QueryTerminationFailure{
		QueryInstanceId: lifecycle.GetQueryInstanceId(),
		ObservedAtMs:    observedAtMs,
	}
	switch identity := terminated.GetVendorIdentity().(type) {
	case *protocolv1.QueryTerminated_VendorSessionId:
		if identity.VendorSessionId == "" {
			return nil, fmt.Errorf("typed query termination has blank vendor_session_id session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
		}
		detail.VendorIdentity = &frontendv1.QueryTerminationFailure_VendorSessionId{VendorSessionId: identity.VendorSessionId}
	case *protocolv1.QueryTerminated_VendorSessionIdentityUnavailable:
		if identity.VendorSessionIdentityUnavailable == nil {
			return nil, fmt.Errorf("typed query termination has nil vendor_session_identity_unavailable session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
		}
		detail.VendorIdentity = &frontendv1.QueryTerminationFailure_VendorSessionIdentityUnavailable{VendorSessionIdentityUnavailable: proto.Clone(identity.VendorSessionIdentityUnavailable).(*protocolv1.VendorSessionIdentityUnavailable)}
	default:
		return nil, fmt.Errorf("typed query termination has no vendor identity session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
	}
	switch reason := terminated.GetReason().(type) {
	case *protocolv1.QueryTerminated_UnexpectedEof:
		if reason.UnexpectedEof == nil {
			return nil, fmt.Errorf("typed query termination unexpected_eof reason is nil session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
		}
		detail.Reason = &frontendv1.QueryTerminationFailure_UnexpectedEof{UnexpectedEof: proto.Clone(reason.UnexpectedEof).(*protocolv1.UnexpectedQueryEof)}
	case *protocolv1.QueryTerminated_IteratorFailure:
		if reason.IteratorFailure == nil {
			return nil, fmt.Errorf("typed query termination iterator_failure reason is nil session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
		}
		detail.Reason = &frontendv1.QueryTerminationFailure_IteratorFailure{IteratorFailure: proto.Clone(reason.IteratorFailure).(*protocolv1.QueryIteratorFailure)}
	case *protocolv1.QueryTerminated_StartupFailure:
		if reason.StartupFailure == nil {
			return nil, fmt.Errorf("typed query termination startup_failure reason is nil session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
		}
		detail.Reason = &frontendv1.QueryTerminationFailure_StartupFailure{StartupFailure: proto.Clone(reason.StartupFailure).(*protocolv1.QueryStartupFailure)}
	default:
		return nil, fmt.Errorf("typed query termination has no unexpected reason session=%q query_instance_id=%q", sessionID, lifecycle.GetQueryInstanceId())
	}
	item := errclass.UnexpectedQueryTermination("claude-shim-sdk", "unexpected_query_termination")
	item.GetKind().GetQueryTermination().Detail = detail
	return item, nil
}

// FailureUUID is the card uuid derived from the conversation item a failure
// came from. Deriving it — rather than minting a fresh one — is what keeps the
// card stable across a resync and distinct from the legacy item it accompanies.
func FailureUUID(itemUUID string) string {
	return "failure:" + itemUUID
}
