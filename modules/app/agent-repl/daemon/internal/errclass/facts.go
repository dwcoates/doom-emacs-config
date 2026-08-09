package errclass

// facts.go carries a classified failure's TYPED EVIDENCE onto the kind arm the
// classification chose.
//
// The vocabulary bridge (kind.go) mints the arm and nothing else: it answers
// "which failure is this", which is a question about the type alone. What the
// arm's own fields hold — the request that went unacknowledged, the status the
// vendor returned, how many attempts were made, which vendor conversation it
// happened in — is a question about the OCCURRENCE, and only the construction
// site that observed it can answer that. This file is the seam between the two.
//
// IT DOES NOT ADD A SECOND CONSTRUCTION SITE. Every card in this package is
// still minted by exactly one function (Card), the arm is still chosen by
// exactly one table (kindFor), and exactly one arm is still set. populate only
// fills fields INSIDE the arm kindFor already chose, and it can neither pick a
// different arm nor set a second one — which is why the exactly-one property
// survives it structurally rather than by review.
//
// THE DETAIL IS NEVER DROPPED. FailureCardView.detail keeps carrying the raw
// account verbatim exactly as it did before any arm had typed fields. The typed
// fields are what a renderer can act on; the detail is what a human reads when
// the typed fields turn out not to have been enough, and losing it to
// "structured now" would be the one irreversible half of this change.

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Facts is the evidence a construction site holds about ONE failure.
//
// It is one struct rather than a per-kind parameter list because a caller
// supplies what it has and nothing else: a field left zero means "this site did
// not observe that", which is exactly what the arms' own field comments already
// define a zero to mean (0 http_status is "the request never got one", 0
// attempts is "it was not retried"). A caller therefore never has to know which
// arm its type maps to in order to report what it saw.
type Facts struct {
	// RequestID is the daemon-side request the failure answers, for the arms
	// that name one.
	RequestID string
	// Reason is the refusing party's own stated reason, verbatim.
	Reason string
	// Cause is the underlying failure, verbatim, for the arms that carry a
	// cause rather than a reason. The two are kept apart because they are
	// different claims: a reason is a decision someone made, a cause is a
	// failure that happened.
	Cause string
	// Component names the part of the connection or the store the failure was
	// observed in.
	Component string
	// WaitedMs is how long the daemon waited before giving up, milliseconds.
	WaitedMs int64
	// DroppedCount is how much was lost while a window-shaped failure stood.
	DroppedCount int64
	// Vendor is the vendor conversation and request the failure pertains to.
	Vendor *frontendv1.VendorFailureContext
	// HTTPStatus is the status the vendor returned; 0 when the request never
	// got one.
	HTTPStatus int32
	// Attempts is how many attempts were made before giving up; 0 means the
	// request was not retried.
	Attempts int32
	// Model is the model a model-not-found failure asked for.
	Model string
	// StopReason is the vendor's own stop reason, verbatim, for the turn-failed
	// arm — which is exactly the case where it did not map to a named kind.
	StopReason string
	// UncachedInputTokens is what a cold compaction cost.
	UncachedInputTokens int64
	// RawReason is a persisted account this build could not classify, carried
	// so the fact is not lost just because it was not understood.
	RawReason string
	// SinceMs is when a hibernation began, unix millis.
	SinceMs int64
}

// CardWithFacts is Card plus the occurrence's typed evidence.
//
// Card remains the whole-package construction site and this delegates to it, so
// the panic on an unclassifiable type, the prose lookup and the open lifecycle
// are all still decided in exactly one place.
func CardWithFacts(t Type, detail string, f Facts) *frontendv1.FailureCardView {
	card := Card(t, detail)
	populate(card.GetKind(), f)
	return card
}

// populate fills the typed fields of whichever arm kindFor already set.
//
// The switch is over the ARM, not over the type, deliberately: the arm is the
// thing being filled, so a kind that grows a field is a compile-visible gap
// here rather than a silently unfilled message. An arm this build has nothing
// to fill is simply absent from the switch — it carries no typed fields, or
// carries only fields no daemon construction site observes — and its account
// still rides `detail` verbatim.
func populate(kind *frontendv1.FailureKind, f Facts) {
	switch arm := kind.GetKind().(type) {
	case *frontendv1.FailureKind_ShimRejected:
		arm.ShimRejected.RequestId = f.RequestID
		arm.ShimRejected.Reason = f.Reason
	case *frontendv1.FailureKind_ShimAckTimeout:
		arm.ShimAckTimeout.RequestId = f.RequestID
		arm.ShimAckTimeout.WaitedMs = f.WaitedMs
	case *frontendv1.FailureKind_ShimDegraded:
		arm.ShimDegraded.Component = f.Component
	case *frontendv1.FailureKind_ShimStoreWriteRejected:
		arm.ShimStoreWriteRejected.Component = f.Component
		arm.ShimStoreWriteRejected.Reason = f.Reason
		arm.ShimStoreWriteRejected.DroppedCount = f.DroppedCount
	case *frontendv1.FailureKind_ShimHandshakeIncomplete:
		arm.ShimHandshakeIncomplete.RequestId = f.RequestID
		arm.ShimHandshakeIncomplete.Cause = f.Cause
	case *frontendv1.FailureKind_ShimUnhealthy:
		arm.ShimUnhealthy.RequestId = f.RequestID
		arm.ShimUnhealthy.Component = f.Component
		arm.ShimUnhealthy.Reason = f.Reason
	case *frontendv1.FailureKind_SessionNotEstablished:
		arm.SessionNotEstablished.Cause = f.Cause
	case *frontendv1.FailureKind_SessionStartFailed:
		arm.SessionStartFailed.Cause = f.Cause
	case *frontendv1.FailureKind_SessionEndedUnclassified:
		arm.SessionEndedUnclassified.RawReason = f.RawReason
	case *frontendv1.FailureKind_SessionHibernated:
		arm.SessionHibernated.SinceMs = f.SinceMs
	case *frontendv1.FailureKind_KeepAliveWindowUnclosed:
		arm.KeepAliveWindowUnclosed.Reason = f.Reason
	case *frontendv1.FailureKind_KeepAliveWindowInverted:
		arm.KeepAliveWindowInverted.Reason = f.Reason
	case *frontendv1.FailureKind_CompactionColdRead:
		arm.CompactionColdRead.UncachedInputTokens = f.UncachedInputTokens
	case *frontendv1.FailureKind_InternalUnclassified:
		arm.InternalUnclassified.Cause = f.Cause

	// The VENDOR side. Every arm carries the conversation and request the
	// failure pertains to; most also carry the status and the attempt count,
	// and the three that do not carry a fact of their own instead.
	case *frontendv1.FailureKind_ApiAuthenticationFailed:
		arm.ApiAuthenticationFailed.Vendor = f.Vendor
		arm.ApiAuthenticationFailed.HttpStatus = f.HTTPStatus
		arm.ApiAuthenticationFailed.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiBillingError:
		arm.ApiBillingError.Vendor = f.Vendor
		arm.ApiBillingError.HttpStatus = f.HTTPStatus
		arm.ApiBillingError.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiRateLimit:
		arm.ApiRateLimit.Vendor = f.Vendor
		arm.ApiRateLimit.HttpStatus = f.HTTPStatus
		arm.ApiRateLimit.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiInvalidRequest:
		arm.ApiInvalidRequest.Vendor = f.Vendor
		arm.ApiInvalidRequest.HttpStatus = f.HTTPStatus
		arm.ApiInvalidRequest.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiServerError:
		arm.ApiServerError.Vendor = f.Vendor
		arm.ApiServerError.HttpStatus = f.HTTPStatus
		arm.ApiServerError.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiOverloaded:
		arm.ApiOverloaded.Vendor = f.Vendor
		arm.ApiOverloaded.HttpStatus = f.HTTPStatus
		arm.ApiOverloaded.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiOauthOrgNotAllowed:
		arm.ApiOauthOrgNotAllowed.Vendor = f.Vendor
		arm.ApiOauthOrgNotAllowed.HttpStatus = f.HTTPStatus
		arm.ApiOauthOrgNotAllowed.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiRequestFailed:
		arm.ApiRequestFailed.Vendor = f.Vendor
		arm.ApiRequestFailed.HttpStatus = f.HTTPStatus
		arm.ApiRequestFailed.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiUnknown:
		arm.ApiUnknown.Vendor = f.Vendor
		arm.ApiUnknown.HttpStatus = f.HTTPStatus
		arm.ApiUnknown.Attempts = f.Attempts
	case *frontendv1.FailureKind_ApiModelNotFound:
		arm.ApiModelNotFound.Vendor = f.Vendor
		arm.ApiModelNotFound.Model = f.Model
	case *frontendv1.FailureKind_ApiNetworkDown:
		arm.ApiNetworkDown.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiMaxOutputTokens:
		arm.ApiMaxOutputTokens.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiMaxTurns:
		arm.ApiMaxTurns.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiMaxBudget:
		arm.ApiMaxBudget.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiExecutionError:
		arm.ApiExecutionError.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiRefusal:
		arm.ApiRefusal.Vendor = f.Vendor
	case *frontendv1.FailureKind_ApiTurnFailed:
		arm.ApiTurnFailed.Vendor = f.Vendor
		arm.ApiTurnFailed.StopReason = f.StopReason
	}
}

// vendorContext builds the vendor conversation/request context, or nil when
// the site observed none of it. A context of three empty strings is not
// evidence, and carrying one would claim the failure named a conversation it
// never did.
func vendorContext(claudeSessionID, apiRequestID, apiMessageID string) *frontendv1.VendorFailureContext {
	if claudeSessionID == "" && apiRequestID == "" && apiMessageID == "" {
		return nil
	}
	return &frontendv1.VendorFailureContext{
		ClaudeSessionId: claudeSessionID,
		ApiRequestId:    apiRequestID,
		ApiMessageId:    apiMessageID,
	}
}
