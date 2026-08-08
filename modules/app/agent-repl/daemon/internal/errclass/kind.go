// Code in this file is the bridge between the daemon's failure vocabulary and
// the wire's. `Type` names a failure in one closed string vocabulary the whole
// daemon shares; `FailureKind` names the SAME failure as a closed oneof, which
// is what the frontend contract carries. The two tables below are each other's
// inverse, and the round-trip test in errclass_test.go is what keeps them so.
//
// A kind arm's own typed fields are left at their zero values here. Everything
// the classifier holds about a failure rides `FailureCardView.detail`
// verbatim, exactly as it rode `source_detail` before; resolving the typed
// fields is per-kind work that belongs with the call sites that hold the facts,
// not with the vocabulary bridge.

package errclass

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// kindFor builds the wire's failure kind for a daemon failure type. It returns
// nil for a type outside the vocabulary, which callers treat as a defect rather
// than substituting a neighbouring kind.
func kindFor(t Type) *frontendv1.FailureKind {
	switch t {
	case TypeShimNotConnected:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimNotConnected{ShimNotConnected: &frontendv1.FailureShimNotConnected{}}}
	case TypeShimRejected:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimRejected{ShimRejected: &frontendv1.FailureShimRejected{}}}
	case TypeShimAckTimeout:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimAckTimeout{ShimAckTimeout: &frontendv1.FailureShimAckTimeout{}}}
	case TypeShimVersionMismatch:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimVersionMismatch{ShimVersionMismatch: &frontendv1.FailureShimVersionMismatch{}}}
	case TypeShimSeqRegression:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimSeqRegression{ShimSeqRegression: &frontendv1.FailureShimSeqRegression{}}}
	case TypeShimDegraded:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimDegraded{ShimDegraded: &frontendv1.FailureShimDegraded{}}}
	case TypeShimStoreWriteRejected:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimStoreWriteRejected{ShimStoreWriteRejected: &frontendv1.FailureShimStoreWriteRejected{}}}
	case TypeUnexpectedQueryTermination:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_QueryTermination{QueryTermination: &frontendv1.FailureQueryTermination{}}}
	case TypeShimNotSpawned:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimNotSpawned{ShimNotSpawned: &frontendv1.FailureShimNotSpawned{}}}
	case TypeShimHandshakeIncomplete:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimHandshakeIncomplete{ShimHandshakeIncomplete: &frontendv1.FailureShimHandshakeIncomplete{}}}
	case TypeShimUnhealthy:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ShimUnhealthy{ShimUnhealthy: &frontendv1.FailureShimUnhealthy{}}}
	case TypeSessionNotEstablished:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionNotEstablished{SessionNotEstablished: &frontendv1.FailureSessionNotEstablished{}}}
	case TypeSessionNotLive:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_WorkspaceNotLive{WorkspaceNotLive: &frontendv1.FailureWorkspaceNotLive{}}}
	case TypeSessionDeleted:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionDeleted{SessionDeleted: &frontendv1.FailureSessionDeleted{}}}
	case TypeSessionSuperseded:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionSuperseded{SessionSuperseded: &frontendv1.FailureSessionSuperseded{}}}
	case TypeSessionReconnectSuperseded:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ReconnectSuperseded{ReconnectSuperseded: &frontendv1.FailureReconnectSuperseded{}}}
	case TypeSessionShimDied:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionShimDied{SessionShimDied: &frontendv1.FailureSessionShimDied{}}}
	case TypeSessionStartFailed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionStartFailed{SessionStartFailed: &frontendv1.FailureSessionStartFailed{}}}
	case TypeSessionResumeFailed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionResumeFailed{SessionResumeFailed: &frontendv1.FailureSessionResumeFailed{}}}
	case TypeSessionConversationUnresumable:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ConversationUnresumable{ConversationUnresumable: &frontendv1.FailureConversationUnresumable{}}}
	case TypeResumeModeRetired:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ResumeModeRetired{ResumeModeRetired: &frontendv1.FailureResumeModeRetired{}}}
	case TypeSessionEndedUnclassified:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionEndedUnclassified{SessionEndedUnclassified: &frontendv1.FailureSessionEndedUnclassified{}}}
	case TypeHistoryRepullInFlight:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_HistoryRepullInFlight{HistoryRepullInFlight: &frontendv1.FailureHistoryRepullInFlight{}}}
	case TypeHistoryReplayTruncated:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_HistoryReplayTruncated{HistoryReplayTruncated: &frontendv1.FailureHistoryReplayTruncated{}}}
	case TypeInterruptUndelivered:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_InterruptUndelivered{InterruptUndelivered: &frontendv1.FailureInterruptUndelivered{}}}
	case TypeQueueEntrySessionUnwired:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_QueueEntryUnwired{QueueEntryUnwired: &frontendv1.FailureQueueEntryUnwired{}}}
	case TypeQueueEntryKeepAliveHeld:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_QueueEntryKeepAliveHeld{QueueEntryKeepAliveHeld: &frontendv1.FailureQueueEntryKeepAliveHeld{}}}
	case TypeSessionHibernated:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_SessionHibernated{SessionHibernated: &frontendv1.FailureSessionHibernated{}}}
	case TypeKeepAliveWindowUnclosed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_KeepAliveWindowUnclosed{KeepAliveWindowUnclosed: &frontendv1.FailureKeepAliveWindowUnclosed{}}}
	case TypeKeepAliveWindowInverted:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_KeepAliveWindowInverted{KeepAliveWindowInverted: &frontendv1.FailureKeepAliveWindowInverted{}}}
	case TypeCompactionColdRead:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_CompactionColdRead{CompactionColdRead: &frontendv1.FailureCompactionColdRead{}}}
	case TypeClientLogIdentityStale:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ClientLogIdentityStale{ClientLogIdentityStale: &frontendv1.FailureClientLogIdentityStale{}}}
	case TypeInternalUnclassified:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_InternalUnclassified{InternalUnclassified: &frontendv1.FailureInternalUnclassified{}}}
	case TypeAPIAuthenticationFailed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiAuthenticationFailed{ApiAuthenticationFailed: &frontendv1.FailureApiAuthenticationFailed{}}}
	case TypeAPIBillingError:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiBillingError{ApiBillingError: &frontendv1.FailureApiBillingError{}}}
	case TypeAPIRateLimit:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiRateLimit{ApiRateLimit: &frontendv1.FailureApiRateLimit{}}}
	case TypeAPIInvalidRequest:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiInvalidRequest{ApiInvalidRequest: &frontendv1.FailureApiInvalidRequest{}}}
	case TypeAPIServerError:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiServerError{ApiServerError: &frontendv1.FailureApiServerError{}}}
	case TypeAPIUnknown:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiUnknown{ApiUnknown: &frontendv1.FailureApiUnknown{}}}
	case TypeAPIOAuthOrgNotAllowed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiOauthOrgNotAllowed{ApiOauthOrgNotAllowed: &frontendv1.FailureApiOAuthOrgNotAllowed{}}}
	case TypeAPIOverloaded:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiOverloaded{ApiOverloaded: &frontendv1.FailureApiOverloaded{}}}
	case TypeAPIModelNotFound:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiModelNotFound{ApiModelNotFound: &frontendv1.FailureApiModelNotFound{}}}
	case TypeAPIMaxOutputTokens:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiMaxOutputTokens{ApiMaxOutputTokens: &frontendv1.FailureApiMaxOutputTokens{}}}
	case TypeAPINetworkDown:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiNetworkDown{ApiNetworkDown: &frontendv1.FailureApiNetworkDown{}}}
	case TypeAPIRequestFailed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiRequestFailed{ApiRequestFailed: &frontendv1.FailureApiRequestFailed{}}}
	case TypeAPIMaxTurns:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiMaxTurns{ApiMaxTurns: &frontendv1.FailureApiMaxTurns{}}}
	case TypeAPIMaxBudget:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiMaxBudget{ApiMaxBudget: &frontendv1.FailureApiMaxBudget{}}}
	case TypeAPIExecutionError:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiExecutionError{ApiExecutionError: &frontendv1.FailureApiExecutionError{}}}
	case TypeAPIRefusal:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiRefusal{ApiRefusal: &frontendv1.FailureApiRefusal{}}}
	case TypeAPITurnFailed:
		return &frontendv1.FailureKind{Kind: &frontendv1.FailureKind_ApiTurnFailed{ApiTurnFailed: &frontendv1.FailureApiTurnFailed{}}}
	default:
		return nil
	}
}

// TypeOf recovers the daemon failure type a card's kind arm names. ok=false
// means the arm is unset or belongs to the client-local range, neither of which
// the daemon mints.
func TypeOf(kind *frontendv1.FailureKind) (Type, bool) {
	switch kind.GetKind().(type) {
	case *frontendv1.FailureKind_ShimNotConnected:
		return TypeShimNotConnected, true
	case *frontendv1.FailureKind_ShimRejected:
		return TypeShimRejected, true
	case *frontendv1.FailureKind_ShimAckTimeout:
		return TypeShimAckTimeout, true
	case *frontendv1.FailureKind_ShimVersionMismatch:
		return TypeShimVersionMismatch, true
	case *frontendv1.FailureKind_ShimSeqRegression:
		return TypeShimSeqRegression, true
	case *frontendv1.FailureKind_ShimDegraded:
		return TypeShimDegraded, true
	case *frontendv1.FailureKind_ShimStoreWriteRejected:
		return TypeShimStoreWriteRejected, true
	case *frontendv1.FailureKind_QueryTermination:
		return TypeUnexpectedQueryTermination, true
	case *frontendv1.FailureKind_ShimNotSpawned:
		return TypeShimNotSpawned, true
	case *frontendv1.FailureKind_ShimHandshakeIncomplete:
		return TypeShimHandshakeIncomplete, true
	case *frontendv1.FailureKind_ShimUnhealthy:
		return TypeShimUnhealthy, true
	case *frontendv1.FailureKind_SessionNotEstablished:
		return TypeSessionNotEstablished, true
	case *frontendv1.FailureKind_WorkspaceNotLive:
		return TypeSessionNotLive, true
	case *frontendv1.FailureKind_SessionDeleted:
		return TypeSessionDeleted, true
	case *frontendv1.FailureKind_SessionSuperseded:
		return TypeSessionSuperseded, true
	case *frontendv1.FailureKind_ReconnectSuperseded:
		return TypeSessionReconnectSuperseded, true
	case *frontendv1.FailureKind_SessionShimDied:
		return TypeSessionShimDied, true
	case *frontendv1.FailureKind_SessionStartFailed:
		return TypeSessionStartFailed, true
	case *frontendv1.FailureKind_SessionResumeFailed:
		return TypeSessionResumeFailed, true
	case *frontendv1.FailureKind_ConversationUnresumable:
		return TypeSessionConversationUnresumable, true
	case *frontendv1.FailureKind_ResumeModeRetired:
		return TypeResumeModeRetired, true
	case *frontendv1.FailureKind_SessionEndedUnclassified:
		return TypeSessionEndedUnclassified, true
	case *frontendv1.FailureKind_HistoryRepullInFlight:
		return TypeHistoryRepullInFlight, true
	case *frontendv1.FailureKind_HistoryReplayTruncated:
		return TypeHistoryReplayTruncated, true
	case *frontendv1.FailureKind_InterruptUndelivered:
		return TypeInterruptUndelivered, true
	case *frontendv1.FailureKind_QueueEntryUnwired:
		return TypeQueueEntrySessionUnwired, true
	case *frontendv1.FailureKind_QueueEntryKeepAliveHeld:
		return TypeQueueEntryKeepAliveHeld, true
	case *frontendv1.FailureKind_SessionHibernated:
		return TypeSessionHibernated, true
	case *frontendv1.FailureKind_KeepAliveWindowUnclosed:
		return TypeKeepAliveWindowUnclosed, true
	case *frontendv1.FailureKind_KeepAliveWindowInverted:
		return TypeKeepAliveWindowInverted, true
	case *frontendv1.FailureKind_CompactionColdRead:
		return TypeCompactionColdRead, true
	case *frontendv1.FailureKind_ClientLogIdentityStale:
		return TypeClientLogIdentityStale, true
	case *frontendv1.FailureKind_InternalUnclassified:
		return TypeInternalUnclassified, true
	case *frontendv1.FailureKind_ApiAuthenticationFailed:
		return TypeAPIAuthenticationFailed, true
	case *frontendv1.FailureKind_ApiBillingError:
		return TypeAPIBillingError, true
	case *frontendv1.FailureKind_ApiRateLimit:
		return TypeAPIRateLimit, true
	case *frontendv1.FailureKind_ApiInvalidRequest:
		return TypeAPIInvalidRequest, true
	case *frontendv1.FailureKind_ApiServerError:
		return TypeAPIServerError, true
	case *frontendv1.FailureKind_ApiUnknown:
		return TypeAPIUnknown, true
	case *frontendv1.FailureKind_ApiOauthOrgNotAllowed:
		return TypeAPIOAuthOrgNotAllowed, true
	case *frontendv1.FailureKind_ApiOverloaded:
		return TypeAPIOverloaded, true
	case *frontendv1.FailureKind_ApiModelNotFound:
		return TypeAPIModelNotFound, true
	case *frontendv1.FailureKind_ApiMaxOutputTokens:
		return TypeAPIMaxOutputTokens, true
	case *frontendv1.FailureKind_ApiNetworkDown:
		return TypeAPINetworkDown, true
	case *frontendv1.FailureKind_ApiRequestFailed:
		return TypeAPIRequestFailed, true
	case *frontendv1.FailureKind_ApiMaxTurns:
		return TypeAPIMaxTurns, true
	case *frontendv1.FailureKind_ApiMaxBudget:
		return TypeAPIMaxBudget, true
	case *frontendv1.FailureKind_ApiExecutionError:
		return TypeAPIExecutionError, true
	case *frontendv1.FailureKind_ApiRefusal:
		return TypeAPIRefusal, true
	case *frontendv1.FailureKind_ApiTurnFailed:
		return TypeAPITurnFailed, true
	default:
		return "", false
	}
}

// IsAPI reports whether a failure type belongs to the vendor side of the
// vocabulary. The class left the wire with ErrorClass — each kind arm now
// states its own side — but the daemon still classifies by side internally,
// and this is the one place that judgment is made.
func IsAPI(t Type) bool {
	switch t {
	case TypeAPIAuthenticationFailed, TypeAPIBillingError, TypeAPIRateLimit,
		TypeAPIInvalidRequest, TypeAPIServerError, TypeAPIUnknown,
		TypeAPIOAuthOrgNotAllowed, TypeAPIOverloaded, TypeAPIModelNotFound,
		TypeAPIMaxOutputTokens, TypeAPINetworkDown, TypeAPIRequestFailed,
		TypeAPIMaxTurns, TypeAPIMaxBudget, TypeAPIExecutionError,
		TypeAPIRefusal, TypeAPITurnFailed:
		return true
	default:
		return false
	}
}

// Card builds the frontend's failure card for a classified failure: its kind,
// the one sentence the prose table holds for it, and whatever raw account the
// source gave, verbatim.
//
// A fresh card is OPEN. A window-shaped failure is settled by Resolve when its
// closing edge arrives; an event-shaped one simply never is.
//
// A type outside the vocabulary is a defect in this package, not in the caller,
// so it panics rather than minting a card with no kind — an unkinded card would
// reach a renderer as a failure with nothing to draw and no way to say why.
func Card(t Type, detail string) *frontendv1.FailureCardView {
	kind := kindFor(t)
	if kind == nil {
		panic("errclass: no failure kind for type " + string(t))
	}
	msg, ok := prose[t]
	if !ok {
		panic("errclass: no prose for type " + string(t))
	}
	return &frontendv1.FailureCardView{
		Kind:      kind,
		Message:   msg,
		Detail:    detail,
		Lifecycle: &frontendv1.FailureCardView_Open{Open: &frontendv1.FailureCardOpen{}},
	}
}

// Resolve settles a window-shaped card at the instant its failure stopped being
// true. It replaces the magic zero the old resolved_at_ms carried: a card is
// resolved because it holds the resolved arm, never because a timestamp
// happened to be non-zero.
func Resolve(card *frontendv1.FailureCardView, atMs int64) {
	if card == nil {
		return
	}
	card.Lifecycle = &frontendv1.FailureCardView_Resolved{
		Resolved: &frontendv1.FailureCardResolved{ResolvedAtMs: atMs},
	}
}

// IsResolved reports whether a card has settled.
func IsResolved(card *frontendv1.FailureCardView) bool {
	return card.GetResolved() != nil
}

// ResolvedAtMs is the instant a settled card closed, and 0 for one that has
// not. Callers that need to know WHETHER it settled must ask IsResolved: a
// resolved card whose closing instant is genuinely zero is representable, and
// reading the number as the question is the conflation the arms removed.
func ResolvedAtMs(card *frontendv1.FailureCardView) int64 {
	return card.GetResolved().GetResolvedAtMs()
}

// The two tones a failure resolves to, named exactly as
// proto/vocab/render-colors.json names them. They are the colors the shared
// table already assigned to the two sides of the vocabulary — the class enum
// that used to carry the side left the wire, the assignment did not.
const (
	ToneLocal  = "blue"
	ToneVendor = "purple"
)

// Tone is the color-class name a surface renders a failure in: purple for the
// vendor side, blue for agent-repl's own machinery. It is resolved HERE, once,
// for the same reason the sentence is — a footer row and a card that disagreed
// about a failure's color would be two answers to one question.
func Tone(t Type) string {
	if IsAPI(t) {
		return ToneVendor
	}
	return ToneLocal
}

// FooterRow projects a classified card onto the footer's one-line row: the same
// sentence, its tone, and the address of the card to reveal. cardUUID is empty
// when the failure produced no card, and then the row is not clickable rather
// than scrolling somewhere arbitrary.
//
// It returns nil for a nil card so a caller clears the row by passing what it
// was given, without a second nil test of its own.
func FooterRow(card *frontendv1.FailureCardView, cardUUID string) *frontendv1.FooterFailureRow {
	if card == nil {
		return nil
	}
	t, ok := TypeOf(card.GetKind())
	if !ok {
		// A card the daemon minted always has a daemon kind. One that does not
		// is a defect in this package, and the row says so rather than picking
		// a color for a failure it cannot name.
		panic("errclass: footer row for a card with no daemon-classified kind")
	}
	return &frontendv1.FooterFailureRow{
		Message: card.GetMessage(),
		Tone:    Tone(t),
		Card:    &frontendv1.FailureCardRef{CardUuid: cardUUID},
	}
}
