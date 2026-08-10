package sessioncontroller

import (
	"errors"
	"fmt"
	"math"
	"sort"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/tokenutilization"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// turnAccountingReducer joins the ordered shim stream into one terminal turn
// record. It has one owner: the session consumer goroutine, so no mutex can
// permit a terminal result to race its end-of-turn evidence.
type turnAccountingReducer struct {
	queryID             string
	queryCreatedSeq     uint64
	queryStoreSessionID string
	runtime             *corev1.QueryRuntimeIdentity
	activeTurnID        string
	turns               map[string]*accountingTurn
	latencies           map[string]responseLatency
	// knownVendorSessionIDs is this conversation's PROVEN vendor session
	// lineage: every vendor session id durable evidence has actually tied to
	// this conversation, via a shim handshake, a query's own observed runtime
	// identity, or a resumed query's requested_vendor_session_id (the prior
	// session it continued FROM). It is what lets a historical response
	// carrying a RETIRED vendor session id be recognized as this
	// conversation's own history rather than rejected as evidence about some
	// other conversation entirely — see isKnownVendorSession.
	knownVendorSessionIDs map[string]struct{}
	// corrections is the session's ONE ledger of response usage corrections:
	// each response's FINAL vendor usage, keyed by API message id, as relayed
	// from the vendor's message_delta frame.
	//
	// It exists because the assistant message a response record is built from
	// carries the `message_start` usage SNAPSHOT: settled input and cache
	// counters, interim output. The final output_tokens are reported only on
	// message_delta, so without this the per-response ledger could never agree
	// with the terminal result's totals — 563 against 4407 on one measured live
	// turn, with input reconciling exactly.
	//
	// IT IS A LEDGER RATHER THAN AN IN-PLACE PATCH because neither arrival order
	// is guaranteed: the SDK emits one assistant message per content block, and
	// whether those land before or after the frame's message_delta is the
	// vendor's business. A correction is recorded here on arrival and applied to
	// its response whichever of the two comes second.
	//
	// IT IS ALSO WHAT THE ACCOUNTING-STAMP ENRICHMENT HOLDS READ, at install
	// time and on every later filing alike, which is what makes a correction
	// that precedes its hold indistinguishable from one that follows it. See
	// accountinghold.go.
	corrections *accountingCorrections
	// logf is the consumer's session-tagged logging channel. It is never nil;
	// newTurnAccountingReducer substitutes a discard when a caller has none.
	logf dlog.Logf
}

type responseLatency struct {
	ttftMs             int64
	messageStartedAtMs int64
}

type accountingTurn struct {
	startedAt         int64
	startUsage        *corev1.AccountUsageObservation
	endUsage          *corev1.AccountUsageObservation
	responses         []*frontendv1.TokenUtilization
	result            *datav1.ResultMessage
	resultAt          int64
	responseConflicts []string
}

// unknownAccountingTurnError rejects usage evidence that cannot be joined to
// an admitted turn. Dropping this event would make the terminal ledger look
// complete while silently omitting one of its account-window boundaries.
type unknownAccountingTurnError struct {
	turnID   string
	queryID  string
	boundary string
}

type malformedAccountUsageObservationError struct {
	turnID string
	reason string
}

func (e *malformedAccountUsageObservationError) Error() string {
	return fmt.Sprintf("account usage observation for turn %q violates its evidence contract: %s", e.turnID, e.reason)
}

// unattributedResponseUsageError rejects billable response evidence before
// the conversation or response ledger can mutate. An admitted root turn must
// own the response; SDK request identifiers remain raw evidence because the
// Agent SDK may omit them or use an API identity unrelated to the daemon turn.
type unattributedResponseUsageError struct {
	queryID      string
	activeTurnID string
	eventRequest string
	apiRequest   string
	apiMessageID string
	reason       string
}

func (e *unattributedResponseUsageError) Error() string {
	return fmt.Sprintf("assistant response usage has no validated root-turn claim (query_instance_id=%q active_turn_id=%q event_request_id=%q api_request_id=%q api_message_id=%q reason=%s)", e.queryID, e.activeTurnID, e.eventRequest, e.apiRequest, e.apiMessageID, e.reason)
}

func (e *unknownAccountingTurnError) Error() string {
	return fmt.Sprintf("account usage observation names unknown turn %q (query_instance_id=%q boundary=%s)", e.turnID, e.queryID, e.boundary)
}

func newTurnAccountingReducer(logf dlog.Logf) *turnAccountingReducer {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &turnAccountingReducer{turns: map[string]*accountingTurn{}, latencies: map[string]responseLatency{}, knownVendorSessionIDs: map[string]struct{}{}, corrections: newAccountingCorrections(logf), logf: logf}
}

// recordKnownVendorSession admits id into the conversation's proven vendor
// session lineage. A blank id is never proof of anything and is silently
// ignored rather than admitted as a wildcard.
func (r *turnAccountingReducer) recordKnownVendorSession(id string) {
	if id == "" {
		return
	}
	r.knownVendorSessionIDs[id] = struct{}{}
}

// isKnownVendorSession reports whether id has been durably proven, by this
// conversation's own handshake, runtime, or resume evidence, to belong to
// THIS conversation. It is never a blanket allowlist: an id this conversation
// has never once produced or named as its own resume ancestor stays unknown,
// however real a Claude session it names elsewhere.
func (r *turnAccountingReducer) isKnownVendorSession(id string) bool {
	_, ok := r.knownVendorSessionIDs[id]
	return ok
}

// recordQueryLifecycleVendorLineage folds one QueryLifecycle event's vendor
// session evidence into the conversation's proven lineage, REGARDLESS of
// whether the event is live or historical: a retired query's own resume
// record and observed identity are exactly the durable proof a later
// historical response needs to be recognized as this conversation's history
// rather than rejected as evidence about an unrelated one.
func (r *turnAccountingReducer) recordQueryLifecycleVendorLineage(q *corev1.QueryLifecycle) {
	if resumed := q.GetCreated().GetResumed(); resumed != nil {
		r.recordKnownVendorSession(resumed.GetRequestedVendorSessionId())
	}
	if observed := q.GetRuntimeObserved(); observed != nil {
		r.recordKnownVendorSession(observed.GetIdentity().GetVendorSessionId())
	}
}

// bindHandshakeIdentity gives a replacement daemon the query facts that its
// durable cursor may correctly have passed. Validation finishes before the
// handshake can reconcile a turn or open a store subscription.
func (r *turnAccountingReducer) bindHandshakeIdentity(hello *corev1.ShimHello) error {
	queryID := strings.TrimSpace(hello.GetQueryInstanceId())
	if queryID == "" {
		return fmt.Errorf("shim hello omitted query_instance_id")
	}
	if r.queryID != "" && r.queryID != queryID {
		return fmt.Errorf("shim hello query_instance_id %q does not match bound query_instance_id %q", queryID, r.queryID)
	}
	vendorSessionID := hello.GetVendorSessionId()
	if r.queryID == queryID && r.queryStoreSessionID == vendorSessionID && r.queryCreatedSeq != hello.GetQueryCreatedSeq() {
		return fmt.Errorf("shim hello query_created_seq %d does not match bound query_created_seq %d for vendor_session_id %q", hello.GetQueryCreatedSeq(), r.queryCreatedSeq, vendorSessionID)
	}
	runtime := hello.GetQueryRuntimeIdentity()
	if runtime != nil {
		if vendorSessionID == "" {
			return fmt.Errorf("shim hello supplied query_runtime_identity without vendor_session_id")
		}
		if runtime.GetVendorSessionId() == "" {
			return fmt.Errorf("shim hello query_runtime_identity omitted vendor_session_id")
		}
		if runtime.GetVendorSessionId() != vendorSessionID {
			return fmt.Errorf("shim hello query_runtime_identity vendor_session_id %q does not match vendor_session_id %q", runtime.GetVendorSessionId(), vendorSessionID)
		}
		if r.runtime != nil && r.runtime.GetVendorSessionId() == runtime.GetVendorSessionId() && !proto.Equal(r.runtime, runtime) {
			return fmt.Errorf("shim hello query_runtime_identity does not match the bound runtime identity")
		}
	}
	// Mutation is deliberately last: every rejected hello leaves the reducer
	// as it was before the gate opened.
	r.queryID = queryID
	r.queryCreatedSeq = hello.GetQueryCreatedSeq()
	r.queryStoreSessionID = vendorSessionID
	r.recordKnownVendorSession(vendorSessionID)
	if runtime != nil {
		r.runtime = runtime
	}
	return nil
}

// liveEvidence is PROOF that an event belongs to the live query instance.
//
// IT IS THE ONLY THING THE LIVE-IDENTITY CHECK ACCEPTS, and liveEvidenceFor is
// its only meaningful constructor. That is the design: a replayed row cannot
// reach a live-identity comparison because a call site forgot to ask whether it
// was historical — it has nothing to pass. The zero value carries an empty
// query id, so even a hand-built one fails closed on "authoritative
// query_instance_id is required" rather than matching anything.
//
// WHY THIS TYPE EXISTS. The replay/live test used to be re-derived per event
// type: lifecycle rows were exempted from the live handshake's authority and
// AccountUsageObservations were not, six sequence numbers apart in the same
// replay. The observation was judged against a query id minted THIS process,
// and since that id is regenerated on every bring-up while the stored one is
// frozen, the two could never agree — one row made its workspace permanently
// unopenable, 13 attempts against 13 distinct live ids. Every event type added
// afterwards would have inherited the same defect silently.
type liveEvidence struct{ queryID string }

// liveEvidenceFor classifies one event's epoch ONCE, returning proof only when
// the event belongs to the live query.
//
// THE CLASSIFICATION IS ONE COMPARISON: the query the event's producer stamped
// on its envelope against the query this reducer is bound to. Nothing about how
// the event was DELIVERED enters into it, which is the point — a row the store
// serves during subscription catch-up still names the query that wrote it, so
// the session's own QueryCreated (written before its subscription can exist)
// classifies as live on that single comparison and needs no companion fact.
//
// historical means the row belongs to a DIFFERENT query. A shim restart keeps
// the vendor session and its ordered store sequence while creating a new
// query() invocation, so a resuming daemon replays the retired query's rows
// before reaching the replacement's. Those rows stay authoritative history;
// they simply cannot rebind, or be judged by, the query carried by the live
// handshake.
//
// EMPTY IS LIVE, DELIBERATELY. A producer that predates query_instance_id
// stamps nothing, and such an event must keep precisely the behavior it had
// before the field existed: judged against the live query, fatal on a
// contradiction. Fail closed — an unstamped row is never waved through as
// history.
//
// An unbound reducer (no query id yet) has nothing to compare against, so it
// admits no history at all. Query-specific resume validation is owned by
// resumeIdentityTracker, which independently follows every query_instance_id in
// the durable sequence.
func (r *turnAccountingReducer) liveEvidenceFor(ev *corev1.Event) (live liveEvidence, historical bool) {
	if r.queryID == "" {
		return liveEvidence{queryID: r.queryID}, false
	}
	if eventQuery := ev.GetQueryInstanceId(); eventQuery != "" && eventQuery != r.queryID {
		return liveEvidence{}, true
	}
	return liveEvidence{queryID: r.queryID}, false
}

// HistoricalQueryLifecycle reports whether a lifecycle row is replayed history,
// for the consumer's decision log. It is the same classification every other
// event type now gets, asked by name because the log names it.
func (r *turnAccountingReducer) HistoricalQueryLifecycle(ev *corev1.Event) (string, bool) {
	lifecycle := ev.GetQueryLifecycle()
	if lifecycle == nil {
		return "", false
	}
	_, historical := r.liveEvidenceFor(ev)
	return lifecycle.GetQueryInstanceId(), historical
}

// observeTurnClaimBridge admits the durable cross-session proof into the
// accounting correlation state without fabricating a lifecycle edge. A
// reconnect may construct a fresh consumer in the rotated vendor sequence,
// where the original TurnStarted is intentionally absent; the bridge is the
// ordered proof that lets response usage name that live root turn.
func (r *turnAccountingReducer) observeTurnClaimBridge(ev *corev1.Event) {
	id := ev.GetTurnClaimBridge().GetTurnId()
	if r.turns[id] == nil {
		r.turns[id] = &accountingTurn{startedAt: ev.GetProducedAtMs()}
	}
	r.activeTurnID = id
}

// activeTurn reports the turn the reducer is currently attributing evidence to.
//
// It exists so a reader OUTSIDE the ledger — the cold keep-alive ping's cost
// report — names a result's turn with the SAME attribution the durable ledger
// used for that result, rather than re-deriving one from the event and risking
// the two disagreeing about which turn paid.
func (r *turnAccountingReducer) activeTurn() string {
	return r.activeTurnID
}

// ErrAccountingQueryIdentityContradiction marks the ONE error observe can
// return that is NOT bookkeeping: a LIVE query lifecycle event claiming an
// identity other than the one this reducer is already bound to. That is a
// genuine protocol contradiction — the running query() invocation disagreeing
// with itself about its own identity — and stays fatal to the delivery,
// unlike every other observe failure (a malformed usage observation, an
// unattributed response, a response session mismatch), which is pure
// token-utilization bookkeeping and must not gate session establishment or
// conversation delivery. See consumer.degradeAccountingObservation.
var ErrAccountingQueryIdentityContradiction = errors.New("session-controller: live query lifecycle contradicts its own bound identity")

func (r *turnAccountingReducer) observe(ev *corev1.Event, daemonSessionID string) error {
	if q := ev.GetQueryLifecycle(); q != nil {
		r.recordQueryLifecycleVendorLineage(q)
		if _, historical := r.HistoricalQueryLifecycle(ev); historical {
			// Lifecycle history from a retired query is retained and translated by
			// the consumer, while the live handshake remains the accounting
			// authority for subsequent turn evidence.
			return nil
		}
		if r.queryID != "" && r.queryID != q.GetQueryInstanceId() {
			return fmt.Errorf("%w: query lifecycle query_instance_id %q does not match bound query_instance_id %q at seq %d (event_query_instance_id=%q)", ErrAccountingQueryIdentityContradiction, q.GetQueryInstanceId(), r.queryID, ev.GetSeq(), ev.GetQueryInstanceId())
		}
		r.queryID = q.GetQueryInstanceId()
		if observed := q.GetRuntimeObserved(); observed != nil {
			r.runtime = observed.GetIdentity()
		}
	}
	if started := ev.GetTurnStarted(); started != nil && started.GetTurnId() != "" {
		r.turns[started.GetTurnId()] = &accountingTurn{startedAt: ev.GetProducedAtMs()}
		r.activeTurnID = started.GetTurnId()
	}
	if observation := ev.GetAccountUsageObservation(); observation != nil {
		live, historical := r.liveEvidenceFor(ev)
		if historical {
			// Retired-query evidence: retained as history, not admitted as live
			// accounting, on the same terms as the lifecycle rows around it.
			return nil
		}
		if err := validateAccountUsageObservation(live, ev.GetRequestId(), observation); err != nil {
			return err
		}
		turn := r.turns[observation.GetTurnId()]
		if turn == nil {
			boundary := "unspecified"
			if observation.GetTurnStart() != nil {
				boundary = "turn_start"
			} else if observation.GetTurnEnd() != nil {
				boundary = "turn_end"
			}
			return &unknownAccountingTurnError{turnID: observation.GetTurnId(), queryID: r.queryID, boundary: boundary}
		}
		if observation.GetTurnStart() != nil {
			turn.startUsage = observation
		}
		if observation.GetTurnEnd() != nil {
			turn.endUsage = observation
			// FILED LAST for this observation, and for the same reason a
			// correction is: filing releases the holds that read this ledger,
			// and a hold released before the assignment above would resolve
			// the stamp from a turn with no end boundary on it.
			r.corrections.RecordFact(turnEndUsageKey(observation.GetTurnId()))
		}
	}
	if latency := ev.GetMessageLatency(); latency != nil && latency.GetUuid() != "" {
		r.latencies[latency.GetUuid()] = responseLatency{ttftMs: latency.GetTtftMs(), messageStartedAtMs: ev.GetProducedAtMs()}
	}
	if delta := messageDeltaFromVendor(ev.GetVendor()); delta != nil && delta.GetApiMessageId() != "" && delta.GetVendorUsage() != nil {
		if _, historical := r.liveEvidenceFor(ev); !historical {
			r.recordResponseUsageCorrection(delta.GetApiMessageId(), delta.GetVendorUsage())
		}
	}
	observation, err := tokenUtilizationObservationFromEvent(ev, daemonSessionID, r.isKnownVendorSession)
	if err != nil {
		return err
	}
	if observation != nil && observation.historical {
		// A transcript line is immutable conversation evidence, but it cannot
		// claim the reducer's mutable active turn. Its usage is attached directly
		// to that historical conversation item by pushConversation.
		return nil
	}
	var usage *frontendv1.TokenUtilization
	if observation != nil {
		usage = observation.record
	}
	if usage != nil && usage.GetApiMessageId() == "" {
		return fmt.Errorf("turn accounting response with token usage has no API message id (query_instance_id=%q active_turn_id=%q)", r.queryID, r.activeTurnID)
	}
	if usage != nil {
		claim := &unattributedResponseUsageError{queryID: r.queryID, activeTurnID: r.activeTurnID, eventRequest: ev.GetRequestId(), apiRequest: usage.GetApiRequestId(), apiMessageID: usage.GetApiMessageId()}
		switch {
		case r.activeTurnID == "":
			claim.reason = "no_active_turn"
			return claim
		case r.turns[r.activeTurnID] == nil:
			claim.reason = "active_turn_missing_from_response_ledger"
			return claim
		case ev.GetRequestId() == "":
			claim.reason = "missing_root_turn_id"
			return claim
		case ev.GetRequestId() != r.activeTurnID:
			claim.reason = "root_turn_id_mismatch"
			return claim
		}
		usage.RootTurnId = r.activeTurnID
		// BEFORE validation and before the duplicate compare below. The SDK
		// emits one assistant message per content block, all carrying the same
		// API message id and the same snapshot usage, and the compare treats any
		// difference between two such records as a ledger conflict. Correcting
		// one copy and not the other would manufacture exactly that conflict.
		r.applyResponseUsageCorrection(usage)
		if err := tokenutilization.Validate(usage, tokenutilization.Identity{
			AgentReplSessionID: daemonSessionID,
			ClaudeSessionID:    ev.GetSessionId(),
			RootTurnID:         r.activeTurnID,
		}); err != nil {
			return fmt.Errorf("turn accounting response identity: %w", err)
		}
		if latency, ok := r.latencies[usage.GetApiMessageId()]; ok {
			usage.ResponseTiming = &frontendv1.TokenResponseTiming{TimeToFirstTokenMs: &latency.ttftMs}
			if latency.messageStartedAtMs > 0 && ev.GetProducedAtMs() >= latency.messageStartedAtMs {
				duration := ev.GetProducedAtMs() - latency.messageStartedAtMs
				usage.ResponseTiming.OutputGenerationDurationMs = &duration
			}
			delete(r.latencies, usage.GetApiMessageId())
		}
		if turn := r.turns[r.activeTurnID]; turn != nil {
			duplicate := false
			for _, prior := range turn.responses {
				if prior.GetApiMessageId() != usage.GetApiMessageId() {
					continue
				}
				duplicate = true
				if usage.GetResponseTiming() == nil && prior.GetResponseTiming() != nil {
					usage.ResponseTiming = proto.Clone(prior.GetResponseTiming()).(*frontendv1.TokenResponseTiming)
				} else if usage.GetResponseTiming() != nil && prior.GetResponseTiming() == nil {
					prior.ResponseTiming = proto.Clone(usage.GetResponseTiming()).(*frontendv1.TokenResponseTiming)
				}
				if !proto.Equal(prior, usage) {
					turn.responseConflicts = append(turn.responseConflicts, "responses."+usage.GetApiMessageId())
				}
				break
			}
			if !duplicate {
				turn.responses = append(turn.responses, usage)
			}
		}
	}
	if result := resultFromVendor(ev.GetVendor()); result != nil {
		if turn := r.turns[r.activeTurnID]; turn != nil {
			turn.result = result
			turn.resultAt = ev.GetProducedAtMs()
		}
	}
	return nil
}

// validateAccountUsageObservation rejects malformed proto states before the
// reducer can retain them. The query and request identities bind the boundary
// evidence to one admitted turn. Available means a measured five-hour window
// exists; unavailable means one exact reason exists. Neither meaning may be
// invented later by a projector from an absent nested message or oneof.
// validateAccountUsageObservation takes liveEvidence rather than a bare id so a
// replayed row cannot reach it: only liveEvidenceFor produces the proof, and it
// produces none for history.
func validateAccountUsageObservation(live liveEvidence, requestID string, observation *corev1.AccountUsageObservation) error {
	queryID := live.queryID
	if strings.TrimSpace(queryID) == "" {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "authoritative query_instance_id is required"}
	}
	if strings.TrimSpace(observation.GetQueryInstanceId()) == "" {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "query_instance_id is required"}
	}
	if observation.GetQueryInstanceId() != queryID {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("query_instance_id %q does not match authoritative query_instance_id %q", observation.GetQueryInstanceId(), queryID)}
	}
	if strings.TrimSpace(observation.GetTurnId()) == "" {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "turn_id is required"}
	}
	if requestID != observation.GetTurnId() {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("event request_id %q does not match turn_id %q", requestID, observation.GetTurnId())}
	}
	if observation.GetBoundaryAtMs() <= 0 {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("boundary_at_ms must be positive, got %d", observation.GetBoundaryAtMs())}
	}
	if observation.GetObservedAtMs() <= 0 {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("observed_at_ms must be positive, got %d", observation.GetObservedAtMs())}
	}
	if observation.GetObservedAtMs() < observation.GetBoundaryAtMs() {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("observed_at_ms %d precedes boundary_at_ms %d", observation.GetObservedAtMs(), observation.GetBoundaryAtMs())}
	}
	if observation.GetSampleLatencyMs() < 0 {
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("sample_latency_ms must be nonnegative, got %d", observation.GetSampleLatencyMs())}
	}
	switch boundary := observation.GetBoundary().(type) {
	case *corev1.AccountUsageObservation_TurnStart:
		if boundary == nil || boundary.TurnStart == nil {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "turn_start boundary is nil"}
		}
	case *corev1.AccountUsageObservation_TurnEnd:
		if boundary == nil || boundary.TurnEnd == nil {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "turn_end boundary is nil"}
		}
	default:
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "boundary is required"}
	}
	switch outcome := observation.GetOutcome().(type) {
	case *corev1.AccountUsageObservation_Available:
		if outcome == nil || outcome.Available == nil || outcome.Available.GetFiveHour() == nil {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "available outcome requires five_hour"}
		}
		window := outcome.Available.GetFiveHour()
		if utilization := window.GetUtilizationPercent(); math.IsNaN(utilization) || math.IsInf(utilization, 0) || utilization < 0 || utilization > 100 {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("available.five_hour.utilization_percent must be finite and within [0,100], got %v", utilization)}
		}
		if window.GetResetsAtMs() <= 0 {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: fmt.Sprintf("available.five_hour.resets_at_ms must be positive, got %d", window.GetResetsAtMs())}
		}
	case *corev1.AccountUsageObservation_Unavailable:
		if outcome == nil || outcome.Unavailable == nil || outcome.Unavailable.GetReason() == nil {
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable outcome requires a reason"}
		}
		switch reason := outcome.Unavailable.GetReason().(type) {
		case *corev1.AccountUsageUnavailable_ServiceUnavailable:
			if reason == nil || reason.ServiceUnavailable == nil {
				return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable.service_unavailable is nil"}
			}
		case *corev1.AccountUsageUnavailable_WindowUnavailable:
			if reason == nil || reason.WindowUnavailable == nil {
				return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable.window_unavailable is nil"}
			}
		case *corev1.AccountUsageUnavailable_UtilizationUnavailable:
			if reason == nil || reason.UtilizationUnavailable == nil {
				return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable.utilization_unavailable is nil"}
			}
		case *corev1.AccountUsageUnavailable_SamplingFailure:
			if reason == nil || reason.SamplingFailure == nil {
				return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable.sampling_failure is nil"}
			}
			if strings.TrimSpace(reason.SamplingFailure.GetCause()) == "" {
				return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable.sampling_failure.cause is required"}
			}
		default:
			return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "unavailable outcome has an unrecognized reason"}
		}
	default:
		return &malformedAccountUsageObservationError{turnID: observation.GetTurnId(), reason: "outcome is required"}
	}
	return nil
}

func (r *turnAccountingReducer) resolve(ended *corev1.Event, settledAt int64) *frontendv1.TurnAccounting {
	return r.resolveTurn(ended.GetTurnEnded().GetTurnId(), settledAt)
}

// resolveTurn is resolve's body, reachable WITHOUT a TurnEnded event.
//
// A turn's end is written by two authorities: the stream's `TurnEnded`, and the
// daemon itself when the turn behind a claim can no longer produce one
// (SynthesizeTurnClose). Only the first used to be able to resolve accounting,
// so a synthesized close left the turn's retained terminal result stranded
// forever — 16 held against 0 released in one daemon lifetime. Both authorities
// resolve through this one function so neither can drift from the other.
func (r *turnAccountingReducer) resolveTurn(turnID string, settledAt int64) *frontendv1.TurnAccounting {
	turn := r.turns[turnID]
	if turn == nil {
		return invalidTurnAccounting(turnID, r.queryID, nil, settledAt, runtimeIdentityProblem(r.queryID, incompleteRuntimeIdentityPaths(nil)))
	}
	problems := make([]*frontendv1.TurnAccountingProblem, 0, 3)
	if turn.startUsage == nil || turn.startUsage.GetAvailable() == nil {
		problems = append(problems, missingUsageProblem(true))
	}
	if turn.endUsage == nil || turn.endUsage.GetAvailable() == nil {
		problems = append(problems, missingUsageProblem(false))
	}
	if startWindow, endWindow := turn.startUsage.GetAvailable().GetFiveHour(), turn.endUsage.GetAvailable().GetFiveHour(); startWindow != nil && endWindow != nil && usageWindowRolledOver(startWindow.GetResetsAtMs(), endWindow.GetResetsAtMs()) {
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_WindowReset{WindowReset: &frontendv1.UsageWindowReset{StartResetsAtMs: startWindow.GetResetsAtMs(), EndResetsAtMs: endWindow.GetResetsAtMs()}}})
	}
	if missing := incompleteRuntimeIdentityPaths(r.runtime); len(missing) > 0 {
		problems = append(problems, runtimeIdentityProblem(r.queryID, missing))
	}
	if turn.result == nil {
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_TelemetryRecordMissing{TelemetryRecordMissing: &frontendv1.TelemetryRecordMissing{Record: &frontendv1.TelemetryRecordMissing_PersistenceReceipt{PersistenceReceipt: &frontendv1.TelemetryRecordMissingPersistenceReceipt{TurnId: turnID}}}}})
	}
	reconciliation := reconcileTokenUsage(turn.responses, turn.result, dlog.Tag(r.logf, "turn", turnID, "query", r.queryID))
	reconciliation.mismatch = append(reconciliation.mismatch, turn.responseConflicts...)
	sort.Strings(reconciliation.mismatch)
	if len(reconciliation.mismatch) > 0 {
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_TokenLedgerMismatch{TokenLedgerMismatch: &frontendv1.TokenLedgerMismatch{DifferingFieldPaths: reconciliation.mismatch}}})
	}
	var unmodeled []string
	for _, response := range turn.responses {
		for field := range response.GetUsage().GetUnmodeledUsage().GetFields() {
			unmodeled = append(unmodeled, field)
		}
	}
	if len(unmodeled) > 0 {
		sort.Strings(unmodeled)
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_UnmodeledUsageFields{UnmodeledUsageFields: &frontendv1.UnmodeledUsageFields{SourceFieldPaths: unmodeled}}})
	}
	promptAt, resultAt := turn.startedAt, turn.resultAt
	if turn.startUsage != nil && turn.startUsage.GetBoundaryAtMs() > 0 {
		promptAt = turn.startUsage.GetBoundaryAtMs()
	}
	if turn.endUsage != nil && turn.endUsage.GetBoundaryAtMs() > 0 {
		resultAt = turn.endUsage.GetBoundaryAtMs()
	}
	accounting := &frontendv1.TurnAccounting{TurnId: turnID, QueryInstanceId: r.queryID, Runtime: r.runtime, UsageAtStart: turn.startUsage, UsageAtEnd: turn.endUsage, Responses: turn.responses, Reconciliation: reconciliation.reconciliation, Timing: &frontendv1.TurnAccountingTiming{PromptAdmittedAtMs: promptAt, ResultReceivedAtMs: resultAt, AccountingSettledAtMs: settledAt, PromptToResultMs: resultAt - promptAt, ResultToSettlementMs: settledAt - resultAt}}
	if len(problems) == 0 {
		accounting.Verdict = &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}
	} else {
		accounting.Verdict = &frontendv1.TurnAccounting_Invalid{Invalid: &frontendv1.TurnAccountingInvalid{Problems: problems}}
	}
	return accounting
}

// ErrAccountingCommitUnknownTurn marks a terminal accounting commit naming a
// turn this reducer holds no unsettled ledger entry for.
//
// IT USED TO BE A PANIC, and a panic is the wrong shape for it. The condition
// is reachable from ordinary DATA — two legitimate release authorities naming
// the same turn (a synthesized close and a teardown, each snapshotting the held
// set before the other discharged it), or a replayed historical turn whose
// accounting was settled and retired by an earlier generation. Killing the
// daemon process on either one turns a bookkeeping collision into an outage
// that recurs on every boot that replays the same rows. The invariant is not
// weakened: the condition still travels as an error and is still recorded
// LOUDLY through the consumer's WARN channel by the settlement path that owns
// it. See consumer.settleTurnAccounting.
var ErrAccountingCommitUnknownTurn = errors.New("session-controller: turn accounting commit names unknown turn")

// hasTurn reports whether the reducer still holds an unsettled ledger entry for
// turnID. A settled turn is retired by commitResolved, so a false answer means
// "already settled, or never admitted" — the two cases the settlement path must
// serve from the durable store rather than recompute.
func (r *turnAccountingReducer) hasTurn(turnID string) bool {
	return r.turns[turnID] != nil
}

// commitResolved retires reducer memory only after the durable terminal
// accounting transaction succeeds. A persistence failure therefore leaves the
// complete turn available for the replayed TurnEnded to resolve again.
func (r *turnAccountingReducer) commitResolved(turnID string) error {
	if r.turns[turnID] == nil {
		return fmt.Errorf("%w %q", ErrAccountingCommitUnknownTurn, turnID)
	}
	delete(r.turns, turnID)
	if r.activeTurnID == turnID {
		r.activeTurnID = ""
	}
	// THE CORRECTIONS LEDGER OUTLIVES THE SETTLEMENT, deliberately. It used to be
	// cleared here, which made a correction arriving after its turn settled land
	// on an empty ledger and vanish. Late corrections are now a first-class
	// outcome — they REVISE the settled stamp (terminalsettlement.go) — so the
	// ledger is retired by its own bounded window instead (accountinghold.go,
	// enrichmentRetention) rather than by this commit.
	return nil
}

func (r *turnAccountingReducer) response(apiMessageID string) *frontendv1.TokenUtilization {
	turn := r.turns[r.activeTurnID]
	if turn == nil {
		return nil
	}
	for i := len(turn.responses) - 1; i >= 0; i-- {
		if turn.responses[i].GetApiMessageId() == apiMessageID {
			return turn.responses[i]
		}
	}
	return nil
}

// recordResponseUsageCorrection files one response's FINAL vendor usage and
// applies it to that response if the reducer already holds it.
//
// The correction is authoritative over the snapshot it replaces: it is the
// vendor's own cumulative usage for the same API message, reported later.
func (r *turnAccountingReducer) recordResponseUsageCorrection(apiMessageID string, usage *datav1.ApiUsage) {
	corrected := vendorTokenUsageFromAPI(usage)
	if existing := r.response(apiMessageID); existing != nil {
		existing.Usage = proto.Clone(corrected).(*frontendv1.VendorTokenUsage)
		r.logf("session-controller: corrected a response's token usage from its message_delta (api_message_id=%s output_tokens=%d)", apiMessageID, corrected.GetOutputTokens())
	}
	// FILED LAST, because filing is what releases the enrichment holds that read
	// this ledger, and a hold released before the in-place patch above would
	// resolve the turn's stamp from the response record it is about to correct.
	r.corrections.Record(apiMessageID, corrected)
}

// enrichmentDependencies names everything one turn's accounting stamp is not
// complete without: the final usage of every response it is derived from, and
// the turn's own END-BOUNDARY account-usage observation.
//
// THE END BOUNDARY IS A DEPENDENCY OF THE STAMP AND OF NOTHING ELSE. The turn
// is settled by the vendor's result and the user has their answer long before
// this set is satisfied; what waits on it is only the figure in the footer. A
// turn the reducer no longer holds names nothing.
func (r *turnAccountingReducer) enrichmentDependencies(turnID string) []string {
	turn := r.turns[turnID]
	if turn == nil {
		return nil
	}
	deps := make([]string, 0, len(turn.responses)+1)
	for _, response := range turn.responses {
		if id := response.GetApiMessageId(); id != "" {
			deps = append(deps, id)
		}
	}
	return append(deps, turnEndUsageKey(turnID))
}

// applyResponseUsageCorrection gives a response record the final usage its
// message_delta already reported, when the delta arrived first.
//
// A response with no correction on file is left EXACTLY as the vendor's
// assistant message described it. That is the pre-amendment world, and it stays
// representable: the ledger reports its disagreement honestly rather than being
// quietly reconciled against evidence nobody sent.
func (r *turnAccountingReducer) applyResponseUsageCorrection(usage *frontendv1.TokenUtilization) {
	corrected := r.corrections.Correction(usage.GetApiMessageId())
	if corrected == nil {
		return
	}
	usage.Usage = corrected
	r.logf("session-controller: applied a filed message_delta token-usage correction to its response (api_message_id=%s output_tokens=%d)", usage.GetApiMessageId(), corrected.GetOutputTokens())
}

// messageDeltaFromVendor extracts a relayed `message_delta` frame from an
// event's vendor payload, or nil when the payload is anything else.
func messageDeltaFromVendor(a *anypb.Any) *datav1.MessageDeltaEvent {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	stream, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return stream.GetStreamEvent().GetEvent().GetMessageDelta()
}

func resultFromVendor(a *anypb.Any) *datav1.ResultMessage {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	if direct, ok := msg.(*datav1.ResultMessage); ok {
		return direct
	}
	if stream, ok := msg.(*datav1.ClaudeStreamMessage); ok {
		return stream.GetResult()
	}
	return nil
}

type tokenUtilizationObservation struct {
	record     *frontendv1.TokenUtilization
	historical bool
	// priorVendorSession is set only when this record was admitted under the
	// prior-session exception below: a historical response whose embedded
	// Claude session id is a RETIRED session this conversation itself proved
	// as its own resume ancestor, not the file's own authoritative id. Empty
	// on every ordinary record, including every live one.
	priorVendorSession string
}

// knownVendorSessionFunc reports whether id has been durably proven to belong
// to the caller's own conversation. Satisfied by
// turnAccountingReducer.isKnownVendorSession; a nil func admits none, which
// is the fail-closed default for any caller not tracking conversation
// lineage at all.
type knownVendorSessionFunc func(id string) bool

func tokenUtilizationFromEvent(ev *corev1.Event, daemonSessionID string, knownVendorSession knownVendorSessionFunc) (*frontendv1.TokenUtilization, error) {
	observation, err := tokenUtilizationObservationFromEvent(ev, daemonSessionID, knownVendorSession)
	if observation == nil || err != nil {
		return nil, err
	}
	return observation.record, nil
}

// tokenUtilizationObservationFromEvent classifies response usage by source.
// Stream responses require an admitted live root turn in the reducer. File
// transcript responses have stable response/session identity but no reliable
// enclosing turn or stream timing, so replay may attach them only as explicit
// historical evidence.
func tokenUtilizationObservationFromEvent(ev *corev1.Event, daemonSessionID string, knownVendorSession knownVendorSessionFunc) (*tokenUtilizationObservation, error) {
	a := ev.GetVendor()
	if a == nil {
		return nil, nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil, nil
	}
	var assistant *datav1.AssistantMessage
	historical := false
	switch v := msg.(type) {
	case *datav1.AssistantMessage:
		assistant = v
	case *datav1.ClaudeStreamMessage:
		assistant = v.GetAssistant()
	case *datav1.TranscriptLine:
		line := v.GetAssistant()
		if line == nil {
			return nil, nil
		}
		envelope := line.GetEnvelope()
		assistant = &datav1.AssistantMessage{
			Message:         line.GetMessage(),
			SessionId:       envelope.GetSessionId(),
			AgentId:         envelope.GetAgentId(),
			ParentToolUseId: envelope.GetSourceToolUseId(),
		}
		if requestID := envelope.GetRequestId(); requestID != "" {
			assistant.RequestId = &requestID
		}
		historical = true
	}
	if assistant == nil {
		return nil, nil
	}
	if assistant.GetMessage() == nil {
		return nil, fmt.Errorf("turn accounting assistant response has no message")
	}
	if assistant.GetMessage().GetUsage() == nil {
		// Not every assistant transcript item carries a Messages-API usage
		// object. It is conversation content, not a token-utilization record;
		// every record that is constructed below is still complete-or-rejected.
		return nil, nil
	}
	if daemonSessionID == "" {
		return nil, fmt.Errorf("turn accounting response has blank agent-repl session id")
	}
	authoritativeClaudeSessionID := ev.GetSessionId()
	if authoritativeClaudeSessionID == "" {
		return nil, fmt.Errorf("turn accounting response has blank authoritative Claude session id (agent_repl_session_id=%q api_message_id=%q)", daemonSessionID, assistant.GetMessage().GetId())
	}
	if assistant.GetSessionId() == "" {
		return nil, fmt.Errorf("turn accounting response has blank assistant Claude session id (agent_repl_session_id=%q authoritative_claude_session_id=%q api_message_id=%q)", daemonSessionID, authoritativeClaudeSessionID, assistant.GetMessage().GetId())
	}
	priorVendorSession := ""
	if assistant.GetSessionId() != authoritativeClaudeSessionID {
		// A RESUMED CONVERSATION'S OLD FILE IS STILL THIS CONVERSATION'S OWN
		// HISTORY. The SDK carries prior transcript content forward into a
		// resumed session's own file, unchanged, so an assistant message
		// produced under a RETIRED vendor session id can be read back from the
		// CURRENT file's replay — the record's own embedded session id names
		// the retired session, while the envelope names the file it is being
		// read from today. That disagreement is expected, not corruption, for
		// exactly the ids this conversation has already proven belong to it:
		// see recordQueryLifecycleVendorLineage. Live evidence gets no such
		// exception — a live stream response naming a session other than the
		// one it is streaming on is a genuine contradiction, not history.
		if !historical || knownVendorSession == nil || !knownVendorSession(assistant.GetSessionId()) {
			return nil, fmt.Errorf("turn accounting response Claude session mismatch (agent_repl_session_id=%q authoritative_claude_session_id=%q assistant_claude_session_id=%q api_message_id=%q historical=%v)", daemonSessionID, authoritativeClaudeSessionID, assistant.GetSessionId(), assistant.GetMessage().GetId(), historical)
		}
		priorVendorSession = assistant.GetSessionId()
	}
	u := assistant.GetMessage().GetUsage()
	record := &frontendv1.TokenUtilization{AgentReplSessionId: daemonSessionID, ClaudeSessionId: assistant.GetSessionId(), ApiMessageId: assistant.GetMessage().GetId(), Model: assistant.GetMessage().GetModel(), Usage: vendorTokenUsageFromAPI(u)}
	if assistant.RequestId != nil {
		requestID := assistant.GetRequestId()
		record.ApiRequestId = &requestID
	}
	frontend.SetTokenUtilizationActor(record, assistant)
	if historical {
		// expectedClaudeSessionID is the record's OWN session id when the
		// prior-session exception admitted it: the identity agreement was
		// already proven above (against the conversation's known lineage,
		// not the current file alone), so this check is left to enforce every
		// OTHER historical invariant without re-litigating a question already
		// answered.
		expectedClaudeSessionID := authoritativeClaudeSessionID
		if priorVendorSession != "" {
			expectedClaudeSessionID = priorVendorSession
		}
		if err := tokenutilization.ValidateHistorical(record, tokenutilization.Identity{
			AgentReplSessionID: daemonSessionID,
			ClaudeSessionID:    expectedClaudeSessionID,
		}); err != nil {
			return nil, fmt.Errorf("historical token utilization identity: %w", err)
		}
	}
	return &tokenUtilizationObservation{record: record, historical: historical, priorVendorSession: priorVendorSession}, nil
}

// vendorTokenUsageFromAPI builds the DURABLE, vendor-faithful record this
// response is persisted as. It is not the economics layer: the canonical
// TokenUsage is derived from this at read time (internal/tokenusage), never
// stored beside it.
//
// THE SHAPE IS FROZEN, INCLUDING THE RATES. cache_rates is durable-legacy — see
// frontendv1.TokenCacheRates — and it stays populated because a persisted row
// written by an earlier build must keep replaying byte-identically through
// statedb's proto.Equal comparison. It is computed HERE, from the same counters
// the shim used to divide before its rate partition was retired, so a replayed
// pre-retirement store event produces the identical durable record it always
// did. Nothing reads it to decide anything.
func vendorTokenUsageFromAPI(u *datav1.ApiUsage) *frontendv1.VendorTokenUsage {
	usage := &frontendv1.VendorTokenUsage{InputTokens: u.GetInputTokens(), OutputTokens: u.GetOutputTokens(), CacheReadInputTokens: u.GetCacheReadInputTokens(), CacheCreationInputTokens: u.GetCacheCreationInputTokens(), ServiceTier: u.GetServiceTier(), Speed: u.GetSpeed(), InferenceGeo: u.GetInferenceGeo(), RawUsage: proto.Clone(u).(*datav1.ApiUsage), CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: structInt(u.GetCacheCreation(), "ephemeral_5m_input_tokens"), Ephemeral_1HInputTokens: structInt(u.GetCacheCreation(), "ephemeral_1h_input_tokens")}, ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: structInt(u.GetServerToolUse(), "web_search_requests"), WebFetchRequests: structInt(u.GetServerToolUse(), "web_fetch_requests")}}
	usage.CacheRates = frontend.CacheRatesFromCounters(u.GetInputTokens(), u.GetCacheReadInputTokens(), u.GetCacheCreationInputTokens())
	usage.Iterations = tokenIterations(u.GetIterations())
	if u.GetCacheCreation() == nil {
		// The vendor did not report the TTL breakdown at this level at all —
		// see cacheCreationFromIterations. A breakdown that IS reported is
		// taken verbatim above, zeros included.
		if recovered := cacheCreationFromIterations(usage.GetIterations(), u.GetCacheCreationInputTokens()); recovered != nil {
			usage.CacheCreation = recovered
		}
	}
	thinkingTokens := structInt(u.GetOutputTokensDetails(), "thinking_tokens")
	if u.GetOutputTokensDetails().GetFields()["thinking_tokens"] == nil {
		thinkingTokens = structInt(u.GetOutputTokensDetails(), "reasoning_tokens")
	}
	usage.OutputDetails = &frontendv1.TokenOutputDetails{ThinkingTokens: thinkingTokens}
	usage.FallbackCredit = cloneStruct(u.GetFallbackCredit())
	usage.UnmodeledUsage = collectUnmodeledUsage(u)
	usage.CacheDiagnostic = cacheDiagnostic(u.GetCacheDiagnostic())
	return usage
}

func cloneStruct(s *structpb.Struct) *structpb.Struct {
	if s == nil {
		return nil
	}
	return proto.Clone(s).(*structpb.Struct)
}

func collectUnmodeledUsage(u *datav1.ApiUsage) *structpb.Struct {
	out := cloneStruct(u.GetUnmodeledUsage())
	if out == nil {
		out = &structpb.Struct{Fields: map[string]*structpb.Value{}}
	}
	copyUnknown := func(prefix string, source *structpb.Struct, known ...string) {
		knownSet := map[string]struct{}{}
		for _, key := range known {
			knownSet[key] = struct{}{}
		}
		for key, value := range source.GetFields() {
			if _, ok := knownSet[key]; !ok {
				out.Fields[prefix+"."+key] = proto.Clone(value).(*structpb.Value)
			}
		}
	}
	copyUnknown("cache_creation", u.GetCacheCreation(), "ephemeral_5m_input_tokens", "ephemeral_1h_input_tokens")
	copyUnknown("server_tool_use", u.GetServerToolUse(), "web_search_requests", "web_fetch_requests")
	copyUnknown("output_tokens_details", u.GetOutputTokensDetails(), "thinking_tokens", "reasoning_tokens")
	copyUnknown("cache_diagnostic", u.GetCacheDiagnostic(), "cache_miss_reason", "cache_missed_input_tokens")
	for index, value := range u.GetIterations().GetValues() {
		if vendorIterationKind(value) == iterationUnmodeled {
			out.Fields[fmt.Sprintf("iterations.%d", index)] = proto.Clone(value).(*structpb.Value)
		}
	}
	if len(out.GetFields()) == 0 {
		return nil
	}
	return out
}

func cacheDiagnostic(s *structpb.Struct) *frontendv1.TokenCacheDiagnostic {
	if s == nil {
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_DiagnosticsUnavailable{DiagnosticsUnavailable: &frontendv1.TokenCacheDiagnosticDiagnosticsUnavailable{}}}
	}
	reason := s.GetFields()["cache_miss_reason"]
	if reason == nil || reason.GetKind() == nil {
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_Pending{Pending: &frontendv1.TokenCacheDiagnosticPending{}}}
	}
	if _, ok := reason.GetKind().(*structpb.Value_NullValue); ok {
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_Pending{Pending: &frontendv1.TokenCacheDiagnosticPending{}}}
	}
	missed := structInt(s, "cache_missed_input_tokens")
	switch reason.GetStringValue() {
	case "model_changed":
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_ModelChanged{ModelChanged: &frontendv1.TokenCacheDiagnosticModelChanged{CacheMissedInputTokens: missed}}}
	case "system_changed":
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_SystemChanged{SystemChanged: &frontendv1.TokenCacheDiagnosticSystemChanged{CacheMissedInputTokens: missed}}}
	case "tools_changed":
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_ToolsChanged{ToolsChanged: &frontendv1.TokenCacheDiagnosticToolsChanged{CacheMissedInputTokens: missed}}}
	case "messages_changed":
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_MessagesChanged{MessagesChanged: &frontendv1.TokenCacheDiagnosticMessagesChanged{CacheMissedInputTokens: missed}}}
	case "previous_message_unavailable":
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_PreviousMessageUnavailable{PreviousMessageUnavailable: &frontendv1.TokenCacheDiagnosticPreviousMessageUnavailable{}}}
	default:
		return &frontendv1.TokenCacheDiagnostic{Reason: &frontendv1.TokenCacheDiagnostic_DiagnosticsUnavailable{DiagnosticsUnavailable: &frontendv1.TokenCacheDiagnosticDiagnosticsUnavailable{}}}
	}
}

func structInt(s *structpb.Struct, key string) int64 {
	if s == nil || s.GetFields()[key] == nil {
		return 0
	}
	return int64(s.GetFields()[key].GetNumberValue())
}

// iterationKind is the schema arm one vendor usage iteration belongs in, or
// iterationUnmodeled when the vendor named a kind this build cannot file.
type iterationKind int

const (
	iterationUnmodeled iterationKind = iota
	iterationSampling
	iterationCompaction
	iterationAdvisor
	iterationFallback
)

// vendorIterationKind reads the vendor's own discriminator off one entry of
// `usage.iterations`.
//
// THE DISCRIMINATOR VALUES ARE THE VENDOR'S, NOT THE SCHEMA'S ARM NAMES. The
// SDK's iteration union (`@anthropic-ai/sdk`
// `resources/beta/messages/messages.d.ts`, BetaIterationsUsage) is
// BetaMessageIterationUsage `message`, BetaCompactionIterationUsage
// `compaction`, BetaAdvisorMessageIterationUsage `advisor_message`, and
// BetaFallbackMessageIterationUsage `fallback_message`. Matching on the arm
// names instead — `sampling`, `advisor`, `fallback` — meant every real
// iteration fell through to unmodeled: on one measured live turn all 113
// responses carried a `message` iteration, each dropped from the record, each
// filed as an unmodeled `iterations.0`, and the turn condemned for it.
//
// ONE FUNCTION ANSWERS FOR BOTH READERS — the mapper that files an iteration
// and the detector that flags an unmodeled one — so the modeled set cannot
// drift between them.
//
// An entry with no `type` at all is an ordinary sampling hop: absence is the
// unlabeled default, not an unknown kind.
func vendorIterationKind(value *structpb.Value) iterationKind {
	s := value.GetStructValue()
	if s == nil {
		return iterationUnmodeled
	}
	switch s.GetFields()["type"].GetStringValue() {
	case "", "message":
		return iterationSampling
	case "compaction":
		return iterationCompaction
	case "advisor_message":
		return iterationAdvisor
	case "fallback_message":
		return iterationFallback
	default:
		return iterationUnmodeled
	}
}

func tokenIterations(values *structpb.ListValue) []*frontendv1.TokenUsageIteration {
	if values == nil {
		return nil
	}
	out := make([]*frontendv1.TokenUsageIteration, 0, len(values.GetValues()))
	for _, value := range values.GetValues() {
		kind := vendorIterationKind(value)
		if kind == iterationUnmodeled {
			continue
		}
		s := value.GetStructValue()
		i, o := structInt(s, "input_tokens"), structInt(s, "output_tokens")
		r, c := structInt(s, "cache_read_input_tokens"), structInt(s, "cache_creation_input_tokens")
		creation := &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: structInt(s.GetFields()["cache_creation"].GetStructValue(), "ephemeral_5m_input_tokens"), Ephemeral_1HInputTokens: structInt(s.GetFields()["cache_creation"].GetStructValue(), "ephemeral_1h_input_tokens")}
		model := s.GetFields()["model"].GetStringValue()
		switch kind {
		case iterationCompaction:
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Compaction{Compaction: &frontendv1.TokenUsageIterationCompaction{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation}}})
		case iterationAdvisor:
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Advisor{Advisor: &frontendv1.TokenUsageIterationAdvisor{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: model}}})
		case iterationFallback:
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Fallback{Fallback: &frontendv1.TokenUsageIterationFallback{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: model}}})
		default:
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Sampling{Sampling: &frontendv1.TokenUsageIterationSampling{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: model}}})
		}
	}
	return out
}

// iterationCacheCreation reads one filed iteration's TTL split, whichever arm
// it took.
func iterationCacheCreation(iteration *frontendv1.TokenUsageIteration) *frontendv1.TokenCacheCreation {
	switch {
	case iteration.GetSampling() != nil:
		return iteration.GetSampling().GetCacheCreation()
	case iteration.GetCompaction() != nil:
		return iteration.GetCompaction().GetCacheCreation()
	case iteration.GetAdvisor() != nil:
		return iteration.GetAdvisor().GetCacheCreation()
	case iteration.GetFallback() != nil:
		return iteration.GetFallback().GetCacheCreation()
	default:
		return nil
	}
}

// cacheCreationFromIterations recovers a response's cache-creation TTL split
// from its own iterations, for the one case where the vendor's usage object
// carries no `cache_creation` member at all.
//
// WHY THE SPLIT GOES MISSING. A response's FINAL usage is the one relayed on
// `message_delta`, and the vendor's delta usage shape has no `cache_creation`
// member to relay: `@anthropic-ai/sdk` BetaMessageDeltaUsage carries
// `cache_creation_input_tokens` but not the per-TTL breakdown, while BetaUsage
// on the assistant message carries both. Replacing the message_start snapshot
// with the authoritative delta therefore erased the breakdown, and every
// session running the 1h cache TTL settled invalid at
// `cache_creation.ephemeral_1h_input_tokens` — 0 on the response plane against
// 193,050 on the result plane, on one measured 113-response turn.
//
// WHY THE ITERATIONS ARE THE RIGHT SOURCE. The same delta carries the same
// response's `iterations`, and each entry states its own TTL split. Across all
// 181 usage-bearing responses in that session's transcript, every response
// carried exactly one `message` iteration whose counters were IDENTICAL to the
// response's own, and the iteration aggregate equalled the response aggregate
// in every dimension.
//
// NO MULTI-HOP AGGREGATION IS ASSUMED. The recovered split is accepted only
// when the two TTL buckets summed over the iterations equal the response's own
// `cache_creation_input_tokens`, a total already held independently. Anything
// else returns nothing, so the ledger keeps reporting its disagreement rather
// than being reconciled against a relation this evidence does not demonstrate.
func cacheCreationFromIterations(iterations []*frontendv1.TokenUsageIteration, cacheCreationInputTokens int64) *frontendv1.TokenCacheCreation {
	if len(iterations) == 0 {
		return nil
	}
	out := &frontendv1.TokenCacheCreation{}
	for _, iteration := range iterations {
		creation := iterationCacheCreation(iteration)
		out.Ephemeral_5MInputTokens += creation.GetEphemeral_5MInputTokens()
		out.Ephemeral_1HInputTokens += creation.GetEphemeral_1HInputTokens()
	}
	if out.GetEphemeral_5MInputTokens()+out.GetEphemeral_1HInputTokens() != cacheCreationInputTokens {
		return nil
	}
	return out
}

type reconciliationResult struct {
	reconciliation *frontendv1.TokenUsageReconciliation
	mismatch       []string
}

// reconcileTokenUsage compares the stream plane's per-response evidence against
// the result plane's terminal usage. logf carries the resolving turn's identity
// and is never nil — callers route it through the reducer's tagged channel.
func reconcileTokenUsage(records []*frontendv1.TokenUtilization, result *datav1.ResultMessage, logf dlog.Logf) reconciliationResult {
	logf("session-controller: reconciling turn token ledger (responses=%d result_present=%t)", len(records), result != nil)
	aggregate := frontend.AggregateTokenUtilization(records)
	apiMessageIDSet := make(map[string]struct{}, len(records))
	for _, record := range records {
		if record == nil || record.GetApiMessageId() == "" {
			panic("turn accounting reconciliation received a response without an API message id")
		}
		apiMessageID := record.GetApiMessageId()
		if _, duplicate := apiMessageIDSet[apiMessageID]; duplicate {
			panic(fmt.Sprintf("turn accounting reconciliation received duplicate API message id %q", apiMessageID))
		}
		apiMessageIDSet[apiMessageID] = struct{}{}
	}
	apiMessageIDs := make([]string, 0, len(apiMessageIDSet))
	for apiMessageID := range apiMessageIDSet {
		apiMessageIDs = append(apiMessageIDs, apiMessageID)
	}
	sort.Strings(apiMessageIDs)
	r := &frontendv1.TokenUsageReconciliation{ResponseRecordCount: int64(len(records)), ResponseAllAgents: aggregate.GetAllAgents(), ResponseMainAgent: aggregate.GetMainAgent(), ApiMessageIds: apiMessageIDs}
	if result == nil {
		return reconciliationResult{reconciliation: r}
	}
	u := result.GetUsage()
	r.ResultMainAgent = &frontendv1.TokenUsageTotals{InputTokens: u.GetInputTokens(), OutputTokens: u.GetOutputTokens(), CacheReadInputTokens: u.GetCacheReadInputTokens(), CacheCreationInputTokens: u.GetCacheCreationInputTokens(), CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: structInt(u.GetCacheCreation(), "ephemeral_5m_input_tokens"), Ephemeral_1HInputTokens: structInt(u.GetCacheCreation(), "ephemeral_1h_input_tokens")}, ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: structInt(u.GetServerToolUse(), "web_search_requests"), WebFetchRequests: structInt(u.GetServerToolUse(), "web_fetch_requests")}}
	r.ResponseModels = aggregate.GetModels()
	modelKeys := make([]string, 0, len(result.GetModelUsage()))
	for model := range result.GetModelUsage() {
		modelKeys = append(modelKeys, model)
	}
	sort.Strings(modelKeys)
	for _, model := range modelKeys {
		usage := result.GetModelUsage()[model]
		r.ResultModels = append(r.ResultModels, &frontendv1.ModelTokenUtilization{
			Model:           model,
			CanonicalModel:  cloneOptional(usage.CanonicalModel),
			Provider:        cloneOptional(usage.Provider),
			ContextWindow:   cloneOptional(usage.ContextWindow),
			MaxOutputTokens: cloneOptional(usage.MaxOutputTokens),
			CostUsd:         cloneOptional(usage.CostUsd),
			Totals: &frontendv1.TokenUsageTotals{
				InputTokens: usage.GetInputTokens(), OutputTokens: usage.GetOutputTokens(),
				CacheReadInputTokens: usage.GetCacheReadInputTokens(), CacheCreationInputTokens: usage.GetCacheCreationInputTokens(),
				ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: usage.GetWebSearchRequests()},
			},
		})
	}
	var mismatch []string
	if r.GetResponseMainAgent().GetInputTokens() != r.GetResultMainAgent().GetInputTokens() {
		mismatch = append(mismatch, "input_tokens")
	}
	if r.GetResponseMainAgent().GetOutputTokens() != r.GetResultMainAgent().GetOutputTokens() {
		mismatch = append(mismatch, "output_tokens")
	}
	if r.GetResponseMainAgent().GetCacheReadInputTokens() != r.GetResultMainAgent().GetCacheReadInputTokens() {
		mismatch = append(mismatch, "cache_read_input_tokens")
	}
	if r.GetResponseMainAgent().GetCacheCreationInputTokens() != r.GetResultMainAgent().GetCacheCreationInputTokens() {
		mismatch = append(mismatch, "cache_creation_input_tokens")
	}
	if r.GetResponseMainAgent().GetCacheCreation().GetEphemeral_5MInputTokens() != r.GetResultMainAgent().GetCacheCreation().GetEphemeral_5MInputTokens() {
		mismatch = append(mismatch, "cache_creation.ephemeral_5m_input_tokens")
	}
	if r.GetResponseMainAgent().GetCacheCreation().GetEphemeral_1HInputTokens() != r.GetResultMainAgent().GetCacheCreation().GetEphemeral_1HInputTokens() {
		mismatch = append(mismatch, "cache_creation.ephemeral_1h_input_tokens")
	}
	if r.GetResponseMainAgent().GetServerToolUse().GetWebSearchRequests() != r.GetResultMainAgent().GetServerToolUse().GetWebSearchRequests() {
		mismatch = append(mismatch, "server_tool_use.web_search_requests")
	}
	if r.GetResponseMainAgent().GetServerToolUse().GetWebFetchRequests() != r.GetResultMainAgent().GetServerToolUse().GetWebFetchRequests() {
		mismatch = append(mismatch, "server_tool_use.web_fetch_requests")
	}
	responseModels := map[string]*frontendv1.ModelTokenUtilization{}
	for _, model := range r.GetResponseModels() {
		responseModels[model.GetModel()] = model
	}
	resultModels := map[string]*frontendv1.ModelTokenUtilization{}
	for _, model := range r.GetResultModels() {
		resultModels[model.GetModel()] = model
		observed := responseModels[model.GetModel()]
		if observed == nil || observed.GetTotals().GetInputTokens() != model.GetTotals().GetInputTokens() || observed.GetTotals().GetOutputTokens() != model.GetTotals().GetOutputTokens() || observed.GetTotals().GetCacheReadInputTokens() != model.GetTotals().GetCacheReadInputTokens() || observed.GetTotals().GetCacheCreationInputTokens() != model.GetTotals().GetCacheCreationInputTokens() || observed.GetTotals().GetServerToolUse().GetWebSearchRequests() != model.GetTotals().GetServerToolUse().GetWebSearchRequests() {
			mismatch = append(mismatch, "model_usage."+model.GetModel())
		}
	}
	unmatched := make([]string, 0, len(responseModels))
	for model := range responseModels {
		if resultModels[model] == nil {
			unmatched = append(unmatched, model)
		}
	}
	sort.Strings(unmatched)
	for _, model := range unmatched {
		// The vendor's zero-usage `<synthetic>` pseudo-model is the one
		// stream-plane entry the result plane is contractually silent about.
		// See tokenutilization.SyntheticModelReconciliationExcluded.
		if tokenutilization.SyntheticModelReconciliationExcluded(model, responseModels[model].GetTotals()) {
			logf("session-controller: token ledger reconciliation excluded the zero-usage synthetic pseudo-model from the result-plane compare (model=%s)", model)
			continue
		}
		mismatch = append(mismatch, "model_usage."+model)
	}
	sort.Strings(mismatch)
	return reconciliationResult{reconciliation: r, mismatch: mismatch}
}

func cloneOptional[T any](value *T) *T {
	if value == nil {
		return nil
	}
	cloned := *value
	return &cloned
}

// usageWindowRolloverFloorMs is the smallest movement of the five-hour window's
// resets_at that can mean the window ROLLED OVER rather than that the vendor
// re-projected the same boundary slightly differently between two samples.
//
// WHY A FLOOR AT ALL. The check used to be raw inequality, and raw inequality
// is self-refuting on live evidence: every settled turn in the stack was
// condemned for a "reset" where the END boundary landed 693ms EARLIER than the
// start's. A window that has rolled over resets LATER, never sooner, and never
// by a fraction of a second. What the two samples actually disagreed about was
// the projection of one unchanged boundary — the vendor recomputes resets_at
// per request from its own clock, so two samples of the SAME window differ by
// request-to-request skew, on the order of milliseconds.
//
// WHY FIVE MINUTES. A genuine rollover advances the boundary by a full window,
// five hours (18,000,000ms). The jitter this must ignore is sub-second. Any
// floor strictly between those discriminates; five minutes sits three orders of
// magnitude above the noise and two orders below the signal, so neither a
// pathologically skewed projection nor a rollover shortened by vendor-side
// window accounting can cross into the wrong answer.
//
// IT IS A MAGNITUDE, NOT A DIRECTION. A forward move past the floor is the
// rollover the design already flagged. A BACKWARD move past the floor is not a
// rollover, but it is the same thing the finding exists to state: the turn's two
// boundaries were measured against windows that are not the same window, so the
// quota delta between them is not a delta. It stays a stated invalidity rather
// than being filtered out with the jitter.
const usageWindowRolloverFloorMs = 5 * 60 * 1000

// usageWindowRolledOver reports whether a turn's two five-hour-window samples
// name different windows, as opposed to the same window projected twice.
func usageWindowRolledOver(startResetsAtMs, endResetsAtMs int64) bool {
	delta := endResetsAtMs - startResetsAtMs
	if delta < 0 {
		delta = -delta
	}
	return delta >= usageWindowRolloverFloorMs
}

func missingUsageProblem(start bool) *frontendv1.TurnAccountingProblem {
	if start {
		return &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_MissingUsageBoundary{MissingUsageBoundary: &frontendv1.MissingUsageBoundary{Boundary: &frontendv1.MissingUsageBoundary_TurnStart{TurnStart: &frontendv1.MissingUsageBoundaryTurnStart{}}}}}
	}
	return &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_MissingUsageBoundary{MissingUsageBoundary: &frontendv1.MissingUsageBoundary{Boundary: &frontendv1.MissingUsageBoundary_TurnEnd{TurnEnd: &frontendv1.MissingUsageBoundaryTurnEnd{}}}}}
}
func runtimeIdentityProblem(queryID string, missing []string) *frontendv1.TurnAccountingProblem {
	paths := make([]string, len(missing))
	for i, path := range missing {
		if path == "" {
			paths[i] = fmt.Sprintf("query_lifecycle[%s].runtime_observed", queryID)
			continue
		}
		paths[i] = fmt.Sprintf("query_lifecycle[%s].runtime_observed.identity.%s", queryID, path)
	}
	return &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_RuntimeIdentityIncomplete{RuntimeIdentityIncomplete: &frontendv1.RuntimeIdentityIncomplete{MissingFieldPaths: paths}}}
}

// incompleteRuntimeIdentityPaths names every field of the query's runtime
// identity that the producer did not settle. Nothing here is an OPINION about
// whether a value is plausible — only about whether the shim answered.
//
// TWO FIELDS ARE DELIBERATELY NOT REQUIRED, each for a reason about the
// evidence rather than about the convenience of passing:
//
//   - subscription_type is not initialization evidence at all. It is reported
//     by the usage service, and this same TurnAccounting already carries it on
//     both of the turn's AccountUsageObservations. Demanding the init message
//     duplicate it condemned every turn for the absence of a fact the record
//     holds twice over.
//   - fast_mode_reason is the EXPLANATION for fast_mode_state, and a state that
//     needs no explaining has none. Requiring it unconditionally made "fast
//     mode is on and working" indistinguishable from missing evidence.
//     fast_mode_state itself stays required, so the state is never unstated.
func incompleteRuntimeIdentityPaths(runtime *corev1.QueryRuntimeIdentity) []string {
	if runtime == nil {
		return []string{""}
	}
	missing := make([]string, 0, 12)
	for _, field := range []struct {
		path  string
		value string
	}{
		{"vendor_session_id", runtime.GetVendorSessionId()},
		{"effective_model", runtime.GetEffectiveModel()},
		{"sdk_version", runtime.GetSdkVersion()},
		{"claude_code_version", runtime.GetClaudeCodeVersion()},
		{"shim_build_sha", runtime.GetShimBuildSha()},
		{"auth_source", runtime.GetAuthSource()},
		{"fast_mode_state", runtime.GetFastModeState()},
	} {
		if field.value == "" {
			missing = append(missing, field.path)
		}
	}
	for _, fingerprint := range []struct {
		path  string
		value *corev1.EvidenceFingerprint
	}{
		{"effective_options", runtime.GetEffectiveOptions()},
		{"settings", runtime.GetSettings()},
		{"tools", runtime.GetTools()},
		{"mcp", runtime.GetMcp()},
		{"context_prefix", runtime.GetContextPrefix()},
	} {
		if !evidenceFingerprintSettled(fingerprint.value) {
			missing = append(missing, fingerprint.path)
		}
	}
	return missing
}

// evidenceFingerprintSettled reports whether the producer ANSWERED for one
// fingerprint — either it hashed the evidence or it stated why it could not.
//
// The check used to accept only a digest, which made the proto's own
// `unavailable` arm unsatisfiable: a shim that correctly reported "I hold
// nothing to fingerprint, and here is the cause" was recorded as having said
// nothing at all. That is the same available/unavailable settlement
// AccountUsageObservation already uses for its boundary samples, and it is read
// the same way here. A fingerprint that is absent, carries no arm, or carries an
// empty digest or a causeless unavailability is still unsettled.
func evidenceFingerprintSettled(f *corev1.EvidenceFingerprint) bool {
	switch evidence := f.GetEvidence().(type) {
	case *corev1.EvidenceFingerprint_Sha256:
		return evidence.Sha256 != ""
	case *corev1.EvidenceFingerprint_Unavailable:
		return strings.TrimSpace(evidence.Unavailable.GetCause()) != ""
	default:
		return false
	}
}
func invalidTurnAccounting(turnID, queryID string, runtime *corev1.QueryRuntimeIdentity, settledAt int64, problem *frontendv1.TurnAccountingProblem) *frontendv1.TurnAccounting {
	return &frontendv1.TurnAccounting{TurnId: turnID, QueryInstanceId: queryID, Runtime: runtime, Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: settledAt}, Verdict: &frontendv1.TurnAccounting_Invalid{Invalid: &frontendv1.TurnAccountingInvalid{Problems: []*frontendv1.TurnAccountingProblem{problem}}}}
}
