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

func newTurnAccountingReducer() *turnAccountingReducer {
	return &turnAccountingReducer{turns: map[string]*accountingTurn{}, latencies: map[string]responseLatency{}, knownVendorSessionIDs: map[string]struct{}{}}
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
		}
	}
	if latency := ev.GetMessageLatency(); latency != nil && latency.GetUuid() != "" {
		r.latencies[latency.GetUuid()] = responseLatency{ttftMs: latency.GetTtftMs(), messageStartedAtMs: ev.GetProducedAtMs()}
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
	if turn.startUsage != nil && turn.endUsage != nil && turn.startUsage.GetAvailable() != nil && turn.endUsage.GetAvailable() != nil && turn.startUsage.GetAvailable().GetFiveHour().GetResetsAtMs() != turn.endUsage.GetAvailable().GetFiveHour().GetResetsAtMs() {
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_WindowReset{WindowReset: &frontendv1.UsageWindowReset{StartResetsAtMs: turn.startUsage.GetAvailable().GetFiveHour().GetResetsAtMs(), EndResetsAtMs: turn.endUsage.GetAvailable().GetFiveHour().GetResetsAtMs()}}})
	}
	if missing := incompleteRuntimeIdentityPaths(r.runtime); len(missing) > 0 {
		problems = append(problems, runtimeIdentityProblem(r.queryID, missing))
	}
	if turn.result == nil {
		problems = append(problems, &frontendv1.TurnAccountingProblem{Problem: &frontendv1.TurnAccountingProblem_TelemetryRecordMissing{TelemetryRecordMissing: &frontendv1.TelemetryRecordMissing{Record: &frontendv1.TelemetryRecordMissing_PersistenceReceipt{PersistenceReceipt: &frontendv1.TelemetryRecordMissingPersistenceReceipt{TurnId: turnID}}}}})
	}
	reconciliation := reconcileTokenUsage(turn.responses, turn.result)
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

// commitResolved retires reducer memory only after the durable terminal
// accounting transaction succeeds. A persistence failure therefore leaves the
// complete turn available for the replayed TurnEnded to resolve again.
func (r *turnAccountingReducer) commitResolved(turnID string) {
	if r.turns[turnID] == nil {
		panic(fmt.Sprintf("turn accounting commit names unknown turn %q", turnID))
	}
	delete(r.turns, turnID)
	if r.activeTurnID == turnID {
		r.activeTurnID = ""
	}
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
	record := &frontendv1.TokenUtilization{AgentReplSessionId: daemonSessionID, ClaudeSessionId: assistant.GetSessionId(), ApiMessageId: assistant.GetMessage().GetId(), Model: assistant.GetMessage().GetModel(), Usage: tokenUsageFromAPI(u, assistant.GetCacheRates())}
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

func tokenUsageFromAPI(u *datav1.ApiUsage, rates *datav1.PromptCacheRates) *frontendv1.TokenUsage {
	usage := &frontendv1.TokenUsage{InputTokens: u.GetInputTokens(), OutputTokens: u.GetOutputTokens(), CacheReadInputTokens: u.GetCacheReadInputTokens(), CacheCreationInputTokens: u.GetCacheCreationInputTokens(), ServiceTier: u.GetServiceTier(), Speed: u.GetSpeed(), InferenceGeo: u.GetInferenceGeo(), RawUsage: proto.Clone(u).(*datav1.ApiUsage), CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: structInt(u.GetCacheCreation(), "ephemeral_5m_input_tokens"), Ephemeral_1HInputTokens: structInt(u.GetCacheCreation(), "ephemeral_1h_input_tokens")}, ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: structInt(u.GetServerToolUse(), "web_search_requests"), WebFetchRequests: structInt(u.GetServerToolUse(), "web_fetch_requests")}}
	if rates != nil {
		usage.CacheRates = &frontendv1.TokenCacheRates{TotalPromptInputTokens: rates.GetTotalPromptInputTokens(), CacheHitRate: rates.GetCacheHitRate(), CacheWriteRate: rates.GetCacheWriteRate(), UncachedInputRate: rates.GetUncachedInputRate()}
	} else {
		usage.CacheRates = frontend.CacheRatesFromCounters(u.GetInputTokens(), u.GetCacheReadInputTokens(), u.GetCacheCreationInputTokens())
	}
	usage.Iterations = tokenIterations(u.GetIterations())
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
		s := value.GetStructValue()
		if s == nil {
			out.Fields[fmt.Sprintf("iterations.%d", index)] = proto.Clone(value).(*structpb.Value)
			continue
		}
		kind := s.GetFields()["type"].GetStringValue()
		if kind != "" && kind != "sampling" && kind != "compaction" && kind != "advisor" && kind != "fallback" {
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

func tokenIterations(values *structpb.ListValue) []*frontendv1.TokenUsageIteration {
	if values == nil {
		return nil
	}
	out := make([]*frontendv1.TokenUsageIteration, 0, len(values.GetValues()))
	for _, value := range values.GetValues() {
		s := value.GetStructValue()
		if s == nil {
			continue
		}
		base := func() (int64, int64, int64, int64, *frontendv1.TokenCacheCreation) {
			return structInt(s, "input_tokens"), structInt(s, "output_tokens"), structInt(s, "cache_read_input_tokens"), structInt(s, "cache_creation_input_tokens"), &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: structInt(s.GetFields()["cache_creation"].GetStructValue(), "ephemeral_5m_input_tokens"), Ephemeral_1HInputTokens: structInt(s.GetFields()["cache_creation"].GetStructValue(), "ephemeral_1h_input_tokens")}
		}
		i, o, r, c, creation := base()
		switch s.GetFields()["type"].GetStringValue() {
		case "compaction":
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Compaction{Compaction: &frontendv1.TokenUsageIterationCompaction{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation}}})
		case "advisor":
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Advisor{Advisor: &frontendv1.TokenUsageIterationAdvisor{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: s.GetFields()["model"].GetStringValue()}}})
		case "fallback":
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Fallback{Fallback: &frontendv1.TokenUsageIterationFallback{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: s.GetFields()["model"].GetStringValue()}}})
		case "", "sampling":
			out = append(out, &frontendv1.TokenUsageIteration{Iteration: &frontendv1.TokenUsageIteration_Sampling{Sampling: &frontendv1.TokenUsageIterationSampling{InputTokens: i, OutputTokens: o, CacheReadInputTokens: r, CacheCreationInputTokens: c, CacheCreation: creation, Model: s.GetFields()["model"].GetStringValue()}}})
		default:
			continue
		}
	}
	return out
}

type reconciliationResult struct {
	reconciliation *frontendv1.TokenUsageReconciliation
	mismatch       []string
}

func reconcileTokenUsage(records []*frontendv1.TokenUtilization, result *datav1.ResultMessage) reconciliationResult {
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
	for model := range responseModels {
		if resultModels[model] == nil {
			mismatch = append(mismatch, "model_usage."+model)
		}
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

func incompleteRuntimeIdentityPaths(runtime *corev1.QueryRuntimeIdentity) []string {
	if runtime == nil {
		return []string{""}
	}
	missing := make([]string, 0, 14)
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
		{"subscription_type", runtime.GetSubscriptionType()},
		{"fast_mode_state", runtime.GetFastModeState()},
		{"fast_mode_reason", runtime.GetFastModeReason()},
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
		if fingerprint.value == nil || fingerprint.value.GetSha256() == "" {
			missing = append(missing, fingerprint.path)
		}
	}
	return missing
}
func invalidTurnAccounting(turnID, queryID string, runtime *corev1.QueryRuntimeIdentity, settledAt int64, problem *frontendv1.TurnAccountingProblem) *frontendv1.TurnAccounting {
	return &frontendv1.TurnAccounting{TurnId: turnID, QueryInstanceId: queryID, Runtime: runtime, Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: settledAt}, Verdict: &frontendv1.TurnAccounting_Invalid{Invalid: &frontendv1.TurnAccountingInvalid{Problems: []*frontendv1.TurnAccountingProblem{problem}}}}
}
