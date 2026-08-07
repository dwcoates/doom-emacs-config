package errclass

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// capture returns a logf that records every line, so the LOUDNESS of a
// fallthrough is asserted rather than assumed.
func capture() (func(string, ...any), *[]string) {
	var lines []string
	return func(format string, args ...any) {
		lines = append(lines, fmt.Sprintf(format, args...))
	}, &lines
}

func TestSentinelClassifiesEachSentinel(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want Type
	}{
		{"shim not connected", ErrShimNotConnected, TypeShimNotConnected},
		{"shim nack", ErrShimNack, TypeShimRejected},
		{"shim ack timeout", ErrShimAckTimeout, TypeShimAckTimeout},
		{"shim version mismatch", ErrShimVersionMismatch, TypeShimVersionMismatch},
		{"shim seq regression", ErrShimSeqRegression, TypeShimSeqRegression},
		{"not live session", ErrNotLiveSession, TypeSessionNotLive},
		{"session superseded by resync", ErrSessionSuperseded, TypeSessionReconnectSuperseded},
		{"repull in flight", ErrRepullInFlight, TypeHistoryRepullInFlight},
		{"repull truncated", ErrRepullTruncated, TypeHistoryReplayTruncated},
		{"interrupt undelivered", ErrInterruptUndelivered, TypeInterruptUndelivered},
		{"no live session controller", ErrNoLiveSessionController, TypeShimNotSpawned},
		{"shim not ready", ErrShimNotReady, TypeShimHandshakeIncomplete},
		{"shim unhealthy", ErrShimUnhealthy, TypeShimUnhealthy},
		{"session not established", ErrSessionNotEstablished, TypeSessionNotEstablished},
		{"queue entry session unwired", ErrQueueEntrySessionUnwired, TypeQueueEntrySessionUnwired},
		{"client log identity stale", ErrClientLogIdentityStale, TypeClientLogIdentityStale},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the bare sentinel.
			// Act.
			got, ok := Sentinel(tc.err)
			// Assert.
			if !ok {
				t.Fatalf("Sentinel(%v) reported no match; every sentinel must classify", tc.err)
			}
			if got != tc.want {
				t.Fatalf("Sentinel(%v) = %q, want %q", tc.err, got, tc.want)
			}
		})
	}
}

func TestSentinelMatchesThroughAWrap(t *testing.T) {
	// Arrange: the shape control.go actually produces — a %w wrap carrying the
	// nack reason, which is how the sentinel reaches the classifier in
	// production.
	wrapped := fmt.Errorf("%w: request_id=%s reason=%q", ErrShimNack, "req-1", "store rejected batch")
	// Act.
	got, ok := Sentinel(wrapped)
	// Assert.
	if !ok || got != TypeShimRejected {
		t.Fatalf("Sentinel(wrapped nack) = (%q, %v), want (%q, true)", got, ok, TypeShimRejected)
	}
}

func TestSentinelReportsNoMatchForNil(t *testing.T) {
	// Arrange: no error at all.
	// Act.
	_, ok := Sentinel(nil)
	// Assert.
	if ok {
		t.Fatal("Sentinel(nil) claimed a match; a nil error is not a failure")
	}
}

func TestAssistantMapsEveryTypedMember(t *testing.T) {
	tests := []struct {
		name string
		in   datav1.AssistantMessageError
		want Type
	}{
		{"authentication failed", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_AUTHENTICATION_FAILED, TypeAPIAuthenticationFailed},
		{"billing error", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_BILLING_ERROR, TypeAPIBillingError},
		{"rate limit", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_RATE_LIMIT, TypeAPIRateLimit},
		{"invalid request", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_INVALID_REQUEST, TypeAPIInvalidRequest},
		{"server error", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_SERVER_ERROR, TypeAPIServerError},
		{"unknown", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_UNKNOWN, TypeAPIUnknown},
		{"oauth org not allowed", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_OAUTH_ORG_NOT_ALLOWED, TypeAPIOAuthOrgNotAllowed},
		{"overloaded", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_OVERLOADED, TypeAPIOverloaded},
		{"model not found", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_MODEL_NOT_FOUND, TypeAPIModelNotFound},
		{"max output tokens", datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_MAX_OUTPUT_TOKENS, TypeAPIMaxOutputTokens},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the typed SDK enum member.
			// Act.
			got, ok := Assistant(tc.in)
			// Assert.
			if !ok {
				t.Fatalf("Assistant(%v) reported no mapping", tc.in)
			}
			if got != tc.want {
				t.Fatalf("Assistant(%v) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestAssistantReportsNoMappingForUnspecified(t *testing.T) {
	// Arrange: an unset enum, which is the ABSENCE of a classification rather
	// than a classification of "unknown".
	// Act.
	_, ok := Assistant(datav1.AssistantMessageError_ASSISTANT_MESSAGE_ERROR_UNSPECIFIED)
	// Assert.
	if ok {
		t.Fatal("Assistant(UNSPECIFIED) claimed a mapping; unset is not a failure type")
	}
}

func TestAssistantCoversEveryEnumMemberOnTheWire(t *testing.T) {
	// Arrange: the generated name table is the authority on the enum's
	// membership, so a member added to the proto with no mapping here fails.
	// Act + Assert.
	for value, name := range datav1.AssistantMessageError_name {
		if value == 0 {
			continue // UNSPECIFIED is deliberately unmapped.
		}
		if _, ok := Assistant(datav1.AssistantMessageError(value)); !ok {
			t.Errorf("AssistantMessageError %s (%d) has no failure type; the vocabulary is behind the proto", name, value)
		}
	}
}

func TestCommandClassifiesASentinel(t *testing.T) {
	// Arrange.
	logf, lines := capture()
	// Act.
	got := Command(logf, ErrShimNotConnected)
	// Assert.
	if got.GetErrorType() != string(TypeShimNotConnected) {
		t.Fatalf("error_type = %q, want %q", got.GetErrorType(), TypeShimNotConnected)
	}
	if len(*lines) != 0 {
		t.Fatalf("a classified error logged %v; only the fallthrough is loud", *lines)
	}
}

func TestCommandCarriesTheRawTextInSourceDetail(t *testing.T) {
	// Arrange: the wrap shape production produces.
	logf, _ := capture()
	wrapped := fmt.Errorf("%w: reason=%q", ErrShimNack, "store rejected batch")
	// Act.
	got := Command(logf, wrapped)
	// Assert: the raw evidence survives beside the prose rather than replacing it.
	if !strings.Contains(got.GetSourceDetail(), "store rejected batch") {
		t.Fatalf("source_detail = %q, want it to carry the raw reason", got.GetSourceDetail())
	}
}

func TestCommandProseIsNotTheRawGoText(t *testing.T) {
	// Arrange.
	logf, _ := capture()
	// Act.
	got := Command(logf, ErrShimNack)
	// Assert: the headline is human prose; package-prefixed Go text belongs in
	// source_detail, which is the whole reason the two fields are separate.
	if strings.Contains(got.GetMessage(), "shimclient:") {
		t.Fatalf("message = %q, want prose rather than the sentinel's Go text", got.GetMessage())
	}
}

func TestCommandFallsThroughLoudlyForAnUnknownError(t *testing.T) {
	// Arrange: an error matching no sentinel.
	logf, lines := capture()
	// Act.
	got := Command(logf, fmt.Errorf("some handler blew up"))
	// Assert.
	if got.GetErrorType() != string(TypeInternalUnclassified) {
		t.Fatalf("error_type = %q, want %q", got.GetErrorType(), TypeInternalUnclassified)
	}
	if len(*lines) == 0 {
		t.Fatal("the unclassified fallthrough was SILENT; a silent fallthrough lets the vocabulary rot unnoticed")
	}
}

func TestCommandKeepsTheRawTextOnTheFallthrough(t *testing.T) {
	// Arrange.
	logf, _ := capture()
	// Act.
	got := Command(logf, fmt.Errorf("some handler blew up"))
	// Assert.
	if got.GetSourceDetail() != "some handler blew up" {
		t.Fatalf("source_detail = %q, want the raw text preserved", got.GetSourceDetail())
	}
}

func TestCommandReturnsNilForNoError(t *testing.T) {
	// Arrange.
	logf, _ := capture()
	// Act.
	got := Command(logf, nil)
	// Assert.
	if got != nil {
		t.Fatalf("Command(nil) = %v, want nil; a successful command has no failure", got)
	}
}

func TestCommandIsAlwaysInternalClass(t *testing.T) {
	// Arrange: a command refusal is agent-repl machinery by construction —
	// nothing about the account is implicated.
	logf, _ := capture()
	// Act.
	got := Command(logf, ErrRepullTruncated)
	// Assert.
	if got.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("class = %v, want INTERNAL", got.GetErrorClass())
	}
}

func TestInterruptErrorFailedIsAFailure(t *testing.T) {
	// Arrange.
	// Act.
	got := InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)
	// Assert.
	if !errors.Is(got, ErrInterruptUndelivered) {
		t.Fatalf("InterruptError(FAILED) = %v, want ErrInterruptUndelivered", got)
	}
}

func TestInterruptErrorFailedClassifiesAsUndelivered(t *testing.T) {
	// Arrange: the outcome routed through the ONE command door.
	logf, _ := capture()
	// Act.
	got := Command(logf, InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED))
	// Assert.
	if got.GetErrorType() != string(TypeInterruptUndelivered) {
		t.Fatalf("error_type = %q, want %q", got.GetErrorType(), TypeInterruptUndelivered)
	}
}

func TestInterruptErrorAlreadyCompleteIsQuietSuccess(t *testing.T) {
	// Arrange: the outcome that exists precisely so a stop landing on a
	// finished turn stops being painted as a failed stop.
	// Act.
	got := InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	// Assert.
	if got != nil {
		t.Fatalf("InterruptError(ALREADY_COMPLETE) = %v, want nil; the user asked for the turn to be over and it already is", got)
	}
}

func TestInterruptErrorInterruptedIsQuietSuccess(t *testing.T) {
	// Arrange.
	// Act.
	got := InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	// Assert.
	if got != nil {
		t.Fatalf("InterruptError(INTERRUPTED) = %v, want nil", got)
	}
}

func TestInterruptErrorUnspecifiedIsQuietSuccess(t *testing.T) {
	// Arrange: every non-interrupt command acks with UNSPECIFIED, so treating
	// it as a failure would fail every command in the tree.
	// Act.
	got := InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_UNSPECIFIED)
	// Assert.
	if got != nil {
		t.Fatalf("InterruptError(UNSPECIFIED) = %v, want nil", got)
	}
}

func TestAPIErrorClassifiesByStatus(t *testing.T) {
	tests := []struct {
		name   string
		status int64
		want   Type
	}{
		{"bad request", 400, TypeAPIInvalidRequest},
		{"unauthorized", 401, TypeAPIAuthenticationFailed},
		{"payment required", 402, TypeAPIBillingError},
		{"forbidden", 403, TypeAPIOAuthOrgNotAllowed},
		{"not found", 404, TypeAPIModelNotFound},
		{"unprocessable", 422, TypeAPIInvalidRequest},
		{"too many requests", 429, TypeAPIRateLimit},
		{"overloaded", 529, TypeAPIOverloaded},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: tc.status}}
			// Act.
			got := APIError(ae, "item-1")
			// Assert.
			if got.GetErrorType() != string(tc.want) {
				t.Fatalf("status %d classified %q, want %q", tc.status, got.GetErrorType(), tc.want)
			}
		})
	}
}

func TestAPIErrorClassifiesAnUnlistedServerStatusAsServerError(t *testing.T) {
	// Arrange: a 5xx outside the exact table is still the server failing.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: 503}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if got.GetErrorType() != string(TypeAPIServerError) {
		t.Fatalf("503 classified %q, want %q", got.GetErrorType(), TypeAPIServerError)
	}
}

func TestAPIErrorClassifiesANeverDeliveredRequestAsNetworkDown(t *testing.T) {
	// Arrange: no status at all, because the request never got a response.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{IsNetworkDown: true}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if got.GetErrorType() != string(TypeAPINetworkDown) {
		t.Fatalf("network-down classified %q, want %q", got.GetErrorType(), TypeAPINetworkDown)
	}
}

func TestAPIErrorFallsBackToRequestFailedWithNoEvidence(t *testing.T) {
	// Arrange: a line carrying nothing structured. The honest generic, not a
	// guess at something more specific.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Message: "boom"}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if got.GetErrorType() != string(TypeAPIRequestFailed) {
		t.Fatalf("evidence-free line classified %q, want %q", got.GetErrorType(), TypeAPIRequestFailed)
	}
}

func TestAPIErrorIsAlwaysAPIClass(t *testing.T) {
	// Arrange.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: 500}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if got.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_API {
		t.Fatalf("class = %v, want API", got.GetErrorClass())
	}
}

func TestAPIErrorCarriesTheAddressingItemUuid(t *testing.T) {
	// Arrange.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: 500}}
	// Act.
	got := APIError(ae, "item-42")
	// Assert: this is what lets a footer row scroll the feed to the card.
	if got.GetItemUuid() != "item-42" {
		t.Fatalf("item_uuid = %q, want %q", got.GetItemUuid(), "item-42")
	}
}

func TestAPIErrorReportsTheAttemptCountInTheMessage(t *testing.T) {
	// Arrange: a line whose retries are exhausted.
	ae := &datav1.ApiErrorLine{
		Error:      &datav1.ApiErrorDetail{Message: "overloaded"},
		MaxRetries: 3,
	}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if !strings.Contains(got.GetMessage(), "after 3 attempts") {
		t.Fatalf("message = %q, want the exhausted attempt count", got.GetMessage())
	}
}

func TestAPIErrorFallsBackToProseWhenTheLineCarriedNoMessage(t *testing.T) {
	// Arrange: a typed status but no human text.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: 429}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if got.GetMessage() != prose[TypeAPIRateLimit] {
		t.Fatalf("message = %q, want the rate-limit prose", got.GetMessage())
	}
}

func TestAPIErrorPutsTheStructuredEvidenceInSourceDetail(t *testing.T) {
	// Arrange.
	ae := &datav1.ApiErrorLine{Error: &datav1.ApiErrorDetail{Status: 500, RequestId: "req-9"}}
	// Act.
	got := APIError(ae, "item-1")
	// Assert.
	if !strings.Contains(got.GetSourceDetail(), "request_id=req-9") {
		t.Fatalf("source_detail = %q, want the request id", got.GetSourceDetail())
	}
}

func TestTurnEndClassifiesEachBlockingStopReason(t *testing.T) {
	tests := []struct {
		name string
		stop string
		want Type
	}{
		{"max turns", "error_max_turns", TypeAPIMaxTurns},
		{"max budget", "error_max_budget", TypeAPIMaxBudget},
		{"during execution", "error_during_execution", TypeAPIExecutionError},
		{"refusal", "refusal", TypeAPIRefusal},
		{"authentication failed", "authentication_failed", TypeAPIAuthenticationFailed},
		{"billing error", "billing_error", TypeAPIBillingError},
		{"invalid request", "invalid_request", TypeAPIInvalidRequest},
		{"server error", "server_error", TypeAPIServerError},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			te := &corev1.TurnEnded{StopReason: tc.stop}
			// Act.
			got := TurnEnd(te)
			// Assert.
			if got == nil {
				t.Fatalf("TurnEnd(%q) = nil; the SSM blocks on it, so it must have a name", tc.stop)
			}
			if got.GetErrorType() != string(tc.want) {
				t.Fatalf("TurnEnd(%q) = %q, want %q", tc.stop, got.GetErrorType(), tc.want)
			}
		})
	}
}

func TestTurnEndReturnsNilForACleanConclusion(t *testing.T) {
	// Arrange.
	te := &corev1.TurnEnded{StopReason: "end_turn"}
	// Act.
	got := TurnEnd(te)
	// Assert.
	if got != nil {
		t.Fatalf("TurnEnd(clean) = %v, want nil", got)
	}
}

func TestTurnEndReturnsNilForAUserInterrupt(t *testing.T) {
	// Arrange: `aborted` is a conclusion the user themselves asked for, so it
	// is not a failure — the same carve-out the SSM makes.
	te := &corev1.TurnEnded{StopReason: "aborted", IsError: true}
	// Act.
	got := TurnEnd(te)
	// Assert.
	if got != nil {
		t.Fatalf("TurnEnd(aborted) = %v, want nil", got)
	}
}

func TestTurnEndClassifiesAnUnrecognizedErrorEndAsTurnFailed(t *testing.T) {
	// Arrange: an is_error end whose reason is not in the known family. The
	// SSM still blocks on it, so it must still get a name.
	te := &corev1.TurnEnded{StopReason: "something_new", IsError: true}
	// Act.
	got := TurnEnd(te)
	// Assert.
	if got == nil || got.GetErrorType() != string(TypeAPITurnFailed) {
		t.Fatalf("TurnEnd(unknown error end) = %v, want %q", got, TypeAPITurnFailed)
	}
}

func TestTurnEndKeepsTheStopReasonAsEvidence(t *testing.T) {
	// Arrange.
	te := &corev1.TurnEnded{StopReason: "refusal"}
	// Act.
	got := TurnEnd(te)
	// Assert.
	if !strings.Contains(got.GetSourceDetail(), "refusal") {
		t.Fatalf("source_detail = %q, want the raw stop reason", got.GetSourceDetail())
	}
}

func TestDeathClassifiesEachPersistedLiteral(t *testing.T) {
	tests := []struct {
		name   string
		reason string
		want   Type
	}{
		{"deleted", "delete session", TypeSessionDeleted},
		{"superseded", "superseded", TypeSessionSuperseded},
		{"shim died", "shim_died", TypeSessionShimDied},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			logf, lines := capture()
			// Act.
			got := Death(logf, "s_1", tc.reason, 0)
			// Assert.
			if got.GetErrorType() != string(tc.want) {
				t.Fatalf("Death(%q) = %q, want %q", tc.reason, got.GetErrorType(), tc.want)
			}
			if len(*lines) != 0 {
				t.Fatalf("a known death reason logged %v; only the unknown default is loud", *lines)
			}
		})
	}
}

func TestDeathClassifiesALegacyStringLoudly(t *testing.T) {
	// Arrange: a registry carried over from a build that predates the
	// vocabulary may hold anything.
	logf, lines := capture()
	// Act.
	got := Death(logf, "s_1", "some ancient reason", 0)
	// Assert.
	if got.GetErrorType() != string(TypeSessionEndedUnclassified) {
		t.Fatalf("error_type = %q, want %q", got.GetErrorType(), TypeSessionEndedUnclassified)
	}
	if len(*lines) == 0 {
		t.Fatal("an unknown death reason passed through SILENTLY; the default must be loud")
	}
}

func TestDeathKeepsALegacyStringVerbatim(t *testing.T) {
	// Arrange.
	logf, _ := capture()
	// Act.
	got := Death(logf, "s_1", "some ancient reason", 0)
	// Assert: the raw value is preserved rather than guessed at.
	if got.GetSourceDetail() != "some ancient reason" {
		t.Fatalf("source_detail = %q, want the raw string", got.GetSourceDetail())
	}
}

func TestDeathCarriesTheResolutionItWasGiven(t *testing.T) {
	// Arrange: a supersede whose successor is up is no longer true, and the
	// item is re-derived on every push, so it must carry its own close.
	logf, _ := capture()
	// Act.
	got := Death(logf, "s_1", DeathReasonSuperseded, 4242)
	// Assert.
	if got.GetResolvedAtMs() != 4242 {
		t.Fatalf("resolved_at_ms = %d, want 4242", got.GetResolvedAtMs())
	}
}

func TestDeathIsOpenWithoutAResolution(t *testing.T) {
	// Arrange: a supersede that just happened has not been closed by anything.
	logf, _ := capture()
	// Act.
	got := Death(logf, "s_1", DeathReasonSuperseded, 0)
	// Assert.
	if got.GetResolvedAtMs() != 0 {
		t.Fatalf("resolved_at_ms = %d, want 0", got.GetResolvedAtMs())
	}
}

func TestDeathKeysTheCardOnTheSession(t *testing.T) {
	// Arrange: a resolved re-push must SETTLE the open card rather than land
	// beside it, which needs a stable per-session item uuid.
	logf, _ := capture()
	// Act.
	open := Death(logf, "s_1", DeathReasonSuperseded, 0)
	settled := Death(logf, "s_1", DeathReasonSuperseded, 4242)
	// Assert.
	if open.GetItemUuid() != settled.GetItemUuid() || open.GetItemUuid() == "" {
		t.Fatalf("open uuid %q, settled uuid %q; want one non-empty shared identity",
			open.GetItemUuid(), settled.GetItemUuid())
	}
}

func TestDeathGivesTwoSessionsDistinctCards(t *testing.T) {
	// Arrange: two sessions of one workspace both die; neither may settle the
	// other's card.
	logf, _ := capture()
	// Act.
	first := Death(logf, "s_1", DeathReasonSuperseded, 0)
	second := Death(logf, "s_2", DeathReasonSuperseded, 0)
	// Assert.
	if first.GetItemUuid() == second.GetItemUuid() {
		t.Fatalf("both sessions share item uuid %q", first.GetItemUuid())
	}
}

func TestDeathReturnsNilForNoReason(t *testing.T) {
	// Arrange: a session with no recorded reason has not died.
	logf, _ := capture()
	// Act.
	got := Death(logf, "s_1", "", 0)
	// Assert.
	if got != nil {
		t.Fatalf("Death(\"\") = %v, want nil", got)
	}
}

func TestDegradedKeepsTheDroppedCount(t *testing.T) {
	// Arrange: how much conversation was lost is the most useful fact about a
	// store outage, and the old passthrough discarded it.
	// Act.
	got := Degraded("shim-store", "store rejected batch", 17)
	// Assert.
	if !strings.Contains(got.GetSourceDetail(), "dropped=17") {
		t.Fatalf("source_detail = %q, want the dropped count", got.GetSourceDetail())
	}
}

func TestDegradedNamesTheComponent(t *testing.T) {
	// Arrange.
	// Act.
	got := Degraded("shim-store", "store rejected batch", 0)
	// Assert.
	if !strings.Contains(got.GetSourceDetail(), "component=shim-store") {
		t.Fatalf("source_detail = %q, want the component", got.GetSourceDetail())
	}
}

func TestDegradedIsInternalClass(t *testing.T) {
	// Arrange: a store outage is agent-repl's own machinery.
	// Act.
	got := Degraded("shim-store", "boom", 0)
	// Assert.
	if got.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("class = %v, want INTERNAL", got.GetErrorClass())
	}
}

func TestConnectionDegradedCarriesTheHeartbeatReason(t *testing.T) {
	// Arrange.
	// Act.
	got := ConnectionDegraded("no shim traffic for 30s (>20s window)")
	// Assert.
	if got.GetSourceDetail() != "no shim traffic for 30s (>20s window)" {
		t.Fatalf("source_detail = %q, want the raw window reason", got.GetSourceDetail())
	}
}

func TestConnectionDegradedIsTheShimDegradedType(t *testing.T) {
	// Arrange.
	// Act.
	got := ConnectionDegraded("no traffic")
	// Assert.
	if got.GetErrorType() != string(TypeShimDegraded) {
		t.Fatalf("error_type = %q, want %q", got.GetErrorType(), TypeShimDegraded)
	}
}

func TestNoDaemonTypeCarriesTheClientPrefix(t *testing.T) {
	// Arrange: the namespace partition, made a test failure rather than a
	// review miss.
	// Act + Assert.
	for _, typ := range AllTypes() {
		if !IsDaemonType(string(typ)) {
			t.Errorf("daemon type %q carries the frontend-reserved %q prefix", typ, ClientPrefix)
		}
	}
}

func TestIsDaemonTypeRejectsAClientType(t *testing.T) {
	// Arrange: a type a FRONTEND would mint.
	// Act.
	got := IsDaemonType("client.daemon_unreachable")
	// Assert.
	if got {
		t.Fatal("IsDaemonType accepted a client.-prefixed type; the partition must be detectable by inspection")
	}
}

func TestEveryTypeHasProse(t *testing.T) {
	// Arrange: a type with no sentence renders as an empty card, which is a
	// silent failure wearing a card's clothes.
	// Act + Assert.
	for _, typ := range AllTypes() {
		if prose[typ] == "" {
			t.Errorf("type %q has no prose; it would render as an empty card", typ)
		}
	}
}

func TestStaleSessionIdentityProseNamesTheReloadCommand(t *testing.T) {
	// Arrange: the card tells the reader to resync, and the webapp ships no
	// control that does it — so the sentence has to name the Emacs command
	// that does, or it is an instruction with no way to follow it.
	want := "agent-repl-frontend-reload-webview"

	// Act.
	got := prose[TypeSessionReconnectSuperseded]

	// Assert.
	if !strings.Contains(got, want) {
		t.Fatalf("stale-identity prose = %q, want it to name %q", got, want)
	}
}

func TestProseHasNoRowsOutsideTheVocabulary(t *testing.T) {
	// Arrange: the other direction — a sentence for a type that no longer
	// exists is dead weight that reads as coverage.
	known := map[Type]bool{}
	for _, typ := range AllTypes() {
		known[typ] = true
	}
	// Act + Assert.
	for typ := range prose {
		if !known[typ] {
			t.Errorf("prose carries %q, which is not in the vocabulary", typ)
		}
	}
}
