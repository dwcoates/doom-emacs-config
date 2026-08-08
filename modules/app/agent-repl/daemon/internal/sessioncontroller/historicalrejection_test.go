package sessioncontroller

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
)

// The defect these tests pin: a durable row written by a RETIRED query carried a
// TokenUtilization with a blank model (the producer was fixed long ago, the row
// is poisoned forever). Every bring-up replays it, and every bring-up re-emitted
// the same three warn records — one of them claiming historical=false about a
// row no live query had ever touched, because the log's classifier
// short-circuited on the absent AccountUsageObservation payload instead of
// asking the envelope.

// rejectingConsumer is a consumer bound to the live query with the info and warn
// channels split, which is the shape a resuming daemon has while the store
// replays rows at it.
func rejectingConsumer(t *testing.T, logs *levelSplitLogs) *consumer {
	t.Helper()
	c := degradedAccountingConsumer(logs)
	if err := c.accounting.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId: "live-query",
		QueryCreatedSeq: 100,
		VendorSessionId: "vendor-session",
	}); err != nil {
		t.Fatalf("bind handshake: %v", err)
	}
	c.accounting.activeTurnID, c.accounting.turns["t"] = "t", &accountingTurn{}
	return c
}

// blankModelUtilizationEvent is a stream response whose token utilization names
// no model, stamped as produced by ENVELOPEQUERY. It is the seq-182 row's shape.
func blankModelUtilizationEvent(t *testing.T, envelopeQuery string) *corev1.Event {
	t.Helper()
	ev := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{
		Id:      "c18f98fa",
		Model:   "",
		Usage:   &datav1.ApiUsage{InputTokens: 1},
		Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "response"}}}},
	}}}})
	ev.Seq, ev.Plane = 182, corev1.Plane_PLANE_STREAM
	ev.QueryInstanceId = envelopeQuery
	return ev
}

// applyBlankModelUtilization drives one rejection through Apply and asserts only
// that the event itself survived it, which every case below depends on.
func applyBlankModelUtilization(t *testing.T, c *consumer, envelopeQuery string) {
	t.Helper()
	if err := c.Apply(blankModelUtilizationEvent(t, envelopeQuery)); err != nil {
		t.Fatalf("Apply error = %v, want the event still applied despite the rejected utilization", err)
	}
}

// --- the classifier no longer short-circuits on a missing payload arm ---------

func TestRejectionIsHistoricalReadsTheEnvelopeWithNoObservationPayload(t *testing.T) {
	tests := []struct {
		name          string
		envelopeQuery string
		want          bool
		why           string
	}{
		{
			name:          "retired query produced the utilization row",
			envelopeQuery: "retired-query",
			want:          true,
			why:           "the row's epoch is a property of its envelope, and no payload arm can make a replayed row live",
		},
		{
			name:          "live query produced the utilization row",
			envelopeQuery: "live-query",
			want:          false,
			why:           "a row the bound query wrote is live, and its rejection is news",
		},
		{
			name:          "no producer stamp",
			envelopeQuery: "",
			want:          false,
			why:           "empty fails closed exactly as it does for every other event type",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange -- a utilization-only event: no AccountUsageObservation at all.
			r := boundReducer(t)
			ev := &corev1.Event{Seq: 182, QueryInstanceId: tt.envelopeQuery}

			// Act
			got := rejectionIsHistorical(r, ev)

			// Assert
			if got != tt.want {
				t.Fatalf("rejectionIsHistorical = %v, want %v: %s", got, tt.want, tt.why)
			}
		})
	}
}

// --- the replayed row: full record, withheld severity ------------------------

func TestReplayedUtilizationRejectionLeavesTheWarnChannelUntouched(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- re-alarming about a durable row on every boot is the whole defect.
	if strings.Contains(strings.Join(logs.warn, "\n"), "token utilization REJECTED") {
		t.Fatalf("warn = %v, want the replayed utilization rejection off the warn channel", logs.warn)
	}
}

func TestReplayedUtilizationRejectionKeepsItsFullIdentityAtInfo(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- demotion moves the severity and nothing else.
	log := strings.Join(logs.info, "\n")
	for _, field := range []string{
		"token utilization REJECTED before mutation",
		`field_path="TokenUtilization.model"`,
		`api_message_id="c18f98fa"`,
		`model=""`,
		"source_plane=PLANE_STREAM",
		"seq=182",
	} {
		if !strings.Contains(log, field) {
			t.Fatalf("info = %q, want field %q retained on the withheld record", log, field)
		}
	}
}

func TestReplayedUtilizationRejectionNamesTheWithheldWarnBranch(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- a reader must be able to tell a demoted record from a routine one.
	if !strings.Contains(strings.Join(logs.info, "\n"), "decision="+historicalRejectionDecision) {
		t.Fatalf("info = %v, want the withheld-warn branch named", logs.info)
	}
}

func TestReplayedObservationRejectionLeavesTheWarnChannelUntouched(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert
	if strings.Contains(strings.Join(logs.warn, "\n"), "turn accounting observation REJECTED") {
		t.Fatalf("warn = %v, want the replayed observation rejection off the warn channel", logs.warn)
	}
}

func TestReplayedObservationRejectionReportsHistoricalTruthfully(t *testing.T) {
	// Arrange -- THE LYING FIELD: this record printed historical=false for a row
	// stamped by a retired query, because it asked about the absent payload arm
	// rather than the envelope.
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert
	log := strings.Join(logs.info, "\n")
	if !strings.Contains(log, "turn accounting observation REJECTED") || !strings.Contains(log, "historical=true") {
		t.Fatalf("info = %q, want the replayed row reported as history", log)
	}
}

func TestReplayedObservationRejectionStillNamesTheProducingQuery(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- the two ids are the whole basis of the verdict above.
	log := strings.Join(logs.info, "\n")
	if !strings.Contains(log, `event_query_instance_id="retired-query"`) || !strings.Contains(log, `authoritative_query_instance_id="live-query"`) {
		t.Fatalf("info = %q, want both query identities retained on the withheld record", log)
	}
}

// --- the live row: byte-identical to what it always was ----------------------

func TestLiveUtilizationRejectionStillTakesTheWarnChannel(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "live-query")

	// Assert
	if !strings.Contains(strings.Join(logs.warn, "\n"), "token utilization REJECTED before mutation") {
		t.Fatalf("warn = %v, want a live utilization rejection still at warn", logs.warn)
	}
}

func TestLiveObservationRejectionStillTakesTheWarnChannel(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "live-query")

	// Assert
	if !strings.Contains(strings.Join(logs.warn, "\n"), "turn accounting observation REJECTED before mutation") {
		t.Fatalf("warn = %v, want a live observation rejection still at warn", logs.warn)
	}
}

func TestLiveRejectionCarriesNoWithheldWarnDecision(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "live-query")

	// Assert -- the live arm is unchanged, so it gained no new field.
	if strings.Contains(strings.Join(logs.warn, "\n"), "decision="+historicalRejectionDecision) {
		t.Fatalf("warn = %v, want the live record byte-identical to its previous shape", logs.warn)
	}
}

// --- the degradation itself is unchanged on both arms ------------------------

func TestReplayedRejectionStillDegradesAccounting(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- demotion is a severity decision, never a swallow.
	if !strings.Contains(strings.Join(logs.info, "\n"), "ACCOUNTING DEGRADED") {
		t.Fatalf("info = %v, want the replayed row's degradation still recorded in full", logs.info)
	}
}

func TestReplayedRejectionStillWithholdsTheUtilizationFromTheLedger(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "retired-query")

	// Assert -- the evidence remains unavailable exactly as before.
	if got := len(c.accounting.turns["t"].responses); got != 0 {
		t.Fatalf("responses = %d, want the invalid utilization still kept out of the ledger", got)
	}
}

func TestLiveRejectionDegradationStillTakesTheWarnChannel(t *testing.T) {
	// Arrange
	logs := &levelSplitLogs{}
	c := rejectingConsumer(t, logs)

	// Act
	applyBlankModelUtilization(t, c, "live-query")

	// Assert
	if !strings.Contains(strings.Join(logs.warn, "\n"), "ACCOUNTING DEGRADED") {
		t.Fatalf("warn = %v, want a live degradation still at warn", logs.warn)
	}
}
