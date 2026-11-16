package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// summarizerRefusal is SummarizeConversation refused summarizer_failed with
// LABEL and DETAIL.
func summarizerRefusal(label *shimv1.StartSessionVendorStartFailed, detail string) *shimv1.SummarizeConversationResponse {
	return &shimv1.SummarizeConversationResponse{
		Result: &shimv1.SummarizeConversationResponse_Failure{Failure: &shimv1.SummarizeConversationFailure{
			Cause:  &shimv1.SummarizeConversationFailure_SummarizerFailed{SummarizerFailed: label},
			Detail: detail,
		}},
	}
}

// compactStart is the StartSession of `vendor-1` whose remediation is the
// cold gate's compaction on Haiku, with one rolled-back turn.
func compactStart() *shimv1.StartSessionRequest {
	return &shimv1.StartSessionRequest{
		WorkspaceName: "w1",
		Profile:       &shimv1.SessionProfile{Kind: &shimv1.SessionProfile_Chat{Chat: &shimv1.SessionProfileChat{}}},
		Source: &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{
			VendorSessionId: "vendor-1",
			ColdRemediation: &conversationv1.SessionColdRemediation{
				Remediation: &conversationv1.SessionColdRemediation_Compact{Compact: wantSummarizer},
			},
			RolledBackTurns: []*conversationv1.TurnId{{Value: "turn-2"}},
		}},
	}
}

// summaryLabel answers the startLabel ERR is, failing the test when it is
// none.
func summaryLabel(t *testing.T, err error) *startLabel {
	t.Helper()
	var label *startLabel
	if !errors.As(err, &label) {
		t.Fatalf("err = %v, want a labeled start failure", err)
	}
	return label
}

func TestSummarizeBeforeStartAsksTheResumesCompaction(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	req := compactStart()

	// Act.
	summary, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, req, nil)

	// Assert.
	if err != nil || summary != "" {
		t.Fatalf("summarizeBeforeStart = (%q, %v), want no carried summary and no error", summary, err)
	}
	want := &shimv1.SummarizeConversationRequest{
		WorkspaceName: "w1",
		Profile:       req.GetProfile(),
		Summarizer:    wantSummarizer,
		Use: &shimv1.SummarizeConversationRequest_Compact{Compact: &shimv1.SummarizeConversationCompact{
			VendorSessionId: "vendor-1",
			RolledBackTurns: []*conversationv1.TurnId{{Value: "turn-2"}},
		}},
	}
	if len(f.client.summaries) != 1 || !proto.Equal(f.client.summaries[0], want) {
		t.Fatalf("SummarizeConversation requests = %v, want exactly %v", f.client.summaries, want)
	}
}

func TestSummarizeBeforeStartAsksNothingOfAStartThatOwesNoSummary(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	req := compactStart()
	req.GetResume().ColdRemediation = payRemediation()

	// Act.
	summary, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, req, &wsm.OwedStartWork{})

	// Assert.
	if err != nil || summary != "" || len(f.client.summaries) != 0 {
		t.Fatalf("summarizeBeforeStart = (%q, %v) asking %v, want nothing asked", summary, err, f.client.summaries)
	}
}

func TestSummarizeBeforeStartLogsTheRequestedModel(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)

	// Act.
	if _, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, compactStart(), nil); err != nil {
		t.Fatalf("summarizeBeforeStart: %v", err)
	}

	// Assert.
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opSummarize,
		Message: "asking the shim for the summary before the start"})
	if !ok || record.Context["requested_model"] != wantSummarizer.GetModel().GetName() || record.Context["use"] != "compact" {
		t.Fatalf("record = %+v (found %v), want the requested model and the use", record, ok)
	}
}

func TestSummarizeBeforeStartRefusesAnEmptyCarriedSummary(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.summarizeResponse = &shimv1.SummarizeConversationResponse{Result: &shimv1.SummarizeConversationResponse_Success{
		Success: &shimv1.SummarizeConversationSuccess{Use: &shimv1.SummarizeConversationSuccess_Carried{Carried: &shimv1.SummarizeConversationCarried{}}},
	}}
	req := compactStart()
	req.GetResume().ColdRemediation = nil

	// Act.
	_, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, req, &wsm.OwedStartWork{SummaryOf: "queue-1"})

	// Assert.
	if err == nil {
		t.Fatal("summarizeBeforeStart = nil, want the empty summary refused")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opSummarize,
		Message: "the shim answered the carried summary without one; the session was not started"}); !ok {
		t.Fatalf("records = %+v, want the invariant violation at ERROR", f.log.logger.Records())
	}
}

// TestSummarizeLabelsEachFailure pins how every way the summary can fail is
// labeled for the vendor-start run: the shim's own retry verdict, always
// marked as the compaction, and the daemon's defects unlabeled.
func TestSummarizeLabelsEachFailure(t *testing.T) {
	tests := []struct {
		name          string
		response      *shimv1.SummarizeConversationResponse
		callErr       error
		wantLabeled   bool
		wantRetryable bool
		wantNetwork   bool
		wantMessage   string
	}{
		{
			name:          "a summarizer the vendor failed transiently is retryable",
			response:      summarizerRefusal(retryableVendorStart(), "overloaded"),
			wantLabeled:   true,
			wantRetryable: true,
			wantMessage:   "the shim's summarizer did not complete; the start is refused",
		},
		{
			name:          "a summarizer the network failed is retryable on the network",
			response:      summarizerRefusal(offlineVendorStart(), "getaddrinfo ENOTFOUND"),
			wantLabeled:   true,
			wantRetryable: true,
			wantNetwork:   true,
			wantMessage:   "the shim's summarizer did not complete; the start is refused",
		},
		{
			name:        "a summarizer the vendor rejected is not retried",
			response:    summarizerRefusal(rejectedVendorStart(), "prompt is too long"),
			wantLabeled: true,
			wantMessage: "the shim's summarizer did not complete; the start is refused",
		},
		{
			name: "a conversation with no transcript is not retried",
			response: &shimv1.SummarizeConversationResponse{Result: &shimv1.SummarizeConversationResponse_Failure{Failure: &shimv1.SummarizeConversationFailure{
				Cause: &shimv1.SummarizeConversationFailure_UnknownSession{UnknownSession: &shimv1.StartSessionUnknownSession{}}, Detail: "no transcript"}}},
			wantLabeled: true,
			wantMessage: "the conversation to summarize has no transcript; the start is refused",
		},
		{
			name: "a shim that already started is this daemon's defect",
			response: &shimv1.SummarizeConversationResponse{Result: &shimv1.SummarizeConversationResponse_Failure{Failure: &shimv1.SummarizeConversationFailure{
				Cause: &shimv1.SummarizeConversationFailure_AlreadyStarted{AlreadyStarted: &shimv1.StartSessionAlreadyStarted{}}, Detail: "started"}}},
			wantMessage: "the shim refused the summary already_started; this daemon asked a started shim",
		},
		{
			name:        "a refusal naming no cause is the shim's defect",
			response:    &shimv1.SummarizeConversationResponse{Result: &shimv1.SummarizeConversationResponse_Failure{Failure: &shimv1.SummarizeConversationFailure{Detail: "?"}}},
			wantMessage: "the shim's SummarizeConversation refusal sets no cause",
		},
		{
			name:        "an answer setting no result is the shim's defect",
			response:    &shimv1.SummarizeConversationResponse{},
			wantMessage: "the shim's SummarizeConversation answer sets no result",
		},
		{
			name:        "a call that failed in transport",
			callErr:     errFake,
			wantMessage: "the SummarizeConversation call failed",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			f.client.summarizeResponse = tt.response
			f.client.summarizeErr = tt.callErr

			// Act.
			_, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, compactStart(), nil)

			// Assert.
			if err == nil {
				t.Fatal("summarizeBeforeStart = nil, want the failure")
			}
			var label *startLabel
			if errors.As(err, &label) != tt.wantLabeled {
				t.Fatalf("err = %v labeled %v, want labeled %v", err, label != nil, tt.wantLabeled)
			}
			if tt.wantLabeled && (!label.compaction || label.retryable != tt.wantRetryable || label.network != tt.wantNetwork) {
				t.Fatalf("label = %+v, want compaction, retryable %v, network %v", label, tt.wantRetryable, tt.wantNetwork)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Message: tt.wantMessage}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.wantMessage)
			}
		})
	}
}

// TestSummarizeBoundFires pins the summary's own bound: a summarizer that
// does not answer inside it fails the start loudly as a compaction that is
// not retried, naming the bound and the time it was sized from.
func TestSummarizeBoundFires(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.fleet.summarizeBound = 10 * time.Millisecond
	f.client.summarizeHold = make(chan struct{})

	// Act.
	_, err := f.fleet.summarizeBeforeStart(context.Background(), f.log.logger, ws, f.client, compactStart(), nil)

	// Assert.
	label := summaryLabel(t, err)
	if !label.compaction || label.retryable || !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("label = %+v (err %v), want a compaction failure that is not retried and wraps the deadline", label, err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opSummarize,
		Message: "the shim's summarizer did not answer inside its bound; the start is refused and not retried"})
	if !ok || record.Context["bound_ms"] != int64(10) || record.Context["expected_ms"] != summarizeExpected.Milliseconds() {
		t.Fatalf("record = %+v (found %v), want the bound and the expected time", record, ok)
	}
}

// TestAStartSummarizesBeforeItsStartSession pins the order on a whole
// bring-up: the compaction is asked before the StartSession, and a
// compaction that fails sends no StartSession at all.
func TestAStartSummarizesBeforeItsStartSession(t *testing.T) {
	tests := []struct {
		name       string
		summarize  *shimv1.SummarizeConversationResponse
		wantStarts int
	}{
		{name: "a written compaction is followed by its start", wantStarts: 1},
		{name: "a failed compaction sends no start", summarize: summarizerRefusal(rejectedVendorStart(), "prompt is too long"), wantStarts: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := owing(f, wsm.OwedStartWork{Compact: true})
			f.client.summarizeResponse = tt.summarize
			f.client.response = startedResponse("vendor-1")
			summariesAtStart := -1
			f.client.onStart = func() { summariesAtStart = len(f.client.summaries) }

			// Act.
			_ = f.fleet.Start(context.Background(), ws)

			// Assert.
			if len(f.client.requests) != tt.wantStarts {
				t.Fatalf("StartSession requests = %d, want %d", len(f.client.requests), tt.wantStarts)
			}
			if tt.wantStarts == 1 && summariesAtStart != 1 {
				t.Fatalf("summaries asked before the start = %d, want the one compaction", summariesAtStart)
			}
		})
	}
}
