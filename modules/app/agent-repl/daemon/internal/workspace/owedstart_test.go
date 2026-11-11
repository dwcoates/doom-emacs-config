package workspace

import (
	"context"
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// wantSummarizer is the cold gate's default compaction choice.
var wantSummarizer = &conversationv1.SessionColdCompact{
	Model: &conversationv1.AgentModel{Name: headless.ModelHaiku},
	Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
}

// owing arranges a resumable workspace whose binding owes OWED.
func owing(f *fleetFixture, owed wsm.OwedStartWork) ids.WorkspaceID {
	ws := resumable(f)
	f.db.owedWork = map[ids.WorkspaceID]wsm.OwedStartWork{ws: owed}
	return ws
}

// withCarriedSummary is RESPONSE answering TEXT as the summary it carried.
func withCarriedSummary(response *shimv1.StartSessionResponse, text string) *shimv1.StartSessionResponse {
	response.GetSuccess().CarriedSummary = &shimv1.StartSessionCarriedSummary{Text: text}
	return response
}

func TestDefaultSummarizerIsTheColdGatesFirstChoice(t *testing.T) {
	// Arrange.
	served, _ := coldCompactMenu()

	// Act.
	got := defaultSummarizer()

	// Assert.
	if !proto.Equal(got, &conversationv1.SessionColdCompact{Model: served.Models[0], Scope: served.Scopes[0]}) || !proto.Equal(got, wantSummarizer) {
		t.Fatalf("defaultSummarizer() = %v, want the gate menu's first model and scope (%v)", got, wantSummarizer)
	}
}

func TestCarryOwedStartWork(t *testing.T) {
	pay := &conversationv1.SessionColdRemediation{Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}}}
	compact := &conversationv1.SessionColdRemediation{Remediation: &conversationv1.SessionColdRemediation_Compact{Compact: wantSummarizer}}
	tests := []struct {
		name        string
		owed        *wsm.OwedStartWork
		remediation *conversationv1.SessionColdRemediation
		want        *shimv1.StartSessionResume
		wantCarried *wsm.OwedStartWork
	}{
		{
			name: "nothing owed leaves the resume as built",
			want: &shimv1.StartSessionResume{VendorSessionId: "vendor-1"},
		},
		{
			name:        "an owed compaction is the resume's compact remediation",
			owed:        &wsm.OwedStartWork{Compact: true},
			want:        &shimv1.StartSessionResume{VendorSessionId: "vendor-1", ColdRemediation: compact},
			wantCarried: &wsm.OwedStartWork{Compact: true},
		},
		{
			name:        "an owed compaction replaces the remediation the resume carried",
			owed:        &wsm.OwedStartWork{Compact: true},
			remediation: pay,
			want:        &shimv1.StartSessionResume{VendorSessionId: "vendor-1", ColdRemediation: compact},
			wantCarried: &wsm.OwedStartWork{Compact: true},
		},
		{
			name: "an owed summary is the resume's carry_summary",
			owed: &wsm.OwedStartWork{SummaryOf: "queue-1"},
			want: &shimv1.StartSessionResume{VendorSessionId: "vendor-1",
				CarrySummary: &shimv1.StartSessionCarrySummary{VendorSessionId: "queue-1", Summarizer: wantSummarizer}},
			wantCarried: &wsm.OwedStartWork{SummaryOf: "queue-1"},
		},
		{
			name:        "an owed summary keeps the remediation the resume carried",
			owed:        &wsm.OwedStartWork{SummaryOf: "queue-1"},
			remediation: pay,
			want: &shimv1.StartSessionResume{VendorSessionId: "vendor-1", ColdRemediation: pay,
				CarrySummary: &shimv1.StartSessionCarrySummary{VendorSessionId: "queue-1", Summarizer: wantSummarizer}},
			wantCarried: &wsm.OwedStartWork{SummaryOf: "queue-1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			if tt.owed != nil {
				f.db.owedWork = map[ids.WorkspaceID]wsm.OwedStartWork{ws: *tt.owed}
			}
			req := &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{
				VendorSessionId: "vendor-1", ColdRemediation: tt.remediation}}}

			// Act.
			carried, err := f.fleet.carryOwedStartWork(context.Background(), f.log.logger, ws, req)

			// Assert.
			if err != nil {
				t.Fatalf("carryOwedStartWork: %v", err)
			}
			if !proto.Equal(req.GetResume(), tt.want) {
				t.Fatalf("resume = %v, want %v", req.GetResume(), tt.want)
			}
			if (carried == nil) != (tt.wantCarried == nil) || (carried != nil && *carried != *tt.wantCarried) {
				t.Fatalf("carried = %+v, want %+v", carried, tt.wantCarried)
			}
		})
	}
}

func TestCarryOwedStartWorkRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture, ws ids.WorkspaceID)
		req     *shimv1.StartSessionRequest
		message string
	}{
		{
			name:    "an unreadable owed work",
			arrange: func(f *fleetFixture, _ ids.WorkspaceID) { f.db.owedWorkErr = errFake },
			req:     &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Resume{Resume: &shimv1.StartSessionResume{VendorSessionId: "vendor-1"}}},
			message: "could not read the start work the workspace's binding owes; the session was not started",
		},
		{
			name: "a fresh start of a binding that owes work",
			arrange: func(f *fleetFixture, ws ids.WorkspaceID) {
				f.db.owedWork = map[ids.WorkspaceID]wsm.OwedStartWork{ws: {Compact: true}}
			},
			req:     &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{Fresh: &shimv1.StartSessionFresh{}}},
			message: "a fresh start was asked of a binding that owes start work; the session was not started",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			tt.arrange(f, ws)
			before := proto.Clone(tt.req)

			// Act.
			carried, err := f.fleet.carryOwedStartWork(context.Background(), f.log.logger, ws, tt.req)

			// Assert.
			if err == nil || carried != nil {
				t.Fatalf("carryOwedStartWork = (%+v, %v), want a refusal carrying nothing", carried, err)
			}
			if !proto.Equal(tt.req, before) {
				t.Fatalf("request = %v, want it unchanged (%v)", tt.req, before)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opOwedStartWork, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.message)
			}
		})
	}
}

func TestSettleOwedStartWork(t *testing.T) {
	tests := []struct {
		name        string
		carried     *wsm.OwedStartWork
		summary     string
		wantSummary string
	}{
		{name: "a start that carried nothing settles nothing"},
		{name: "a compaction settles with no summary", carried: &wsm.OwedStartWork{Compact: true}},
		{name: "a summary settles and is kept for the next prompt", carried: &wsm.OwedStartWork{SummaryOf: "queue-1"}, summary: "the queue did X", wantSummary: "the queue did X"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			if tt.carried != nil {
				f.db.owedWork = map[ids.WorkspaceID]wsm.OwedStartWork{ws: *tt.carried}
			}
			response := startedResponse("vendor-1")
			if tt.summary != "" {
				withCarriedSummary(response, tt.summary)
			}

			// Act.
			err := f.fleet.settleOwedStartWork(context.Background(), f.log.logger, ws, tt.carried, response.GetSuccess())

			// Assert.
			if err != nil {
				t.Fatalf("settleOwedStartWork: %v", err)
			}
			if _, owed := f.db.owedWork[ws]; owed {
				t.Fatalf("owed work = %+v, want it settled", f.db.owedWork[ws])
			}
			if got := f.db.contextSummaries[ws]; got != tt.wantSummary {
				t.Fatalf("context summary = %q, want %q", got, tt.wantSummary)
			}
		})
	}
}

func TestSettleOwedStartWorkRefuses(t *testing.T) {
	tests := []struct {
		name    string
		carried *wsm.OwedStartWork
		summary string
		dbErr   error
		message string
	}{
		{
			name:    "a summary asked for and not answered",
			carried: &wsm.OwedStartWork{SummaryOf: "queue-1"},
			message: "the start's carried summary disagrees with what it asked for; the session's start is refused",
		},
		{
			name:    "a summary answered for a compaction",
			carried: &wsm.OwedStartWork{Compact: true},
			summary: "unasked",
			message: "the start's carried summary disagrees with what it asked for; the session's start is refused",
		},
		{
			name:    "a settlement the store refuses",
			carried: &wsm.OwedStartWork{Compact: true},
			dbErr:   errFake,
			message: "could not settle the start work the session carried out; the next start asks for it again",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			f.db.owedWork = map[ids.WorkspaceID]wsm.OwedStartWork{ws: *tt.carried}
			f.db.settleErr = tt.dbErr
			response := startedResponse("vendor-1")
			if tt.summary != "" {
				withCarriedSummary(response, tt.summary)
			}

			// Act.
			err := f.fleet.settleOwedStartWork(context.Background(), f.log.logger, ws, tt.carried, response.GetSuccess())

			// Assert.
			if err == nil {
				t.Fatal("settleOwedStartWork = nil, want a refusal")
			}
			if tt.dbErr != nil && !errors.Is(err, tt.dbErr) {
				t.Fatalf("err = %v, want the store's failure", err)
			}
			if got := f.db.owedWork[ws]; got != *tt.carried {
				t.Fatalf("owed work = %+v, want it still owed (%+v)", got, *tt.carried)
			}
			if _, kept := f.db.contextSummaries[ws]; kept {
				t.Fatal("a context summary was kept, want none")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opOwedStartWork, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.message)
			}
		})
	}
}

// TestAStartCarriesAndSettlesTheOwedWork asserts the bring-up's StartSession
// is the builder's request with the owed work carried, and that its answer
// settles it.
func TestAStartCarriesAndSettlesTheOwedWork(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := owing(f, wsm.OwedStartWork{SummaryOf: "queue-1"})
	f.client.response = withCarriedSummary(startedResponse("vendor-1"), "the queue did X")

	// Act.
	err := f.fleet.Start(context.Background(), ws)

	// Assert.
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if len(f.client.requests) != 1 {
		t.Fatalf("StartSession requests = %v, want exactly one", f.client.requests)
	}
	if got := f.client.requests[0].GetResume().GetCarrySummary(); !proto.Equal(got, &shimv1.StartSessionCarrySummary{VendorSessionId: "queue-1", Summarizer: wantSummarizer}) {
		t.Fatalf("carry_summary = %v, want the owed summary", got)
	}
	if _, owed := f.db.owedWork[ws]; owed || f.db.contextSummaries[ws] != "the queue did X" {
		t.Fatalf("owed = %+v, summaries = %+v, want the work settled with its summary kept", f.db.owedWork, f.db.contextSummaries)
	}
}

func TestCompactionFailure(t *testing.T) {
	tests := []struct {
		name       string
		compaction bool
		want       bool
	}{
		{name: "a failed compaction is marked as one", compaction: true, want: true},
		{name: "any other vendor start failure is not", compaction: false, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := owing(f, wsm.OwedStartWork{Compact: true})
			label := rejectedVendorStart()
			if tt.compaction {
				label.Compaction = &shimv1.StartSessionCompactionFailed{}
			}
			f.client.response = vendorRefusal(label, "the summarizer failed")

			// Act.
			err := f.fleet.Start(context.Background(), ws)
			detail, got := compactionFailure(err)

			// Assert.
			if err == nil {
				t.Fatal("Start = nil, want the vendor start failure")
			}
			if got != tt.want || (got && detail != "the summarizer failed") {
				t.Fatalf("compactionFailure = (%q, %v), want (the shim's account, %v)", detail, got, tt.want)
			}
			if f.db.owedWork[ws] != (wsm.OwedStartWork{Compact: true}) {
				t.Fatalf("owed work = %+v, want it still owed for the next start", f.db.owedWork[ws])
			}
		})
	}
}
