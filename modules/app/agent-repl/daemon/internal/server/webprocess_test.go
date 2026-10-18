package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

// webhostRecord is a webhost ClientLog request carrying one record.
func webhostRecord(t *testing.T, operation, timestamp string, ctx map[string]any) *connect.Request[agentreplv1.ClientLogRequest] {
	t.Helper()
	fields, err := structpb.NewStruct(ctx)
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return connect.NewRequest(&agentreplv1.ClientLogRequest{
		Source: &agentreplv1.ClientLogRequest_Webhost{Webhost: &agentreplv1.ClientLogWebhost{}},
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: operation,
			Message:   "a record",
			Timestamp: timestamp,
			Context:   fields,
		},
	})
}

func TestClientLogHandsOnlyAWebhostStallToTheStallBeat(t *testing.T) {
	const stamp = "2026-10-18T12:00:09.250000-04:00"
	at, err := time.Parse(time.RFC3339Nano, stamp)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	tests := []struct {
		name      string
		operation string
		want      []observedStall
	}{
		{name: "a stall reaches the beat with its moment and duration", operation: "webhost.stall", want: []observedStall{{at: at, blockedMS: 412}}},
		{name: "any other webhost record does not", operation: "webhost.census"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, tt.operation, stamp, map[string]any{"blocked_ms": 412})); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			got := h.StallBeat.stalls()
			if len(got) != len(tt.want) || (len(got) == 1 && (!got[0].at.Equal(tt.want[0].at) || got[0].blockedMS != tt.want[0].blockedMS)) {
				t.Fatalf("observed %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestClientLogRecordsAnUnreadableStallAndObservesNothing(t *testing.T) {
	tests := []struct {
		name      string
		timestamp string
		ctx       map[string]any
	}{
		{name: "a stall with no readable moment", timestamp: "not a time", ctx: map[string]any{"blocked_ms": 300}},
		{name: "a stall with no blocked duration", timestamp: "2026-10-18T12:00:09.250000-04:00", ctx: map[string]any{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

			// Act
			if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, "webhost.stall", tt.timestamp, tt.ctx)); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			if got := h.StallBeat.stalls(); len(got) != 0 {
				t.Fatalf("observed %+v, want nothing", got)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.server.webhost_stall_unreadable"})
			if !ok || rec.Context["cause"] == nil {
				t.Fatalf("records = %+v, want an error naming the cause", log.Records())
			}
		})
	}
}

func TestAWorkspaceSubscriptionTellsTheSupersededPageTracker(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SubscribePageRequest
		want []servedPage
	}{
		{
			name: "a topbar subscription names its page and workspace",
			req: &agentreplv1.SubscribePageRequest{Page: "page-1", Subscription: "topbar-1",
				Request: &agentreplv1.SubscribePageRequest_Topbar{Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()}}},
			want: []servedPage{{page: "page-1", ws: testWorkspaceID}},
		},
		{
			name: "a roster subscription serves no workspace",
			req: &agentreplv1.SubscribePageRequest{Page: "page-1", Subscription: "roster-1",
				Request: &agentreplv1.SubscribePageRequest_Roster{Roster: &agentreplv1.WatchWorkspaceRosterRequest{}}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithTimeout(context.Background(), openBound)
			defer cancel()
			attachPageStream(t, h, ctx, "page-1")

			// Act
			if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(tt.req)); err != nil {
				t.Fatalf("SubscribePage: %v", err)
			}

			// Assert
			got := h.Pages.pages()
			if len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("served %+v, want %+v", got, tt.want)
			}
		})
	}
}
