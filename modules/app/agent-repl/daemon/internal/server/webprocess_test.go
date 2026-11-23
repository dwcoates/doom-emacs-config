package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/pageheld"
	"agent-repld/internal/stallbeat"
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
			Site: "0123456789ab", Template: "a test record",
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
		{name: "a stall reaches the beat with its document, moment and duration", operation: "webhost.stall", want: []observedStall{{document: "sidebar", at: at, blockedMS: 412}}},
		{name: "any other webhost record does not", operation: "webhost.ready"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, tt.operation, stamp, map[string]any{"blocked_ms": 412, "webhost_document": "sidebar"})); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			got := h.StallBeat.stalls()
			if len(got) != len(tt.want) || (len(got) == 1 && (got[0].document != tt.want[0].document || !got[0].at.Equal(tt.want[0].at) || got[0].blockedMS != tt.want[0].blockedMS)) {
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
		{name: "a stall with no readable moment", timestamp: "not a time", ctx: map[string]any{"blocked_ms": 300, "webhost_document": "host"}},
		{name: "a stall with no blocked duration", timestamp: "2026-10-18T12:00:09.250000-04:00", ctx: map[string]any{"webhost_document": "host"}},
		{name: "a stall naming no document", timestamp: "2026-10-18T12:00:09.250000-04:00", ctx: map[string]any{"blocked_ms": 300}},
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

// workspaceRecord is a workspace ClientLog request carrying one record.
func workspaceRecord(t *testing.T, operation, timestamp string, ctx map[string]any) *connect.Request[agentreplv1.ClientLogRequest] {
	t.Helper()
	req := webhostRecord(t, operation, timestamp, ctx)
	req.Msg.Source = &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()}
	return req
}

const censusStamp = "2026-11-14T15:12:01.600000+00:00"

func TestClientLogHandsACensusToTheStallBeat(t *testing.T) {
	at, err := time.Parse(time.RFC3339Nano, censusStamp)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	three := int64(3)
	tests := []struct {
		name string
		req  func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest]
		want []observedCensus
	}{
		{
			name: "a page census reaches the beat named by its workspace",
			req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
				return workspaceRecord(t, "page.census", censusStamp, map[string]any{
					"dom_nodes": 1701, "feed_rows": 53, "animations": 3, "visibility": "hidden"})
			},
			want: []observedCensus{{doc: stallbeat.Document{Kind: stallbeat.KindPage, Name: string(testWorkspaceID)}, at: at,
				c: stallbeat.Census{DomNodes: 1701, FeedRows: 53, Animations: &three, Visibility: "hidden"}}},
		},
		{
			name: "a webhost census reaches the beat named by its document",
			req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
				return webhostRecord(t, "webhost.census", censusStamp, map[string]any{
					"webhost_document": "sidebar", "pages": 0, "host": map[string]any{"dom_nodes": 8911, "feed_rows": 0}})
			},
			want: []observedCensus{{doc: stallbeat.Document{Kind: stallbeat.KindWebhost, Name: "sidebar"}, at: at,
				c: stallbeat.Census{DomNodes: 8911}}},
		},
		{
			name: "any other workspace record does not",
			req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
				return workspaceRecord(t, "page.repaint", censusStamp, map[string]any{"dom_nodes": 1})
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			if _, err := h.Client.ClientLog(context.Background(), tt.req(t)); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			got := h.StallBeat.censuses()
			if len(got) != len(tt.want) {
				t.Fatalf("observed %+v, want %+v", got, tt.want)
			}
			for i := range got {
				g, w := got[i], tt.want[i]
				sameAnimations := (g.c.Animations == nil) == (w.c.Animations == nil) &&
					(g.c.Animations == nil || *g.c.Animations == *w.c.Animations)
				if g.doc != w.doc || !g.at.Equal(w.at) || g.c.DomNodes != w.c.DomNodes || g.c.FeedRows != w.c.FeedRows ||
					g.c.Visibility != w.c.Visibility || !sameAnimations {
					t.Fatalf("observed %+v, want %+v", g, w)
				}
			}
		})
	}
}

func TestClientLogRecordsAnUnreadableCensusAndObservesNothing(t *testing.T) {
	tests := []struct {
		name string
		req  func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest]
	}{
		{name: "a page census with no readable moment", req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
			return workspaceRecord(t, "page.census", "not a time", map[string]any{"dom_nodes": 1, "feed_rows": 0, "animations": 0, "visibility": "hidden"})
		}},
		{name: "a page census with no element count", req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
			return workspaceRecord(t, "page.census", censusStamp, map[string]any{"feed_rows": 0, "animations": 0, "visibility": "hidden"})
		}},
		{name: "a page census with no visibility", req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
			return workspaceRecord(t, "page.census", censusStamp, map[string]any{"dom_nodes": 1, "feed_rows": 0, "animations": 0})
		}},
		{name: "a webhost census with no document", req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
			return webhostRecord(t, "webhost.census", censusStamp, map[string]any{"host": map[string]any{"dom_nodes": 1, "feed_rows": 0}})
		}},
		{name: "a webhost census with no document size", req: func(t *testing.T) *connect.Request[agentreplv1.ClientLogRequest] {
			return webhostRecord(t, "webhost.census", censusStamp, map[string]any{"webhost_document": "host"})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

			// Act
			if _, err := h.Client.ClientLog(context.Background(), tt.req(t)); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			if got := h.StallBeat.censuses(); len(got) != 0 {
				t.Fatalf("observed %+v, want nothing", got)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.server.census_unreadable"})
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
			subscribePage(t, ctx, h, tt.req)

			// Assert
			got := h.Pages.pages()
			if len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("served %+v, want %+v", got, tt.want)
			}
		})
	}
}

// hostConn is the connection of the host document the frame tests attach.
const hostConn = "webhost-host-1"

// hostCensus is the host document's census naming FRAMES, taken for TRIGGER.
func hostCensus(t *testing.T, frames []any, trigger string) *connect.Request[agentreplv1.ClientLogRequest] {
	return webhostRecord(t, "webhost.census", censusStamp, map[string]any{
		"webhost_document": "host", "connection_id": hostConn, "pages": len(frames),
		"host": map[string]any{"dom_nodes": 1, "feed_rows": 0}, "frames": frames, "trigger": trigger,
	})
}

// attachHost holds the host document's page stream on H for the test's
// length, answering its release.
func attachHost(t *testing.T, h *harness) func() {
	t.Helper()
	release := h.Server.(*server).holdWebhostPage(hostConn, &agentreplv1.WebhostPage{WebappBuild: "Web1"})
	released := false
	t.Cleanup(func() {
		if !released {
			release()
		}
	})
	return func() { released = true; release() }
}

func TestAHostCensusIsTakenOnlyWhileItsDocumentsPageStreamStands(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

	// Act
	if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a"}, "change")); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert
	if got := h.HeldPages.Snapshot(); len(got) != 0 {
		t.Fatalf("held pages = %v, want none from a document with no page stream", got)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Operation: "daemon.server.host_frames"})
	if !ok || rec.Context["connection_id"] != hostConn {
		t.Fatalf("records = %+v, want a debug record naming the connection", log.Records())
	}
}

func TestTheHostsFramesGoWithItsPageStream(t *testing.T) {
	// Arrange
	h := newHarness(t)
	release := attachHost(t, h)
	if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a"}, "change")); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Act
	release()

	// Assert
	if got := h.HeldPages.Snapshot(); len(got) != 0 {
		t.Fatalf("held pages = %v, want none once the host's stream ended", got)
	}
}

func TestTheHostsCensusSetsTheHeldPages(t *testing.T) {
	// Arrange
	h := newHarness(t)
	attachHost(t, h)

	// Act
	if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a", "b"}, "change")); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert
	got := h.HeldPages.Snapshot()
	if len(got) != 2 || got["a"] != pageheld.LoadUnreported || got["b"] != pageheld.LoadUnreported {
		t.Fatalf("held pages = %v, want a and b held, unreported", got)
	}
}

func TestTheSidebarsCensusSetsNoHeldPages(t *testing.T) {
	// Arrange
	h := newHarness(t)
	attachHost(t, h)

	// Act
	if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, "webhost.census", censusStamp, map[string]any{
		"webhost_document": "sidebar", "connection_id": hostConn, "pages": 0, "host": map[string]any{"dom_nodes": 1, "feed_rows": 0},
		"frames": []any{"a"}, "trigger": "tick",
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert
	if got := h.HeldPages.Snapshot(); len(got) != 0 {
		t.Fatalf("held pages = %v, want none from the sidebar's census", got)
	}
}

func TestAHostCensusWithNoFramesIsAnOlderBuildRecordedAtDebug(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	attachHost(t, h)
	if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a"}, "change")); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Act
	if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, "webhost.census", censusStamp, map[string]any{
		"webhost_document": "host", "connection_id": hostConn, "pages": 1, "host": map[string]any{"dom_nodes": 1, "feed_rows": 0},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert
	if got := h.HeldPages.Snapshot(); len(got) != 1 {
		t.Fatalf("held pages = %v, want the last frames kept", got)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Operation: "daemon.server.host_frames"}); !ok {
		t.Fatalf("records = %+v, want a debug record of the census with no frames", log.Records())
	}
}

func TestAHostCensusWhoseFramesCannotBeReadIsRecordedAndSetsNothing(t *testing.T) {
	blank := ""
	tests := []struct {
		name string
		// conn is the census's connection_id: hostConn when unset, none when
		// noConn.
		conn    *string
		noConn  bool
		frames  any
		trigger any
	}{
		{name: "no connection", noConn: true, frames: []any{"a"}, trigger: "tick"},
		{name: "a blank connection", conn: &blank, frames: []any{"a"}, trigger: "tick"},
		{name: "frames that are not a list", frames: "a", trigger: "tick"},
		{name: "a frame that is not a workspace id", frames: []any{"a", 7}, trigger: "tick"},
		{name: "a blank frame", frames: []any{""}, trigger: "tick"},
		{name: "a trigger that is neither tick nor change", frames: []any{"a"}, trigger: "later"},
		{name: "no trigger", frames: []any{"a"}, trigger: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			attachHost(t, h)
			ctx := map[string]any{
				"webhost_document": "host", "pages": 1, "host": map[string]any{"dom_nodes": 1, "feed_rows": 0},
				"frames": tt.frames,
			}
			switch {
			case tt.noConn:
			case tt.conn != nil:
				ctx["connection_id"] = *tt.conn
			default:
				ctx["connection_id"] = hostConn
			}
			if tt.trigger != nil {
				ctx["trigger"] = tt.trigger
			}

			// Act
			if _, err := h.Client.ClientLog(context.Background(), webhostRecord(t, "webhost.census", censusStamp, ctx)); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert
			if got := h.HeldPages.Snapshot(); len(got) != 0 {
				t.Fatalf("held pages = %v, want none", got)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.server.census_unreadable"})
			if !ok || rec.Context["cause"] == nil {
				t.Fatalf("records = %+v, want an error naming the cause", log.Records())
			}
		})
	}
}

func TestADisagreementThatStandsIsRecordedOnceAtWarnWithBothSets(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	attachHost(t, h)
	h.HeldPages.Held("a", pageheld.LoadShown)
	h.HeldPages.Held("gone", pageheld.LoadSlot)
	tick := func() {
		if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a"}, "tick")); err != nil {
			t.Fatalf("ClientLog: %v", err)
		}
	}

	// Act
	tick()
	tick()
	tick()

	// Assert
	var warns []dlog.Record
	for _, rec := range log.Records() {
		if rec.Level == "warn" && rec.Operation == "daemon.server.host_frames" {
			warns = append(warns, rec)
		}
	}
	if len(warns) != 1 {
		t.Fatalf("warns = %+v, want one for the one standing cause", warns)
	}
	c := warns[0].Context
	if c["cause"] != "emacs_only" || !sameNames(c["workspaces"], "gone") || !sameNames(c["host_frames"], "a") ||
		!sameNames(c["emacs_pages"], "a", "gone") {
		t.Fatalf("warn context = %+v, want the cause, the workspaces and both sets", c)
	}
}

func TestADisagreementThatClearsIsRecordedAtInfo(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	attachHost(t, h)
	for range 2 {
		if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{"a"}, "tick")); err != nil {
			t.Fatalf("ClientLog: %v", err)
		}
	}

	// Act
	if _, err := h.Client.ClientLog(context.Background(), hostCensus(t, []any{}, "tick")); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.server.host_frames"})
	if !ok || rec.Context["cause"] != "host_only" {
		t.Fatalf("records = %+v, want an info record of host_only clearing", log.Records())
	}
}

// sameNames reports whether V, a record's list of workspaces, is exactly WANT.
func sameNames(v any, want ...string) bool {
	got, ok := v.([]string)
	if !ok || len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}
