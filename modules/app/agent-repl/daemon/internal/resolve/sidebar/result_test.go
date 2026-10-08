package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// ---- the last turn result outlives the daemon that saw it ----

// resultReport is one call of the result sink.
type resultReport struct {
	ws     ids.WorkspaceID
	result *wsm.TurnResult
}

// resultResolver builds a live resolver whose one workspace's durable record
// holds STORED, recording every result it reports.
func resultResolver(t *testing.T, stored *wsm.TurnResult) (sidebar.Resolver, *[]resultReport) {
	t.Helper()
	reports := &[]resultReport{}
	r, _ := newResolver(t, sidebar.WithResultSink(
		func(ws ids.WorkspaceID, result *wsm.TurnResult) {
			*reports = append(*reports, resultReport{ws: ws, result: result})
		}))
	rec := workspace(string(theWS), "one")
	rec.Result = stored
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	return r, reports
}

func TestARestoredResultDrawsTheRowAsItStood(t *testing.T) {
	tests := []struct {
		name       string
		stored     *wsm.TurnResult
		wantStatus string
		wantViewed bool
	}{
		{name: "an unread done is FULL done", stored: &wsm.TurnResult{End: wsm.TurnResultDone}, wantStatus: "done"},
		{name: "a read interrupted is PARTIAL interrupted", stored: &wsm.TurnResult{End: wsm.TurnResultInterrupted, Read: true}, wantStatus: "interrupted", wantViewed: true},
		{name: "an unread failed turn is FULL turn_failed", stored: &wsm.TurnResult{End: wsm.TurnResultFailed}, wantStatus: "turn_failed"},
		{name: "no stored result is ready", stored: nil, wantStatus: "ready"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act: a daemon that did not see the turn end.
			r, reports := resultResolver(t, tt.stored)

			// Assert.
			row := onlyRow(t, r)
			if got := statusName(row); got != tt.wantStatus {
				t.Fatalf("status = %q, want %q", got, tt.wantStatus)
			}
			if got := row.GetViewed() != nil; got != tt.wantViewed {
				t.Fatalf("viewed = %v, want %v", got, tt.wantViewed)
			}
			if len(*reports) != 0 {
				t.Fatalf("reports = %+v, want none: a restored result is already durable", *reports)
			}
		})
	}
}

func TestATurnEndIsReportedUnreadThenRead(t *testing.T) {
	// Arrange.
	r, reports := resultResolver(t, nil)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	r.SetViewed(theWS)

	// Assert.
	want := []wsm.TurnResult{{End: wsm.TurnResultDone}, {End: wsm.TurnResultDone, Read: true}}
	if len(*reports) != len(want) {
		t.Fatalf("reports = %+v, want %v", *reports, want)
	}
	for i, report := range *reports {
		if report.ws != theWS || report.result == nil || *report.result != want[i] {
			t.Fatalf("report %d = %+v, want %v", i, report, want[i])
		}
	}
}

func TestANewTurnRetiresTheRestoredResultAndReportsItGone(t *testing.T) {
	// Arrange.
	r, reports := resultResolver(t, &wsm.TurnResult{End: wsm.TurnResultDone, Read: true})

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "submitting" {
		t.Fatalf("status = %q, want submitting", got)
	}
	if len(*reports) != 1 || (*reports)[0].result != nil {
		t.Fatalf("reports = %+v, want the result cleared once", *reports)
	}
}

func TestARestoredResultIsViewedLikeALiveOne(t *testing.T) {
	// Arrange.
	r, reports := resultResolver(t, &wsm.TurnResult{End: wsm.TurnResultInterrupted})

	// Act.
	r.SetViewed(theWS)

	// Assert.
	if onlyRow(t, r).GetViewed() == nil {
		t.Fatal("viewed = none, want the restored interrupted row PARTIAL once viewed")
	}
	if len(*reports) != 1 || (*reports)[0].result == nil || !(*reports)[0].result.Read {
		t.Fatalf("reports = %+v, want the read reported once", *reports)
	}
}

func TestALiveTurnBeforeTheRegistryIsNotOverwrittenByTheRecord(t *testing.T) {
	// Arrange: the adopted shim's running turn is told before the registry.
	r, _ := newResolver(t)
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	r.OnTurnRunningAtAttach(theWS, "turn-1", nil)

	// Act.
	rec := workspace(string(theWS), "one")
	rec.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
	r.SetRegistry(registry(rec))

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking: the live turn outranks the stored result", got)
	}
}
