package sidebar_test

import (
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
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
	}{
		{name: "a done is done", stored: &wsm.TurnResult{End: wsm.TurnResultDone}, wantStatus: "done"},
		{name: "an interrupted is interrupted", stored: &wsm.TurnResult{End: wsm.TurnResultInterrupted}, wantStatus: "interrupted"},
		{name: "a failed turn is turn_failed", stored: &wsm.TurnResult{End: wsm.TurnResultFailed}, wantStatus: "turn_failed"},
		{name: "no stored result is ready", stored: nil, wantStatus: "ready"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act: a daemon that did not see the turn end.
			r, reports := resultResolver(t, tt.stored)

			// Assert: nobody interacted with it, so it is FULL.
			row := onlyRow(t, r)
			if got := statusName(row); got != tt.wantStatus {
				t.Fatalf("status = %q, want %q", got, tt.wantStatus)
			}
			if row.GetViewed() != nil {
				t.Fatalf("viewed = %v, want unset with no durable viewed fact", row.GetViewed())
			}
			if len(*reports) != 0 {
				t.Fatalf("reports = %+v, want none: a restored result is already durable", *reports)
			}
		})
	}
}

func TestATurnEndIsReportedOnceAndAnInteractionReportsNoResult(t *testing.T) {
	// Arrange.
	r, reports := resultResolver(t, nil)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	r.SetInteracted(theWS, epoch)

	// Assert: the result is how the turn ended; viewing it is another fact.
	want := []wsm.TurnResult{{End: wsm.TurnResultDone}}
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
	r, reports := resultResolver(t, &wsm.TurnResult{End: wsm.TurnResultDone})

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

// ---- the viewed fact outlives the daemon that saw the interaction ----

// viewedReport is one call of the viewed sink.
type viewedReport struct {
	ws  ids.WorkspaceID
	arm string
}

// viewedResolver builds a live resolver whose one workspace's durable record
// holds the turn end STORED and the viewed arm VIEWED, recording every viewed
// fact it reports.
func viewedResolver(t *testing.T, stored *wsm.TurnResult, viewed string) (sidebar.Resolver, *[]viewedReport) {
	t.Helper()
	reports := &[]viewedReport{}
	r, _ := newResolver(t, sidebar.WithViewedSink(
		func(ws ids.WorkspaceID, arm string) {
			*reports = append(*reports, viewedReport{ws: ws, arm: arm})
		}))
	rec := workspace(string(theWS), "one")
	rec.Result = stored
	rec.ViewedStatus = viewed
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	return r, reports
}

func TestARestoredViewedFactDrawsTheRowViewedWhateverItsArm(t *testing.T) {
	tests := []struct {
		name        string
		viewed      string
		wantViewed  bool
		wantReports []viewedReport
	}{
		{name: "viewed on the arm the row draws is PARTIAL", viewed: "done", wantViewed: true},
		{name: "viewed on another arm is PARTIAL: only a turn lowers it", viewed: "thinking", wantViewed: true},
		{name: "no durable fact is FULL", viewed: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act: a daemon that did not see the interaction.
			r, reports := viewedResolver(t, &wsm.TurnResult{End: wsm.TurnResultDone}, tt.viewed)

			// Assert.
			row := onlyRow(t, r)
			if got := statusName(row); got != "done" {
				t.Fatalf("status = %q, want done", got)
			}
			if got := row.GetViewed() != nil; got != tt.wantViewed {
				t.Fatalf("viewed = %v, want %v", got, tt.wantViewed)
			}
			if !slices.Equal(*reports, tt.wantReports) {
				t.Fatalf("viewed reports = %+v, want %+v", *reports, tt.wantReports)
			}
		})
	}
}

func TestTheViewedFactIsReportedWhenSetAndWhenATurnClearsIt(t *testing.T) {
	// Arrange.
	r, reports := viewedResolver(t, &wsm.TurnResult{End: wsm.TurnResultDone}, "")

	// Act: the user interacts, then a new turn starts.
	r.SetInteracted(theWS, epoch)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	want := []viewedReport{{ws: theWS, arm: "done"}, {ws: theWS, arm: ""}}
	if !slices.Equal(*reports, want) {
		t.Fatalf("viewed reports = %+v, want %+v", *reports, want)
	}
}

func TestARepeatedInteractionReportsTheViewedFactOnce(t *testing.T) {
	// Arrange.
	r, reports := viewedResolver(t, &wsm.TurnResult{End: wsm.TurnResultDone}, "")
	r.SetInteracted(theWS, epoch)

	// Act.
	r.SetInteracted(theWS, epoch.Add(time.Second))

	// Assert: the durable record already holds the arm.
	want := []viewedReport{{ws: theWS, arm: "done"}}
	if !slices.Equal(*reports, want) {
		t.Fatalf("viewed reports = %+v, want %+v", *reports, want)
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

func TestARestoredViewedFactWaitsOutTheBringUpsArms(t *testing.T) {
	// Arrange: the registry arrives before the shim's link, so the row draws
	// the bring-up's arms first, as a booting daemon does.
	reports := &[]viewedReport{}
	r, _ := newResolver(t, sidebar.WithViewedSink(func(ws ids.WorkspaceID, arm string) {
		*reports = append(*reports, viewedReport{ws: ws, arm: arm})
	}))
	rec := workspace(string(theWS), "one")
	rec.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
	rec.ViewedStatus = "done"
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkDialing)

	// Act: the link connects and the row settles on its restored done.
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done", got)
	}
	if row.GetViewed() == nil {
		t.Fatal("viewed = unset, want the restored fact standing once the row reached its arm")
	}
	if len(*reports) != 0 {
		t.Fatalf("viewed reports = %+v, want none: the bring-up's arms lowered nothing", *reports)
	}
}

func TestATurnBeforeTheRestoredArmDropsTheRestoredViewedFact(t *testing.T) {
	// Arrange: the row is still bringing up when a prompt is accepted.
	reports := &[]viewedReport{}
	r, _ := newResolver(t, sidebar.WithViewedSink(func(ws ids.WorkspaceID, arm string) {
		*reports = append(*reports, viewedReport{ws: ws, arm: arm})
	}))
	rec := workspace(string(theWS), "one")
	rec.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
	rec.ViewedStatus = "done"
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act: that turn ends on done, the arm the fact was restored for.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert: the new done is a result the user has not interacted with.
	if got := onlyRow(t, r).GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset after a new turn", got)
	}
	want := []viewedReport{{ws: theWS, arm: ""}}
	if !slices.Equal(*reports, want) {
		t.Fatalf("viewed reports = %+v, want the restored fact cleared once", *reports)
	}
}
