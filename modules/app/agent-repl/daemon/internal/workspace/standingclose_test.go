package workspace

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// standingRequest is a fixture whose w1 holds a standing close request by BY.
func standingRequest(t *testing.T, by wsm.CloseCause) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.db.RequestClose(context.Background(), "w1", wsm.CloseRequest{At: instantFor(f), By: by}); err != nil {
		t.Fatalf("RequestClose: %v", err)
	}
	return f
}

// instantFor is the fixture's own clock reading.
func instantFor(f *fixture) time.Time { return f.verbs.(*verbs).now() }

// blockers are the ways a close can be blocked, each a fixture arrangement
// on w1.
var blockers = []struct {
	name    string
	arrange func(f *fixture)
}{
	{name: "a turn in flight", arrange: func(f *fixture) { runningTurn(f, 0) }},
	{name: "live detached work", arrange: func(f *fixture) {
		runningTurn(f, 1)
		f.running.Turn = nil
	}},
	{name: "a held prompt", arrange: func(f *fixture) { f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}} }},
	{name: "a merge in the queue", arrange: func(f *fixture) { f.merge.facts["w1"] = footer.MergeFacts{State: "queued"} }},
	{name: "an open child", arrange: func(f *fixture) { f.db.noOpenChildrenErr = errOpenChildren }},
}

func TestCloseStandingClosesAQuietWorkspaceNow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	waits, err := f.verbs.CloseStanding(context.Background(), "w1", wsm.CloseByUser)

	// Assert
	if err != nil || waits != nil {
		t.Fatalf("CloseStanding = %+v, %v; want closed now", waits, err)
	}
	if closures := f.db.closures["w1"]; len(closures) != 1 || closures[0].By != wsm.CloseByUser || f.db.workspaces["w1"].CloseRequest != nil {
		t.Fatalf("closures %+v, request %+v; want one close by the user and no request", closures, f.db.workspaces["w1"].CloseRequest)
	}
}

func TestCloseStandingStandsOnEachBlockerThatClearsOnItsOwn(t *testing.T) {
	for _, tt := range blockers {
		if tt.name == "a turn in flight" {
			continue // a close interrupts the turn, so it never stands on one.
		}
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			tt.arrange(f)

			// Act
			waits, err := f.verbs.CloseStanding(context.Background(), "w1", wsm.CloseByAgent)

			// Assert
			if err != nil || waits == nil || waits.None() {
				t.Fatalf("CloseStanding = %+v, %v; want it to stand", waits, err)
			}
			request := f.db.workspaces["w1"].CloseRequest
			if f.db.closedFlags["w1"] || request == nil || request.By != wsm.CloseByAgent {
				t.Fatalf("closed %v, request %+v; want open with the agent's request", f.db.closedFlags["w1"], request)
			}
			if blocked := f.footer.closing["w1"]; blocked == nil || !blocked.Standing || blocked.Detail == "" {
				t.Fatalf("footer = %+v, want the standing close with its reasons", blocked)
			}
			if !slices.Equal(f.standingCloses.reconsidered, []ids.WorkspaceID{"w1"}) {
				t.Fatalf("reconsidered = %v, want w1 owed a judgment", f.standingCloses.reconsidered)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opStandingClose,
				Message: "the close stands; the daemon closes the workspace once nothing blocks it"}); !ok {
				t.Fatalf("no info record of the standing close: %+v", f.log.logger.Records())
			}
		})
	}
}

func TestCloseStandingWhoseRequestCannotBeRegisteredStandsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.noOpenChildrenErr = errOpenChildren
	f.db.requestCloseErr = errors.New("db gone")

	// Act
	_, err := f.verbs.CloseStanding(context.Background(), "w1", wsm.CloseByUser)

	// Assert
	if err == nil || f.db.workspaces["w1"].CloseRequest != nil || f.footer.closing["w1"] != nil || len(f.standingCloses.reconsidered) != 0 {
		t.Fatalf("CloseStanding = %v, request %+v, footer %+v; want a failure and nothing standing", err, f.db.workspaces["w1"].CloseRequest, f.footer.closing["w1"])
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStandingClose, Message: "could not register the standing close request"}); !ok {
		t.Fatalf("no error record: %+v", f.log.logger.Records())
	}
}

func TestReconsideringLeavesAWorkspaceWithNoRequestAlone(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "no request stands", arrange: func(f *fixture) {}},
		{name: "the workspace is closed", arrange: func(f *fixture) {
			ws := f.db.workspaces["w1"]
			ws.Closed, ws.CloseRequest = true, &wsm.CloseRequest{By: wsm.CloseByAgent}
			f.db.workspaces["w1"] = ws
		}},
		{name: "another daemon serves it", arrange: func(f *fixture) {
			ws := f.db.workspaces["w1"]
			ws.CloseRequest = &wsm.CloseRequest{By: wsm.CloseByAgent}
			f.db.workspaces["w1"] = ws
			f.owner.standing = StandingTransferringAway
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			tt.arrange(f)

			// Act
			err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

			// Assert
			if err != nil || len(f.db.closures["w1"]) != 0 {
				t.Fatalf("ReconsiderCloseRequest = %v, closures %+v; want nothing done", err, f.db.closures["w1"])
			}
		})
	}
}

func TestEachBlockerClearingClosesTheStandingRequest(t *testing.T) {
	for _, by := range []wsm.CloseCause{wsm.CloseByAgent, wsm.CloseByUser} {
		for _, tt := range blockers {
			t.Run(string(by)+"/"+tt.name, func(t *testing.T) {
				// Arrange: the request stood on the blocker, which has cleared.
				f := standingRequest(t, by)
				tt.arrange(f)
				if err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1"); err != nil || f.db.closedFlags["w1"] {
					t.Fatalf("ReconsiderCloseRequest = %v, closed %v; want it to wait first", err, f.db.closedFlags["w1"])
				}
				clearBlockers(f)

				// Act
				err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

				// Assert
				if err != nil {
					t.Fatalf("ReconsiderCloseRequest: %v", err)
				}
				if closures := f.db.closures["w1"]; len(closures) != 1 || closures[0].By != by {
					t.Fatalf("closures = %+v, want one close as the request's asker %q", closures, by)
				}
				if f.db.workspaces["w1"].CloseRequest != nil || f.footer.closing["w1"] != nil {
					t.Fatalf("request %+v, footer %+v; want both gone with the close", f.db.workspaces["w1"].CloseRequest, f.footer.closing["w1"])
				}
			})
		}
	}
}

// clearBlockers clears every blocker the table arranges.
func clearBlockers(f *fixture) {
	f.running.Turn = nil
	f.running.LiveWork = sessionwatcher.LiveWorkSet{}
	delete(f.db.held, "w1")
	delete(f.merge.facts, "w1")
	f.db.noOpenChildrenErr = nil
}

func TestARemainingBlockerKeepsTheStandingRequestWaiting(t *testing.T) {
	for _, tt := range blockers {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := standingRequest(t, wsm.CloseByAgent)
			tt.arrange(f)

			// Act
			err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

			// Assert
			if err != nil || f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseRequest == nil {
				t.Fatalf("ReconsiderCloseRequest = %v, closed %v, request %+v; want it waiting", err, f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseRequest)
			}
			if len(f.shim.killedTurns) != 0 {
				t.Fatalf("killed turns = %+v, want no turn cut short by a standing close", f.shim.killedTurns)
			}
			if blocked := f.footer.closing["w1"]; blocked == nil || !blocked.Standing {
				t.Fatalf("footer = %+v, want the standing close waiting", blocked)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opStandingClose, Message: "the standing close still waits"}); !ok {
				t.Fatalf("no info record: %+v", f.log.logger.Records())
			}
		})
	}
}

func TestAStandingCloseBlockedAgainAsItIsCarriedOutKeepsWaiting(t *testing.T) {
	// Arrange: a child opened between the judgment and the close's write.
	f := standingRequest(t, wsm.CloseByAgent)
	f.db.setClosedErr = errOpenChildren

	// Act
	err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

	// Assert
	if err != nil || f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseRequest == nil {
		t.Fatalf("ReconsiderCloseRequest = %v, closed %v, request %+v; want it still standing", err, f.db.closedFlags["w1"], f.db.workspaces["w1"].CloseRequest)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opStandingClose,
		Message: "the standing close was blocked again as it was carried out; it keeps waiting"}); !ok {
		t.Fatalf("no info record: %+v", f.log.logger.Records())
	}
}

func TestReconsideringFailsLoudlyOnAFailedRead(t *testing.T) {
	// Arrange
	f := standingRequest(t, wsm.CloseByAgent)
	f.db.noOpenChildrenErr = errors.New("db gone")

	// Act
	err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

	// Assert
	if err == nil || f.db.closedFlags["w1"] || f.db.workspaces["w1"].CloseRequest == nil {
		t.Fatalf("ReconsiderCloseRequest = %v, closed %v; want a failure, open, still standing", err, f.db.closedFlags["w1"])
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opClose, Message: "could not read whether the workspace has open children"}); !ok {
		t.Fatalf("no error record: %+v", f.log.logger.Records())
	}
}

func TestReconsideringEveryRequestJudgesEachOne(t *testing.T) {
	// Arrange: w1 is quiet, w2 still has a held prompt.
	f := standingRequest(t, wsm.CloseByAgent)
	f.workspace("w2", t.TempDir())
	if err := f.db.RequestClose(context.Background(), "w2", wsm.CloseRequest{By: wsm.CloseByUser}); err != nil {
		t.Fatalf("RequestClose: %v", err)
	}
	f.db.held["w2"] = []wsm.HeldPrompt{{Turn: "held-1"}}

	// Act
	err := f.verbs.ReconsiderCloseRequests(context.Background())

	// Assert
	if err != nil || !f.db.closedFlags["w1"] || f.db.closedFlags["w2"] {
		t.Fatalf("ReconsiderCloseRequests = %v, closed w1 %v, w2 %v; want w1 closed and w2 waiting", err, f.db.closedFlags["w1"], f.db.closedFlags["w2"])
	}
}

func TestReconsideringEveryRequestFailsWhenTheRequestsCannotBeRead(t *testing.T) {
	// Arrange
	f := standingRequest(t, wsm.CloseByAgent)
	f.db.closeRequestsErr = errors.New("db gone")

	// Act
	err := f.verbs.ReconsiderCloseRequests(context.Background())

	// Assert
	if err == nil || f.db.closedFlags["w1"] {
		t.Fatalf("ReconsiderCloseRequests = %v, closed %v; want a failure and nothing closed", err, f.db.closedFlags["w1"])
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStandingClose, Message: "could not read the standing close requests"}); !ok {
		t.Fatalf("no error record: %+v", f.log.logger.Records())
	}
}

func TestWithdrawingTheCloseRequest(t *testing.T) {
	tests := []struct {
		name          string
		standing      bool
		wantWithdrawn bool
	}{
		{name: "a standing request is withdrawn", standing: true, wantWithdrawn: true},
		{name: "none stood"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tt.standing {
				f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}}
				if _, err := f.verbs.CloseStanding(context.Background(), "w1", wsm.CloseByAgent); err != nil {
					t.Fatalf("CloseStanding: %v", err)
				}
			}

			// Act
			withdrawn, err := f.verbs.WithdrawCloseRequest(context.Background(), "w1", WithdrawnByUserPrompt)

			// Assert
			if err != nil || withdrawn != tt.wantWithdrawn || f.db.workspaces["w1"].CloseRequest != nil || f.footer.closing["w1"] != nil {
				t.Fatalf("WithdrawCloseRequest = %v, %v, request %+v, footer %+v; want withdrawn %v and nothing left standing", withdrawn, err, f.db.workspaces["w1"].CloseRequest, f.footer.closing["w1"], tt.wantWithdrawn)
			}
			_, logged := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opStandingClose, Message: "withdrew the standing close request; the workspace stays open"})
			if logged != tt.wantWithdrawn {
				t.Fatalf("withdrawal recorded = %v, want %v", logged, tt.wantWithdrawn)
			}
		})
	}
}

func TestAWithdrawnRequestIsNeverCarriedOut(t *testing.T) {
	// Arrange
	f := standingRequest(t, wsm.CloseByAgent)
	if _, err := f.verbs.WithdrawCloseRequest(context.Background(), "w1", WithdrawnByRequest); err != nil {
		t.Fatalf("WithdrawCloseRequest: %v", err)
	}

	// Act
	err := f.verbs.ReconsiderCloseRequest(context.Background(), "w1")

	// Assert
	if err != nil || f.db.closedFlags["w1"] {
		t.Fatalf("ReconsiderCloseRequest = %v, closed %v; want the quiet workspace left open", err, f.db.closedFlags["w1"])
	}
}

func TestAWithdrawalThatFailsIsLoud(t *testing.T) {
	// Arrange
	f := standingRequest(t, wsm.CloseByAgent)
	f.db.withdrawCloseErr = errors.New("db gone")

	// Act
	_, err := f.verbs.WithdrawCloseRequest(context.Background(), "w1", WithdrawnByRequest)

	// Assert
	if err == nil || f.db.workspaces["w1"].CloseRequest == nil {
		t.Fatalf("WithdrawCloseRequest = %v, request %+v; want a failure and the request standing", err, f.db.workspaces["w1"].CloseRequest)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opStandingClose, Message: "could not withdraw the standing close request"}); !ok {
		t.Fatalf("no error record: %+v", f.log.logger.Records())
	}
}

func TestTheStandingCloseComposesEveryBlocker(t *testing.T) {
	landing := &wsm.HasOpenChildrenError{LandingInto: []wsm.FamilyRef{{ID: "w3", Name: "lander"}}}
	tests := []struct {
		name       string
		waits      CloseWaits
		wantReason string
		wantDetail string
	}{
		{name: "nothing blocks", waits: CloseWaits{}},
		{name: "a quietness blocker", waits: CloseWaits{Blocked: &footer.CloseBlocked{Reason: "held_prompts", Detail: "1 held prompts have not been delivered"}},
			wantReason: "held_prompts", wantDetail: "waiting to close: 1 held prompts have not been delivered"},
		{name: "open children", waits: CloseWaits{Family: &wsm.HasOpenChildrenError{Children: []wsm.FamilyRef{{Name: "a"}, {Name: "b"}}}},
			wantReason: "open_children", wantDetail: "waiting to close: its open children have not closed: a, b"},
		{name: "a workspace landing into it", waits: CloseWaits{Family: landing},
			wantReason: "open_children", wantDetail: "waiting to close: the workspaces landing into it have not closed: lander"},
		{name: "both", waits: CloseWaits{Blocked: &footer.CloseBlocked{Reason: "live_work", Detail: "work is live"}, Family: &wsm.HasOpenChildrenError{Children: []wsm.FamilyRef{{Name: "a"}}}},
			wantReason: "live_work", wantDetail: "waiting to close: work is live; its open children have not closed: a"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.waits.standing()

			// Assert
			if tt.wantDetail == "" {
				if got != nil {
					t.Fatalf("standing = %+v, want none", got)
				}
				return
			}
			if got == nil || !got.Standing || got.Reason != tt.wantReason || got.Detail != tt.wantDetail {
				t.Fatalf("standing = %+v, want reason %q and detail %q", got, tt.wantReason, tt.wantDetail)
			}
		})
	}
}

func TestTheCloseWaitsOnTheWireCarryEveryBlocker(t *testing.T) {
	// Arrange
	waits := CloseWaits{
		Blocked: &footer.CloseBlocked{TurnInFlight: true, LiveWork: 2, HeldPrompts: 1, MergeQueued: true, Detail: "busy"},
		Family:  errOpenChildren,
	}

	// Act
	got := waits.Proto()

	// Assert
	b := got.GetBlocked()
	if !b.GetTurnInFlight() || b.GetLiveWork() != 2 || b.GetHeldPrompts() != 1 || !b.GetMergeQueued() || b.GetSummary() != "busy" {
		t.Fatalf("blocked = %v, want every field carried", b)
	}
	c := got.GetOpenChildren()
	if c.GetParent().GetId() != "w1" || len(c.GetChildren()) != 1 || c.GetChildren()[0].GetId() != "w2" || len(c.GetLandingInto()) != 0 {
		t.Fatalf("open children = %v, want w1's child w2", c)
	}
}

func TestMessageFieldsCarriesEveryScalarFieldWithItsDefault(t *testing.T) {
	// Arrange
	m := &agentreplv1.CloseWorkspaceBlocked{LiveWork: 2, Summary: "busy"}

	// Act
	got := messageFields(m)

	// Assert
	want := map[string]any{"turn_in_flight": false, "live_work": uint32(2), "held_prompts": uint32(0), "merge_queued": false, "summary": "busy"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("messageFields = %v, want %v", got, want)
	}
}

func TestMessageFieldsRefusesANonScalarField(t *testing.T) {
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("messageFields did not panic on a message field")
		}
	}()

	// Act
	messageFields(&agentreplv1.WorkspaceCloseWaits{})
}

// THE REFUSAL AND THE STANDING CLOSE SHARE ONE MAPPING: the `blocked` arm's
// fields are exactly the standing close's blocked evidence, field for field.
func TestTheBlockedRefusalAndTheStandingCloseCarryTheSameEvidence(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.held["w1"] = []wsm.HeldPrompt{{Turn: "held-1"}}
	blocked := &footer.CloseBlocked{HeldPrompts: 1, Detail: "1 held prompts have not been delivered; release or drop them first", Reason: "held_prompts"}

	// Act
	err := f.verbs.Close(context.Background(), "w1", wsm.CloseByUser)

	// Assert
	refusal := asRefusal(t, err, "blocked")
	if want := messageFields(CloseWaits{Blocked: blocked}.Proto().GetBlocked()); !reflect.DeepEqual(refusal.Fields, want) {
		t.Fatalf("refusal fields = %v, want the standing close's evidence %v", refusal.Fields, want)
	}
}
