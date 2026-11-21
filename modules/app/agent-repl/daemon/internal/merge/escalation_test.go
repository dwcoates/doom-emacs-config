package merge

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// bannerRaisedFor reports whether a banner of KIND was raised whose click
// opens workspace WS.
func (h *harness) bannerRaisedFor(kind string, ws ids.WorkspaceID) bool {
	for _, b := range h.banners.seen() {
		if b.kind == kind && b.ws == ws {
			return true
		}
	}
	return false
}

// escalationBriefs is every escalation brief the harness submitted to the
// deploy mediator.
func escalationBriefs(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-fixer", BriefDeployEscalation+" ")
}

// escalatedByTheDaemon holds the owed check escalated with SUMMARY by the
// daemon, as a stopped fix or an undecided turn is: the mediator did not
// say it.
func escalatedByTheDaemon(t *testing.T, h *harness, summary string) {
	t.Helper()
	m := owedCheck(t, h)
	window, err := h.o.toldWindow(context.Background(), mediationKey(h), dlog.Context{})
	if err != nil {
		t.Fatal(err)
	}
	h.o.holdEscalated(context.Background(), queueOf(t, h), *m, summary, nil, window, false, dlog.Context{})
}

func TestEveryEscalationPutsTheDeployBeforeTheUserInTheMediator(t *testing.T) {
	tests := []struct {
		name         string
		act          func(t *testing.T, h *harness)
		wantBriefed  bool
		wantSummary  string
		wantProblems string
	}{
		{name: "the mediator's own escalation is told by the mediator, never briefed back to it",
			act: func(t *testing.T, h *harness) {
				if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}); err != nil {
					t.Fatalf("DecideMediation: %v", err)
				}
			}},
		{name: "a turn that decided nothing", wantBriefed: true,
			act:         func(t *testing.T, h *harness) { h.o.Mediate(context.Background()) },
			wantSummary: "the deploy mediator ended its turn without deciding the deploy", wantProblems: "states no separate findings"},
		{name: "a mediator closed before its fix landed", wantBriefed: true,
			act: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, aFixerTurn); err != nil {
					t.Fatal(err)
				}
				h.o.OnWorkspaceClosed(context.Background(), "ws-fixer")
			}, wantSummary: "the deploy mediator was closed before its fix landed", wantProblems: "states no separate findings"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if !strings.HasPrefix(tt.name, "a turn") {
				withMediator(t, h)
			}

			// Act
			tt.act(t, h)

			// Assert: the check stays owed, holding the queue for the user in
			// the mediator, shown and announced.
			if got := heldEscalation(t, h); got != "ws-fixer" {
				t.Fatalf("the check waits on escalation %q, want the mediator", got)
			}
			if !slices.Contains(h.checks.resumed, "ws-fixer") || !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-fixer") {
				t.Fatalf("resumed %v; want the mediator shown and the user notified with a click opening it", h.checks.resumed)
			}
			subs := escalationBriefs(h)
			if !tt.wantBriefed {
				if len(subs) != 0 {
					t.Fatalf("briefed the mediator %d times with its own escalation, want never", len(subs))
				}
				return
			}
			if len(subs) != 1 {
				t.Fatalf("briefed the escalation %d times, want once", len(subs))
			}
			values := h.briefValues[len(h.briefValues)-1]
			if !strings.Contains(values["summary"], tt.wantSummary) || !strings.Contains(values["problems"], tt.wantProblems) {
				t.Fatalf("brief values %v, want the summary %q and findings %q", values, tt.wantSummary, tt.wantProblems)
			}
		})
	}
}

// TestAnEscalationThatCannotReachTheMediatorStillNotifiesTheUser pins that
// the user's notice never depends on the mediator: it is posted as the
// escalation is recorded, and a telling that fails leaves the check
// escalated and untold for the next pass (2026-11-21).
func TestAnEscalationThatCannotReachTheMediatorStillNotifiesTheUser(t *testing.T) {
	tests := []struct {
		name    string
		byUs    bool
		arrange func(h *harness)
		message string
	}{
		{name: "a mediator that cannot be shown", message: "the deploy mediator could not be shown for its brief; it is not briefed",
			arrange: func(h *harness) { h.checks.resumeErr = errors.New("parent hidden") }},
		{name: "an effort that cannot be set", byUs: true, message: "the deploy mediator's effort could not be set; it is not briefed",
			arrange: func(h *harness) { h.checks.effortErr = errors.New("no catalog") }},
		{name: "a worktree that cannot be refreshed", byUs: true, message: "the deploy mediator's worktree could not be brought to the main branch's tip; it is not briefed",
			arrange: func(h *harness) { h.checks.refreshErr = errors.New("git gone") }},
		{name: "a daemon's brief that cannot be composed", byUs: true, message: "the escalation's brief could not be composed; the mediator is not briefed, and the next pass tells it",
			arrange: func(h *harness) { delete(h.briefs, BriefDeployEscalation) }},
		{name: "a daemon's brief that cannot be submitted", byUs: true, message: "the escalation's brief could not be submitted to the mediator; the next pass tells it",
			arrange: func(h *harness) { h.queue.err = errors.New("queue closed") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			tt.arrange(h)

			// Act
			if tt.byUs {
				escalatedByTheDaemon(t, h, "yours")
			} else if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
				t.Fatalf("DecideMediation = %v", err)
			}

			// Assert: the queue stays held, escalated and untold to the
			// mediator for the next pass to tell, the failed telling is
			// recorded at ERROR, and the user is notified all the same.
			if heldEscalation(t, h) != "" {
				t.Fatal("want the check held, escalated and untold")
			}
			record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message})
			if !ok || record.Context["summary"] != "yours" || record.Context["error"] == nil {
				t.Fatalf("records = %v, want %q at ERROR with the summary and its cause", h.logs.Records(), tt.message)
			}
			if !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-fixer") {
				t.Fatal("the user was not notified of the escalated check")
			}
		})
	}
}

func TestAMadeMediatorThatCannotBeRecordedIsNotAnswered(t *testing.T) {
	// Arrange: no mediator stands yet.
	h := checked(t)
	owe(t, h)
	h.db.mu.Lock()
	h.db.mediatorRecordErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	_, err := h.o.mediatorOf(context.Background(), queueOf(t, h), dlog.Context{})

	// Assert
	if err == nil {
		t.Fatal("mediatorOf = nil, want the failed record answered")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the deploy mediator was made but could not be recorded"}); !ok {
		t.Fatalf("records = %+v, want the failed record at ERROR", h.logs.Records())
	}
}

// heldEscalation answers the deploy mediator the owed check's escalation
// was told to, empty while it is untold, failing unless the check is owed
// and escalated: an escalation holds the queue for the user.
func heldEscalation(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	m := owedCheck(t, h)
	if m == nil || !m.Escalated() {
		t.Fatalf("check %+v, want it owed and escalated, holding the queue", m)
	}
	if !m.EscalationTold {
		return ""
	}
	mediator, found, err := h.db.DeployMediatorOf(context.Background(), mediationKey(h))
	if err != nil || !found {
		t.Fatalf("DeployMediatorOf = (%v, %v), want the mediator the escalation was told to", found, err)
	}
	return mediator.Workspace
}

// aProblemKey is aProblem's one group.
var aProblemKey = wsm.ProblemKey{Operation: "daemon.worktreereap.pass", Message: "boom"}

// problemOf reads the problem KEY of the harness's queue.
func problemOf(t *testing.T, h *harness, key wsm.ProblemKey) wsm.DeployProblem {
	t.Helper()
	problems, err := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range problems {
		if p.Key == key {
			return p
		}
	}
	t.Fatalf("problems %+v, want one for %+v", problems, key)
	return wsm.DeployProblem{}
}

// escalationMade reports whether the mediator took a new escalation: it was
// shown for it.
func escalationMade(h *harness) bool {
	return slices.Contains(h.checks.resumed, "ws-fixer")
}

func TestAnEscalationTakesOnAgainAProblemWhoseFixDeployed(t *testing.T) {
	// Arrange: the mediator took the problem on and its fix deployed, and
	// the problem came back.
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	ctx := context.Background()
	if err := h.db.TakeOnDeployProblems(ctx, mediationKey(h), []wsm.ProblemKey{aProblemKey}, "the location grant", deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.FixDeployProblems(ctx, mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}
	toldWith(t, h, wsm.WindowGroup{Operation: aProblemKey.Operation, Message: aProblemKey.Message, Class: wsm.WindowRecurred, Fixes: 1})

	// Act
	err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "the grant is missing again", Problems: aProblem})

	// Assert
	if got := heldEscalation(t, h); err != nil || got != "ws-fixer" {
		t.Fatalf("DecideMediation = %v, check waits on %q; want the mediator", err, got)
	}
	if p := problemOf(t, h, aProblemKey); p.FixedAt != nil || !strings.Contains(p.Findings, "the location grant") {
		t.Fatalf("problem %+v, want it open again with its earlier findings kept", p)
	}
}

// TestADecisionNeverStopsTheMediator covers the owner's 2026-11-17 ruling: a
// decision command is never cut mid-run, since a cut was saved in the
// mediator's transcript as "User rejected tool use"; and the 2026-11-20
// ruling: its session is never ended at all.
func TestADecisionNeverStopsTheMediator(t *testing.T) {
	tests := []struct {
		name     string
		decision Decision
	}{
		{name: "an escalation", decision: Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem}},
		{name: "a dispatch", decision: Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem}},
		{name: "an approval", decision: Decision{Kind: MediationApprove, Summary: "nothing new"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)

			// Act
			err := decideTold(h, tt.decision)

			// Assert
			if err != nil || len(h.stoppedSessions) != 0 {
				t.Fatalf("DecideMediation = %v, stopped %v; want the decision recorded and the mediator's session left running", err, h.stoppedSessions)
			}
		})
	}
}

func TestADecisionWhileTheDaemonStopsLeavesTheCheckOwed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	if idle := h.o.stopMediation(); idle != nil {
		<-idle
	}

	// Act
	err := decideTold(h, Decision{Kind: MediationApprove, Summary: "nothing new"})

	// Assert
	if !errors.Is(err, errMediationStopped) || owedCheck(t, h) == nil {
		t.Fatalf("DecideMediation = %v, want it refused with the check still owed", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: mediationNotRun}); !ok {
		t.Fatalf("records = %v, want the refused decision recorded", h.logs.Records())
	}
}

// escalatedBy escalates the owed check through its mediator and answers the
// mediator that holds it.
func escalatedBy(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	withMediator(t, h)
	if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours", Problems: aProblem}); err != nil {
		t.Fatalf("DecideMediation: %v", err)
	}
	return heldEscalation(t, h)
}

func TestClosingTheEscalationIsWhatReleasesItsHold(t *testing.T) {
	tests := []struct {
		name         string
		closed       ids.WorkspaceID
		wantReleased bool
	}{
		{name: "the user closes the mediator the escalation waits on", closed: "ws-fixer", wantReleased: true},
		{name: "another workspace closing leaves the hold", closed: theWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			escalatedBy(t, h)

			// Act
			h.o.OnWorkspaceClosed(context.Background(), tt.closed)

			// Assert
			if released := owedCheck(t, h) == nil; released != tt.wantReleased {
				t.Fatalf("released %v, want %v", released, tt.wantReleased)
			}
			_, recorded := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "the user closed the deploy mediator holding the escalation; its check is released and the queue advances"})
			if recorded != tt.wantReleased {
				t.Fatalf("records = %v, want the release recorded %v", h.logs.Records(), tt.wantReleased)
			}
		})
	}
}

func TestTheMediatorsOwnFixDeployingEndsItsEscalation(t *testing.T) {
	// Arrange: the check is escalated, and the mediator's finish lands.
	h := checked(t)
	owe(t, h)
	esc := escalatedBy(t, h)
	ctx := context.Background()
	landed := deployStarted.Add(30 * time.Second)
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: esc, Repo: mediationKey(h), StartedAt: landed}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, esc, wsm.RemediationGating, wsm.RemediationLanding, landed); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, esc, tipAfter, nil, landed); err != nil {
		t.Fatal(err)
	}

	// Act: the mediator's fix deploys.
	oweAt(t, h, deployStarted.Add(time.Minute))

	// Assert
	m := owedCheck(t, h)
	if m == nil || m.Escalated() || m.Prompted() {
		t.Fatalf("check %+v, want the fix's deploy owed afresh, unescalated and untold", m)
	}
}

func TestAnUnrelatedDeployLeavesTheEscalationsHold(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	esc := escalatedBy(t, h)

	// Act
	oweAt(t, h, deployStarted.Add(time.Minute))
	h.o.Mediate(context.Background())

	// Assert: still held by the same escalation, and the mediator not told
	// of the newer deploy.
	if got := heldEscalation(t, h); got != esc || len(intakeTellings(h)) != 0 {
		t.Fatalf("check waits on %q, mediator told %d times; want the hold kept and no check told", got, len(intakeTellings(h)))
	}
}

func TestAnEscalationThatCouldNotBeToldIsToldByTheNextPass(t *testing.T) {
	// Arrange: the mediator could not be readied for the escalation.
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	h.checks.refreshErr = errors.New("git gone")
	escalatedByTheDaemon(t, h, "the reaper errors on every pass")
	if heldEscalation(t, h) != "" {
		t.Fatal("the check is told of an escalation the mediator was never readied for")
	}
	h.checks.mu.Lock()
	h.checks.refreshErr = nil
	h.checks.mu.Unlock()

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if got := heldEscalation(t, h); got != "ws-fixer" {
		t.Fatalf("check waits on %q, want the escalation told by the pass", got)
	}
	if subs := escalationBriefs(h); len(subs) != 1 || !strings.Contains(submittedText(subs[0]), "the reaper errors on every pass") {
		t.Fatalf("briefed %v, want the escalation briefed once with the kept findings", subs)
	}
	// THE USER WAS NOTIFIED AS THE ESCALATION WAS RECORDED, and the pass's
	// telling notifies no one a second time.
	if n := bannersOf(h, desktopnotify.KindDeployMediated); n != 1 {
		t.Fatalf("raised %d escalation notices, want the one raised as it was recorded", n)
	}
}

// bannersOf counts the banners of KIND the harness raised.
func bannersOf(h *harness, kind string) int {
	n := 0
	for _, b := range h.banners.seen() {
		if b.kind == kind {
			n++
		}
	}
	return n
}

func TestTheMediatorIsNeverToldWhatItSaidItself(t *testing.T) {
	// Arrange: the mediator took a problem on with an earlier escalation,
	// and its own check escalates it again.
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	if err := h.db.TakeOnDeployProblems(context.Background(), mediationKey(h), []wsm.ProblemKey{aProblemKey}, "the location grant", deployStarted); err != nil {
		t.Fatal(err)
	}
	toldWith(t, h, wsm.WindowGroup{Operation: aProblemKey.Operation, Message: aProblemKey.Message, Class: wsm.WindowOwned})

	// Act
	err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "the grant is missing", Problems: aProblem})

	// Assert: held by the mediator, no brief prompted to it, and the
	// problem still open.
	if got := heldEscalation(t, h); err != nil || got != "ws-fixer" {
		t.Fatalf("DecideMediation = %v, check waits on %q; want the mediator", err, got)
	}
	if briefs := escalationBriefs(h); len(briefs) != 0 {
		t.Fatalf("briefs %d; want the mediator told nothing it said", len(briefs))
	}
	if p := problemOf(t, h, aProblemKey); p.FixedAt != nil {
		t.Fatalf("problem %+v, want it open", p)
	}
}

func TestTheMediatorsOwnEscalationOnlyShowsIt(t *testing.T) {
	// Arrange: the mediator is mid-turn, deciding its own check.
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	refreshed, efforts := len(h.checks.refreshed), len(h.checks.efforts)

	// Act
	err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "pages regressed", Problems: aProblem})

	// Assert: shown, but its worktree and effort are left as its turn has them.
	if err != nil || !slices.Contains(h.checks.resumed, "ws-fixer") {
		t.Fatalf("DecideMediation = %v, resumed %v; want the mediator shown", err, h.checks.resumed)
	}
	if len(h.checks.refreshed) != refreshed || len(h.checks.efforts) != efforts {
		t.Fatalf("refreshed %v, efforts %v; want neither touched mid-turn", h.checks.refreshed, h.checks.efforts)
	}
}

func TestShowMediatorAnswersAMediatorThatCannotBeShown(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.checks.resumeErr = errors.New("parent hidden")

	// Act
	_, err := h.o.showMediator(context.Background(), queueOf(t, h), dlog.Context{})

	// Assert
	if err == nil {
		t.Fatal("showMediator = nil, want the failure answered")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the deploy mediator could not be shown for its brief; it is not briefed"}); !ok {
		t.Fatalf("records = %v, want the show failure at ERROR", h.logs.Records())
	}
	if len(h.checks.refreshed) != 0 || len(h.checks.efforts) != 0 {
		t.Fatalf("refreshed %v, efforts %v; want nothing else readied", h.checks.refreshed, h.checks.efforts)
	}
}

func TestAnEscalationTheDaemonsExitCutIsRecordedAtInfo(t *testing.T) {
	// Arrange: the daemon's exit cuts the mediation's lifetime while the
	// mediator is readied for the escalation.
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.checks.onEffort = cancel
	h.checks.effortErr = context.Canceled
	m := owedCheck(t, h)

	// Act
	h.o.holdEscalated(ctx, queueOf(t, h), *m, "yours", nil, wsm.CheckWindow{}, false, dlog.Context{})

	// Assert: held for the next pass, and no ERROR for the cut.
	if heldEscalation(t, h) != "" {
		t.Fatal("want the check held, escalated and untold")
	}
	for _, r := range h.logs.Records() {
		if r.Level == "error" && r.Operation == opMediation {
			t.Fatalf("ERROR record %+v, want the cut recorded at INFO", r)
		}
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "the escalated check was not told to the mediator; the queue stays held, and the next pass tells it again"}); !ok {
		t.Fatalf("records = %v, want the held check recorded at INFO", h.logs.Records())
	}
}

// assertBriefFailureEscalated asserts that a check whose brief could not be
// composed, WHAT failing, was escalated: it is still owed and holds the
// queue as an escalation naming WHAT and CAUSE, the user was notified, the
// failure is recorded at ERROR, and the intake was never told of the deploy.
func assertBriefFailureEscalated(t *testing.T, h *harness, what, cause string) {
	t.Helper()
	if heldEscalation(t, h) == "" {
		t.Fatalf("check %+v, want its escalation told", owedCheck(t, h))
	}
	if brief := owedCheck(t, h).EscalationBrief; !strings.Contains(brief, "the deploy check could not run: "+what) || !strings.Contains(brief, cause) {
		t.Fatalf("the escalation says %q, want it to name %q and its cause %q", brief, what, cause)
	}
	if !h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatal("the user was not told the check could not run")
	}
	record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: what + "; the check is escalated to the user"})
	if !ok || !strings.Contains(fmt.Sprint(record.Context["error"]), cause) {
		t.Fatalf("records = %v, want the failed brief at ERROR with its cause %q", h.logs.Records(), cause)
	}
	if len(intakeTellings(h)) != 0 {
		t.Fatalf("told the intake of the deploy %d times, want it never told", len(intakeTellings(h)))
	} // THE MEDIATOR IS TOLD THE CHECK COULD NOT RUN, naming what failed and
	// why, never that the deploy was escalated for something found.
	notRun := submissionsTo(h, "ws-fixer", BriefDeployCheckNotRun+" ")
	if len(notRun) != 1 || !strings.Contains(submittedText(notRun[0]), what) || !strings.Contains(submittedText(notRun[0]), cause) {
		t.Fatalf("the mediator was sent %v, want one brief that the check could not run, naming %q and %q", h.queue.submissions, what, cause)
	}
	if len(escalationBriefs(h)) != 0 {
		t.Fatalf("the mediator was sent %d escalation briefs, want none for a check that could not run", len(escalationBriefs(h)))
	}
}

// TestTheMediatorApprovesACheckThatCouldNotRun pins that a check escalated
// because its brief could not be composed, and so never told to the mediator
// as a check, takes the mediator's approval: on 2026-11-20 that approval was
// refused as a check the mediator was never told of, and the queue stayed
// held for the user.
func TestTheMediatorApprovesACheckThatCouldNotRun(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return nil, errors.New("logs.sh exited 2") }
	h.o.Mediate(context.Background())
	if m := owedCheck(t, h); m == nil || !m.Escalated() || m.Prompted() {
		t.Fatalf("check %+v, want it escalated and never prompted", m)
	}

	asked := h.o.deps.Now()

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationApprove, Summary: "it broke nothing"})

	// Assert
	if err != nil {
		t.Fatalf("DecideMediation = %v, want the approval accepted", err)
	}
	if m := owedCheck(t, h); m != nil {
		t.Fatalf("check %+v, want it released", m)
	} // THE NEXT CHECK'S WINDOW STARTS AT THE APPROVAL, never where the last
	// readable check ended: the records this check could not read are behind it.
	through, found, err := h.db.CheckedThrough(context.Background(), mediationKey(h))
	if err != nil || !found || through.Before(asked) {
		t.Fatalf("CheckedThrough = (%v, %v, %v), want the checks read through the approval, asked at %v", through, found, err, asked)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "the mediator approved a check that could not run; its window is recorded as read, and the next check's starts here"}); !ok {
		t.Fatalf("records = %v, want the window recorded as read", h.logs.Records())
	}
}

// TestAnApprovalOfACheckThatCouldNotRunFailsWhenItsWindowCannotBeRecorded
// pins the error path: the approval is not recorded and the check stays owed.
func TestAnApprovalOfACheckThatCouldNotRunFailsWhenItsWindowCannotBeRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return nil, errors.New("logs.sh exited 2") }
	h.o.Mediate(context.Background())
	h.db.windowErr = errors.New("disk gone")

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationApprove, Summary: "it broke nothing"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("DecideMediation = %v, want the failed record surfaced", err)
	}
	if m := owedCheck(t, h); m == nil || !m.Escalated() {
		t.Fatalf("check %+v, want it still owed and escalated", m)
	}
	record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the window of a check that could not run could not be recorded as read; the approval is not recorded and the queue stays held"})
	if !ok || !strings.Contains(fmt.Sprint(record.Context["error"]), "disk gone") {
		t.Fatalf("records = %v, want the failure at ERROR with its cause", h.logs.Records())
	}
}

// TestACheckThatCouldNotRunIsToldToTheMediatorAsItStands pins the owner's
// 2026-11-21 ruling: every failed round tells the mediator what failed and
// why, verbatim. It is shown and never readied for it, since readying it may
// be the very step that failed, and it is told whether or not it was told of
// the check before: the brief is queued behind any turn it runs.
func TestACheckThatCouldNotRunIsToldToTheMediatorAsItStands(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{name: "a mediator whose worktree cannot be refreshed", arrange: func(t *testing.T, h *harness) {
			h.checks.refreshErr = errors.New("git gone")
		}},
		{name: "a mediator whose effort cannot be set", arrange: func(t *testing.T, h *harness) {
			h.checks.effortErr = errors.New("the selector served [AGENT_EFFORT_LEVEL_MEDIUM] and never offered AGENT_EFFORT_LEVEL_LOW")
		}},
		{name: "a mediator told of the check already", arrange: func(t *testing.T, h *harness) {
			told(t, h, deployStarted, "turn-told")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			tt.arrange(t, h)
			const what, cause = "the deploy mediator could not be found, made or readied for the check", "the shim said no"
			m := owedCheck(t, h)

			// Act
			h.o.roundFailed(context.Background(), queueOf(t, h), *m, what, errors.New(cause), dlog.Context{"repo": string(mediationKey(h))})

			// Assert
			if got := heldEscalation(t, h); got != "ws-fixer" {
				t.Fatalf("the check waits on escalation %q, want it told to the mediator", got)
			}
			notRun := submissionsTo(h, "ws-fixer", BriefDeployCheckNotRun+" ")
			if len(notRun) != 1 || !strings.Contains(submittedText(notRun[0]), what) || !strings.Contains(submittedText(notRun[0]), cause) {
				t.Fatalf("the mediator was sent %v, want one brief that the check could not run naming %q and %q", h.queue.submissions, what, cause)
			}
			if !slices.Contains(h.checks.resumed, "ws-fixer") || !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-fixer") {
				t.Fatalf("resumed %v; want the mediator shown and the user notified with a click opening it", h.checks.resumed)
			}
			if len(h.checks.refreshed) != 0 || len(h.checks.efforts) != 0 {
				t.Fatalf("refreshed %v, efforts %v; want the mediator told as it stands", h.checks.refreshed, h.checks.efforts)
			}
		})
	}
}

// TestAnEscalationWithNoMediatorRecordedNotifiesNoOneAndIsAnError pins the
// broken-invariant branch of the user's notice: a notice names the recorded
// mediator, so an escalation recorded with none is recorded at ERROR and
// notifies no one rather than a guessed workspace.
func TestAnEscalationWithNoMediatorRecordedNotifiesNoOneAndIsAnError(t *testing.T) {
	// Arrange: no mediator is recorded, and none can be made.
	h := checked(t)
	owe(t, h)
	h.checks.createErr = errors.New("worktree gone")

	// Act
	h.o.holdEscalated(context.Background(), queueOf(t, h), *owedCheck(t, h), "yours", nil, wsm.CheckWindow{}, false, dlog.Context{"repo": string(mediationKey(h))})

	// Assert
	if h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatal("a notice was raised with no mediator recorded to name it")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the check is held with no deploy mediator recorded to name its notice; the user is not notified of it"}); !ok {
		t.Fatalf("records = %v, want the broken invariant at ERROR", h.logs.Records())
	}
}

// notRunValues answers the values the one brief that the check could not
// run was composed with.
func notRunValues(t *testing.T, h *harness) map[string]string {
	t.Helper()
	h.mu.Lock()
	defer h.mu.Unlock()
	var out []map[string]string
	for i, name := range h.briefNames {
		if name == BriefDeployCheckNotRun {
			out = append(out, h.briefValues[i])
		}
	}
	if len(out) != 1 {
		t.Fatalf("composed %d briefs that the check could not run, want one", len(out))
	}
	return out[0]
}

// TestTheCheckNotRunBriefSaysWhetherTheMediatorWasTold pins the two
// wordings of the brief's {{told_standing}} (2026-11-21): a failure before
// the mediator was told says nothing has looked at the deploy, and one after
// it (its turn's await lost) says the mediator decides the check itself.
func TestTheCheckNotRunBriefSaysWhetherTheMediatorWasTold(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{name: "a brief that could not be submitted, before the mediator was told", want: "before you were told of it, so nothing has looked at that deploy yet.",
			arrange: func(h *harness) {
				onBriefThenNext(h, BriefDeployIntake+" ", setQueueErr(h, errors.New("queue closed")), setQueueErr(h, nil))
			}},
		{name: "an await of the turn the mediator was told in, lost", want: "after you were told of it, so agent-repl no longer follows what you do with the check: decide it yourself as below.",
			arrange: func(h *harness) { h.awaitErr = errors.New("the session watcher is gone") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			tt.arrange(h)

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if got := notRunValues(t, h)["told_standing"]; got != tt.want {
				t.Fatalf("told_standing = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestAnEscalationThatCannotBeRecordedStillNotifiesTheUser pins the hold
// nothing would decide: the escalation's write failed, so no escalation
// stands for the mediator to approve or the user to release, and the user
// is notified the check is held and why.
func TestAnEscalationThatCannotBeRecordedStillNotifiesTheUser(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	setDB(h, func(db *fakeDB) { db.escalateErr = errors.New("database is locked") })()

	// Act
	escalatedByTheDaemon(t, h, "the reaper errors on every pass")

	// Assert
	if m := owedCheck(t, h); m == nil || m.Escalated() {
		t.Fatalf("check %+v, want it owed and not escalated", m)
	}
	record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the check could not be recorded as escalated; nothing is escalated and the queue stays held"})
	if !ok || record.Context["error"] != "database is locked" {
		t.Fatalf("records = %v, want the failed write at ERROR with its cause", h.logs.Records())
	}
	if !h.bannerRaisedFor(desktopnotify.KindDeployMediated, "ws-fixer") {
		t.Fatal("the user was not notified of the held check")
	}
	if b := h.banners.seen(); !strings.Contains(fmt.Sprint(b), "could not be escalated (database is locked)") {
		t.Fatalf("banners %+v, want the notice to say why the check is held", b)
	}
}
