package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// queueOf answers the harness's checked queue.
func queueOf(t *testing.T, h *harness) checkedQueue {
	t.Helper()
	q, ok, err := h.o.checkedQueueOf(context.Background(), mediationKey(h))
	if err != nil || !ok {
		t.Fatalf("checkedQueueOf = (%v, %v)", ok, err)
	}
	return q
}

// standingIn records WS, open or CLOSED, as the harness queue's standing
// workspace in ROLE, briefed.
func standingIn(t *testing.T, h *harness, role wsm.DeployCheckRoleName, ws ids.WorkspaceID, closed bool) {
	t.Helper()
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaces[ws] = wsm.Workspace{ID: ws, Name: "deploy-" + string(role), Repo: theRepository, Closed: closed}
	h.db.mu.Unlock()
	if err := h.db.RecordStandingDeployCheck(ctx, mediationKey(h), role, ws); err != nil {
		t.Fatal(err)
	}
	if err := h.db.MarkStandingBriefed(ctx, mediationKey(h), role); err != nil {
		t.Fatal(err)
	}
}

func TestMediatorOfAnswersTheQueuesOne(t *testing.T) {
	tests := []struct {
		name         string
		closed       bool
		wantReopened []ids.WorkspaceID
	}{
		{name: "an open one is answered as it is"},
		{name: "a closed one is reopened", closed: true, wantReopened: []ids.WorkspaceID{"ws-standing"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			standingIn(t, h, wsm.StandingMediator, "ws-standing", tt.closed)

			// Act
			got, err := h.o.mediatorOf(context.Background(), queueOf(t, h), dlog.Context{})

			// Assert
			if err != nil || got != (standing{ws: "ws-standing", briefed: true}) || len(h.checks.made) != 0 || !slices.Equal(h.checks.reopened, tt.wantReopened) {
				t.Fatalf("mediatorOf = (%+v, %v), made %v, reopened %v; want the standing one", got, err, h.checks.made, h.checks.reopened)
			}
		})
	}
}

func TestMediatorOfMakesOneForAQueueWithNone(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{name: "a queue with no mediator recorded", arrange: func(*testing.T, *harness) {}},
		{name: "a queue whose recorded mediator is gone from the registry", arrange: func(t *testing.T, h *harness) {
			if err := h.db.RecordStandingDeployCheck(context.Background(), mediationKey(h), wsm.StandingMediator, "ws-gone"); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			tt.arrange(t, h)

			// Act
			got, err := h.o.mediatorOf(context.Background(), queueOf(t, h), dlog.Context{})

			// Assert
			if err != nil || got != (standing{ws: "ws-fixer"}) || !slices.Equal(h.checks.made, []string{mediatorName}) {
				t.Fatalf("mediatorOf = (%+v, %v), made %v; want %s made", got, err, h.checks.made, mediatorName)
			}
			recorded, found, readErr := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingMediator)
			if readErr != nil || !found || recorded.Workspace != got.ws {
				t.Fatalf("standing record = (%+v, %v, %v), want the made mediator", recorded, found, readErr)
			}
			if _, lands, err := h.db.DeployEscalationOf(context.Background(), got.ws); err != nil || !lands {
				t.Fatalf("DeployEscalationOf = (%v, %v), want the made mediator recorded as landing directly", lands, err)
			}
		})
	}
}

func TestMediatorOfAnswersWhatItCannotDo(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		message string
	}{
		{name: "a mediator that cannot be read", message: "could not read the deploy mediator",
			arrange: func(t *testing.T, h *harness) {
				standingIn(t, h, wsm.StandingMediator, "ws-standing", false)
				h.db.mu.Lock()
				h.db.workspaceErr = errors.New("database is locked")
				h.db.mu.Unlock()
			}},
		{name: "a closed one that cannot be reopened", message: "the deploy mediator could not be reopened",
			arrange: func(t *testing.T, h *harness) {
				standingIn(t, h, wsm.StandingMediator, "ws-standing", true)
				h.checks.reopenErr = errors.New("branch gone")
			}},
		{name: "one that cannot be made", message: "the deploy mediator could not be made",
			arrange: func(t *testing.T, h *harness) { h.checks.createErr = errors.New("disk full") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			q := queueOf(t, h)
			tt.arrange(t, h)

			// Act
			_, err := h.o.mediatorOf(context.Background(), q, dlog.Context{})

			// Assert
			if err == nil {
				t.Fatal("mediatorOf = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestReadyMediatorRefreshesSetsItsEffortAndShowsItOnlyWhenAsked(t *testing.T) {
	tests := []struct {
		name        string
		show        bool
		wantResumed []ids.WorkspaceID
	}{
		{name: "a check's brief leaves it hidden"},
		{name: "an escalation's brief shows it", show: true, wantResumed: []ids.WorkspaceID{"ws-fixer"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)

			// Act
			got, err := h.o.readyMediator(context.Background(), queueOf(t, h), tt.show, dlog.Context{})

			// Assert: the standing is recorded before the effort starts its shim.
			if err != nil || got.ws != "ws-fixer" || !slices.Equal(h.checks.refreshed, []ids.WorkspaceID{"ws-fixer"}) ||
				!slices.Equal(h.checks.efforts, []string{"mediator:ws-fixer:standing=true"}) || !slices.Equal(h.checks.resumed, tt.wantResumed) {
				t.Fatalf("readyMediator = (%+v, %v), refreshed %v, efforts %v, resumed %v", got, err, h.checks.refreshed, h.checks.efforts, h.checks.resumed)
			}
		})
	}
}

func TestReadyMediatorAnswersWhatItCannotDo(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "a worktree that cannot be refreshed", message: "the deploy mediator's worktree could not be brought to the main branch's tip; it is not briefed",
			arrange: func(h *harness) { h.checks.refreshErr = errors.New("rebase conflicts") }},
		{name: "an effort that cannot be set", message: "the deploy mediator's effort could not be set; it is not briefed",
			arrange: func(h *harness) { h.checks.effortErr = errors.New("no selector") }},
		{name: "a mediator that cannot be shown", message: "the deploy mediator could not be shown for its brief; it is not briefed",
			arrange: func(h *harness) { h.checks.resumeErr = errors.New("parent hidden") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			tt.arrange(h)

			// Act
			_, err := h.o.readyMediator(context.Background(), queueOf(t, h), true, dlog.Context{})

			// Assert
			if err == nil {
				t.Fatal("readyMediator = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestTheRetiredIntakeIsClosedAndForgotten(t *testing.T) {
	tests := []struct {
		name       string
		closed     bool
		toldIt     bool
		wantClosed []ids.WorkspaceID
		wantKept   bool
	}{
		{name: "an open intake is closed and forgotten", wantClosed: []ids.WorkspaceID{"ws-old-intake"}},
		{name: "a closed intake is only forgotten", closed: true},
		{name: "an intake still told the owed check waits for its decision", toldIt: true, wantKept: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			standingIn(t, h, wsm.StandingIntake, "ws-old-intake", tt.closed)
			if tt.toldIt {
				if ok, err := h.db.RecordMediationIntake(context.Background(), mediationKey(h), deployStarted, "ws-old-intake"); err != nil || !ok {
					t.Fatalf("RecordMediationIntake = (%v, %v)", ok, err)
				}
			}

			// Act
			h.o.retireStandingIntake(context.Background(), queueOf(t, h), owedCheck(t, h))

			// Assert
			_, kept, err := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingIntake)
			if err != nil || kept != tt.wantKept || !slices.Equal(h.checks.closed, tt.wantClosed) {
				t.Fatalf("intake kept %v (%v), closed %v; want kept %v, closed %v", kept, err, h.checks.closed, tt.wantKept, tt.wantClosed)
			}
		})
	}
}

func TestARetiredIntakeThatCannotBeClosedIsKeptAndRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	standingIn(t, h, wsm.StandingIntake, "ws-old-intake", false)
	h.checks.closeErr = errors.New("worktree busy")

	// Act
	h.o.retireStandingIntake(context.Background(), queueOf(t, h), owedCheck(t, h))

	// Assert
	if _, kept, _ := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingIntake); !kept {
		t.Fatal("the intake was forgotten though it could not be closed; want it kept for the next pass")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the retired intake could not be closed; it is retired at the next pass"}); !ok {
		t.Fatalf("records = %v, want the failure recorded", h.logs.Records())
	}
}

// approvedChecks has the harness's standing intake approve N deploys in a
// row, each a minute after the last, from deployStarted.
func approvedChecks(t *testing.T, h *harness, n int) {
	t.Helper()
	for i := range n {
		at := deployStarted.Add(time.Duration(i) * time.Minute)
		oweAt(t, h, at)
		withIntake(t, h, at)
		if err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatalf("DecideMediation: %v", err)
		}
	}
}

func TestAQuietRunAfterWorkCompactsTheMediator(t *testing.T) {
	tests := []struct {
		name  string
		work  bool
		quiet int
		want  []ids.WorkspaceID
	}{
		{name: "three quiet checks after work compact the mediator", work: true, quiet: 3, want: []ids.WorkspaceID{"ws-fixer"}},
		{name: "two quiet checks after work compact nothing yet", work: true, quiet: 2},
		{name: "quiet checks with no work before compact nothing", quiet: 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			standingIn(t, h, wsm.StandingMediator, "ws-fixer", false)
			if tt.work {
				if err := h.db.NoteDeployCheckWork(context.Background(), mediationKey(h)); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			approvedChecks(t, h, tt.quiet)

			// Assert
			if !slices.Equal(h.checks.compacted, tt.want) {
				t.Fatalf("compacted %v, want %v", h.checks.compacted, tt.want)
			}
		})
	}
}

func TestADispatchArmsTheCompaction(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	if err := dispatched(h, Decision{}); err != nil {
		t.Fatal(err)
	}
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
	withIntake(t, h, deployStarted.Add(time.Minute))

	// Act: the fix's deploy and two more are approved.
	if err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
		t.Fatal(err)
	}
	for i := 2; i <= 3; i++ {
		at := deployStarted.Add(time.Duration(i) * time.Minute)
		oweAt(t, h, at)
		withIntake(t, h, at)
		if err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatal(err)
		}
	}

	// Assert: the standing remediation the dispatch made is owed its compaction.
	if !slices.Contains(h.checks.compacted, "ws-fixer") {
		t.Fatalf("compacted %v, want the standing fixer's", h.checks.compacted)
	}
}

func TestReleaseEscalation(t *testing.T) {
	tests := []struct {
		name      string
		escalated bool
		wantArm   string
	}{
		{name: "an escalated check is released and the queue advances", escalated: true},
		{name: "a queue no escalation holds is refused", wantArm: ArmNoEscalationHeld},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			if tt.escalated {
				if err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := h.o.ReleaseEscalation(context.Background(), theScope)

			// Assert
			if tt.wantArm != "" {
				var refused *RefusalError
				if !errors.As(err, &refused) || refused.Arm != tt.wantArm || owedCheck(t, h) == nil {
					t.Fatalf("ReleaseEscalation = %v, want the %s refusal and the check still owed", err, tt.wantArm)
				}
				return
			}
			if err != nil || owedCheck(t, h) != nil {
				t.Fatalf("ReleaseEscalation = %v, check %+v; want it released", err, owedCheck(t, h))
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "the user released the escalation's hold; its check is released and the queue advances"}); !ok {
				t.Fatalf("records = %v, want the release recorded", h.logs.Records())
			}
			if h.checks.closed != nil && slices.Contains(h.checks.closed, "ws-fixer") {
				t.Fatal("the release closed the escalation workspace; it stays open")
			}
		})
	}
}

func TestTheChecksWorkspacesAreAdoptedAsTheStandingOnes(t *testing.T) {
	// Arrange: a check escalated to a workspace made before the standing ones.
	h := checked(t)
	owe(t, h)
	// The check was told to an intake before the mediator stood.
	if ok, err := h.db.RecordMediationIntake(context.Background(), mediationKey(h), deployStarted, "ws-old-intake"); err != nil || !ok {
		t.Fatalf("RecordMediationIntake = (%v, %v)", ok, err)
	}
	if _, err := h.db.EscalateMediation(context.Background(), mediationKey(h), deployStarted, "{}"); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordMediationEscalation(context.Background(), mediationKey(h), deployStarted, "ws-old-escalation"); err != nil {
		t.Fatal(err)
	}
	m := owedCheck(t, h)

	// Act
	h.o.adoptStanding(context.Background(), queueOf(t, h), m)

	// Assert
	got, found, err := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingMediator)
	if err != nil || !found || got != (wsm.StandingDeployCheck{Workspace: "ws-old-escalation", Briefed: true}) {
		t.Fatalf("mediator = (%+v, %v, %v), want the escalation adopted, briefed", got, found, err)
	}
	if _, intake, _ := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingIntake); intake {
		t.Fatal("the check's intake was adopted; the intake is retired")
	}
}

func TestAdoptionKeepsAStandingWorkspaceTheQueueHasAlready(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	standingIn(t, h, wsm.StandingMediator, "ws-standing", false)
	if _, err := h.db.EscalateMediation(context.Background(), mediationKey(h), deployStarted, "{}"); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordMediationEscalation(context.Background(), mediationKey(h), deployStarted, "ws-old-escalation"); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.adoptStanding(context.Background(), queueOf(t, h), owedCheck(t, h))

	// Assert
	if got, _, _ := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingMediator); got.Workspace != "ws-standing" {
		t.Fatalf("mediator = %+v, want the one the queue had", got)
	}
}

// oneOffEscalation records WS as an escalation made for the harness queue,
// open or CLOSED.
func oneOffEscalation(t *testing.T, h *harness, ws ids.WorkspaceID, closed bool) {
	t.Helper()
	h.db.mu.Lock()
	h.db.workspaces[ws] = wsm.Workspace{ID: ws, Name: "deploy-escalation-" + string(ws), Repo: theRepository, Closed: closed}
	h.db.mu.Unlock()
	if err := h.db.RecordDeployEscalation(context.Background(), ws, mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}
}

func TestEarlierEscalationsAreRetiredOnceTheStandingOneExists(t *testing.T) {
	tests := []struct {
		name        string
		standing    bool
		held        ids.WorkspaceID
		wantRetired []ids.WorkspaceID
	}{
		{name: "an open earlier escalation is retired", standing: true, wantRetired: []ids.WorkspaceID{"ws-a"}},
		{name: "the escalation holding the queue is kept", standing: true, held: "ws-a"},
		{name: "nothing is retired before the standing one exists"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			oneOffEscalation(t, h, "ws-a", false)
			oneOffEscalation(t, h, "ws-closed", true)
			if tt.standing {
				standingIn(t, h, wsm.StandingMediator, "ws-fixer", false)
				oneOffEscalation(t, h, "ws-fixer", false)
			}
			m := &wsm.Mediation{Escalation: tt.held}

			// Act
			h.o.retireOneOffEscalations(context.Background(), queueOf(t, h), m)

			// Assert
			if !slices.Equal(h.checks.closed, tt.wantRetired) {
				t.Fatalf("closed %v, want %v", h.checks.closed, tt.wantRetired)
			}
		})
	}
}

func TestAnEarlierEscalationThatCannotBeRetiredIsRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	oneOffEscalation(t, h, "ws-a", false)
	standingIn(t, h, wsm.StandingMediator, "ws-fixer", false)
	h.checks.closeErr = errors.New("database is locked")

	// Act
	h.o.retireOneOffEscalations(context.Background(), queueOf(t, h), nil)

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "an earlier escalation could not be retired"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestAnAdoptedRemediationLandsDirectlyAsTheFixer(t *testing.T) {
	// Arrange: a check dispatched to a remediation made before the fixer.
	h := checked(t)
	owe(t, h)
	// The check was told to an intake before the mediator stood.
	if ok, err := h.db.RecordMediationIntake(context.Background(), mediationKey(h), deployStarted, "ws-old-intake"); err != nil || !ok {
		t.Fatalf("RecordMediationIntake = (%v, %v)", ok, err)
	}
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-old-remedy", aFixerTurn); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.adoptStanding(context.Background(), queueOf(t, h), owedCheck(t, h))

	// Assert
	got, found, err := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingMediator)
	repo, lands, landErr := h.db.DeployEscalationOf(context.Background(), "ws-old-remedy")
	if err != nil || !found || got.Workspace != "ws-old-remedy" || landErr != nil || !lands || repo != mediationKey(h) {
		t.Fatalf("fixer (%+v, %v, %v), lands directly (%v, %v); want the remediation adopted, landing directly", got, found, err, lands, landErr)
	}
}

func TestTheStandingFixerStaysOpenAfterItsFixDeploys(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.db.mu.Lock()
	h.db.workspaces["ws-fixer"] = wsm.Workspace{ID: "ws-fixer", Name: "deploy-fixer", Repo: theRepository, StandingDeployCheck: true}
	h.db.mu.Unlock()

	// Act
	err := h.o.closeRemediation(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h)}, dlog.Context{})

	// Assert
	h.db.mu.Lock()
	record := h.db.workspaces["ws-fixer"]
	h.db.mu.Unlock()
	if err != nil || record.Closed {
		t.Fatalf("closeRemediation = %v, closed %v; want the fixer left open", err, record.Closed)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediationFinish,
		Message: "the landed fix deployed; the standing fixer stays open for the next brief"}); !ok {
		t.Fatalf("records = %v, want the fixer kept open recorded", h.logs.Records())
	}
}

func TestNoteDeployEndingRecordsTheEndingOnTheOwedCheck(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)

	// Act
	err := h.o.NoteDeployEnding(context.Background(), deployStarted, wsm.DeployStoppedShort)

	// Assert
	if m := owedCheck(t, h); err != nil || m == nil || m.DeployEnding != wsm.DeployStoppedShort {
		t.Fatalf("NoteDeployEnding = %v, check %+v; want it stopped short", err, m)
	}
}

func TestRenderEndingSaysHowTheDeployEnded(t *testing.T) {
	tests := []struct {
		ending wsm.DeployEnding
		want   string
	}{
		{wsm.DeployFinished, "It finished: every component built and rolled out."},
		{wsm.DeployUnchanged, "It finished, changing nothing."},
		{wsm.DeployStoppedShort, "It STOPPED SHORT of finishing"},
		{"", "How it ended was not recorded"},
	}
	for _, tt := range tests {
		t.Run(string(tt.ending), func(t *testing.T) {
			// Act / Assert
			if got := RenderEnding(tt.ending); !strings.HasPrefix(got, tt.want) {
				t.Fatalf("RenderEnding(%q) = %q, want it to open %q", tt.ending, got, tt.want)
			}
		})
	}
}

func TestTheIntakeIsToldHowItsDeployEnded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	if err := h.o.NoteDeployEnding(context.Background(), deployStarted, wsm.DeployStoppedShort); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	var told string
	for _, values := range h.briefValues {
		if v, ok := values["ending"]; ok {
			told = v
		}
	}
	if !strings.HasPrefix(told, "It STOPPED SHORT") {
		t.Fatalf("the intake's brief ended %q, want it told the deploy stopped short", told)
	}
}

func TestAnEndingThatCannotBeRecordedIsAnsweredAndRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.db.mu.Lock()
	h.db.endingErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.NoteDeployEnding(context.Background(), deployStarted, wsm.DeployFinished)

	// Assert
	if err == nil {
		t.Fatal("NoteDeployEnding = nil, want the failure answered")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "could not record how the deploy ended on its check; the intake is told without it"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

// fixerInSight records "ws-fixer" as the harness queue's standing fixer,
// HIDDEN or in sight.
func fixerInSight(t *testing.T, h *harness, hidden bool) {
	t.Helper()
	standingIn(t, h, wsm.StandingMediator, "ws-fixer", false)
	h.db.mu.Lock()
	record := h.db.workspaces["ws-fixer"]
	record.Hidden = hidden
	h.db.workspaces["ws-fixer"] = record
	h.db.mu.Unlock()
}

func TestTheFixerIsInSightOnlyWhileAtWorkHoldingItsQueue(t *testing.T) {
	tests := []struct {
		name       string
		hidden     bool
		arrange    func(t *testing.T, h *harness)
		wantShown  []ids.WorkspaceID
		wantHidden []ids.WorkspaceID
	}{
		{name: "a hidden fixer a fix is dispatched to is shown", hidden: true, wantShown: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a fixer whose finish is under way is hidden", wantHidden: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
					t.Fatal(err)
				}
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a hidden fixer a check is escalated to is shown", hidden: true, wantShown: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.EscalateMediation(context.Background(), mediationKey(h), deployStarted, "{}"); err != nil {
					t.Fatal(err)
				}
				if _, err := h.db.RecordMediationEscalation(context.Background(), mediationKey(h), deployStarted, "ws-fixer"); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a fixer in sight that no check holds the queue for is hidden", arrange: func(*testing.T, *harness) {}, wantHidden: []ids.WorkspaceID{"ws-fixer"}},
		{name: "a hidden fixer no check holds the queue for is left hidden", hidden: true, arrange: func(*testing.T, *harness) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			fixerInSight(t, h, tt.hidden)
			tt.arrange(t, h)

			// Act
			h.o.settleFixerSight(context.Background(), queueOf(t, h), owedCheck(t, h))

			// Assert
			if !slices.Equal(h.checks.resumed, tt.wantShown) || !slices.Equal(h.checks.hidden, tt.wantHidden) {
				t.Fatalf("shown %v, hidden %v; want shown %v, hidden %v", h.checks.resumed, h.checks.hidden, tt.wantShown, tt.wantHidden)
			}
		})
	}
}

func TestAReleasedEscalationsFixerIsHidden(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	if err := h.o.DecideMediation(context.Background(), "ws-fixer", Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.o.ReleaseEscalation(context.Background(), theScope)

	// Assert
	if err != nil || !slices.Contains(h.checks.hidden, "ws-fixer") {
		t.Fatalf("ReleaseEscalation = %v, hidden %v; want the released fixer hidden", err, h.checks.hidden)
	}
}
