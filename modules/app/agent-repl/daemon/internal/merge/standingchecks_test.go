package merge

import (
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
	"context"
	"errors"
	"fmt"
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

// mediatorIn records WS, open or CLOSED, as the harness queue's deploy
// mediator, briefed.
func mediatorIn(t *testing.T, h *harness, ws ids.WorkspaceID, closed bool) {
	t.Helper()
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaces[ws] = wsm.Workspace{ID: ws, Name: deployRole.role.Name(), Repo: theRepository, Closed: closed}
	h.db.mu.Unlock()
	if err := h.db.RecordDeployMediator(ctx, mediationKey(h), ws); err != nil {
		t.Fatal(err)
	}
	if err := h.db.MarkMediatorBriefed(ctx, mediationKey(h)); err != nil {
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
			mediatorIn(t, h, "ws-standing", tt.closed)

			// Act
			got, err := h.o.mediatorOf(context.Background(), deployRole, queueOf(t, h), dlog.Context{})

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
			if err := h.db.RecordDeployMediator(context.Background(), mediationKey(h), "ws-gone"); err != nil {
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
			got, err := h.o.mediatorOf(context.Background(), deployRole, queueOf(t, h), dlog.Context{})

			// Assert
			if err != nil || got != (standing{ws: "ws-fixer"}) || !slices.Equal(h.checks.made, []string{deployRole.role.Name()}) {
				t.Fatalf("mediatorOf = (%+v, %v), made %v; want %s made", got, err, h.checks.made, deployRole.role.Name())
			}
			recorded, found, readErr := h.db.DeployMediatorOf(context.Background(), mediationKey(h))
			if readErr != nil || !found || recorded.Workspace != got.ws {
				t.Fatalf("standing record = (%+v, %v, %v), want the made mediator", recorded, found, readErr)
			}
			if repo, err := h.o.directLandingRepo(context.Background(), got.ws); err != nil || repo == "" {
				t.Fatalf("directLandingRepo = (%q, %v), want the made mediator landing directly", repo, err)
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
		{name: "a mediator that cannot be read", message: "could not read the mediator",
			arrange: func(t *testing.T, h *harness) {
				mediatorIn(t, h, "ws-standing", false)
				h.db.mu.Lock()
				h.db.workspaceErr = errors.New("database is locked")
				h.db.mu.Unlock()
			}},
		{name: "a closed one that cannot be reopened", message: "the mediator could not be reopened",
			arrange: func(t *testing.T, h *harness) {
				mediatorIn(t, h, "ws-standing", true)
				h.checks.reopenErr = errors.New("branch gone")
			}},
		{name: "one that cannot be made", message: "the mediator could not be made",
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
			_, err := h.o.mediatorOf(context.Background(), deployRole, q, dlog.Context{})

			// Assert
			if err == nil {
				t.Fatal("mediatorOf = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message,
				Match: func(r dlog.Record) bool { return r.Context["role"] == string(wsm.MediatorDeploy) }}); !ok {
				t.Fatalf("records = %v, want %q at ERROR naming the deploy role", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestReadyMediatorRefreshesSetsItsEffortAndShowsIt(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)

	// Act
	got, err := h.o.readyMediator(context.Background(), deployRole, queueOf(t, h), dlog.Context{})

	// Assert: the standing is recorded before the effort starts its shim, and
	// every brief finds the mediator in sight.
	if err != nil || got.ws != "ws-fixer" || !slices.Equal(h.checks.refreshed, []ids.WorkspaceID{"ws-fixer"}) ||
		!slices.Equal(h.checks.efforts, []string{"mediator:ws-fixer:standing=true"}) || !slices.Equal(h.checks.resumed, []ids.WorkspaceID{"ws-fixer"}) {
		t.Fatalf("readyMediator = (%+v, %v), refreshed %v, efforts %v, resumed %v", got, err, h.checks.refreshed, h.checks.efforts, h.checks.resumed)
	}
}

func TestAMediatorIsMadeAndReadiedAsItsRepositoryConfiguresIt(t *testing.T) {
	// Arrange: the repository runs its deploy mediator on fable.
	h := checked(t)
	model := "fable"
	h.config.DeployChecks.Agent = &repoconfigv1.MediatorAgent{Model: &model}
	owe(t, h)

	// Act
	_, err := h.o.readyMediator(context.Background(), deployRole, queueOf(t, h), dlog.Context{})

	// Assert: the create and the readying were both given the section.
	if err != nil || len(h.checks.agents) != 2 || h.checks.agents[0].GetModel() != "fable" || h.checks.agents[1].GetModel() != "fable" {
		t.Fatalf("readyMediator = %v, agents %v; want the configured model at the create and the readying", err, h.checks.agents)
	}
}

func TestAProductionMediatorIsRecordedInItsOwnRole(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)

	// Act
	got, err := h.o.mediatorOf(context.Background(), productionRole, queueOf(t, h), dlog.Context{})

	// Assert
	recorded, found, err2 := h.db.checks.StandingMediatorOf(context.Background(), wsm.MediatorProduction, mediationKey(h))
	_, deployFound, err3 := h.db.checks.StandingMediatorOf(context.Background(), wsm.MediatorDeploy, mediationKey(h))
	if err != nil || err2 != nil || err3 != nil || !found || recorded.Workspace != got.ws || deployFound || !slices.Equal(h.checks.made, []string{"production-mediator"}) {
		t.Fatalf("mediatorOf = (%+v, %v), recorded (%+v, %v, %v), deploy %v (%v), made %v; want only a production mediator made", got, err, recorded, found, err2, deployFound, err3, h.checks.made)
	}
}

func TestReadyMediatorAnswersWhatItCannotDo(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "a worktree that cannot be refreshed", message: "the mediator's worktree could not be brought to the main branch's tip; it is not briefed",
			arrange: func(h *harness) { h.checks.refreshErr = errors.New("rebase conflicts") }},
		{name: "an effort that cannot be set", message: "the mediator's model and effort could not be set; it is not briefed",
			arrange: func(h *harness) { h.checks.effortErr = errors.New("no selector") }},
		{name: "a mediator that cannot be shown", message: "the mediator could not be shown for its brief; it is not briefed",
			arrange: func(h *harness) { h.checks.resumeErr = errors.New("parent backlogged") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			tt.arrange(h)

			// Act
			_, err := h.o.readyMediator(context.Background(), deployRole, queueOf(t, h), dlog.Context{})

			// Assert
			if err == nil {
				t.Fatal("readyMediator = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message,
				Match: func(r dlog.Record) bool { return r.Context["role"] == string(wsm.MediatorDeploy) }}); !ok {
				t.Fatalf("records = %v, want %q at ERROR naming the deploy role", h.logs.Records(), tt.message)
			}
		})
	}
}

// approvedChecks has the harness's standing intake approve N deploys in a
// row, each a minute after the last, from deployStarted.
func approvedChecks(t *testing.T, h *harness, n int) {
	t.Helper()
	for i := range n {
		at := deployStarted.Add(time.Duration(i) * time.Minute)
		oweAt(t, h, at)
		withMediator(t, h)
		if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatalf("DecideMediation: %v", err)
		}
	}
}

func TestAQuietCheckCompactsTheMediatorOnlyPastItsBound(t *testing.T) {
	tests := []struct {
		name   string
		tokens int64
		want   []ids.WorkspaceID
	}{
		{name: "a context past the bound is compacted", tokens: QuietCheckCompactionTokens + 1, want: []ids.WorkspaceID{"ws-fixer"}},
		{name: "a context at the bound is left warm", tokens: QuietCheckCompactionTokens},
		{name: "an empty context is left warm"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			mediatorIn(t, h, "ws-fixer", false)
			h.checks.contextTokens = tt.tokens

			// Act
			approvedChecks(t, h, 1)

			// Assert
			if !slices.Equal(h.checks.compacted, tt.want) {
				t.Fatalf("compacted %v, want %v", h.checks.compacted, tt.want)
			}
		})
	}
}

func TestAQuietCheckThatCannotReadTheContextCompactsNothing(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	mediatorIn(t, h, "ws-fixer", false)
	h.checks.contextErr = errors.New("verbs unbound")

	// Act
	approvedChecks(t, h, 1)

	// Assert
	if len(h.checks.compacted) != 0 {
		t.Fatalf("compacted %v, want none", h.checks.compacted)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "could not read the deploy mediator's context; it is not compacted"}); !ok {
		t.Fatalf("records = %v, want the failed read at ERROR", h.logs.Records())
	}
}

func TestQuietApprovalsAfterADispatchCompactNothingWithinTheBound(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	if err := dispatched(h, Decision{}); err != nil {
		t.Fatal(err)
	}
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
	withMediator(t, h)

	// Act: the fix's deploy and two more are approved.
	if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
		t.Fatal(err)
	}
	for i := 2; i <= 3; i++ {
		at := deployStarted.Add(time.Duration(i) * time.Minute)
		oweAt(t, h, at)
		withMediator(t, h)
		if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatal(err)
		}
	}

	// Assert: a dispatch arms nothing and the context is within its bound, so
	// only the fix's own deploy owes the mediator's compaction
	// (remediationDeploy).
	if len(h.checks.compacted) != 0 {
		t.Fatalf("compacted %v, want none from quiet approvals within the bound", h.checks.compacted)
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
			withMediator(t, h)
			if tt.escalated {
				if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
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

func TestTheStandingFixerStaysOpenAfterItsFixDeploys(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.db.mu.Lock()
	h.db.workspaces["ws-fixer"] = wsm.Workspace{ID: "ws-fixer", Name: "deploy-fixer", Repo: theRepository, Mediator: wsm.MediatorDeploy}
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
		Message: "the landed fix deployed; the deploy mediator stays open for the next brief"}); !ok {
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
		{wsm.DaemonCrashed, "Find what killed it and fix it: `daemon.boot.predecessor_died`"},
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

// mediatorInSight records "ws-fixer" as the harness queue's deploy
// mediator, BACKLOGGED or in sight.
func mediatorInSight(t *testing.T, h *harness, backlogged bool) {
	t.Helper()
	mediatorIn(t, h, "ws-fixer", false)
	h.db.mu.Lock()
	record := h.db.workspaces["ws-fixer"]
	record.Backlogged = backlogged
	h.db.workspaces["ws-fixer"] = record
	h.db.mu.Unlock()
}

func TestTheMediatorIsKeptInSightWhateverItsCheck(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
		arrange    func(t *testing.T, h *harness)
		wantShown  []ids.WorkspaceID
	}{
		{name: "a backlogged mediator a fix is dispatched for is shown", backlogged: true, wantShown: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, aFixerTurn); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a mediator in sight whose finish is under way stays in sight",
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, aFixerTurn); err != nil {
					t.Fatal(err)
				}
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a backlogged mediator whose finish is under way is shown", backlogged: true, wantShown: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, aFixerTurn); err != nil {
					t.Fatal(err)
				}
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a mediator in sight that no check holds the queue for stays in sight", arrange: func(*testing.T, *harness) {}},
		{name: "a backlogged mediator no check holds the queue for is shown", backlogged: true, arrange: func(*testing.T, *harness) {}, wantShown: []ids.WorkspaceID{"ws-fixer"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			mediatorInSight(t, h, tt.backlogged)
			tt.arrange(t, h)

			// Act
			h.o.keepMediatorShown(context.Background(), deployRole, queueOf(t, h))

			// Assert
			if !slices.Equal(h.checks.resumed, tt.wantShown) {
				t.Fatalf("shown %v; want shown %v", h.checks.resumed, tt.wantShown)
			}
		})
	}
}

func TestABackloggedMediatorThatCannotBeShownIsAnError(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	mediatorInSight(t, h, true)
	h.checks.resumeErr = errors.New("parent backlogged")

	// Act
	h.o.keepMediatorShown(context.Background(), deployRole, queueOf(t, h))

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the backlogged mediator could not be shown"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestAReleasedEscalationsMediatorStaysInSight(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.o.ReleaseEscalation(context.Background(), theScope)

	// Assert
	h.db.mu.Lock()
	record := h.db.workspaces["ws-fixer"]
	h.db.mu.Unlock()
	if err != nil || record.Backlogged {
		t.Fatalf("ReleaseEscalation = %v, record %+v; want the released mediator still in sight", err, record)
	}
}

func TestRenderOccasionNamesTheDeployOrTheCrashTheCheckIsOf(t *testing.T) {
	owed := time.Date(2026, 11, 21, 20, 22, 30, 0, time.UTC)
	tests := []struct {
		name   string
		ending wsm.DeployEnding
		want   string
	}{
		{name: "a deploy's check names the deploy", ending: wsm.DeployFinished, want: "A deploy of `/r` began at 2026-11-21T20:22:30Z (v1)."},
		{name: "a check of no recorded ending names the deploy", want: "A deploy of `/r` began at 2026-11-21T20:22:30Z (v1)."},
		{name: "a crash's check names no deploy", ending: wsm.DaemonCrashed,
			want: "No deploy ran: the daemon serving `/r` restarted at 2026-11-21T20:22:30Z (v1) because the one before it died without its orderly exit."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderOccasion(tt.ending, "/r", owed, "v1")

			// Assert
			if got != tt.want {
				t.Fatalf("RenderOccasion = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestMediatorStandingAnswersTheRecordedMediatorsStanding(t *testing.T) {
	tests := []struct {
		name         string
		recorded     bool
		standing     ServingStanding
		standingErr  error
		wantMediator ids.WorkspaceID
		wantStanding ServingStanding
		wantErr      bool
	}{
		{name: "a queue with no mediator recorded is served, its standing never read", standingErr: errors.New("never read"), wantStanding: ServedHere},
		{name: "a recorded mediator served here", recorded: true, wantMediator: "ws-fixer", wantStanding: ServedHere},
		{name: "a recorded mediator not adopted yet", recorded: true, standing: NotYetAdopted, wantMediator: "ws-fixer", wantStanding: NotYetAdopted},
		{name: "a recorded mediator handed away", recorded: true, standing: TransferringAway, wantMediator: "ws-fixer", wantStanding: TransferringAway},
		{name: "a recorded mediator whose standing cannot be read", recorded: true, standingErr: errors.New("unrecognized standing"), wantMediator: "ws-fixer", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			if tt.recorded {
				withMediator(t, h)
			}
			h.standing.stand("ws-fixer", tt.standing)
			h.standing.standingErr = tt.standingErr

			// Act
			mediator, standing, err := h.o.mediatorStanding(context.Background(), deployRole, mediationKey(h))

			// Assert
			if mediator != tt.wantMediator || (err != nil) != tt.wantErr || (!tt.wantErr && standing != tt.wantStanding) {
				t.Fatalf("mediatorStanding = (%q, %v, %v), want (%q, %v, error %v)", mediator, standing, err, tt.wantMediator, tt.wantStanding, tt.wantErr)
			}
		})
	}
}

// mediatorPassedOver has a pass run while ws-fixer, the queue's recorded mediator,
// is not adopted here, then has this daemon serve it.
func mediatorPassedOver(t *testing.T, h *harness) {
	t.Helper()
	owe(t, h)
	withMediator(t, h)
	h.standing.stand("ws-fixer", NotYetAdopted)
	h.o.Mediate(context.Background())
	if len(h.checks.refreshed) != 0 || len(h.queue.submissions) != 0 {
		t.Fatalf("refreshed %v, submitted %d; want the round passed over", h.checks.refreshed, len(h.queue.submissions))
	}
	h.standing.stand("ws-fixer", ServedHere)
}

func TestTheMediatorsAdoptionRunsTheRoundItsPassPassedOver(t *testing.T) {
	// Arrange
	h := checked(t)
	mediatorPassedOver(t, h)
	decideInTurn(t, h, Decision{Kind: MediationApprove, Summary: "clean"})

	// Act
	if err := h.o.AdoptWorkspace(context.Background(), "ws-fixer"); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}

	// Assert: the round readied the mediator, briefed it, and its decision
	// released the check.
	if !slices.Equal(h.checks.refreshed, []ids.WorkspaceID{"ws-fixer"}) || len(intakeTellings(h)) != 1 || owedCheck(t, h) != nil {
		t.Fatalf("refreshed %v, told %d, check %+v; want the mediator readied, briefed once and its check released",
			h.checks.refreshed, len(intakeTellings(h)), owedCheck(t, h))
	}
}

func TestAnAdoptionRunsThePassOnlyForAMediatorOrAFinishsWorkspace(t *testing.T) {
	tests := []struct {
		name     string
		adopted  ids.WorkspaceID
		finish   bool
		wantPass bool
	}{
		{name: "the recorded mediator's adoption runs the pass", adopted: "ws-fixer", wantPass: true},
		{name: "a remediation finish's workspace's adoption runs the pass", adopted: "ws-old", finish: true, wantPass: true},
		{name: "any other workspace's adoption runs no pass", adopted: "ws-other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a pass that runs is seen by its record that this
			// daemon does not serve the queues, and does nothing else.
			h := checked(t)
			withMediator(t, h)
			if tt.finish {
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-old", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
			}
			h.standing.serves = false

			// Act
			if err := h.o.AdoptWorkspace(context.Background(), tt.adopted); err != nil {
				t.Fatalf("AdoptWorkspace: %v", err)
			}

			// Assert
			_, ran := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "this daemon does not serve the queues; their checks are run by the daemon that does"})
			_, said := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "this daemon adopted a mediator; the mediation pass runs what it passed over while the mediator was not served here"})
			if ran != tt.wantPass || said != tt.wantPass {
				t.Fatalf("pass ran %v, said %v; want %v", ran, said, tt.wantPass)
			}
		})
	}
}

func TestAnAdoptionWhoseMediatorCannotBeReadSaysSo(t *testing.T) {
	// Arrange
	h := checked(t)
	h.db.mu.Lock()
	h.db.mediatorRepoErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.AdoptWorkspace(context.Background(), "ws-fixer")

	// Assert
	if err != nil {
		t.Fatalf("AdoptWorkspace = %v, want the merges taken all the same", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "could not read whether the adopted workspace is a mediator; a round its adoption owes waits for the queue's next pass"}); !ok {
		t.Fatalf("records = %v, want the unread mediator at ERROR", h.logs.Records())
	}
}

func TestABackloggedMediatorHandedAwayBeforeItIsShownIsNoError(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	mediatorInSight(t, h, true)
	h.checks.resumeErr = fmt.Errorf("resume: %w", &standingRefused{standing: TransferringAway})

	// Act
	h.o.keepMediatorShown(context.Background(), deployRole, queueOf(t, h))

	// Assert
	if rec, errored := h.recordFor("error", opMediation); errored {
		t.Fatalf("ERROR %+v, want none", rec)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: mediatorTransferredAway}); !ok {
		t.Fatalf("records = %v, want the handed-away mediator at INFO", h.logs.Records())
	}
}

func TestServingWidenedRunsTheMediationPass(t *testing.T) {
	// Arrange: a pass that runs is seen by its record that this daemon does
	// not serve the queues, and does nothing else.
	h := checked(t)
	h.standing.serves = false

	// Act
	h.o.ServingWidened(context.Background())

	// Assert
	if _, ran := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "this daemon does not serve the queues; their checks are run by the daemon that does"}); !ran {
		t.Fatalf("records = %v, want the pass run", h.logs.Records())
	}
}
