package merge

import (
	"context"
	"errors"
	"slices"
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

func TestStandingWorkspaceAnswersTheQueuesOne(t *testing.T) {
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
			standingIn(t, h, wsm.StandingEscalation, "ws-standing", tt.closed)

			// Act
			got, err := h.o.standingWorkspace(context.Background(), queueOf(t, h), DeployCheckEscalation, dlog.Context{})

			// Assert
			if err != nil || got != (standing{ws: "ws-standing", briefed: true}) || len(h.checks.made) != 0 || !slices.Equal(h.checks.reopened, tt.wantReopened) {
				t.Fatalf("standingWorkspace = (%+v, %v), made %v, reopened %v; want the standing one", got, err, h.checks.made, h.checks.reopened)
			}
		})
	}
}

func TestStandingWorkspaceMakesOneForARoleWithNone(t *testing.T) {
	tests := []struct {
		name        string
		role        DeployCheckRole
		wantName    string
		wantEfforts []string
	}{
		{name: "an intake, its effort left for its check", role: DeployCheckIntake, wantName: "deploy-intake"},
		{name: "a remediation, its effort left for its dispatch", role: DeployCheckRemediation, wantName: "deploy-remediation"},
		{name: "an escalation, recorded landing directly and its effort set", role: DeployCheckEscalation, wantName: "deploy-escalation", wantEfforts: []string{"escalation:ws-escalate:recorded=false"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)

			// Act
			got, err := h.o.standingWorkspace(context.Background(), queueOf(t, h), tt.role, dlog.Context{})

			// Assert
			if err != nil || !got.made || got.briefed || !slices.Equal(h.checks.made, []string{tt.wantName}) || !slices.Equal(h.checks.efforts, tt.wantEfforts) {
				t.Fatalf("standingWorkspace = (%+v, %v), made %v, efforts %v; want %s made", got, err, h.checks.made, h.checks.efforts, tt.wantName)
			}
			recorded, found, readErr := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), standingRole(tt.role))
			if readErr != nil || !found || recorded.Workspace != got.ws {
				t.Fatalf("standing record = (%+v, %v, %v), want the made workspace", recorded, found, readErr)
			}
		})
	}
}

func TestStandingWorkspaceMakesANewOneWhenTheRecordedOneIsGone(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	if err := h.db.RecordStandingDeployCheck(context.Background(), mediationKey(h), wsm.StandingRemediation, "ws-gone"); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := h.o.standingWorkspace(context.Background(), queueOf(t, h), DeployCheckRemediation, dlog.Context{})

	// Assert
	if err != nil || got.ws != "ws-remedy" || !got.made {
		t.Fatalf("standingWorkspace = (%+v, %v), want a new one made", got, err)
	}
}

func TestStandingWorkspaceAnswersWhatItCannotDo(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		message string
	}{
		{name: "a workspace that cannot be read", message: "could not read the standing deploy check workspace",
			arrange: func(t *testing.T, h *harness) {
				standingIn(t, h, wsm.StandingIntake, "ws-standing", false)
				h.db.mu.Lock()
				h.db.workspaceErr = errors.New("database is locked")
				h.db.mu.Unlock()
			}},
		{name: "a closed one that cannot be reopened", message: "the standing deploy check workspace could not be reopened",
			arrange: func(t *testing.T, h *harness) {
				standingIn(t, h, wsm.StandingIntake, "ws-standing", true)
				h.checks.reopenErr = errors.New("branch gone")
			}},
		{name: "one that cannot be made", message: "the standing deploy check workspace could not be made",
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
			_, err := h.o.standingWorkspace(context.Background(), q, DeployCheckIntake, dlog.Context{})

			// Assert
			if err == nil {
				t.Fatal("standingWorkspace = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
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
		withIntake(t, h, at)
		if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatalf("DecideMediation: %v", err)
		}
	}
}

func TestAQuietRunAfterWorkCompactsEveryStandingWorkspace(t *testing.T) {
	tests := []struct {
		name  string
		work  bool
		quiet int
		want  []ids.WorkspaceID
	}{
		{name: "three quiet checks after work compact each standing workspace", work: true, quiet: 3, want: []ids.WorkspaceID{"ws-intake", "ws-remedy", "ws-escalate"}},
		{name: "two quiet checks after work compact nothing yet", work: true, quiet: 2},
		{name: "quiet checks with no work before compact nothing", quiet: 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			for _, s := range []struct {
				role wsm.DeployCheckRoleName
				ws   ids.WorkspaceID
			}{{wsm.StandingIntake, "ws-intake"}, {wsm.StandingRemediation, "ws-remedy"}, {wsm.StandingEscalation, "ws-escalate"}} {
				standingIn(t, h, s.role, s.ws, false)
			}
			if tt.work {
				if err := h.db.NoteDeployCheckWork(context.Background(), mediationKey(h)); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			approvedChecks(t, h, tt.quiet)

			// Assert
			if !slices.Equal(h.db.owedCompactions, tt.want) {
				t.Fatalf("owed compactions %v, want %v", h.db.owedCompactions, tt.want)
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
	if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
		t.Fatal(err)
	}
	for i := 2; i <= 3; i++ {
		at := deployStarted.Add(time.Duration(i) * time.Minute)
		oweAt(t, h, at)
		withIntake(t, h, at)
		if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
			t.Fatal(err)
		}
	}

	// Assert: the standing remediation the dispatch made is owed its compaction.
	if !slices.Contains(h.db.owedCompactions, "ws-remedy") {
		t.Fatalf("owed compactions %v, want the standing remediation's", h.db.owedCompactions)
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
				if err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "yours"}); err != nil {
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
			if h.checks.closed != nil && slices.Contains(h.checks.closed, "ws-escalate") {
				t.Fatal("the release closed the escalation workspace; it stays open")
			}
		})
	}
}

func TestTheChecksWorkspacesAreAdoptedAsTheStandingOnes(t *testing.T) {
	// Arrange: a check escalated to a workspace made before the standing ones.
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
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
	for _, want := range []struct {
		role wsm.DeployCheckRoleName
		ws   ids.WorkspaceID
	}{{wsm.StandingIntake, "ws-intake"}, {wsm.StandingEscalation, "ws-old-escalation"}} {
		got, found, err := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), want.role)
		if err != nil || !found || got != (wsm.StandingDeployCheck{Workspace: want.ws, Briefed: true}) {
			t.Fatalf("standing %s = (%+v, %v, %v), want %s adopted, briefed", want.role, got, found, err, want.ws)
		}
	}
	if _, found, _ := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingRemediation); found {
		t.Fatal("a remediation was adopted though the check has none")
	}
}

func TestAdoptionKeepsAStandingWorkspaceTheQueueHasAlready(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	standingIn(t, h, wsm.StandingIntake, "ws-standing", false)
	withIntake(t, h, deployStarted)

	// Act
	h.o.adoptStanding(context.Background(), queueOf(t, h), owedCheck(t, h))

	// Assert
	if got, _, _ := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingIntake); got.Workspace != "ws-standing" {
		t.Fatalf("standing intake = %+v, want the one the queue had", got)
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
				standingIn(t, h, wsm.StandingEscalation, "ws-escalate", false)
				oneOffEscalation(t, h, "ws-escalate", false)
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
	standingIn(t, h, wsm.StandingEscalation, "ws-escalate", false)
	h.checks.closeErr = errors.New("database is locked")

	// Act
	h.o.retireOneOffEscalations(context.Background(), queueOf(t, h), nil)

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "an earlier escalation could not be retired"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}
