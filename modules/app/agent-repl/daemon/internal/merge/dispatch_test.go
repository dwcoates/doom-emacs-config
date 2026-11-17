package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// dispatched has the intake dispatch D on the check owed at deployStarted.
func dispatched(h *harness, d Decision) error {
	d.Kind, d.Summary = MediationDispatch, "broken"
	if d.Problems == nil {
		d.Problems = aProblem
	}
	return h.o.DecideMediation(context.Background(), "ws-intake", d)
}

// withRemediation records "ws-remedy" as the queue's standing remediation,
// briefed and closed by its landed fix: a fix dispatched to it for the check
// owed at deployStarted, whose deploy began a minute later and is told to
// the standing intake.
func withRemediation(t *testing.T, h *harness) {
	t.Helper()
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaces["ws-remedy"] = wsm.Workspace{ID: "ws-remedy", Name: "deploy-remediation", Repo: theRepository, Closed: true}
	h.db.mu.Unlock()
	if err := h.db.RecordStandingDeployCheck(ctx, mediationKey(h), wsm.StandingRemediation, "ws-remedy"); err != nil {
		t.Fatal(err)
	}
	if err := h.db.MarkStandingBriefed(ctx, mediationKey(h), wsm.StandingRemediation); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.DispatchMediation(ctx, mediationKey(h), deployStarted, deployStarted, "ws-remedy"); err != nil {
		t.Fatal(err)
	}
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
	withIntake(t, h, deployStarted.Add(time.Minute))
}

func TestTheFirstDispatchMakesTheStandingRemediationAndBriefsItTheProblems(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act
	err := dispatched(h, Decision{})

	// Assert
	m := owedCheck(t, h)
	if err != nil || m == nil || !m.Dispatched || m.Remediation != "ws-remedy" || m.LastDispatchedAt == nil {
		t.Fatalf("dispatch = %v, check %+v; want it dispatched to the made remediation", err, m)
	}
	if !slices.Equal(h.checks.made, []string{"deploy-remediation"}) || h.briefNames[len(h.briefNames)-1] != BriefDeployRemediation {
		t.Fatalf("made %v, want one remediation workspace", h.checks.made)
	}
	subs := h.queue.submissions
	if len(subs) != 1 || subs[0].WS != "ws-remedy" || subs[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION {
		t.Fatalf("submitted %+v, want the remediation briefed once", subs)
	}
	values := h.briefValues[len(h.briefValues)-1]
	if !strings.Contains(values["problems"], "1. the reaper errors on every pass") || !strings.Contains(values["problems"], `   > {"level":"error"`) {
		t.Fatalf("problems = %q, want the statement and its verbatim evidence", values["problems"])
	}
	if !slices.Contains(h.meter.all(), "mediation:"+MediationSpanAssessment+":"+MediationDispatched) && m.PromptedAt != nil {
		t.Fatalf("timed %v, want the assessment ended dispatched", h.meter.all())
	}
}

func TestALaterDispatchReopensTheStandingRemediationAndBriefsOnlyTheNews(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	withRemediation(t, h)

	// Act
	err := dispatched(h, Decision{})

	// Assert
	if err != nil || !slices.Equal(h.checks.reopened, []ids.WorkspaceID{"ws-remedy"}) || len(h.checks.made) != 0 {
		t.Fatalf("dispatch = %v, reopened %v, made %v; want the standing remediation reopened", err, h.checks.reopened, h.checks.made)
	}
	if len(h.queue.submissions) != 1 || h.briefNames[len(h.briefNames)-1] != BriefDeployRemediationRestored {
		t.Fatalf("submitted %v, briefs %v; want one brief of only the news", h.queue.submissions, h.briefNames)
	}
	if !slices.Contains(h.meter.all(), "remediation:"+RemediationRedispatched) {
		t.Fatalf("recorded %v, want the earlier remediation ended redispatched", h.meter.all())
	}
}

func TestTheStandingRemediationIsShownAgain(t *testing.T) {
	tests := []struct {
		name      string
		resumeErr error
		wantErr   bool
	}{
		{name: "the standing remediation is shown"},
		{name: "one that cannot be shown holds the queue for the intake", resumeErr: errors.New("database is locked"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			withRemediation(t, h)
			h.checks.resumeErr = tt.resumeErr

			// Act
			err := dispatched(h, Decision{})

			// Assert
			if !slices.Equal(h.checks.resumed, []ids.WorkspaceID{"ws-remedy"}) {
				t.Fatalf("resumed %v, want the standing remediation shown", h.checks.resumed)
			}
			if (err != nil) != tt.wantErr {
				t.Fatalf("dispatch = %v, want an error %v", err, tt.wantErr)
			}
			if !tt.wantErr {
				return
			}
			if m := owedCheck(t, h); m == nil || m.Dispatched {
				t.Fatalf("check %+v, want it still owed with nothing dispatched", m)
			}
			if len(h.queue.submissions) != 0 {
				t.Fatalf("submitted %v, want the hidden remediation unbriefed", h.queue.submissions)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
				Message: "the standing remediation workspace could not be shown; the queue stays held for the intake"}); !ok {
				t.Fatalf("records = %v, want the failed show at ERROR", h.logs.Records())
			}
		})
	}
}

func TestADispatchIsRefusedWhereItCannotBeHonored(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		wantArm   string
		wantBound bool
	}{
		{name: "a dispatch once the chain reached its bound", wantArm: ArmRemediationBoundReached, wantBound: true,
			arrange: func(t *testing.T, h *harness) {
				for i := 1; i <= theBound; i++ {
					at := deployStarted.Add(time.Duration(i-1) * time.Minute)
					if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), at, at, "ws-remedy"); err != nil {
						t.Fatal(err)
					}
					fixDeploysAt(t, h, deployStarted.Add(time.Duration(i)*time.Minute))
				}
				withIntake(t, h, deployStarted.Add(time.Duration(theBound)*time.Minute))
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}

			// Act
			err := dispatched(h, Decision{})

			// Assert
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("dispatch = %v, want the %s refusal", err, tt.wantArm)
			}
			if tt.wantBound && refused.Fields["remediations_before_escalation"] != uint32(theBound) {
				t.Fatalf("fields = %v, want the bound", refused.Fields)
			}
			if m := owedCheck(t, h); m == nil || m.Dispatched || len(h.checks.made) != 0 || len(h.queue.submissions) != 0 {
				t.Fatalf("check %+v, made %v; want nothing dispatched", m, h.checks.made)
			}
			if _, ok := h.recordFor("warn", opMediation); !ok {
				t.Fatal("no WARN record of the refused dispatch")
			}
		})
	}
}

func TestADispatchWhoseWorkspaceCannotBeReadiedLeavesTheQueueHeldForTheIntake(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		message string
	}{
		{name: "a standing workspace that cannot be made", message: "the standing deploy check workspace could not be made",
			arrange: func(t *testing.T, h *harness) { h.checks.createErr = errors.New("disk full") }},
		{name: "a standing workspace that cannot be reopened", message: "the standing deploy check workspace could not be reopened",
			arrange: func(t *testing.T, h *harness) {
				withRemediation(t, h)
				h.checks.reopenErr = errors.New("branch gone")
			}},
		{name: "a dispatch that cannot be recorded", message: "the remediation workspace is ready but the dispatch could not be recorded; the queue stays held for the intake",
			arrange: func(t *testing.T, h *harness) { h.db.dispatchErr = errors.New("disk gone") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			tt.arrange(t, h)

			// Act
			err := dispatched(h, Decision{})

			// Assert
			if err == nil {
				t.Fatal("dispatch = nil, want the failure answered")
			}
			if m := owedCheck(t, h); m == nil || m.Dispatched {
				t.Fatalf("check %+v, want it still owed and not dispatched", m)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestADispatchWhoseBriefCannotBeSubmittedIsAnsweredWithTheDispatchStanding(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.queue.err = errors.New("queue closed")

	// Act
	err := dispatched(h, Decision{})

	// Assert
	if err == nil {
		t.Fatal("dispatch = nil, want the unsubmitted brief answered")
	}
	if m := owedCheck(t, h); m == nil || !m.Dispatched {
		t.Fatalf("check %+v, want the dispatch recorded, the remediation visible to brief by hand", m)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the remediation's brief could not be submitted; the dispatch stands with its workspace unprompted"}); !ok {
		t.Fatalf("records = %v, want the unsubmitted brief at ERROR", h.logs.Records())
	}
}

func TestRenderProblemsNumbersEachWithItsEvidence(t *testing.T) {
	// Act
	got := renderProblems([]DeployProblem{
		{Statement: "first", Evidence: []string{"a", "b"}},
		{Statement: "second", Evidence: []string{"c"}},
	})

	// Assert
	want := "1. first\n   > a\n   > b\n\n2. second\n   > c"
	if got != want {
		t.Fatalf("renderProblems = %q, want %q", got, want)
	}
}

func TestAMadeRemediationsEffortIsSetOnceItsDispatchIsRecorded(t *testing.T) {
	tests := []struct {
		name        string
		standing    bool
		wantEfforts []string
	}{
		{name: "a made remediation", wantEfforts: []string{"remediation:ws-remedy:recorded=true"}},
		{name: "the standing remediation keeps the effort it ran at", standing: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			if tt.standing {
				withRemediation(t, h)
			}

			// Act
			err := dispatched(h, Decision{})

			// Assert
			if err != nil || !slices.Equal(h.checks.efforts, tt.wantEfforts) {
				t.Fatalf("dispatch = %v, efforts %v; want %v", err, h.checks.efforts, tt.wantEfforts)
			}
		})
	}
}

func TestARemediationWhoseEffortCannotBeSetIsNotBriefed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.checks.effortErr = errors.New("no catalog")

	// Act
	err := dispatched(h, Decision{})

	// Assert
	if err == nil || len(h.queue.submissions) != 0 {
		t.Fatalf("dispatch = %v, submitted %d; want the failure and no brief", err, len(h.queue.submissions))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the remediation's effort could not be set; the dispatch stands with its workspace unprompted"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}
