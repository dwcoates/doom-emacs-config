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

// dispatched has the mediator dispatch D to itself on the check owed now,
// from the turn it was told the check in.
func dispatched(h *harness, d Decision) error {
	d.Kind, d.Summary = MediationDispatch, "broken"
	if d.Problems == nil {
		d.Problems = aProblem
	}
	return decideTold(h, d)
}

// decideTold has the mediator decide D on the check owed now, from the turn
// it was told the check in ("turn-told" when it was told in none yet): a
// decision always comes from that turn.
func decideTold(h *harness, d Decision) error {
	if m, err := h.db.MediationOf(context.Background(), mediationKey(h)); err == nil && m != nil && m.Turn == "" {
		if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), m.OwedAt, m.OwedAt, "turn-told"); err != nil {
			return err
		}
	}
	return h.o.DecideMediation(context.Background(), "ws-fixer", d)
}

// withRemediation records "ws-fixer" as the queue's standing remediation,
// briefed and closed by its landed fix: a fix dispatched to it for the check
// owed at deployStarted, whose deploy began a minute later and is told to
// the standing intake.
func withRemediation(t *testing.T, h *harness) {
	t.Helper()
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaces["ws-fixer"] = wsm.Workspace{ID: "ws-fixer", Name: "deploy-fixer", Repo: theRepository, Closed: true}
	h.db.mu.Unlock()
	if err := h.db.RecordStandingDeployCheck(ctx, mediationKey(h), wsm.StandingMediator, "ws-fixer"); err != nil {
		t.Fatal(err)
	}
	if err := h.db.MarkStandingBriefed(ctx, mediationKey(h), wsm.StandingMediator); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.DispatchMediation(ctx, mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
		t.Fatal(err)
	}
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
	withIntake(t, h, deployStarted.Add(time.Minute))
}

func TestADispatchIsTheMediatorsOwnFixOfItsTurnAndShowsIt(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act
	err := dispatched(h, Decision{})

	// Assert: dispatched to the mediator itself, its told turn the one the
	// check waits on, and nothing new made or briefed.
	m := owedCheck(t, h)
	if err != nil || m == nil || !m.Dispatched || m.Remediation != "ws-fixer" || m.LastDispatchedAt == nil {
		t.Fatalf("dispatch = %v, check %+v; want it dispatched to the mediator", err, m)
	}
	if m.FixerTurn.ID != "turn-told" || m.FixerTurn.Origin != int32(conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION) {
		t.Fatalf("fixer turn %+v, want the turn the mediator was told the check in", m.FixerTurn)
	}
	if len(h.checks.made) != 0 || len(h.queue.submissions) != 0 || !slices.Equal(h.checks.resumed, []ids.WorkspaceID{"ws-fixer"}) {
		t.Fatalf("made %v, submitted %d, resumed %v; want nothing made or briefed, and the mediator shown", h.checks.made, len(h.queue.submissions), h.checks.resumed)
	}
	values := h.briefValues[len(h.briefValues)-1]
	if h.briefNames[len(h.briefNames)-1] != BriefDeployRemediationRestored || !strings.Contains(values["problems"], "1. the reaper errors on every pass") || !strings.Contains(values["problems"], `   > {"level":"error"`) {
		t.Fatalf("briefs %v, problems = %q; want the fix's standing brief with the statement and its verbatim evidence", h.briefNames, values["problems"])
	}
	if !slices.Contains(h.meter.all(), "mediation:"+MediationSpanAssessment+":"+MediationDispatched) && m.PromptedAt != nil {
		t.Fatalf("timed %v, want the assessment ended dispatched", h.meter.all())
	}
}

func TestALaterDispatchEndsTheEarlierRemediationRedispatched(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	withRemediation(t, h)

	// Act
	err := dispatched(h, Decision{})

	// Assert
	if err != nil || len(h.checks.made) != 0 || len(h.queue.submissions) != 0 {
		t.Fatalf("dispatch = %v, made %v, submitted %d; want the mediator to fix it itself", err, h.checks.made, len(h.queue.submissions))
	}
	if !slices.Contains(h.meter.all(), "remediation:"+RemediationRedispatched) {
		t.Fatalf("recorded %v, want the earlier remediation ended redispatched", h.meter.all())
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
					if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), at, at, "ws-fixer", aFixerTurn); err != nil {
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

func TestADispatchThatCannotBeMadeLeavesTheQueueHeldForTheMediator(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		message string
	}{
		{name: "a brief that cannot be composed", message: "the dispatch's brief could not be composed; nothing is dispatched and the queue stays held for the mediator",
			arrange: func(t *testing.T, h *harness) { delete(h.briefs, BriefDeployRemediationRestored) }},
		{name: "a dispatch that cannot be recorded", message: "the dispatch could not be recorded; the queue stays held for the mediator",
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

func TestADispatchWhoseMediatorCannotBeShownStandsAndIsRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.checks.resumeErr = errors.New("parent hidden")

	// Act
	err := dispatched(h, Decision{})

	// Assert
	if m := owedCheck(t, h); err != nil || m == nil || !m.Dispatched {
		t.Fatalf("dispatch = %v, check %+v; want the dispatch to stand", err, m)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the dispatch is recorded but the mediator could not be shown while it fixes; the next pass shows it"}); !ok {
		t.Fatalf("records = %v, want the failed show at ERROR", h.logs.Records())
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
