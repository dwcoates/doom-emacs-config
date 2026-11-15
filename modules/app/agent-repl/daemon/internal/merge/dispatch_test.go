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

// withRemediation records "ws-remedy" as the chain's last remediation: a fix
// dispatched to it for the check owed at deployStarted, whose deploy began a
// minute later.
func withRemediation(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-remedy"); err != nil {
		t.Fatal(err)
	}
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
}

func TestAFreshDispatchMakesAVisibleRemediationAndBriefsItTheProblems(t *testing.T) {
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
	if len(h.checks.made) != 1 || !strings.HasPrefix(h.checks.made[0], "deploy-remediation-") {
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

func TestARestoredDispatchReopensTheLastRemediation(t *testing.T) {
	tests := []struct {
		name        string
		percent     int64
		wantCompact bool
	}{
		{name: "a small context is briefed as it is", percent: 20},
		{name: "a large context is compacted before its brief", percent: restoredCompactPercent, wantCompact: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			withRemediation(t, h)
			h.db.contextUsage["ws-remedy"] = wsm.ContextUsageRecord{Usage: &conversationv1.SessionContextUsage{Percentage: tt.percent}}

			// Act
			err := dispatched(h, Decision{Restore: true})

			// Assert
			if err != nil || !slices.Equal(h.checks.reopened, []ids.WorkspaceID{"ws-remedy"}) || len(h.checks.made) != 0 {
				t.Fatalf("dispatch = %v, reopened %v, made %v; want the last remediation reopened", err, h.checks.reopened, h.checks.made)
			}
			var origins []conversationv1.PromptOrigin
			for _, sub := range h.queue.submissions {
				origins = append(origins, sub.Origin)
			}
			want := []conversationv1.PromptOrigin{conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION}
			if tt.wantCompact {
				want = append([]conversationv1.PromptOrigin{conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT}, want...)
			}
			if !slices.Equal(origins, want) {
				t.Fatalf("submitted %v, want %v", origins, want)
			}
			if !slices.Contains(h.meter.all(), "remediation:"+RemediationRedispatched) {
				t.Fatalf("recorded %v, want the earlier remediation ended redispatched", h.meter.all())
			}
		})
	}
}

func TestADispatchIsRefusedWhereItCannotBeHonored(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		restore   bool
		wantArm   string
		wantBound bool
	}{
		{name: "a restore when the chain has no remediation", restore: true, wantArm: ArmNoRemediationToRestore},
		{name: "a dispatch once the chain reached its bound", wantArm: ArmRemediationBoundReached, wantBound: true,
			arrange: func(t *testing.T, h *harness) {
				for i := 1; i <= theBound; i++ {
					at := deployStarted.Add(time.Duration(i-1) * time.Minute)
					if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), at, at, "ws-remedy"); err != nil {
						t.Fatal(err)
					}
					fixDeploysAt(t, h, deployStarted.Add(time.Duration(i)*time.Minute))
				}
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
			err := dispatched(h, Decision{Restore: tt.restore})

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
		restore bool
		arrange func(t *testing.T, h *harness)
		message string
	}{
		{name: "a fresh workspace that cannot be made", message: "the remediation workspace could not be made; the queue stays held for the intake",
			arrange: func(t *testing.T, h *harness) { h.checks.createErr = errors.New("disk full") }},
		{name: "a restored workspace that cannot be reopened", restore: true, message: "the chain's last remediation workspace could not be reopened; the queue stays held for the intake",
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
			err := dispatched(h, Decision{Restore: tt.restore})

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

func TestAFreshRemediationsEffortIsSetOnceItsDispatchIsRecorded(t *testing.T) {
	tests := []struct {
		name        string
		restore     bool
		wantEfforts []string
	}{
		{name: "a fresh remediation", wantEfforts: []string{"remediation:ws-remedy:recorded=true"}},
		{name: "a restored remediation keeps the effort it ran at", restore: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			if tt.restore {
				withRemediation(t, h)
			}

			// Act
			err := dispatched(h, Decision{Restore: tt.restore})

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
