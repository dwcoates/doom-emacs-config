package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/semver"
	"agent-repld/internal/wsm"
)

// The main branch's tip before and after the remediation lands.
const (
	tipBefore = "1111111111111111111111111111111111111111"
	tipAfter  = "2222222222222222222222222222222222222222"
)

// aRemediationPatch is a remediation finish's version increment.
var aRemediationPatch = &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "fix the reaper"}

// finishing is a checked harness whose chain's intake dispatched a fix to
// remediation workspace "ws-remedy", whose branch "feature" holds the main
// branch's tip tipBefore and lands at tipAfter.
func finishing(t *testing.T) *harness {
	t.Helper()
	h := checked(t)
	// A REMEDIATION LANDS WHERE A LOCAL MERGE TESTS AND VERSIONS.
	checks := h.config.DeployChecks
	h.config = localMergeConfig()
	h.config.DeployChecks = checks
	// THE CLOCK READS AFTER THE DEPLOY THE FIX IS DISPATCHED FOR.
	h.mu.Lock()
	h.now = deployStarted.Add(time.Minute)
	h.mu.Unlock()
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.register("ws-remedy", "deploy-remediation-x")
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-remedy"); err != nil {
		t.Fatal(err)
	}
	h.git.mu.Lock()
	if h.git.mainWorktrees == nil {
		h.git.mainWorktrees = map[string]string{}
	}
	h.git.mainWorktrees[h.sourceD] = h.targetD
	h.git.refSeqs["HEAD"] = []string{tipBefore, tipBefore, tipAfter}
	h.git.ancestry[tipBefore+">HEAD"] = true
	h.git.mu.Unlock()
	return h
}

// finishOf reads ws-remedy's finish.
func finishOf(t *testing.T, h *harness) *wsm.RemediationFinish {
	t.Helper()
	f, err := h.db.RemediationFinishOf(context.Background(), "ws-remedy")
	if err != nil {
		t.Fatalf("RemediationFinishOf: %v", err)
	}
	return f
}

func TestLandsDirectlyAnswersTheWorkspaceAFixIsDispatchedTo(t *testing.T) {
	tests := []struct {
		name string
		ws   ids.WorkspaceID
		want bool
	}{
		{name: "the remediation at work", ws: "ws-remedy", want: true},
		{name: "the chain's intake", ws: "ws-intake"},
		{name: "a deploy escalation workspace", ws: "ws-escalation", want: true},
		{name: "any other workspace", ws: theWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if err := h.db.RecordDeployEscalation(context.Background(), "ws-escalation", mediationKey(h), deployStarted); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := h.o.LandsDirectly(context.Background(), tt.ws)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("LandsDirectly = (%v, %v), want %v", got, err, tt.want)
			}
		})
	}
}

func TestARemediationFinishGatesLandsDirectlyAndDeploys(t *testing.T) {
	// Arrange
	h := finishing(t)

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-remedy", aRemediationPatch)

	// Assert
	if err != nil {
		t.Fatalf("FinishRemediation: %v", err)
	}
	f := finishOf(t, h)
	if f == nil || f.Step != wsm.RemediationDeploying || f.LandedHead != tipAfter {
		t.Fatalf("finish %+v, want it deploying having landed %s", f, tipAfter)
	}
	if len(h.runner.argv) != 1 || len(h.hooks.runs) != 1 || h.hooks.runs[0].argv[3] != "patch" || h.hooks.runs[0].argv[4] != "fix the reaper" {
		t.Fatalf("gate runs %v, landing runs %+v; want one full gate and one direct landing of a patch", h.runner.argv, h.hooks.runs)
	}
	h.db.mu.Lock()
	closed, closure, merged := h.db.closed["ws-remedy"], h.db.closures["ws-remedy"], !h.db.mergedAt["ws-remedy"].IsZero()
	h.db.mu.Unlock()
	if !closed || closure.By != wsm.CloseByMerge || !merged {
		t.Fatalf("closed %v by %q, merged %v; want it closed as a landed merge", closed, closure.By, merged)
	}
	if h.rollout.fired != 1 {
		t.Fatalf("rollout fired %d, want once", h.rollout.fired)
	}
	if m := owedCheck(t, h); m == nil || !m.Dispatched {
		t.Fatalf("check %+v, want the queue still held for the fix's deploy", m)
	}
	if !slices.Contains(h.meter.all(), "remediation_gates:"+RemediationGateLanded+":0") {
		t.Fatalf("recorded %v, want the landing's gate rounds", h.meter.all())
	}

	// Act: the deploy begins.
	oweAt(t, h, h.now.Add(time.Minute))

	// Assert
	if f := finishOf(t, h); f == nil || f.Step != wsm.RemediationDeployed {
		t.Fatalf("finish %+v, want it deployed once its deploy began", f)
	}
	if m := owedCheck(t, h); m == nil || m.Dispatched || m.ChainRedeploys != 1 {
		t.Fatalf("check %+v, want the fix's deploy owing the chain's next check", m)
	}
}

func TestARemediationFinishSendsTheWorkspaceBackToWork(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(h *harness)
		wantBrief  string
		wantOrigin conversationv1.PromptOrigin
		wantGates  int
	}{
		{name: "a branch that does not hold the main branch's tip rebases first", wantBrief: BriefRemediationBehind,
			arrange: func(h *harness) { h.git.ancestry[tipBefore+">HEAD"] = false }},
		{name: "a failing gate is fixed", wantBrief: BriefRemediationGateFailed, wantGates: 1,
			arrange: func(h *harness) { h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}} }},
		{name: "a branch that does not merge cleanly rebases first", wantBrief: BriefRemediationBehind, wantGates: 1,
			arrange: func(h *harness) { h.hooks.exits["feature"] = 1 }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			h.git.mu.Lock()
			tt.arrange(h)
			h.git.mu.Unlock()

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-remedy", aRemediationPatch)

			// Assert
			if err != nil {
				t.Fatalf("FinishRemediation: %v", err)
			}
			f := finishOf(t, h)
			if f == nil || f.Step != wsm.RemediationFixing || f.GateRounds != 1 {
				t.Fatalf("finish %+v, want it fixing after one round", f)
			}
			subs := h.queue.submissions
			if len(subs) != 1 || subs[0].WS != "ws-remedy" || subs[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR {
				t.Fatalf("submitted %+v, want the remediation resumed once", subs)
			}
			if got := h.briefValues[len(h.briefValues)-1]; got["attempt"] != "1" || len(h.runner.argv) != tt.wantGates || h.rollout.fired != 0 {
				t.Fatalf("brief values %v, gates %d, rollouts %d; want attempt 1, %d gates and no deploy", got, len(h.runner.argv), h.rollout.fired, tt.wantGates)
			}
			// THE NEXT FINISH GATES AGAIN.
			h.git.mu.Lock()
			h.git.ancestry[tipBefore+">HEAD"] = true
			h.git.ancestry[tipAfter+">HEAD"] = true
			h.git.mu.Unlock()
			h.hooks.exits["feature"] = 0
			if err := h.o.FinishRemediation(context.Background(), "ws-remedy", aRemediationPatch); err != nil {
				t.Fatalf("FinishRemediation again: %v", err)
			}
			if f := finishOf(t, h); f == nil || f.Step != wsm.RemediationDeploying {
				t.Fatalf("finish %+v, want the second finish landed", f)
			}
		})
	}
}

func TestARemediationPastItsFixingBoundIsEscalatedForTheIntake(t *testing.T) {
	// Arrange: MaxFixAttempts failed gates already.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-remedy", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < MaxFixAttempts; i++ {
		if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-remedy", wsm.RemediationGating, wsm.RemediationFixing, deployStarted); err != nil {
			t.Fatal(err)
		}
		if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-remedy", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
			t.Fatal(err)
		}
	}
	h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}}
	f := finishOf(t, h)

	// Act
	h.o.runRemediationFinish(ctx, *f)

	// Assert
	if owedCheck(t, h) != nil || !h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatalf("check %+v, banner %v; want the chain escalated and the queue let through", owedCheck(t, h), h.bannerRaised(desktopnotify.KindDeployMediated))
	}
	if !slices.Equal(h.checks.closed, []ids.WorkspaceID{"ws-intake"}) || !slices.Contains(h.meter.all(), "remediation_gates:"+RemediationGateGivenUp+":4") {
		t.Fatalf("closed %v, recorded %v; want the intake closed and the rounds given up", h.checks.closed, h.meter.all())
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish,
		Message: "the deploy remediation cannot land its fix; the chain is escalated to the user and the queue advances"}); !ok {
		t.Fatalf("records = %v, want the escalation at ERROR", h.logs.Records())
	}
}

func TestARemediationFinishIsRefusedWhereItCannotRun(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		version *wsm.VersionIncrement
		wantArm string
	}{
		{name: "a versioned repository's finish names its increment", wantArm: ArmVersionIncrementRequired},
		{name: "an unversioned repository's finish names none", version: aRemediationPatch, wantArm: ArmVersionIncrementUnexpected,
			arrange: func(t *testing.T, h *harness) { h.config.PreDeploy.LocalMerge.VersionTag = nil }},
		{name: "a finish already running", version: aRemediationPatch, wantArm: ArmAlreadyMerging,
			arrange: func(t *testing.T, h *harness) {
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-remedy", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-remedy", tt.version)

			// Assert
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("FinishRemediation = %v, want the %s refusal", err, tt.wantArm)
			}
			if len(h.runner.argv) != 0 {
				t.Fatalf("gate runs %v, want none", h.runner.argv)
			}
		})
	}
}

func TestAFinishOfAWorkspaceNoFixIsDispatchedToFailsLoudly(t *testing.T) {
	// Arrange
	h := finishing(t)

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-intake", aRemediationPatch)

	// Assert
	if err == nil {
		t.Fatal("FinishRemediation finished the intake")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish,
		Message: "a remediation finish reached a workspace no fix is dispatched to"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestALandingThatFailsLeavesTheFinishToBeTakenUpAgain(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.hooks.exits["feature"] = 2

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-remedy", aRemediationPatch)

	// Assert
	if err != nil {
		t.Fatalf("FinishRemediation: %v", err)
	}
	if f := finishOf(t, h); f == nil || f.Step != wsm.RemediationLanding {
		t.Fatalf("finish %+v, want it still landing for the next daemon", f)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the remediation's finish could not go on; it is taken up again by the next daemon that serves the queues"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestATakenUpFinishWhoseDeployBeganIsDeployed(t *testing.T) {
	// Arrange: a finish recorded deploying before a restart, its deploy
	// begun since.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-remedy", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-remedy", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, "ws-remedy", tipAfter, nil, deployStarted.Add(time.Second)); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.OweMediation(ctx, mediationKey(h), deployStarted.Add(time.Minute), ""); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.resumeRemediationFinishes(ctx)

	// Assert
	if f := finishOf(t, h); f == nil || f.Step != wsm.RemediationDeployed || h.rollout.fired != 0 {
		t.Fatalf("finish %+v, rollouts %d; want it deployed and no second deploy", f, h.rollout.fired)
	}
}

func TestARemediationClosedBeforeItsFixDeploysHandsTheHoldBack(t *testing.T) {
	tests := []struct {
		name         string
		step         wsm.RemediationStep
		wantEscalate bool
	}{
		{name: "closed with no finish", wantEscalate: true},
		{name: "closed while fixing", step: wsm.RemediationFixing, wantEscalate: true},
		{name: "closed by its own deploy", step: wsm.RemediationDeploying},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			ctx := context.Background()
			if tt.step != "" {
				if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-remedy", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
				if tt.step == wsm.RemediationDeploying {
					if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-remedy", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
						t.Fatal(err)
					}
					if _, err := h.db.RecordRemediationLanded(ctx, "ws-remedy", tipAfter, nil, deployStarted); err != nil {
						t.Fatal(err)
					}
				} else if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-remedy", wsm.RemediationGating, tt.step, deployStarted); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			h.o.OnWorkspaceClosed(ctx, "ws-remedy")

			// Assert
			if escalated := owedCheck(t, h) == nil; escalated != tt.wantEscalate {
				t.Fatalf("escalated %v, want %v", escalated, tt.wantEscalate)
			}
			if h.bannerRaised(desktopnotify.KindDeployMediated) != tt.wantEscalate {
				t.Fatalf("banner %v, want %v", h.bannerRaised(desktopnotify.KindDeployMediated), tt.wantEscalate)
			}
		})
	}
}

func TestARemediationClosedWhoseNameCannotBeReadKeepsTheHoldAndSaysSo(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaceErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	h.o.OnWorkspaceClosed(ctx, "ws-remedy")

	// Assert
	if owedCheck(t, h) == nil {
		t.Fatal("the check was escalated with no name to put before the user")
	}
	if !loggedLike(h, logLike{level: "error", op: opRemediationFinish, text: "could not be escalated without its name; the queue stays held", context: map[string]string{"workspace": "ws-remedy"}}) {
		t.Fatal("no ERROR record names the held queue and the workspace")
	}
}

func TestADeployEscalationsFinishLandsDirectly(t *testing.T) {
	// Arrange: the escalation workspace the daemon made, its chain ended.
	h := finishing(t)
	h.db.mu.Lock()
	h.db.workspaces["ws-escalation"] = h.db.workspaces["ws-remedy"]
	h.db.mu.Unlock()
	if err := h.db.RecordDeployEscalation(context.Background(), "ws-escalation", mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-escalation", aRemediationPatch)

	// Assert
	f, readErr := h.db.RemediationFinishOf(context.Background(), "ws-escalation")
	if err != nil || readErr != nil || f == nil || !sameQueue(f.Repo, mediationKey(h)) {
		t.Fatalf("FinishRemediation = %v; finish %+v (%v), want its direct finish recorded for its queue", err, f, readErr)
	}
}
