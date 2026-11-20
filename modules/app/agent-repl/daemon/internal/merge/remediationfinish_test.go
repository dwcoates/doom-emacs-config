package merge

import (
	"agent-repld/internal/prompts"
	"agent-repld/internal/sourcescan"
	"context"
	"errors"
	"fmt"
	"regexp"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
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
// remediation workspace "ws-fixer", whose branch "feature" holds the main
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
	withMediator(t, h)
	h.register("ws-fixer", "deploy-remediation-x")
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, aFixerTurn); err != nil {
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

// finishOf reads ws-fixer's finish.
func finishOf(t *testing.T, h *harness) *wsm.RemediationFinish {
	t.Helper()
	f, err := h.db.RemediationFinishOf(context.Background(), "ws-fixer")
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
		{name: "the deploy mediator", ws: "ws-fixer", want: true},
		{name: "any other workspace", ws: theWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)

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
	err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	if err != nil {
		t.Fatalf("FinishRemediation: %v", err)
	}
	f := finishOf(t, h)
	if f == nil || f.Step != wsm.RemediationDeploying || f.LandedHead != tipAfter {
		t.Fatalf("finish %+v, want it deploying having landed %s", f, tipAfter)
	}
	// The landing lands the commit its gate passed on, without a second
	// suite: land-direct.sh --gated-at COMMIT REPO PART LINE BRANCH.
	if len(h.runner.argv) != 1 || len(h.hooks.runs) != 1 || h.hooks.runs[0].argv[2] != "--gated-at" || h.hooks.runs[0].argv[3] == "" ||
		h.hooks.runs[0].argv[5] != "patch" || h.hooks.runs[0].argv[6] != "fix the reaper" {
		t.Fatalf("gate runs %v, landing runs %+v; want one full gate and one direct landing of a patch at its gated commit", h.runner.argv, h.hooks.runs)
	}
	h.db.mu.Lock()
	closed, closure, merged := h.db.closed["ws-fixer"], h.db.closures["ws-fixer"], !h.db.mergedAt["ws-fixer"].IsZero()
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

func TestARemediationsDeployCompactsTheMediatorWhateverItsContext(t *testing.T) {
	// Arrange: a context far under the quiet check's bound.
	h := finishing(t)
	h.checks.contextTokens = 1

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	if err != nil {
		t.Fatalf("FinishRemediation: %v", err)
	}
	if !slices.Equal(h.checks.compacted, []ids.WorkspaceID{"ws-fixer"}) {
		t.Fatalf("compacted %v, want the mediator compacted as its deploy starts", h.checks.compacted)
	}
}

func TestARemediationThatDoesNotDeployCompactsNothing(t *testing.T) {
	// Arrange: the gate fails, so nothing lands or deploys.
	h := finishing(t)
	h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}}

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	if err != nil {
		t.Fatalf("FinishRemediation: %v", err)
	}
	if len(h.checks.compacted) != 0 {
		t.Fatalf("compacted %v, want none before a deploy starts", h.checks.compacted)
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
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			if err != nil {
				t.Fatalf("FinishRemediation: %v", err)
			}
			f := finishOf(t, h)
			if f == nil || f.Step != wsm.RemediationFixing || f.GateRounds != 1 {
				t.Fatalf("finish %+v, want it fixing after one round", f)
			}
			subs := h.queue.submissions
			if len(subs) != 1 || subs[0].WS != "ws-fixer" || subs[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR {
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
			if err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch); err != nil {
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
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < MaxFixAttempts; i++ {
		if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationFixing, deployStarted); err != nil {
			t.Fatal(err)
		}
		if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
			t.Fatal(err)
		}
	}
	h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}}
	f := finishOf(t, h)

	// Act
	h.o.runRemediationFinish(ctx, *f)

	// Assert
	if heldEscalation(t, h) != "ws-fixer" || !h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatalf("check %+v, banner %v; want the chain escalated and the queue held for the user", owedCheck(t, h), h.bannerRaised(desktopnotify.KindDeployMediated))
	}
	if !slices.Contains(h.meter.all(), "remediation_gates:"+RemediationGateGivenUp+":4") {
		t.Fatalf("recorded %v; want the rounds given up", h.meter.all())
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish,
		Message: "the deploy mediator cannot land its fix; the chain is escalated to the user and the queue is held for them"}); !ok {
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
				if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
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
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", tt.version)

			// Assert
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("FinishRemediation = %v, want the %s refusal", err, tt.wantArm)
			}
			if len(h.runner.argv) != 0 {
				t.Fatalf("gate runs %v, want none", h.runner.argv)
			}
			if tt.wantArm == ArmAlreadyMerging {
				if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediationFinish, Message: "refused a remediation finish while its finish runs"}); !ok {
					t.Fatalf("records = %v, want the refusal at INFO: the caller is answered", h.logs.Records())
				}
			}
		})
	}
}

func TestAFinishOfAWorkspaceNoFixIsDispatchedToFailsLoudly(t *testing.T) {
	// Arrange
	h := finishing(t)

	// Act
	err := h.o.FinishRemediation(context.Background(), theWorkspace, aRemediationPatch)

	// Assert
	if err == nil {
		t.Fatal("FinishRemediation finished a workspace no fix is dispatched to")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish,
		Message: "a direct finish reached a workspace that lands through the queue"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestALandingThatFailsLeavesTheFinishToBeTakenUpAgain(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.hooks.exits["feature"] = 2

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

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
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, "ws-fixer", tipAfter, nil, deployStarted.Add(time.Second)); err != nil {
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
				if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
					t.Fatal(err)
				}
				if tt.step == wsm.RemediationDeploying {
					if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
						t.Fatal(err)
					}
					if _, err := h.db.RecordRemediationLanded(ctx, "ws-fixer", tipAfter, nil, deployStarted); err != nil {
						t.Fatal(err)
					}
				} else if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, tt.step, deployStarted); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			h.o.OnWorkspaceClosed(ctx, "ws-fixer")

			// Assert
			if m := owedCheck(t, h); (m != nil && m.Escalated()) != tt.wantEscalate {
				t.Fatalf("check %+v, want escalated %v", m, tt.wantEscalate)
			}
			if h.bannerRaised(desktopnotify.KindDeployMediated) != tt.wantEscalate {
				t.Fatalf("banner %v, want %v", h.bannerRaised(desktopnotify.KindDeployMediated), tt.wantEscalate)
			}
		})
	}
}

// TestEveryBriefTheQueueNamesIsCheckedIn pins that each brief name the merge
// package declares has its prompt in the repository: the remediation's
// gate-failure brief was named but never written, so a failed gate stranded
// the finish (2026-11-15, deploy-escalation-20261115-174608).
func TestEveryBriefTheQueueNamesIsCheckedIn(t *testing.T) {
	// Arrange
	declared := regexp.MustCompile(`\bBrief[A-Za-z]+\s*=\s*"([a-z][a-z-]*)"`)
	var names []string
	for _, f := range sourcescan.Production(t) {
		for _, m := range declared.FindAllSubmatch(f.Source, -1) {
			names = append(names, string(m[1]))
		}
	}
	if len(names) == 0 {
		t.Fatal("no brief name was found; the scan reads nothing")
	}

	// Act, Assert
	for _, name := range names {
		if _, err := prompts.Load("../../../prompts", name); err != nil {
			t.Errorf("brief %q: %v", name, err)
		}
	}
}

func TestADeployedEscalationsFinishIsEnded(t *testing.T) {
	// Arrange: the mediator's finish for an escalation stands deploying.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted, ServesEscalation: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationDeploying, deployStarted); err != nil {
		t.Fatal(err)
	}

	// Act: its deploy begins.
	err := h.o.remediationDeployed(ctx, mediationKey(h), deployStarted.Add(time.Minute))

	// Assert
	f, readErr := h.db.RemediationFinishOf(ctx, "ws-fixer")
	if err != nil || readErr != nil || f != nil {
		t.Fatalf("remediationDeployed = %v; finish %+v (%v), want the escalation's finish ended at its deploy", err, f, readErr)
	}
}

// TestTheRemediationsResumeBriefsTakeTheValuesTheResumeSupplies composes each
// brief a remediation is resumed with from the checked-in prompts, with
// exactly the values resumeRemediation passes: the behind brief once lacked
// the attempt placeholders the resume adds, and stranded the finish
// (2026-11-15 18:56, deploy-escalation-20261115-174608).
func TestTheRemediationsResumeBriefsTakeTheValuesTheResumeSupplies(t *testing.T) {
	tests := []struct {
		brief  string
		values map[string]string
	}{
		{brief: BriefRemediationGateFailed, values: map[string]string{"archive_path": "/a", "failing_tests": "TestX"}},
		{brief: BriefRemediationBehind, values: map[string]string{"main_branch": "master", "why": "behind"}},
	}
	for _, tt := range tests {
		t.Run(tt.brief, func(t *testing.T) {
			// Arrange: the values resumeRemediation adds to every brief.
			tt.values["attempt"], tt.values["max_attempts"] = "1", fmt.Sprint(MaxFixAttempts)

			// Act
			_, err := BriefsFrom("../../../prompts")(tt.brief, tt.values)

			// Assert
			if err != nil {
				t.Fatalf("compose %s: %v", tt.brief, err)
			}
		})
	}
}

func TestABranchThatMovedWhileItsGateRanIsSentBackNotLanded(t *testing.T) {
	// Arrange: the branch's head reads one commit before the gate and
	// another after it passes.
	h := finishing(t)
	record, err := h.db.Workspace(context.Background(), "ws-fixer")
	if err != nil {
		t.Fatal(err)
	}
	h.git.mu.Lock()
	h.git.refSeqs[record.Branch] = []string{"1111111111111111111111111111111111111111", "3333333333333333333333333333333333333333"}
	h.git.mu.Unlock()

	// Act
	err = h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	f := finishOf(t, h)
	if err != nil || f == nil || f.Step != wsm.RemediationFixing || len(h.hooks.runs) != 0 {
		t.Fatalf("FinishRemediation = %v, finish %+v, landings %d; want it sent back to work, nothing landed", err, f, len(h.hooks.runs))
	}
}

func TestAClosedMediatorsEscalationFinishIsEnded(t *testing.T) {
	// Arrange: the check is escalated, and the mediator's finish for it
	// stands at fixing.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.EscalateMediation(ctx, mediationKey(h), deployStarted, "{}"); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted, ServesEscalation: true}); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.OnWorkspaceClosed(ctx, "ws-fixer")

	// Assert
	f, err := h.db.RemediationFinishOf(ctx, "ws-fixer")
	if err != nil || f != nil || owedCheck(t, h) != nil {
		t.Fatalf("finish = %+v (%v), check %+v; want the escalation released and its finish ended", f, err, owedCheck(t, h))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediationFinish, Message: "a closed deploy mediator's finish was ended"}); !ok {
		t.Fatalf("records = %+v, want the end recorded", h.logs.Records())
	}
}

// TestAFinishedRemediationKeepsItsSessionAndReturnsOnlyWithWork covers the
// owner's 2026-11-20 ruling: the mediator stands in remote control for good,
// so its finish never ends its session.
func TestAFinishedRemediationKeepsItsSessionAndReturnsOnlyWithWork(t *testing.T) {
	tests := []struct {
		name        string
		arrange     func(h *harness)
		wantResumed []ids.WorkspaceID
	}{
		{name: "a finish that lands keeps its session"},
		{name: "a gate that fails brings it back for its brief", wantResumed: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(h *harness) { h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			asDeployMediator(h, "ws-fixer")
			if tt.arrange != nil {
				h.git.mu.Lock()
				tt.arrange(h)
				h.git.mu.Unlock()
			}

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			if err != nil || len(h.stoppedSessions) != 0 || !slices.Equal(h.checks.resumed, tt.wantResumed) {
				t.Fatalf("FinishRemediation = %v, stopped %v, resumed %v; want its session left running and resumed %v", err, h.stoppedSessions, h.checks.resumed, tt.wantResumed)
			}
		})
	}
}

// TestAFinishedWorkspacesGateWaitsForItsTurnToEnd covers the owner's
// 2026-11-17 ruling: the finish command is never cut mid-run, so the gate
// awaits the turn that finished; a wait that fails is recorded and the
// finish still lands.
func TestAFinishedWorkspacesGateWaitsForItsTurnToEnd(t *testing.T) {
	tests := []struct {
		name         string
		awaitErr     error
		openTurnsErr error
		wantAwaited  bool
		wantRecord   string
	}{
		{name: "the turn that finished ends, then the gate runs", wantAwaited: true},
		{name: "a turn that cannot be awaited is recorded", awaitErr: errors.New("watcher gone"), wantAwaited: true,
			wantRecord: "the finished workspace's turn could not be awaited; its gate runs without waiting for it"},
		{name: "open turns that cannot be read are recorded", openTurnsErr: errors.New("database is locked"),
			wantRecord: "the finished workspace's open turns could not be read; its gate runs without waiting for them"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			h.db.mu.Lock()
			h.db.turns["ws-fixer"] = []wsm.Turn{{ID: "t-finish", Workspace: "ws-fixer"}}
			h.db.openTurnsErr = tt.openTurnsErr
			h.db.mu.Unlock()
			awaited := make(chan ids.TurnID, 1)
			h.mu.Lock()
			h.awaitStarted, h.awaitErr = awaited, tt.awaitErr
			h.mu.Unlock()

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			select {
			case turn := <-awaited:
				if !tt.wantAwaited || turn != "t-finish" {
					t.Fatalf("awaited %q, want awaited %v the turn that finished", turn, tt.wantAwaited)
				}
			default:
				if tt.wantAwaited {
					t.Fatal("the turn that finished was never awaited")
				}
			}
			h.db.mu.Lock()
			h.db.openTurnsErr = nil
			h.db.mu.Unlock()
			if f := finishOf(t, h); err != nil || f == nil || f.Step != wsm.RemediationDeploying {
				t.Fatalf("FinishRemediation = %v, finish %+v; want it landed", err, f)
			}
			if tt.wantRecord != "" {
				if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish, Message: tt.wantRecord}); !ok {
					t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.wantRecord)
				}
			}
		})
	}
}

// asDeployMediator records WS as its repository's deploy mediator, as
// production always records the workspace a finish lands directly for.
func asDeployMediator(h *harness, ws ids.WorkspaceID) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	record := h.db.workspaces[ws]
	record.DeployMediator = true
	h.db.workspaces[ws] = record
}

func TestTheFinishingTurnIsAwaitedOnTheMediationsLifetimeNotTheRequests(t *testing.T) {
	// Arrange: the finish request's context is cut while its turn is awaited.
	h := finishing(t)
	h.db.mu.Lock()
	h.db.turns["ws-fixer"] = []wsm.Turn{{ID: "t-finish", Workspace: "ws-fixer"}}
	h.db.mu.Unlock()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	started, gate := make(chan ids.TurnID), make(chan struct{})
	h.mu.Lock()
	h.awaitStarted, h.awaitGate = started, gate
	h.mu.Unlock()
	go func() {
		<-started
		cancel()
		close(gate)
	}()

	// Act
	err := h.o.FinishRemediation(ctx, "ws-fixer", aRemediationPatch)

	// Assert
	if f := finishOf(t, h); err != nil || f == nil || f.Step != wsm.RemediationDeploying {
		t.Fatalf("FinishRemediation = %v, finish %+v; want it landed", err, f)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish}); ok {
		t.Fatalf("records = %v, want the turn awaited on a live context", h.logs.Records())
	}
}

func TestTheDeployCheckLabelDrawsEachStepOfAFinish(t *testing.T) {
	tests := []struct {
		name    string
		path    []wsm.RemediationStep
		chained bool
		want    *sidebar.QueueMediation
	}{
		{name: "a gating finish tests its fix", path: nil,
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationTesting, Mediator: "ws-fixer"}},
		{name: "a landing finish still tests its fix", path: []wsm.RemediationStep{wsm.RemediationLanding},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationTesting, Mediator: "ws-fixer"}},
		{name: "a finish its gate sent back fixes again", path: []wsm.RemediationStep{wsm.RemediationFixing},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationFixing, Mediator: "ws-fixer"}},
		{name: "a deploying finish hot-loads, selecting the mediator", path: []wsm.RemediationStep{wsm.RemediationLanding, wsm.RemediationDeploying},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationWaiting, Mediator: "ws-fixer"}},
		{name: "a finish deploying after its chain ended hot-loads, selecting nothing", path: []wsm.RemediationStep{wsm.RemediationLanding, wsm.RemediationDeploying}, chained: true,
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationWaiting}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			ctx := context.Background()
			if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
				t.Fatal(err)
			}
			from := wsm.RemediationGating
			for _, to := range tt.path {
				if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", from, to, deployStarted); err != nil {
					t.Fatal(err)
				}
				from = to
			}
			if tt.chained {
				if _, err := h.db.ReleaseMediation(ctx, mediationKey(h), deployStarted); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := h.o.rosterMediation(ctx, mediationKey(h))

			// Assert
			if err != nil || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("rosterMediation = (%+v, %v), want %+v", got, err, tt.want)
			}
		})
	}
}

// toldStages is every deploy check label REPO's queue was told since the
// SINCE'th queue told, as STAGE:MEDIATOR.
func toldStages(h *harness, since int) []string {
	h.sidebar.mu.Lock()
	defer h.sidebar.mu.Unlock()
	var out []string
	for _, q := range h.sidebar.told[since:] {
		m := q.Mediation
		if m == nil {
			out = append(out, "none")
			continue
		}
		out = append(out, fmt.Sprintf("%d:%s", m.Stage, m.Mediator))
	}
	return out
}

func TestEveryStepOfAFinishRedrawsTheDeployCheckLabel(t *testing.T) {
	testing_ := fmt.Sprintf("%d:ws-fixer", sidebar.MediationTesting)
	fixing := fmt.Sprintf("%d:ws-fixer", sidebar.MediationFixing)
	hotLoading := fmt.Sprintf("%d:ws-fixer", sidebar.MediationWaiting)
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    []string
	}{
		{name: "a passing gate is tested, landed and hot-loaded",
			want: []string{testing_, testing_, hotLoading, hotLoading, hotLoading}},
		{name: "a failing gate is fixed, then tested, landed and hot-loaded",
			arrange: func(h *harness) { h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}} },
			want:    []string{testing_, fixing, testing_, testing_, hotLoading, hotLoading, hotLoading}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if tt.arrange != nil {
				tt.arrange(h)
			}
			since := h.sidebar.toldCount()
			ctx := context.Background()

			// Act: finish, again once the gate sent the workspace back, then
			// the fix's deploy begins.
			if err := h.o.FinishRemediation(ctx, "ws-fixer", aRemediationPatch); err != nil {
				t.Fatalf("FinishRemediation: %v", err)
			}
			if f := finishOf(t, h); f != nil && f.Step == wsm.RemediationFixing {
				if err := h.o.FinishRemediation(ctx, "ws-fixer", aRemediationPatch); err != nil {
					t.Fatalf("FinishRemediation again: %v", err)
				}
			}
			oweAt(t, h, h.now.Add(time.Minute))

			// Assert
			if got := toldStages(h, since); !slices.Equal(got, tt.want) {
				t.Fatalf("labels told %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAFinishServesWhatItsCheckHoldsItFor(t *testing.T) {
	tests := []struct {
		name      string
		escalated bool
		want      bool
	}{
		{name: "a fix dispatched to it is its chain's"},
		{name: "an escalation it settles is served", escalated: true, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if tt.escalated {
				if _, err := h.db.EscalateMediation(context.Background(), mediationKey(h), deployStarted, "{}"); err != nil {
					t.Fatal(err)
				}
			}
			h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}}

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			if f := finishOf(t, h); err != nil || f == nil || f.ServesEscalation != tt.want {
				t.Fatalf("FinishRemediation = %v, finish %+v; want serves escalation %v", err, f, tt.want)
			}
		})
	}
}
