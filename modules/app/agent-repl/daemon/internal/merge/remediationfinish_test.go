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
	withIntake(t, h, deployStarted)
	h.register("ws-fixer", "deploy-remediation-x")
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
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
		{name: "the remediation at work", ws: "ws-fixer", want: true},
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
		Message: "the deploy remediation cannot land its fix; the chain is escalated to the user and the queue is held for them"}); !ok {
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

func TestARemediationClosedWhoseNameCannotBeReadKeepsTheHoldAndSaysSo(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx := context.Background()
	h.db.mu.Lock()
	h.db.workspaceErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	h.o.OnWorkspaceClosed(ctx, "ws-fixer")

	// Assert
	if owedCheck(t, h) == nil {
		t.Fatal("the check was escalated with no name to put before the user")
	}
	if !loggedLike(h, logLike{level: "error", op: opRemediationFinish, text: "could not be escalated without its name; the queue stays held", context: map[string]string{"workspace": "ws-fixer"}}) {
		t.Fatal("no ERROR record names the held queue and the workspace")
	}
}

func TestADeployEscalationsFinishLandsDirectly(t *testing.T) {
	// Arrange: the escalation workspace the daemon made, its chain ended.
	h := finishing(t)
	h.db.mu.Lock()
	h.db.workspaces["ws-escalation"] = h.db.workspaces["ws-fixer"]
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
	// Arrange: an escalation's finish stands deploying.
	h := finishing(t)
	ctx := context.Background()
	if err := h.db.RecordDeployEscalation(ctx, "ws-escalation", mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-escalation", Repo: mediationKey(h), StartedAt: deployStarted, ServesEscalation: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-escalation", wsm.RemediationGating, wsm.RemediationDeploying, deployStarted); err != nil {
		t.Fatal(err)
	}

	// Act: its deploy begins.
	err := h.o.remediationDeployed(ctx, mediationKey(h), deployStarted.Add(time.Minute))

	// Assert
	f, readErr := h.db.RemediationFinishOf(ctx, "ws-escalation")
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

func TestAClosedEscalationsFinishIsEnded(t *testing.T) {
	// Arrange: an escalation whose finish stands at fixing.
	h := finishing(t)
	ctx := context.Background()
	if err := h.db.RecordDeployEscalation(ctx, "ws-escalation", mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-escalation", Repo: mediationKey(h), StartedAt: deployStarted, ServesEscalation: true}); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.OnWorkspaceClosed(ctx, "ws-escalation")

	// Assert
	f, err := h.db.RemediationFinishOf(ctx, "ws-escalation")
	if err != nil || f != nil {
		t.Fatalf("finish = %+v (%v), want the closed escalation's finish ended", f, err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediationFinish, Message: "a closed deploy escalation's finish was ended"}); !ok {
		t.Fatalf("records = %+v, want the end recorded", h.logs.Records())
	}
}

func TestAFinishedRemediationStandsByAndReturnsOnlyWithWork(t *testing.T) {
	tests := []struct {
		name        string
		arrange     func(h *harness)
		wantResumed []ids.WorkspaceID
	}{
		{name: "a finish that lands leaves it standing by until its deploy closes it"},
		{name: "a gate that fails brings it back for its brief", wantResumed: []ids.WorkspaceID{"ws-fixer"},
			arrange: func(h *harness) { h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if tt.arrange != nil {
				h.git.mu.Lock()
				tt.arrange(h)
				h.git.mu.Unlock()
			}

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			if err != nil || !slices.Equal(h.checks.stoodBy, []ids.WorkspaceID{"ws-fixer"}) || !slices.Equal(h.checks.resumed, tt.wantResumed) {
				t.Fatalf("FinishRemediation = %v, stood by %v, resumed %v; want it standing by and resumed %v", err, h.checks.stoodBy, h.checks.resumed, tt.wantResumed)
			}
		})
	}
}

// TestAFinishedWorkspaceStandsByOnlyOnceItsTurnEnds covers the owner's
// 2026-11-17 ruling: the finish command is never cut mid-run, so the
// stand-by awaits the turn that finished, and a wait that fails stands
// nothing by and is recorded.
func TestAFinishedWorkspaceStandsByOnlyOnceItsTurnEnds(t *testing.T) {
	tests := []struct {
		name        string
		awaitErr    error
		wantStoodBy []ids.WorkspaceID
		wantRecord  string
	}{
		{name: "the turn that finished ends, then it stands by", wantStoodBy: []ids.WorkspaceID{"ws-fixer"}},
		{name: "a turn that cannot be awaited stands nothing by", awaitErr: errors.New("watcher gone"),
			wantRecord: "the finished workspace's turn could not be awaited; it does not stand by"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			h.db.mu.Lock()
			h.db.turns["ws-fixer"] = []wsm.Turn{{ID: "t-finish", Workspace: "ws-fixer"}}
			h.db.mu.Unlock()
			awaited := make(chan ids.TurnID, 1)
			h.mu.Lock()
			h.awaitStarted, h.awaitErr = awaited, tt.awaitErr
			h.mu.Unlock()

			// Act
			_ = h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			select {
			case turn := <-awaited:
				if turn != "t-finish" {
					t.Fatalf("awaited %q, want the turn that finished", turn)
				}
			default:
				t.Fatal("the turn that finished was never awaited")
			}
			if !slices.Equal(h.checks.stoodBy, tt.wantStoodBy) {
				t.Fatalf("stood by %v, want %v", h.checks.stoodBy, tt.wantStoodBy)
			}
			if tt.wantRecord != "" {
				if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish, Message: tt.wantRecord}); !ok {
					t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.wantRecord)
				}
			}
		})
	}
}

func TestAFinishedWorkspaceThatCannotStandByIsRecordedAndStillLands(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.checks.standByErr = errors.New("the shim would not stop")

	// Act
	err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

	// Assert
	if f := finishOf(t, h); err != nil || f == nil || f.Step != wsm.RemediationDeploying {
		t.Fatalf("FinishRemediation = %v, finish %+v; want it landed regardless", err, f)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish, Message: "the finished workspace could not stand by; its session runs on and it stays among the working rows"}); !ok {
		t.Fatalf("records = %v, want the failed stand-by at ERROR", h.logs.Records())
	}
}

func TestAStandByThatEndsTheFinishingSessionRunsOnALiveContext(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.checks.onStandBy = cancel

	// Act
	err := h.o.FinishRemediation(ctx, "ws-fixer", aRemediationPatch)

	// Assert
	if err != nil || !slices.Equal(h.checks.standByCtxErrs, []error{nil}) {
		t.Fatalf("FinishRemediation = %v, stand-by context errors %v; want one stand-by on a live context", err, h.checks.standByCtxErrs)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "could not republish the queue whose check changed"}); ok {
		t.Fatalf("records = %v, want the queue republished", h.logs.Records())
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
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationTesting, Remediation: "ws-fixer"}},
		{name: "a landing finish still tests its fix", path: []wsm.RemediationStep{wsm.RemediationLanding},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationTesting, Remediation: "ws-fixer"}},
		{name: "a finish its gate sent back fixes again", path: []wsm.RemediationStep{wsm.RemediationFixing},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationFixing, Intake: "ws-fixer", Remediation: "ws-fixer"}},
		{name: "a deploying finish hot-loads, selecting the chain's intake", path: []wsm.RemediationStep{wsm.RemediationLanding, wsm.RemediationDeploying},
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationWaiting, Intake: "ws-fixer"}},
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
// SINCE'th queue told, as STAGE:WORKSPACE AT WORK.
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
		at := m.Remediation
		if m.Stage == sidebar.MediationWaiting || m.Stage == sidebar.MediationChecking {
			at = m.Intake
		}
		out = append(out, fmt.Sprintf("%d:%s", m.Stage, at))
	}
	return out
}

func TestEveryStepOfAFinishRedrawsTheDeployCheckLabel(t *testing.T) {
	testing_ := fmt.Sprintf("%d:ws-fixer", sidebar.MediationTesting)
	fixing := fmt.Sprintf("%d:ws-fixer", sidebar.MediationFixing)
	hotLoading := fmt.Sprintf("%d:ws-fixer", sidebar.MediationWaiting)
	// THE FIX'S DEPLOY OWES A CHECK FOR A FRESH INTAKE, not yet made: the
	// intake that dispatched it decided once.
	hotLoadingFresh := fmt.Sprintf("%d:", sidebar.MediationWaiting)
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    []string
	}{
		{name: "a passing gate is tested, landed and hot-loaded",
			want: []string{testing_, testing_, hotLoading, hotLoadingFresh, hotLoadingFresh}},
		{name: "a failing gate is fixed, then tested, landed and hot-loaded",
			arrange: func(h *harness) { h.runner.runs = []scriptedRun{{Output: "FAIL", Code: 1}} },
			want:    []string{testing_, fixing, testing_, testing_, hotLoading, hotLoadingFresh, hotLoadingFresh}},
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
				if _, err := h.db.RecordMediationEscalation(context.Background(), mediationKey(h), deployStarted, "ws-fixer"); err != nil {
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
