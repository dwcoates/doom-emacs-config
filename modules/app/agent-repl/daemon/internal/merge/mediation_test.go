package merge

import (
	"context"
	"errors"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// deployStarted is the instant the harness's deploys begin.
var deployStarted = time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)

// mediated is a harness whose repository names the deploy mediator
// "deploy-mediator", its brief registered.
func mediated(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.config.DeployMediator = &repoconfigv1.DeployMediator{Name: "deploy-mediator"}
	h.briefs[BriefDeployMediation] = []string{"deploy_started", "repo_dir", "version"}
	h.mediatorRepo = theRepository
	return h
}

// mediationKey is the queue key the harness's registered repository resolves
// to.
func mediationKey(h *harness) wsm.RepoKey { return scopedKey(h) }

// owe makes the harness's repository owe a mediation of the deploy at
// deployStarted.
func owe(t *testing.T, h *harness) {
	t.Helper()
	if err := h.o.OweMediations(context.Background(), deployStarted); err != nil {
		t.Fatalf("OweMediations: %v", err)
	}
}

func TestADeployOwesAMediationOnlyWhereAMediatorIsNamed(t *testing.T) {
	tests := []struct {
		name     string
		mediator bool
		want     bool
	}{
		{name: "a repository that names a mediator", mediator: true, want: true},
		{name: "a repository that names none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.mediator {
				h.config.DeployMediator = &repoconfigv1.DeployMediator{Name: "deploy-mediator"}
			}

			// Act
			owe(t, h)

			// Assert
			m, err := h.db.MediationOf(context.Background(), mediationKey(h))
			if err != nil || (m != nil) != tt.want {
				t.Fatalf("owed %+v (%v), want owed %v", m, err, tt.want)
			}
		})
	}
}

func TestARoundCompactsTheMediatorThenTellsItOfTheDeploy(t *testing.T) {
	// Arrange
	h := mediated(t)
	owe(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	subs := h.queue.submissions
	if len(subs) != 2 || subs[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT ||
		subs[1].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION || subs[0].WS != "ws-mediator" {
		t.Fatalf("submissions %+v, want the mediator's compaction, then its brief", subs)
	}
	if len(h.mediatorsMade) != 1 || h.awaitedTurns[len(h.awaitedTurns)-1] != subs[1].Turn {
		t.Fatalf("made %v, awaited %v; want the mediator made once and its brief's turn awaited", h.mediatorsMade, h.awaitedTurns)
	}
}

func TestATurnThatDecidesNothingLetsTheQueueThroughAndTellsTheUser(t *testing.T) {
	// Arrange
	h := mediated(t)
	owe(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m != nil {
		t.Fatalf("still owed %+v, want the undecided mediation released", m)
	}
	if !h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatal("the user was not told the mediator decided nothing")
	}
	if _, ok := h.recordFor("error", opMediation); !ok {
		t.Fatal("no ERROR record of the undecided mediation")
	}
}

func TestADecisionInTheTurnLeavesNothingToSettle(t *testing.T) {
	// Arrange: the mediator approves as its brief is delivered.
	h := mediated(t)
	owe(t, h)
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION {
			if err := h.o.DecideMediation(context.Background(), sub.WS, MediationApprove, "clean"); err != nil {
				t.Errorf("DecideMediation: %v", err)
			}
		}
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m != nil {
		t.Fatalf("still owed %+v, want it released by the decision", m)
	}
	if h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatal("an approved deploy raised a banner")
	}
}

func TestAMediationAlreadyToldAwaitsItsTurnWithoutTellingAgain(t *testing.T) {
	// Arrange: the mediator was told in turn-told.
	h := mediated(t)
	owe(t, h)
	if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-told"); err != nil {
		t.Fatal(err)
	}
	h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 || len(h.mediatorsMade) != 0 || h.awaitedTurns[len(h.awaitedTurns)-1] != "turn-told" {
		t.Fatalf("submitted %d, made %v, awaited %v; want only turn-told awaited", len(h.queue.submissions), h.mediatorsMade, h.awaitedTurns)
	}
}

func TestADaemonThatDoesNotServeTheQueuesRunsNoRound(t *testing.T) {
	// Arrange
	h := mediated(t)
	owe(t, h)
	h.standing.serves = false

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %d prompts, want none", len(h.queue.submissions))
	}
}

func TestAMediatorThatCannotBeMadeLeavesTheQueueHeld(t *testing.T) {
	// Arrange
	h := mediated(t)
	owe(t, h)
	h.createMediatorErr = errors.New("worktree gone")

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m == nil {
		t.Fatal("the mediation was released though no mediator could check it")
	}
	if _, ok := h.recordFor("error", opMediation); !ok {
		t.Fatal("no ERROR record of the mediator that could not be made")
	}
}

func TestDecideMediation(t *testing.T) {
	tests := []struct {
		name       string
		from       ids.WorkspaceID
		owed       bool
		decision   MediationDecision
		wantArm    string
		wantBanner bool
	}{
		{name: "an approval releases the deploy", from: "ws-mediator", owed: true, decision: MediationApprove},
		{name: "an escalation releases it and tells the user", from: "ws-mediator", owed: true, decision: MediationEscalate, wantBanner: true},
		{name: "a decision from any other workspace is refused", from: theWorkspace, owed: true, decision: MediationApprove, wantArm: ArmNotMediator},
		{name: "a decision with nothing owed is refused", from: "ws-mediator", decision: MediationApprove, wantArm: ArmNoMediationOwed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			if tt.owed {
				owe(t, h)
			}

			// Act
			err := h.o.DecideMediation(context.Background(), tt.from, tt.decision, "the summary")

			// Assert
			var refused *RefusalError
			if tt.wantArm != "" {
				if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
					t.Fatalf("DecideMediation = %v, want the %s refusal", err, tt.wantArm)
				}
				return
			}
			m, _ := h.db.MediationOf(context.Background(), mediationKey(h))
			if err != nil || m != nil || h.bannerRaised(desktopnotify.KindDeployMediated) != tt.wantBanner {
				t.Fatalf("DecideMediation = %v, owed %+v, banner %v; want released, banner %v", err, m, h.bannerRaised(desktopnotify.KindDeployMediated), tt.wantBanner)
			}
		})
	}
}

func TestTheRosterIsToldTheMediationTheQueueWaitsOn(t *testing.T) {
	tests := []struct {
		name     string
		made     bool
		told     bool
		wantNil  bool
		wantTold bool
	}{
		{name: "a mediator not made yet draws nothing", wantNil: true},
		{name: "a made mediator not yet told waits", made: true},
		{name: "a told mediator is checking", made: true, told: true, wantTold: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			if tt.made {
				h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			}
			owe(t, h)
			if tt.told {
				if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-1"); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := h.o.rosterMediation(context.Background(), mediationKey(h))

			// Assert
			if err != nil || (got == nil) != tt.wantNil || (got != nil && (got.Told != tt.wantTold || got.Mediator != "ws-mediator")) {
				t.Fatalf("rosterMediation = (%+v, %v), want nil %v, told %v", got, err, tt.wantNil, tt.wantTold)
			}
		})
	}
}

func TestMediateOnAStoppingDaemon(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		cancelled bool
		wantLevel string
	}{
		{name: "a registry read the daemon's stop cut is withdrawn", cancelled: true, wantLevel: "info",
			arrange: func(h *harness) { h.db.reposErr = context.Canceled }},
		{name: "a repository's resolve the daemon's stop cut is withdrawn, not passed over", cancelled: true, wantLevel: "info",
			arrange: func(h *harness) { h.git.commonDirErr = context.Canceled }},
		{name: "a mediation read the daemon's stop cut is withdrawn", cancelled: true, wantLevel: "info",
			arrange: func(h *harness) { h.db.mediationErr = context.Canceled }},
		{name: "a registry read that fails while the daemon runs is an error", wantLevel: "error",
			arrange: func(h *harness) { h.db.reposErr = errors.New("disk gone") }},
		{name: "a mediation read that fails while the daemon runs is an error", wantLevel: "error",
			arrange: func(h *harness) { h.db.mediationErr = errors.New("disk gone") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			owe(t, h)
			tt.arrange(h)
			ctx, cancel := context.WithCancel(context.Background())
			if tt.cancelled {
				cancel()
			} else {
				defer cancel()
			}

			// Act
			h.o.Mediate(ctx)

			// Assert
			if len(h.queue.submissions) != 0 {
				t.Fatalf("submitted %d prompts, want none", len(h.queue.submissions))
			}
			_, errored := h.recordFor("error", opMediation)
			_, withdrawn := h.recordFor("info", opMediation)
			_, warned := h.recordFor("warn", opMediation)
			if tt.wantLevel == "info" && (errored || warned || !withdrawn) {
				t.Fatalf("error recorded %v, withdrawal recorded %v; want only the withdrawal at INFO", errored, withdrawn)
			}
			if tt.wantLevel == "error" && !errored {
				t.Fatal("no ERROR record of the failed read")
			}
		})
	}
}
