package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
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
	compact, brief := conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION
	tests := []struct {
		name        string
		conversed   bool
		wantOrigins []conversationv1.PromptOrigin
		wantMade    int
	}{
		{name: "a mediator made for this round has nothing to compact, so it is only told", wantOrigins: []conversationv1.PromptOrigin{brief}, wantMade: 1},
		{name: "a mediator that ran a turn is compacted, then told", conversed: true, wantOrigins: []conversationv1.PromptOrigin{compact, brief}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			owe(t, h)
			if tt.conversed {
				h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
				h.db.turns["ws-mediator"] = []wsm.Turn{{ID: "turn-before", Workspace: "ws-mediator"}}
			}

			// Act
			h.o.Mediate(context.Background())

			// Assert
			subs := h.queue.submissions
			var origins []conversationv1.PromptOrigin
			for _, sub := range subs {
				if sub.WS != "ws-mediator" {
					t.Fatalf("submitted to %s, want the mediator", sub.WS)
				}
				origins = append(origins, sub.Origin)
			}
			if fmt.Sprint(origins) != fmt.Sprint(tt.wantOrigins) {
				t.Fatalf("submitted %v, want %v", origins, tt.wantOrigins)
			}
			if len(h.mediatorsMade) != tt.wantMade || h.awaitedTurns[len(h.awaitedTurns)-1] != subs[len(subs)-1].Turn {
				t.Fatalf("made %v, awaited %v; want %d made and the brief's turn awaited", h.mediatorsMade, h.awaitedTurns, tt.wantMade)
			}
		})
	}
}

func TestAMediatorWhoseTurnsCannotBeReadIsNotTold(t *testing.T) {
	// Arrange
	h := mediated(t)
	owe(t, h)
	h.db.hasTurnsErr = errors.New("disk gone")

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %d prompts, want none", len(h.queue.submissions))
	}
	if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m == nil || m.Prompted() {
		t.Fatalf("mediation %+v, want it still owed and untold", m)
	}
	if _, ok := h.recordFor("error", opMediation); !ok {
		t.Fatal("no ERROR record of the unreadable turns")
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

func TestARoundWhoseTurnCannotBeAwaitedKeepsTheQueueHeld(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{name: "a mediator told in this round"},
		{name: "a mediator told before, its turn reattached", arrange: func(t *testing.T, h *harness) {
			h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-told"); err != nil {
				t.Fatalf("MarkMediationPrompted: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			owe(t, h)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}
			h.awaitErr = errors.New("the session watcher is gone")

			// Act
			h.o.Mediate(context.Background())

			// Assert: recorded at ERROR, and the mediation still owed, so the
			// queue stays held rather than let through as decided nothing.
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
				Message: "the mediator's turn could not be awaited; the queue stays held until the next round"}); !ok {
				t.Fatalf("records = %v, want the failed await at ERROR", h.logs.Records())
			}
			if m, _ := h.db.MediationOf(context.Background(), mediationKey(h)); m == nil {
				t.Fatal("the failed await released the mediation and let the queue through")
			}
			if h.bannerRaised(desktopnotify.KindDeployMediated) {
				t.Fatal("the user was told the mediator decided nothing, for a round that could not await")
			}
		})
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

func TestAMediationAlreadyToldIsReattachedToItsTurn(t *testing.T) {
	tests := []struct {
		name          string
		delivered     bool
		wantSubmitted int
	}{
		{name: "a turn the session ran is awaited without telling again", delivered: true},
		{name: "a turn recorded but never queued is submitted then, with the brief", wantSubmitted: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the mediator was told in turn-told.
			h := mediated(t)
			owe(t, h)
			if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-told"); err != nil {
				t.Fatal(err)
			}
			h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			if tt.delivered {
				h.db.turns["ws-mediator"] = append(h.db.turns["ws-mediator"], wsm.Turn{ID: "turn-told", Workspace: "ws-mediator"})
			}

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if len(h.queue.submissions) != tt.wantSubmitted || len(h.mediatorsMade) != 0 || h.awaitedTurns[len(h.awaitedTurns)-1] != "turn-told" {
				t.Fatalf("submitted %d, made %v, awaited %v; want %d submitted and turn-told awaited", len(h.queue.submissions), h.mediatorsMade, h.awaitedTurns, tt.wantSubmitted)
			}
			for _, sub := range h.queue.submissions {
				if sub.Turn != "turn-told" || sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION {
					t.Fatalf("submitted turn %s with origin %v, want turn-told as the deploy mediation", sub.Turn, sub.Origin)
				}
			}
		})
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
		{name: "a queue owing nothing draws nothing", wantNil: true},
		{name: "a mediation whose mediator is not made yet waits", made: false},
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
			if !tt.wantNil {
				owe(t, h)
			}
			if tt.told {
				if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-1"); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := h.o.rosterMediation(context.Background(), mediationKey(h))

			// Assert
			if err != nil || (got == nil) != tt.wantNil || (got != nil && (got.Told != tt.wantTold || got.Repo != theRepository)) {
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

func TestExistingMediator(t *testing.T) {
	tests := []struct {
		name    string
		present []wsm.Workspace
		listErr error
		wantWS  ids.WorkspaceID
		wantOK  bool
		wantErr bool
	}{
		{name: "none is made yet"},
		{name: "the open workspace of the repository carrying the mediator's name", wantWS: "m1", wantOK: true,
			present: []wsm.Workspace{{ID: "m1", Name: "deploy-mediator", Repo: theRepository}}},
		{name: "a closed one is not the mediator",
			present: []wsm.Workspace{{ID: "m1", Name: "deploy-mediator", Repo: theRepository, Closed: true}}},
		{name: "one of the name in another repository is not the mediator",
			present: []wsm.Workspace{{ID: "m1", Name: "deploy-mediator", Repo: "elsewhere"}}},
		{name: "one of another name in the repository is not the mediator",
			present: []wsm.Workspace{{ID: "m1", Name: "feature", Repo: theRepository}}},
		{name: "a registry that cannot be read is answered", listErr: errors.New("disk gone"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			for _, ws := range tt.present {
				h.db.workspaces[ws.ID] = ws
			}
			h.db.listWorkspacesErr = tt.listErr
			queues, err := h.o.mediatedQueues(context.Background(), opMediation)
			if err != nil || len(queues) != 1 {
				t.Fatalf("mediatedQueues = %v, %v; want the one mediated queue", queues, err)
			}

			// Act
			ws, ok, err := h.o.existingMediator(context.Background(), queues[0])

			// Assert
			if ws != tt.wantWS || ok != tt.wantOK || (err != nil) != tt.wantErr {
				t.Fatalf("existingMediator = %q, %v, %v; want %q, %v, error %v", ws, ok, err, tt.wantWS, tt.wantOK, tt.wantErr)
			}
		})
	}
}

func TestEveryMediatorLookupGoesThroughExistingMediator(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("mediation.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	matches := strings.Count(string(src), "ws.Name == q.mediator")

	// Assert
	if matches != 1 {
		t.Fatalf("mediation.go matches a workspace to the mediator's name %d times; want once, in existingMediator", matches)
	}
}

func TestOwedMediation(t *testing.T) {
	tests := []struct {
		name string
		owed bool
		made bool
		told bool
		want *OwedMediation
	}{
		{name: "a queue owing none answers none"},
		{name: "an owed mediation whose mediator is not made names no workspace", owed: true,
			want: &OwedMediation{Name: "deploy-mediator", Repository: theRepository}},
		{name: "an owed mediation names its made mediator", owed: true, made: true,
			want: &OwedMediation{Name: "deploy-mediator", Repository: theRepository, Mediator: "ws-mediator"}},
		{name: "an owed mediation the mediator was told of is told", owed: true, made: true, told: true,
			want: &OwedMediation{Name: "deploy-mediator", Repository: theRepository, Mediator: "ws-mediator", Told: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			if tt.owed {
				owe(t, h)
			}
			if tt.made {
				h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			}
			if tt.told {
				if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-told"); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := h.o.owedMediation(context.Background(), mediationKey(h))

			// Assert
			if err != nil || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("owedMediation = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestAMediationNoMediatorDecidesIsReleased(t *testing.T) {
	tests := []struct {
		name         string
		dropMediator func(h *harness)
		wantReleased bool
	}{
		{name: "a config that names no mediator now releases it", wantReleased: true,
			dropMediator: func(h *harness) { h.config.DeployMediator = nil }},
		{name: "a repository with no lifecycle config releases it", wantReleased: true,
			dropMediator: func(h *harness) { h.configErr = &lifecycleconfig.MissingError{Path: "/r/.agent-repl/lifecycle.yaml"} }},
		{name: "a config that cannot be read leaves it owed",
			dropMediator: func(h *harness) { h.configErr = errors.New("yaml: broken") }},
		{name: "a config still naming its mediator leaves it to the mediator",
			dropMediator: func(h *harness) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a mediation owed, the mediator made and told.
			h := mediated(t)
			owe(t, h)
			h.db.workspaces["ws-mediator"] = wsm.Workspace{ID: "ws-mediator", Name: "deploy-mediator", Repo: theRepository}
			if _, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted, "turn-told"); err != nil {
				t.Fatal(err)
			}
			h.db.turns["ws-mediator"] = []wsm.Turn{{ID: "turn-told", Workspace: "ws-mediator"}}
			tt.dropMediator(h)

			mediated, err := h.o.mediatedQueues(context.Background(), opMediation)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			h.o.releaseUnmediated(context.Background(), mediated)

			// Assert
			m, _ := h.db.MediationOf(context.Background(), mediationKey(h))
			if (m == nil) != tt.wantReleased {
				t.Fatalf("still owed %+v, want released %v", m, tt.wantReleased)
			}
			_, recorded := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
				Message: "released a mediation owed by a repository whose lifecycle config names no mediator now; its queue advances"})
			if recorded != tt.wantReleased {
				t.Fatalf("release recorded %v, want %v", recorded, tt.wantReleased)
			}
		})
	}
}

// TestEveryMediationEndIsTimedOnceByHowItEnded covers the mediation spans:
// the rollout span when the mediator is told, then one assessment span (when
// it was told) and one total, by how the mediation ended.
func TestEveryMediationEndIsTimedOnceByHowItEnded(t *testing.T) {
	brief := conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION
	told := "mediation:" + MediationSpanRollout + ":" + MediationTold
	ended := func(outcome string, assessed bool) []string {
		out := []string{"mediation:" + MediationSpanTotal + ":" + outcome}
		if assessed {
			out = append([]string{"mediation:" + MediationSpanAssessment + ":" + outcome}, out...)
		}
		return out
	}
	tests := []struct {
		name string
		// inTurn runs as the mediator is given its brief.
		inTurn func(t *testing.T, h *harness, ws ids.WorkspaceID)
		// act ends the mediation another way than a round, when set.
		act  func(t *testing.T, h *harness)
		want []string
	}{
		{name: "an approval", want: append([]string{told}, ended(MediationApproved, true)...),
			inTurn: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.o.DecideMediation(context.Background(), ws, MediationApprove, "clean"); err != nil {
					t.Errorf("DecideMediation: %v", err)
				}
			}},
		{name: "an escalation", want: append([]string{told}, ended(MediationEscalated, true)...),
			inTurn: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.o.DecideMediation(context.Background(), ws, MediationEscalate, "over to you"); err != nil {
					t.Errorf("DecideMediation: %v", err)
				}
			}},
		{name: "a remediation the mediator deploys", want: append([]string{told}, ended(MediationRedeployed, true)...),
			inTurn: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.o.OweMediations(context.Background(), deployStarted.Add(time.Minute)); err != nil {
					t.Errorf("OweMediations: %v", err)
				}
			}},
		{name: "a turn that decides nothing", want: append([]string{told}, ended(MediationUndecided, true)...)},
		{name: "a newer deploy before the mediator is told", want: ended(MediationSuperseded, false),
			act: func(t *testing.T, h *harness) {
				if err := h.o.OweMediations(context.Background(), deployStarted.Add(time.Minute)); err != nil {
					t.Fatalf("OweMediations: %v", err)
				}
			}},
		{name: "a repository that names no mediator any more", want: ended(MediationUnmediated, false),
			act: func(t *testing.T, h *harness) {
				h.config.DeployMediator = nil
				h.o.releaseUnmediated(context.Background(), nil)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediated(t)
			owe(t, h)
			h.queue.onSubmit = func(sub promptqueue.Submission) {
				if sub.Origin == brief && tt.inTurn != nil {
					tt.inTurn(t, h, sub.WS)
				}
			}

			// Act
			if tt.act != nil {
				tt.act(t, h)
			} else {
				h.o.Mediate(context.Background())
			}

			// Assert
			var got []string
			for _, timed := range h.timings.all() {
				if strings.HasPrefix(timed, "mediation:") {
					got = append(got, timed)
				}
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("timed %v, want %v", got, tt.want)
			}
		})
	}
}
