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
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// deployStarted is the instant the harness's deploys begin.
var deployStarted = time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)

// theBound is the harness repository's bound on remediations in a row.
const theBound = 4

// checked is a harness whose repository checks its deploys, bounded at
// theBound remediations in a row, every deploy check brief registered.
func checked(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.config.DeployChecks = &repoconfigv1.DeployChecks{RemediationsBeforeEscalation: theBound}
	h.briefs[BriefDeployIntake] = []string{"deploy_started", "ending", "window_started", "repo_dir", "version", "chain", "bound", "facts", "reuse"}
	h.briefs[BriefDeployEscalation] = []string{"repo_dir", "deploy_started", "summary", "problems", "old_problems"}
	h.briefs[BriefDeployEscalationUpdate] = []string{"repo_dir", "deploy_started", "summary", "problems", "recurred", "persisting"}
	h.briefs[BriefDeployIntakeResumed] = []string{"deploy_started", "version"}
	h.briefs[BriefDeployRemediation] = []string{"repo_dir", "deploy_started", "problems"}
	h.briefs[BriefDeployRemediationRestored] = []string{"repo_dir", "deploy_started", "problems"}
	h.briefs[BriefRemediationGateFailed] = []string{"archive_path", "failing_tests", "attempt", "max_attempts"}
	h.briefs[BriefRemediationBehind] = []string{"main_branch", "why", "attempt", "max_attempts"}
	// A CHECK IS TOLD AFTER ITS DEPLOY BEGAN, so its window runs forward from
	// the deploy's start to the clock (wsm.RecordMediationWindow).
	h.now = deployStarted.Add(time.Minute)
	return h
}

// mediationKey is the queue key the harness's registered repository resolves
// to.
func mediationKey(h *harness) wsm.RepoKey { return scopedKey(h) }

// fixDeploysAt makes the deploy at AT the fix's: the chain's last
// remediation's finish lands just before it, so its deploy replaces the
// dispatched check (wsm.OweMediation ends a dispatch only at its fix's deploy).
func fixDeploysAt(t *testing.T, h *harness, at time.Time) {
	t.Helper()
	ctx := context.Background()
	m := owedCheck(t, h)
	if m == nil || m.Remediation == "" {
		t.Fatalf("check %+v, want a remediation whose fix deploys", m)
	}
	landed := at.Add(-time.Second)
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: m.Remediation, Repo: mediationKey(h), StartedAt: landed}); err != nil {
		t.Fatalf("BeginRemediationFinish: %v", err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, m.Remediation, wsm.RemediationGating, wsm.RemediationLanding, landed); err != nil {
		t.Fatalf("AdvanceRemediationFinish: %v", err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, m.Remediation, "abc123", nil, landed); err != nil {
		t.Fatalf("RecordRemediationLanded: %v", err)
	}
	oweAt(t, h, at)
}

// owe makes the harness's repository owe a check of the deploy at AT.
func oweAt(t *testing.T, h *harness, at time.Time) {
	t.Helper()
	if err := h.o.OweMediations(context.Background(), at); err != nil {
		t.Fatalf("OweMediations: %v", err)
	}
}

// owe makes the harness's repository owe a check of the deploy at
// deployStarted.
func owe(t *testing.T, h *harness) { t.Helper(); oweAt(t, h, deployStarted) }

// owedCheck reads the check the harness's repository owes.
func owedCheck(t *testing.T, h *harness) *wsm.Mediation {
	t.Helper()
	m, err := h.db.MediationOf(context.Background(), mediationKey(h))
	if err != nil {
		t.Fatalf("MediationOf: %v", err)
	}
	return m
}

// withIntake records "ws-intake" as the intake of the check owed at OWED,
// registered as a workspace of the harness's repository.
func withIntake(t *testing.T, h *harness, owed time.Time) {
	t.Helper()
	h.db.mu.Lock()
	h.db.workspaces["ws-intake"] = wsm.Workspace{ID: "ws-intake", Name: "deploy-intake-x", Repo: theRepository}
	h.db.mu.Unlock()
	if ok, err := h.db.RecordMediationIntake(context.Background(), mediationKey(h), owed, "ws-intake"); err != nil || !ok {
		t.Fatalf("RecordMediationIntake = (%v, %v)", ok, err)
	}
}

// told marks the check owed at OWED told to its intake in TURN.
func told(t *testing.T, h *harness, owed time.Time, turn ids.TurnID) {
	t.Helper()
	if ok, err := h.db.MarkMediationPrompted(context.Background(), mediationKey(h), owed, owed, turn); err != nil || !ok {
		t.Fatalf("MarkMediationPrompted = (%v, %v)", ok, err)
	}
}

// decide has the intake decide D as its brief is delivered.
func decideInTurn(t *testing.T, h *harness, d Decision) {
	t.Helper()
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION && sub.WS == "ws-intake" {
			if err := h.o.DecideMediation(context.Background(), sub.WS, d); err != nil {
				t.Errorf("DecideMediation: %v", err)
			}
		}
	}
}

// submissionsTo is every prompt the harness submitted to WS whose text
// opens with PREFIX, the brief's name, in order; an empty PREFIX takes
// every one.
func submissionsTo(h *harness, ws ids.WorkspaceID, prefix string) []promptqueue.Submission {
	var out []promptqueue.Submission
	for _, sub := range h.queue.submissions {
		if sub.WS == ws && strings.HasPrefix(submittedText(sub), prefix) {
			out = append(out, sub)
		}
	}
	return out
}

// intakeSubmissions is every prompt the harness submitted to the intake.
func intakeSubmissions(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-intake", "")
}

// intakeTellings is every brief telling the intake of a deploy, the
// resuming briefs of its undecided turns left out.
func intakeTellings(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-intake", BriefDeployIntake+" ")
}

// checkWorkspacesMade is every intake and remediation workspace made, the
// escalations an undecided round ends in left out.
func checkWorkspacesMade(h *harness) []string {
	var out []string
	for _, name := range h.checks.made {
		if name != "deploy-fixer" {
			out = append(out, name)
		}
	}
	return out
}

// aProblem is a dispatch's one problem.
var aProblem = []DeployProblem{{Statement: "the reaper errors on every pass", Evidence: []string{`{"level":"error","operation":"daemon.worktreereap.pass","message":"boom"}`}}}

func TestADeployOwesACheckOnlyWhereItsDeploysAreChecked(t *testing.T) {
	tests := []struct {
		name    string
		checked bool
	}{
		{name: "a repository that checks its deploys", checked: true},
		{name: "a repository that checks none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.checked {
				h.config.DeployChecks = &repoconfigv1.DeployChecks{RemediationsBeforeEscalation: theBound}
			}

			// Act
			owe(t, h)

			// Assert
			if m := owedCheck(t, h); (m != nil) != tt.checked {
				t.Fatalf("owed %+v, want owed %v", m, tt.checked)
			}
		})
	}
}

func TestARoundMakesTheChainsIntakeAndTellsItUncompacted(t *testing.T) {
	// Arrange: the chain's first deploy.
	h := checked(t)
	owe(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert: the round ended undecided, so an escalation was made after.
	if made := checkWorkspacesMade(h); len(made) != 1 || made[0] != "deploy-intake-standing" {
		t.Fatalf("made %v, want one intake for the chain", h.checks.made)
	}
	for _, sub := range intakeSubmissions(h) {
		if sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION {
			t.Fatalf("submitted %v to the intake, want no compaction", sub.Origin)
		}
	}
	if told := intakeTellings(h); len(told) != 1 {
		t.Fatalf("told the intake %d times, want one brief", len(told))
	}
}

func TestADeployThatIsNotTheFixsKeepsTheIntakeUntold(t *testing.T) {
	// Arrange: the intake dispatched the first deploy; another landing's
	// deploy replaces its check before the fix lands.
	h := checked(t)
	owe(t, h)
	decideInTurn(t, h, Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem})
	h.o.Mediate(context.Background())
	h.queue.onSubmit = nil
	told := len(h.queue.submissions)
	oweAt(t, h, deployStarted.Add(time.Minute))

	// Act
	h.o.Mediate(context.Background())

	// Assert: the fix is still dispatched, and the intake decides nothing
	// until its deploy.
	if m := owedCheck(t, h); m == nil || !m.Dispatched || m.ChainRedeploys != 0 {
		t.Fatalf("check %+v, want the dispatch carried and no remediation counted", m)
	}
	if len(h.queue.submissions) != told {
		t.Fatalf("submissions %v, want the intake left untold", h.queue.submissions[told:])
	}
}

func TestEachDeployOfTheChainIsToldToTheOneStandingIntake(t *testing.T) {
	// Arrange: the intake dispatches the first deploy; its fix deploys.
	h := checked(t)
	owe(t, h)
	decideInTurn(t, h, Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem})
	h.o.Mediate(context.Background())
	decideInTurn(t, h, Decision{Kind: MediationApprove, Summary: "clean"})
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))

	// Act
	h.o.Mediate(context.Background())

	// Assert: one intake made, told of each deploy, its session ended by each decision.
	made := h.checks.made
	if len(intakeTellings(h)) != 2 || !slices.Equal(made, []string{"deploy-intake-standing", "deploy-fixer"}) {
		t.Fatalf("intake told %d times, made %v; want the one standing intake told of each deploy", len(intakeTellings(h)), made)
	}
	if !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake", "ws-intake"}) || len(h.checks.closed) != 0 {
		t.Fatalf("stopped %v, closed %v; want each decision to end the intake's session and close nothing", h.checks.stopped, h.checks.closed)
	}
}

func TestAnIntakeThatCannotBeMadeLeavesTheQueueHeld(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.checks.createErr = errors.New("worktree gone")

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if m := owedCheck(t, h); m == nil || m.Intake != "" {
		t.Fatalf("check %+v, want it still owed with no intake", m)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the chain's intake could not be found or made; the queue stays held for it"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

func TestTheStandingIntakeOfACheckThatEndedWaitsForTheNext(t *testing.T) {
	// Arrange: the check is released while its intake is found.
	h := checked(t)
	owe(t, h)
	m := owedCheck(t, h)
	if _, err := h.db.ReleaseMediation(context.Background(), mediationKey(h), deployStarted); err != nil {
		t.Fatal(err)
	}

	// Act
	ws, err := h.o.intakeOf(context.Background(), checkedQueue{registeredQueue: registeredQueue{key: mediationKey(h)}}, *m, dlog.Context{})

	// Assert
	standing, found, readErr := h.db.StandingDeployCheckOf(context.Background(), mediationKey(h), wsm.StandingIntake)
	if err != nil || ws != "" || len(h.checks.closed) != 0 || readErr != nil || !found || standing.Workspace != "ws-intake" {
		t.Fatalf("intakeOf = (%q, %v), closed %v, standing (%+v, %v); want none answered and the standing intake kept open", ws, err, h.checks.closed, standing, found)
	}
}

func TestATurnThatDecidesNothingTwiceEscalatesAndHoldsTheQueue(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if heldEscalation(t, h) != "ws-fixer" {
		t.Fatalf("check %+v, want the undecided check held by an escalation", owedCheck(t, h))
	}
	if !h.bannerRaised(desktopnotify.KindDeployMediated) {
		t.Fatal("the user was not told the intake decided nothing")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the intake's turn ended deciding nothing again after it was resumed to decide; the intake failed, and the check is escalated to the user"}); !ok {
		t.Fatalf("records = %v, want the undecided check at ERROR", h.logs.Records())
	}
	if !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) {
		t.Fatalf("stopped %v, want the chain's intake stopped", h.checks.stopped)
	}
}

func TestATurnUndecidedTwiceIsEscalatedBeforeItsIntakeIsStopped(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	decided := decidedAtStop(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) || !*decided {
		t.Fatalf("stopped %v, escalated at the stop %v; want the check escalated before its intake is stopped", h.checks.stopped, *decided)
	}
}

func TestATurnThatEscalatedKeepsTheQueueHeldAndResumesNothing(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	decideInTurn(t, h, Decision{Kind: MediationEscalate, Summary: "yours"})

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if heldEscalation(t, h) != "ws-fixer" || len(resumingBriefs(h)) != 0 {
		t.Fatalf("check %+v, resumed %d times; want it held by its escalation and the intake never resumed", owedCheck(t, h), len(resumingBriefs(h)))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "the intake's turn ended with the deploy escalated; the queue stays held for the user"}); !ok {
		t.Fatalf("records = %v, want the escalated turn's end recorded", h.logs.Records())
	}
}

func TestATurnThatDispatchedKeepsTheQueueHeld(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	decideInTurn(t, h, Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem})

	// Act
	h.o.Mediate(context.Background())

	// Assert
	m := owedCheck(t, h)
	if m == nil || !m.Dispatched || m.Remediation != "ws-fixer" {
		t.Fatalf("check %+v, want it dispatched to the remediation and still owed", m)
	}
	if h.bannerRaised(desktopnotify.KindDeployMediated) || len(h.checks.closed) != 0 {
		t.Fatalf("banner %v, closed %v; want neither while the fix is at work", h.bannerRaised(desktopnotify.KindDeployMediated), h.checks.closed)
	}
}

func TestADispatchedCheckHasNoRound(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	told(t, h, deployStarted, "turn-told")
	if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer"); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 || len(h.awaitedTurns) != 0 {
		t.Fatalf("submitted %d, awaited %v; want nothing while the remediation works", len(h.queue.submissions), h.awaitedTurns)
	}
}

func TestARoundWhoseTurnCannotBeAwaitedKeepsTheQueueHeld(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{name: "an intake told in this round"},
		{name: "an intake told before, its turn reattached", arrange: func(t *testing.T, h *harness) {
			withIntake(t, h, deployStarted)
			told(t, h, deployStarted, "turn-told")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}
			h.awaitErr = errors.New("the session watcher is gone")

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
				Message: "the intake's turn could not be awaited; the queue stays held until the next round"}); !ok {
				t.Fatalf("records = %v, want the failed await at ERROR", h.logs.Records())
			}
			if m := owedCheck(t, h); m == nil {
				t.Fatal("the failed await released the check and let the queue through")
			}
		})
	}
}

func TestACheckAlreadyToldIsReattachedToItsTurn(t *testing.T) {
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
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			told(t, h, deployStarted, "turn-told")
			if tt.delivered {
				h.db.turns["ws-intake"] = append(h.db.turns["ws-intake"], wsm.Turn{ID: "turn-told", Workspace: "ws-intake"})
			}

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if len(intakeTellings(h)) != tt.wantSubmitted || len(checkWorkspacesMade(h)) != 0 || !slices.Contains(h.awaitedTurns, "turn-told") {
				t.Fatalf("submitted %d, made %v, awaited %v; want %d submitted and turn-told awaited", len(intakeTellings(h)), h.checks.made, h.awaitedTurns, tt.wantSubmitted)
			}
		})
	}
}

func TestADaemonThatDoesNotServeTheQueuesRunsNoRound(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.standing.serves = false

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 || len(h.checks.made) != 0 {
		t.Fatalf("submitted %d, made %v; want nothing", len(h.queue.submissions), h.checks.made)
	}
}

func TestDecideMediation(t *testing.T) {
	tests := []struct {
		name       string
		from       ids.WorkspaceID
		dispatched bool
		decision   MediationDecision
		wantArm    string
		wantBanner bool
		wantHeld   bool
		escalated  bool
	}{
		{name: "an approval releases the deploy and ends the chain", from: "ws-intake", decision: MediationApprove},
		{name: "an escalation holds it and tells the user", from: "ws-intake", decision: MediationEscalate, wantBanner: true, wantHeld: true},
		{name: "a decision from any other workspace is refused", from: theWorkspace, decision: MediationApprove, wantArm: ArmNotMediator},
		{name: "a decision on a deploy a fix was dispatched for is refused", from: "ws-intake", dispatched: true, decision: MediationApprove, wantArm: ArmNoMediationOwed},
		{name: "a decision on a deploy escalated already is refused", from: "ws-intake", escalated: true, decision: MediationApprove, wantArm: ArmNoMediationOwed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			if tt.dispatched {
				if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer"); err != nil {
					t.Fatal(err)
				}
			}
			if tt.escalated {
				if _, err := h.db.EscalateMediation(context.Background(), mediationKey(h), deployStarted, `{"summary":"s"}`); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := h.o.DecideMediation(context.Background(), tt.from, Decision{Kind: tt.decision, Summary: "the summary"})

			// Assert
			var refused *RefusalError
			if tt.wantArm != "" {
				if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
					t.Fatalf("DecideMediation = %v, want the %s refusal", err, tt.wantArm)
				}
				if owedCheck(t, h) == nil {
					t.Fatal("a refused decision released the check")
				}
				return
			}
			if err != nil || (owedCheck(t, h) != nil) != tt.wantHeld || h.bannerRaised(desktopnotify.KindDeployMediated) != tt.wantBanner {
				t.Fatalf("DecideMediation = %v, owed %+v, banner %v; want held %v, banner %v", err, owedCheck(t, h), h.bannerRaised(desktopnotify.KindDeployMediated), tt.wantHeld, tt.wantBanner)
			}
			if tt.wantHeld && heldEscalation(t, h) != "ws-fixer" {
				t.Fatalf("check %+v, want it held by the escalation", owedCheck(t, h))
			}
			if !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) {
				t.Fatalf("stopped %v, want the chain's intake stopped", h.checks.stopped)
			}
		})
	}
}

func TestADecisionFromARepositoryThatChecksNoMoreIsRefused(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.config.DeployChecks = nil

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"})

	// Assert
	var refused *RefusalError
	if !errors.As(err, &refused) || refused.Arm != ArmNotMediator {
		t.Fatalf("DecideMediation = %v, want the not_mediator refusal", err)
	}
}

func TestTheRosterDrawsWhereTheCheckStands(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    *sidebar.QueueMediation
	}{
		{name: "a queue owing nothing draws nothing"},
		{name: "a chain's first deploy rolling out has no intake to select", arrange: func(t *testing.T, h *harness) { owe(t, h) },
			want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationWaiting}},
		{name: "a later deploy rolling out selects the chain's intake", arrange: func(t *testing.T, h *harness) {
			owe(t, h)
			withIntake(t, h, deployStarted)
		}, want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationWaiting, Intake: "ws-intake"}},
		{name: "an assessed deploy selects the intake", arrange: func(t *testing.T, h *harness) {
			owe(t, h)
			withIntake(t, h, deployStarted)
			told(t, h, deployStarted, "turn-1")
		}, want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationChecking, Intake: "ws-intake"}},
		{name: "a dispatched deploy selects the remediation", arrange: func(t *testing.T, h *harness) {
			owe(t, h)
			withIntake(t, h, deployStarted)
			told(t, h, deployStarted, "turn-1")
			if _, err := h.db.DispatchMediation(context.Background(), mediationKey(h), deployStarted, deployStarted, "ws-fixer"); err != nil {
				t.Fatal(err)
			}
		}, want: &sidebar.QueueMediation{Repo: theRepository, Stage: sidebar.MediationFixing, Intake: "ws-intake", Remediation: "ws-fixer"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}

			// Act
			got, err := h.o.rosterMediation(context.Background(), mediationKey(h))

			// Assert
			if err != nil || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("rosterMediation = (%+v, %v), want %+v", got, err, tt.want)
			}
		})
	}
}

func TestOwedFromRefusesAToldCheckWithNoWorkspaceAtWork(t *testing.T) {
	at := deployStarted
	tests := []struct {
		name    string
		m       wsm.Mediation
		want    *OwedMediation
		wantErr bool
	}{
		{name: "rolling out counts the chain's remediations", m: wsm.Mediation{OwedAt: at, ChainRedeploys: 2, Intake: "i"},
			want: &OwedMediation{Repository: theRepository, Stage: CheckRollingOut, Intake: "i", InARow: 2}},
		{name: "a told check with no intake is a broken store", m: wsm.Mediation{OwedAt: at, PromptedAt: &at}, wantErr: true},
		{name: "a dispatch carried into a newer deploy's check is fixing with no intake",
			m:    wsm.Mediation{OwedAt: at, ChainRedeploys: 2, Remediation: "r", Dispatched: true},
			want: &OwedMediation{Repository: theRepository, Stage: CheckFixing, Remediation: "r", InARow: 2}},
		{name: "a dispatched check with no remediation is a broken store", m: wsm.Mediation{OwedAt: at, PromptedAt: &at, Intake: "i", Dispatched: true}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := owedFrom(theRepository, tt.m)

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("owedFrom = (%+v, %v), want %+v (error %v)", got, err, tt.want, tt.wantErr)
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
		{name: "a check's read the daemon's stop cut is withdrawn", cancelled: true, wantLevel: "info",
			arrange: func(h *harness) { h.db.mediationErr = context.Canceled }},
		{name: "a registry read that fails while the daemon runs is an error", wantLevel: "error",
			arrange: func(h *harness) { h.db.reposErr = errors.New("disk gone") }},
		{name: "a check's read that fails while the daemon runs is an error", wantLevel: "error",
			arrange: func(h *harness) { h.db.mediationErr = errors.New("disk gone") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
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

func TestACheckNoIntakeDecidesIsReleased(t *testing.T) {
	tests := []struct {
		name         string
		dropChecks   func(h *harness)
		wantReleased bool
	}{
		{name: "a config that checks no deploys now releases it", wantReleased: true,
			dropChecks: func(h *harness) { h.config.DeployChecks = nil }},
		{name: "a repository with no lifecycle config releases it", wantReleased: true,
			dropChecks: func(h *harness) { h.configErr = &lifecycleconfig.MissingError{Path: "/r/.agent-repl/lifecycle.yaml"} }},
		{name: "a config that cannot be read leaves it owed",
			dropChecks: func(h *harness) { h.configErr = errors.New("yaml: broken") }},
		{name: "a config still checking its deploys leaves it to the intake",
			dropChecks: func(h *harness) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			tt.dropChecks(h)
			checkedNow, err := h.o.checkedQueues(context.Background(), opMediation)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			h.o.releaseUnchecked(context.Background(), checkedNow)

			// Assert
			if m := owedCheck(t, h); (m == nil) != tt.wantReleased {
				t.Fatalf("still owed %+v, want released %v", m, tt.wantReleased)
			}
			if stopped := slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}); stopped != tt.wantReleased {
				t.Fatalf("stopped %v, want the intake stopped %v", h.checks.stopped, tt.wantReleased)
			}
		})
	}
}

func TestAChainThatEndsForgetsWhatItsRemediationsLanded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"})

	// Assert
	finishes, readErr := h.db.RemediationFinishes(context.Background())
	if err != nil || readErr != nil || len(finishes) != 0 {
		t.Fatalf("DecideMediation = %v, finishes (%+v, %v); want none left", err, finishes, readErr)
	}
}

func TestAnIntakeThatCannotBeStoppedAtTheChainsEndIsRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	h.checks.stopErr = errors.New("still busy")

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"})

	// Assert
	if err != nil || owedCheck(t, h) != nil {
		t.Fatalf("DecideMediation = %v, want the approval to release the queue regardless", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the chain ended but its intake could not be stopped"}); !ok {
		t.Fatalf("records = %v, want the failed stop at ERROR", h.logs.Records())
	}
}

func TestAnApprovalStopsItsIntakeAtOnce(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"})

	// Assert
	if err != nil || !slices.Equal(h.checks.stopped, []ids.WorkspaceID{"ws-intake"}) || len(h.checks.closed) != 0 {
		t.Fatalf("DecideMediation = %v, stopped %v, closed %v; want the intake stopped at once and never closed at its turn's end", err, h.checks.stopped, h.checks.closed)
	}
}

// TestEveryCheckEndIsTimedOnceByHowItEnded covers the check spans: the
// rollout span when the intake is told, then one assessment span (when it
// was told) and one total, by how the check ended.
func TestEveryCheckEndIsTimedOnceByHowItEnded(t *testing.T) {
	brief := conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION
	toldSpan := "mediation:" + MediationSpanRollout + ":" + MediationTold
	assessed := func(outcome string) string { return "mediation:" + MediationSpanAssessment + ":" + outcome }
	total := func(outcome string) string { return "mediation:" + MediationSpanTotal + ":" + outcome }
	decide := func(d Decision) func(t *testing.T, h *harness, ws ids.WorkspaceID) {
		return func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if err := h.o.DecideMediation(context.Background(), ws, d); err != nil {
				t.Errorf("DecideMediation: %v", err)
			}
		}
	}
	tests := []struct {
		name   string
		inTurn func(t *testing.T, h *harness, ws ids.WorkspaceID)
		after  func(t *testing.T, h *harness)
		act    func(t *testing.T, h *harness)
		want   []string
	}{
		{name: "an approval", inTurn: decide(Decision{Kind: MediationApprove, Summary: "clean"}),
			want: []string{toldSpan, assessed(MediationApproved), total(MediationApproved)}},
		{name: "an escalation holds the check, so it ends untimed", inTurn: decide(Decision{Kind: MediationEscalate, Summary: "yours"}),
			want: []string{toldSpan}},
		{name: "an escalation the user closes", inTurn: decide(Decision{Kind: MediationEscalate, Summary: "yours"}),
			after: func(t *testing.T, h *harness) { h.o.OnWorkspaceClosed(context.Background(), "ws-fixer") },
			want:  []string{toldSpan, assessed(MediationEscalated), total(MediationEscalated)}},
		{name: "a dispatch whose fix deploys", inTurn: decide(Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem}),
			after: func(t *testing.T, h *harness) { fixDeploysAt(t, h, deployStarted.Add(time.Minute)) },
			want:  []string{toldSpan, assessed(MediationDispatched), total(MediationDispatched)}},
		{name: "a dispatch a deploy not its fix's replaces", inTurn: decide(Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem}),
			after: func(t *testing.T, h *harness) { oweAt(t, h, deployStarted.Add(time.Minute)) },
			want:  []string{toldSpan, assessed(MediationDispatched), total(MediationSuperseded)}},
		{name: "a turn that decides nothing escalates, holding the check untimed", want: []string{toldSpan}},
		{name: "a newer deploy before the intake is told", want: []string{total(MediationSuperseded)},
			act: func(t *testing.T, h *harness) { oweAt(t, h, deployStarted.Add(time.Minute)) }},
		{name: "a repository that checks its deploys no more", want: []string{total(MediationUnmediated)},
			act: func(t *testing.T, h *harness) {
				h.config.DeployChecks = nil
				h.o.releaseUnchecked(context.Background(), nil)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			h.queue.onSubmit = func(sub promptqueue.Submission) {
				if sub.Origin == brief && sub.WS == "ws-intake" && tt.inTurn != nil {
					tt.inTurn(t, h, sub.WS)
				}
			}

			// Act
			if tt.act != nil {
				tt.act(t, h)
			} else {
				h.o.Mediate(context.Background())
			}
			if tt.after != nil {
				tt.after(t, h)
			}

			// Assert
			var got []string
			for _, timed := range h.meter.all() {
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

// TestAChainOfRemediationsIsRecordedWhenTheQueueAdvances covers the chain:
// each remediation deployed in a row is counted, the count stated while the
// chain is owed, and the chain and its last remediation recorded, by how
// they ended, once the queue advances past it.
func TestAChainOfRemediationsIsRecordedWhenTheQueueAdvances(t *testing.T) {
	// Arrange: the intake dispatches the first deploy, then approves its
	// fix's deploy.
	h := checked(t)
	owe(t, h)
	brief := conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION
	rounds := 0
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.Origin != brief || sub.WS != "ws-intake" {
			return
		}
		rounds++
		d := Decision{Kind: MediationApprove, Summary: "clean"}
		if rounds == 1 {
			d = Decision{Kind: MediationDispatch, Summary: "broken", Problems: aProblem}
		}
		if err := h.o.DecideMediation(context.Background(), sub.WS, d); err != nil {
			t.Errorf("DecideMediation: %v", err)
		}
	}

	// Act: the first deploy's round, its fix's deploy, then its round.
	h.o.Mediate(context.Background())
	fixDeploysAt(t, h, deployStarted.Add(time.Minute))
	h.o.Mediate(context.Background())

	// Assert
	var got []string
	for _, m := range h.meter.all() {
		if strings.HasPrefix(m, "chain:") || strings.HasPrefix(m, "in_a_row:") || strings.HasPrefix(m, "remediation:") {
			got = append(got, m)
		}
	}
	want := []string{"in_a_row:0", "in_a_row:1", "chain:" + MediationApproved + ":1", "in_a_row:0", "remediation:" + RemediationLanded}
	if !slices.Equal(got, want) {
		t.Fatalf("recorded %v, want %v", got, want)
	}
	// The chain is held from the FIRST deploy: its duration reaches back to
	// deployStarted, not to its fix's deploy a minute later.
	h.mu.Lock()
	now := h.now
	h.mu.Unlock()
	took := h.meter.chainTook
	if len(took) != 1 || deployStarted.Add(took[0]).After(now) || now.Sub(deployStarted.Add(took[0])) > time.Second {
		t.Fatalf("chain took %v, want it measured from the first deploy at %v (now %v)", took, deployStarted, now)
	}
}

func TestARoundForAReplacedCheckNeverBlocksTheNewerChecksRound(t *testing.T) {
	// Arrange: the first check's round still holds its claim, awaiting its
	// turn, when a newer deploy replaces the check.
	h := checked(t)
	owe(t, h)
	if !h.o.claimMediation(mediationKey(h), deployStarted) {
		t.Fatal("the first check's claim was refused")
	}
	newer := deployStarted.Add(time.Minute)
	oweAt(t, h, newer)

	// Act
	h.o.Mediate(context.Background())

	// Assert: the newer check's round ran, and told the intake.
	if subs := intakeTellings(h); len(subs) != 1 {
		t.Fatalf("submitted %+v, want the newer check told to the intake", h.queue.submissions)
	}
}

func TestASecondPassForACheckWhoseRoundRunsIsRecorded(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	if !h.o.claimMediation(mediationKey(h), deployStarted) {
		t.Fatal("the claim was refused")
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %+v, want the running round left alone", h.queue.submissions)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation,
		Message: "a round for this check runs already; this pass leaves it to that round"}); !ok {
		t.Fatalf("records = %v, want the skip recorded", h.logs.Records())
	}
}

func TestTheIntakesRoleIsRecordedBeforeItsEffortStartsItsSession(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.checks.efforts) == 0 || h.checks.efforts[0] != "intake:ws-intake:recorded=true" {
		t.Fatalf("efforts %v, want the intake's set once its role was recorded", h.checks.efforts)
	}
}

func TestAnIntakeWhoseEffortCannotBeSetIsNotTold(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.checks.effortErr = errors.New("no catalog")

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if len(h.queue.submissions) != 0 || owedCheck(t, h) == nil {
		t.Fatalf("submitted %d, want no brief and the check still owed", len(h.queue.submissions))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the chain's intake could not be found or made; the queue stays held for it"}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", h.logs.Records())
	}
}

// resumingBriefs is every resuming brief the harness submitted to the intake.
func resumingBriefs(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-intake", BriefDeployIntakeResumed)
}

func TestAnUndecidedIntakeIsResumedOnceBeforeTheQueueIsLetThrough(t *testing.T) {
	tests := []struct {
		name         string
		decide       bool
		wantReleased bool
		wantEscalate bool
		wantFault    bool
	}{
		{name: "the resumed turn decides", decide: true, wantReleased: true},
		{name: "the resumed turn ends undecided again", wantEscalate: true, wantFault: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if tt.decide {
				h.queue.onSubmit = func(sub promptqueue.Submission) {
					if sub.WS == "ws-intake" && strings.HasPrefix(submittedText(sub), BriefDeployIntakeResumed) {
						if err := h.o.DecideMediation(context.Background(), sub.WS, Decision{Kind: MediationApprove, Summary: "nothing new"}); err != nil {
							t.Errorf("DecideMediation: %v", err)
						}
					}
				}
			}

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if subs := resumingBriefs(h); len(subs) != 1 {
				t.Fatalf("resumed the intake %d times, want once", len(subs))
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "warn", Operation: opMediation,
				Message: "the intake's turn ended deciding nothing about the deploy; it is resumed once to decide it, the queue still held"}); !ok {
				t.Fatalf("records = %v, want the resume at WARN", h.logs.Records())
			}
			if released := owedCheck(t, h) == nil; released != tt.wantReleased {
				t.Fatalf("released %v, want %v", released, tt.wantReleased)
			}
			escalated := slices.ContainsFunc(h.checks.made, func(n string) bool { return n == "deploy-fixer" })
			if escalated != tt.wantEscalate {
				t.Fatalf("made %v, want an escalation %v", h.checks.made, tt.wantEscalate)
			}
			_, fault := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
				Message: "the intake's turn ended deciding nothing again after it was resumed to decide; the intake failed, and the check is escalated to the user"})
			if fault != tt.wantFault {
				t.Fatalf("records = %v, want the intake's fault at ERROR %v", h.logs.Records(), tt.wantFault)
			}
		})
	}
}

func TestAnUndecidedIntakeThatCannotBeResumedHoldsTheQueue(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "a resume that cannot be recorded", message: "could not record that the undecided intake is resumed; the queue stays held",
			arrange: func(h *harness) {
				h.db.mu.Lock()
				h.db.resumeMarkErr = errors.New("database is locked")
				h.db.mu.Unlock()
			}},
		{name: "a resuming brief that cannot be composed", message: "the undecided intake's resuming brief could not be composed; the queue stays held",
			arrange: func(h *harness) { delete(h.briefs, BriefDeployIntakeResumed) }},
		{name: "a resuming brief that cannot be submitted", message: "the undecided intake's resuming brief could not be submitted; the queue stays held",
			arrange: func(h *harness) {
				h.queue.onSubmit = func(sub promptqueue.Submission) {
					if !strings.HasPrefix(submittedText(sub), BriefDeployIntakeResumed) {
						return
					}
					h.queue.mu.Lock()
					h.queue.err = errors.New("queue closed")
					h.queue.mu.Unlock()
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			tt.arrange(h)

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if owedCheck(t, h) == nil {
				t.Fatal("the check was released, want the queue held")
			}
			if slices.ContainsFunc(h.checks.made, func(n string) bool { return n == "deploy-fixer" }) {
				t.Fatalf("made %v, want no escalation", h.checks.made)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestAResumedIntakesTurnIsReattachedWithItsResumingBrief(t *testing.T) {
	// Arrange: the intake was told, then resumed, and the daemon restarted
	// before the resuming turn reached its session.
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	told(t, h, deployStarted, "turn-told")
	if ok, err := h.db.MarkMediationResumed(context.Background(), mediationKey(h), deployStarted, "turn-resumed"); err != nil || !ok {
		t.Fatalf("MarkMediationResumed = (%v, %v)", ok, err)
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	subs := intakeSubmissions(h)
	if len(subs) == 0 || subs[0].Turn != "turn-resumed" || !strings.HasPrefix(submittedText(subs[0]), BriefDeployIntakeResumed) {
		t.Fatalf("submitted %+v, want the resuming brief resubmitted in its recorded turn", subs)
	}
	if len(resumingBriefs(h)) != 1 {
		t.Fatalf("resumed the intake %d times, want the recorded resume only", len(resumingBriefs(h)))
	}
}

func TestSubmissionsToTakesOnlyTheNamedWorkspacesPromptsOfTheNamedBrief(t *testing.T) {
	tests := []struct {
		name   string
		ws     ids.WorkspaceID
		prefix string
		want   []ids.TurnID
	}{
		{name: "every prompt to the workspace", ws: "ws-a", want: []ids.TurnID{"t1", "t3"}},
		{name: "only the named brief's", ws: "ws-a", prefix: "brief-b", want: []ids.TurnID{"t3"}},
		{name: "a workspace sent nothing", ws: "ws-c"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			for _, sub := range []promptqueue.Submission{
				{WS: "ws-a", Turn: "t1", Said: saidText("brief-a one")},
				{WS: "ws-b", Turn: "t2", Said: saidText("brief-b two")},
				{WS: "ws-a", Turn: "t3", Said: saidText("brief-b three")},
			} {
				if _, err := h.queue.Submit(context.Background(), sub); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got := submissionsTo(h, tt.ws, tt.prefix)

			// Assert
			var turns []ids.TurnID
			for _, sub := range got {
				turns = append(turns, sub.Turn)
			}
			if !slices.Equal(turns, tt.want) {
				t.Fatalf("submissionsTo = %v, want %v", turns, tt.want)
			}
		})
	}
}

// TestEverySubmissionFilterGoesThroughSubmissionsTo keeps the deploy-check
// tests' filters of the harness's prompts on the one helper: a hand-rolled
// loop over the submissions by workspace fails here.
func TestEverySubmissionFilterGoesThroughSubmissionsTo(t *testing.T) {
	for _, file := range []string{"mediation_test.go", "escalation_test.go"} {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		if n := strings.Count(string(src), "range h.queue.submissions {\n\t\tif sub.WS =="); n != 0 && !(file == "mediation_test.go" && n == 1) {
			t.Fatalf("%s filters the submissions by workspace by hand %d times; use submissionsTo", file, n)
		}
	}
}

func TestADeployThatChangedNothingIsReleasedUnchecked(t *testing.T) {
	later := deployStarted.Add(time.Minute)
	tests := []struct {
		name         string
		arrange      func(t *testing.T, h *harness)
		release      time.Time
		wantReleased bool
		wantMessage  string
	}{
		{name: "a check that began its own chain", release: deployStarted, wantReleased: true,
			wantMessage: "the deploy changed nothing; its check is released unchecked and the queue advances"},
		{name: "a check carrying an earlier check's chain", release: later,
			arrange: func(t *testing.T, h *harness) {
				withIntake(t, h, deployStarted)
				oweAt(t, h, later)
			},
			wantMessage: "the deploy changed nothing, but its check carries an earlier chain; the chain's intake checks it"},
		{name: "another deploy's check", release: later},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}

			// Act
			err := h.o.ReleaseUnchangedDeploy(context.Background(), tt.release)

			// Assert
			if err != nil {
				t.Fatalf("ReleaseUnchangedDeploy: %v", err)
			}
			if released := owedCheck(t, h) == nil; released != tt.wantReleased {
				t.Fatalf("released %v, want %v", released, tt.wantReleased)
			}
			if tt.wantMessage != "" {
				if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: tt.wantMessage}); !ok {
					t.Fatalf("records = %v, want %q", h.logs.Records(), tt.wantMessage)
				}
			}
			if len(h.queue.submissions) != 0 || len(h.checks.made) != 0 {
				t.Fatalf("submitted %v, made %v; want no intake told or made", h.queue.submissions, h.checks.made)
			}
		})
	}
}

func TestAnUnchangedDeploysCheckThatCannotBeReadStaysOwed(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.db.mu.Lock()
	h.db.mediationErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.ReleaseUnchangedDeploy(context.Background(), deployStarted)

	// Assert
	h.db.mu.Lock()
	h.db.mediationErr = nil
	h.db.mu.Unlock()
	if err == nil || owedCheck(t, h) == nil {
		t.Fatalf("ReleaseUnchangedDeploy = %v, want the failure answered and the check still owed", err)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "could not read the check an unchanged deploy owes; it stays owed"}); !ok {
		t.Fatalf("records = %v, want the failed read at ERROR", h.logs.Records())
	}
}

func TestReleaseCheckEndsTheChainOfOnlyACheckItReleased(t *testing.T) {
	tests := []struct {
		name         string
		owed         time.Time
		wantReleased bool
	}{
		{name: "the check owed", owed: deployStarted, wantReleased: true},
		{name: "a check a newer deploy replaced", owed: deployStarted.Add(-time.Minute)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			m := *owedCheck(t, h)
			m.OwedAt = tt.owed

			// Act
			released, err := h.o.releaseCheck(context.Background(), mediationKey(h), m, MediationApproved)

			// Assert
			if err != nil || released != tt.wantReleased {
				t.Fatalf("releaseCheck = (%v, %v), want %v", released, err, tt.wantReleased)
			}
			if stopped := slices.Contains(h.checks.stopped, "ws-intake"); stopped != tt.wantReleased {
				t.Fatalf("stopped %v, want the chain's intake stopped %v", h.checks.stopped, tt.wantReleased)
			}
			if stillOwed := owedCheck(t, h) != nil; stillOwed == tt.wantReleased {
				t.Fatalf("still owed %v, want %v", stillOwed, !tt.wantReleased)
			}
		})
	}
}

// TestEveryCheckReleaseGoesThroughReleaseCheck keeps one path from a
// released check to its chain's end: a site releasing a check by hand, which
// could leave its chain running, fails here.
func TestEveryCheckReleaseGoesThroughReleaseCheck(t *testing.T) {
	if sites := sourcescan.Sites(t, "DB.ReleaseMediation("); !slices.Equal(sites, []string{"mediation.go"}) {
		t.Fatalf("checks are released in %v, want only releaseCheck in mediation.go", sites)
	}
}

func TestAnAwaitCutByTheIntakesOwnDecisionIsNoFailure(t *testing.T) {
	tests := []struct {
		name      string
		decide    bool
		wantLevel string
		message   string
	}{
		{name: "a decision released the check, then stopped the intake", decide: true, wantLevel: "info",
			message: "the intake's turn ended with its session, which its own decision stopped; the check is decided"},
		{name: "the session ended with the check still owed", wantLevel: "error",
			message: "the intake's turn could not be awaited; the queue stays held until the next round"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the await fails as a stopped session's does, after
			// the intake decided or not.
			h := checked(t)
			owe(t, h)
			if tt.decide {
				decideInTurn(t, h, Decision{Kind: MediationApprove, Summary: "nothing new"})
			}
			h.mu.Lock()
			h.awaitErr = errors.New("workspace has no live session to await the turn on")
			h.mu.Unlock()

			// Act
			h.o.Mediate(context.Background())

			// Assert
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at %s", h.logs.Records(), tt.message, tt.wantLevel)
			}
			if released := owedCheck(t, h) == nil; released != tt.decide {
				t.Fatalf("released %v, want %v", released, tt.decide)
			}
		})
	}
}

func TestOwesIsTheCheckOfTheDeployThatBeganThen(t *testing.T) {
	tests := []struct {
		name string
		m    *wsm.Mediation
		want bool
	}{
		{name: "the deploy's check", m: &wsm.Mediation{OwedAt: deployStarted}, want: true},
		{name: "a newer deploy's check", m: &wsm.Mediation{OwedAt: deployStarted.Add(time.Minute)}},
		{name: "no check owed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := owes(tt.m, deployStarted)

			// Assert
			if got != tt.want {
				t.Fatalf("owes = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEveryStillOwedTestGoesThroughOwes keeps one reading of "this check is
// still the one owed": a hand-rolled comparison of a check's instant fails
// here.
func TestEveryStillOwedTestGoesThroughOwes(t *testing.T) {
	if sites := sourcescan.Sites(t, ".OwedAt.Equal("); !slices.Equal(sites, []string{"mediation.go"}) {
		t.Fatalf("an owed check's instant is compared in %v, want only owes in mediation.go", sites)
	}
}

func TestAnUnchangedDeploysReleaseCutByTheDaemonsExitIsNoFailure(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness) context.Context
		message string
	}{
		{name: "a release cut mid-read", message: "the mediation was cut by the daemon's exit; the next boot's pass runs it again",
			arrange: func(h *harness) context.Context {
				ctx, cancel := context.WithCancel(context.Background())
				cancel()
				h.db.mu.Lock()
				h.db.mediationErr = context.Canceled
				h.db.mu.Unlock()
				return ctx
			}},
		{name: "a release after the mediation stopped", message: mediationNotRun,
			arrange: func(h *harness) context.Context {
				if idle := h.o.stopMediation(); idle != nil {
					<-idle
				}
				return context.Background()
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			ctx := tt.arrange(h)

			// Act
			_ = h.o.ReleaseUnchangedDeploy(ctx, deployStarted)

			// Assert
			h.db.mu.Lock()
			h.db.mediationErr = nil
			h.db.mu.Unlock()
			if owedCheck(t, h) == nil {
				t.Fatal("the cut release let the check go")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at INFO", h.logs.Records(), tt.message)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error"}); ok {
				t.Fatalf("records = %v, want no error", h.logs.Records())
			}
		})
	}
}
