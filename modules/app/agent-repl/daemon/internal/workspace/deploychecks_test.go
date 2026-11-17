package workspace

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

func TestTheDeployCheckWorkspacesAreMadeOnTheirModelsWithNoPrompt(t *testing.T) {
	tests := []struct {
		name       string
		spec       CreateSpec
		wantModel  string
		wantHidden bool
	}{
		{name: "the intake is hidden on sonnet", spec: deployCheckSpec("/repo", "deploy-intake-x", intakeModel, true), wantModel: "sonnet", wantHidden: true},
		{name: "the remediation is visible on opus", spec: deployCheckSpec("/repo", "deploy-remediation-x", remediationModel, false), wantModel: "opus"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert
			if tt.spec.RepoDir != "/repo" || tt.spec.Model != tt.wantModel || tt.spec.Hidden != tt.wantHidden || tt.spec.InitialPrompt != "" {
				t.Fatalf("spec = %+v, want an unprompted workspace on %s, hidden %v", tt.spec, tt.wantModel, tt.wantHidden)
			}
		})
	}
}

func TestADeployCheckWorkspaceIsMadeUnpromptedWithNoSession(t *testing.T) {
	tests := []struct {
		name   string
		create func(v *verbs, repoDir string) error
	}{
		{name: "the intake", create: func(v *verbs, repoDir string) error {
			_, err := v.CreateDeployIntake(context.Background(), repoDir, "deploy-intake-20261114-120000")
			return err
		}},
		{name: "the remediation", create: func(v *verbs, repoDir string) error {
			_, err := v.CreateDeployRemediation(context.Background(), repoDir, "deploy-remediation-20261114-120000")
			return err
		}},
		{name: "the escalation", create: func(v *verbs, repoDir string) error {
			_, err := v.CreateDeployEscalation(context.Background(), repoDir, "deploy-escalation-20261114-120000")
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}

			// Act
			err := tt.create(f.verbs.(*verbs), mainWorktree(t, f))

			// Assert
			if err != nil {
				t.Fatalf("create: %v", err)
			}
			if len(f.fleet.efforts) != 0 {
				t.Fatalf("efforts = %+v, want none set at the create", f.fleet.efforts)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opDeployChecks, Message: "made the deploy check workspace"}); !ok {
				t.Fatalf("records = %+v, want the made workspace recorded", f.log.logger.Records())
			}
		})
	}
}

func TestOnlyAnEscalationIsMadeInRemoteControl(t *testing.T) {
	tests := []struct {
		name   string
		create func(v *verbs, repoDir string) (wsm.Workspace, error)
		wantOn bool
	}{
		{name: "the intake", create: func(v *verbs, repoDir string) (wsm.Workspace, error) {
			return v.CreateDeployIntake(context.Background(), repoDir, "deploy-intake-20261114-120000")
		}},
		{name: "the remediation", create: func(v *verbs, repoDir string) (wsm.Workspace, error) {
			return v.CreateDeployRemediation(context.Background(), repoDir, "deploy-remediation-20261114-120000")
		}},
		{name: "the escalation", wantOn: true, create: func(v *verbs, repoDir string) (wsm.Workspace, error) {
			return v.CreateDeployEscalation(context.Background(), repoDir, "deploy-escalation-20261114-120000")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}

			// Act
			made, err := tt.create(f.verbs.(*verbs), mainWorktree(t, f))

			// Assert
			if err != nil {
				t.Fatalf("create: %v", err)
			}
			f.db.remoteMu.Lock()
			standing := f.db.remote[made.ID]
			f.db.remoteMu.Unlock()
			if standing.On != tt.wantOn {
				t.Fatalf("remote control %+v, want on %v", standing, tt.wantOn)
			}
		})
	}
}

func TestAnEscalationWhoseRemoteControlCannotBeRecordedIsNotMade(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}
	f.db.remoteSetErr = errors.New("database is locked")

	// Act
	_, err := f.verbs.(*verbs).CreateDeployEscalation(context.Background(), mainWorktree(t, f), "deploy-escalation-20261114-120000")

	// Assert
	if err == nil {
		t.Fatal("CreateDeployEscalation = nil, want the escalation refused")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks,
		Message: "the escalation workspace was made but its remote control could not be turned on"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestADeployCheckWorkspaceIsSetToItsRolesEffort(t *testing.T) {
	tests := []struct {
		name string
		role merge.DeployCheckRole
		want conversationv1.AgentEffortLevel
	}{
		{name: "the intake at low effort", role: merge.DeployCheckIntake, want: conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW},
		{name: "the remediation at low effort", role: merge.DeployCheckRemediation, want: conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW},
		{name: "the escalation at medium effort", role: merge.DeployCheckEscalation, want: conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir()).ID
			f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{intakeEffort, remediationEffort, escalationEffort}, true

			// Act
			err := f.verbs.(*verbs).SetDeployCheckEffort(context.Background(), ws, tt.role)

			// Assert
			if err != nil || len(f.fleet.efforts) != 1 || f.fleet.efforts[0].Level != tt.want {
				t.Fatalf("SetDeployCheckEffort = %v, efforts %+v; want one at %v", err, f.fleet.efforts, tt.want)
			}
		})
	}
}

func TestASessionlessDeployCheckWorkspaceHasItsEffortPickedForItsStart(t *testing.T) {
	// Arrange: no session yet, so no effort selector is served.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID

	// Act
	err := f.verbs.(*verbs).SetDeployCheckEffort(context.Background(), ws, merge.DeployCheckIntake)

	// Assert
	if err != nil || f.topbarPicked != intakeEffort || len(f.fleet.efforts) != 0 {
		t.Fatalf("SetDeployCheckEffort = %v, picked %v, set %+v; want the intake's effort picked and nothing set", err, f.topbarPicked, f.fleet.efforts)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opDeployChecks, Message: "picked the deploy check workspace's effort; its session starts at it"}); !ok {
		t.Fatalf("records = %+v, want the pick at INFO", f.log.logger.Records())
	}
}

func TestADeployCheckWorkspaceWhoseEffortCannotBeSetIsRecorded(t *testing.T) {
	// Arrange: the live session serves no level the role asks for; only the
	// escalation's medium, never the intake's low.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{escalationEffort}, true

	// Act
	err := f.verbs.(*verbs).SetDeployCheckEffort(context.Background(), ws, merge.DeployCheckIntake)

	// Assert
	if err == nil {
		t.Fatal("SetDeployCheckEffort set an effort the model does not serve")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks, Message: "the deploy check workspace's effort could not be set"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestADeployCheckWorkspaceThatCannotBeMadeIsRecorded(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	_, err := f.verbs.(*verbs).CreateDeployIntake(context.Background(), t.TempDir()+"/not-a-repository", "deploy-intake-x")

	// Assert
	if err == nil {
		t.Fatal("CreateDeployIntake made a workspace in no repository")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks, Message: "the deploy check workspace could not be made"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestAStoppedDeployCheckWorkspaceIsKilledAndLeftOpen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID

	// Act
	err := f.verbs.(*verbs).StopDeployCheck(context.Background(), ws)

	// Assert
	if err != nil || len(f.shim.killedSession) != 1 || !f.shim.killedSession[0] {
		t.Fatalf("StopDeployCheck = %v, kills %v; want one forced kill", err, f.shim.killedSession)
	}
	if closures := f.db.closures[ws]; len(closures) != 0 || f.db.closedFlags[ws] {
		t.Fatalf("close records = %+v, want the standing workspace left open", closures)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opDeployChecks,
		Message: "stopped the deploy check workspace's session after its turn ended; the workspace stays open"}); !ok {
		t.Fatalf("records = %+v, want the stop recorded", f.log.logger.Records())
	}
}

func TestStoppingAnUnknownDeployCheckWorkspaceKillsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.(*verbs).StopDeployCheck(context.Background(), "no-such-workspace")

	// Assert
	if err == nil || len(f.shim.killedSession) != 0 {
		t.Fatalf("StopDeployCheck = %v, kills %v; want a refusal and no kill", err, f.shim.killedSession)
	}
}

func TestAFinishedDeployCheckWorkspaceStandsByHiddenAndOpen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID

	// Act
	err := f.verbs.(*verbs).StandByDeployCheck(context.Background(), ws)

	// Assert
	if err != nil || len(f.shim.killedSession) != 1 || !f.db.workspaces[ws].Hidden || f.db.closedFlags[ws] {
		t.Fatalf("StandByDeployCheck = %v, kills %v, record %+v; want its session killed, hidden and still open", err, f.shim.killedSession, f.db.workspaces[ws])
	}
}

func TestHideDeployCheck(t *testing.T) {
	tests := []struct {
		name       string
		hidden     bool
		closed     bool
		wantHidden bool
	}{
		{name: "a fixer in sight is hidden, its session untouched", wantHidden: true},
		{name: "a hidden fixer is left as it is", hidden: true, wantHidden: true},
		{name: "a closed fixer is left as it is", closed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())
			record.Hidden, record.Closed = tt.hidden, tt.closed
			f.db.workspaces[record.ID] = record

			// Act
			err := f.verbs.(*verbs).HideDeployCheck(context.Background(), record.ID)

			// Assert
			if err != nil || f.db.workspaces[record.ID].Hidden != tt.wantHidden || len(f.shim.killedSession) != 0 {
				t.Fatalf("HideDeployCheck = %v, hidden %v, kills %v; want hidden %v and no kill", err, f.db.workspaces[record.ID].Hidden, f.shim.killedSession, tt.wantHidden)
			}
		})
	}
}

func TestCompactDeployCheck(t *testing.T) {
	tests := []struct {
		name        string
		compactable bool
		openTurn    bool
		live        bool
		want        bool
		wantKills   int
		wantStarts  int
		message     string
	}{
		{name: "a conversation is compacted by a fresh start", compactable: true, want: true, wantStarts: 1,
			message: "compacted the deploy check workspace's conversation; its session runs on the summary"},
		{name: "a live session is ended before the compacting start", compactable: true, live: true, want: true, wantKills: 1, wantStarts: 1,
			message: "compacted the deploy check workspace's conversation; its session runs on the summary"},
		{name: "no conversation has nothing to compact", message: "the deploy check workspace has no conversation to compact"},
		{name: "a running turn is never cut", compactable: true, openTurn: true, live: true,
			message: "a turn runs in the deploy check workspace; its compaction waits for its next start"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir()).ID
			f.db.compactable = map[ids.WorkspaceID]bool{ws: tt.compactable}
			if tt.openTurn {
				f.db.openTurns = append(f.db.openTurns, wsm.Turn{Workspace: ws, ID: "t1"})
			}
			if tt.live {
				f.fleet.live[ws] = true
			}

			// Act
			got, err := f.verbs.(*verbs).CompactDeployCheck(context.Background(), ws)

			// Assert
			if err != nil || got != tt.want || len(f.shim.killedSession) != tt.wantKills || len(f.fleet.started) != tt.wantStarts {
				t.Fatalf("CompactDeployCheck = (%v, %v), kills %d, starts %d; want (%v), %d kills, %d starts", got, err, len(f.shim.killedSession), len(f.fleet.started), tt.want, tt.wantKills, tt.wantStarts)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opDeployChecks, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want %q", f.log.logger.Records(), tt.message)
			}
		})
	}
}

func TestCompactDeployCheckAnswersWhatItCannotDo(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture, ws ids.WorkspaceID)
		message string
	}{
		{name: "a compaction that cannot be owed", message: "the deploy check workspace's compaction could not be owed",
			arrange: func(f *fixture, _ ids.WorkspaceID) { f.db.oweCompactionErr = errors.New("database is locked") }},
		{name: "open turns that cannot be read", message: "could not read whether a turn runs in the deploy check workspace; it is not compacted now",
			arrange: func(f *fixture, ws ids.WorkspaceID) {
				f.db.compactable = map[ids.WorkspaceID]bool{ws: true}
				f.db.openTurnsErr = errors.New("database is locked")
			}},
		{name: "a session that cannot start", message: "the deploy check workspace's session could not start to compact",
			arrange: func(f *fixture, ws ids.WorkspaceID) {
				f.db.compactable = map[ids.WorkspaceID]bool{ws: true}
				f.fleet.startErr = errors.New("the shim died")
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir()).ID
			tt.arrange(f, ws)

			// Act
			_, err := f.verbs.(*verbs).CompactDeployCheck(context.Background(), ws)

			// Assert
			if err == nil {
				t.Fatal("CompactDeployCheck = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.message)
			}
		})
	}
}

func TestAnswerDeployCheckColdGateAnswersOnlyAStandingDeployChecksGate(t *testing.T) {
	tests := []struct {
		name     string
		standing bool
		gate     bool
	}{
		{name: "a workspace that is no standing deploy check is left as it is", gate: true},
		{name: "a standing deploy check with no gate is left as it is", standing: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())
			record.StandingDeployCheck = tt.standing
			f.db.workspaces[record.ID] = record
			if tt.gate {
				f.cards.coldGate = &ServedColdGate{VendorSessionID: "v-1"}
			}

			// Act
			err := f.verbs.(*verbs).AnswerDeployCheckColdGate(context.Background(), record.ID)

			// Assert
			if err != nil || len(f.cards.ended) != 0 {
				t.Fatalf("AnswerDeployCheckColdGate = %v, ended %v; want nothing answered", err, f.cards.ended)
			}
		})
	}
}

func TestAResumedDeployCheckWorkspaceIsShownAgain(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID
	if err := f.verbs.(*verbs).StandByDeployCheck(context.Background(), ws); err != nil {
		t.Fatal(err)
	}

	// Act
	err := f.verbs.(*verbs).ResumeDeployCheck(context.Background(), ws)

	// Assert
	if err != nil || f.db.workspaces[ws].Hidden {
		t.Fatalf("ResumeDeployCheck = %v, record %+v; want it shown again", err, f.db.workspaces[ws])
	}
}

func TestRefreshDeployCheckBringsTheFixerToItsParentsTip(t *testing.T) {
	// Arrange
	f := newFixture(t)
	record := f.workspace("w1", t.TempDir())
	record.ParentBranch = "master"
	f.db.with(record)
	f.git.existingBranches = map[string]bool{record.Branch: true, "master": true}

	// Act
	err := f.verbs.(*verbs).RefreshDeployCheck(context.Background(), record.ID)

	// Assert
	if want := record.Dir + "->master"; err != nil || len(f.git.fastForwarded) != 1 || f.git.fastForwarded[0] != want {
		t.Fatalf("RefreshDeployCheck = %v, fast-forwards %v; want exactly %q", err, f.git.fastForwarded, want)
	}
}

func TestRefreshDeployCheckFailsLoudlyWhenTheAdvanceCannotRun(t *testing.T) {
	// Arrange
	f := newFixture(t)
	record := f.workspace("w1", t.TempDir())
	record.ParentBranch = "master"
	f.db.with(record)
	f.git.existingBranches = map[string]bool{record.Branch: true, "master": true}
	f.git.fastForwardErr = errors.New("not a fast-forward")

	// Act
	err := f.verbs.(*verbs).RefreshDeployCheck(context.Background(), record.ID)

	// Assert
	if err == nil {
		t.Fatal("RefreshDeployCheck = nil, want the failed advance answered")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks,
		Message: "could not advance the standing deploy fixer to its parent's tip: fast-forward"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestEveryAdvanceToAParentsTipGoesThroughOneHelper(t *testing.T) {
	// Arrange
	sources := map[string]string{}
	for _, file := range []string{"open.go", "deploychecks.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		sources[file] = string(body)
	}

	// Assert: both callers share advanceToParent, and only it fast-forwards.
	for file, src := range sources {
		if !strings.Contains(src, "v.advanceToParent(") {
			t.Fatalf("%s does not advance through advanceToParent", file)
		}
	}
	if strings.Count(sources["open.go"]+sources["deploychecks.go"], "Git.FastForward(") != 1 {
		t.Fatal("a second fast-forward to a parent's tip was written beside advanceToParent")
	}
}
