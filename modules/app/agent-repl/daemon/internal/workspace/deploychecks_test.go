package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
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

func TestADeployCheckWorkspaceStoppedAtOnceIsKilledAndLeftOpen(t *testing.T) {
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
		Message: "stopped the deploy check workspace's session at once; its turn is cut, not ended, and the workspace stays open"}); !ok {
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

func TestADeployCheckWorkspaceThatCannotBeHiddenIsNotStoodBy(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID
	f.db.setHiddenErr = errors.New("disk gone")

	// Act
	err := f.verbs.(*verbs).StandByDeployCheck(context.Background(), ws)

	// Assert
	if err == nil || len(f.shim.killedSession) != 0 {
		t.Fatalf("StandByDeployCheck = %v, kills %v; want a refusal and its session left", err, f.shim.killedSession)
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
