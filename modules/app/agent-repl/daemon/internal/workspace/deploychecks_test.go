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
	"agent-repld/internal/wsm"
)

func TestTheDeployMediatorIsMadeHiddenOnOpusWithNoPrompt(t *testing.T) {
	// Act
	spec := deployMediatorSpec("/repo", "deploy-mediator")

	// Assert
	if spec.RepoDir != "/repo" || spec.Name != "deploy-mediator" || spec.Model != "opus" || !spec.Hidden || spec.InitialPrompt != "" {
		t.Fatalf("spec = %+v, want an unprompted, hidden workspace on opus", spec)
	}
}

func TestTheDeployMediatorIsMadeUnpromptedInRemoteControl(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}

	// Act
	made, err := f.verbs.(*verbs).CreateDeployMediator(context.Background(), mainWorktree(t, f), "deploy-mediator")

	// Assert
	if err != nil {
		t.Fatalf("CreateDeployMediator: %v", err)
	}
	if len(f.fleet.efforts) != 0 {
		t.Fatalf("efforts = %+v, want none set at the create", f.fleet.efforts)
	}
	f.db.remoteMu.Lock()
	standing := f.db.remote[made.ID]
	f.db.remoteMu.Unlock()
	if !standing.On {
		t.Fatalf("remote control %+v, want it on", standing)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opDeployChecks, Message: "made the deploy mediator"}); !ok {
		t.Fatalf("records = %+v, want the made mediator recorded", f.log.logger.Records())
	}
}

func TestADeployMediatorThatCannotBeMadeIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture) string
		message string
	}{
		{name: "no repository to make it in", message: "the deploy mediator could not be made",
			arrange: func(t *testing.T, f *fixture) string { return t.TempDir() + "/not-a-repository" }},
		{name: "a remote control that cannot be recorded", message: "the deploy mediator was made but its remote control could not be turned on",
			arrange: func(t *testing.T, f *fixture) string {
				f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}
				f.db.remoteSetErr = errors.New("database is locked")
				return mainWorktree(t, f)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			repoDir := tt.arrange(t, f)

			// Act
			_, err := f.verbs.(*verbs).CreateDeployMediator(context.Background(), repoDir, "deploy-mediator")

			// Assert
			if err == nil {
				t.Fatal("CreateDeployMediator = nil, want the failure answered")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.message)
			}
		})
	}
}

func TestTheDeployMediatorIsSetToLowEffort(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{mediatorEffort}, true

	// Act
	err := f.verbs.(*verbs).SetDeployMediatorEffort(context.Background(), ws)

	// Assert
	if err != nil || len(f.fleet.efforts) != 1 || f.fleet.efforts[0].Level != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW {
		t.Fatalf("SetDeployMediatorEffort = %v, efforts %+v; want one at low", err, f.fleet.efforts)
	}
}

func TestASessionlessDeployMediatorHasItsEffortPickedForItsStart(t *testing.T) {
	// Arrange: no session yet, so no effort selector is served.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID

	// Act
	err := f.verbs.(*verbs).SetDeployMediatorEffort(context.Background(), ws)

	// Assert
	if err != nil || f.topbarPicked != mediatorEffort || len(f.fleet.efforts) != 0 {
		t.Fatalf("SetDeployMediatorEffort = %v, picked %v, set %+v; want its effort picked and nothing set", err, f.topbarPicked, f.fleet.efforts)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opDeployChecks, Message: "picked the deploy mediator's effort; its session starts at it"}); !ok {
		t.Fatalf("records = %+v, want the pick at INFO", f.log.logger.Records())
	}
}

func TestADeployMediatorWhoseEffortCannotBeSetIsRecorded(t *testing.T) {
	// Arrange: the live session serves no low effort.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir()).ID
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM}, true

	// Act
	err := f.verbs.(*verbs).SetDeployMediatorEffort(context.Background(), ws)

	// Assert
	if err == nil {
		t.Fatal("SetDeployMediatorEffort set an effort the model does not serve")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opDeployChecks, Message: "the deploy mediator's effort could not be set"}); !ok {
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
			record.DeployMediator = tt.standing
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

// fixerWithChild is a standing fixer with an open child it made.
func fixerWithChild(t *testing.T, f *fixture) (fixer, child ids.WorkspaceID) {
	t.Helper()
	fixer = f.workspace("fixer", t.TempDir()).ID
	record := f.workspace("child", t.TempDir())
	record.Parent = &fixer
	f.db.workspaces[record.ID] = record
	return fixer, record.ID
}

func TestHidingTheFixerHidesTheWorkspacesItMade(t *testing.T) {
	tests := []struct {
		name  string
		hide  func(v *verbs, ws ids.WorkspaceID) error
		kills int
	}{
		{name: "a fixer standing by at its finish", kills: 1, hide: func(v *verbs, ws ids.WorkspaceID) error {
			return v.StandByDeployCheck(context.Background(), ws)
		}},
		{name: "a fixer whose queue no check holds", hide: func(v *verbs, ws ids.WorkspaceID) error {
			return v.HideDeployCheck(context.Background(), ws)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			fixer, child := fixerWithChild(t, f)

			// Act
			err := tt.hide(f.verbs.(*verbs), fixer)

			// Assert
			if err != nil || !f.db.workspaces[fixer].Hidden || !f.db.workspaces[child].Hidden || len(f.shim.killedSession) != tt.kills {
				t.Fatalf("hide = %v, fixer hidden %v, child hidden %v, kills %v; want both hidden", err, f.db.workspaces[fixer].Hidden, f.db.workspaces[child].Hidden, f.shim.killedSession)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opSetHidden, Message: "hid the deploy check workspace with its family"}); !ok {
				t.Fatalf("records = %+v, want the family's hide recorded", f.log.logger.Records())
			}
		})
	}
}

func TestResumingTheFixerShowsTheWorkspacesHiddenWithIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	fixer, child := fixerWithChild(t, f)
	if err := f.verbs.(*verbs).StandByDeployCheck(context.Background(), fixer); err != nil {
		t.Fatal(err)
	}

	// Act
	err := f.verbs.(*verbs).ResumeDeployCheck(context.Background(), fixer)

	// Assert
	if err != nil || f.db.workspaces[fixer].Hidden || f.db.workspaces[child].Hidden {
		t.Fatalf("ResumeDeployCheck = %v, fixer hidden %v, child hidden %v; want both shown", err, f.db.workspaces[fixer].Hidden, f.db.workspaces[child].Hidden)
	}
}

func TestAFamilyHideOrShowThatFailsIsRecordedAndChangesNothing(t *testing.T) {
	tests := []struct {
		name    string
		act     func(v *verbs, ws ids.WorkspaceID) error
		message string
	}{
		{name: "a stand-by", message: "could not hide the deploy check workspace with its family", act: func(v *verbs, ws ids.WorkspaceID) error {
			return v.StandByDeployCheck(context.Background(), ws)
		}},
		{name: "a hide", message: "could not hide the deploy check workspace with its family", act: func(v *verbs, ws ids.WorkspaceID) error {
			return v.HideDeployCheck(context.Background(), ws)
		}},
		{name: "a resume", message: "could not show the deploy check workspace with its family", act: func(v *verbs, ws ids.WorkspaceID) error {
			return v.ResumeDeployCheck(context.Background(), ws)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			fixer, _ := fixerWithChild(t, f)
			f.db.familyHiddenErr = errors.New("database is locked")

			// Act
			err := tt.act(f.verbs.(*verbs), fixer)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "database is locked") || len(f.shim.killedSession) != 0 {
				t.Fatalf("err = %v, kills %v; want the store's failure surfaced and no session ended", err, f.shim.killedSession)
			}
			rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opSetHidden, Message: tt.message})
			if !ok || rec.Context["cause"] != "database is locked" {
				t.Fatalf("records = %+v, want the failure recorded with its cause", f.log.logger.Records())
			}
		})
	}
}
