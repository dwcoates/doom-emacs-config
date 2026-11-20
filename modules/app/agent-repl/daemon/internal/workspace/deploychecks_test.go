package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

func TestTheDeployMediatorIsMadeShownOnOpusWithNoPrompt(t *testing.T) {
	// Act
	spec := deployMediatorSpec("/repo", "deploy-mediator")

	// Assert
	if spec.RepoDir != "/repo" || spec.Name != StandingBranch("deploy-mediator") || !spec.standing || spec.Model != "opus" || spec.Hidden || spec.InitialPrompt != "" {
		t.Fatalf("spec = %+v, want an unprompted standing workspace on opus, in sight, in the reserved namespace", spec)
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
	// An earlier build hid it.
	if _, err := f.db.HideFamily(context.Background(), ws, time.Unix(0, 0)); err != nil {
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

func TestResumingTheFixerShowsTheWorkspacesHiddenWithIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	fixer, child := fixerWithChild(t, f)
	// An earlier build hid the fixer with its family.
	if _, err := f.db.HideFamily(context.Background(), fixer, time.Unix(0, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	err := f.verbs.(*verbs).ResumeDeployCheck(context.Background(), fixer)

	// Assert
	if err != nil || f.db.workspaces[fixer].Hidden || f.db.workspaces[child].Hidden {
		t.Fatalf("ResumeDeployCheck = %v, fixer hidden %v, child hidden %v; want both shown", err, f.db.workspaces[fixer].Hidden, f.db.workspaces[child].Hidden)
	}
}

func TestAFamilyShowThatFailsIsRecordedAndChangesNothing(t *testing.T) {
	tests := []struct {
		name    string
		act     func(v *verbs, ws ids.WorkspaceID) error
		message string
	}{
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

// TestCompactRequeuedMergeColdGate pins the owner request of 2026-11-20: a
// requeued merge's cold gate is answered by a compaction of the whole
// conversation with the gate's default summarizer, keeping the held prompts.
func TestCompactRequeuedMergeColdGate(t *testing.T) {
	tests := []struct {
		name        string
		gate        bool
		wantResumes int
	}{
		{name: "a standing gate is answered by a compaction that keeps the held prompts", gate: true, wantResumes: 1},
		{name: "a workspace with no gate standing is left as it is"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tt.gate {
				served, _ := coldCompactMenu()
				f.cards.coldGate = &ServedColdGate{VendorSessionID: "vendor-1", Compact: served}
			}

			// Act
			err := f.verbs.CompactRequeuedMergeColdGate(context.Background(), "w1")

			// Assert
			if err != nil || len(f.fleet.resumes) != tt.wantResumes {
				t.Fatalf("CompactRequeuedMergeColdGate = %v, %d resumes; want %d", err, len(f.fleet.resumes), tt.wantResumes)
			}
			if !tt.gate {
				return
			}
			compact := f.fleet.resumes[0].Remediation.GetCompact()
			if compact.GetModel().GetName() != headless.ModelHaiku || compact.GetScope() != conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL {
				t.Fatalf("compact remediation = %v, want Haiku over the whole conversation", compact)
			}
			if len(f.queue.gateFates) != 1 || f.queue.gateFates[0] != promptqueue.GateKeep {
				t.Fatalf("gate fates = %v, want the held prompts kept", f.queue.gateFates)
			}
		})
	}
}

func TestTheDeployMediatorIsMadeInTheReservedNamespaceBesideAHandMadeBranch(t *testing.T) {
	// Arrange: a branch a person cut under the mediator's slug, held by no
	// workspace (as the 2026-11-17 `deploy-intake` was at 3913181be5).
	f := newFixture(t)
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	repo := mainWorktree(t, f)
	f.git.existingBranches = map[string]bool{"deploy-mediator": true}

	// Act
	made, err := f.verbs.(*verbs).CreateDeployMediator(context.Background(), repo, "deploy-mediator")

	// Assert: made on its own branch and directory; the person's branch is
	// untouched.
	want := StandingBranch("deploy-mediator")
	if err != nil {
		t.Fatalf("CreateDeployMediator: %v", err)
	}
	if made.Name != want || made.Branch != want {
		t.Fatalf("made = %+v, want name and branch %q", made, want)
	}
	if len(f.git.created) != 1 || f.git.created[0].Branch != want || filepath.Base(f.git.created[0].WorktreeDir) != ".agent-repl-standing-deploy-mediator" {
		t.Fatalf("created = %+v, want one worktree on %q in its dot-led directory", f.git.created, want)
	}
	if len(f.git.nuked) != 0 {
		t.Fatalf("nuked = %+v, want the hand-made branch untouched", f.git.nuked)
	}
}

func TestACreateInTheReservedNamespaceIsRefused(t *testing.T) {
	tests := []struct {
		name     string
		supplied string
	}{
		{name: "a standing workspace's branch", supplied: "agent-repl/standing/deploy-mediator"},
		{name: "the namespace's root", supplied: "agent-repl"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			t.Setenv(PrefixEnv, "")
			t.Setenv(LegacyPrefixEnv, "")
			repo := mainWorktree(t, f)

			// Act
			_, err := f.verbs.Create(context.Background(), CreateSpec{originator: lifecycleconfig.OriginatorUserOrWorkspace, RepoDir: repo, Name: tt.supplied})

			// Assert: refused typed, recorded, and nothing claimed or built.
			refusal, ok := AsRefusal(err)
			if !ok || refusal.Arm != ArmInvalidName || refusal.Fields["component"] != ReservedNamespace || refusal.Fields["rule"] != NameRuleReserved {
				t.Fatalf("Create = %v, want the %s refusal under the %s rule", err, ArmInvalidName, NameRuleReserved)
			}
			if len(f.git.created) != 0 {
				t.Fatalf("created = %+v, want nothing built", f.git.created)
			}
			if claimed := f.verbs.(*verbs).names.claimed; len(claimed) != 0 {
				t.Fatalf("claimed = %+v, want no claim", claimed)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
			if !ok || record.Context["arm"] != ArmInvalidName || record.Context["rpc"] != "CreateWorkspace" {
				t.Fatalf("records = %+v, want an INFO refusal of CreateWorkspace naming %s", f.log.logger.Records(), ArmInvalidName)
			}
		})
	}
}

func TestDeployCheckContextTokensAreTheContextChipsFigure(t *testing.T) {
	tests := []struct {
		name   string
		tokens map[ids.WorkspaceID]int64
		want   int64
	}{
		{name: "a conversation holding context answers its figure", tokens: map[ids.WorkspaceID]int64{"w1": 250_000}, want: 250_000},
		{name: "a conversation with none answers zero"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			v := f.verbs.(*verbs)
			v.deps.Topbar = stubTopbar{contextTokens: tt.tokens}

			// Act
			got := v.DeployCheckContextTokens("w1")

			// Assert
			if got != tt.want {
				t.Fatalf("DeployCheckContextTokens = %d, want %d", got, tt.want)
			}
		})
	}
}
