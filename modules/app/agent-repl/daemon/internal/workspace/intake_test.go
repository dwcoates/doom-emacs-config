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
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
)

// intakeBrief is the brief every arranged intake workspace is handed.
const intakeBrief = "an-intake-brief"

// intakeFixture arranges a repository an intake workspace can be cut from and
// the brief it is handed, answering the repository's main worktree.
func intakeFixture(t *testing.T) (*fixture, string) {
	t.Helper()
	f := newFixture(t)
	repo := mainWorktree(t, f)
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus")}
	f.briefs[intakeBrief] = prompts.Prompt{
		Name:         intakeBrief,
		Body:         "work out what {{who}} asked: {{what}}",
		Placeholders: []string{"who", "what"},
	}
	return f, repo
}

// intakeSpec is an intake workspace in REPO, named NAME.
func intakeSpec(repo, name string) IntakeSpec {
	return IntakeSpec{
		RepoDir: repo, Name: name, Model: "opus", Brief: intakeBrief,
		Values: map[string]string{"who": "the-asker", "what": "why is the sky blue"},
	}
}

func TestCreateIntakeCreatesAHiddenMarkedWorkspaceHandedItsBrief(t *testing.T) {
	// Arrange
	f, repo := intakeFixture(t)

	// Act
	created, err := f.verbs.CreateIntake(context.Background(), intakeSpec(repo, "an-intake"))

	// Assert
	if err != nil {
		t.Fatalf("CreateIntake: %v", err)
	}
	if len(f.git.created) != 1 || f.git.created[0].RepoDir != repo {
		t.Fatalf("created worktrees = %+v, want one in %q", f.git.created, repo)
	}
	if !f.db.intakes[created.ID] || !f.db.workspaces[created.ID].Hidden {
		t.Errorf("intake mark %v, hidden %v; want the workspace marked and hidden", f.db.intakes[created.ID], f.db.workspaces[created.ID].Hidden)
	}
	if got := f.db.putSessions[0].Model; got != "opus" {
		t.Errorf("session model = %q, want the spec's", got)
	}
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(sent, "work out what the-asker asked: why is the sky blue") {
		t.Errorf("initial prompt = %q, want the brief with its values spliced in", sent)
	}
	if len(operationRecords(f, dlog.LevelInfo, opIntake)) != 1 {
		t.Errorf("want one INFO %s record, got %+v", opIntake, f.log.logger.Records())
	}
}

func TestCreateIntakeCreatesNothingWhenItsBriefWillNotCompose(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(spec *IntakeSpec)
	}{
		{name: "the brief is missing", arrange: func(spec *IntakeSpec) { spec.Brief = "no-such-brief" }},
		{name: "a placeholder has no value", arrange: func(spec *IntakeSpec) { delete(spec.Values, "what") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, repo := intakeFixture(t)
			spec := intakeSpec(repo, "an-intake")
			tt.arrange(&spec)

			// Act
			_, err := f.verbs.CreateIntake(context.Background(), spec)

			// Assert
			if err == nil {
				t.Fatal("CreateIntake succeeded without its brief")
			}
			if len(f.git.created) != 0 || len(f.queue.submissions) != 0 {
				t.Fatalf("created %+v, submitted %d; want nothing made", f.git.created, len(f.queue.submissions))
			}
			if len(operationRecords(f, dlog.LevelError, opIntake)) != 1 {
				t.Fatalf("want one ERROR %s record, got %+v", opIntake, f.log.logger.Records())
			}
		})
	}
}

func TestAnIntakeWorkspaceIsMarkedBeforeItsSessionStarts(t *testing.T) {
	// Arrange: the mark fails, so what ran before it is all that ran.
	f, repo := intakeFixture(t)
	f.db.markIntakeErr = errors.New("disk I/O error")

	// Act
	_, err := f.verbs.CreateIntake(context.Background(), intakeSpec(repo, "an-intake"))

	// Assert
	if err == nil {
		t.Fatal("CreateIntake succeeded with a failed mark")
	}
	if len(f.fleet.started) != 0 || len(f.queue.submissions) != 0 {
		t.Fatalf("started %v, submitted %d; want no session and no prompt before the mark", f.fleet.started, len(f.queue.submissions))
	}
	if len(f.git.nuked) != 1 {
		t.Fatalf("nuked %+v, want the failed create's worktree undone", f.git.nuked)
	}
	if !hasRecord(f, dlog.LevelError, opCreate) {
		t.Fatalf("want an ERROR %s record, got %+v", opCreate, f.log.logger.Records())
	}
}

func TestAnIntakeCreateSpecThatContradictsItselfIsRefused(t *testing.T) {
	parent := ids.WorkspaceID("w-parent")
	tests := []struct {
		name    string
		arrange func(spec *CreateSpec)
	}{
		{name: "no name", arrange: func(spec *CreateSpec) { spec.Name = "" }},
		{name: "no initial prompt", arrange: func(spec *CreateSpec) { spec.InitialPrompt = " " }},
		{name: "a one-shot", arrange: func(spec *CreateSpec) { spec.OneShot = true }},
		{name: "a child", arrange: func(spec *CreateSpec) { spec.Parent = &parent }},
		{name: "a fork", arrange: func(spec *CreateSpec) { spec.ForkFrom = &parent }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, repo := intakeFixture(t)
			spec := CreateSpec{
				Originator: lifecycleconfig.OriginatorUserOrWorkspace, RepoDir: repo,
				InitialPrompt: "work it out", Name: "an-intake", Intake: true,
			}
			tt.arrange(&spec)

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if err == nil {
				t.Fatal("Create accepted a contradictory intake spec")
			}
			if len(f.git.created) != 0 {
				t.Fatalf("created %+v, want nothing made", f.git.created)
			}
			if !hasRecord(f, dlog.LevelError, opCreate) {
				t.Fatalf("want an ERROR %s record, got %+v", opCreate, f.log.logger.Records())
			}
		})
	}
}

// intakeWorkspace arranges a quiet intake workspace in a real directory.
func intakeWorkspace(t *testing.T, f *fixture) wsm.Workspace {
	t.Helper()
	ws := f.workspace("w1", t.TempDir())
	f.db.intakes[ws.ID] = true
	return ws
}

func TestATurnEndRetiresAQuietIntakeWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := intakeWorkspace(t, f)

	// Act
	err := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CarryOutSelfClose: %v", err)
	}
	if len(f.fleet.stopped) != 1 {
		t.Errorf("stopped %+v, want the session stopped once", f.fleet.stopped)
	}
	if len(f.git.nuked) != 1 || f.git.nuked[0].WorktreeDir != ws.Dir || f.git.nuked[0].Branch != ws.Branch {
		t.Errorf("nuked %+v, want the intake workspace's worktree and branch", f.git.nuked)
	}
	closures := f.db.closures[ws.ID]
	if len(closures) == 0 {
		t.Fatal("the retired intake workspace was not closed")
	}
	for _, closure := range closures {
		if closure.By != wsm.CloseByDaemon {
			t.Errorf("closure by %q, want every close of the retirement recorded as the daemon's", closure.By)
		}
	}
	if len(operationRecords(f, dlog.LevelInfo, opIntakeRetire)) != 1 {
		t.Errorf("want one INFO %s record, got %+v", opIntakeRetire, f.log.logger.Records())
	}
}

func TestATurnEndRetiresAnIntakeWorkspaceThatAlsoAskedToCloseItself(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := intakeWorkspace(t, f)
	if err := f.db.MarkCloseWhenTurnEnds(context.Background(), ws.ID, f.verbs.(*verbs).now()); err != nil {
		t.Fatalf("MarkCloseWhenTurnEnds: %v", err)
	}

	// Act
	err := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

	// Assert
	if err != nil || len(f.git.nuked) != 1 {
		t.Fatalf("CarryOutSelfClose = %v, nuked %+v; want the worktree destroyed", err, f.git.nuked)
	}
}

func TestAnIntakeWorkspaceWithSomethingStandingIsRetiredAtALaterTurnEnd(t *testing.T) {
	// Arrange: a held prompt blocks the first turn end's retirement.
	f := newFixture(t)
	ws := intakeWorkspace(t, f)
	f.db.held[ws.ID] = []wsm.HeldPrompt{{Turn: "held-1"}}

	// Act
	first := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)
	keptClosed, keptNuked := f.db.closedFlags[ws.ID], len(f.git.nuked)
	delete(f.db.held, ws.ID)
	second := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

	// Assert
	if first != nil || second != nil {
		t.Fatalf("CarryOutSelfClose = %v, then %v", first, second)
	}
	if keptClosed || keptNuked != 0 {
		t.Errorf("after the blocked turn end: closed %v, nuked %d; want it kept whole", keptClosed, keptNuked)
	}
	if !f.db.closedFlags[ws.ID] || len(f.git.nuked) != 1 {
		t.Errorf("after the quiet turn end: closed %v, nuked %+v; want it retired", f.db.closedFlags[ws.ID], f.git.nuked)
	}
}

func TestAnIntakeWorkspaceWhoseWorktreeIsGoneIsLeftAlone(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := intakeWorkspace(t, f)
	if err := os.RemoveAll(ws.Dir); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}

	// Act
	err := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CarryOutSelfClose: %v", err)
	}
	if len(f.fleet.stopped) != 0 || len(f.git.nuked) != 0 || f.db.closedFlags[ws.ID] {
		t.Fatalf("stopped %+v, nuked %+v, closed %v; want nothing done twice", f.fleet.stopped, f.git.nuked, f.db.closedFlags[ws.ID])
	}
}

func TestRetiringAnIntakeWorkspaceFailsLoudly(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(f *fixture)
		operation string
		wantNuked int
	}{
		{name: "the intake mark cannot be read", arrange: func(f *fixture) { f.db.isIntakeErr = errors.New("disk I/O error") }, operation: opSelfClose},
		{name: "the close cannot be recorded", arrange: func(f *fixture) { f.db.setClosedErr = errors.New("disk I/O error") }, operation: opIntakeRetire},
		{name: "git will not destroy the worktree", arrange: func(f *fixture) { f.git.nukeErr = errors.New("fatal: worktree is locked") }, operation: opIntakeRetire},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := intakeWorkspace(t, f)
			tt.arrange(f)

			// Act
			err := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

			// Assert
			if err == nil {
				t.Fatal("CarryOutSelfClose succeeded")
			}
			if len(f.git.nuked) != 0 {
				t.Fatalf("nuked %+v, want the worktree kept", f.git.nuked)
			}
			if len(operationRecords(f, dlog.LevelError, tt.operation)) == 0 {
				t.Fatalf("want an ERROR %s record, got %+v", tt.operation, f.log.logger.Records())
			}
		})
	}
}

// questionFixture arranges an asking workspace, the home repository agent-repl's
// own checkout is in, and the question brief.
func questionFixture(t *testing.T) (*fixture, string) {
	t.Helper()
	f := newFixture(t)
	home := mainWorktree(t, f)
	f.ownRepository(t, home)
	f.git.mainOf = map[string]string{f.mutable(t).deps.CheckoutRoot: home}
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf(DefaultLaunchModel)}
	asker := f.workspace("w1", t.TempDir())
	asker.Name = "the asker (doom)"
	f.db.with(asker)
	f.briefs[BriefQuestionIntake] = prompts.Prompt{
		Name:         BriefQuestionIntake,
		Body:         "{{asker}} asked: {{question}}",
		Placeholders: []string{"asker", "question"},
	}
	return f, home
}

func TestAskQuestionIntakeCreatesAnOpusIntakeWorkspaceInTheHomeRepository(t *testing.T) {
	// Arrange
	f, home := questionFixture(t)

	// Act
	created, err := f.verbs.AskQuestionIntake(context.Background(), "w1", "why did the merge stall")

	// Assert
	if err != nil {
		t.Fatalf("AskQuestionIntake: %v", err)
	}
	if len(f.git.created) != 1 || f.git.created[0].RepoDir != home {
		t.Fatalf("created worktrees = %+v, want one in the home repository %q", f.git.created, home)
	}
	if !f.db.intakes[created.ID] {
		t.Error("the question's workspace is no intake workspace")
	}
	if !strings.HasPrefix(created.Name, "question-intake-") {
		t.Errorf("name = %q, want a question-intake name", created.Name)
	}
	if got := f.db.putSessions[0].Model; got != DefaultLaunchModel {
		t.Errorf("session model = %q, want %q", got, DefaultLaunchModel)
	}
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(sent, "the asker (doom) asked: why did the merge stall") {
		t.Errorf("initial prompt = %q, want the asker's name and the question", sent)
	}
}

func TestAskQuestionIntakeFails(t *testing.T) {
	tests := []struct {
		name      string
		asker     ids.WorkspaceID
		arrange   func(f *fixture)
		operation string
	}{
		{name: "an unknown asker", asker: "w-nobody", arrange: func(*fixture) {}, operation: "daemon.refusal.typed"},
		{name: "git cannot name the home repository", asker: "w1", arrange: func(f *fixture) {
			f.git.commonDirErr = errors.New("fatal: not a git repository")
		}, operation: opIntake},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, _ := questionFixture(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.AskQuestionIntake(context.Background(), tt.asker, "why")

			// Assert
			if err == nil {
				t.Fatal("AskQuestionIntake succeeded")
			}
			if len(f.git.created) != 0 {
				t.Fatalf("created %+v, want no intake workspace", f.git.created)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: tt.operation}); !ok {
				t.Fatalf("want a %s record, got %+v", tt.operation, f.log.logger.Records())
			}
		})
	}
}
