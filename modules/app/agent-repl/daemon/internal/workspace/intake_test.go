package workspace

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
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

func TestAnIntakeWorkspaceIsRegisteredBornMarkedAndHidden(t *testing.T) {
	// Arrange
	f, repo := intakeFixture(t)

	// Act
	created, err := f.verbs.CreateIntake(context.Background(), intakeSpec(repo, "an-intake"))

	// Assert
	if err != nil {
		t.Fatalf("CreateIntake: %v", err)
	}
	facts := registeredFactsOf(f, created.ID)
	if facts == nil || !facts.Intake || !facts.Hidden {
		t.Fatalf("registered facts = %+v, want the insert itself to carry Intake and Hidden", facts)
	}
}

func TestNoRosterPublishedDuringAnIntakeCreateDrawsItVisible(t *testing.T) {
	// Arrange
	f, repo := intakeFixture(t)

	// Act
	created, err := f.verbs.CreateIntake(context.Background(), intakeSpec(repo, "an-intake"))

	// Assert
	if err != nil {
		t.Fatalf("CreateIntake: %v", err)
	}
	seen := 0
	for i, reg := range f.sidebar.registries {
		for _, ws := range reg.Workspaces {
			if ws.ID != created.ID {
				continue
			}
			seen++
			if !ws.Hidden {
				t.Fatalf("roster publication %d carried the intake workspace visible", i)
			}
		}
	}
	if seen == 0 {
		t.Fatal("no roster publication carried the intake workspace, so the test saw nothing")
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
		{name: "an intake card on no intake", arrange: func(spec *CreateSpec) {
			spec.Intake, spec.IntakeCard = false, &wsm.IntakeCard{Asker: "w1", Row: "row-1"}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, repo := intakeFixture(t)
			spec := CreateSpec{
				originator: lifecycleconfig.OriginatorUserOrWorkspace, RepoDir: repo,
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

// questionCard is the card every arranged question is asked about.
var questionCard = wsm.IntakeCard{Asker: "w1", Row: "row-1"}

// cardSend records every workspace a card's question is sent to, answering
// REFUSED and ERR.
type cardSend struct {
	to      []wsm.Workspace
	refused *agentreplv1.SubmitPromptError
	err     error
}

func (c *cardSend) send(_ context.Context, to wsm.Workspace) (*agentreplv1.SubmitPromptError, error) {
	c.to = append(c.to, to)
	return c.refused, c.err
}

// addressee registers an open workspace named NAME in the fixture's registry.
func addressee(t *testing.T, f *fixture, id ids.WorkspaceID, name string) wsm.Workspace {
	t.Helper()
	ws := f.workspace(id, t.TempDir())
	ws.Name = name
	f.db.with(ws)
	return ws
}

func TestAskAboutCardCreatesAnOpusIntakeWorkspaceInTheHomeRepositoryForAnUnaddressedCard(t *testing.T) {
	// Arrange
	f, home := questionFixture(t)
	sent := &cardSend{}

	// Act
	answer, err := f.verbs.AskAboutCard(context.Background(), questionCard, "why did the merge stall", sent.send)

	// Assert
	if err != nil {
		t.Fatalf("AskAboutCard: %v", err)
	}
	created := answer.Recipient
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
	text := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(text, "the asker (doom) asked: why did the merge stall") {
		t.Errorf("initial prompt = %q, want the asker's name and the question", text)
	}
	if len(sent.to) != 0 || answer.Refused != nil {
		t.Errorf("sent to %+v, refused %v; want the question in the intake's brief alone", sent.to, answer.Refused)
	}
}

func TestAskAboutCardRecordsTheCardWithItsIntakeWorkspace(t *testing.T) {
	// Arrange
	f, _ := questionFixture(t)

	// Act
	answer, err := f.verbs.AskAboutCard(context.Background(), questionCard, "why", (&cardSend{}).send)

	// Assert
	if err != nil {
		t.Fatalf("AskAboutCard: %v", err)
	}
	if got, ok := f.db.intakeCards[answer.Recipient.ID]; !ok || got != questionCard {
		t.Fatalf("intake card = %+v, %v; want %+v registered with the intake", got, ok, questionCard)
	}
}

func TestAskAboutCardSendsToTheWorkspaceAlreadyAddressingTheCard(t *testing.T) {
	tests := []struct {
		name       string
		addressees func(work, intake ids.WorkspaceID) wsm.CardAddressees
		want       string
	}{
		{name: "the workspace the intake created", addressees: func(work, _ ids.WorkspaceID) wsm.CardAddressees {
			return wsm.CardAddressees{Work: &work}
		}, want: "card-work"},
		{name: "the intake still taking the first question in", addressees: func(_, intake ids.WorkspaceID) wsm.CardAddressees {
			return wsm.CardAddressees{OpenIntake: &intake}
		}, want: "question-intake-1"},
		{name: "the created workspace over the intake still open", addressees: func(work, intake ids.WorkspaceID) wsm.CardAddressees {
			return wsm.CardAddressees{Work: &work, OpenIntake: &intake}
		}, want: "card-work"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, _ := questionFixture(t)
			work := addressee(t, f, "w-work", "card-work")
			intake := addressee(t, f, "w-intake", "question-intake-1")
			f.db.cardAddressees = tt.addressees(work.ID, intake.ID)
			sent := &cardSend{}

			// Act
			answer, err := f.verbs.AskAboutCard(context.Background(), questionCard, "and the footer?", sent.send)

			// Assert
			if err != nil {
				t.Fatalf("AskAboutCard: %v", err)
			}
			if len(sent.to) != 1 || sent.to[0].Name != tt.want || answer.Recipient.Name != tt.want {
				t.Fatalf("sent to %+v, answered %q; want the question sent to %q", sent.to, answer.Recipient.Name, tt.want)
			}
			if len(f.git.created) != 0 {
				t.Fatalf("created %+v, want no intake workspace", f.git.created)
			}
			if len(operationRecords(f, dlog.LevelInfo, opIntake)) != 1 {
				t.Fatalf("want one INFO %s record, got %+v", opIntake, f.log.logger.Records())
			}
		})
	}
}

func TestAskAboutCardAnswersTheAddresseesRefusal(t *testing.T) {
	// Arrange
	f, _ := questionFixture(t)
	work := addressee(t, f, "w-work", "card-work")
	f.db.cardAddressees = wsm.CardAddressees{Work: &work.ID}
	refusal := &agentreplv1.SubmitPromptError{Reason: &agentreplv1.SubmitPromptError_NoSession{NoSession: &agentreplv1.SubmitPromptNoSession{}}}
	sent := &cardSend{refused: refusal}

	// Act
	answer, err := f.verbs.AskAboutCard(context.Background(), questionCard, "why", sent.send)

	// Assert
	if err != nil {
		t.Fatalf("AskAboutCard: %v", err)
	}
	if answer.Refused != refusal || answer.Recipient.ID != work.ID {
		t.Fatalf("answered %+v, want the addressee's refusal", answer)
	}
	if len(f.git.created) != 0 {
		t.Fatalf("created %+v, want no intake workspace in the refusal's place", f.git.created)
	}
}

func TestAQuestionHandedToAnOpenIntakeHoldsOffItsRetirement(t *testing.T) {
	// Arrange
	f, _ := questionFixture(t)
	intake := addressee(t, f, "w-intake", "question-intake-1")
	f.db.cardAddressees = wsm.CardAddressees{OpenIntake: &intake.ID}
	v := f.mutable(t)
	heldDuringSend := false
	send := func(context.Context, wsm.Workspace) (*agentreplv1.SubmitPromptError, error) {
		heldDuringSend = !v.intakeHandoffs.TryLock()
		if !heldDuringSend {
			v.intakeHandoffs.Unlock()
		}
		return nil, nil
	}

	// Act
	_, err := f.verbs.AskAboutCard(context.Background(), questionCard, "why", send)

	// Assert
	if err != nil {
		t.Fatalf("AskAboutCard: %v", err)
	}
	if !heldDuringSend {
		t.Fatal("the question was sent without the handoff lock retirement takes")
	}
}

func TestARetirementJudgesQuietnessUnderTheHandoffLock(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := intakeWorkspace(t, f)
	v := f.mutable(t)
	heldDuringCheck := false
	v.deps.Freeness = func(ids.WorkspaceID) (Running, bool) {
		heldDuringCheck = !v.intakeHandoffs.TryLock()
		if !heldDuringCheck {
			v.intakeHandoffs.Unlock()
		}
		return Running{}, false
	}

	// Act
	err := f.verbs.CarryOutSelfClose(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CarryOutSelfClose: %v", err)
	}
	if !heldDuringCheck {
		t.Fatal("the intake's quietness was judged without the handoff lock a card's question takes")
	}
	if v.intakeHandoffs.TryLock() {
		v.intakeHandoffs.Unlock()
	} else {
		t.Fatal("the retirement kept the handoff lock after it returned")
	}
}

func TestAskAboutCardFails(t *testing.T) {
	tests := []struct {
		name      string
		asker     ids.WorkspaceID
		arrange   func(t *testing.T, f *fixture, sent *cardSend)
		operation string
	}{
		{name: "an unknown asker", asker: "w-nobody", arrange: func(*testing.T, *fixture, *cardSend) {}, operation: "daemon.refusal.typed"},
		{name: "git cannot name the home repository", asker: "w1", arrange: func(_ *testing.T, f *fixture, _ *cardSend) {
			f.git.commonDirErr = errors.New("fatal: not a git repository")
		}, operation: opIntake},
		{name: "the card's addressees cannot be read", asker: "w1", arrange: func(_ *testing.T, f *fixture, _ *cardSend) {
			f.db.cardAddresseesErr = errors.New("disk I/O error")
		}, operation: opIntake},
		{name: "the addressee's record cannot be read", asker: "w1", arrange: func(_ *testing.T, f *fixture, _ *cardSend) {
			gone := ids.WorkspaceID("w-gone")
			f.db.cardAddressees = wsm.CardAddressees{Work: &gone}
		}, operation: opIntake},
		{name: "the send to the addressee fails", asker: "w1", arrange: func(t *testing.T, f *fixture, sent *cardSend) {
			work := addressee(t, f, "w-work", "card-work")
			f.db.cardAddressees = wsm.CardAddressees{Work: &work.ID}
			sent.err = errors.New("the daemon is shutting down")
		}, operation: opIntake},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f, _ := questionFixture(t)
			sent := &cardSend{}
			tt.arrange(t, f, sent)
			card := questionCard
			card.Asker = tt.asker

			// Act
			_, err := f.verbs.AskAboutCard(context.Background(), card, "why", sent.send)

			// Assert
			if err == nil {
				t.Fatal("AskAboutCard succeeded")
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

// TestTheShippedQuestionIntakeBriefSendsReworkToTheWorkspaceThatMadeIt pins
// that a card question asking to rework a change goes to the workspace whose
// work made it, per the metaprompt's "Rework" section, and creates no
// workspace for it.
func TestTheShippedQuestionIntakeBriefSendsReworkToTheWorkspaceThatMadeIt(t *testing.T) {
	// Arrange.
	brief := loadShippedBrief(t, BriefQuestionIntake)

	// Act.
	body := brief.Body

	// Assert.
	assertCarries(t, "brief", body,
		`send the rework to the workspace that made it, as the metaprompt's "Rework" section says`,
		"Create no workspace for it.",
		"If it asks for any other change, create one async one-shot workspace",
	)
}
