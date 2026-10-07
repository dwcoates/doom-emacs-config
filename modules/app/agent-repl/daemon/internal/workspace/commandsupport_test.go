package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// addSupportBrief arranges the brief RequestCommandSupport composes from.
func addSupportBrief(f *fixture) {
	f.briefs[BriefAddSupport] = prompts.Prompt{
		Name:         BriefAddSupport,
		Body:         "no support for /{{command}}; config lives at {{config_root}}.",
		Placeholders: []string{"command", "config_root"},
	}
}

// supportFixture arranges a workspace whose repository is a real main worktree,
// which is what the support workspace is created from.
func supportFixture(t *testing.T) (*fixture, string) {
	t.Helper()
	f := newFixture(t)
	repo := mainWorktree(t, f)
	ws := f.workspace("w1", t.TempDir())
	f.db.repositories = []wsm.Repository{{ID: ws.Repo, Dir: repo}}
	addSupportBrief(f)
	return f, repo
}

func TestRequestCommandSupportCreatesThroughTheOrdinaryForm(t *testing.T) {
	// Arrange.
	f, repo := supportFixture(t)

	// Act.
	created, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status")

	// Assert.
	if err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}
	if created.ID == "" {
		t.Fatal("RequestCommandSupport returned no workspace")
	}
	if len(f.git.created) != 1 || f.git.created[0].RepoDir != repo {
		t.Fatalf("created worktrees = %+v, want one in %q", f.git.created, repo)
	}
}

func TestRequestCommandSupportSplicesTheCommandAndConfigRoot(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}

	// Assert.
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(sent, "/status") || !strings.Contains(sent, "/config") {
		t.Fatalf("brief = %q, want the command and the config root spliced in", sent)
	}
}

func TestRequestCommandSupportStripsTheLeadingSlash(t *testing.T) {
	// Arrange: the brief writes the slash itself, so the command must not carry
	// a second one.
	f, _ := supportFixture(t)

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}

	// Assert.
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if strings.Contains(sent, "//status") {
		t.Fatalf("brief = %q, want a single leading slash", sent)
	}
}

func TestRequestCommandSupportNamesTheWorkspaceAfterTheCommand(t *testing.T) {
	// Arrange: a slug derived from a thousand-word brief would name the brief
	// rather than the command.
	f, _ := supportFixture(t)
	t.Setenv(PrefixEnv, "ABC")

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "ABC/support-status" {
		t.Fatalf("branch = %q, want ABC/support-status", f.git.created[0].Branch)
	}
}

func TestRequestCommandSupportRefusesABlankCommand(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)

	// Act.
	_, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "  /  ")

	// Assert.
	asRefusal(t, err, ArmBlankCommand)
}

func TestRequestCommandSupportIsLoudWhenTheBriefIsMissing(t *testing.T) {
	// Arrange: an agent with nothing to investigate would sit idle in a
	// worktree nobody asked for.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)

	// Act.
	_, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status")

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
	if len(f.git.created) != 0 {
		t.Fatalf("created worktrees = %+v, want none", f.git.created)
	}
}

func TestRequestCommandSupportIsLoudWhenTheBriefWillNotSplice(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	f.briefs[BriefAddSupport] = prompts.Prompt{
		Body: "{{comand}}", Placeholders: []string{"comand"},
	}

	// Act.
	_, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status")

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

func TestRequestCommandSupportRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)

	// Act.
	_, err := f.verbs.RequestCommandSupport(context.Background(), "nope", "/status")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

// TestRequestCommandSupportOpensADaemonFaultWhenTheBriefIsMissing pins that a
// brief_missing refusal is EVIDENCE ABOUT THE DAEMON, not one caller's bad
// luck: every composed brief reads the same directory, so DaemonHealth must
// answer unhealthy rather than healthy while it cannot furnish one.
func TestRequestCommandSupportOpensADaemonFaultWhenTheBriefIsMissing(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err == nil {
		t.Fatal("RequestCommandSupport with no brief = success, want the brief_missing refusal")
	}

	// Assert.
	if len(f.health.opened) != 1 {
		t.Fatalf("opened faults = %+v, want exactly one", f.health.opened)
	}
	if got := f.health.opened[0].Kind; got != health.KindPromptsDirMissing {
		t.Fatalf("fault kind = %q, want %q", got, health.KindPromptsDirMissing)
	}
}

// TestTheDaemonFaultNamesThePromptsDirectory pins the arm's own field: the
// operator reading DaemonFault.prompts_dir_missing gets the path.
func TestTheDaemonFaultNamesThePromptsDirectory(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err == nil {
		t.Fatal("RequestCommandSupport with no brief = success, want a refusal")
	}

	// Assert.
	if got := f.health.opened[0].Evidence["path"]; got != "/prompts" {
		t.Fatalf("fault evidence path = %q, want the prompts directory", got)
	}
}

// TestASecondBriefFailureDoesNotOpenASecondFault pins that at most one such
// fault stands: a duplicate row tells the operator nothing the first did not,
// and faults stay open until they are closed.
func TestASecondBriefFailureDoesNotOpenASecondFault(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err == nil {
		t.Fatal("the first RequestCommandSupport = success, want a refusal")
	}

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/mcp"); err == nil {
		t.Fatal("the second RequestCommandSupport = success, want a refusal")
	}

	// Assert.
	if len(f.health.opened) != 1 {
		t.Fatalf("opened faults = %d, want the standing one to be reused", len(f.health.opened))
	}
}

// TestABriefFailureStillOpensTheFaultWhenTheStandingReadFails pins that a
// failing fault read does not excuse leaving the fault unrecorded.
func TestABriefFailureStillOpensTheFaultWhenTheStandingReadFails(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)
	f.health.listErr = errors.New("the state client will not read")

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err == nil {
		t.Fatal("RequestCommandSupport with no brief = success, want a refusal")
	}

	// Assert.
	if len(f.health.opened) != 1 {
		t.Fatalf("opened faults = %+v, want the fault recorded anyway", f.health.opened)
	}
}

// TestTheBriefRefusalSurvivesAFailingFaultWrite pins that the caller's refusal
// is never replaced by the bookkeeping's failure: the fault is the operator's
// signal, the refusal is the caller's answer, and losing either would be worse.
func TestTheBriefRefusalSurvivesAFailingFaultWrite(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)
	f.health.openErr = errors.New("the state client will not write")

	// Act.
	_, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status")

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

// TestABriefThatReadsAgainClosesThePromptsDirectoryFault pins FAULT CLOSURE:
// the successful read+splice is the health probe, and the condition the fault
// records no longer holds.
func TestABriefThatReadsAgainClosesThePromptsDirectoryFault(t *testing.T) {
	// Arrange: a fault stands from an earlier failed read.
	f, _ := supportFixture(t)
	delete(f.briefs, BriefAddSupport)
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err == nil {
		t.Fatal("RequestCommandSupport with no brief = success, want the brief_missing refusal")
	}
	addSupportBrief(f)

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status2"); err != nil {
		t.Fatalf("RequestCommandSupport after the brief returned: %v", err)
	}

	// Assert.
	if len(f.health.closed) != 1 {
		t.Fatalf("closed faults = %v, want exactly the standing prompts-directory fault", f.health.closed)
	}
}

// TestAWorkspaceScopedFaultIsNotClosedByABriefReading pins that the closure is
// scoped to the DAEMON-scoped row raisePromptsFault opens: a workspace-scoped
// fault of the same kind is another party's record and is left alone.
func TestAWorkspaceScopedFaultIsNotClosedByABriefReading(t *testing.T) {
	// Arrange.
	f, _ := supportFixture(t)
	ws := ids.WorkspaceID("w1")
	if _, err := f.health.OpenFault(context.Background(), wsm.Fault{
		Kind:      health.KindPromptsDirMissing,
		Workspace: &ws,
	}); err != nil {
		t.Fatalf("arranging the workspace-scoped fault: %v", err)
	}

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}

	// Assert.
	if len(f.health.closed) != 0 {
		t.Fatalf("closed faults = %v, want none: the standing fault is workspace-scoped", f.health.closed)
	}
}
