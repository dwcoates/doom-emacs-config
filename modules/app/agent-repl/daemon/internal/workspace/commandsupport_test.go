package workspace

import (
	"context"
	"strings"
	"testing"

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
	repo := mainWorktree(t)
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
	t.Setenv(PrefixEnv, "DWC")

	// Act.
	if _, err := f.verbs.RequestCommandSupport(context.Background(), "w1", "/status"); err != nil {
		t.Fatalf("RequestCommandSupport: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "DWC/support-status" {
		t.Fatalf("branch = %q, want DWC/support-status", f.git.created[0].Branch)
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
