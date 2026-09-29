package commandfile

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// skillCreate is VERBATIM what the /create-or-update-workspace skill's
// `run.sh --emit-commands --dry-run` wrote for a create carrying every field it
// has (2026-09-29), with the paths moved onto the fixture's repository. Before
// this reader declared them, all seven fields after `prompt` were dropped
// without a word.
const skillCreate = `[{"type": "create", "name": "probe", "git_root": "/repo", ` +
	`"source_ws": {"name": "master", "path": "/repo"}, "base_commit": "origin/master", ` +
	`"priority": "p1", "model": "sonnet", "prompt": "hi", ` +
	`"before_ws_merge": "b", "postprocessing_prompt": "p"}]`

// createFixture is a fixture holding repository r1 at /repo with one open
// workspace, w1 ("feature-one"), at /worktrees/feature-one.
func createFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.repository("r1", "/repo")
	f.member("w1", "r1", "feature-one", "/worktrees/feature-one")
	return f
}

// appliedSpec applies one command file and answers the one create it made.
func appliedSpec(t *testing.T, f *fixture, body string) workspace.CreateSpec {
	t.Helper()
	path := f.write(t, "workspace_commands_create.json", body)
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].Verb != "create" {
		t.Fatalf("verb calls = %v, want one create", verbNames(f.verbs.calls))
	}
	return f.verbs.calls[0].Spec
}

// refusedCause applies one command file that must be refused, asserts nothing
// was created and the file was quarantined, and answers the warning's cause.
func refusedCause(t *testing.T, f *fixture, body string) string {
	t.Helper()
	path := f.write(t, "workspace_commands_refused.json", body)
	if err := f.ingress.ApplyFile(context.Background(), path); !errors.Is(err, ErrQuarantined) {
		t.Fatalf("ApplyFile = %v, want ErrQuarantined", err)
	}
	if len(f.verbs.calls) != 0 {
		t.Fatalf("verb calls = %v, want no create", verbNames(f.verbs.calls))
	}
	var cause string
	for _, record := range f.log.logger.Records() {
		if record.Operation == opQuarantine && record.Level == "warn" {
			cause, _ = record.Context["cause"].(string)
		}
	}
	if cause == "" {
		t.Fatalf("records = %v, want a quarantine warning with a cause", f.log.logger.Records())
	}
	return cause
}

func TestTheSkillsCreateMapsEveryFieldOntoTheSpec(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, skillCreate)

	// Assert.
	p1 := wsm.PriorityP1
	want := workspace.CreateSpec{
		RepoDir: "/repo", InitialPrompt: "hi", Name: "probe", BaseRef: "origin/master",
		Model: "sonnet", Priority: &p1,
		MergeActions: wsm.MergeActions{Before: []string{"b"}, After: []string{"p"}},
	}
	if !reflect.DeepEqual(spec, want) {
		t.Fatalf("create spec = %+v, want %+v", spec, want)
	}
}

func TestACreateMapsEachPriority(t *testing.T) {
	tests := []struct {
		priority string
		want     wsm.Priority
	}{
		{"p05", wsm.PriorityP05}, {"p1", wsm.PriorityP1}, {"p2", wsm.PriorityP2}, {"p3", wsm.PriorityP3},
	}
	for _, tt := range tests {
		t.Run(tt.priority, func(t *testing.T) {
			// Arrange.
			f := createFixture(t)

			// Act.
			spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","priority":"`+tt.priority+`"}]`)

			// Assert.
			if spec.Priority == nil || *spec.Priority != tt.want {
				t.Fatalf("priority = %v, want %v", spec.Priority, tt.want)
			}
		})
	}
}

func TestACreateWithoutAPriorityLeavesItUnset(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo"}]`)

	// Assert.
	if spec.Priority != nil {
		t.Fatalf("priority = %v, want unset", *spec.Priority)
	}
}

func TestABlankModelIsTheDefault(t *testing.T) {
	// Arrange: the skill says a blank model is normalized away downstream.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","model":"   "}]`)

	// Assert.
	if spec.Model != "" {
		t.Fatalf("model = %q, want the default", spec.Model)
	}
}

func TestTheOlderBaseRefSpellingStillNamesTheBase(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","base_ref":"HEAD"}]`)

	// Assert.
	if spec.BaseRef != "HEAD" {
		t.Fatalf("base = %q, want HEAD", spec.BaseRef)
	}
}

func TestASourceThatIsTheMainCheckoutIsNoParent(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","source_ws":{"name":"master","path":"/repo"}}]`)

	// Assert.
	if spec.Parent != nil || spec.ForkFrom != nil {
		t.Fatalf("parent = %v, fork = %v, want neither", spec.Parent, spec.ForkFrom)
	}
}

func TestASourceWorkspaceIsTheParent(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo",`+
		`"source_ws":{"name":"feature-one","path":"/worktrees/feature-one"}}]`)

	// Assert.
	if spec.Parent == nil || *spec.Parent != "w1" || spec.ForkFrom != nil {
		t.Fatalf("parent = %v, fork = %v, want parent w1 and no fork", spec.Parent, spec.ForkFrom)
	}
}

// TestAWorktreeGitRootIsItsRepository pins the skill's default: `git_root` is
// the source workspace's own path, a worktree for any workspace but the main
// checkout, and the create is cut from its repository.
func TestAWorktreeGitRootIsItsRepository(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/worktrees/feature-one",`+
		`"source_ws":{"name":"feature-one","path":"/worktrees/feature-one"}}]`)

	// Assert.
	if spec.RepoDir != "/repo" || spec.Parent == nil || *spec.Parent != "w1" {
		t.Fatalf("repo = %q, parent = %v, want /repo and parent w1", spec.RepoDir, spec.Parent)
	}
}

// TestAnUnregisteredGitRootIsLeftForTheVerbToRefuse pins that the ingress
// never mints or guesses a repository: the verb refuses it as
// unknown_repository with its own record.
func TestAnUnregisteredGitRootIsLeftForTheVerbToRefuse(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/elsewhere",`+
		`"source_ws":{"name":"x","path":"/elsewhere"}}]`)

	// Assert.
	if spec.RepoDir != "/elsewhere" || spec.Parent != nil {
		t.Fatalf("repo = %q, parent = %v, want /elsewhere as written and no parent", spec.RepoDir, spec.Parent)
	}
}

func TestAnUnregisteredSourceIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo",`+
		`"source_ws":{"name":"gone","path":"/worktrees/gone"}}]`)

	// Assert.
	if !strings.Contains(cause, `source_ws.path "/worktrees/gone" is neither repository "/repo"'s main checkout nor a registered workspace`) {
		t.Fatalf("cause = %q, want the unregistered source named", cause)
	}
}

func TestASourceInAnotherRepositoryIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.repository("r2", "/other")
	f.member("w9", "r2", "elsewhere", "/worktrees/elsewhere")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo",`+
		`"source_ws":{"name":"elsewhere","path":"/worktrees/elsewhere"}}]`)

	// Assert.
	if !strings.Contains(cause, `is workspace "w9" of repository "r2", not of the create's repository "r1"`) {
		t.Fatalf("cause = %q, want the repository mismatch named", cause)
	}
}

func TestAForkIsFromTheNamedWorkspaceAndIsItsParent(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one"}]`)

	// Assert.
	if spec.ForkFrom == nil || *spec.ForkFrom != "w1" || spec.Parent == nil || *spec.Parent != "w1" {
		t.Fatalf("fork = %v, parent = %v, want both w1", spec.ForkFrom, spec.Parent)
	}
}

func TestAForkWhoseSourceIsTheSameWorkspaceIsAccepted(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one",`+
		`"source_ws":{"name":"feature-one","path":"/worktrees/feature-one"}}]`)

	// Assert.
	if spec.ForkFrom == nil || *spec.ForkFrom != "w1" || spec.Parent == nil || *spec.Parent != "w1" {
		t.Fatalf("fork = %v, parent = %v, want both w1", spec.ForkFrom, spec.Parent)
	}
}

func TestAForkFromTheMainCheckoutSourceIsFromTheForkedWorkspace(t *testing.T) {
	// Arrange: a source that is the main checkout names no parent, so the
	// fork's own workspace is the only one.
	f := createFixture(t)

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one",`+
		`"source_ws":{"name":"master","path":"/repo"}}]`)

	// Assert.
	if spec.Parent == nil || *spec.Parent != "w1" {
		t.Fatalf("parent = %v, want w1", spec.Parent)
	}
}

func TestAForkWhoseSourceIsAnotherWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.member("w2", "r1", "feature-two", "/worktrees/feature-two")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one",`+
		`"source_ws":{"name":"feature-two","path":"/worktrees/feature-two"}}]`)

	// Assert.
	if !strings.Contains(cause, `fork_from "feature-one" is workspace "w1", but source_ws names workspace "w2"`) {
		t.Fatalf("cause = %q, want the two parents named", cause)
	}
}

func TestAForkOfNoOpenWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"nobody"}]`)

	// Assert.
	if !strings.Contains(cause, `fork_from "nobody" names no open workspace of repository "r1"`) {
		t.Fatalf("cause = %q, want the missing fork named", cause)
	}
}

func TestAForkNeverResolvesAClosedWorkspace(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	closed := f.db.byDir["/worktrees/feature-one"]
	closed.Closed = true
	f.db.byDir["/worktrees/feature-one"] = closed

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one"}]`)

	// Assert.
	if !strings.Contains(cause, "names no open workspace") {
		t.Fatalf("cause = %q, want the closed workspace unmatched", cause)
	}
}

func TestAForkOfAnAmbiguousNameIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.member("w2", "r1", "feature-one", "/worktrees/feature-one-again")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one"}]`)

	// Assert.
	if !strings.Contains(cause, `fork_from "feature-one" names 2 open workspaces`) {
		t.Fatalf("cause = %q, want the ambiguity named", cause)
	}
}

func TestAForkNeverResolvesAWorkspaceOfAnotherRepository(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.repository("r2", "/other")
	f.member("w9", "r2", "elsewhere", "/worktrees/elsewhere")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"elsewhere"}]`)

	// Assert.
	if !strings.Contains(cause, `fork_from "elsewhere" names no open workspace of repository "r1"`) {
		t.Fatalf("cause = %q, want the other repository's workspace unmatched", cause)
	}
}

func TestAnUnreadableRegistryRefusesTheCreate(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.db.listErr = errors.New("database is locked")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/repo","fork_from":"feature-one"}]`)

	// Assert.
	if !strings.Contains(cause, "read the repository registry: database is locked") {
		t.Fatalf("cause = %q, want the registry failure named", cause)
	}
}

func TestACreateNamingASourceNeedsAStateClient(t *testing.T) {
	// Arrange: an ingress built without a state client.
	f := createFixture(t)
	ing := f.ingress.(*ingress)
	ing.deps.DB = nil

	// Act.
	_, err := ing.createSpec(context.Background(), Entry{
		Type: TypeCreate, Name: "n", GitRoot: "/repo", SourceWS: &SourceWorkspace{Path: "/repo"},
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no state client") {
		t.Fatalf("createSpec = %v, want the missing state client named", err)
	}
}

func TestACreateNamingNoSourceNeedsNoRegistry(t *testing.T) {
	// Arrange: a registry that cannot be read is never asked.
	f := createFixture(t)
	f.db.listErr = errors.New("database is locked")

	// Act.
	spec := appliedSpec(t, f, `[{"type":"create","name":"n","git_root":"/repo"}]`)

	// Assert.
	if spec.RepoDir != "/repo" {
		t.Fatalf("repo = %q, want /repo", spec.RepoDir)
	}
}

// TestAWorkspaceWhoseRepositoryIsUnregisteredIsRefused pins the registry
// invariant: a workspace names a repository the registry holds.
func TestAWorkspaceWhoseRepositoryIsUnregisteredIsRefused(t *testing.T) {
	// Arrange.
	f := createFixture(t)
	f.member("w3", ids.RepoID("r-gone"), "orphan", "/worktrees/orphan")

	// Act.
	cause := refusedCause(t, f, `[{"type":"create","name":"n","git_root":"/worktrees/orphan",`+
		`"source_ws":{"name":"orphan","path":"/worktrees/orphan"}}]`)

	// Assert.
	if !strings.Contains(cause, `names repository "r-gone", which the registry does not hold`) {
		t.Fatalf("cause = %q, want the missing repository named", cause)
	}
}
