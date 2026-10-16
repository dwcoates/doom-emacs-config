package workspace

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

func TestNewRefusesEveryMissingCollaborator(t *testing.T) {
	// Arrange: a verb that silently skipped a missing collaborator would answer
	// success for work it never did, so construction refuses instead.
	full := func() Deps {
		f := newFixture(t)
		return f.mutable(t).deps
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "no state client", strip: func(d *Deps) { d.DB = nil }},
		{name: "no git client", strip: func(d *Deps) { d.Git = nil }},
		{name: "no account resolver", strip: func(d *Deps) { d.Accounts = nil }},
		{name: "no prompt queue", strip: func(d *Deps) { d.Queue = nil }},
		{name: "no revival-turn withdrawer", strip: func(d *Deps) { d.RevivalTurns = nil }},
		{name: "no merge orchestrator", strip: func(d *Deps) { d.Merge = nil }},
		{name: "no rollout controller", strip: func(d *Deps) { d.Rollout = nil }},
		{name: "no feed resolver", strip: func(d *Deps) { d.Feed = nil }},
		{name: "no footer resolver", strip: func(d *Deps) { d.Footer = nil }},
		{name: "no topbar resolver", strip: func(d *Deps) { d.Topbar = nil }},
		{name: "no sidebar resolver", strip: func(d *Deps) { d.Sidebar = nil }},
		{name: "no host relay", strip: func(d *Deps) { d.Host = nil }},
		{name: "no session fleet", strip: func(d *Deps) { d.Sessions = nil }},
		{name: "no shim resolver", strip: func(d *Deps) { d.Shim = nil }},
		{name: "no freeness probe", strip: func(d *Deps) { d.Freeness = nil }},
		{name: "no ownership probe", strip: func(d *Deps) { d.Ownership = nil }},
		{name: "no served-card store", strip: func(d *Deps) { d.Cards = nil }},
		{name: "no prompts directory", strip: func(d *Deps) { d.PromptsDir = "" }},
		{name: "no home directory", strip: func(d *Deps) { d.HomeDir = "" }},
		{name: "a relative home directory", strip: func(d *Deps) { d.HomeDir = "home/me" }},
		{name: "no log surfaces", strip: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := full()
			tt.strip(&deps)
			// Act.
			_, err := New(deps)
			// Assert.
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestRepublishRegistryCarriesEveryDurableHalf(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.tasks = []wsm.Task{{ID: "task-1", Title: "ship it"}}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	registry := f.sidebar.registries[len(f.sidebar.registries)-1]
	switch {
	case len(registry.Workspaces) != 1:
		t.Fatalf("registry workspaces = %d, want one", len(registry.Workspaces))
	case len(registry.Repositories) != 1:
		t.Fatalf("registry repositories = %d, want one", len(registry.Repositories))
	case len(registry.Tasks) != 1:
		t.Fatalf("registry tasks = %d, want one", len(registry.Tasks))
	case registry.Current == nil || *registry.Current != "w1":
		t.Fatalf("registry current = %v, want w1", registry.Current)
	}
}

func TestRecordRunsTheVerbWhenTheWorkspaceSinkCannotBeResolved(t *testing.T) {
	// Arrange: a registered workspace whose directory cannot host a sink —
	// a scratch path, or a worktree that has been deleted.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.log.workspaceErr = errors.New("the symlink target is gone")

	// Act.
	err := f.verbs.Select(context.Background(), "w1")

	// Assert: resolving a sink is total, so the verb answers on its own terms.
	if err != nil {
		t.Fatalf("Select() = %v, want the verb to run against the central sink", err)
	}
}

func TestRecordNamesTheWorkspaceOnCentrallyRoutedRecords(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	f.log.workspaceErr = errors.New("the symlink target is gone")

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: the record still says which workspace it is about.
	for _, rec := range f.log.logger.Records() {
		if rec.Context[dlog.KeyUnroutableWorkspace] == ws.Dir {
			return
		}
	}
	t.Fatalf("no record named the unroutable workspace %q", ws.Dir)
}

func TestPromptSubmissionCarriesTheCreationOrigin(t *testing.T) {
	// Arrange. Act.
	sub := promptSubmission("w1", "t1", SaidText("hello"))

	// Assert.
	if sub.WS != "w1" || sub.Turn != "t1" || sub.Target != nil {
		t.Fatalf("submission = %+v, want the workspace and turn with no bubble target", sub)
	}
}

// TestARepublishLevelsACancelledRosterReadAtInfo pins the level of a roster
// read abandoned by the daemon's own exit. The serving context is cancelled
// under whatever is in flight, and a roster nobody is left to receive is no
// loss -- while every other refusal of the state client keeps its ERROR.
func TestARepublishLevelsACancelledRosterReadAtInfo(t *testing.T) {
	tests := []struct {
		name      string
		listErr   error
		wantLevel string
	}{
		{name: "the read was cancelled", listErr: context.Canceled, wantLevel: "info"},
		{name: "the state client refused", listErr: errors.New("the database is gone"), wantLevel: "error"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.listWorkspacesErr = tt.listErr

			// Act.
			if err := f.verbs.Select(context.Background(), "w1"); err != nil {
				t.Fatalf("Select: %v", err)
			}

			// Assert.
			var level string
			for _, r := range f.log.logger.Records() {
				if strings.Contains(r.Message, "the workspaces for the roster") ||
					strings.Contains(r.Message, "roster read ended") {
					level = r.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("the roster-read record is %q, want %q", level, tt.wantLevel)
			}
		})
	}
}

func TestRepublishRegistryLogsItsCompletionAtInfo(t *testing.T) {
	// Arrange: a verb whose only roster publish is republishRegistry's success.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: a roster republish is a production-visible mutation, so its
	// completion stands at info rather than debug.
	var level string
	for _, r := range f.log.logger.Records() {
		if r.Message == "republished the roster registry" {
			level = r.Level
		}
	}
	if level != "info" {
		t.Fatalf("the roster republish completion is %q, want info", level)
	}
}

func TestDeferredBriefIsDeferred(t *testing.T) {
	// Arrange, Act.
	got := deferredBrief("w1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if got.Delivery != wsm.DeliveryDeferred {
		t.Fatalf("delivery = %v, want deferred", got.Delivery)
	}
}

func TestDeferredBriefCarriesItsWorkspaceOriginAndText(t *testing.T) {
	// Arrange, Act.
	got := deferredBrief("w1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if got.WS != "w1" || got.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT || submittedText(got.Said) != "the brief" {
		t.Fatalf("submission = %+v, want w1's MERGE_REVERT brief saying the brief", got)
	}
}

func TestDeferredBriefMintsAFreshTurnEachTime(t *testing.T) {
	// Arrange, Act.
	first := deferredBrief("w1", "a", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)
	second := deferredBrief("w1", "a", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT)

	// Assert.
	if first.Turn == "" || first.Turn == second.Turn {
		t.Fatalf("turns = %q, %q, want two distinct minted turns", first.Turn, second.Turn)
	}
}

// TestOnlyDeferredBriefDefersASubmission pins deferredBrief as the one place
// this package composes a deferred submission: a verb hand-rolling one would
// be free to drift from it.
func TestOnlyDeferredBriefDefersASubmission(t *testing.T) {
	// Arrange, Act.
	got := functionsReferencing(t, "DeliveryDeferred")

	// Assert.
	if !slices.Equal(got, []string{"deferredBrief"}) {
		t.Fatalf("functions naming DeliveryDeferred = %v, want only deferredBrief", got)
	}
}

// functionsReferencing names, sorted and once each, every function of this
// package's production sources whose body names the identifier NAME, bare or
// as a selector's field.
func functionsReferencing(t *testing.T, name string) []string {
	t.Helper()
	files := token.NewFileSet()
	var found []string
	for _, file := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(files, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(node ast.Node) bool {
				if ident, ok := node.(*ast.Ident); ok && ident.Name == name && !slices.Contains(found, fn.Name.Name) {
					found = append(found, fn.Name.Name)
				}
				return true
			})
		}
	}
	slices.Sort(found)
	return found
}
