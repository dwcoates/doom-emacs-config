package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
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
		{name: "no merge orchestrator", strip: func(d *Deps) { d.Merge = nil }},
		{name: "no rollout controller", strip: func(d *Deps) { d.Rollout = nil }},
		{name: "no feed resolver", strip: func(d *Deps) { d.Feed = nil }},
		{name: "no footer resolver", strip: func(d *Deps) { d.Footer = nil }},
		{name: "no topbar resolver", strip: func(d *Deps) { d.Topbar = nil }},
		{name: "no sidebar resolver", strip: func(d *Deps) { d.Sidebar = nil }},
		{name: "no holds resolver", strip: func(d *Deps) { d.Holds = nil }},
		{name: "no host relay", strip: func(d *Deps) { d.Host = nil }},
		{name: "no session fleet", strip: func(d *Deps) { d.Sessions = nil }},
		{name: "no shim resolver", strip: func(d *Deps) { d.Shim = nil }},
		{name: "no freeness probe", strip: func(d *Deps) { d.Freeness = nil }},
		{name: "no ownership probe", strip: func(d *Deps) { d.Ownership = nil }},
		{name: "no served-card store", strip: func(d *Deps) { d.Cards = nil }},
		{name: "no prompts directory", strip: func(d *Deps) { d.PromptsDir = "" }},
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

func TestRecordSurfacesAnUnresolvableWorkspaceSink(t *testing.T) {
	// Arrange: failing to resolve a KNOWN workspace's sink is an invariant
	// violation, never a reason to write globally.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.log.workspaceErr = errors.New("the symlink target is gone")

	// Act.
	err := f.verbs.Select(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Select() = nil error, want the unresolvable sink surfaced")
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatalf("Select() = %v, want a failure rather than a refusal", err)
	}
}

func TestPromptSubmissionCarriesTheCreationOrigin(t *testing.T) {
	// Arrange. Act.
	sub := promptSubmission("w1", "t1", SaidText("hello"))

	// Assert.
	if sub.WS != "w1" || sub.Turn != "t1" || sub.Target != nil {
		t.Fatalf("submission = %+v, want the workspace and turn with no bubble target", sub)
	}
}
