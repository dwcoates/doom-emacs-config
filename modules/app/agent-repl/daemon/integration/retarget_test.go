//go:build integration

package integration

import (
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
)

// retarget_test.go exercises SetWorkspaceParent and the command file's
// `retarget` entry: the roster's nesting moves, the workspace's session is
// told in the same operation, and the merge lands at the new target.

// retargetFamily is a repository with a parent workspace and a child created
// under it, each with its fake shim brought up by the create.
type retargetFamily struct {
	d      *harness.Daemon
	repo   *harness.Repo
	parent *workspacev1.WorkspaceRef
	child  *workspacev1.WorkspaceRef
	roster *harness.Stream[*frontendv1.WorkspaceRoster]
}

// newRetargetFamily creates a parent workspace and a child of it.
func newRetargetFamily(t *testing.T, opts harness.Opts, repo *harness.Repo) *retargetFamily {
	t.Helper()
	d := newDaemon(t, opts)
	repository := createRepositoryRef(t, d, repo)
	create := func(parent *workspacev1.WorkspaceRef) *workspacev1.WorkspaceRef {
		req := &agentreplv1.CreateWorkspaceRequest{
			Repository:    repository,
			Requester:     harness.User(),
			ContextOrigin: harness.Fresh(),
			Form:          &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: harness.Primary()}},
		}
		if parent != nil {
			req.GetStandard().Topology = harness.ChildOf(parent, "the child's work")
		}
		resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(req))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Fatalf("CreateWorkspace(parent=%v) = (%v, %v), want a success", parent, resp, err)
		}
		return resp.Msg.GetSuccess().GetWorkspace()
	}
	f := &retargetFamily{d: d, repo: repo}
	f.parent = create(nil)
	f.child = create(f.parent)
	f.roster = d.WatchRoster()
	awaitRoster(t, d, f.roster, "the child nested under its parent", f.nested)
	return f
}

// nested reports whether the roster draws the child under the parent.
func (f *retargetFamily) nested(r *frontendv1.WorkspaceRoster) bool {
	return rosterNests(r, f.parent.GetId(), f.child.GetId())
}

// topLevel reports whether the roster draws the child as a row of its own.
func (f *retargetFamily) topLevel(r *frontendv1.WorkspaceRoster) bool {
	return rosterRepoRow(r, f.child.GetId()) != nil && !f.nested(r)
}

// expectNotice awaits the retarget's notice on the child's session and
// answers its text.
func (f *retargetFamily) expectNotice(t *testing.T) string {
	t.Helper()
	req := f.d.Shim(f.child).ExpectStartTurn()
	if req.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED {
		t.Fatalf("StartTurn.origin = %v, want PROMPT_ORIGIN_WORKSPACE_RETARGETED", req.GetOrigin())
	}
	return text(req.GetSaid())
}

func TestSetWorkspaceParentToTopLevelUnnestsTellsTheSessionAndMergesToTheMainCheckout(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon's own checkout IS this repository and a test-all
	// script stands in for the gate, because the emacs-repo method is the one
	// that runs `git merge`, so where the merge lands is read off git's -C.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	f := newRetargetFamily(t, harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)},
		Timeout:  harness.MergeChainTimeout,
	}, repo)

	// Act
	resp, err := f.d.Client().SetWorkspaceParent(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{
		Workspace: f.child,
		Parent:    &agentreplv1.SetWorkspaceParentRequest_TopLevel{TopLevel: &agentreplv1.SetWorkspaceParentTopLevel{}},
	}))

	// Assert: answered, unnested, and told.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetWorkspaceParent = (%v, %v), want a success", resp, err)
	}
	awaitRoster(t, f.d, f.roster, "the child at the top level", f.topLevel)
	notice := f.expectNotice(t)
	if !strings.Contains(notice, "main checkout at `"+repo.Dir+"`") || !strings.Contains(notice, "`/merge-queue one-shot`") {
		t.Fatalf("notice = %q, want the main checkout named and /merge-queue one-shot", notice)
	}
	pushConcludedTurn(f.d.Shim(f.child), mainAgent, "notice-read")

	// Assert: the child's own-branch merge now lands in the main checkout.
	harness.CommitWork(t, f.child.GetDir())
	if _, err := f.d.Client().MergeWorkspace(f.d.Ctx(), connect.NewRequest(harness.MergeRequest(f.child, harness.OneShot()))); err != nil {
		t.Fatalf("MergeWorkspace(child) = error %v, want the merge enqueued", err)
	}
	awaitRoster(t, f.d, f.roster, "the child's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.child.GetId())
		return row != nil && row.GetMerged() != nil
	})
	for _, c := range f.d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == f.parent.GetDir() {
			t.Fatalf("git calls = %v, want no merge inside the former parent's worktree %q", f.d.Git.Calls(), f.parent.GetDir())
		}
	}
	landedInMain := false
	for _, c := range f.d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == repo.Dir {
			landedInMain = true
		}
	}
	if !landedInMain {
		t.Fatalf("git calls = %v, want a merge run inside the main checkout %q", f.d.Git.Calls(), repo.Dir)
	}
}

func TestRosterNestsReadsOnlyTheParentsChildren(t *testing.T) {
	row := func(id string, children ...*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
		return &frontendv1.RosterOpenRow{
			Row:      &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}}},
			Children: children,
		}
	}
	roster := &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{
		Sections: []*frontendv1.RosterRepoSection{{OpenRows: &frontendv1.RosterOpenRows{Rows: []*frontendv1.RosterOpenRow{
			row("p", row("c")), row("top"),
		}}}},
	}}
	tests := []struct {
		name          string
		parent, child string
		want          bool
	}{
		{name: "a child under its parent", parent: "p", child: "c", want: true},
		{name: "a top-level row under no parent", parent: "p", child: "top"},
		{name: "a parent the roster does not draw", parent: "gone", child: "c"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := rosterNests(roster, tt.parent, tt.child)

			// Assert
			if got != tt.want {
				t.Fatalf("rosterNests(%s, %s) = %v, want %v", tt.parent, tt.child, got, tt.want)
			}
		})
	}
}
