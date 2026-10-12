package workspace

import (
	"context"
	"testing"

	"agent-repld/internal/account"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// mergeRequester arranges a requester a merge workspace can be forked from: a
// registered workspace on branch "ABC/sample", landing on "master" in
// targetDir, with a conversation to fork and a creation job carrying its
// merge actions.
func mergeRequester(t *testing.T, f *fixture) (wsm.Workspace, string) {
	t.Helper()
	requester := f.workspace("requester", t.TempDir())
	requester.ParentBranch = "master"
	f.db.with(requester)
	f.db.sessions[requester.ID] = wsm.Session{Workspace: requester.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/roots/default"}
	targetDir := t.TempDir()
	f.db.jobs[requester.ID] = wsm.CreationJob{
		Workspace: requester.ID,
		Layout:    wsm.MergeLayout{SourceBranch: requester.Branch, SourceDir: requester.Dir, TargetDir: targetDir},
		Actions:   wsm.MergeActions{Before: []string{"before the merge"}},
	}
	return requester, targetDir
}

// mergeWorkspaceSpec is the spec a detached merge asks Create for.
func mergeWorkspaceSpec(t *testing.T, f *fixture, requester ids.WorkspaceID) CreateSpec {
	t.Helper()
	return CreateSpec{
		RepoDir:  mainWorktree(t, f),
		BaseRef:  "ABC/sample",
		ForkFrom: &requester,
		MergeOf:  &wsm.MergeOrigin{Requester: requester, Request: "request-1"},
	}
}

func TestCreateNamesAMergeWorkspaceAfterTheBranchItMergesWithNoNamingCall(t *testing.T) {
	// Arrange
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")
	requester, _ := mergeRequester(t, f)

	// Act
	if _, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if len(f.git.created) != 1 || f.git.created[0].Branch != "ABC/sample-merge" {
		t.Fatalf("created = %+v, want branch ABC/sample-merge", f.git.created)
	}
	if len(f.headless.calls) != 0 {
		t.Fatalf("naming calls = %d, want none", len(f.headless.calls))
	}
}

func TestCreateDisambiguatesASecondMergeWorkspaceOfTheSameBranch(t *testing.T) {
	// Arrange: an earlier merge workspace's branch still stands.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")
	requester, _ := mergeRequester(t, f)
	f.git.existingBranches = map[string]bool{"ABC/sample-merge": true}

	// Act
	if _, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if len(f.git.created) != 1 || f.git.created[0].Branch != "ABC/sample-merge-2" {
		t.Fatalf("created = %+v, want branch ABC/sample-merge-2", f.git.created)
	}
}

func TestCreateLandsAMergeWorkspaceWhereItsRequesterWould(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, targetDir := mergeRequester(t, f)

	// Act
	if _, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the requester's target and actions on the job, its branch base
	// and parent branch on the registration, and the merge origin recorded.
	job := f.db.putJobs[len(f.db.putJobs)-1]
	if job.Layout.TargetDir != targetDir || len(job.Actions.Before) != 1 || job.Actions.Before[0] != "before the merge" || job.BaseRef != "ABC/sample" {
		t.Fatalf("job = %+v, want the requester's target, actions and branch as base", job)
	}
	facts := f.db.registered[len(f.db.registered)-1]
	want := wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}
	if facts.ParentBranch != "master" || facts.Parent == nil || *facts.Parent != requester.ID || facts.MergeOf == nil || *facts.MergeOf != want {
		t.Fatalf("registered = %+v, want parent branch master, parent and merge origin the requester's", facts)
	}
}

func TestCreateRefusesAContradictoryMergeWorkspaceSpecWithNothingBuilt(t *testing.T) {
	tests := []struct {
		name  string
		alter func(spec *CreateSpec)
	}{
		{name: "a prompt", alter: func(spec *CreateSpec) { spec.InitialPrompt = "do more" }},
		{name: "a name", alter: func(spec *CreateSpec) { spec.Name = "chosen" }},
		{name: "no fork", alter: func(spec *CreateSpec) { spec.ForkFrom = nil }},
		{name: "a one-shot", alter: func(spec *CreateSpec) { spec.OneShot = true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			requester, _ := mergeRequester(t, f)
			spec := mergeWorkspaceSpec(t, f, requester.ID)
			tt.alter(&spec)

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if err == nil || len(f.git.created) != 0 || len(f.db.putJobs) != 0 {
				t.Fatalf("Create = %v with %d worktrees and %d jobs, want an error with nothing built", err, len(f.git.created), len(f.db.putJobs))
			}
			if !loggedAtError(f, opCreate) {
				t.Fatal("the contradictory spec was not recorded at ERROR")
			}
		})
	}
}

func TestCreateFailsAMergeWorkspaceWhoseRequesterHasNoCreationJob(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	delete(f.db.jobs, requester.ID)

	// Act
	_, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID))

	// Assert
	if err == nil || len(f.git.created) != 0 {
		t.Fatalf("Create = %v with %d worktrees, want an error with nothing built", err, len(f.git.created))
	}
	if !loggedAtError(f, opCreate) {
		t.Fatal("the missing creation job was not recorded at ERROR")
	}
}

func TestCreateMergeWorkspaceForksInTheRequestersRepository(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	repoDir := mainWorktree(t, f)
	requester.Repo = f.db.repositories[len(f.db.repositories)-1].ID
	f.db.with(requester)
	origin := wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}

	// Act
	if _, err := f.verbs.CreateMergeWorkspace(context.Background(), requester, origin, "ABC/sample"); err != nil {
		t.Fatalf("CreateMergeWorkspace: %v", err)
	}

	// Assert: forked from the requester, cut from its branch, recorded as its
	// merge workspace.
	facts := f.db.registered[len(f.db.registered)-1]
	if facts.MergeOf == nil || *facts.MergeOf != origin || len(f.account.ported) != 1 {
		t.Fatalf("registered %+v with %d ported transcripts, want the requester's fork", facts, len(f.account.ported))
	}
	if job := f.db.putJobs[len(f.db.putJobs)-1]; job.BaseRef != "ABC/sample" {
		t.Fatalf("base = %q, want the requester's branch", job.BaseRef)
	}
	if len(f.git.created) != 1 || f.git.created[0].RepoDir != repoDir {
		t.Fatalf("created = %+v, want the worktree cut in the requester's repository", f.git.created)
	}
}

func TestCreateMergeWorkspaceFailsLoudlyForARequesterWithNoRepository(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	requester.Repo = "repo-unregistered"

	// Act
	_, err := f.verbs.CreateMergeWorkspace(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "ABC/sample")

	// Assert
	if err == nil || len(f.git.created) != 0 {
		t.Fatalf("CreateMergeWorkspace = %v with %d worktrees, want an error with nothing built", err, len(f.git.created))
	}
	if !loggedAtError(f, opCreate) {
		t.Fatal("the unresolved repository was not recorded at ERROR")
	}
}
