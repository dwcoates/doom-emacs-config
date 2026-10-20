package workspace

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// mergeRequester arranges a requester a merge workspace can be forked from: a
// registered workspace on branch "ABC/sample", landing on "master" in
// targetDir, with a conversation to fork and a creation job carrying merge
// actions of ITS OWN, which no merge workspace of it may inherit.
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
		Actions:   wsm.MergeActions{Before: []string{"the requester's own before"}},
	}
	return requester, targetDir
}

// theLanding is the landing the merge queue decided at hand-off, distinct
// from everything the requester records, so a create that read the requester
// instead is caught.
var theLanding = wsm.MergeOrigin{TargetDir: "/decided/target", ParentBranch: "decided-parent",
	Actions: wsm.MergeActions{Before: []string{"the request's before"}, After: []string{"the request's after"}}}

// landedOrigin is the hand-off's origin for REQUESTER's request-1, carrying
// theLanding.
func landedOrigin(requester ids.WorkspaceID) wsm.MergeOrigin {
	origin := theLanding
	origin.Requester, origin.Request = requester, "request-1"
	return origin
}

// mergeWorkspaceSpec is the spec an async merge asks Create for, named
// "reconnect-fix", landing by theLanding.
func mergeWorkspaceSpec(t *testing.T, f *fixture, requester ids.WorkspaceID) CreateSpec {
	t.Helper()
	origin := landedOrigin(requester)
	origin.ForkHead = "fork-head"
	return CreateSpec{
		RepoDir:   mainWorktree(t, f),
		BaseRef:   "ABC/sample",
		ForkFrom:  &requester,
		MergeOf:   &origin,
		MergeName: "reconnect-fix",
	}
}

func TestCreateNamesAMergeWorkspaceAsItsRequestNamedItWithNoNamingCall(t *testing.T) {
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
	if len(f.git.created) != 1 || f.git.created[0].Branch != "ABC/reconnect-fix" {
		t.Fatalf("created = %+v, want branch ABC/reconnect-fix", f.git.created)
	}
	if len(f.headless.calls) != 0 {
		t.Fatalf("naming calls = %d, want none", len(f.headless.calls))
	}
}

func TestCreateTakesTheNameAMergeWorkspacesOwnRequestReserved(t *testing.T) {
	// Arrange: the requester's request still holds the reservation.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")
	requester, _ := mergeRequester(t, f)
	f.db.reserved = map[string]ids.WorkspaceID{"reconnect-fix": requester.ID}

	// Act
	_, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID))

	// Assert
	if err != nil || len(f.git.created) != 1 || f.git.created[0].Branch != "ABC/reconnect-fix" {
		t.Fatalf("Create = %v, created %+v; want the reserved name taken by its own fork", err, f.git.created)
	}
}

func TestCreateFailsAMergeWorkspaceWithNoNameWithNothingBuilt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	spec := mergeWorkspaceSpec(t, f, requester.ID)
	spec.MergeName = ""

	// Act
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if err == nil || len(f.git.created) != 0 {
		t.Fatalf("Create = %v with %d worktrees, want an error with nothing built", err, len(f.git.created))
	}
	if !loggedAtError(f, opCreate) {
		t.Fatal("the nameless merge workspace was not recorded at ERROR")
	}
}

func TestCreateLandsAMergeWorkspaceByItsOriginsLanding(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	spec := mergeWorkspaceSpec(t, f, requester.ID)

	// Act
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the origin's target and actions on the merge workspace's own
	// job, the requester's branch as its base, the origin's parent branch on
	// the registration, and the merge origin recorded.
	job := f.db.putJobs[len(f.db.putJobs)-1]
	if job.Layout.TargetDir != theLanding.TargetDir || !reflect.DeepEqual(job.Actions, theLanding.Actions) || job.BaseRef != "ABC/sample" {
		t.Fatalf("job = %+v, want the origin's target and actions and the requester's branch as base", job)
	}
	facts := f.db.registered[len(f.db.registered)-1]
	if facts.ParentBranch != theLanding.ParentBranch || facts.Parent == nil || *facts.Parent != requester.ID || facts.MergeOf == nil || !reflect.DeepEqual(*facts.MergeOf, *spec.MergeOf) {
		t.Fatalf("registered = %+v, want the origin's parent branch, the requester as parent and the merge origin", facts)
	}
}

func TestCreateGivesAMergeWorkspaceNoActionWhenItsRequestNamedNone(t *testing.T) {
	// Arrange: the requester's creation job carries actions of its own.
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	spec := mergeWorkspaceSpec(t, f, requester.ID)
	spec.MergeOf.Actions = wsm.MergeActions{}

	// Act
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if job := f.db.putJobs[len(f.db.putJobs)-1]; !reflect.DeepEqual(job.Actions, wsm.MergeActions{}) {
		t.Fatalf("job actions = %+v, want none: a merge workspace never inherits its requester's", job.Actions)
	}
}

func TestCreateRecordsTheActionsAMergeWorkspacesRequestNamed(t *testing.T) {
	// Arrange: an async request that names a prompt to run before its merge
	// and one after it lands.
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	spec := mergeWorkspaceSpec(t, f, requester.ID)
	named := wsm.MergeActions{Before: []string{"tidy the branch"}, After: []string{"announce the landing"}}
	spec.MergeOf.Actions = named

	// Act
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if job := f.db.putJobs[len(f.db.putJobs)-1]; !reflect.DeepEqual(job.Actions, named) {
		t.Fatalf("job actions = %+v, want the request's own %+v", job.Actions, named)
	}
}

func TestCreateFailsAMergeWorkspaceWhoseOriginCarriesNoLandingWithNothingBuilt(t *testing.T) {
	tests := []struct {
		name  string
		alter func(origin *wsm.MergeOrigin)
		field string
	}{
		{name: "no target", alter: func(origin *wsm.MergeOrigin) { origin.TargetDir = "" }, field: "target_dir"},
		{name: "no parent branch", alter: func(origin *wsm.MergeOrigin) { origin.ParentBranch = "" }, field: "parent_branch"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			requester, _ := mergeRequester(t, f)
			spec := mergeWorkspaceSpec(t, f, requester.ID)
			tt.alter(spec.MergeOf)

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if err == nil || len(f.git.created) != 0 || len(f.db.putJobs) != 0 || len(f.db.registered) != 0 {
				t.Fatalf("Create = %v with %d worktrees, %d jobs and %d registrations, want an error with nothing built",
					err, len(f.git.created), len(f.db.putJobs), len(f.db.registered))
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opCreate, Message: "a merge workspace's origin carries no landing"})
			if !ok {
				t.Fatal("the landingless origin was not recorded at ERROR")
			}
			if record.Context["requester"] != string(requester.ID) || record.Context["request"] != "request-1" || record.Context[tt.field] != "" {
				t.Fatalf("record context = %+v, want the requester, the request and the empty %s", record.Context, tt.field)
			}
		})
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
		{name: "no fork head", alter: func(spec *CreateSpec) { spec.MergeOf.ForkHead = "" }},
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

func TestCreateForksAMergeWorkspaceFromARequesterWithNoCreationJob(t *testing.T) {
	// Arrange: the main checkout, registered and never created.
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	delete(f.db.jobs, requester.ID)

	// Act
	_, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID))

	// Assert
	if err != nil || len(f.git.created) != 1 {
		t.Fatalf("Create = %v with %d worktrees, want the merge workspace forked", err, len(f.git.created))
	}
	if job := f.db.putJobs[len(f.db.putJobs)-1]; job.Layout.TargetDir != theLanding.TargetDir {
		t.Fatalf("job target = %q, want the origin's %q", job.Layout.TargetDir, theLanding.TargetDir)
	}
}

func TestCreateMergeWorkspaceForksInTheRequestersRepository(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	repoDir := mainWorktree(t, f)
	requester.Repo = f.db.repositories[len(f.db.repositories)-1].ID
	f.db.with(requester)
	origin := landedOrigin(requester.ID)

	// Act
	created, err := f.verbs.CreateMergeWorkspace(context.Background(), requester, origin, "reconnect-fix", "ABC/sample")
	if err != nil {
		t.Fatalf("CreateMergeWorkspace: %v", err)
	}

	// Assert: forked from the requester under the name its request reserved,
	// cut at its branch's tip, recorded as its merge workspace with that tip as
	// its fork head.
	facts := f.db.registered[len(f.db.registered)-1]
	want := landedOrigin(requester.ID)
	want.ForkHead = "sha-of-refs/heads/ABC/sample"
	if facts.MergeOf == nil || !reflect.DeepEqual(*facts.MergeOf, want) {
		t.Fatalf("registered %+v, want the requester's merge workspace", facts)
	}
	if fork, owed := f.db.pendingForks[created.ID]; !owed || fork.Parent != requester.ID {
		t.Fatalf("pending forks = %+v, want the requester's conversation owed", f.db.pendingForks)
	}
	if job := f.db.putJobs[len(f.db.putJobs)-1]; job.BaseRef != "ABC/sample" {
		t.Fatalf("base = %q, want the requester's branch", job.BaseRef)
	}
	if len(f.git.created) != 1 || f.git.created[0].RepoDir != repoDir || f.git.created[0].BaseRef != want.ForkHead {
		t.Fatalf("created = %+v, want the worktree cut at the fork head in the requester's repository", f.git.created)
	}
}

func TestCreateCutsAMergeWorkspaceAtItsForkHeadAndKeepsTheBranchAsItsBase(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)

	// Act
	if _, err := f.verbs.Create(context.Background(), mergeWorkspaceSpec(t, f, requester.ID)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if len(f.git.created) != 1 || f.git.created[0].BaseRef != "fork-head" {
		t.Fatalf("created = %+v, want the worktree cut at the fork head", f.git.created)
	}
	if job := f.db.putJobs[len(f.db.putJobs)-1]; job.BaseRef != "ABC/sample" {
		t.Fatalf("base = %q, want the requester's branch", job.BaseRef)
	}
}

func TestCreateMergeWorkspaceFailsLoudlyWhenTheBranchDoesNotResolve(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	mainWorktree(t, f)
	requester.Repo = f.db.repositories[len(f.db.repositories)-1].ID
	f.db.with(requester)
	f.git.resolveErr = errors.New("fatal: not a valid ref")

	// Act
	_, err := f.verbs.CreateMergeWorkspace(context.Background(), requester, landedOrigin(requester.ID), "reconnect-fix", "ABC/sample")

	// Assert
	if err == nil || len(f.git.created) != 0 || len(f.db.putJobs) != 0 {
		t.Fatalf("CreateMergeWorkspace = %v with %d worktrees and %d jobs, want an error with nothing built", err, len(f.git.created), len(f.db.putJobs))
	}
	if !loggedAtError(f, opCreate) {
		t.Fatal("the unresolved branch was not recorded at ERROR")
	}
}

func TestCreateMergeWorkspaceFailsLoudlyForARequesterWithNoRepository(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester, _ := mergeRequester(t, f)
	requester.Repo = "repo-unregistered"

	// Act
	_, err := f.verbs.CreateMergeWorkspace(context.Background(), requester, landedOrigin(requester.ID), "reconnect-fix", "ABC/sample")

	// Assert
	if err == nil || len(f.git.created) != 0 {
		t.Fatalf("CreateMergeWorkspace = %v with %d worktrees, want an error with nothing built", err, len(f.git.created))
	}
	if !loggedAtError(f, opCreate) {
		t.Fatal("the unresolved repository was not recorded at ERROR")
	}
}

func TestStartMergeWorkspace(t *testing.T) {
	tests := []struct {
		name string
		// merge makes the workspace a merge workspace.
		merge    bool
		startErr error
		// wantStarted says the fleet must have been asked to start it.
		wantStarted bool
		wantErr     bool
	}{
		{name: "a merge workspace's session is started", merge: true, wantStarted: true},
		{name: "a start failure is answered", merge: true, startErr: errors.New("the shim would not spawn"), wantStarted: true, wantErr: true},
		{name: "a workspace that is no merge workspace is refused", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("merge-ws", t.TempDir())
			if tt.merge {
				ws.MergeOf = &wsm.MergeOrigin{Requester: "requester", Request: "request-1"}
				f.db.with(ws)
			}
			f.fleet.startErr = tt.startErr

			// Act
			err := f.verbs.StartMergeWorkspace(context.Background(), ws.ID)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("StartMergeWorkspace = %v, want error %v", err, tt.wantErr)
			}
			if tt.startErr != nil && !errors.Is(err, tt.startErr) {
				t.Fatalf("StartMergeWorkspace = %v, want the start's failure", err)
			}
			if asked := slices.Contains(f.fleet.startCalls, ws.ID); asked != tt.wantStarted {
				t.Fatalf("start calls = %v, want started %v", f.fleet.startCalls, tt.wantStarted)
			}
		})
	}
}
