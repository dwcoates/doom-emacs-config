package wsm

import (
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"context"
	"errors"
	"testing"
)

func TestPutCreationJobRoundTripsTheMergeGeometry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	job := CreationJob{
		Workspace:            ws.ID,
		Layout:               MergeLayout{SourceBranch: "feature", SourceDir: "/src", TargetDir: "/target", Origin: "create"},
		Hooks:                DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("pre")}, PostDeploy: []*repoconfigv1.DeployHook{promptHook("post"), scriptHook("bin/notify.sh", 5)}},
		BaseRef:              "master",
		Materialized:         true,
		OneShot:              true,
		InitialPrompt:        "do the thing",
		ConsentedUngatedMode: "bypassPermissions",
		CreatedAt:            instant,
	}

	// Act
	if err := s.PutCreationJob(context.Background(), job); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	got, found, err := s.CreationJob(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CreationJob: %v", err)
	}
	if !found {
		t.Fatalf("found = false for a recorded job")
	}
	if got.Layout != job.Layout || got.BaseRef != job.BaseRef || !got.Materialized || !got.OneShot {
		t.Fatalf("job = %+v, want %+v", got, job)
	}
	if got.InitialPrompt != job.InitialPrompt || got.ConsentedUngatedMode != job.ConsentedUngatedMode {
		t.Fatalf("job = %+v, want the creation facts", got)
	}
	if !got.CreatedAt.Equal(instant) {
		t.Fatalf("created at = %v, want %v", got.CreatedAt, instant)
	}
}

func TestPutCreationJobRoundTripsTheDeployHooks(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	hooks := DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("a"), scriptHook("bin/b.sh", 9)}, PostDeploy: []*repoconfigv1.DeployHook{promptHook("c")}}

	// Act
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, Hooks: hooks, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	got, _, err := s.CreationJob(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CreationJob: %v", err)
	}
	if !got.Hooks.Equal(hooks) {
		t.Fatalf("hooks = %+v, want %+v", got.Hooks, hooks)
	}
}

func TestPutCreationJobPrecedesRegistration(t *testing.T) {
	// Arrange — a creation job is recorded BEFORE the worktree is materialized,
	// so it must not require a registered workspace.
	s, _ := testStore(t)

	// Act
	err := s.PutCreationJob(context.Background(), CreationJob{Workspace: WorkspaceID("unregistered"), CreatedAt: instant})

	// Assert
	if err != nil {
		t.Fatalf("PutCreationJob for an unregistered workspace: %v", err)
	}
}

func TestPutCreationJobReplacesTheWorkspacesJob(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, BaseRef: "old", CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}

	// Act
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, BaseRef: "new", Materialized: true, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}

	// Assert
	got, _, err := s.CreationJob(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("CreationJob: %v", err)
	}
	if got.BaseRef != "new" || !got.Materialized {
		t.Fatalf("job = %+v, want the replacement's facts", got)
	}
}

func TestCreationJobReportsAbsenceRatherThanGuessing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, found, err := s.CreationJob(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("CreationJob: %v", err)
	}
	if found {
		t.Fatalf("found = true with no job recorded")
	}
	if got.Layout != (MergeLayout{}) {
		t.Fatalf("job = %+v, want a zero record the caller must refuse on", got)
	}
}

func TestCreationJobFailsWholeOnACorruptActionList(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	corrupt(t, s, `UPDATE creation_jobs SET pre_deploy_hooks = 'not json' WHERE workspace_id = ?`, ws.ID)

	// Act
	_, found, err := s.CreationJob(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "creation_jobs" || refusal.Field != "pre_deploy_hooks" {
		t.Fatalf("CreationJob = %v, want a *DecodeError naming creation_jobs.pre_deploy_hooks", err)
	}
	if found {
		t.Fatalf("found = true alongside the refusal")
	}
	if !loggedOperation(log, "daemon.wsm.creation_job", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}
