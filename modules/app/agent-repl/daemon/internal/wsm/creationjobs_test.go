package wsm

import (
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"context"
	"database/sql"
	"errors"
	"strings"
	"testing"
)

func TestPutCreationJobRoundTripsTheMergeGeometry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
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

func TestDeleteCreationJob(t *testing.T) {
	tests := []struct {
		name string
		// arrange answers the id whose job is deleted.
		arrange   func(t *testing.T, s *store) WorkspaceID
		wantErr   string
		wantFound bool
	}{
		{
			name: "the job of a create that never registered is removed",
			arrange: func(t *testing.T, s *store) WorkspaceID {
				id := NewWorkspaceID()
				if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: id, CreatedAt: instant}); err != nil {
					t.Fatalf("PutCreationJob: %v", err)
				}
				return id
			},
		},
		{
			name:    "a job already gone is no error",
			arrange: func(t *testing.T, s *store) WorkspaceID { return NewWorkspaceID() },
		},
		{
			name: "a registered workspace's job is refused and kept",
			arrange: func(t *testing.T, s *store) WorkspaceID {
				ws := testLinkedWorkspace(t, s)
				if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, CreatedAt: instant}); err != nil {
					t.Fatalf("PutCreationJob: %v", err)
				}
				return ws.ID
			},
			wantErr:   "the workspace is registered",
			wantFound: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			id := tt.arrange(t, s)

			// Act
			err := s.DeleteCreationJob(context.Background(), id)

			// Assert
			if tt.wantErr == "" && err != nil {
				t.Fatalf("DeleteCreationJob = %v, want success", err)
			}
			if tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("DeleteCreationJob = %v, want %q", err, tt.wantErr)
			}
			if _, found, err := s.CreationJob(context.Background(), id); err != nil || found != tt.wantFound {
				t.Fatalf("CreationJob = found %v, %v; want found %v", found, err, tt.wantFound)
			}
		})
	}
}

func TestWriteCreationJobReplacesTheRowWholesale(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	none, err := encodeHooks(nil)
	if err != nil {
		t.Fatalf("encodeHooks: %v", err)
	}
	write := func(job CreationJob) {
		t.Helper()
		if err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
			return writeCreationJob(ctx, tx, job, none, none)
		}); err != nil {
			t.Fatalf("writeCreationJob: %v", err)
		}
	}
	write(CreationJob{Workspace: ws.ID, BaseRef: "old", OneShot: true, CreatedAt: instant})

	// Act
	write(CreationJob{Workspace: ws.ID, Layout: MergeLayout{SourceBranch: "b", SourceDir: ws.Dir, TargetDir: "/t", Origin: "o"}, Materialized: true, CreatedAt: instant})

	// Assert
	got, found, err := s.CreationJob(context.Background(), ws.ID)
	if err != nil || !found {
		t.Fatalf("CreationJob = found %v, %v", found, err)
	}
	if got.BaseRef != "" || got.OneShot || !got.Materialized || got.Layout.TargetDir != "/t" {
		t.Fatalf("job = %+v, want the second write whole", got)
	}
}

// TestBothCreationJobWritersWriteOneShape pins that PutCreationJob and a
// restated landing write the same row for the same facts: both go through
// writeCreationJob, so a writer spelling its own INSERT fails here.
func TestBothCreationJobWritersWriteOneShape(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s)
	restated := registerInRepo(t, s, f.repoDir, "restated", false)
	put := registerInRepo(t, s, f.repoDir, "put", false)
	if _, err := s.SetParent(context.Background(), restated.ID, Retarget{KeepParent: true, Landing: &LandingChoice{}}); err != nil {
		t.Fatalf("SetParent: %v", err)
	}
	want, _, err := s.CreationJob(context.Background(), restated.ID)
	if err != nil {
		t.Fatalf("CreationJob: %v", err)
	}

	// Act
	mirror := want
	mirror.Workspace = put.ID
	if err := s.PutCreationJob(context.Background(), mirror); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}

	// Assert
	row := func(id WorkspaceID) string {
		return scalar[string](t, s, `SELECT source_branch||'|'||source_dir||'|'||target_dir||'|'||layout_origin||'|'||pre_deploy_hooks||'|'||post_deploy_hooks||'|'||base_ref||'|'||materialized||'|'||one_shot||'|'||initial_prompt||'|'||consented_ungated_mode||'|'||created_at FROM creation_jobs WHERE workspace_id = ?`, id)
	}
	if got, restatedRow := row(put.ID), row(restated.ID); got != restatedRow {
		t.Fatalf("PutCreationJob wrote %q, the restated landing %q; want one shape", got, restatedRow)
	}
}
