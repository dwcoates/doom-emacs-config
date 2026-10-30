package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// PutCreationJob records a workspace's merge geometry, deploy hooks and
// materialization state. It replaces the workspace's job wholesale: the record
// is current state, never an append-only history.
func (s *store) PutCreationJob(ctx context.Context, job CreationJob) error {
	const op = "daemon.wsm.put_creation_job"
	fields := dlog.Context{"workspace": string(job.Workspace), "materialized": job.Materialized, "one_shot": job.OneShot}
	before, err := encodeHooks(job.Hooks.PreDeploy)
	if err != nil {
		s.log.Error(op, "refused unencodable pre-deploy hooks", withError(fields, err))
		return err
	}
	after, err := encodeHooks(job.Hooks.PostDeploy)
	if err != nil {
		s.log.Error(op, "refused unencodable post-deploy hooks", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseMainWorkspaceOneShot(ctx, tx, job.Workspace, job.OneShot); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO creation_jobs (workspace_id, source_branch, source_dir, target_dir, layout_origin, pre_deploy_hooks, post_deploy_hooks,
			   base_ref, materialized, one_shot, initial_prompt, consented_ungated_mode, created_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   source_branch = excluded.source_branch, source_dir = excluded.source_dir, target_dir = excluded.target_dir,
			   layout_origin = excluded.layout_origin, pre_deploy_hooks = excluded.pre_deploy_hooks, post_deploy_hooks = excluded.post_deploy_hooks,
			   base_ref = excluded.base_ref, materialized = excluded.materialized, one_shot = excluded.one_shot,
			   initial_prompt = excluded.initial_prompt, consented_ungated_mode = excluded.consented_ungated_mode,
			   created_at = excluded.created_at`,
			job.Workspace, job.Layout.SourceBranch, job.Layout.SourceDir, job.Layout.TargetDir, job.Layout.Origin,
			before, after, job.BaseRef, job.Materialized, job.OneShot, job.InitialPrompt, job.ConsentedUngatedMode,
			nanos(job.CreatedAt))
		return err
	})
}

// CreationJob loads one workspace's creation job; the bool reports existence.
// Merge REFUSES rather than guessing when it is absent, so absence is reported
// and never filled in with a default geometry.
func (s *store) CreationJob(ctx context.Context, id WorkspaceID) (CreationJob, bool, error) {
	var (
		job   CreationJob
		found bool
	)
	err := s.read(ctx, "daemon.wsm.creation_job", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var (
			before, after string
			created       int64
		)
		row := s.q(ctx).QueryRowContext(ctx,
			`SELECT workspace_id, source_branch, source_dir, target_dir, layout_origin, pre_deploy_hooks, post_deploy_hooks,
			        base_ref, materialized, one_shot, initial_prompt, consented_ungated_mode, created_at
			 FROM creation_jobs WHERE workspace_id = ?`, id)
		err := row.Scan(&job.Workspace, &job.Layout.SourceBranch, &job.Layout.SourceDir, &job.Layout.TargetDir, &job.Layout.Origin,
			&before, &after, &job.BaseRef, &job.Materialized, &job.OneShot, &job.InitialPrompt, &job.ConsentedUngatedMode, &created)
		if errors.Is(err, sql.ErrNoRows) {
			job, found = CreationJob{}, false
			return nil
		}
		if err != nil {
			return err
		}
		if job.Hooks.PreDeploy, err = decodeHooks("creation_jobs", string(id), "pre_deploy_hooks", before); err != nil {
			return err
		}
		if job.Hooks.PostDeploy, err = decodeHooks("creation_jobs", string(id), "post_deploy_hooks", after); err != nil {
			return err
		}
		job.CreatedAt = fromNanos(created)
		found = true
		return nil
	})
	if err != nil {
		return CreationJob{}, false, err
	}
	return job, found, nil
}
