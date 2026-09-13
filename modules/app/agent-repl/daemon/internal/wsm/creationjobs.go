package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// encodeActions renders a merge action list as the JSON array the column holds.
// An absent list and an empty one are both stored as "[]", so a decode never
// has to guess.
func encodeActions(names []string) (string, error) {
	if names == nil {
		names = []string{}
	}
	out, err := json.Marshal(names)
	if err != nil {
		return "", fmt.Errorf("wsm: encode merge actions: %w", err)
	}
	return string(out), nil
}

// decodeActions parses one stored action list, failing the whole read when the
// column is not a JSON array of names.
func decodeActions(row, field, raw string) ([]string, error) {
	var names []string
	if err := json.Unmarshal([]byte(raw), &names); err != nil {
		return nil, &DecodeError{Table: "creation_jobs", Row: row, Field: field, Err: err}
	}
	return names, nil
}

// PutCreationJob records a workspace's merge geometry, configured actions and
// materialization state. It replaces the workspace's job wholesale: the record
// is current state, never an append-only history.
func (s *store) PutCreationJob(ctx context.Context, job CreationJob) error {
	const op = "daemon.wsm.put_creation_job"
	fields := dlog.Context{"workspace": string(job.Workspace), "materialized": job.Materialized, "one_shot": job.OneShot}
	before, err := encodeActions(job.Actions.Before)
	if err != nil {
		s.log.Error(op, "refused unencodable before-merge actions", withError(fields, err))
		return err
	}
	after, err := encodeActions(job.Actions.After)
	if err != nil {
		s.log.Error(op, "refused unencodable after-merge actions", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO creation_jobs (workspace_id, source_branch, source_dir, target_dir, layout_origin, actions_before, actions_after,
			   base_ref, materialized, one_shot, initial_prompt, consented_ungated_mode, created_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   source_branch = excluded.source_branch, source_dir = excluded.source_dir, target_dir = excluded.target_dir,
			   layout_origin = excluded.layout_origin, actions_before = excluded.actions_before, actions_after = excluded.actions_after,
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
		row := s.db().QueryRowContext(ctx,
			`SELECT workspace_id, source_branch, source_dir, target_dir, layout_origin, actions_before, actions_after,
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
		if job.Actions.Before, err = decodeActions(string(id), "actions_before", before); err != nil {
			return err
		}
		if job.Actions.After, err = decodeActions(string(id), "actions_after", after); err != nil {
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
