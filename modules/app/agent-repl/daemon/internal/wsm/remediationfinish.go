package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"
)

// remediationFinishesDDL is the layout-138 addition: the FINISH of a deploy
// remediation workspace (agentrepl.v1.FinishWorkspaceSuccess
// .landing_directly), one row per workspace finishing. The daemon runs the
// full gate on its branch, lands it directly on the main branch, and deploys,
// and each step is recorded here BEFORE it starts: the deploy restarts the
// daemon, so the one that serves after it reads where the finish stood.
//
// IT IS ADDITIVE: the build before it neither reads nor writes the table.
const remediationFinishesDDL = `
CREATE TABLE deploy_remediation_finishes (
  workspace_id   TEXT PRIMARY KEY,
  repo_key       TEXT NOT NULL,
  step           TEXT NOT NULL,
  step_at        INTEGER NOT NULL,
  gate_rounds    INTEGER NOT NULL DEFAULT 0,
  version_part   TEXT NOT NULL DEFAULT '',
  changelog_line TEXT NOT NULL DEFAULT '',
  started_at     INTEGER NOT NULL
);
`

// RemediationStep is where a remediation workspace's finish stands.
type RemediationStep string

const (
	// RemediationGating: the full gate runs on the workspace's branch,
	// rebased onto the main branch.
	RemediationGating RemediationStep = "gating"
	// RemediationFixing: the gate failed and the workspace was resumed with
	// the failures; its next finish gates again.
	RemediationFixing RemediationStep = "fixing"
	// RemediationLanding: the gate passed and the branch is landing directly
	// on the main branch.
	RemediationLanding RemediationStep = "landing"
	// RemediationDeploying: the branch landed and the deploy is starting; the
	// deploy owes the chain's next check.
	RemediationDeploying RemediationStep = "deploying"
)

// valid reports whether the step is one of the four.
func (s RemediationStep) valid() bool {
	switch s {
	case RemediationGating, RemediationFixing, RemediationLanding, RemediationDeploying:
		return true
	}
	return false
}

// RemediationFinish is one remediation workspace's finish.
type RemediationFinish struct {
	// Workspace is the remediation workspace finishing.
	Workspace WorkspaceID
	// Repo is the queue whose check the remediation answers.
	Repo RepoKey
	// Step is where the finish stands, and StepAt when it got there.
	Step   RemediationStep
	StepAt time.Time
	// GateRounds counts the gates that failed and resumed the workspace.
	GateRounds int
	// Version is how the landing moves the repository's version, nil when
	// the repository versions nothing.
	Version *VersionIncrement
	// StartedAt is the first finish's instant.
	StartedAt time.Time
}

// ErrRemediationFinishing refuses a finish of a remediation workspace whose
// finish is already gating, landing or deploying.
var ErrRemediationFinishing = errors.New("wsm: the remediation workspace's finish is already running")

// BeginRemediationFinish records F's finish at the gating step: a first
// finish inserts its row, and a finish after a failed gate (step fixing)
// moves the row back to gating, keeping its rounds and taking F's version.
// A finish already gating, landing or deploying is ErrRemediationFinishing.
// It answers the row as recorded.
func (s *store) BeginRemediationFinish(ctx context.Context, f RemediationFinish) (RemediationFinish, error) {
	const op = "daemon.wsm.begin_remediation_finish"
	fields := dlog.Context{"workspace": string(f.Workspace), "repo": string(f.Repo)}
	key, err := normalizeRepoKey(f.Repo)
	if err != nil || f.Workspace == "" || f.StartedAt.IsZero() {
		if err == nil {
			err = fmt.Errorf("wsm: a remediation finish names its workspace and instant (%+v)", f)
		}
		s.log.Error(op, "refused a remediation finish that names no workspace, queue or instant", withError(fields, err))
		return RemediationFinish{}, err
	}
	part, line := versionColumns(f.Version)
	var out RemediationFinish
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		prior, err := scanRemediationFinish(tx.QueryRowContext(ctx, remediationFinishQuery+` WHERE workspace_id = ?`, string(f.Workspace)))
		if err != nil {
			return err
		}
		switch {
		case prior == nil:
			_, err = tx.ExecContext(ctx,
				`INSERT INTO deploy_remediation_finishes (workspace_id, repo_key, step, step_at, gate_rounds, version_part, changelog_line, started_at) VALUES (?, ?, ?, ?, 0, ?, ?, ?)`,
				string(f.Workspace), string(key), string(RemediationGating), nanos(f.StartedAt), part, line, nanos(f.StartedAt))
			out = RemediationFinish{Workspace: f.Workspace, Repo: key, Step: RemediationGating, StepAt: f.StartedAt.UTC(), Version: f.Version, StartedAt: f.StartedAt.UTC()}
		case prior.Step == RemediationFixing:
			_, err = tx.ExecContext(ctx,
				`UPDATE deploy_remediation_finishes SET step = ?, step_at = ?, version_part = ?, changelog_line = ? WHERE workspace_id = ?`,
				string(RemediationGating), nanos(f.StartedAt), part, line, string(f.Workspace))
			out = *prior
			out.Step, out.StepAt, out.Version = RemediationGating, f.StartedAt.UTC(), f.Version
		default:
			return ErrRemediationFinishing
		}
		return err
	})
	if err != nil {
		return RemediationFinish{}, err
	}
	return out, nil
}

// versionColumns spells V as its two columns, empty for none.
func versionColumns(v *VersionIncrement) (string, string) {
	if v == nil {
		return "", ""
	}
	return v.Part.String(), v.ChangelogLine
}

// AdvanceRemediationFinish moves WS's finish from step FROM to step TO at AT,
// answering false when the finish is not at FROM (it moved meanwhile, or
// ended). A move to fixing counts a failed gate round.
func (s *store) AdvanceRemediationFinish(ctx context.Context, ws WorkspaceID, from, to RemediationStep, at time.Time) (bool, error) {
	const op = "daemon.wsm.advance_remediation_finish"
	fields := dlog.Context{"workspace": string(ws), "from": string(from), "to": string(to)}
	if !from.valid() || !to.valid() {
		err := fmt.Errorf("wsm: a remediation finish moves between its steps, not %q to %q", from, to)
		s.log.Error(op, "refused a remediation finish step that is no step", withError(fields, err))
		return false, err
	}
	rounds := 0
	if to == RemediationFixing {
		rounds = 1
	}
	changed := false
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE deploy_remediation_finishes SET step = ?, step_at = ?, gate_rounds = gate_rounds + ? WHERE workspace_id = ? AND step = ?`,
			string(to), nanos(at), rounds, string(ws), string(from))
		if err != nil {
			return err
		}
		changed, err = touchedRow(res)
		return err
	})
	return changed, err
}

// RemediationFinishOf answers WS's finish, nil when it has none.
func (s *store) RemediationFinishOf(ctx context.Context, ws WorkspaceID) (*RemediationFinish, error) {
	const op = "daemon.wsm.remediation_finish_of"
	var out *RemediationFinish
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		f, err := scanRemediationFinish(s.q(ctx).QueryRowContext(ctx, remediationFinishQuery+` WHERE workspace_id = ?`, string(ws)))
		out = f
		return err
	})
	return out, err
}

// RemediationFinishes answers every remediation finish recorded, oldest
// first: what a daemon serving the queues takes up at its boot.
func (s *store) RemediationFinishes(ctx context.Context) ([]RemediationFinish, error) {
	const op = "daemon.wsm.remediation_finishes"
	var out []RemediationFinish
	err := s.read(ctx, op, nil, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, remediationFinishQuery+` ORDER BY started_at, workspace_id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			f, err := scanRemediationFinishRow(rows)
			if err != nil {
				return err
			}
			out = append(out, f)
		}
		return rows.Err()
	})
	return out, err
}

// EndRemediationFinish removes WS's finish, answering whether one was
// recorded.
func (s *store) EndRemediationFinish(ctx context.Context, ws WorkspaceID) (bool, error) {
	const op = "daemon.wsm.end_remediation_finish"
	changed := false
	err := s.write(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM deploy_remediation_finishes WHERE workspace_id = ?`, string(ws))
		if err != nil {
			return err
		}
		changed, err = touchedRow(res)
		return err
	})
	return changed, err
}

// remediationFinishQuery reads the finish columns; callers add the WHERE.
const remediationFinishQuery = `SELECT workspace_id, repo_key, step, step_at, gate_rounds, version_part, changelog_line, started_at FROM deploy_remediation_finishes`

// rowScanner is a *sql.Row or *sql.Rows.
type rowScanner interface{ Scan(dest ...any) error }

// scanRemediationFinish decodes ROW, nil when there is none.
func scanRemediationFinish(row *sql.Row) (*RemediationFinish, error) {
	f, err := scanRemediationFinishRow(row)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &f, nil
}

// scanRemediationFinishRow is the one decoding of a finish row. A step or a
// version part the daemon never writes is a DecodeError.
func scanRemediationFinishRow(row rowScanner) (RemediationFinish, error) {
	var ws, repo, step, part, line string
	var stepAt, started int64
	var rounds int
	if err := row.Scan(&ws, &repo, &step, &stepAt, &rounds, &part, &line, &started); err != nil {
		return RemediationFinish{}, err
	}
	f := RemediationFinish{Workspace: WorkspaceID(ws), Repo: RepoKey(repo), Step: RemediationStep(step), StepAt: fromNanos(stepAt),
		GateRounds: rounds, StartedAt: fromNanos(started)}
	if !f.Step.valid() {
		return RemediationFinish{}, &DecodeError{Table: "deploy_remediation_finishes", Row: ws, Field: "step", Err: fmt.Errorf("no such step %q", step)}
	}
	if part != "" {
		p, err := semver.ParsePart(part)
		if err != nil {
			return RemediationFinish{}, &DecodeError{Table: "deploy_remediation_finishes", Row: ws, Field: "version_part", Err: err}
		}
		f.Version = &VersionIncrement{Part: p, ChangelogLine: line}
	}
	return f, nil
}
