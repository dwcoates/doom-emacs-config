package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// THE DEPLOY CHECKS' MEMORY OF PROBLEMS (owner ruling, 2026-11-16). A
// problem the deploy mediator dispatched or escalated is kept per
// repository, keyed by the operation and message of the warning or error it
// was, with every finding the mediator stated. Since layout 168 it lives in
// deploy_mediator_problems, which names no owner: the mediator owns every
// problem. It is never
// closed by a quiet window: a remediation's deploy marks it FIXED, and the
// group logged again on any later landing makes it a recurrence the next
// check must dispatch or escalate again, the earlier findings beside it.
// Before it, a warning that recurred after its fix was read as "did not
// recur" by the next check's short window, and the chain closed on it
// (2026-11-16 02:44: the durable log sink warning).

// deployProblemsDDL is the layout-156 addition: the deploy problems, each
// owned check's window, and where each repository's checks have read up to.
// IT IS ADDITIVE: a new table and two new columns the previous build never
// names.
const deployProblemsDDL = `
CREATE TABLE deploy_problems (
  repo_key   TEXT NOT NULL CHECK (repo_key <> ''),
  operation  TEXT NOT NULL CHECK (operation <> ''),
  message    TEXT NOT NULL,
  owner      TEXT NOT NULL CHECK (owner <> ''),
  opened_at  INTEGER NOT NULL CHECK (opened_at > 0),
  fixed_at   INTEGER,
  findings   TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (repo_key, operation, message)
);
ALTER TABLE merge_queue_repos ADD COLUMN checked_through INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN mediation_window TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_window_start INTEGER;
`

// ProblemKey names one group of warnings or errors: its operation and
// message, as the log reader groups them.
type ProblemKey struct {
	Operation string
	Message   string
}

// DeployProblem is one problem a repository's deploy checks keep.
type DeployProblem struct {
	Key ProblemKey
	// OpenedAt is when it was first dispatched or escalated.
	OpenedAt time.Time
	// FixedAt is when the mediator's fix of it deployed, nil while the
	// mediator is at work on it.
	FixedAt *time.Time
	// Findings is every statement the mediator gave of it, oldest first.
	Findings string
	// Fixes is how many of the mediator's fixes of it deployed: a problem that comes
	// back after more than two is the user's (owner ruling, 2026-11-16).
	Fixes int
}

// deployProblemFixesDDL is the layout-161 addition: how many fixes of each
// problem deployed (DeployProblem.Fixes). IT IS ADDITIVE: the build before
// it neither reads nor writes the column.
const deployProblemFixesDDL = `
ALTER TABLE deploy_problems ADD COLUMN fixes INTEGER NOT NULL DEFAULT 0;
`

// WindowClass is how a check's window found one group against the problems
// the repository keeps.
type WindowClass string

const (
	// WindowNew is a group the span before the window did not hold, and no
	// problem names.
	WindowNew WindowClass = "new"
	// WindowSeenBefore is a group the span before held, which no problem
	// names: an old problem the mediator never took on.
	WindowSeenBefore WindowClass = "seen_before"
	// WindowOwned is a group a problem names that the mediator is still at
	// work on.
	WindowOwned WindowClass = "owned"
	// WindowRecurred is a group a problem names whose fix deployed: the fix
	// did not end it.
	WindowRecurred WindowClass = "recurred"
)

// CheckWindow is the window of logs a check's intake was told: from Start,
// where the repository's previous check read to, through Through, and its
// groups as the intake was told them.
type CheckWindow struct {
	Start   time.Time
	Through time.Time
	Groups  []WindowGroup
}

// WindowGroup is one group of a check's window, as the intake was told it.
type WindowGroup struct {
	Operation string      `json:"operation"`
	Message   string      `json:"message"`
	Class     WindowClass `json:"class"`
	// Fixes is how many fixes of the group's problem deployed, for an owned
	// or recurred group.
	Fixes int `json:"fixes,omitempty"`
}

// DeployProblems answers every problem REPO's deploy checks keep.
func (s *store) DeployProblems(ctx context.Context, repo RepoKey) ([]DeployProblem, error) {
	const op = "daemon.wsm.deploy_problems"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return nil, err
	}
	var out []DeployProblem
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT operation, message, opened_at, fixed_at, findings, fixes FROM deploy_mediator_problems WHERE repo_key = ? ORDER BY opened_at, operation, message`, string(key))
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var p DeployProblem
			var opened int64
			var fixed sql.NullInt64
			if err := rows.Scan(&p.Key.Operation, &p.Key.Message, &opened, &fixed, &p.Findings, &p.Fixes); err != nil {
				return err
			}
			p.OpenedAt, p.FixedAt = fromNanos(opened), optTime(fixed)
			out = append(out, p)
		}
		return rows.Err()
	})
	return out, err
}

// TakeOnDeployProblems records that the mediator took on every problem KEYS
// names in REPO at AT, adding FINDINGS to each: a problem it opens, or a
// recurrence, which is open again from here. A call naming no key or no
// findings is refused and writes nothing.
func (s *store) TakeOnDeployProblems(ctx context.Context, repo RepoKey, keys []ProblemKey, findings string, at time.Time) error {
	const op = "daemon.wsm.take_on_deploy_problems"
	fields := dlog.Context{"repo": string(repo), "problems": len(keys)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	if len(keys) == 0 || strings.TrimSpace(findings) == "" || at.IsZero() {
		err := errors.New("wsm: a deploy problem is taken on for named groups, with its findings and an instant")
		s.log.Error(op, "refused to take on deploy problems that name no group, findings or instant", withError(fields, err))
		return err
	}
	for _, k := range keys {
		if k.Operation == "" {
			err := fmt.Errorf("wsm: a deploy problem names its operation (message %q)", k.Message)
			s.log.Error(op, "refused a deploy problem that names no operation", withError(fields, err))
			return err
		}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, k := range keys {
			if _, err := tx.ExecContext(ctx, `INSERT INTO deploy_mediator_problems (repo_key, operation, message, opened_at, fixed_at, findings) VALUES (?, ?, ?, ?, NULL, ?)
				ON CONFLICT(repo_key, operation, message) DO UPDATE SET fixed_at = NULL,
				  findings = deploy_mediator_problems.findings || char(10) || excluded.findings`,
				string(key), k.Operation, k.Message, nanos(at), findings); err != nil {
				return err
			}
		}
		return nil
	})
}

// FixDeployProblems marks every open problem of REPO fixed at AT: the
// mediator's fix deployed. It answers how many it marked.
func (s *store) FixDeployProblems(ctx context.Context, repo RepoKey, at time.Time) (int, error) {
	const op = "daemon.wsm.fix_deploy_problems"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return 0, err
	}
	if at.IsZero() {
		err := errors.New("wsm: a fix names its instant")
		s.log.Error(op, "refused to mark deploy problems fixed with no instant", withError(fields, err))
		return 0, err
	}
	n := 0
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE deploy_mediator_problems SET fixed_at = ?, fixes = fixes + 1 WHERE repo_key = ? AND fixed_at IS NULL`, nanos(at), string(key))
		if err != nil {
			return err
		}
		rows, err := res.RowsAffected()
		n = int(rows)
		return err
	})
	return n, err
}

// CheckedThrough answers where REPO's deploy checks have read its logs up to:
// the end of the last window an intake was told, false before any.
func (s *store) CheckedThrough(ctx context.Context, repo RepoKey) (time.Time, bool, error) {
	const op = "daemon.wsm.checked_through"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return time.Time{}, false, err
	}
	var through sql.NullInt64
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT checked_through FROM merge_queue_repos WHERE repo_key = ?`, string(key)).Scan(&through)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return err
	})
	if err != nil || !through.Valid {
		return time.Time{}, false, err
	}
	return fromNanos(through.Int64), true, nil
}

// RecordMediationWindow records W, the window the intake of REPO's check owed
// at OWED was told; its Through is where the next check's window starts. It
// answers false when that check is no longer the one owed.
func (s *store) RecordMediationWindow(ctx context.Context, repo RepoKey, owed time.Time, w CheckWindow) (bool, error) {
	const op = "daemon.wsm.record_mediation_window"
	if w.Start.IsZero() || w.Through.IsZero() || w.Through.Before(w.Start) {
		err := fmt.Errorf("wsm: a check's window runs forward from its start (%v through %v)", w.Start, w.Through)
		s.log.Error(op, "refused a check's window that does not run forward", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	groups := w.Groups
	if groups == nil {
		groups = []WindowGroup{}
	}
	encoded, err := json.Marshal(groups)
	if err != nil {
		s.log.Error(op, "could not encode the check's window", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, op, repo, owed,
		`UPDATE merge_queue_repos SET checked_through = ?, mediation_window_start = ?, mediation_window = ? WHERE repo_key = ? AND mediation_owed_at = ?`,
		nanos(w.Through), nanos(w.Start), string(encoded))
}

// MediationWindowOf answers the window the intake of REPO's owed check was
// told, false when none was recorded for it. A window that does not decode
// is a broken store, refused at ERROR.
func (s *store) MediationWindowOf(ctx context.Context, repo RepoKey) (CheckWindow, bool, error) {
	const op = "daemon.wsm.mediation_window_of"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return CheckWindow{}, false, err
	}
	var raw string
	var start, through sql.NullInt64
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT mediation_window, mediation_window_start, checked_through FROM merge_queue_repos WHERE repo_key = ? AND mediation_owed_at IS NOT NULL`, string(key)).Scan(&raw, &start, &through)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return err
	})
	if err != nil || raw == "" {
		return CheckWindow{}, false, err
	}
	out := CheckWindow{}
	if err := json.Unmarshal([]byte(raw), &out.Groups); err != nil || !start.Valid || !through.Valid {
		if err == nil {
			err = errors.New("its start or end is missing")
		}
		err = fmt.Errorf("wsm: the owed check's window of %s does not decode: %w", key, err)
		s.log.Error(op, "the owed check's window does not decode", withError(dlog.Context{"repo": string(key)}, err))
		return CheckWindow{}, false, err
	}
	out.Start, out.Through = fromNanos(start.Int64), fromNanos(through.Int64)
	return out, true, nil
}
