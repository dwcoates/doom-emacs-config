package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of WORKSPACE GATHERS (agentrepl.v1
// GatherWorkspaces): one workspace's question to many, each target's
// resolution, and whether the caller was told. DURABLE because handovers and
// restarts happen at every deploy, and a gather must outlive them: its latch
// and its deadline re-arm from these rows on boot, and every notice is marked
// told here, so a restart neither loses nor doubles one.
//
// A GATHER ENDS WITH ITS CALLER: forgetting the caller cascades its gathers
// away. A TARGET carries no reference to its workspace on purpose, so a
// target forgotten before it answered stays a row the gather resolves as
// closed instead of vanishing from it.

// gathersDDL is the layout-122 addition, with turnSuccessionsDDL. The
// fresh-file schema appends this same text (schema.go), so a migrated file and
// a created one cannot differ.
const gathersDDL = `
CREATE TABLE gathers (
  id              TEXT PRIMARY KEY,
  caller_id       TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  caller_name     TEXT NOT NULL,
  caller_dir      TEXT NOT NULL,
  idempotency_key TEXT NOT NULL UNIQUE,
  prompt_text     TEXT NOT NULL,
  created_at      INTEGER NOT NULL,
  deadline_at     INTEGER NOT NULL,
  first_told_at   INTEGER,
  first_trigger   TEXT CHECK (first_trigger IN ('all_resolved', 'timed_out')),
  CHECK ((first_told_at IS NULL) = (first_trigger IS NULL))
);
CREATE TABLE gather_targets (
  gather_id    TEXT NOT NULL REFERENCES gathers(id) ON DELETE CASCADE,
  position     INTEGER NOT NULL,
  workspace_id TEXT NOT NULL,
  name         TEXT NOT NULL,
  dir          TEXT NOT NULL,
  turn_id      TEXT NOT NULL,
  resolved_at  INTEGER,
  resolution   TEXT CHECK (resolution IN ('answered', 'closed')),
  answer_text  TEXT,
  told_at      INTEGER,
  PRIMARY KEY (gather_id, workspace_id),
  CHECK ((resolved_at IS NULL) = (resolution IS NULL)),
  CHECK ((resolution = 'answered') = (answer_text IS NOT NULL)),
  CHECK (told_at IS NULL OR resolved_at IS NOT NULL)
);
CREATE INDEX gather_targets_outstanding ON gather_targets(workspace_id) WHERE resolved_at IS NULL;
`

// GatherID is a gather's daemon-minted identity.
type GatherID string

// NewGatherID mints a gather identity.
func NewGatherID() GatherID { return GatherID(mint()) }

// GatherTrigger is what sent a gather's first update. ITS SPELLINGS ARE
// DURABLE.
type GatherTrigger string

// The triggers.
const (
	// GatherAllResolved is every target resolved: the latch.
	GatherAllResolved GatherTrigger = "all_resolved"
	// GatherTimedOut is the deadline passing first.
	GatherTimedOut GatherTrigger = "timed_out"
)

// GatherResolutionKind is how a target resolved. ITS SPELLINGS ARE DURABLE.
type GatherResolutionKind string

// The resolutions.
const (
	// GatherAnswered is the target's turn ending on a final response.
	GatherAnswered GatherResolutionKind = "answered"
	// GatherClosed is the target closing, or being forgotten, unanswered.
	GatherClosed GatherResolutionKind = "closed"
)

// Gather is one gather and its targets, in the order the caller named them.
type Gather struct {
	ID GatherID
	// Caller is the workspace the gather prompts on behalf of, with its name
	// and dir as they stood when the gather was made.
	Caller     WorkspaceID
	CallerName string
	CallerDir  string
	// Key is the request's idempotency key: one gather per key.
	Key        string
	PromptText string
	CreatedAt  time.Time
	DeadlineAt time.Time
	// FirstTold is the first update, nil until it is told.
	FirstTold *GatherFirstTold
	Targets   []GatherTarget
}

// GatherFirstTold is when a gather's first update was told, and what sent it.
type GatherFirstTold struct {
	At      time.Time
	Trigger GatherTrigger
}

// GatherTarget is one accepted target.
type GatherTarget struct {
	Workspace WorkspaceID
	// Name and Dir are the workspace's as they stood when the gather was made.
	Name string
	Dir  string
	// Turn is the turn the prompt was minted as; SucceedingTurn follows it to
	// the turn that now carries it.
	Turn TurnID
	// Resolution is how the target resolved, nil while it is outstanding.
	Resolution *GatherResolution
	// ToldAt is when the caller was told the resolution: in the first update,
	// or in its own later notice. Nil while untold.
	ToldAt *time.Time
}

// GatherResolution is how and when a target resolved.
type GatherResolution struct {
	Kind GatherResolutionKind
	At   time.Time
	// Text is the answer's markdown, set exactly when Kind is GatherAnswered.
	Text string
}

// Outstanding reports whether the target has not resolved.
func (t GatherTarget) Outstanding() bool { return t.Resolution == nil }

// Settled reports whether nothing more is owed about the gather: its first
// update was told and so was every target's resolution.
func (g Gather) Settled() bool {
	if g.FirstTold == nil {
		return false
	}
	for _, t := range g.Targets {
		if t.ToldAt == nil {
			return false
		}
	}
	return true
}

// AllResolved reports whether every target resolved.
func (g Gather) AllResolved() bool {
	for _, t := range g.Targets {
		if t.Outstanding() {
			return false
		}
	}
	return true
}

// ErrDuplicateGather is a gather whose idempotency key another gather holds.
var ErrDuplicateGather = errors.New("wsm: a gather already holds this idempotency key")

// validateGather refuses a gather the rows could not hold faithfully.
func validateGather(g Gather) error {
	switch {
	case g.ID == "" || g.Caller == "" || g.Key == "" || g.PromptText == "":
		return errors.New("wsm: a gather names its id, caller, key and prompt")
	case len(g.Targets) == 0:
		return errors.New("wsm: a gather has at least one target")
	case !g.DeadlineAt.After(g.CreatedAt):
		return errors.New("wsm: a gather's deadline follows its creation")
	case g.FirstTold != nil:
		return errors.New("wsm: a gather is put before its first update is told")
	}
	for _, t := range g.Targets {
		if t.Workspace == "" || t.Name == "" || t.Turn == "" || t.Resolution != nil || t.ToldAt != nil {
			return fmt.Errorf("wsm: gather target %q names its workspace, name and turn, and is put unresolved", t.Name)
		}
	}
	return nil
}

// PutGather records a new gather and its targets in one transaction. A key
// another gather holds is refused ErrDuplicateGather.
func (s *store) PutGather(ctx context.Context, g Gather) error {
	const op = "daemon.wsm.put_gather"
	fields := dlog.Context{"gather": string(g.ID), "caller": string(g.Caller), "targets": len(g.Targets)}
	if err := validateGather(g); err != nil {
		s.log.Error(op, "refused an inconsistent gather", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var held GatherID
		err := tx.QueryRowContext(ctx, `SELECT id FROM gathers WHERE idempotency_key = ?`, g.Key).Scan(&held)
		switch {
		case err == nil:
			return fmt.Errorf("%w: %s", ErrDuplicateGather, held)
		case !errors.Is(err, sql.ErrNoRows):
			return err
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO gathers (id, caller_id, caller_name, caller_dir, idempotency_key, prompt_text, created_at, deadline_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
			g.ID, g.Caller, g.CallerName, g.CallerDir, g.Key, g.PromptText, nanos(g.CreatedAt), nanos(g.DeadlineAt)); err != nil {
			return err
		}
		for i, t := range g.Targets {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO gather_targets (gather_id, position, workspace_id, name, dir, turn_id) VALUES (?, ?, ?, ?, ?, ?)`,
				g.ID, i, t.Workspace, t.Name, t.Dir, t.Turn); err != nil {
				return err
			}
		}
		return nil
	})
}

// Gather reads one gather, ErrNotFound when none has the id.
func (s *store) Gather(ctx context.Context, id GatherID) (Gather, error) {
	const op = "daemon.wsm.gather"
	var out Gather
	err := s.read(ctx, op, dlog.Context{"gather": string(id)}, func(ctx context.Context) error {
		got, err := s.readGathers(ctx, `WHERE id = ?`, id)
		if err != nil {
			return err
		}
		if len(got) == 0 {
			return ErrNotFound
		}
		out = got[0]
		return nil
	})
	return out, err
}

// GatherByKey reads the gather holding an idempotency key, if one does.
func (s *store) GatherByKey(ctx context.Context, key string) (Gather, bool, error) {
	const op = "daemon.wsm.gather_by_key"
	var out []Gather
	err := s.read(ctx, op, dlog.Context{"key": key}, func(ctx context.Context) error {
		var err error
		out, err = s.readGathers(ctx, `WHERE idempotency_key = ?`, key)
		return err
	})
	if err != nil || len(out) == 0 {
		return Gather{}, false, err
	}
	return out[0], true, nil
}

// UnsettledGathers reads every gather something is still owed about
// (Gather.Settled), oldest first: what a boot re-arms.
func (s *store) UnsettledGathers(ctx context.Context) ([]Gather, error) {
	const op = "daemon.wsm.unsettled_gathers"
	var out []Gather
	err := s.read(ctx, op, nil, func(ctx context.Context) error {
		var err error
		out, err = s.readGathers(ctx, `WHERE first_told_at IS NULL OR EXISTS (
			SELECT 1 FROM gather_targets t WHERE t.gather_id = gathers.id AND t.told_at IS NULL)`)
		return err
	})
	return out, err
}

// GathersOutstandingOn reads every gather with a target on WS still
// outstanding, oldest first.
func (s *store) GathersOutstandingOn(ctx context.Context, ws WorkspaceID) ([]Gather, error) {
	const op = "daemon.wsm.gathers_outstanding_on"
	var out []Gather
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		var err error
		out, err = s.readGathers(ctx, `WHERE id IN (
			SELECT gather_id FROM gather_targets WHERE workspace_id = ? AND resolved_at IS NULL)`, ws)
		return err
	})
	return out, err
}

// readGathers reads the gathers WHERE selects, oldest first, with their
// targets in order. It runs inside a read.
func (s *store) readGathers(ctx context.Context, where string, args ...any) ([]Gather, error) {
	rows, err := s.q(ctx).QueryContext(ctx,
		`SELECT id, caller_id, caller_name, caller_dir, idempotency_key, prompt_text, created_at, deadline_at, first_told_at, first_trigger
		 FROM gathers `+where+` ORDER BY created_at, id`, args...)
	if err != nil {
		return nil, err
	}
	var out []Gather
	for rows.Next() {
		var g Gather
		var created, deadline int64
		var toldAt sql.NullInt64
		var trigger sql.NullString
		if err := rows.Scan(&g.ID, &g.Caller, &g.CallerName, &g.CallerDir, &g.Key, &g.PromptText, &created, &deadline, &toldAt, &trigger); err != nil {
			rows.Close()
			return nil, err
		}
		g.CreatedAt, g.DeadlineAt = fromNanos(created), fromNanos(deadline)
		if toldAt.Valid {
			g.FirstTold = &GatherFirstTold{At: fromNanos(toldAt.Int64), Trigger: GatherTrigger(trigger.String)}
		}
		out = append(out, g)
	}
	if err := rows.Close(); err != nil {
		return nil, err
	}
	for i := range out {
		targets, err := s.readGatherTargets(ctx, out[i].ID)
		if err != nil {
			return nil, err
		}
		out[i].Targets = targets
	}
	return out, nil
}

// readGatherTargets reads one gather's targets, in the order named.
func (s *store) readGatherTargets(ctx context.Context, id GatherID) ([]GatherTarget, error) {
	rows, err := s.q(ctx).QueryContext(ctx,
		`SELECT workspace_id, name, dir, turn_id, resolved_at, resolution, answer_text, told_at
		 FROM gather_targets WHERE gather_id = ? ORDER BY position`, id)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []GatherTarget
	for rows.Next() {
		var t GatherTarget
		var resolvedAt, toldAt sql.NullInt64
		var resolution, text sql.NullString
		if err := rows.Scan(&t.Workspace, &t.Name, &t.Dir, &t.Turn, &resolvedAt, &resolution, &text, &toldAt); err != nil {
			return nil, err
		}
		if resolvedAt.Valid {
			t.Resolution = &GatherResolution{Kind: GatherResolutionKind(resolution.String), At: fromNanos(resolvedAt.Int64), Text: text.String}
		}
		if toldAt.Valid {
			at := fromNanos(toldAt.Int64)
			t.ToldAt = &at
		}
		out = append(out, t)
	}
	return out, rows.Err()
}

// ResolveGatherTarget records how the target on WS of gather ID resolved,
// answering whether this call resolved it: a target already resolved keeps its
// first resolution and answers false.
func (s *store) ResolveGatherTarget(ctx context.Context, id GatherID, ws WorkspaceID, r GatherResolution) (bool, error) {
	const op = "daemon.wsm.resolve_gather_target"
	fields := dlog.Context{"gather": string(id), "workspace": string(ws), "resolution": string(r.Kind)}
	if (r.Kind == GatherAnswered) != (r.Text != "") || (r.Kind != GatherAnswered && r.Kind != GatherClosed) {
		err := fmt.Errorf("wsm: a %q resolution carries text exactly when it is answered", r.Kind)
		s.log.Error(op, "refused an inconsistent gather resolution", withError(fields, err))
		return false, err
	}
	var text any
	if r.Kind == GatherAnswered {
		text = r.Text
	}
	var resolved bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE gather_targets SET resolved_at = ?, resolution = ?, answer_text = ?
			 WHERE gather_id = ? AND workspace_id = ? AND resolved_at IS NULL`,
			nanos(r.At), string(r.Kind), text, id, ws)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		resolved = n == 1
		return err
	})
	return resolved, err
}

// MarkGatherFirstTold records gather ID's first update told, and every
// resolution it carried told with it, in one transaction. A first update
// already told is left as it stands.
func (s *store) MarkGatherFirstTold(ctx context.Context, id GatherID, told GatherFirstTold) error {
	const op = "daemon.wsm.mark_gather_first_told"
	fields := dlog.Context{"gather": string(id), "trigger": string(told.Trigger)}
	if told.Trigger != GatherAllResolved && told.Trigger != GatherTimedOut {
		err := fmt.Errorf("wsm: undeclared gather trigger %q", told.Trigger)
		s.log.Error(op, "refused an undeclared first-update trigger", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE gathers SET first_told_at = ?, first_trigger = ? WHERE id = ? AND first_told_at IS NULL`,
			nanos(told.At), string(told.Trigger), id)
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil || n == 0 {
			return err
		}
		_, err = tx.ExecContext(ctx,
			`UPDATE gather_targets SET told_at = ? WHERE gather_id = ? AND resolved_at IS NOT NULL AND told_at IS NULL`,
			nanos(told.At), id)
		return err
	})
}

// MarkGatherTargetTold records the caller told the resolution of the target on
// WS of gather ID in its own later notice.
func (s *store) MarkGatherTargetTold(ctx context.Context, id GatherID, ws WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.mark_gather_target_told"
	return s.write(ctx, op, dlog.Context{"gather": string(id), "workspace": string(ws)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`UPDATE gather_targets SET told_at = ? WHERE gather_id = ? AND workspace_id = ? AND resolved_at IS NOT NULL AND told_at IS NULL`,
			nanos(at), id, ws)
		return err
	})
}
