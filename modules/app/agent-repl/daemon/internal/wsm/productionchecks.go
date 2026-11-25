package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// THE PRODUCTION CHECKS (repoconfig.v1.LifecycleConfig.production_checks;
// docs/investigations/2026-11-23-production-mediator-plan.md). A repository's
// standing production mediator reviews the SPAN between two landings: from
// where the repository's last check read to, to when the next landing began
// (Mediation.LandingStartedAt). The span is owed when that landing begins and
// falls due when its deploy ends; the mediator decides it with nothing to do
// or with children to dispatch. Each child owns the log sites its findings
// cover until it lands or closes, so no site is worked by two children.

// productionChecksDDL is the layout-201 addition. IT IS ADDITIVE: new tables
// the previous build never names.
//
//   - production_mediators: each queue's production mediator, as
//     deploy_mediators holds its deploy mediator.
//   - production_checks: each span owed, keyed by its end (the landing's
//     start), and how it was decided.
//   - production_children: each child a check dispatched, and how it ended.
//   - production_child_sites: the site each open child owns, at most one
//     child per site.
//   - production_fixed_sites: each site a child owned when it landed, so a
//     later brief names the site's fix when it fires again.
const productionChecksDDL = `
CREATE TABLE production_mediators (
  repo_key     TEXT PRIMARY KEY CHECK (repo_key <> ''),
  workspace_id TEXT NOT NULL CHECK (workspace_id <> ''),
  briefed      INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX production_mediators_by_workspace ON production_mediators(workspace_id);

CREATE TABLE production_checks (
  repo_key    TEXT NOT NULL CHECK (repo_key <> ''),
  span_until  INTEGER NOT NULL,
  span_since  INTEGER NOT NULL,
  deploy_at   INTEGER NOT NULL,
  version     TEXT NOT NULL DEFAULT '',
  due_at      INTEGER,
  prompted_at INTEGER,
  turn        TEXT NOT NULL DEFAULT '',
  resumed     INTEGER NOT NULL DEFAULT 0,
  decided_at  INTEGER,
  decision    TEXT NOT NULL DEFAULT '' CHECK (decision IN ('', 'nothing_to_do', 'dispatched', 'unanswered')),
  summary     TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (repo_key, span_until),
  CHECK (span_since < span_until)
);

CREATE TABLE production_children (
  workspace_id  TEXT PRIMARY KEY CHECK (workspace_id <> ''),
  repo_key      TEXT NOT NULL CHECK (repo_key <> ''),
  span_until    INTEGER NOT NULL,
  kind          TEXT NOT NULL CHECK (kind IN ('remediation', 'investigation')),
  dispatched_at INTEGER NOT NULL,
  ordinal       INTEGER NOT NULL,
  ended_at      INTEGER,
  ending        TEXT NOT NULL DEFAULT '' CHECK (ending IN ('', 'landed', 'closed', 'folded'))
);
CREATE INDEX production_children_by_repo ON production_children(repo_key);

CREATE TABLE production_child_sites (
  repo_key     TEXT NOT NULL,
  site         TEXT NOT NULL CHECK (length(site) = 12),
  workspace_id TEXT NOT NULL,
  PRIMARY KEY (repo_key, site)
);
CREATE INDEX production_child_sites_by_workspace ON production_child_sites(workspace_id);

CREATE TABLE production_fixed_sites (
  repo_key     TEXT NOT NULL,
  site         TEXT NOT NULL CHECK (length(site) = 12),
  workspace_id TEXT NOT NULL,
  landed_at    INTEGER NOT NULL,
  PRIMARY KEY (repo_key, site, landed_at)
);
`

// ProductionCheck is one span a repository's production mediator is owed.
type ProductionCheck struct {
	// Since and Until bound the span: from where the repository's last check
	// read to, to when the next landing began. Until is the check's identity.
	Since, Until time.Time
	// Version is the build the landing installed, empty when unnamed.
	Version string
	// DueAt is when the landing's deploy ended, nil while it has not: the
	// check is told only once due.
	DueAt *time.Time
	// PromptedAt and Turn are when and in which turn the mediator was told,
	// nil and empty while it has not been.
	PromptedAt *time.Time
	Turn       TurnID
	// Resumed reports the mediator's turn on it ended once deciding nothing
	// and it was resumed in Turn to decide it.
	Resumed bool
}

// Prompted reports whether the mediator was told of the check.
func (c ProductionCheck) Prompted() bool { return c.PromptedAt != nil }

// ProductionDecision is how a production check was decided.
type ProductionDecision string

// The decisions.
const (
	ProductionNothingToDo ProductionDecision = "nothing_to_do"
	ProductionDispatched  ProductionDecision = "dispatched"
	// ProductionUnanswered is a check whose mediator ended its turn deciding
	// nothing, again after it was resumed to decide: recorded so the next
	// span is told, and the failure stands at ERROR.
	ProductionUnanswered ProductionDecision = "unanswered"
)

// ProductionChildKind is the work a production child does.
type ProductionChildKind string

// The kinds of child.
const (
	ProductionRemediation   ProductionChildKind = "remediation"
	ProductionInvestigation ProductionChildKind = "investigation"
)

// ProductionChildEnding is how a production child ended.
type ProductionChildEnding string

// The endings.
const (
	ProductionChildLanded ProductionChildEnding = "landed"
	ProductionChildClosed ProductionChildEnding = "closed"
	ProductionChildFolded ProductionChildEnding = "folded"
)

// ProductionChild is one child a production check dispatched.
type ProductionChild struct {
	Workspace WorkspaceID
	Repo      RepoKey
	// Check is the Until of the check that dispatched it.
	Check        time.Time
	Kind         ProductionChildKind
	DispatchedAt time.Time
	// Sites are the log sites it owns while open.
	Sites []string
	// Named are every log site it was given, at its dispatch or by a fold,
	// owned or not: what it was asked to fix, kept after it ends.
	Named []string
	// Ending and EndedAt are how and when it ended; empty and nil while open.
	Ending  ProductionChildEnding
	EndedAt *time.Time
	// FoldedInto is the child it was folded into, set only for a fold.
	FoldedInto *WorkspaceID
}

// Open reports whether the child has not ended.
func (c ProductionChild) Open() bool { return c.Ending == "" }

// FixedSite is a site a production child owned when it landed.
type FixedSite struct {
	Site      string
	Workspace WorkspaceID
	LandedAt  time.Time
}

// SiteOwnedError refuses a site an open child already owns, or one named
// twice in one dispatch (Holder empty, Index the second child naming it).
type SiteOwnedError struct {
	Site   string
	Holder WorkspaceID
	Index  int
}

func (e *SiteOwnedError) Error() string {
	if e.Holder != "" {
		return fmt.Sprintf("wsm: the log site %s is owned by the open child %s", e.Site, e.Holder)
	}
	return fmt.Sprintf("wsm: the log site %s is named again by child %d of the dispatch", e.Site, e.Index)
}

// ErrNoProductionCheckOwed refuses a decision on a check that is not the
// one owed, or not told yet.
var ErrNoProductionCheckOwed = errors.New("wsm: no production check is owed")

// ErrNotFoldable refuses a fold of a workspace that is not an open child of
// the mediator's repository, or a fold of a child into itself.
var ErrNotFoldable = errors.New("wsm: the workspace is not an open production child of the repository")

// OweProductionCheck owes REPO's production mediator the span that closed
// when the landing of the deploy that began AT started (the landing's start
// as Mediation.LandingStartedAt reads it), installing VERSION. The span
// starts where the repository's last deploy check read to, or where the last
// production span ended when that is later, so no two checks read the same
// record; at the deploy before this one when nothing read the repository. A
// span skipped for a pause (productionpause.go) moves that start too. It
// answers the check and ProductionSpanOwed; ProductionSpanEmpty when the span
// is empty (overlapping landings) or no earlier deploy is known to start
// from; or, owing nothing, ProductionSpanPaused for a span that ended while
// the checks are paused and ProductionSpanBeganInPause for one that began
// before the last resume.
func (s *store) OweProductionCheck(ctx context.Context, repo RepoKey, at time.Time, version string) (ProductionCheck, ProductionOwing, error) {
	const op = "daemon.wsm.owe_production_check"
	fields := dlog.Context{"repo": string(repo), "deploy_at": nanos(at)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return ProductionCheck{}, "", err
	}
	if at.IsZero() {
		err := fmt.Errorf("wsm: a production check of %s names no deploy", key)
		s.log.Error(op, "refused a production check that names no deploy", withError(fields, err))
		return ProductionCheck{}, "", err
	}
	var (
		out   ProductionCheck
		owing = ProductionSpanEmpty
	)
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		owing = ProductionSpanEmpty
		landing, err := landingStartedIn(ctx, tx, key, at)
		if err != nil {
			return err
		}
		var through, lastUntil sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT checked_through FROM merge_queue_repos WHERE repo_key = ?`, string(key)).Scan(&through); err != nil && !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		if err := tx.QueryRowContext(ctx, `SELECT MAX(span_until) FROM production_checks WHERE repo_key = ?`, string(key)).Scan(&lastUntil); err != nil {
			return err
		}
		pausedAt, resumedAt, skippedThrough, err := productionPauseIn(ctx, tx, key)
		if err != nil {
			return err
		}
		since := through
		for _, later := range []sql.NullInt64{lastUntil, skippedThrough} {
			if later.Valid && (!since.Valid || later.Int64 > since.Int64) {
				since = later
			}
		}
		// A REPOSITORY NO CHECK HAS READ starts at the deploy before this
		// one: with no deploy check, no landing is read twice.
		if !since.Valid {
			if err := tx.QueryRowContext(ctx, `SELECT MAX(started_at) FROM log_site_deploys WHERE started_at > 0 AND started_at < ?`, nanos(at)).Scan(&since); err != nil {
				return err
			}
		}
		if !since.Valid || since.Int64 >= nanos(landing) {
			return nil
		}
		// A SPAN TOUCHING A PAUSE IS SKIPPED, never owed: one ending while
		// the checks are paused, and one that began before the last resume.
		switch {
		case pausedAt.Valid:
			owing = ProductionSpanPaused
		case resumedAt.Valid && since.Int64 < resumedAt.Int64:
			owing = ProductionSpanBeganInPause
		}
		if owing != ProductionSpanEmpty {
			out = ProductionCheck{Since: fromNanos(since.Int64), Until: landing, Version: version}
			return skipProductionSpanIn(ctx, tx, key, landing)
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO production_checks (repo_key, span_until, span_since, deploy_at, version) VALUES (?, ?, ?, ?, ?)`,
			string(key), nanos(landing), since.Int64, nanos(at), version); err != nil {
			return err
		}
		out = ProductionCheck{Since: fromNanos(since.Int64), Until: landing, Version: version}
		owing = ProductionSpanOwed
		return nil
	})
	if err != nil {
		return ProductionCheck{}, "", err
	}
	return out, owing, nil
}

// DropProductionCheck forgets the span REPO owed for the deploy that began
// AT and is not yet due: the deploy was a crash's, which closes no span, so
// the span runs on into the next. It answers whether it dropped one.
func (s *store) DropProductionCheck(ctx context.Context, repo RepoKey, at time.Time) (bool, error) {
	const op = "daemon.wsm.drop_production_check"
	fields := dlog.Context{"repo": string(repo), "deploy_at": nanos(at)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return false, err
	}
	var dropped bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM production_checks WHERE repo_key = ? AND deploy_at = ? AND due_at IS NULL AND decided_at IS NULL`, string(key), nanos(at))
		if err != nil {
			return err
		}
		dropped, err = touchedRow(res)
		return err
	})
	return dropped, err
}

// MarkProductionChecksDue records that the landing's deploy ended AT: every
// check of REPO owed and not yet due falls due, to be told.
func (s *store) MarkProductionChecksDue(ctx context.Context, repo RepoKey, at time.Time) (int, error) {
	const op = "daemon.wsm.mark_production_checks_due"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return 0, err
	}
	var n int64
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE production_checks SET due_at = ? WHERE repo_key = ? AND due_at IS NULL AND decided_at IS NULL`, nanos(at), string(key))
		if err != nil {
			return err
		}
		n, err = res.RowsAffected()
		return err
	})
	return int(n), err
}

// productionCheckColumns are the columns scanProductionCheck decodes.
const productionCheckColumns = `span_since, span_until, version, due_at, prompted_at, turn, resumed`

func scanProductionCheck(row interface{ Scan(...any) error }) (ProductionCheck, error) {
	var since, until int64
	var due, prompted sql.NullInt64
	var c ProductionCheck
	if err := row.Scan(&since, &until, &c.Version, &due, &prompted, &c.Turn, &c.Resumed); err != nil {
		return ProductionCheck{}, err
	}
	c.Since, c.Until = fromNanos(since), fromNanos(until)
	if due.Valid {
		at := fromNanos(due.Int64)
		c.DueAt = &at
	}
	if prompted.Valid {
		at := fromNanos(prompted.Int64)
		c.PromptedAt = &at
	}
	return c, nil
}

// ProductionCheckOf answers the check REPO's production mediator is to
// decide next: the oldest due and undecided one, nil when none is.
func (s *store) ProductionCheckOf(ctx context.Context, repo RepoKey) (*ProductionCheck, error) {
	const op = "daemon.wsm.production_check_of"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out *ProductionCheck
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		c, err := scanProductionCheck(s.q(ctx).QueryRowContext(ctx,
			`SELECT `+productionCheckColumns+` FROM production_checks WHERE repo_key = ? AND due_at IS NOT NULL AND decided_at IS NULL ORDER BY span_until LIMIT 1`, string(key)))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out = &c
		return nil
	})
	return out, err
}

// PreviousProductionSpan answers the span of REPO's check before the one
// ending UNTIL, which the check's sites are compared against, and whether
// there is one.
func (s *store) PreviousProductionSpan(ctx context.Context, repo RepoKey, until time.Time) (ProductionCheck, bool, error) {
	const op = "daemon.wsm.previous_production_span"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return ProductionCheck{}, false, err
	}
	var (
		out   ProductionCheck
		found bool
	)
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		c, err := scanProductionCheck(s.q(ctx).QueryRowContext(ctx,
			`SELECT `+productionCheckColumns+` FROM production_checks WHERE repo_key = ? AND span_until < ? ORDER BY span_until DESC LIMIT 1`, string(key), nanos(until)))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out, found = c, true
		return nil
	})
	return out, found, err
}

// MarkProductionCheckPrompted records that REPO's mediator was told the
// check ending UNTIL at AT, in TURN. It answers false when that check is no
// longer undecided.
func (s *store) MarkProductionCheckPrompted(ctx context.Context, repo RepoKey, until, at time.Time, turn TurnID) (bool, error) {
	return s.productionCheckWrite(ctx, "daemon.wsm.mark_production_check_prompted", repo, until,
		`UPDATE production_checks SET prompted_at = ?, turn = ? WHERE repo_key = ? AND span_until = ? AND decided_at IS NULL`, nanos(at), string(turn))
}

// MarkProductionCheckResumed records that the mediator's turn on the check
// ending UNTIL ended deciding nothing, and it was resumed in TURN.
func (s *store) MarkProductionCheckResumed(ctx context.Context, repo RepoKey, until time.Time, turn TurnID) (bool, error) {
	return s.productionCheckWrite(ctx, "daemon.wsm.mark_production_check_resumed", repo, until,
		`UPDATE production_checks SET resumed = 1, turn = ? WHERE repo_key = ? AND span_until = ? AND decided_at IS NULL`, string(turn))
}

// productionCheckWrite runs QUERY, whose last two arguments are REPO's key
// and UNTIL, answering whether it touched the check.
func (s *store) productionCheckWrite(ctx context.Context, op string, repo RepoKey, until time.Time, query string, args ...any) (bool, error) {
	fields := dlog.Context{"repo": string(repo), "span_until": nanos(until)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return false, err
	}
	var marked bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, query, append(args, string(key), nanos(until))...)
		if err != nil {
			return err
		}
		marked, err = touchedRow(res)
		return err
	})
	return marked, err
}

// DecideProductionCheck records REPO's decision on the told check ending
// UNTIL, at AT, with SUMMARY, and every child it dispatched with the sites
// each owns, in one write: a site an open child already owns, or one two
// children name, refuses the whole decision (SiteOwnedError), and a check
// not owed or not told is ErrNoProductionCheckOwed.
func (s *store) DecideProductionCheck(ctx context.Context, repo RepoKey, until, at time.Time, decision ProductionDecision, summary string, children []ProductionChild) error {
	const op = "daemon.wsm.decide_production_check"
	fields := dlog.Context{"repo": string(repo), "span_until": nanos(until), "decision": string(decision), "children": len(children)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	if decision == ProductionUnanswered {
		return s.recordUnanswered(ctx, op, key, until, at, summary, fields)
	}
	if (decision == ProductionDispatched) != (len(children) > 0) {
		err := fmt.Errorf("wsm: a %s decision with %d children", decision, len(children))
		s.log.Error(op, "refused a production decision whose children disagree with it", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE production_checks SET decided_at = ?, decision = ?, summary = ? WHERE repo_key = ? AND span_until = ? AND decided_at IS NULL AND prompted_at IS NOT NULL`,
			nanos(at), string(decision), summary, string(key), nanos(until))
		if err != nil {
			return err
		}
		if touched, err := touchedRow(res); err != nil {
			return err
		} else if !touched {
			return ErrNoProductionCheckOwed
		}
		named := map[string]bool{}
		for i, child := range children {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO production_children (workspace_id, repo_key, span_until, kind, dispatched_at, ordinal) VALUES (?, ?, ?, ?, ?, ?)`,
				string(child.Workspace), string(key), nanos(until), string(child.Kind), nanos(at), i); err != nil {
				return err
			}
			for _, site := range child.Sites {
				if named[site] {
					return &SiteOwnedError{Site: site, Index: i}
				}
				named[site] = true
				if err := claimSite(ctx, tx, key, site, child.Workspace); err != nil {
					return err
				}
				if err := nameSite(ctx, tx, site, child.Workspace); err != nil {
					return err
				}
			}
		}
		return nil
	})
}

// recordUnanswered records the check of KEY ending UNTIL decided as
// unanswered at AT: its mediator, told and resumed, decided nothing.
func (s *store) recordUnanswered(ctx context.Context, op string, key RepoKey, until, at time.Time, summary string, fields dlog.Context) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE production_checks SET decided_at = ?, decision = 'unanswered', summary = ? WHERE repo_key = ? AND span_until = ? AND decided_at IS NULL AND resumed = 1`,
			nanos(at), summary, string(key), nanos(until))
		if err != nil {
			return err
		}
		if touched, err := touchedRow(res); err != nil {
			return err
		} else if !touched {
			return ErrNoProductionCheckOwed
		}
		return nil
	})
}

// nameSite records SITE as one WS was given, kept after its ownership ends.
func nameSite(ctx context.Context, tx *sql.Tx, site string, ws WorkspaceID) error {
	_, err := tx.ExecContext(ctx, `INSERT OR IGNORE INTO production_child_named_sites (workspace_id, site) VALUES (?, ?)`, string(ws), site)
	return err
}

// claimSite records WS as SITE's owner in KEY, refusing a site an open child
// already owns.
func claimSite(ctx context.Context, tx *sql.Tx, key RepoKey, site string, ws WorkspaceID) error {
	var holder string
	err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM production_child_sites WHERE repo_key = ? AND site = ?`, string(key), site).Scan(&holder)
	switch {
	case err == nil:
		return &SiteOwnedError{Site: site, Holder: WorkspaceID(holder)}
	case !errors.Is(err, sql.ErrNoRows):
		return err
	}
	_, err = tx.ExecContext(ctx, `INSERT INTO production_child_sites (repo_key, site, workspace_id) VALUES (?, ?, ?)`, string(key), site, string(ws))
	return err
}

// OwnedSites answers every site of REPO an open child owns, and its owner.
func (s *store) OwnedSites(ctx context.Context, repo RepoKey) (map[string]WorkspaceID, error) {
	const op = "daemon.wsm.owned_sites"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	type owned struct {
		site string
		ws   WorkspaceID
	}
	var rows []owned
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		var err error
		rows, err = listRows(ctx, s.q(ctx), `SELECT site, workspace_id FROM production_child_sites WHERE repo_key = ?`, func(row interface{ Scan(...any) error }) (owned, error) {
			var o owned
			return o, row.Scan(&o.site, &o.ws)
		}, string(key))
		return err
	})
	if err != nil {
		return nil, err
	}
	out := make(map[string]WorkspaceID, len(rows))
	for _, o := range rows {
		out[o.site] = o.ws
	}
	return out, nil
}

// FixedSites answers every site of REPO a child owned when it landed, the
// latest landing of each.
func (s *store) FixedSites(ctx context.Context, repo RepoKey) ([]FixedSite, error) {
	const op = "daemon.wsm.fixed_sites"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out []FixedSite
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx),
			`SELECT site, workspace_id, MAX(landed_at) FROM production_fixed_sites WHERE repo_key = ? GROUP BY site ORDER BY site`,
			func(row interface{ Scan(...any) error }) (FixedSite, error) {
				var f FixedSite
				var landed int64
				if err := row.Scan(&f.Site, &f.Workspace, &landed); err != nil {
					return FixedSite{}, err
				}
				f.LandedAt = fromNanos(landed)
				return f, nil
			}, string(key))
		return err
	})
	return out, err
}

// productionChildColumns are the columns scanProductionChild decodes.
const productionChildColumns = `workspace_id, repo_key, span_until, kind, dispatched_at, ended_at, ending, folded_into`

func scanProductionChild(row interface{ Scan(...any) error }) (ProductionChild, error) {
	var c ProductionChild
	var until, dispatched int64
	var ended sql.NullInt64
	var into sql.NullString
	if err := row.Scan(&c.Workspace, &c.Repo, &until, &c.Kind, &dispatched, &ended, &c.Ending, &into); err != nil {
		return ProductionChild{}, err
	}
	if into.Valid != (c.Ending == ProductionChildFolded) {
		return ProductionChild{}, &DecodeError{Table: "production_children", Row: string(c.Workspace), Field: "folded_into", Err: errors.New("a child names the child it was folded into exactly when it was folded")}
	}
	if into.Valid {
		id := WorkspaceID(into.String)
		c.FoldedInto = &id
	}
	c.Check, c.DispatchedAt = fromNanos(until), fromNanos(dispatched)
	if ended.Valid {
		at := fromNanos(ended.Int64)
		c.EndedAt = &at
	}
	return c, nil
}

// ProductionChildOf answers the production child WS is, with the sites it
// owns, and whether it is one.
func (s *store) ProductionChildOf(ctx context.Context, ws WorkspaceID) (ProductionChild, bool, error) {
	const op = "daemon.wsm.production_child_of"
	var (
		out   ProductionChild
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		c, err := scanProductionChild(s.q(ctx).QueryRowContext(ctx, `SELECT `+productionChildColumns+` FROM production_children WHERE workspace_id = ?`, string(ws)))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		sites, err := s.childSites(ctx, ws)
		if err != nil {
			return err
		}
		c.Sites = sites
		out, found = c, true
		return nil
	})
	return out, found, err
}

// ProductionChildrenOf answers REPO's open production children, oldest
// first and in their dispatch's order, each with the sites it owns.
func (s *store) ProductionChildrenOf(ctx context.Context, repo RepoKey) ([]ProductionChild, error) {
	return s.productionChildren(ctx, "daemon.wsm.production_children_of", repo, dlog.Context{"repo": string(repo)}, `ending = ''`)
}

// ProductionChildrenOpenSince answers REPO's production children that were
// open at any moment from SINCE on: those open now, and those that ended at
// or after it, in their dispatch's order, each with the sites it owns, the
// sites it was given, and how and when it ended.
func (s *store) ProductionChildrenOpenSince(ctx context.Context, repo RepoKey, since time.Time) ([]ProductionChild, error) {
	return s.productionChildren(ctx, "daemon.wsm.production_children_open_since", repo, dlog.Context{"repo": string(repo), "since": nanos(since)},
		`(ending = '' OR ended_at >= ?)`, nanos(since))
}

// productionChildren is the one read of REPO's production children: those
// WHERE (a condition on production_children, with ARGS) holds, oldest first
// and in their dispatch's order, each with the sites it owns and the sites
// it was given.
func (s *store) productionChildren(ctx context.Context, op string, repo RepoKey, fields dlog.Context, where string, args ...any) ([]ProductionChild, error) {
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out []ProductionChild
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx),
			`SELECT `+productionChildColumns+` FROM production_children WHERE repo_key = ? AND `+where+` ORDER BY dispatched_at, ordinal`, scanProductionChild, append([]any{string(key)}, args...)...)
		if err != nil {
			return err
		}
		for i := range out {
			if out[i].Sites, err = s.childSites(ctx, out[i].Workspace); err != nil {
				return err
			}
			if out[i].Named, err = s.namedSites(ctx, out[i].Workspace); err != nil {
				return err
			}
		}
		return nil
	})
	return out, err
}

// namedSites answers every site WS was given, in order.
func (s *store) namedSites(ctx context.Context, ws WorkspaceID) ([]string, error) {
	return listRows(ctx, s.q(ctx), `SELECT site FROM production_child_named_sites WHERE workspace_id = ? ORDER BY site`, scanSite, string(ws))
}

// childSites answers the sites WS owns, in order.
func (s *store) childSites(ctx context.Context, ws WorkspaceID) ([]string, error) {
	return listRows(ctx, s.q(ctx), `SELECT site FROM production_child_sites WHERE workspace_id = ? ORDER BY site`, scanSite, string(ws))
}

// scanSite reads one row's site.
func scanSite(row interface{ Scan(...any) error }) (string, error) {
	var site string
	return site, row.Scan(&site)
}

// FoldProductionChild moves every site FOLDED owns to SURVIVING and ends
// FOLDED as folded, at AT. Both must be open children of one repository, and
// distinct (ErrNotFoldable).
func (s *store) FoldProductionChild(ctx context.Context, folded, surviving WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.fold_production_child"
	fields := dlog.Context{"folded": string(folded), "surviving": string(surviving)}
	if folded == surviving {
		return fmt.Errorf("%w: %s is named as both sides of the fold", ErrNotFoldable, folded)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var repoFolded, repoSurviving string
		err := tx.QueryRowContext(ctx, `SELECT repo_key FROM production_children WHERE workspace_id = ? AND ending = ''`, string(folded)).Scan(&repoFolded)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("%w: %s", ErrNotFoldable, folded)
		}
		if err != nil {
			return err
		}
		err = tx.QueryRowContext(ctx, `SELECT repo_key FROM production_children WHERE workspace_id = ? AND ending = ''`, string(surviving)).Scan(&repoSurviving)
		if errors.Is(err, sql.ErrNoRows) || (err == nil && repoSurviving != repoFolded) {
			return fmt.Errorf("%w: %s", ErrNotFoldable, surviving)
		}
		if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE production_child_sites SET workspace_id = ? WHERE workspace_id = ?`, string(surviving), string(folded)); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `INSERT OR IGNORE INTO production_child_named_sites (workspace_id, site) SELECT ?, site FROM production_child_named_sites WHERE workspace_id = ?`, string(surviving), string(folded)); err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `UPDATE production_children SET ending = 'folded', ended_at = ?, folded_into = ? WHERE workspace_id = ?`, nanos(at), string(surviving), string(folded))
		return err
	})
}

// EndProductionChild records that WS ended at AT, HOW: a landed child's
// sites are recorded fixed, and every child's sites are released. It
// answers false when WS is no open production child.
func (s *store) EndProductionChild(ctx context.Context, ws WorkspaceID, how ProductionChildEnding, at time.Time) (bool, error) {
	const op = "daemon.wsm.end_production_child"
	fields := dlog.Context{"workspace": string(ws), "ending": string(how)}
	if how != ProductionChildLanded && how != ProductionChildClosed {
		err := fmt.Errorf("wsm: a production child ends landed or closed, never %q", how)
		s.log.Error(op, "refused a production child's ending", withError(fields, err))
		return false, err
	}
	var ended bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE production_children SET ending = ?, ended_at = ? WHERE workspace_id = ? AND ending = ''`, string(how), nanos(at), string(ws))
		if err != nil {
			return err
		}
		if touched, err := touchedRow(res); err != nil || !touched {
			return err
		}
		ended = true
		if how == ProductionChildLanded {
			if _, err := tx.ExecContext(ctx,
				`INSERT OR IGNORE INTO production_fixed_sites (repo_key, site, workspace_id, landed_at) SELECT repo_key, site, workspace_id, ? FROM production_child_sites WHERE workspace_id = ?`,
				nanos(at), string(ws)); err != nil {
				return err
			}
		}
		_, err = tx.ExecContext(ctx, `DELETE FROM production_child_sites WHERE workspace_id = ?`, string(ws))
		return err
	})
	return ended, err
}
