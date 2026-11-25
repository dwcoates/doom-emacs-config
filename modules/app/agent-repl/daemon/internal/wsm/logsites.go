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

// THE LOG SITES THAT FIRED, PER DEPLOY (logging-contract.md "Log sites",
// docs/protobuf-design/log-site-keys.md). Every record of every sink is
// tallied into one row per (deploy, site, level, hour) by the daemon's pass
// (internal/logsites), independently of log retention, so a window of time
// is answered from here whether or not its log files still exist.
//
// A DEPLOY IS ITS START INSTANT, the one the deploy mediator names its check
// by. A record is credited to the deploy serving at its timestamp: the latest
// deploy that began at or before it. The deploy at instant 0 stands for
// everything before the first deploy this store saw begin.
//
// EACH FILE IS READ FROM ITS OWN CURSOR, keyed by the file's identity (its
// device, inode and birth instant), so a rotation's rename keeps its place
// and a new file on a reused inode starts afresh. A chunk's rows and the
// cursor's advance commit in ONE transaction that first requires the cursor
// to stand where the chunk was read from: two daemons tallying one file
// (an incumbent and its successor) can never both count the same bytes.

// logSitesDDL is the layout-193 addition: the deploys, the tally rows, and
// each file's cursor. IT IS ADDITIVE: three tables the previous build never
// names.
const logSitesDDL = `
CREATE TABLE log_site_deploys (
  started_at INTEGER PRIMARY KEY CHECK (started_at >= 0)
);
INSERT INTO log_site_deploys (started_at) VALUES (0);

CREATE TABLE log_site_tally (
  deploy_at        INTEGER NOT NULL,
  site             TEXT NOT NULL CHECK (length(site) = 12),
  level            TEXT NOT NULL CHECK (level IN ('debug', 'info', 'warn', 'error')),
  hour             INTEGER NOT NULL,
  runtime          TEXT NOT NULL CHECK (runtime <> ''),
  template         TEXT NOT NULL CHECK (template <> ''),
  operation        TEXT NOT NULL,
  count            INTEGER NOT NULL CHECK (count > 0),
  first_seen       INTEGER NOT NULL,
  last_seen        INTEGER NOT NULL,
  sample_message   TEXT NOT NULL,
  sample_workspace TEXT NOT NULL,
  sample_context   TEXT NOT NULL,
  PRIMARY KEY (deploy_at, site, level, hour)
);
CREATE INDEX log_site_tally_by_hour ON log_site_tally(hour);

CREATE TABLE log_site_cursors (
  file       TEXT PRIMARY KEY CHECK (file <> ''),
  path       TEXT NOT NULL,
  offset     INTEGER NOT NULL CHECK (offset >= 0),
  updated_at INTEGER NOT NULL
);
`

// LogSiteLevels are the levels a tally row is kept at, lowest first.
var LogSiteLevels = []string{"debug", "info", "warn", "error"}

// LogSiteCount is what one chunk adds to one (deploy, site, level, hour) row.
type LogSiteCount struct {
	Deploy   time.Time
	Site     string
	Level    string
	Hour     time.Time
	Runtime  string
	Template string
	// Operation and the Sample fields are the chunk's latest record's.
	Operation string
	Count     int64
	FirstSeen time.Time
	LastSeen  time.Time
	// SampleMessage is the latest record's rendered message.
	SampleMessage string
	// SampleWorkspace is the latest record's workspace id, empty for a central
	// record.
	SampleWorkspace string
	// SampleContext is the latest record's context, as JSON.
	SampleContext string
}

// LogSiteChunk is one read of one file: the bytes [From, To) and what they
// tallied.
type LogSiteChunk struct {
	// File is the file's identity, its cursor's key.
	File string
	// Path is where the file was read, recorded for a reader of the table.
	Path   string
	From   int64
	To     int64
	Counts []LogSiteCount
	At     time.Time
}

// LogSiteRow is one kept tally row, as LogSiteRows answers it.
type LogSiteRow struct {
	LogSiteCount
}

// LogSiteDeploys answers every kept deploy's start, oldest first. The first
// is the instant-0 deploy until enough deploys were seen to prune it.
func (s *store) LogSiteDeploys(ctx context.Context) ([]time.Time, error) {
	const op = "daemon.wsm.log_site_deploys"
	var out []time.Time
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		out = nil
		var err error
		out, err = logSiteDeploys(ctx, s.q(ctx))
		return err
	})
	return out, err
}

// LogSiteSnapshot answers, in one write transaction, every kept deploy's
// start and the instant the transaction read them at. A pass tallies only
// records written before that instant: a deploy recorded after it is
// recorded at a later instant (RecordLogSiteDeploy takes its instant inside
// its own transaction, and the two serialize), so every record the pass
// credits belongs to a deploy it knows.
func (s *store) LogSiteSnapshot(ctx context.Context, now func() time.Time) ([]time.Time, time.Time, error) {
	const op = "daemon.wsm.log_site_snapshot"
	var (
		deploys []time.Time
		at      time.Time
	)
	err := s.write(ctx, op, dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		deploys, err = logSiteDeploys(ctx, tx)
		at = now()
		return err
	})
	return deploys, at, err
}

// logSiteDeploys reads every kept deploy's start, oldest first.
func logSiteDeploys(ctx context.Context, q interface {
	QueryContext(context.Context, string, ...any) (*sql.Rows, error)
}) ([]time.Time, error) {
	rows, err := q.QueryContext(ctx, `SELECT started_at FROM log_site_deploys ORDER BY started_at`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []time.Time
	for rows.Next() {
		var at int64
		if err := rows.Scan(&at); err != nil {
			return nil, err
		}
		out = append(out, fromNanos(at))
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	if len(out) == 0 {
		return nil, errors.New("wsm: log_site_deploys keeps no deploy; it always keeps at least the latest")
	}
	return out, nil
}

// RecordLogSiteDeploy records that a deploy began at the instant NOW answers
// inside the transaction, and answers that instant. Taking the instant inside
// the transaction orders it against every pass's snapshot (LogSiteSnapshot).
func (s *store) RecordLogSiteDeploy(ctx context.Context, now func() time.Time) (time.Time, error) {
	const op = "daemon.wsm.record_log_site_deploy"
	var at time.Time
	err := s.write(ctx, op, dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		at = now()
		var latest int64
		if err := tx.QueryRowContext(ctx, `SELECT MAX(started_at) FROM log_site_deploys`).Scan(&latest); err != nil {
			return err
		}
		if nanos(at) <= latest {
			return fmt.Errorf("wsm: a deploy beginning at %s is not after the latest one, at %s", at, fromNanos(latest))
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO log_site_deploys (started_at) VALUES (?)`, nanos(at))
		return err
	})
	return at, err
}

// PruneLogSiteDeploys drops every deploy older than the KEEP most recent, and
// every tally row credited to one, answering how many rows went. ROWS AN
// UNDECIDED PRODUCTION SPAN NEEDS ARE NEVER PRUNED: spans wait on their
// mediator one at a time and can be older than KEEP deploys, so the cutoff
// is the start of the deploy that was running at the oldest undecided span's
// start (across every repository) when that is older than KEEP deploys. A
// span older than every deploy kept prunes nothing.
func (s *store) PruneLogSiteDeploys(ctx context.Context, keep int) (int64, error) {
	const op = "daemon.wsm.prune_log_site_deploys"
	fields := dlog.Context{"keep": keep}
	if keep < 1 {
		err := fmt.Errorf("wsm: keeping %d deploys of log sites would keep none", keep)
		s.log.Error(op, "refused to keep no deploy of log sites", withError(fields, err))
		return 0, err
	}
	var removed int64
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var cutoff sql.NullInt64
		err := tx.QueryRowContext(ctx,
			`SELECT started_at FROM log_site_deploys ORDER BY started_at DESC LIMIT 1 OFFSET ?`, keep-1).Scan(&cutoff)
		if errors.Is(err, sql.ErrNoRows) {
			removed = 0
			return nil
		}
		if err != nil {
			return err
		}
		var oldestSpan sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT MIN(span_since) FROM production_checks WHERE decided_at IS NULL`).Scan(&oldestSpan); err != nil {
			return err
		}
		if oldestSpan.Valid {
			var spanDeploy sql.NullInt64
			if err := tx.QueryRowContext(ctx, `SELECT MAX(started_at) FROM log_site_deploys WHERE started_at <= ?`, oldestSpan.Int64).Scan(&spanDeploy); err != nil {
				return err
			}
			if !spanDeploy.Valid {
				removed = 0
				return nil
			}
			cutoff.Int64 = min(cutoff.Int64, spanDeploy.Int64)
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM log_site_tally WHERE deploy_at < ?`, cutoff.Int64)
		if err != nil {
			return err
		}
		if removed, err = res.RowsAffected(); err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `DELETE FROM log_site_deploys WHERE started_at < ?`, cutoff.Int64)
		return err
	})
	return removed, err
}

// LogSiteCursor answers where FILE's cursor stands, and whether it has one.
func (s *store) LogSiteCursor(ctx context.Context, file string) (int64, bool, error) {
	const op = "daemon.wsm.log_site_cursor"
	var (
		offset int64
		found  bool
	)
	err := s.read(ctx, op, dlog.Context{"file": file}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT offset FROM log_site_cursors WHERE file = ?`, file).Scan(&offset)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			offset, found = 0, false
			return nil
		case err != nil:
			return err
		}
		found = true
		return nil
	})
	return offset, found, err
}

// TallyLogSites adds a chunk's counts and moves its file's cursor to the
// chunk's end, in one transaction that first requires the cursor to stand at
// the chunk's start (absent counts as 0), and answers whether it did. A
// cursor elsewhere means another pass tallied those bytes first (a
// successor's beside its incumbent's): the chunk changes nothing and answers
// false, which is no failure.
func (s *store) TallyLogSites(ctx context.Context, c LogSiteChunk) (bool, error) {
	const op = "daemon.wsm.tally_log_sites"
	fields := dlog.Context{"file": c.File, "path": c.Path, "from": c.From, "to": c.To, "counts": len(c.Counts)}
	if c.File == "" || c.From < 0 || c.To < c.From {
		err := fmt.Errorf("wsm: a log site chunk names file %q and bytes [%d, %d)", c.File, c.From, c.To)
		s.log.Error(op, "refused a log site chunk that names no file or no span", withError(fields, err))
		return false, err
	}
	applied := false
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		applied = false
		var offset int64
		err := tx.QueryRowContext(ctx, `SELECT offset FROM log_site_cursors WHERE file = ?`, c.File).Scan(&offset)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			offset = 0
		case err != nil:
			return err
		}
		if offset != c.From {
			return nil
		}
		for _, n := range c.Counts {
			if _, err := tx.ExecContext(ctx, `
INSERT INTO log_site_tally (deploy_at, site, level, hour, runtime, template, operation, count,
                            first_seen, last_seen, sample_message, sample_workspace, sample_context)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (deploy_at, site, level, hour) DO UPDATE SET
  count            = count + excluded.count,
  first_seen       = MIN(first_seen, excluded.first_seen),
  last_seen        = MAX(last_seen, excluded.last_seen),
  runtime          = CASE WHEN excluded.last_seen >= last_seen THEN excluded.runtime ELSE runtime END,
  template         = CASE WHEN excluded.last_seen >= last_seen THEN excluded.template ELSE template END,
  operation        = CASE WHEN excluded.last_seen >= last_seen THEN excluded.operation ELSE operation END,
  sample_message   = CASE WHEN excluded.last_seen >= last_seen THEN excluded.sample_message ELSE sample_message END,
  sample_workspace = CASE WHEN excluded.last_seen >= last_seen THEN excluded.sample_workspace ELSE sample_workspace END,
  sample_context   = CASE WHEN excluded.last_seen >= last_seen THEN excluded.sample_context ELSE sample_context END`,
				nanos(n.Deploy), n.Site, n.Level, nanos(n.Hour), n.Runtime, n.Template, n.Operation, n.Count,
				nanos(n.FirstSeen), nanos(n.LastSeen), n.SampleMessage, n.SampleWorkspace, n.SampleContext); err != nil {
				return err
			}
		}
		_, err = tx.ExecContext(ctx, `
INSERT INTO log_site_cursors (file, path, offset, updated_at) VALUES (?, ?, ?, ?)
ON CONFLICT (file) DO UPDATE SET path = excluded.path, offset = excluded.offset, updated_at = excluded.updated_at`,
			c.File, c.Path, c.To, nanos(c.At))
		applied = err == nil
		return err
	})
	return applied && err == nil, err
}

// ForgetLogSiteCursors drops the cursor of every file not in LIVE (the files
// a pass found), answering how many it dropped: a file that is gone is never
// read again, and its identity may be a new file's later.
func (s *store) ForgetLogSiteCursors(ctx context.Context, live []string) (int64, error) {
	const op = "daemon.wsm.forget_log_site_cursors"
	var removed int64
	// A nil list marshals as JSON null, which json_each answers as one NULL
	// row, and NOT IN a set holding NULL matches nothing: the empty set is
	// spelled [].
	if live == nil {
		live = []string{}
	}
	encoded, err := json.Marshal(live)
	if err != nil {
		s.log.Error(op, "could not encode the live log files", withError(dlog.Context{"live": len(live)}, err))
		return 0, err
	}
	err = s.write(ctx, op, dlog.Context{"live": len(live)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM log_site_cursors WHERE file NOT IN (SELECT value FROM json_each(?))`, string(encoded))
		if err != nil {
			return err
		}
		removed, err = res.RowsAffected()
		return err
	})
	return removed, err
}

// LogSiteRows answers every kept row whose hour lies in [FROM, TO) at a level
// in LEVELS.
func (s *store) LogSiteRows(ctx context.Context, from, to time.Time, levels []string) ([]LogSiteRow, error) {
	const op = "daemon.wsm.log_site_rows"
	fields := dlog.Context{"from": from, "to": to, "levels": strings.Join(levels, ",")}
	if len(levels) == 0 {
		err := errors.New("wsm: a log site read names no level")
		s.log.Error(op, "refused a log site read that names no level", withError(fields, err))
		return nil, err
	}
	var out []LogSiteRow
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		out = nil
		args := []any{nanos(from), nanos(to)}
		marks := make([]string, len(levels))
		for i, l := range levels {
			marks[i] = "?"
			args = append(args, l)
		}
		rows, err := s.q(ctx).QueryContext(ctx, `
SELECT deploy_at, site, level, hour, runtime, template, operation, count,
       first_seen, last_seen, sample_message, sample_workspace, sample_context
FROM log_site_tally WHERE hour >= ? AND hour < ? AND level IN (`+strings.Join(marks, ",")+`)
ORDER BY site, level, hour, deploy_at`, args...)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				r                                 LogSiteRow
				deploy, hour, firstSeen, lastSeen int64
			)
			if err := rows.Scan(&deploy, &r.Site, &r.Level, &hour, &r.Runtime, &r.Template, &r.Operation, &r.Count,
				&firstSeen, &lastSeen, &r.SampleMessage, &r.SampleWorkspace, &r.SampleContext); err != nil {
				return err
			}
			r.Deploy, r.Hour, r.FirstSeen, r.LastSeen = fromNanos(deploy), fromNanos(hour), fromNanos(firstSeen), fromNanos(lastSeen)
			out = append(out, r)
		}
		return rows.Err()
	})
	return out, err
}

// OldestLogSiteHour answers the hour of the oldest kept row, and whether any
// row is kept.
func (s *store) OldestLogSiteHour(ctx context.Context) (time.Time, bool, error) {
	const op = "daemon.wsm.oldest_log_site_hour"
	var (
		at    time.Time
		found bool
	)
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		var hour sql.NullInt64
		if err := s.q(ctx).QueryRowContext(ctx, `SELECT MIN(hour) FROM log_site_tally`).Scan(&hour); err != nil {
			return err
		}
		at, found = fromNanos(hour.Int64), hour.Valid
		return nil
	})
	return at, found, err
}
