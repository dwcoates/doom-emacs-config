package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// accountUsageDDL is the last usage figures the daemon read for each ACCOUNT
// ROOT (a Claude config dir), as a fresh file declares the table, so a
// restarted daemon draws every workspace's usage line at once instead of an
// empty cell until a session speaks (owner ruling, 2026-10-06: the usage is
// the account's, and the activity cell is never empty).
//
// ONE ROW PER ACCOUNT ROOT, keyed by its path, because the usage belongs to
// the account and not to any workspace. Each window's three figure columns
// (utilization, reset, verdict) are NULL together when the window was never
// figured -- a figure nobody reported is never invented on reload -- and a row
// whose window is half NULL is a corrupt record (DecodeError). The verdict's
// own reset is NULL on its own when unknown.
//
// Every figure comes from a vendor rate-limit event: the account's usage is
// never sampled, so there is no per-seat spend, no five-hour window and no
// sample stamp to keep. observed_at is when the daemon last filed evidence for
// the account (unix nanos).
//
// A FILE OLDER THAN LAYOUT 91 REACHES THIS SHAPE BY STEPS: layout 24 created
// the table (accountUsageLayout24DDL), layouts 25 and 72 added columns
// (accountUsageSeatDDL, accountUsageWindowsDDL), and layout 91 dropped the
// ones nothing produces (accountUsageDropUnproducedDDL). The columns are
// declared here in the order those steps leave them, and
// TestAMigratedAccountUsageTableMatchesAFreshOne keeps the two routes to one
// shape.
const accountUsageDDL = `
CREATE TABLE account_usage (
  config_dir    TEXT PRIMARY KEY,
  observed_at   INTEGER NOT NULL,
  weekly_utilization   REAL,
  weekly_resets_at_s   INTEGER,
  weekly_verdict       TEXT,
  overage_utilization  REAL,
  overage_resets_at_s  INTEGER,
  overage_verdict      TEXT,
  weekly_opus_utilization REAL,
  weekly_opus_resets_at_s INTEGER,
  weekly_opus_verdict TEXT,
  weekly_sonnet_utilization REAL,
  weekly_sonnet_resets_at_s INTEGER,
  weekly_sonnet_verdict TEXT,
  weekly_verdict_resets_at_s INTEGER,
  weekly_opus_verdict_resets_at_s INTEGER,
  weekly_sonnet_verdict_resets_at_s INTEGER,
  overage_verdict_resets_at_s INTEGER
);
`

// accountUsageLayout24DDL is the layout-24 step exactly as it shipped: the
// table with the five-hour session window, the sample stamps and the
// no_allowance flag. Layout 91 drops what nothing produces
// (accountUsageDropUnproducedDDL); the text is kept because a file older
// than layout 24 still migrates through it.
const accountUsageLayout24DDL = `
CREATE TABLE account_usage (
  config_dir    TEXT PRIMARY KEY,
  observed_at   INTEGER NOT NULL,
  no_allowance  INTEGER NOT NULL,
  session_utilization  REAL,
  session_resets_at_s  INTEGER,
  session_sampled_at_ms INTEGER,
  session_verdict      TEXT,
  weekly_utilization   REAL,
  weekly_resets_at_s   INTEGER,
  weekly_sampled_at_ms INTEGER,
  weekly_verdict       TEXT,
  overage_utilization  REAL,
  overage_resets_at_s  INTEGER,
  overage_sampled_at_ms INTEGER,
  overage_verdict      TEXT
);
`

// accountUsageSeatDDL is the layout-25 addition: a per-seat account's spend
// (owner ruling, 2026-10-06: the usage line is chosen by the account's billing
// mode). The seat's allotment, currency and sample stamp are NULL together
// when the account is not billed per seat; its month-to-date spend is NULL on
// its own while the vendor has reported none.
//
// THE `no_allowance` COLUMN IS RETIRED BY THIS STEP. This build never reads
// it and writes it 0, so a row the build before it stored with
// no_allowance=1 reloads as unobserved and re-resolves on the account's next
// sample. Dropping the column is a later breaking step; this one is additive,
// so the build before it keeps working across it.
const accountUsageSeatDDL = `
ALTER TABLE account_usage ADD COLUMN seat_allotment_minor INTEGER;
ALTER TABLE account_usage ADD COLUMN seat_spent_minor INTEGER;
ALTER TABLE account_usage ADD COLUMN seat_currency TEXT;
ALTER TABLE account_usage ADD COLUMN seat_sampled_at_ms INTEGER;
`

// accountUsageWindowsDDL is the layout-72 addition: the per-model seven-day
// windows (Opus, Sonnet) beside the account-wide one, and, for every window,
// the reset of the window INSTANCE its verdict is about, so a verdict on a
// window that has since reset is not drawn beside the next instance's figure
// after a restart.
//
// IT IS ADDITIVE. The previous build's upsert names none of these columns:
// a row it inserts leaves them NULL (no per-model window, a verdict reset
// unknown), and a row it updates keeps their values, which the next write of
// this build replaces. A NULL verdict reset is legal on its own and decodes
// as unknown (0); the per-model windows' four figure columns are NULL or set
// together, like every other window's.
const accountUsageWindowsDDL = `
ALTER TABLE account_usage ADD COLUMN weekly_opus_utilization REAL;
ALTER TABLE account_usage ADD COLUMN weekly_opus_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_opus_sampled_at_ms INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_opus_verdict TEXT;
ALTER TABLE account_usage ADD COLUMN weekly_sonnet_utilization REAL;
ALTER TABLE account_usage ADD COLUMN weekly_sonnet_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_sonnet_sampled_at_ms INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_sonnet_verdict TEXT;
ALTER TABLE account_usage ADD COLUMN session_verdict_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_verdict_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_opus_verdict_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN weekly_sonnet_verdict_resets_at_s INTEGER;
ALTER TABLE account_usage ADD COLUMN overage_verdict_resets_at_s INTEGER;
`

// accountUsageDropUnproducedDDL is the layout-91 step: the columns nothing
// produces leave the table. The account's usage is never sampled (2026-10-30),
// so no per-seat spend, no sample stamp and no five-hour figure is ever read,
// and the owner ruled the five-hour window gone: it is neither drawn nor
// restored. no_allowance has been written 0 and never read since layout 25.
//
// The step is BREAKING: the build before it names every one of these columns
// in its upsert, and no_allowance is NOT NULL with no default, so that build's
// writes fail once they are gone. Dropping them loses nothing this build
// reads.
const accountUsageDropUnproducedDDL = `
ALTER TABLE account_usage DROP COLUMN no_allowance;
ALTER TABLE account_usage DROP COLUMN session_utilization;
ALTER TABLE account_usage DROP COLUMN session_resets_at_s;
ALTER TABLE account_usage DROP COLUMN session_sampled_at_ms;
ALTER TABLE account_usage DROP COLUMN session_verdict;
ALTER TABLE account_usage DROP COLUMN session_verdict_resets_at_s;
ALTER TABLE account_usage DROP COLUMN weekly_sampled_at_ms;
ALTER TABLE account_usage DROP COLUMN overage_sampled_at_ms;
ALTER TABLE account_usage DROP COLUMN weekly_opus_sampled_at_ms;
ALTER TABLE account_usage DROP COLUMN weekly_sonnet_sampled_at_ms;
ALTER TABLE account_usage DROP COLUMN seat_allotment_minor;
ALTER TABLE account_usage DROP COLUMN seat_spent_minor;
ALTER TABLE account_usage DROP COLUMN seat_currency;
ALTER TABLE account_usage DROP COLUMN seat_sampled_at_ms;
`

// AllowanceVerdict is the vendor's last verdict on one allowance window, as
// the account_usage row stores it. Empty is "no verdict observed yet".
type AllowanceVerdict string

// The verdicts, spelled as the rows store them.
const (
	VerdictNone           AllowanceVerdict = ""
	VerdictAllowed        AllowanceVerdict = "allowed"
	VerdictAllowedWarning AllowanceVerdict = "allowed_warning"
	VerdictRejected       AllowanceVerdict = "rejected"
)

// valid reports whether v is one of the stored verdicts.
func (v AllowanceVerdict) valid() bool {
	switch v {
	case VerdictNone, VerdictAllowed, VerdictAllowedWarning, VerdictRejected:
		return true
	}
	return false
}

// AllowanceFigures is one figured allowance window.
type AllowanceFigures struct {
	// Utilization is the drawn fraction, 0..1.
	Utilization float64
	// ResetsAtS is when the window resets, epoch seconds.
	ResetsAtS int64
	// Verdict is the vendor's last verdict on the window.
	Verdict AllowanceVerdict
	// VerdictResetsAtS is the reset, epoch seconds, of the window instance
	// the verdict is about; 0 when unknown.
	VerdictResetsAtS int64
}

// AccountUsage is the last usage evidence the daemon read for one account
// root: the allowance windows the vendor's rate-limit events figured.
type AccountUsage struct {
	// ConfigDir is the account root's path.
	ConfigDir string
	// ObservedAt is when the daemon last filed evidence for the account.
	ObservedAt time.Time
	// Weekly, WeeklyOpus, WeeklySonnet and Overage are the figured windows,
	// nil when a window was never figured.
	Weekly       *AllowanceFigures
	WeeklyOpus   *AllowanceFigures
	WeeklySonnet *AllowanceFigures
	Overage      *AllowanceFigures
}

// SetAccountUsage records an account root's usage evidence, replacing what
// the root had. An empty root or a verdict outside the vocabulary is a caller
// defect, refused loudly rather than persisted.
//
// A READ-ONLY HANDLE HOLDS THE EVIDENCE INSTEAD, latest per root, and the
// promotion writes it. A read-only handle is the joining successor's
// ordinary state, not a failure: the incumbent is still the writer, yet the
// successor's footer files every workspace's usage as it boots. Refusing
// that at ERROR, as an ordinary write on a read-only handle is refused,
// made every handover log two ERRORs per account root and lose the
// evidence the successor had filed by the time it became the writer.
func (s *store) SetAccountUsage(ctx context.Context, usage AccountUsage) error {
	const op = "daemon.wsm.set_account_usage"
	fields := dlog.Context{"config_dir": usage.ConfigDir}
	if err := usage.validate(); err != nil {
		s.log.Error(op, "refused account usage that cannot be stored", withError(fields, err))
		return err
	}
	s.mu.RLock()
	defer s.mu.RUnlock()
	if s.ReadOnly() {
		s.holdUsageLocked(usage)
		s.log.Info(op, "held the account's usage until this handle writes; the incumbent is still the writer", fields)
		return nil
	}
	return s.writeAccountUsage(ctx, op, fields, usage)
}

// holdUsageLocked keeps usage as its root's latest held evidence. The caller
// holds mu (read side); the held map has its own exclusion through heldMu.
func (s *store) holdUsageLocked(usage AccountUsage) {
	s.heldMu.Lock()
	defer s.heldMu.Unlock()
	if s.heldUsage == nil {
		s.heldUsage = make(map[string]AccountUsage)
	}
	s.heldUsage[usage.ConfigDir] = usage
}

// writeHeldUsageLocked writes every held account usage and forgets it. The
// caller holds mu EXCLUSIVELY and the handle already writes. A failed write
// is recorded at ERROR by the write itself and is not held again: the next
// evidence the footer files for the root writes it afresh.
func (s *store) writeHeldUsageLocked(ctx context.Context) {
	const op = "daemon.wsm.set_account_usage"
	s.heldMu.Lock()
	held := s.heldUsage
	s.heldUsage = nil
	s.heldMu.Unlock()
	for _, usage := range held {
		fields := dlog.Context{"config_dir": usage.ConfigDir, "held": true}
		if err := s.writeAccountUsage(ctx, op, fields, usage); err == nil {
			s.log.Info(op, "wrote the account's usage held while this handle was read-only", fields)
		}
	}
}

// accountUsageWindowNames is every allowance window the row stores, in
// column order: each contributes the four columns windowColumns names. The
// one list the upsert, the select, validation and decoding all walk, so no
// window can be written without being read.
var accountUsageWindowNames = []string{"weekly", "weekly_opus", "weekly_sonnet", "overage"}

// windowColumns is one window's columns, in the order windowArgs and
// storedWindow.dest lay them out.
func windowColumns(name string) []string {
	return []string{name + "_utilization", name + "_resets_at_s", name + "_verdict", name + "_verdict_resets_at_s"}
}

// accountUsageColumns is every column the row's upsert writes and its
// select reads, in order.
func accountUsageColumns() []string {
	cols := []string{"config_dir", "observed_at"}
	for _, name := range accountUsageWindowNames {
		cols = append(cols, windowColumns(name)...)
	}
	return cols
}

// accountUsageUpsert is the row's insert-or-replace, built from
// accountUsageColumns.
func accountUsageUpsert() string {
	cols := accountUsageColumns()
	marks := make([]string, len(cols))
	sets := make([]string, 0, len(cols)-1)
	for i, col := range cols {
		marks[i] = "?"
		if col != "config_dir" {
			sets = append(sets, col+" = excluded."+col)
		}
	}
	return "INSERT INTO account_usage (" + strings.Join(cols, ", ") + ")\nVALUES (" + strings.Join(marks, ", ") +
		")\nON CONFLICT(config_dir) DO UPDATE SET " + strings.Join(sets, ", ")
}

// accountUsageSelect is the select of every stored root, built from
// accountUsageColumns.
func accountUsageSelect() string {
	return "SELECT " + strings.Join(accountUsageColumns(), ", ") + " FROM account_usage ORDER BY config_dir"
}

// windows is the usage's windows in accountUsageWindowNames order.
func (u *AccountUsage) windows() []**AllowanceFigures {
	return []**AllowanceFigures{&u.Weekly, &u.WeeklyOpus, &u.WeeklySonnet, &u.Overage}
}

// writeAccountUsage is the row's upsert, through the package's one writer.
func (s *store) writeAccountUsage(ctx context.Context, op string, fields dlog.Context, usage AccountUsage) error {
	args := []any{usage.ConfigDir, usage.ObservedAt.UnixNano()}
	for _, w := range usage.windows() {
		args = append(args, windowArgs(*w)...)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, accountUsageUpsert(), args...)
		return err
	})
}

// validate refuses what the table cannot faithfully hold.
func (u AccountUsage) validate() error {
	if u.ConfigDir == "" {
		return fmt.Errorf("wsm: account usage names no account root")
	}
	for _, w := range u.windows() {
		if *w != nil && !(*w).Verdict.valid() {
			return fmt.Errorf("wsm: invalid allowance verdict %q", (*w).Verdict)
		}
	}
	return nil
}

// windowArgs is one window's four columns, all NULL for an unfigured window.
// An unknown verdict reset (0) is stored NULL.
func windowArgs(w *AllowanceFigures) []any {
	if w == nil {
		return []any{nil, nil, nil, nil}
	}
	var verdictResetsAtS any
	if w.VerdictResetsAtS != 0 {
		verdictResetsAtS = w.VerdictResetsAtS
	}
	return []any{w.Utilization, w.ResetsAtS, string(w.Verdict), verdictResetsAtS}
}

// AccountUsages loads every account root's usage evidence, all-or-nothing,
// ordered by root. A window whose columns are partly NULL, or a verdict
// outside the vocabulary, is a corrupt record (DecodeError).
func (s *store) AccountUsages(ctx context.Context) ([]AccountUsage, error) {
	var out []AccountUsage
	err := s.read(ctx, "daemon.wsm.account_usages", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, accountUsageSelect())
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				usage    AccountUsage
				observed int64
				windows  = make([]storedWindow, len(accountUsageWindowNames))
			)
			dest := []any{&usage.ConfigDir, &observed}
			for i := range windows {
				dest = append(dest, windows[i].dest()...)
			}
			if err := rows.Scan(dest...); err != nil {
				return err
			}
			usage.ObservedAt = fromNanos(observed)
			decoded := usage.windows()
			for i, name := range accountUsageWindowNames {
				figures, err := windows[i].decode()
				if err != nil {
					return &DecodeError{Table: "account_usage", Row: usage.ConfigDir, Field: name, Err: err}
				}
				*decoded[i] = figures
			}
			if err := usage.validate(); err != nil {
				return &DecodeError{Table: "account_usage", Row: usage.ConfigDir, Field: "config_dir", Err: err}
			}
			out = append(out, usage)
		}
		return rows.Err()
	})
	return out, err
}

// storedWindow is one window's four nullable columns as scanned.
type storedWindow struct {
	utilization      sql.NullFloat64
	resetsAtS        sql.NullInt64
	verdict          sql.NullString
	verdictResetsAtS sql.NullInt64
}

// dest is the scan destinations, in column order.
func (w *storedWindow) dest() []any {
	return []any{&w.utilization, &w.resetsAtS, &w.verdict, &w.verdictResetsAtS}
}

// decode is the window's figures, nil when its three figure columns are
// NULL. The verdict reset is NULL on its own when unknown (a row the build
// before layout 72 wrote), and set with no figures it is a corrupt record.
func (w *storedWindow) decode() (*AllowanceFigures, error) {
	set := 0
	for _, valid := range []bool{w.utilization.Valid, w.resetsAtS.Valid, w.verdict.Valid} {
		if valid {
			set++
		}
	}
	switch set {
	case 0:
		if w.verdictResetsAtS.Valid {
			return nil, fmt.Errorf("a verdict reset is stored for a window with no figures")
		}
		return nil, nil
	case 3:
	default:
		return nil, fmt.Errorf("a window's columns are all set or all NULL, not %d of 3", set)
	}
	verdict := AllowanceVerdict(w.verdict.String)
	if !verdict.valid() {
		return nil, fmt.Errorf("a stored verdict is allowed, allowed_warning, rejected or empty, not %q", w.verdict.String)
	}
	return &AllowanceFigures{
		Utilization:      w.utilization.Float64,
		ResetsAtS:        w.resetsAtS.Int64,
		Verdict:          verdict,
		VerdictResetsAtS: w.verdictResetsAtS.Int64,
	}, nil
}
