package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// accountUsageDDL is the layout-24 addition: the last usage figures the daemon
// read for each ACCOUNT ROOT (a Claude config dir), so a restarted daemon
// draws every workspace's usage line at once instead of an empty cell until a
// session speaks (owner ruling, 2026-10-06: the usage is the account's, and
// the activity cell is never empty).
//
// ONE ROW PER ACCOUNT ROOT, keyed by its path, because the usage belongs to
// the account and not to any workspace. Each window's four columns are NULL
// together when the window was never figured — a figure nobody reported is
// never invented on reload — and a row whose window is half NULL is a corrupt
// record (DecodeError).
//
// observed_at is when the daemon last filed evidence for the account (unix
// nanos); each window's sampled_at_ms is the shim's own stamp on the newest
// sample filed for it (0 when only a rate-limit event figured it), which is
// what keeps an older sample from overwriting a newer one across a restart.
//
// A new table, so the step is additive: the build before it never names it.
const accountUsageDDL = `
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
	// SampledAtMs is the shim's stamp on the newest usage sample filed for
	// the window, 0 when only a rate-limit event figured it.
	SampledAtMs int64
	// Verdict is the vendor's last verdict on the window.
	Verdict AllowanceVerdict
}

// AccountUsage is the last usage evidence the daemon read for one account
// root.
type AccountUsage struct {
	// ConfigDir is the account root's path.
	ConfigDir string
	// ObservedAt is when the daemon last filed evidence for the account.
	ObservedAt time.Time
	// NoAllowance reports that the vendor's usage service answered for the
	// account with no five-hour session window.
	NoAllowance bool
	// Session, Weekly and Overage are the figured windows, nil when a window
	// was never figured.
	Session *AllowanceFigures
	Weekly  *AllowanceFigures
	Overage *AllowanceFigures
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
	fields := dlog.Context{"config_dir": usage.ConfigDir, "no_allowance": usage.NoAllowance}
	if err := usage.validate(); err != nil {
		s.log.Error(op, "refused account usage that cannot be stored", withError(fields, err))
		return err
	}
	s.mu.RLock()
	defer s.mu.RUnlock()
	if s.readOnly {
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
		fields := dlog.Context{"config_dir": usage.ConfigDir, "no_allowance": usage.NoAllowance, "held": true}
		if err := s.writeAccountUsage(ctx, op, fields, usage); err == nil {
			s.log.Info(op, "wrote the account's usage held while this handle was read-only", fields)
		}
	}
}

// writeAccountUsage is the row's upsert, through the package's one writer.
func (s *store) writeAccountUsage(ctx context.Context, op string, fields dlog.Context, usage AccountUsage) error {
	args := []any{usage.ConfigDir, usage.ObservedAt.UnixNano(), usage.NoAllowance}
	for _, w := range []*AllowanceFigures{usage.Session, usage.Weekly, usage.Overage} {
		args = append(args, windowArgs(w)...)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `
INSERT INTO account_usage (config_dir, observed_at, no_allowance,
  session_utilization, session_resets_at_s, session_sampled_at_ms, session_verdict,
  weekly_utilization, weekly_resets_at_s, weekly_sampled_at_ms, weekly_verdict,
  overage_utilization, overage_resets_at_s, overage_sampled_at_ms, overage_verdict)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(config_dir) DO UPDATE SET
  observed_at = excluded.observed_at, no_allowance = excluded.no_allowance,
  session_utilization = excluded.session_utilization, session_resets_at_s = excluded.session_resets_at_s,
  session_sampled_at_ms = excluded.session_sampled_at_ms, session_verdict = excluded.session_verdict,
  weekly_utilization = excluded.weekly_utilization, weekly_resets_at_s = excluded.weekly_resets_at_s,
  weekly_sampled_at_ms = excluded.weekly_sampled_at_ms, weekly_verdict = excluded.weekly_verdict,
  overage_utilization = excluded.overage_utilization, overage_resets_at_s = excluded.overage_resets_at_s,
  overage_sampled_at_ms = excluded.overage_sampled_at_ms, overage_verdict = excluded.overage_verdict`, args...)
		return err
	})
}

// validate refuses what the table cannot faithfully hold.
func (u AccountUsage) validate() error {
	if u.ConfigDir == "" {
		return fmt.Errorf("wsm: account usage names no account root")
	}
	for _, w := range []*AllowanceFigures{u.Session, u.Weekly, u.Overage} {
		if w != nil && !w.Verdict.valid() {
			return fmt.Errorf("wsm: invalid allowance verdict %q", w.Verdict)
		}
	}
	return nil
}

// windowArgs is one window's four columns, all NULL for an unfigured window.
func windowArgs(w *AllowanceFigures) []any {
	if w == nil {
		return []any{nil, nil, nil, nil}
	}
	return []any{w.Utilization, w.ResetsAtS, w.SampledAtMs, string(w.Verdict)}
}

// AccountUsages loads every account root's usage evidence, all-or-nothing,
// ordered by root. A window whose columns are partly NULL, or a verdict
// outside the vocabulary, is a corrupt record (DecodeError).
func (s *store) AccountUsages(ctx context.Context) ([]AccountUsage, error) {
	var out []AccountUsage
	err := s.read(ctx, "daemon.wsm.account_usages", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `
SELECT config_dir, observed_at, no_allowance,
  session_utilization, session_resets_at_s, session_sampled_at_ms, session_verdict,
  weekly_utilization, weekly_resets_at_s, weekly_sampled_at_ms, weekly_verdict,
  overage_utilization, overage_resets_at_s, overage_sampled_at_ms, overage_verdict
FROM account_usage ORDER BY config_dir`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				usage    AccountUsage
				observed int64
				windows  [3]storedWindow
			)
			dest := []any{&usage.ConfigDir, &observed, &usage.NoAllowance}
			for i := range windows {
				dest = append(dest, windows[i].dest()...)
			}
			if err := rows.Scan(dest...); err != nil {
				return err
			}
			usage.ObservedAt = fromNanos(observed)
			decoded := [3]**AllowanceFigures{&usage.Session, &usage.Weekly, &usage.Overage}
			for i, name := range []string{"session", "weekly", "overage"} {
				figures, err := windows[i].decode()
				if err != nil {
					return &DecodeError{Table: "account_usage", Row: usage.ConfigDir, Field: name, Err: err}
				}
				*decoded[i] = figures
			}
			out = append(out, usage)
		}
		return rows.Err()
	})
	return out, err
}

// storedWindow is one window's four nullable columns as scanned.
type storedWindow struct {
	utilization sql.NullFloat64
	resetsAtS   sql.NullInt64
	sampledAtMs sql.NullInt64
	verdict     sql.NullString
}

// dest is the scan destinations, in column order.
func (w *storedWindow) dest() []any {
	return []any{&w.utilization, &w.resetsAtS, &w.sampledAtMs, &w.verdict}
}

// decode is the window's figures, nil when all four columns are NULL.
func (w *storedWindow) decode() (*AllowanceFigures, error) {
	set := 0
	for _, valid := range []bool{w.utilization.Valid, w.resetsAtS.Valid, w.sampledAtMs.Valid, w.verdict.Valid} {
		if valid {
			set++
		}
	}
	switch set {
	case 0:
		return nil, nil
	case 4:
	default:
		return nil, fmt.Errorf("a window's columns are all set or all NULL, not %d of 4", set)
	}
	verdict := AllowanceVerdict(w.verdict.String)
	if !verdict.valid() {
		return nil, fmt.Errorf("a stored verdict is allowed, allowed_warning, rejected or empty, not %q", w.verdict.String)
	}
	return &AllowanceFigures{
		Utilization: w.utilization.Float64,
		ResetsAtS:   w.resetsAtS.Int64,
		SampledAtMs: w.sampledAtMs.Int64,
		Verdict:     verdict,
	}, nil
}
