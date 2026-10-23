package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
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
	// VerdictResetsAtS is the reset, epoch seconds, of the window instance
	// the verdict is about; 0 when unknown.
	VerdictResetsAtS int64
}

// SeatSpend is a per-seat account's month-to-date spend against its monthly
// allotment, in minor units of one currency.
type SeatSpend struct {
	// AllotmentMinor is the seat's monthly allotment.
	AllotmentMinor int64
	// SpentMinor is the month-to-date spend, nil while the vendor has
	// reported none.
	SpentMinor *int64
	// Currency is the ISO 4217 code both amounts are in.
	Currency string
	// SampledAtMs is the shim's stamp on the sample that read it.
	SampledAtMs int64
}

// AccountUsage is the last usage evidence the daemon read for one account
// root. THE BILLING MODE IS ONE OF TWO: allowance windows (a subscription) or
// a seat's spend (Seat set); never both, which validate refuses.
type AccountUsage struct {
	// ConfigDir is the account root's path.
	ConfigDir string
	// ObservedAt is when the daemon last filed evidence for the account.
	ObservedAt time.Time
	// Session, Weekly, WeeklyOpus, WeeklySonnet and Overage are the figured
	// windows, nil when a window was never figured.
	Session      *AllowanceFigures
	Weekly       *AllowanceFigures
	WeeklyOpus   *AllowanceFigures
	WeeklySonnet *AllowanceFigures
	Overage      *AllowanceFigures
	// Seat is a per-seat account's spend, nil when the account is not known
	// to be billed per seat.
	Seat *SeatSpend
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
	fields := dlog.Context{"config_dir": usage.ConfigDir, "seat": usage.Seat != nil}
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
		fields := dlog.Context{"config_dir": usage.ConfigDir, "seat": usage.Seat != nil, "held": true}
		if err := s.writeAccountUsage(ctx, op, fields, usage); err == nil {
			s.log.Info(op, "wrote the account's usage held while this handle was read-only", fields)
		}
	}
}

// accountUsageWindowNames is every allowance window the row stores, in
// column order: each contributes the five columns windowColumns names. The
// one list the upsert, the select, validation and decoding all walk, so no
// window can be written without being read.
var accountUsageWindowNames = []string{"session", "weekly", "weekly_opus", "weekly_sonnet", "overage"}

// windowColumns is one window's columns, in the order windowArgs and
// storedWindow.dest lay them out.
func windowColumns(name string) []string {
	return []string{
		name + "_utilization", name + "_resets_at_s", name + "_sampled_at_ms",
		name + "_verdict", name + "_verdict_resets_at_s",
	}
}

// accountUsageColumns is every column the row's upsert writes and its
// select reads, in order. no_allowance is retired (layout 25): it is written
// 0 and never read, so the select starts after it.
func accountUsageColumns() []string {
	cols := []string{"config_dir", "observed_at", "no_allowance"}
	for _, name := range accountUsageWindowNames {
		cols = append(cols, windowColumns(name)...)
	}
	return append(cols, "seat_allotment_minor", "seat_spent_minor", "seat_currency", "seat_sampled_at_ms")
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
// accountUsageColumns without the retired no_allowance.
func accountUsageSelect() string {
	var cols []string
	for _, col := range accountUsageColumns() {
		if col != "no_allowance" {
			cols = append(cols, col)
		}
	}
	return "SELECT " + strings.Join(cols, ", ") + " FROM account_usage ORDER BY config_dir"
}

// windows is the usage's windows in accountUsageWindowNames order.
func (u *AccountUsage) windows() []**AllowanceFigures {
	return []**AllowanceFigures{&u.Session, &u.Weekly, &u.WeeklyOpus, &u.WeeklySonnet, &u.Overage}
}

// writeAccountUsage is the row's upsert, through the package's one writer.
func (s *store) writeAccountUsage(ctx context.Context, op string, fields dlog.Context, usage AccountUsage) error {
	// no_allowance is retired (layout 25) and always written 0.
	args := []any{usage.ConfigDir, usage.ObservedAt.UnixNano(), 0}
	for _, w := range usage.windows() {
		args = append(args, windowArgs(*w)...)
	}
	args = append(args, seatArgs(usage.Seat)...)
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
	if u.Seat != nil {
		if u.figured() {
			return fmt.Errorf("wsm: account usage carries both allowance windows and a seat's spend")
		}
		if u.Seat.Currency == "" {
			return fmt.Errorf("wsm: a seat's spend names no currency")
		}
	}
	return nil
}

// figured reports whether any allowance window is stored.
func (u *AccountUsage) figured() bool {
	for _, w := range u.windows() {
		if *w != nil {
			return true
		}
	}
	return false
}

// seatArgs is the seat's four columns, all NULL when the account is not
// billed per seat; the spend alone is NULL while none has been reported.
func seatArgs(seat *SeatSpend) []any {
	if seat == nil {
		return []any{nil, nil, nil, nil}
	}
	var spent any
	if seat.SpentMinor != nil {
		spent = *seat.SpentMinor
	}
	return []any{seat.AllotmentMinor, spent, seat.Currency, seat.SampledAtMs}
}

// windowArgs is one window's five columns, all NULL for an unfigured window.
// An unknown verdict reset (0) is stored NULL.
func windowArgs(w *AllowanceFigures) []any {
	if w == nil {
		return []any{nil, nil, nil, nil, nil}
	}
	var verdictResetsAtS any
	if w.VerdictResetsAtS != 0 {
		verdictResetsAtS = w.VerdictResetsAtS
	}
	return []any{w.Utilization, w.ResetsAtS, w.SampledAtMs, string(w.Verdict), verdictResetsAtS}
}

// AccountUsages loads every account root's usage evidence, all-or-nothing,
// ordered by root. A window whose columns are partly NULL, or a verdict
// outside the vocabulary, is a corrupt record (DecodeError).
func (s *store) AccountUsages(ctx context.Context) ([]AccountUsage, error) {
	var out []AccountUsage
	err := s.read(ctx, "daemon.wsm.account_usages", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, accountUsageSelect())
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				usage    AccountUsage
				observed int64
				windows  = make([]storedWindow, len(accountUsageWindowNames))
				seat     storedSeat
			)
			dest := []any{&usage.ConfigDir, &observed}
			for i := range windows {
				dest = append(dest, windows[i].dest()...)
			}
			dest = append(dest, seat.dest()...)
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
			if usage.Seat, err = seat.decode(); err != nil {
				return &DecodeError{Table: "account_usage", Row: usage.ConfigDir, Field: "seat", Err: err}
			}
			if err := usage.validate(); err != nil {
				return &DecodeError{Table: "account_usage", Row: usage.ConfigDir, Field: "billing_mode", Err: err}
			}
			out = append(out, usage)
		}
		return rows.Err()
	})
	return out, err
}

// storedWindow is one window's five nullable columns as scanned.
type storedWindow struct {
	utilization      sql.NullFloat64
	resetsAtS        sql.NullInt64
	sampledAtMs      sql.NullInt64
	verdict          sql.NullString
	verdictResetsAtS sql.NullInt64
}

// dest is the scan destinations, in column order.
func (w *storedWindow) dest() []any {
	return []any{&w.utilization, &w.resetsAtS, &w.sampledAtMs, &w.verdict, &w.verdictResetsAtS}
}

// decode is the window's figures, nil when its four figure columns are
// NULL. The verdict reset is NULL on its own when unknown (a row the build
// before layout 72 wrote), and set with no figures it is a corrupt record.
func (w *storedWindow) decode() (*AllowanceFigures, error) {
	set := 0
	for _, valid := range []bool{w.utilization.Valid, w.resetsAtS.Valid, w.sampledAtMs.Valid, w.verdict.Valid} {
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
	case 4:
	default:
		return nil, fmt.Errorf("a window's columns are all set or all NULL, not %d of 4", set)
	}
	verdict := AllowanceVerdict(w.verdict.String)
	if !verdict.valid() {
		return nil, fmt.Errorf("a stored verdict is allowed, allowed_warning, rejected or empty, not %q", w.verdict.String)
	}
	return &AllowanceFigures{
		Utilization:      w.utilization.Float64,
		ResetsAtS:        w.resetsAtS.Int64,
		SampledAtMs:      w.sampledAtMs.Int64,
		Verdict:          verdict,
		VerdictResetsAtS: w.verdictResetsAtS.Int64,
	}, nil
}

// storedSeat is the seat's four nullable columns as scanned.
type storedSeat struct {
	allotmentMinor sql.NullInt64
	spentMinor     sql.NullInt64
	currency       sql.NullString
	sampledAtMs    sql.NullInt64
}

// dest is the scan destinations, in column order.
func (w *storedSeat) dest() []any {
	return []any{&w.allotmentMinor, &w.spentMinor, &w.currency, &w.sampledAtMs}
}

// decode is the seat's spend, nil when the account is not billed per seat.
// The allotment, currency and stamp are set or NULL together; a spend with
// no allotment is a corrupt record.
func (w *storedSeat) decode() (*SeatSpend, error) {
	set := 0
	for _, valid := range []bool{w.allotmentMinor.Valid, w.currency.Valid, w.sampledAtMs.Valid} {
		if valid {
			set++
		}
	}
	switch set {
	case 0:
		if w.spentMinor.Valid {
			return nil, fmt.Errorf("a seat's spend is stored with no allotment")
		}
		return nil, nil
	case 3:
	default:
		return nil, fmt.Errorf("a seat's allotment, currency and stamp are all set or all NULL, not %d of 3", set)
	}
	seat := &SeatSpend{
		AllotmentMinor: w.allotmentMinor.Int64,
		Currency:       w.currency.String,
		SampledAtMs:    w.sampledAtMs.Int64,
	}
	if w.spentMinor.Valid {
		spent := w.spentMinor.Int64
		seat.SpentMinor = &spent
	}
	return seat, nil
}
