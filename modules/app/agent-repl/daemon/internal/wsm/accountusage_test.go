package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// fullUsage is an account with every window figured.
func fullUsage(root string) AccountUsage {
	return AccountUsage{
		ConfigDir:  root,
		ObservedAt: time.Unix(1_791_294_066, 0).UTC(),
		Weekly:     &AllowanceFigures{Utilization: 0.53, ResetsAtS: 1_791_532_799, Verdict: VerdictNone},
		WeeklyOpus: &AllowanceFigures{Utilization: 0.12, ResetsAtS: 1_791_532_799,
			Verdict: VerdictRejected, VerdictResetsAtS: 1_791_532_799},
		WeeklySonnet: &AllowanceFigures{Utilization: 0.3, ResetsAtS: 1_791_532_799,
			Verdict: VerdictAllowedWarning},
		Overage: &AllowanceFigures{Utilization: 0.1, ResetsAtS: 1_793_000_000, Verdict: VerdictRejected, VerdictResetsAtS: 1_793_000_000},
	}
}

func ptr[T any](v T) *T { return &v }

func TestAccountUsageIsReadBack(t *testing.T) {
	tests := []struct {
		name  string
		usage AccountUsage
	}{
		{name: "every window figured", usage: fullUsage("/home/a/.claude")},
		{name: "no window figured", usage: AccountUsage{ConfigDir: "/home/a/.claude-work", ObservedAt: time.Unix(5, 0).UTC()}},
		{name: "the weekly window alone unfigured", usage: func() AccountUsage {
			u := fullUsage("/home/a/.claude")
			u.Weekly = nil
			return u
		}()},
		{name: "the model windows unfigured", usage: func() AccountUsage {
			u := fullUsage("/home/a/.claude")
			u.WeeklyOpus, u.WeeklySonnet = nil, nil
			return u
		}()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			err := s.SetAccountUsage(context.Background(), tt.usage)

			// Assert
			if err != nil {
				t.Fatalf("SetAccountUsage: %v", err)
			}
			got, err := s.AccountUsages(context.Background())
			if err != nil {
				t.Fatalf("AccountUsages: %v", err)
			}
			if len(got) != 1 || !reflect.DeepEqual(got[0], tt.usage) {
				t.Fatalf("AccountUsages = %+v, want [%+v]", got, tt.usage)
			}
		})
	}
}

func TestAccountUsageReplacesTheRootsPreviousRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	newer := fullUsage("/home/a/.claude")
	newer.Weekly.Utilization = 0.9

	// Act
	err := s.SetAccountUsage(context.Background(), newer)

	// Assert
	if err != nil {
		t.Fatalf("SetAccountUsage: %v", err)
	}
	got, _ := s.AccountUsages(context.Background())
	if len(got) != 1 || got[0].Weekly.Utilization != 0.9 {
		t.Fatalf("AccountUsages = %+v, want the one root's newer figures", got)
	}
}

func TestAccountUsageKeepsOneRowPerRoot(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}

	// Act
	err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude-work"))

	// Assert
	if err != nil {
		t.Fatalf("SetAccountUsage: %v", err)
	}
	got, _ := s.AccountUsages(context.Background())
	if len(got) != 2 || got[0].ConfigDir != "/home/a/.claude" || got[1].ConfigDir != "/home/a/.claude-work" {
		t.Fatalf("AccountUsages = %+v, want both roots in order", got)
	}
}

func TestSetAccountUsageRefusesAnUnnamedRoot(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.SetAccountUsage(context.Background(), fullUsage(""))

	// Assert
	if err == nil {
		t.Fatal("SetAccountUsage stored usage for no account root")
	}
	if !loggedOperation(log, "daemon.wsm.set_account_usage", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestSetAccountUsageRefusesAnUnknownVerdict(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	usage := fullUsage("/home/a/.claude")
	usage.Weekly.Verdict = "maybe"

	// Act
	err := s.SetAccountUsage(context.Background(), usage)

	// Assert
	if err == nil {
		t.Fatal("SetAccountUsage stored a verdict outside the vocabulary")
	}
	if !loggedOperation(log, "daemon.wsm.set_account_usage", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestAccountUsagesRefusesAHalfStoredWindow(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET weekly_verdict = NULL`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "weekly" {
		t.Fatalf("AccountUsages = %v, want a DecodeError naming the weekly window", err)
	}
	if !loggedOperation(log, "daemon.wsm.account_usages", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestAStoredVerdictWithNoResetReloadsAsAnUnknownReset(t *testing.T) {
	// Arrange: a row as the build before layout 72 leaves it, with no verdict
	// reset beside a figured window.
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET weekly_opus_verdict_resets_at_s = NULL`); err != nil {
		t.Fatalf("clear the verdict reset: %v", err)
	}

	// Act
	got, err := s.AccountUsages(context.Background())

	// Assert
	if err != nil || len(got) != 1 || got[0].WeeklyOpus.VerdictResetsAtS != 0 || got[0].WeeklyOpus.Verdict != VerdictRejected {
		t.Fatalf("AccountUsages = %+v, %v, want the opus verdict with an unknown (0) reset", got, err)
	}
}

func TestAccountUsagesRefusesAVerdictResetForAnUnfiguredWindow(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	usage := fullUsage("/home/a/.claude")
	usage.WeeklySonnet = nil
	if err := s.SetAccountUsage(context.Background(), usage); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET weekly_sonnet_verdict_resets_at_s = 5`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "weekly_sonnet" {
		t.Fatalf("AccountUsages = %v, want a DecodeError naming the weekly_sonnet window", err)
	}
	if !loggedOperation(log, "daemon.wsm.account_usages", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestSetAccountUsageRefusesAnUnknownModelWindowVerdict(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	usage := fullUsage("/home/a/.claude")
	usage.WeeklyOpus.Verdict = "maybe"

	// Act
	err := s.SetAccountUsage(context.Background(), usage)

	// Assert
	if err == nil {
		t.Fatal("SetAccountUsage accepted an unknown verdict on the opus window")
	}
	if !loggedOperation(log, "daemon.wsm.set_account_usage", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
	if got, _ := s.AccountUsages(context.Background()); len(got) != 0 {
		t.Fatalf("AccountUsages = %+v, want nothing stored", got)
	}
}

// THE WINDOW LIST IS THE ONE SHAPE every statement walks: each named window
// has its four columns in the table, and the usage hands one figure slot per
// name, so a window added to one but not the other fails here.
func TestEveryStoredWindowHasItsColumnsAndItsSlot(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	var usage AccountUsage

	// Act
	slots := usage.windows()

	// Assert
	if len(slots) != len(accountUsageWindowNames) {
		t.Fatalf("windows() = %d slots, want one per name (%d)", len(slots), len(accountUsageWindowNames))
	}
	for _, name := range accountUsageWindowNames {
		for _, col := range windowColumns(name) {
			if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('account_usage') WHERE name = ?`, col); got != 1 {
				t.Fatalf("column %s is missing from account_usage", col)
			}
		}
	}
}

func TestAccountUsagesRefusesAStoredUnknownVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET weekly_opus_verdict = 'maybe'`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "weekly_opus" {
		t.Fatalf("AccountUsages = %v, want a DecodeError naming the weekly_opus window", err)
	}
}

func TestAccountUsagesIsEmptyOnAFreshStore(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.AccountUsages(context.Background())

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("AccountUsages = %+v, %v, want none", got, err)
	}
}

func TestTheMigrationAddsTheModelWindowAndVerdictResetColumns(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 71)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-71 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('account_usage')
WHERE name LIKE 'weekly_opus_%' OR name LIKE 'weekly_sonnet_%' OR name LIKE '%_verdict_resets_at_s'`)
	// Four columns per model window (its verdict reset among them), plus a
	// verdict reset for each of the two windows before them that layout 91
	// keeps (weekly, overage).
	if got != 10 {
		t.Fatalf("columns the migrations left = %d, want 10", got)
	}
}

// TestTheMigrationAddsTheAccountUsage pins the layout-24 step.
func TestTheMigrationAddsTheAccountUsage(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 23)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-23 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'account_usage'`); got != 1 {
		t.Fatalf("account_usage tables after the migration = %d, want 1", got)
	}
}

// accountUsageFreshColumns is account_usage's columns once layout 91 has run,
// in the order the migrations leave them.
const accountUsageFreshColumns = "config_dir,observed_at,weekly_utilization,weekly_resets_at_s,weekly_verdict," +
	"overage_utilization,overage_resets_at_s,overage_verdict," +
	"weekly_opus_utilization,weekly_opus_resets_at_s,weekly_opus_verdict," +
	"weekly_sonnet_utilization,weekly_sonnet_resets_at_s,weekly_sonnet_verdict," +
	"weekly_verdict_resets_at_s,weekly_opus_verdict_resets_at_s,weekly_sonnet_verdict_resets_at_s,overage_verdict_resets_at_s"

// TestTheMigrationDropsTheUnproducedAccountUsageColumns pins the layout-91
// step: the five-hour window, the seat's spend, every sample stamp and
// no_allowance leave the table.
func TestTheMigrationDropsTheUnproducedAccountUsageColumns(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 90)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-90 database: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert
	if got := tableColumns(t, path, "account_usage"); got != accountUsageFreshColumns {
		t.Fatalf("account_usage columns after the migration = %q, want %q", got, accountUsageFreshColumns)
	}
}

func TestTheMigrationKeepsTheStandingWeeklyFigures(t *testing.T) {
	// Arrange: a layout-90 row holding a sampled five-hour and weekly figure.
	path := fixtureAt(t, 90)
	execRaw(t, path, `INSERT INTO account_usage (config_dir, observed_at, no_allowance,
  session_utilization, session_resets_at_s, session_sampled_at_ms, session_verdict,
  weekly_utilization, weekly_resets_at_s, weekly_sampled_at_ms, weekly_verdict, weekly_verdict_resets_at_s)
VALUES ('/home/a/.claude', 5, 0, 0.44, 1791299399, 1791294066973, 'allowed', 0.53, 1791532799, 1791294066973, 'rejected', 1791532799)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-90 database: %v", err)
	}
	defer handle.Close()
	got, err := handle.(*store).AccountUsages(context.Background())

	// Assert
	want := AccountUsage{ConfigDir: "/home/a/.claude", ObservedAt: fromNanos(5),
		Weekly: &AllowanceFigures{Utilization: 0.53, ResetsAtS: 1_791_532_799, Verdict: VerdictRejected, VerdictResetsAtS: 1_791_532_799}}
	if err != nil || len(got) != 1 || !reflect.DeepEqual(got[0], want) {
		t.Fatalf("AccountUsages = %+v, %v, want [%+v]", got, err, want)
	}
}

func TestAMigratedAccountUsageTableMatchesAFreshOne(t *testing.T) {
	// Arrange: one file migrated from before the table existed, one created
	// at this build's layout.
	migrated := fixtureAt(t, 23)
	handle, err := Open(context.Background(), migrated, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-23 database: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	fresh := filepath.Join(t.TempDir(), "fresh.db")
	handle, err = Open(context.Background(), fresh, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open a fresh database: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	got, want := tableColumns(t, migrated, "account_usage"), tableColumns(t, fresh, "account_usage")

	// Assert
	if got != want {
		t.Fatalf("migrated account_usage columns = %q, a fresh file's = %q; the two routes drifted", got, want)
	}
}

// readOnlyUsageStore opens a READ-ONLY handle on a fresh file, as a joining
// successor does, with a logger the test reads.
func readOnlyUsageStore(t *testing.T) (DB, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	ro, err := OpenReadOnly(context.Background(), writableStore(t), WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })
	return ro, log
}

func TestAccountUsageOnAReadOnlyHandleIsHeldNotRefused(t *testing.T) {
	// Arrange
	ro, log := readOnlyUsageStore(t)

	// Act
	err := ro.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude"))

	// Assert
	if err != nil {
		t.Fatalf("SetAccountUsage on a read-only handle = %v, want nil: the joining successor's usage is held, not refused", err)
	}
	for _, r := range log.Records() {
		if r.Level == "error" {
			t.Fatalf("a held usage write recorded an ERROR: %+v", r)
		}
	}
}

func TestAccountUsageHeldOnAReadOnlyHandleIsWrittenAtThePromotion(t *testing.T) {
	// Arrange
	ro, _ := readOnlyUsageStore(t)
	want := fullUsage("/home/a/.claude")
	if err := ro.SetAccountUsage(context.Background(), want); err != nil {
		t.Fatalf("SetAccountUsage: %v", err)
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, err := ro.AccountUsages(context.Background())
	if err != nil {
		t.Fatalf("AccountUsages: %v", err)
	}
	if len(got) != 1 || !reflect.DeepEqual(got[0], want) {
		t.Fatalf("AccountUsages after the promotion = %+v, want [%+v]", got, want)
	}
}

func TestAccountUsageHeldTwiceForOneRootWritesTheLatest(t *testing.T) {
	// Arrange
	ro, _ := readOnlyUsageStore(t)
	older := fullUsage("/home/a/.claude")
	newer := fullUsage("/home/a/.claude")
	newer.ObservedAt = older.ObservedAt.Add(time.Minute)
	newer.Weekly.Utilization = 0.61
	for _, u := range []AccountUsage{older, newer} {
		if err := ro.SetAccountUsage(context.Background(), u); err != nil {
			t.Fatalf("SetAccountUsage: %v", err)
		}
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, err := ro.AccountUsages(context.Background())
	if err != nil {
		t.Fatalf("AccountUsages: %v", err)
	}
	if len(got) != 1 || !reflect.DeepEqual(got[0], newer) {
		t.Fatalf("AccountUsages after the promotion = %+v, want the latest held [%+v]", got, newer)
	}
}

func TestAccountUsageIsNotReadBackBeforeThePromotion(t *testing.T) {
	// Arrange
	ro, _ := readOnlyUsageStore(t)

	// Act
	if err := ro.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("SetAccountUsage: %v", err)
	}

	// Assert: the read-only handle changed nothing.
	got, err := ro.AccountUsages(context.Background())
	if err != nil {
		t.Fatalf("AccountUsages: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("AccountUsages before the promotion = %+v, want none: a read-only handle changes nothing", got)
	}
}
