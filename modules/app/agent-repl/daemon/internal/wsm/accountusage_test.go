package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// fullUsage is an account with every window figured.
func fullUsage(root string) AccountUsage {
	return AccountUsage{
		ConfigDir:  root,
		ObservedAt: time.Unix(1_791_294_066, 0).UTC(),
		Session:    &AllowanceFigures{Utilization: 0.44, ResetsAtS: 1_791_299_399, SampledAtMs: 1_791_294_066_973, Verdict: VerdictAllowed},
		Weekly:     &AllowanceFigures{Utilization: 0.53, ResetsAtS: 1_791_532_799, SampledAtMs: 1_791_294_066_973, Verdict: VerdictNone},
		Overage:    &AllowanceFigures{Utilization: 0.1, ResetsAtS: 1_793_000_000, Verdict: VerdictRejected},
	}
}

// seatUsage is a per-seat account with the given month-to-date spend.
func seatUsage(root string, spent *int64) AccountUsage {
	return AccountUsage{
		ConfigDir:  root,
		ObservedAt: time.Unix(1_791_294_066, 0).UTC(),
		Seat:       &SeatSpend{AllotmentMinor: 1_200_000, SpentMinor: spent, Currency: "USD", SampledAtMs: 1_791_294_066_973},
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
		{name: "a seat's spend", usage: seatUsage("/home/a/.claude-work", ptr(int64(22_388)))},
		{name: "a seat with no spend reported", usage: seatUsage("/home/a/.claude-work", nil)},
		{name: "the weekly window alone unfigured", usage: func() AccountUsage {
			u := fullUsage("/home/a/.claude")
			u.Weekly = nil
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
	newer.Session.Utilization = 0.9

	// Act
	err := s.SetAccountUsage(context.Background(), newer)

	// Assert
	if err != nil {
		t.Fatalf("SetAccountUsage: %v", err)
	}
	got, _ := s.AccountUsages(context.Background())
	if len(got) != 1 || got[0].Session.Utilization != 0.9 {
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
	usage.Session.Verdict = "maybe"

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

func TestAccountUsagesRefusesAStoredUnknownVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), fullUsage("/home/a/.claude")); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET session_verdict = 'maybe'`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "session" {
		t.Fatalf("AccountUsages = %v, want a DecodeError naming the session window", err)
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

func TestAccountUsageWithBothWindowsAndASeatIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	usage := fullUsage("/home/a/.claude")
	usage.Seat = seatUsage("/home/a/.claude", nil).Seat

	// Act
	err := s.SetAccountUsage(context.Background(), usage)

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.set_account_usage", "error") {
		t.Fatalf("SetAccountUsage = %v, want a logged refusal of both billing modes at once", err)
	}
}

func TestASeatWithNoCurrencyIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	usage := seatUsage("/home/a/.claude-work", nil)
	usage.Seat.Currency = ""

	// Act
	err := s.SetAccountUsage(context.Background(), usage)

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.set_account_usage", "error") {
		t.Fatalf("SetAccountUsage = %v, want a logged refusal of a currencyless seat", err)
	}
}

func TestASeatHalfStoredIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), seatUsage("/home/a/.claude-work", nil)); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET seat_currency = NULL`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "seat" {
		t.Fatalf("AccountUsages err = %v, want a DecodeError on the seat", err)
	}
}

func TestAStoredSpendWithNoAllotmentIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.SetAccountUsage(context.Background(), AccountUsage{ConfigDir: "/home/a/.claude-work", ObservedAt: time.Unix(5, 0).UTC()}); err != nil {
		t.Fatalf("seed SetAccountUsage: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE account_usage SET seat_spent_minor = 7`); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.AccountUsages(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "seat" {
		t.Fatalf("AccountUsages err = %v, want a DecodeError on the seat", err)
	}
}

func TestARowStoredWithNoAllowanceReloadsAsNothingKnown(t *testing.T) {
	// Arrange: a row as the layout-24 build stored the enterprise seat.
	s, _ := testStore(t)
	if _, err := s.db().Exec(`INSERT INTO account_usage (config_dir, observed_at, no_allowance) VALUES ('/home/a/.claude-work', 5, 1)`); err != nil {
		t.Fatalf("seed the layout-24 row: %v", err)
	}

	// Act
	got, err := s.AccountUsages(context.Background())

	// Assert
	want := AccountUsage{ConfigDir: "/home/a/.claude-work", ObservedAt: fromNanos(5)}
	if err != nil || len(got) != 1 || !reflect.DeepEqual(got[0], want) {
		t.Fatalf("AccountUsages = %+v, %v, want [%+v]", got, err, want)
	}
}

// TestTheMigrationAddsTheSeatColumns pins the layout-25 step.
func TestTheMigrationAddsTheSeatColumns(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 24)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-24 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('account_usage') WHERE name LIKE 'seat_%'`); got != 4 {
		t.Fatalf("seat columns after the migration = %d, want 4", got)
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
	newer.Session.Utilization = 0.61
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
