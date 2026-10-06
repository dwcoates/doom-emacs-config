package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
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

func TestAccountUsageIsReadBack(t *testing.T) {
	tests := []struct {
		name  string
		usage AccountUsage
	}{
		{name: "every window figured", usage: fullUsage("/home/a/.claude")},
		{name: "no window figured", usage: AccountUsage{ConfigDir: "/home/a/.claude-work", ObservedAt: time.Unix(5, 0).UTC(), NoAllowance: true}},
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
