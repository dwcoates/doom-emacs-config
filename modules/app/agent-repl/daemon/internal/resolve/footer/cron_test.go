package footer

import (
	"testing"
	"time"
)

func TestCronResolvesTheNextMinuteForAWildcardExpression(t *testing.T) {
	// Arrange
	from := time.Date(2026, 8, 29, 12, 30, 15, 0, time.UTC)

	// Act
	got, ok := cronNextFire("* * * * *", from)

	// Assert
	if !ok || !got.Equal(time.Date(2026, 8, 29, 12, 31, 0, 0, time.UTC)) {
		t.Fatalf("next = %v (ok=%v), want the next whole minute", got, ok)
	}
}

func TestCronResolvesAStepExpression(t *testing.T) {
	// Arrange
	from := time.Date(2026, 8, 29, 12, 31, 0, 0, time.UTC)

	// Act
	got, ok := cronNextFire("*/5 * * * *", from)

	// Assert
	if !ok || !got.Equal(time.Date(2026, 8, 29, 12, 35, 0, 0, time.UTC)) {
		t.Fatalf("next = %v (ok=%v), want 12:35", got, ok)
	}
}

func TestCronResolvesAFixedTimeOfDay(t *testing.T) {
	// Arrange
	from := time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

	// Act
	got, ok := cronNextFire("30 9 * * *", from)

	// Assert
	want := time.Date(2026, 8, 30, 9, 30, 0, 0, time.UTC)
	if !ok || !got.Equal(want) {
		t.Fatalf("next = %v (ok=%v), want %v", got, ok, want)
	}
}

func TestCronResolvesACommaList(t *testing.T) {
	// Arrange
	from := time.Date(2026, 8, 29, 12, 5, 0, 0, time.UTC)

	// Act
	got, ok := cronNextFire("0,15,30,45 * * * *", from)

	// Assert
	if !ok || got.Minute() != 15 {
		t.Fatalf("next = %v (ok=%v), want the 15th minute", got, ok)
	}
}

func TestCronResolvesARange(t *testing.T) {
	// Arrange
	from := time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

	// Act
	got, ok := cronNextFire("0 9-17 * * *", from)

	// Assert
	if !ok || got.Hour() != 13 {
		t.Fatalf("next = %v (ok=%v), want the next hour inside 9-17", got, ok)
	}
}

func TestCronResolvesADayOfWeek(t *testing.T) {
	// Arrange: a Saturday.
	from := time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

	// Act: Mondays at midnight.
	got, ok := cronNextFire("0 0 * * 1", from)

	// Assert
	if !ok || got.Weekday() != time.Monday {
		t.Fatalf("next = %v (ok=%v), want a Monday", got, ok)
	}
}

func TestCronMatchesEitherDayFieldWhenBothAreRestricted(t *testing.T) {
	// Arrange: the vixie-cron rule — a restricted day-of-month AND a restricted
	// day-of-week match on EITHER, not both.
	from := time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

	// Act: the 1st, or any Monday.
	got, ok := cronNextFire("0 0 1 * 1", from)

	// Assert
	want := time.Date(2026, 8, 31, 0, 0, 0, 0, time.UTC) // the next Monday
	if !ok || !got.Equal(want) {
		t.Fatalf("next = %v (ok=%v), want %v: either day field may match", got, ok, want)
	}
}

func TestCronRefusesASixFieldExpression(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("0 0 0 * * *", instant)

	// Assert
	if ok {
		t.Fatalf("a six-field expression resolved; the resolver takes five fields only")
	}
}

func TestCronRefusesAMacro(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("@daily", instant)

	// Assert
	if ok {
		t.Fatalf("a macro resolved; an unresolvable expression leaves next_fire UNSET")
	}
}

func TestCronRefusesANamedField(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("0 0 * JAN *", instant)

	// Assert
	if ok {
		t.Fatalf("a named month resolved; names are not in this resolver's grammar")
	}
}

func TestCronRefusesAnOutOfRangeValue(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("99 * * * *", instant)

	// Assert
	if ok {
		t.Fatalf("minute 99 resolved")
	}
}

func TestCronRefusesAnInvertedRange(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("30-10 * * * *", instant)

	// Assert
	if ok {
		t.Fatalf("an inverted range resolved")
	}
}

func TestCronRefusesAZeroStep(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("*/0 * * * *", instant)

	// Assert
	if ok {
		t.Fatalf("a zero step resolved")
	}
}

func TestCronRefusesAnEmptyExpression(t *testing.T) {
	// Arrange, Act
	_, ok := cronNextFire("", instant)

	// Assert
	if ok {
		t.Fatalf("an empty expression resolved")
	}
}

func TestCronResolvesFebruaryTwentyNinth(t *testing.T) {
	// Arrange
	from := time.Date(2026, 3, 1, 0, 0, 0, 0, time.UTC)

	// Act
	got, ok := cronNextFire("0 0 29 2 *", from)

	// Assert
	if !ok || got.Year() != 2028 || got.Month() != time.February || got.Day() != 29 {
		t.Fatalf("next = %v (ok=%v), want 2028-02-29", got, ok)
	}
}
