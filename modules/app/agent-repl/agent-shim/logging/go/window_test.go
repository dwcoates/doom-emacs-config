package logging

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// windowFixturePath is the cross-language level window contract.
const windowFixturePath = "../../../proto/vocab/log-level-window.json"

type windowFixture struct {
	WindowSeconds int `json:"window_seconds"`
	Cases         []struct {
		Name           string  `json:"name"`
		Level          *string `json:"level"`
		Until          *string `json:"until"`
		Now            int64   `json:"now"`
		Outcome        string  `json:"outcome"`
		Effective      *string `json:"effective"`
		UntilEffective *int64  `json:"until_effective"`
	} `json:"cases"`
}

func loadWindowFixture(t *testing.T) windowFixture {
	t.Helper()
	raw, err := os.ReadFile(filepath.FromSlash(windowFixturePath))
	if err != nil {
		t.Fatalf("read the level window fixture: %v", err)
	}
	var f windowFixture
	if err := json.Unmarshal(raw, &f); err != nil {
		t.Fatalf("decode the level window fixture: %v", err)
	}
	return f
}

func unsetAsEmpty(value *string) string {
	if value == nil {
		return ""
	}
	return *value
}

func TestWindowMaxMatchesTheFixture(t *testing.T) {
	// Arrange
	f := loadWindowFixture(t)

	// Act
	got := int(WindowMax / time.Second)

	// Assert
	if got != f.WindowSeconds {
		t.Errorf("WindowMax = %ds, fixture says %ds", got, f.WindowSeconds)
	}
}

func TestSelectLevelAnswersEveryFixtureCase(t *testing.T) {
	for _, tc := range loadWindowFixture(t).Cases {
		t.Run(tc.Name, func(t *testing.T) {
			// Arrange: an unset variable reads as empty in Go.
			now := time.Unix(tc.Now, 0)
			level, until := unsetAsEmpty(tc.Level), unsetAsEmpty(tc.Until)

			// Act
			sel, err := SelectLevel(level, until, now)

			// Assert
			if tc.Outcome == "refused" {
				if err == nil {
					t.Fatalf("SelectLevel(%q, %q) = %+v, want refusal", level, until, sel)
				}
				return
			}
			if err != nil {
				t.Fatalf("SelectLevel(%q, %q): %v", level, until, err)
			}
			if string(sel.Outcome) != tc.Outcome {
				t.Errorf("outcome = %s, want %s", sel.Outcome, tc.Outcome)
			}
			if sel.Level.String() != *tc.Effective {
				t.Errorf("level = %s, want %s", sel.Level, *tc.Effective)
			}
			switch {
			case tc.UntilEffective == nil && !sel.Until.IsZero():
				t.Errorf("until = %v, want none", sel.Until)
			case tc.UntilEffective != nil && sel.Until.Unix() != *tc.UntilEffective:
				t.Errorf("until = %d, want %d", sel.Until.Unix(), *tc.UntilEffective)
			}
		})
	}
}

func TestSelectionNoteIsSilentForAnHonoredWindow(t *testing.T) {
	// Arrange
	sel, err := SelectLevel("debug", "1000100", time.Unix(1000000, 0))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	_, ok := sel.Note()

	// Assert
	if ok {
		t.Error("an honored window wrote a note")
	}
}

func TestSelectionNoteIsSilentForTheDefault(t *testing.T) {
	// Arrange
	sel, err := SelectLevel("", "", time.Unix(1000000, 0))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	_, ok := sel.Note()

	// Assert
	if ok {
		t.Error("a default selection wrote a note")
	}
}

func TestSelectionNoteStatesEveryIgnoredLevel(t *testing.T) {
	tests := []struct {
		name, level, until string
	}{
		{name: "no expiry", level: "debug", until: ""},
		{name: "expired", level: "debug", until: "999999"},
		{name: "beyond window", level: "debug", until: "2000000"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			sel, err := SelectLevel(tc.level, tc.until, time.Unix(1000000, 0))
			if err != nil {
				t.Fatal(err)
			}

			// Act
			message, ok := sel.Note()

			// Assert
			if !ok || message == "" {
				t.Errorf("outcome %s wrote no note", sel.Outcome)
			}
		})
	}
}

// fakeClock is a settable clock; tests move it instead of sleeping.
type fakeClock struct{ at time.Time }

func (c *fakeClock) now() time.Time { return c.at }

func honoredWindow(t *testing.T, clock *fakeClock) *Window {
	t.Helper()
	sel, err := SelectLevel("debug", "1000300", clock.at)
	if err != nil {
		t.Fatal(err)
	}
	return NewWindow(sel, clock.now)
}

func TestWindowAdmitsDebugInsideTheWindow(t *testing.T) {
	// Arrange
	clock := &fakeClock{at: time.Unix(1000000, 0)}
	w := honoredWindow(t, clock)
	clock.at = time.Unix(1000299, 0)

	// Act
	allowed, expired := w.Allows("debug")

	// Assert
	if !allowed || expired != nil {
		t.Errorf("Allows(debug) = %v, %v; want true, nil", allowed, expired)
	}
}

func TestWindowRevertsToInfoAtItsEnd(t *testing.T) {
	// Arrange
	clock := &fakeClock{at: time.Unix(1000000, 0)}
	w := honoredWindow(t, clock)
	clock.at = time.Unix(1000300, 0)

	// Act
	allowed, expired := w.Allows("debug")

	// Assert
	if allowed {
		t.Error("a debug record passed after the window ended")
	}
	if expired == nil || expired.From != LevelDebug || expired.Until.Unix() != 1000300 {
		t.Errorf("expiry = %+v, want from debug until 1000300", expired)
	}
}

func TestWindowReportsItsEndOnce(t *testing.T) {
	// Arrange
	clock := &fakeClock{at: time.Unix(1000000, 0)}
	w := honoredWindow(t, clock)
	clock.at = time.Unix(1000400, 0)
	w.Allows("info")

	// Act
	_, expired := w.Allows("info")

	// Assert
	if expired != nil {
		t.Errorf("second call reported the expiry again: %+v", expired)
	}
}

func TestWindowAfterItsEndAdmitsInfo(t *testing.T) {
	// Arrange
	clock := &fakeClock{at: time.Unix(1000000, 0)}
	w := honoredWindow(t, clock)
	clock.at = time.Unix(1000400, 0)

	// Act
	allowed, _ := w.Allows("info")

	// Assert
	if !allowed || w.Level() != LevelInfo {
		t.Errorf("after the window: allowed=%v level=%s, want true info", allowed, w.Level())
	}
}

func TestFixedWindowNeverEnds(t *testing.T) {
	// Arrange
	w := FixedWindow(LevelDebug)

	// Act
	allowed, expired := w.Allows("debug")

	// Assert
	if !allowed || expired != nil {
		t.Errorf("FixedWindow(debug).Allows(debug) = %v, %v", allowed, expired)
	}
}

func TestExpiryContextNamesTheRevert(t *testing.T) {
	// Arrange
	e := Expiry{From: LevelDebug, Until: time.Unix(1000300, 0)}

	// Act
	ctx := e.Context()

	// Assert
	if ctx["from_level"] != "debug" || ctx["effective_level"] != "info" || ctx["outcome"] != "window_ended" {
		t.Errorf("context = %v", ctx)
	}
}
