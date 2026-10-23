package desktopnotify

import (
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

func TestFocusReadsUnfocusedBeforeAnyStreamAttaches(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())

	// Act
	got := f.Focused()

	// Assert
	if got {
		t.Fatal("an unattached focus read as focused")
	}
}

func TestFocusTakesTheAttachedStreamsFocus(t *testing.T) {
	cases := []struct {
		name    string
		focused bool
	}{
		{name: "focused", focused: true},
		{name: "unfocused", focused: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := NewFocus(dlog.NewTestLogger())

			// Act
			f.Attach(tc.focused)

			// Assert
			if got := f.Focused(); got != tc.focused {
				t.Fatalf("Focused() = %v, want %v", got, tc.focused)
			}
		})
	}
}

func TestFocusReportMovesTheStandingStreamsFocusAndAnswersAGain(t *testing.T) {
	cases := []struct {
		name       string
		attached   bool
		reported   bool
		wantGained bool
	}{
		{name: "unfocused to focused is a gain", attached: false, reported: true, wantGained: true},
		{name: "focused to focused is no gain", attached: true, reported: true, wantGained: false},
		{name: "focused to unfocused is no gain", attached: true, reported: false, wantGained: false},
		{name: "unfocused to unfocused is no gain", attached: false, reported: false, wantGained: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			f := NewFocus(log)
			f.Attach(tc.attached)

			// Act
			gained, err := f.Report(tc.reported)

			// Assert
			if err != nil || gained != tc.wantGained {
				t.Fatalf("Report = (%v, %v), want (%v, nil)", gained, err, tc.wantGained)
			}
			if got := f.Focused(); got != tc.reported {
				t.Fatalf("Focused() = %v, want the reported %v", got, tc.reported)
			}
			r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "Emacs reported its focus"})
			if !ok || r.Context["focused"] != tc.reported || r.Context["gained"] != tc.wantGained {
				t.Fatalf("record = %+v (found=%v), want focused and gained recorded", r, ok)
			}
		})
	}
}

func TestFocusReportWithNoStreamIsRefused(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())

	// Act
	gained, err := f.Report(true)

	// Assert
	if !errors.Is(err, ErrNoEmacsStream) || gained {
		t.Fatalf("Report = (%v, %v), want (false, ErrNoEmacsStream)", gained, err)
	}
	if f.Focused() {
		t.Fatal("a refused report changed the focus")
	}
}

func TestFocusReleaseReadsUnfocused(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())
	release := f.Attach(true)

	// Act
	release()

	// Assert
	if f.Focused() {
		t.Fatal("a released stream's focus still stood")
	}
	if _, err := f.Report(true); !errors.Is(err, ErrNoEmacsStream) {
		t.Fatalf("Report after release = %v, want ErrNoEmacsStream", err)
	}
}

func TestFocusASupersededStreamsReleaseLeavesItsSuccessor(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())
	first := f.Attach(false)
	f.Attach(true)

	// Act
	first()

	// Assert
	if !f.Focused() {
		t.Fatal("a superseded stream's release erased its successor's focus")
	}
}
