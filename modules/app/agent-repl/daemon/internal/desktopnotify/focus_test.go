package desktopnotify

import (
	"errors"
	"testing"

	"claude-repld/internal/dlog"
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

func TestFocusReportMovesTheStandingStreamsFocus(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())
	f.Attach(false)

	// Act
	err := f.Report(true)

	// Assert
	if err != nil {
		t.Fatalf("Report: %v", err)
	}
	if !f.Focused() {
		t.Fatal("a reported focus did not stand")
	}
}

func TestFocusReportWithNoStreamIsRefused(t *testing.T) {
	// Arrange
	f := NewFocus(dlog.NewTestLogger())

	// Act
	err := f.Report(true)

	// Assert
	if !errors.Is(err, ErrNoEmacsStream) {
		t.Fatalf("Report = %v, want ErrNoEmacsStream", err)
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
	if err := f.Report(true); !errors.Is(err, ErrNoEmacsStream) {
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
