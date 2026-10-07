package usersetup_test

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/usersetup"
)

func TestErrorf(t *testing.T) {
	cause := errors.New("cause")
	tests := []struct {
		name     string
		format   string
		args     []any
		wantText string
		wantWrap error
	}{
		{
			name:     "formats the message and appends the doc pointer",
			format:   "no profile for %q",
			args:     []any{"a@example.com"},
			wantText: `no profile for "a@example.com" (see ` + usersetup.Doc + `)`,
		},
		{
			name:     "a wrapped cause stays reachable",
			format:   "reading: %w",
			args:     []any{cause},
			wantText: "reading: cause (see " + usersetup.Doc + ")",
			wantWrap: cause,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			err := usersetup.Errorf(tc.format, tc.args...)

			// Assert.
			if err.Error() != tc.wantText {
				t.Fatalf("Errorf() = %q, want %q", err.Error(), tc.wantText)
			}
			if tc.wantWrap != nil && !errors.Is(err, tc.wantWrap) {
				t.Fatalf("Errorf() = %v, want it to wrap %v", err, tc.wantWrap)
			}
		})
	}
}

func TestDocNamesTheUserGuideSetupSection(t *testing.T) {
	// Assert.
	if !strings.Contains(usersetup.Doc, "docs/USER-GUIDE.md") || !strings.Contains(usersetup.Doc, "Setup") {
		t.Fatalf("Doc = %q, want the user guide's Setup section", usersetup.Doc)
	}
}
