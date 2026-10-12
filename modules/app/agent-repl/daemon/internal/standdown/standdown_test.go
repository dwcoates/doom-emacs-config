package standdown

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"
)

func TestCutShort(t *testing.T) {
	live := context.Background()
	ended, cancel := context.WithCancel(context.Background())
	cancel()
	failed := errors.New("signal: killed")
	cases := []struct {
		name string
		ctx  context.Context
		err  error
		want bool
	}{
		{"a failure under an ended lifetime is a stand-down", ended, failed, true},
		{"a failure under a live lifetime is a real failure", live, failed, false},
		{"no failure under an ended lifetime is no stand-down", ended, nil, false},
		{"no failure under a live lifetime is no stand-down", live, nil, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := CutShort(tc.ctx, tc.err)

			// Assert
			if got != tc.want {
				t.Fatalf("CutShort = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestStandDownSitesShareCutShort fails a site that hand-rolls the stand-down
// test instead of asking CutShort: CutShort's body is the one place the
// lifetime's end is read for these callers.
func TestStandDownSitesShareCutShort(t *testing.T) {
	cases := []string{
		"../desktopnotify/notifier.go",
		"../desktopnotify/summary.go",
		"../mergeworkspace/mergeworkspace.go",
	}
	for _, file := range cases {
		t.Run(file, func(t *testing.T) {
			// Arrange
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}

			// Act
			asks := strings.Count(string(src), "standdown.CutShort(")
			reads := strings.Count(string(src), "ctx.Err() != nil")

			// Assert
			if asks == 0 || reads != 0 {
				t.Fatalf("%s: standdown.CutShort( x%d, ctx.Err() != nil x%d (want 0)", file, asks, reads)
			}
		})
	}
}
