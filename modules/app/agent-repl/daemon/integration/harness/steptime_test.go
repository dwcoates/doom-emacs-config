package harness

import (
	"bytes"
	"errors"
	"regexp"
	"testing"
)

func TestTimedStep(t *testing.T) {
	tests := []struct {
		name     string
		err      error
		wantLine *regexp.Regexp
	}{
		{name: "a step that succeeds", err: nil, wantLine: regexp.MustCompile(`^e2e: build the store took \d+ms\n$`)},
		{name: "a step that fails says so", err: errors.New("boom"), wantLine: regexp.MustCompile(`^e2e: build the store took \d+ms \(failed\)\n$`)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var out bytes.Buffer

			// Act.
			got, err := TimedStep(&out, "e2e", "build the store", func() (int, error) { return 7, tt.err })

			// Assert.
			if got != 7 || !errors.Is(err, tt.err) {
				t.Fatalf("TimedStep = %d, %v; want the step's own 7, %v", got, err, tt.err)
			}
			if !tt.wantLine.MatchString(out.String()) {
				t.Fatalf("line = %q, want %v", out.String(), tt.wantLine)
			}
		})
	}
}
