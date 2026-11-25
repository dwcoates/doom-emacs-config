package standdown

import (
	"context"
	"errors"
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
