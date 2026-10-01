package storeclient

import (
	"context"
	"errors"
	"io"
	"testing"

	sharedlogging "agentrepl/logging"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

func TestCallFailureStatesEachOutcomeAtItsLevel(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantLevel string
		wantText  string
	}{
		{name: "the caller withdrew the call", err: context.Canceled, wantLevel: "debug", wantText: "probe abandoned: the caller cancelled the request, so nothing was read"},
		{name: "the store did not answer", err: errors.New("connection refused"), wantLevel: "error", wantText: "probe transport failure: connection refused"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			var lines []string
			bound := logging.NewAtLevel(sliceWriter{lines: &lines}, io.Discard, sharedlogging.LevelDebug).
				With(logging.Context{Component: "storeclient-test", Operation: "storeclient-probe"})

			// Act.
			err := callFailure(bound, "/store.v1.ShimStore/Probe", "probe", "so nothing was read", test.err)

			// Assert.
			if !errors.Is(err, test.err) {
				t.Fatalf("callFailure = %v, want it to wrap %v", err, test.err)
			}
			rec := requireOnceIn(t, parseLogLines(t, lines), "storeclient-probe", test.wantLevel)
			if rec.Message != test.wantText {
				t.Fatalf("message = %q, want %q", rec.Message, test.wantText)
			}
		})
	}
}
