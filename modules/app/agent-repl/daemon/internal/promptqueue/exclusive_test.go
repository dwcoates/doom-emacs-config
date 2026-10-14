package promptqueue

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/ids"
)

func TestExclusivelyRunsTheWorkOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	calls := 0

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", func(context.Context) error {
		calls++
		return nil
	})

	// Assert
	if err != nil || calls != 1 {
		t.Fatalf("Exclusively = %v with %d calls, want nil and one call", err, calls)
	}
}

// TestASubmissionDuringExclusiveWorkIsDeliveredOnlyOnceItSettles proves the
// workspace starts no turn while the work runs: a submission made meanwhile is
// held, and delivered once the work has returned.
func TestASubmissionDuringExclusiveWorkIsDeliveredOnlyOnceItSettles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var during Disposition
	var startedDuring []ids.TurnID
	perform := func(context.Context) error {
		var err error
		during, err = h.q.Submit(context.Background(), submission("t-other", "a concurrent submission"))
		if err != nil {
			t.Errorf("Submit during the exclusive work: %v", err)
		}
		startedDuring = h.sender.started()
		return nil
	}

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", perform)

	// Assert
	if err != nil {
		t.Fatalf("Exclusively: %v", err)
	}
	if during.Delivered || len(startedDuring) != 0 {
		t.Fatalf("submission during the work = %+v with %v started, want it held", during, startedDuring)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t-other" {
		t.Fatalf("started = %v, want the held submission delivered once the work settled", started)
	}
}

func TestExclusivelyReturnsTheWorksErrorAndRecordsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cause := errors.New("the fork failed")

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", func(context.Context) error { return cause })

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("Exclusively = %v, want the work's error", err)
	}
	if !logged(h.log.Records(), "info", opExclusive, "the work done while the workspace's turn sequence was held failed; deliveries resume") {
		t.Fatalf("records = %+v, want the failure logged", h.log.Records())
	}
}
