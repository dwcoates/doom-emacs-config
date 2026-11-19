package merge

import (
	"errors"
	"strings"
	"testing"

	"agentrepl/testrun/seatwait"
)

// seatWaitLine is a runner's line waiting for a seat, all held by fix-footer.
func seatWaitLine() string {
	w := seatwait.Wait{Reason: seatwait.Ceiling, Reading: seatwait.Reading{Holders: []seatwait.Holder{{Workspace: "fix-footer"}}, AvailableMiB: 30720, WiredMiB: 20480, CeilingMiB: 20480, FloorMiB: 10240, Rank: 10}}
	return "[agent-repl-tests] " + w.Line()
}

// seatTookLine is a runner's line taking its seat.
func seatTookLine() string {
	return "[agent-repl-tests] " + seatwait.TookLine(1, 5, "1m40s", seatwait.Reading{})
}

func TestTheSuiteTrackerFollowsEachLine(t *testing.T) {
	tests := []struct {
		name      string
		before    []string
		line      string
		wantMoved bool
		wantSuite string
		wantState suiteState
		wantRows  int
	}{
		{name: "a suite starting stands its row", line: "[agent-repl-tests] daemon: starting",
			wantMoved: true, wantSuite: "daemon", wantState: suiteStateRunning, wantRows: 1},
		{name: "a suite passing moves its row", before: []string{"[agent-repl-tests] daemon: starting"}, line: "[agent-repl-tests] daemon: passed in 2s",
			wantMoved: true, wantSuite: "daemon", wantState: suiteStatePassed, wantRows: 1},
		{name: "a reused suite stands its row", line: reusedLine,
			wantMoved: true, wantSuite: "daemon", wantState: suiteStateReused, wantRows: 1},
		{name: "a started suite's planned total moves its row, as no edge", before: []string{"[agent-repl-tests] daemon: starting"}, line: "[agent-repl-tests] daemon: 3 units planned",
			wantMoved: true, wantRows: 1},
		{name: "an unstarted suite's planned total moves nothing", line: "[agent-repl-tests] daemon: 3 units planned"},
		{name: "a seat wait moves the view", line: seatWaitLine(), wantMoved: true},
		{name: "a seat taken after a wait moves the view", before: []string{seatWaitLine()}, line: seatTookLine(), wantMoved: true},
		{name: "a seat taken with no wait moves nothing", line: seatTookLine()},
		{name: "an ordinary line moves nothing", line: "ok  	agent-repld/internal/merge	1.2s"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tr := newSuiteTracker(func() int64 { return 7 })
			for _, line := range tt.before {
				if _, err := tr.take(line); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := tr.take(tt.line)

			// Assert
			view := tr.view(nil)
			if err != nil || got.moved != tt.wantMoved || got.suite != tt.wantSuite || (tt.wantSuite != "" && got.state != tt.wantState) || len(view.GetSuites()) != tt.wantRows {
				t.Fatalf("take = (%+v, %v), rows %d; want moved %v, suite %q %v, rows %d", got, err, len(view.GetSuites()), tt.wantMoved, tt.wantSuite, tt.wantState, tt.wantRows)
			}
		})
	}
}

func TestTheSuiteTrackerKeepsASeatWaitsStartAndClearsItOnTheSeat(t *testing.T) {
	// Arrange
	now := int64(7)
	tr := newSuiteTracker(func() int64 { return now })

	// Act
	_, _ = tr.take(seatWaitLine())
	now = 99
	_, _ = tr.take(seatWaitLine())
	waiting := tr.view(nil).GetSeatWait()
	_, _ = tr.take(seatTookLine())

	// Assert
	if waiting.GetStartedAtMs() != 7 || tr.view(nil).GetSeatWait() != nil {
		t.Fatalf("wait started %d, then %v; want the first wait's start kept and the seat clearing it", waiting.GetStartedAtMs(), tr.view(nil).GetSeatWait())
	}
}

func TestTheSuiteTrackerNamesALineItCannotRead(t *testing.T) {
	tests := []struct {
		name string
		line string
		want error
	}{
		{name: "an unreadable seat line", line: "[agent-repl-tests] " + seatwait.Marker + "waiting, reason=gremlins (x); holders=[]; available=1MiB wired=0MiB ceiling=1MiB floor=1MiB rank=0", want: errSeatLine},
		{name: "an uncountable line", line: strings.TrimSuffix(unreadableTotal, "\n"), want: errCountLine},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tr := newSuiteTracker(func() int64 { return 7 })

			// Act
			got, err := tr.take(tt.line)

			// Assert: the line moves nothing.
			if !errors.Is(err, tt.want) || got.moved || len(tr.view(nil).GetSuites()) != 0 || tr.view(nil).GetSeatWait() != nil {
				t.Fatalf("take = (%+v, %v); want %v and nothing moved", got, err, tt.want)
			}
		})
	}
}

func TestResettingTheSuiteTrackerForgetsEverySuiteAndWait(t *testing.T) {
	// Arrange
	tr := newSuiteTracker(func() int64 { return 7 })
	for _, line := range []string{"[agent-repl-tests] daemon: starting", seatWaitLine()} {
		if _, err := tr.take(line); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	tr.reset()

	// Assert
	if view := tr.view(nil); len(view.GetSuites()) != 0 || view.GetSeatWait() != nil {
		t.Fatalf("view = %v; want nothing left", view)
	}
}
