package seatwait

import (
	"errors"
	"reflect"
	"strings"
	"testing"
)

func reading(holders ...Holder) Reading {
	return Reading{Holders: holders, AvailableMiB: 26624, WiredMiB: 8192, CeilingMiB: 20480, FloorMiB: 10240, Rank: 10}
}

func TestParseReadsBackEveryWaitLine(t *testing.T) {
	cases := []struct {
		name string
		wait Wait
	}{
		{name: "a ceiling wait behind two workspaces", wait: Wait{Reason: Ceiling, Reading: reading(Holder{Workspace: "fix-footer"}, Holder{Workspace: "sidebar-counts"})}},
		{name: "a memory wait behind nobody", wait: Wait{Reason: Memory, Reading: reading()}},
		{name: "an outranked wait behind a run started by hand", wait: Wait{Reason: Outranked, Reading: reading(Holder{Pid: 4242})}},
		{name: "a workspace name holding a quote and a comma", wait: Wait{Reason: Ceiling, Reading: reading(Holder{Workspace: `odd, "name"`}, Holder{Pid: 7})}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the line as the runner's log prefixes it.
			line := "[agent-repl-tests] " + tc.wait.Line()

			// Act
			got, ok, err := Parse(line)

			// Assert
			if err != nil || !ok || !reflect.DeepEqual(got, tc.wait) {
				t.Fatalf("Parse(%q) = %+v, %v, %v; want %+v", line, got, ok, err, tc.wait)
			}
		})
	}
}

func TestParseIgnoresLinesThatAreNotWaits(t *testing.T) {
	cases := []struct {
		name string
		line string
	}{
		{name: "a suite line", line: "[agent-repl-tests] daemon: starting"},
		{name: "a took line", line: "[agent-repl-tests] " + TookLine(0, 5, "", reading())},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, ok, err := Parse(tc.line)

			// Assert
			if ok || err != nil {
				t.Fatalf("Parse(%q) = %v, %v; want no wait and no error", tc.line, ok, err)
			}
		})
	}
}

func TestParseRefusesAWaitLineThatDoesNotReadBack(t *testing.T) {
	cases := []struct {
		name string
		line string
		want string
	}{
		{name: "an unknown reason", line: Marker + "waiting, reason=gremlins (x); holders=[]; available=1MiB wired=0MiB ceiling=1MiB floor=1MiB rank=0", want: "does not know"},
		{name: "a holder that does not read", line: Marker + "waiting, reason=ceiling (x); holders=[someone]; available=1MiB wired=0MiB ceiling=1MiB floor=1MiB rank=0", want: `the holder "someone" does not read`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, ok, err := Parse(tc.line)

			// Assert
			if !ok || err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Parse(%q) = %v, %v; want a wait line refused with %q", tc.line, ok, err, tc.want)
			}
		})
	}
}

func TestLinePanicsOnAReasonThePackageDoesNotDefine(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("Line accepted an undefined reason")
		}
	}()

	// Act
	_ = Wait{Reason: "gremlins"}.Line()
}

func TestEnded(t *testing.T) {
	cases := []struct {
		name string
		line string
		want bool
	}{
		{name: "a seat taken ends the wait", line: "[agent-repl-tests] " + TookLine(1, 5, "1m40s", reading()), want: true},
		{name: "no seat taken ends the wait", line: "[agent-repl-tests] " + FailedLine(errors.New("boom")), want: true},
		{name: "a wait line does not end it", line: "[agent-repl-tests] " + Wait{Reason: Memory, Reading: reading()}.Line(), want: false},
		{name: "an unrelated line does not end it", line: "[agent-repl-tests] daemon: 3 units planned", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Ended(tc.line)

			// Assert
			if got != tc.want {
				t.Fatalf("Ended(%q) = %v, want %v", tc.line, got, tc.want)
			}
		})
	}
}

func TestTookLineSaysHowLongItWaited(t *testing.T) {
	cases := []struct {
		name   string
		waited string
		want   string
	}{
		{name: "at once", want: Marker + "took seat 2 of 5; holders=[]"},
		{name: "after a wait", waited: "2m10s", want: Marker + "took seat 2 of 5 after 2m10s; holders=[]"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := TookLine(2, 5, tc.waited, reading())

			// Assert
			if !strings.HasPrefix(got, tc.want) {
				t.Fatalf("TookLine = %q, want it to begin %q", got, tc.want)
			}
		})
	}
}
