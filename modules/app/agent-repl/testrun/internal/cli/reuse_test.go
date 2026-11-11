package cli

import (
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/reuse"
)

func TestARunSkipsAReusedSuiteAndSaysSo(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	h.reuser.reused = map[string]bool{"daemon": true}

	// Act
	code := h.run(t)

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	if slices.Contains(h.exec.ran, "daemon") {
		t.Fatal("the reused daemon suite ran")
	}
	if len(h.exec.ran) != fullRunSize()-1 {
		t.Fatalf("ran %d units, want every suite but the reused one", len(h.exec.ran))
	}
	for _, want := range []string{
		"[agent-repl-tests] daemon: reused a green run (inputs ",
		"reused an earlier green run of every unit, NOT run: daemon (--no-reuse runs them)",
	} {
		if !strings.Contains(h.out.String(), want) {
			t.Errorf("stdout lacks %q:\n%s", want, h.out)
		}
	}
}

// A unit that passed is recorded whatever its suite did, so the run hands
// Record every unit's result, the failed suite's included.
func TestARunHandsRecordEveryUnitsResultFailedSuitesIncluded(t *testing.T) {
	// Arrange
	h := newHarness(t, map[string]int{"store": 1}, nil)

	// Act
	h.run(t, "--suites", "daemon,store")

	// Assert
	got := *h.reuser.recordedUnits
	if !slices.Contains(got, "daemon") || !slices.Contains(got, "store") {
		t.Fatalf("Record was handed %v, want the passing daemon's and the failing store's units", got)
	}
}

func TestAnUnplacedFileFailsTheRunWithoutStoppingReuse(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	h.reuser.reused = map[string]bool{"daemon": true}
	h.reuser.unplaced = []string{"newdir/thing.go"}

	// Act
	code := h.run(t)

	// Assert
	if code != 1 {
		t.Fatalf("exit = %d, want 1 for an unplaced file", code)
	}
	if slices.Contains(h.exec.ran, "daemon") {
		t.Fatal("an unplaced file stopped the daemon suite's reuse")
	}
	for _, want := range []string{
		"placement: newdir/thing.go is read by no suite and listed nowhere",
		"placement: 1 file(s) are read by no suite and listed nowhere",
	} {
		if !strings.Contains(h.errOut.String(), want) {
			t.Errorf("stderr lacks %q:\n%s", want, h.errOut)
		}
	}
}

func TestReuseMode(t *testing.T) {
	tests := []struct {
		name string
		args Args
		want reuse.Mode
	}{
		{"a plain run reuses", Args{}, reuse.Reusing},
		{"--no-reuse runs everything", Args{NoReuse: true}, reuse.NoReuse},
		{"a timing record runs everything", Args{Record: true}, reuse.NoReuse},
		{"a coverage run runs everything", Args{Coverage: true}, reuse.NoReuse},
		{"a targeted run neither reuses nor records, whatever else is set", Args{Targeted: true, NoReuse: true}, reuse.Targeted},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.args.ReuseMode()

			// Assert
			if got != tt.want {
				t.Fatalf("ReuseMode() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheRunHandsItsModeToReuse(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)

	// Act
	h.run(t, "--no-reuse", "--suites", "lock")

	// Assert
	if *h.reuser.mode != reuse.NoReuse {
		t.Fatalf("mode = %v, want NoReuse", *h.reuser.mode)
	}
}

func TestParseArgsReadsNoReuse(t *testing.T) {
	// Act
	a, err := ParseArgs([]string{"--module", "/m", "--no-reuse"})

	// Assert
	if err != nil || !a.NoReuse {
		t.Fatalf("ParseArgs = %+v, %v; want NoReuse", a, err)
	}
}
