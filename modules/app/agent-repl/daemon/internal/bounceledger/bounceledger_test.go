package bounceledger

import (
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"
)

func TestJudge(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name        string
		entry       Entry
		holders     []int
		probeErr    error
		wantVerdict string
		wantReason  string
	}{
		{
			name:        "a preserved shim still holding its lock is preserved",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved},
			holders:     []int{27494},
			wantVerdict: VerdictPreserved,
			wantReason:  "never changed",
		},
		{
			name:        "a preserved shim replaced by another pid died",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved},
			holders:     []int{51755},
			wantVerdict: VerdictDied,
			wantReason:  "NOBODY ORDERED THAT",
		},
		{
			name:        "a preserved shim with no holder at all died",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved},
			holders:     nil,
			wantVerdict: VerdictDied,
			wantReason:  "27494",
		},
		{
			name:        "a deliberate roll reads as rolled once the shim is gone",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionRolled, Reason: "the shim bundle was superseded"},
			holders:     nil,
			wantVerdict: VerdictRolled,
			wantReason:  "the shim bundle was superseded",
		},
		{
			name:        "a roll still waiting on its boundary reads as rolled, not preserved",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionRolled, Reason: "the shim bundle was superseded"},
			holders:     []int{27494},
			wantVerdict: VerdictRolled,
			wantReason:  "still finishing",
		},
		{
			name:        "a failed lock probe is unknown rather than a death",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved},
			probeErr:    errors.New("lsof exploded"),
			wantVerdict: VerdictUnknown,
			wantReason:  "unobserved",
		},
		{
			name:        "an unrecognized disposition is unknown rather than guessed",
			entry:       Entry{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: "vaporized"},
			wantVerdict: VerdictUnknown,
			wantReason:  "cannot judge",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			verdict, reason := Judge(tc.entry, tc.holders, tc.probeErr)
			if verdict != tc.wantVerdict {
				t.Fatalf("verdict = %q, want %q", verdict, tc.wantVerdict)
			}
			if !strings.Contains(reason, tc.wantReason) {
				t.Fatalf("reason = %q, want it to contain %q", reason, tc.wantReason)
			}
		})
	}
}

func TestValidate(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		entry   Entry
		wantErr string
	}{
		{"a complete preservation validates", Entry{SessionID: "s_1", Workspace: "/ws", PID: 1, Disposition: DispositionPreserved}, ""},
		{"a roll without a reason is refused", Entry{SessionID: "s_1", Workspace: "/ws", PID: 1, Disposition: DispositionRolled}, "roll with no reason"},
		{"an entry without a pid is refused", Entry{SessionID: "s_1", Workspace: "/ws", Disposition: DispositionPreserved}, "no pid"},
		{"an entry without a workspace is refused", Entry{SessionID: "s_1", PID: 1, Disposition: DispositionPreserved}, "no workspace"},
		{"an entry without a session id is refused", Entry{Workspace: "/ws", PID: 1, Disposition: DispositionPreserved}, "no session id"},
		{"an unknown disposition is refused", Entry{SessionID: "s_1", Workspace: "/ws", PID: 1, Disposition: "vaporized"}, "unknown disposition"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			err := tc.entry.Validate()
			if tc.wantErr == "" {
				if err != nil {
					t.Fatalf("Validate() = %v, want nil", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("Validate() = %v, want an error containing %q", err, tc.wantErr)
			}
		})
	}
}

func TestWriteThenLoadRoundTrips(t *testing.T) {
	t.Parallel()
	path := filepath.Join(t.TempDir(), "bounce.json")
	want := []Entry{{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved}}

	if err := Write(path, want); err != nil {
		t.Fatalf("Write() = %v", err)
	}
	got, err := Load(path)

	if err != nil {
		t.Fatalf("Load() = %v", err)
	}
	if len(got) != 1 || got[0] != want[0] {
		t.Fatalf("Load() = %+v, want %+v", got, want)
	}
}

func TestLoadMissingLedgerIsEmpty(t *testing.T) {
	t.Parallel()
	got, err := Load(filepath.Join(t.TempDir(), "absent.json"))
	if err != nil {
		t.Fatalf("Load() = %v, want nil for a missing ledger", err)
	}
	if len(got) != 0 {
		t.Fatalf("Load() = %+v, want empty", got)
	}
}

func TestWriteRefusesAnInvalidEntry(t *testing.T) {
	t.Parallel()
	path := filepath.Join(t.TempDir(), "bounce.json")

	err := Write(path, []Entry{{SessionID: "s_1", Workspace: "/ws", PID: 1, Disposition: DispositionRolled}})

	if err == nil || !strings.Contains(err.Error(), "roll with no reason") {
		t.Fatalf("Write() = %v, want it refused for a reasonless roll", err)
	}
}

func TestReportNamesEveryVerdictAndSummarizes(t *testing.T) {
	t.Parallel()
	entries := []Entry{
		{SessionID: "s_dead", Workspace: "/dead", PID: 27494, Disposition: DispositionPreserved},
		{SessionID: "s_alive", Workspace: "/alive", PID: 27726, Disposition: DispositionPreserved},
		{SessionID: "s_rolled", Workspace: "/rolled", PID: 27931, Disposition: DispositionRolled, Reason: "the shim bundle was superseded"},
	}
	holders := func(workspace string) ([]int, error) {
		switch workspace {
		case "/alive":
			return []int{27726}, nil
		default:
			return nil, nil
		}
	}
	var lines []string
	logf := func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }

	tally := Report(logf, entries, holders)

	if (tally != Tally{Preserved: 1, Rolled: 1, Died: 1}) {
		t.Fatalf("tally = %+v, want one of each", tally)
	}
	joined := strings.Join(lines, "\n")
	for _, want := range []string{
		"session=s_dead", "verdict=DIED",
		"session=s_alive", "verdict=PRESERVED",
		"session=s_rolled", "verdict=ROLLED",
		"SUMMARY sessions=3 preserved=1 rolled=1 died=1 unknown=0",
		"FLEET LOSS died=1 of 3",
	} {
		if !strings.Contains(joined, want) {
			t.Fatalf("report missing %q; got:\n%s", want, joined)
		}
	}
}

func TestReportSaysNothingRatherThanClaimingHealthWithoutALedger(t *testing.T) {
	t.Parallel()
	var lines []string
	logf := func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }

	tally := Report(logf, nil, func(string) ([]int, error) { return nil, nil })

	if (tally != Tally{}) {
		t.Fatalf("tally = %+v, want zero", tally)
	}
	if len(lines) != 1 || !strings.Contains(lines[0], "EMPTY") {
		t.Fatalf("lines = %v, want a single EMPTY line", lines)
	}
}

// ---------------------------------------------------------------------------
// THE END-STATE HALF. The boot verdict is taken before any bring-up runs, so
// PRESERVED there is a claim the bring-up can still break.
// ---------------------------------------------------------------------------

func preservedEntry() Entry {
	return Entry{SessionID: "s_1", Workspace: "/ws", PID: 33870, Disposition: DispositionPreserved}
}

func captureLines() (func(string, ...any), *[]string) {
	var lines []string
	return func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }, &lines
}

func TestAnAdoptedSurvivorEndsTheBounceAsAdopted(t *testing.T) {
	t.Parallel()
	settlement := NewSettlement()
	settlement.Adopted("/ws", "the bring-up reattached to it")
	logf, lines := captureLines()

	tally := ReportEnd(logf, []Entry{preservedEntry()}, settlement)

	if tally.Adopted != 1 || tally.Replaced != 0 {
		t.Fatalf("tally = %+v, want the adoption counted", tally)
	}
	if !strings.Contains(strings.Join(*lines, "\n"), "verdict=ADOPTED") {
		t.Fatalf("lines = %v, want an ADOPTED verdict", *lines)
	}
}

func TestAReplacedSurvivorEndsTheBounceAsAPreservationThatDidNotHold(t *testing.T) {
	t.Parallel()
	settlement := NewSettlement()
	settlement.Replaced("/ws", "it never redialled")
	logf, lines := captureLines()

	tally := ReportEnd(logf, []Entry{preservedEntry()}, settlement)

	if tally.Replaced != 1 || tally.Adopted != 0 {
		t.Fatalf("tally = %+v, want the replacement counted", tally)
	}
	joined := strings.Join(*lines, "\n")
	if !strings.Contains(joined, "PRESERVATION DID NOT HOLD") || !strings.Contains(joined, "it never redialled") {
		t.Fatalf("lines = %v, want the broken preservation named with its reason", *lines)
	}
}

func TestASessionNoBringUpRuledOnIsUnsettledRatherThanAdopted(t *testing.T) {
	t.Parallel()
	logf, lines := captureLines()

	tally := ReportEnd(logf, []Entry{preservedEntry()}, NewSettlement())

	if tally.Unsettled != 1 || tally.Adopted != 0 {
		t.Fatalf("tally = %+v, want silence counted as unsettled and never as adoption", tally)
	}
	if !strings.Contains(strings.Join(*lines, "\n"), "verdict=UNSETTLED") {
		t.Fatalf("lines = %v, want an UNSETTLED verdict", *lines)
	}
}

func TestAReplacementIsNotLaunderedByTheReplacementsOwnAdoption(t *testing.T) {
	t.Parallel()
	settlement := NewSettlement()
	settlement.Replaced("/ws", "it never redialled")
	settlement.Adopted("/ws", "the freshly spawned shim connected")

	verdict, _, ok := settlement.Outcome("/ws")

	if !ok || verdict != OutcomeReplaced {
		t.Fatalf("outcome = (%q, %v), want the workspace to stay REPLACED", verdict, ok)
	}
}

func TestAReplacementWithNoReasonRecordsTheOmissionRatherThanDroppingIt(t *testing.T) {
	t.Parallel()
	settlement := NewSettlement()
	settlement.Replaced("/ws", "")

	_, reason, _ := settlement.Outcome("/ws")

	if !strings.Contains(reason, "NO REASON WAS RECORDED") {
		t.Fatalf("reason = %q, want the missing cause surfaced as the defect it is", reason)
	}
}

func TestARolledEntryIsNotJudgedAgainstTheEndState(t *testing.T) {
	t.Parallel()
	rolled := Entry{SessionID: "s_2", Workspace: "/ws2", PID: 5, Disposition: DispositionRolled, Reason: "superseded bundle"}
	logf, lines := captureLines()

	tally := ReportEnd(logf, []Entry{rolled}, NewSettlement())

	if (tally != EndTally{}) {
		t.Fatalf("tally = %+v, want a deliberate roll left out of the end-state accounting", tally)
	}
	if len(*lines) != 0 {
		t.Fatalf("lines = %v, want none: the roll's end state was decided when it was ordered", *lines)
	}
}

func TestReportPassesAProbeFailureThroughAsUnknown(t *testing.T) {
	t.Parallel()
	entries := []Entry{{SessionID: "s_1", Workspace: "/ws", PID: 27494, Disposition: DispositionPreserved}}
	var lines []string
	logf := func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }

	tally := Report(logf, entries, func(string) ([]int, error) { return nil, errors.New("lsof exploded") })

	if tally.Unknown != 1 || tally.Died != 0 {
		t.Fatalf("tally = %+v, want the probe failure counted as unknown and never as a death", tally)
	}
	if !strings.Contains(strings.Join(lines, "\n"), "verdict=UNKNOWN") {
		t.Fatalf("lines = %v, want an UNKNOWN verdict", lines)
	}
}
