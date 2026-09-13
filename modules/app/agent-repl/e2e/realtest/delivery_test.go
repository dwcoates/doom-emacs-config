//go:build realtest

package realtest

import (
	"context"
	"strings"
	"testing"
	"time"
)

// The verdict is the whole point of this file, so each of its answers gets its
// own case: a driver that reads "arrived" off a reading that says nothing is
// how a harness hole got filed against the editor three times in one sweep.
func TestJudgeDelivery(t *testing.T) {
	// An ordinary key: Emacs is obliged to leave a mark for it, so silence is
	// an absence. The mark-free cases name their own chord.
	ordinary := Chord{Emacs: "n"}
	quit := Chord{
		Emacs:       "C-g",
		MarkFree:    true,
		MarkFreeWhy: "the quit character is intercepted before `record_char`",
	}

	tests := []struct {
		name        string
		chord       Chord
		before      InputMark
		after       InputMark
		wantVerdict DeliveryVerdict
		wantReason  string
	}{
		{
			name:        "quit-flag coming up is an arrival",
			chord:       ordinary,
			before:      InputMark{Keys: "a b c"},
			after:       InputMark{Keys: "a b c", QuitArmed: true},
			wantVerdict: DeliveryArrived,
			wantReason:  "quit-flag",
		},
		{
			name:        "recent-keys changing is an arrival",
			chord:       ordinary,
			before:      InputMark{Keys: "a b c"},
			after:       InputMark{Keys: "a b c ESC"},
			wantVerdict: DeliveryArrived,
			wantReason:  "(recent-keys) changed",
		},
		{
			name:        "a quit already owed before the press is not read as a drop",
			chord:       ordinary,
			before:      InputMark{Keys: "a b c", QuitArmed: true},
			after:       InputMark{Keys: "a b c", QuitArmed: true},
			wantVerdict: DeliveryArrived,
			wantReason:  "already armed",
		},
		{
			name:        "neither mark on a ring that could have shown one is an absence",
			chord:       ordinary,
			before:      InputMark{Keys: "a b c"},
			after:       InputMark{Keys: "a b c"},
			wantVerdict: DeliveryAbsent,
			wantReason:  "obliged to leave one",
		},
		{
			name:        "a ring of one repeated key cannot answer",
			chord:       ordinary,
			before:      InputMark{Keys: "C-g C-g C-g"},
			after:       InputMark{Keys: "C-g C-g C-g"},
			wantVerdict: DeliveryUndetermined,
			wantReason:  "cannot tell arrival from absence",
		},
		{
			name:        "an editor that would not answer before the press blames nobody",
			chord:       ordinary,
			before:      InputMark{ProbeFailure: "connection refused"},
			after:       InputMark{Keys: "a b c"},
			wantVerdict: DeliveryUndetermined,
			wantReason:  "would not answer before",
		},
		{
			name:        "an editor that would not answer after the press blames nobody",
			chord:       ordinary,
			before:      InputMark{Keys: "a b c"},
			after:       InputMark{ProbeFailure: "timed out"},
			wantVerdict: DeliveryUndetermined,
			wantReason:  "would not answer after",
		},
		{
			// The 2026-09-13 sweep's whole finding: a working `C-g` leaves
			// neither mark, and reading that as a drop filed six harness
			// failures and six product findings for six healthy presses.
			name:        "neither mark on a mark-free chord blames nobody",
			chord:       quit,
			before:      InputMark{Keys: "SPC <tab> n"},
			after:       InputMark{Keys: "SPC <tab> n"},
			wantVerdict: DeliveryUndetermined,
			wantReason:  "NOT obliged to leave either mark",
		},
		{
			name:        "a mark-free chord that does leave a mark still arrived",
			chord:       quit,
			before:      InputMark{Keys: "SPC <tab> n"},
			after:       InputMark{Keys: "SPC <tab> n", QuitArmed: true},
			wantVerdict: DeliveryArrived,
			wantReason:  "quit-flag",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			verdict, reason := judgeDelivery(test.chord, test.before, test.after)

			if verdict != test.wantVerdict {
				t.Errorf("verdict on %v then %v = %d, want %d", test.before, test.after, verdict, test.wantVerdict)
			}
			if !strings.Contains(reason, test.wantReason) {
				t.Errorf("reason = %q, want it to carry %q", reason, test.wantReason)
			}
		})
	}
}

// recentKeysUniform decides whether an unchanged reading is an absence or an
// ambiguity, which is the difference between naming a defect and inventing one.
func TestRecentKeysUniform(t *testing.T) {
	tests := []struct {
		name string
		keys string
		want bool
	}{
		{name: "an empty ring is not uniform", keys: "", want: false},
		{name: "a single key cannot have been shifted out of anything", keys: "C-g", want: false},
		{name: "the same key for the whole ring is uniform", keys: "C-g C-g C-g C-g", want: true},
		{name: "one different key anywhere breaks uniformity", keys: "C-g C-g ESC C-g", want: false},
		{name: "a different key at the head breaks uniformity", keys: "ESC C-g C-g", want: false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := recentKeysUniform(test.keys)

			if got != test.want {
				t.Errorf("recentKeysUniform(%q) = %v, want %v", test.keys, got, test.want)
			}
		})
	}
}

// The two marks travel in one probe answer, so the split has to survive a key
// description that contains anything a key description can contain.
func TestParseInputMark(t *testing.T) {
	tests := []struct {
		name        string
		raw         string
		wantKeys    string
		wantArmed   bool
		wantFailure bool
	}{
		{name: "an armed quit-flag is read as armed", raw: "armed\nSPC TAB", wantKeys: "SPC TAB", wantArmed: true},
		{name: "a down quit-flag is read as down", raw: "down\nSPC TAB", wantKeys: "SPC TAB", wantArmed: false},
		{name: "an empty key ring is not a failure", raw: "down\n", wantKeys: "", wantArmed: false},
		{name: "a key description containing a newline keeps its tail", raw: "down\na\nb", wantKeys: "a\nb"},
		{name: "an answer with no separator is a probe failure", raw: "down", wantFailure: true},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			mark := parseInputMark(test.raw)

			if test.wantFailure {
				if mark.ProbeFailure == "" {
					t.Fatalf("parseInputMark(%q) reported no probe failure, want one", test.raw)
				}
				return
			}
			if mark.ProbeFailure != "" {
				t.Fatalf("parseInputMark(%q) reported a probe failure %q, want none", test.raw, mark.ProbeFailure)
			}
			if mark.Keys != test.wantKeys {
				t.Errorf("keys = %q, want %q", mark.Keys, test.wantKeys)
			}
			if mark.QuitArmed != test.wantArmed {
				t.Errorf("quit armed = %v, want %v", mark.QuitArmed, test.wantArmed)
			}
		})
	}
}

// A finding is only useful if it names the right system, so each verdict's
// wording is asserted where a reader would look for the attribution.
func TestDeliveryReceiptNote(t *testing.T) {
	advancing := Chord{Emacs: "o", Keycode: 31}
	resting := Chord{Emacs: "C-g", Keycode: 5, Repeatable: true, RepeatWhy: "it is a top-level quit at rest"}

	tests := []struct {
		name    string
		receipt DeliveryReceipt
		want    []string
		absent  []string
	}{
		{
			name:    "a key that arrived first time reads as confirmed",
			receipt: DeliveryReceipt{Chord: advancing, Verdict: DeliveryArrived, Attempts: 1, Reason: "(recent-keys) changed"},
			want:    []string{"key delivery confirmed", "o"},
			absent:  []string{"FAILED", "RETRIED"},
		},
		{
			name:    "a key that took two posts says the harness retried it",
			receipt: DeliveryReceipt{Chord: resting, Verdict: DeliveryArrived, Attempts: 2, Reason: "(recent-keys) changed"},
			want:    []string{"HARNESS KEY DELIVERY WAS RETRIED", "posted 2 times", "top-level quit at rest"},
		},
		{
			name:    "an unreadable press blames nobody",
			receipt: DeliveryReceipt{Chord: advancing, Verdict: DeliveryUndetermined, Attempts: 1, Reason: "nobody answered"},
			want:    []string{"KEY DELIVERY UNCONFIRMED, AND NOT BLAMED ON ANYBODY", "conservative direction"},
			absent:  []string{"HARNESS KEY DELIVERY FAILED"},
		},
		{
			name:    "a dropped key is a harness finding and not a product one",
			receipt: DeliveryReceipt{Chord: advancing, Verdict: DeliveryAbsent, Attempts: 1, Reason: "neither mark"},
			want:    []string{"HARNESS KEY DELIVERY FAILED, AND IT IS NOT A PRODUCT FINDING", "not in the editor"},
		},
		{
			name:    "a dropped key that was not retried says why it was not",
			receipt: DeliveryReceipt{Chord: advancing, Verdict: DeliveryAbsent, Attempts: 1, Reason: "neither mark"},
			want:    []string{"not retried", "could make the act happen twice"},
		},
		{
			name:    "a dropped repeatable key does not claim it was left un-retried",
			receipt: DeliveryReceipt{Chord: resting, Verdict: DeliveryAbsent, Attempts: 2, Reason: "neither mark"},
			absent:  []string{"not retried"},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			note := test.receipt.Note()

			for _, want := range test.want {
				if !strings.Contains(note, want) {
					t.Errorf("note %q does not carry %q", note, want)
				}
			}
			for _, absent := range test.absent {
				if strings.Contains(note, absent) {
					t.Errorf("note %q carries %q and should not", note, absent)
				}
			}
		})
	}
}

// Confirmed is what decides whether a press is reported at all, so the one
// press that needs no report is separated from the ones that do.
func TestDeliveryReceiptConfirmed(t *testing.T) {
	tests := []struct {
		name    string
		receipt DeliveryReceipt
		want    bool
	}{
		{name: "arrived on the first post", receipt: DeliveryReceipt{Verdict: DeliveryArrived, Attempts: 1}, want: true},
		{name: "arrived only after a retry", receipt: DeliveryReceipt{Verdict: DeliveryArrived, Attempts: 2}, want: false},
		{name: "undetermined", receipt: DeliveryReceipt{Verdict: DeliveryUndetermined, Attempts: 1}, want: false},
		{name: "absent", receipt: DeliveryReceipt{Verdict: DeliveryAbsent, Attempts: 1}, want: false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := test.receipt.Confirmed()

			if got != test.want {
				t.Errorf("Confirmed() = %v, want %v", got, test.want)
			}
		})
	}
}

// A chord that says nothing about repeating must still explain itself in a
// finding, and it must not be repeatable.
func TestChordRepeatDefault(t *testing.T) {
	tests := []struct {
		name  string
		chord Chord
		want  string
	}{
		{
			name:  "a chord that says nothing gives the advancing answer",
			chord: Chord{Emacs: "o"},
			want:  "advances the editor's state",
		},
		{
			name:  "a chord that explains itself keeps its own words",
			chord: Chord{Emacs: "C-g", Repeatable: true, RepeatWhy: "it is a top-level quit at rest"},
			want:  "top-level quit at rest",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := test.chord.repeatWhy()

			if !strings.Contains(got, test.want) {
				t.Errorf("repeatWhy() = %q, want it to carry %q", got, test.want)
			}
		})
	}
}

// The switching chords advance the selection, so a dropped one must never be
// re-posted: two switches are not one switch.
func TestSwitchingChordsAreNotRepeatable(t *testing.T) {
	tests := []struct {
		name  string
		chord Chord
	}{
		{name: "s-} selects the next workspace", chord: SwitchRight},
		{name: "M-2 selects the second workspace", chord: SwitchToSecond},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if test.chord.Repeatable {
				t.Errorf("%s is marked repeatable, and a second delivery would switch again", test.chord.Emacs)
			}
		})
	}
}

// The hold has to outlive the reading it exists to protect, or the target
// stops being key while this side is still waiting for the mark.
func TestHoldCeilingOutlivesTheConfirmation(t *testing.T) {
	if keyDeliveryHoldCeiling <= keyDeliveryConfirmCeiling {
		t.Errorf("the helper holds the target key for %s and the confirmation may take %s, so the hold can "+
			"expire mid-reading", keyDeliveryHoldCeiling, keyDeliveryConfirmCeiling)
	}
}

// A press that holds must ask the helper to hold, and one that does not must
// not: a `--hold` that went missing would silently restore the old fixed-span
// handback that dropped keys.
func TestKeyDriverArgs(t *testing.T) {
	tests := []struct {
		name string
		hold bool
		want []string
	}{
		{name: "a held press asks for the hold", hold: true, want: []string{"--hold=5", "42", "5", "control"}},
		{name: "a blind press does not", hold: false, want: []string{"42", "5", "control"}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			args := keyDriverArgs(42, Chord{Emacs: "C-g", Keycode: 5, Modifiers: []string{"control"}}, test.hold)

			if len(args) != len(test.want) {
				t.Fatalf("args = %v, want %d of them", args, len(test.want))
			}
			for i, want := range test.want {
				if args[i] != want {
					t.Errorf("args[%d] = %q, want %q", i, args[i], want)
				}
			}
		})
	}
}

// The probe has to read BOTH marks, because either one alone would misread a
// whole class of press — a quit that armed the flag, or an ordinary key that
// never touches it.
func TestInputMarkFormReadsBothMarks(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "it reads the key ring", want: "(recent-keys)"},
		{name: "it reads the quit flag", want: "quit-flag"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			form := inputMarkForm()

			if !strings.Contains(form, test.want) {
				t.Errorf("the input-mark form %q does not read %s", form, test.want)
			}
		})
	}
}

// A driver that was never built must say so rather than post nothing and
// answer success.
func TestPressWithoutABuiltHelper(t *testing.T) {
	driver := &KeyDriver{Pid: 1}

	_, err := driver.PressWithReceipt(context.Background(), SwitchRight)

	if err == nil {
		t.Fatal("pressing through an unbuilt key driver answered no error")
	}
	if !strings.Contains(err.Error(), "has not been built") {
		t.Errorf("error = %v, want it to name the unbuilt helper", err)
	}
}

// A press with no read channel to the editor is a blind post, and it must say
// that rather than report a delivery nobody checked. The helper it runs here is
// a stand-in that only has to exit 0.
func TestPressWithoutAClientIsUndetermined(t *testing.T) {
	driver := &KeyDriver{Pid: 1, helper: "/usr/bin/true"}

	receipt, err := driver.PressWithReceipt(context.Background(), SwitchRight)

	if err != nil {
		t.Fatalf("a blind press through a helper that exits 0 answered %v", err)
	}
	if receipt.Verdict != DeliveryUndetermined {
		t.Errorf("verdict = %d, want undetermined: nothing was read back", receipt.Verdict)
	}
	if !strings.Contains(receipt.Reason, "no read channel") {
		t.Errorf("reason = %q, want it to name the missing read channel", receipt.Reason)
	}
}

// A helper that refuses — which is what it does when the target has no key
// window — must reach the caller as an error carrying what it said.
func TestPressSurfacesAHelperRefusal(t *testing.T) {
	driver := &KeyDriver{Pid: 1, helper: "/usr/bin/false"}

	_, err := driver.PressWithReceipt(context.Background(), SwitchRight)

	if err == nil {
		t.Fatal("a helper that exited non-zero answered no error")
	}
	if !strings.Contains(err.Error(), SwitchRight.Emacs) {
		t.Errorf("error = %v, want it to name the chord that was refused", err)
	}
}

// The confirmation is bounded whatever the editor does, so a press cannot park
// the owner's focus on Emacs indefinitely.
func TestConfirmIsBoundedByAClosedContext(t *testing.T) {
	driver := &KeyDriver{Pid: 1, Client: &Client{Socket: "/nonexistent/socket", Scratch: t.TempDir()}}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	done := make(chan struct{})
	go func() {
		defer close(done)
		driver.confirm(ctx, Chord{Emacs: "n"}, InputMark{Keys: "a b c"}, nil)
	}()

	select {
	case <-done:
	case <-time.After(keyDeliveryConfirmCeiling + 5*time.Second):
		t.Fatal("the confirmation did not return on a cancelled context")
	}
}

// A mark-free chord waits on its EFFECT, and the effect is the account it
// answers with: no mark could have said this, and the press must not need one.
func TestConfirmSettlesAMarkFreeChordOnItsEffect(t *testing.T) {
	// Arrange
	driver := &KeyDriver{Pid: 1, Client: &Client{Socket: "/nonexistent/socket", Scratch: t.TempDir()}}
	quit := Chord{Emacs: "C-g", MarkFree: true, MarkFreeWhy: "the quit character leaves no mark"}
	effect := &DeliveryEffect{
		What:     "the standing minibuffer closed",
		Observed: func(context.Context) bool { return true },
	}

	// Act
	verdict, reason, _, observed := driver.confirm(context.Background(), quit, InputMark{Keys: "a b c"}, effect)

	// Assert
	if verdict != DeliveryArrived || !observed {
		t.Errorf("verdict = %d observed = %v, want an arrival settled by the effect: %s", verdict, observed, reason)
	}
}

// And an effect that never happens must not be reported as a drop: the marks
// cannot speak for this chord either way.
func TestConfirmLeavesAMarkFreeChordUndeterminedWhenItsEffectDoesNotHappen(t *testing.T) {
	// Arrange
	driver := &KeyDriver{Pid: 1, Client: &Client{Socket: "/nonexistent/socket", Scratch: t.TempDir()}}
	quit := Chord{Emacs: "C-g", MarkFree: true, MarkFreeWhy: "the quit character leaves no mark"}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	effect := &DeliveryEffect{
		What:     "the standing minibuffer closed",
		Observed: func(context.Context) bool { return false },
	}

	// Act
	verdict, _, _, observed := driver.confirm(ctx, quit, InputMark{Keys: "a b c"}, effect)

	// Assert
	if verdict == DeliveryAbsent || observed {
		t.Errorf("verdict = %d observed = %v, want the press blamed on nobody", verdict, observed)
	}
}
