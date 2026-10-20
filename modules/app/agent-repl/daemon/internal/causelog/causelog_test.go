package causelog

import (
	"errors"
	"fmt"
	"strings"
	"testing"
)

func TestTracker(t *testing.T) {
	tests := []struct {
		name  string
		steps func(tr *Tracker) []bool
		want  []bool
	}{
		{
			name:  "a first failure is recorded",
			steps: func(tr *Tracker) []bool { return []bool{tr.Failed("list", "EPERM")} },
			want:  []bool{true},
		},
		{
			name:  "a persisting cause is recorded once",
			steps: func(tr *Tracker) []bool { return []bool{tr.Failed("list", "EPERM"), tr.Failed("list", "EPERM")} },
			want:  []bool{true, false},
		},
		{
			name:  "a changed cause is recorded again",
			steps: func(tr *Tracker) []bool { return []bool{tr.Failed("list", "EPERM"), tr.Failed("list", "ENOMEM")} },
			want:  []bool{true, true},
		},
		{
			name:  "a success with nothing standing is not recorded",
			steps: func(tr *Tracker) []bool { return []bool{tr.Succeeded("list")} },
			want:  []bool{false},
		},
		{
			name: "a recovery is recorded once and rearms the failure",
			steps: func(tr *Tracker) []bool {
				return []bool{tr.Failed("list", "EPERM"), tr.Succeeded("list"), tr.Succeeded("list"), tr.Failed("list", "EPERM")}
			},
			want: []bool{true, true, false, true},
		},
		{
			name:  "steps are independent",
			steps: func(tr *Tracker) []bool { return []bool{tr.Failed("list", "EPERM"), tr.Failed("host", "EPERM")} },
			want:  []bool{true, true},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var tr Tracker

			// Act
			got := tt.steps(&tr)

			// Assert
			if len(got) != len(tt.want) {
				t.Fatalf("got %v, want %v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("step %d = %v, want %v (all: %v)", i, got[i], tt.want[i], got)
				}
			}
		})
	}
}

func TestNoteRecordsEachNewCauseAndTheRecoveryOnce(t *testing.T) {
	// Arrange
	var tr Tracker
	var got []string
	note := func(err error) bool {
		return tr.Note("scrape", err, func(cause string) { got = append(got, "failed:"+cause) }, func() { got = append(got, "recovered") })
	}

	// Act
	oks := []bool{
		note(nil),
		note(errors.New("a")),
		note(errors.New("a")),
		note(errors.New("b")),
		note(nil),
		note(nil),
	}

	// Assert
	want := []string{"failed:a", "failed:b", "recovered"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("records = %v, want %v", got, want)
	}
	if fmt.Sprint(oks) != "[true false false false true true]" {
		t.Fatalf("answers = %v, want each repetition's success", oks)
	}
}
