package workspace

import (
	"errors"
	"fmt"
	"testing"
	"time"

	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/shimclient"
)

// sessionSteps flattens REC's session-start observations to
// "phase/cause/outcome", in the order they were made.
func sessionSteps(rec *restarttimingtest.Recorder) []string {
	var out []string
	for _, o := range rec.Of(restarttiming.SessionStart) {
		out = append(out, fmt.Sprintf("%s/%s/%s", o.Phase(), o.Cause(), o.Outcome()))
	}
	return out
}

func TestStartKindNamesTheStart(t *testing.T) {
	tests := []struct {
		name              string
		fresh, hibernated bool
		want              restarttiming.SessionKind
	}{
		{"a fresh conversation", true, false, restarttiming.SessionFresh},
		{"a hibernated conversation", false, true, restarttiming.SessionWake},
		{"a resumed conversation", false, false, restarttiming.SessionResume},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := startKind(tt.fresh, tt.hibernated); got != tt.want {
				t.Fatalf("startKind(%v, %v) = %s, want %s", tt.fresh, tt.hibernated, got, tt.want)
			}
		})
	}
}

func TestSessionTimingRecordsAStepFromItsStart(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want []string
	}{
		{"a step that succeeded", nil, []string{"vendor/resume/ok"}},
		{"a step that failed", errors.New("refused"), []string{"vendor/resume/failed"}},
		{"a step cut short by an ordered stand-down", shimclient.ErrStandDownOrdered, nil},
		{"a step refused by a standing-down supervisor", shimclient.ErrStandingDown, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var rec restarttimingtest.Recorder
			from := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
			timing := sessionTiming{recorder: &rec, now: func() time.Time { return from.Add(3 * time.Second) }, ws: "w1", kind: restarttiming.SessionResume}

			// Act.
			timing.since(restarttiming.SessionVendor, from, tt.err)

			// Assert.
			if got := sessionSteps(&rec); fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("steps = %v, want %v", got, tt.want)
			}
			if obs := rec.Of(restarttiming.SessionStart); len(obs) == 1 && (obs[0].Seconds() != 3 || obs[0].Workspace() != "w1") {
				t.Fatalf("observation = %+v, want 3s on w1", obs[0])
			}
		})
	}
}

func TestSessionTimingAsRenamesTheKindOnly(t *testing.T) {
	// Arrange.
	timing := sessionTiming{ws: "w1", kind: restarttiming.SessionResume}

	// Act.
	adopted := timing.as(restarttiming.SessionAdopted)

	// Assert.
	if adopted.kind != restarttiming.SessionAdopted || adopted.ws != "w1" || timing.kind != restarttiming.SessionResume {
		t.Fatalf("as = %+v from %+v, want only the copy's kind changed", adopted, timing)
	}
}
