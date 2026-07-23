package merge

import (
	"fmt"
	"strings"
	"testing"
)

// TestPhaseConstantsMatchWireVocabulary guards the Phase string values
// against drift from the frontend.v1 RenderState / SSM snake_case vocabulary
// they must resolve against.
func TestPhaseConstantsMatchWireVocabulary(t *testing.T) {
	tests := []struct {
		phase Phase
		want  string
	}{
		{PhaseMerging, "merging"},
		{PhaseMergeQueued, "merge_queued"},
		{PhaseMergeConflict, "merge_conflict"},
		{PhaseMergeFailed, "merge_failed"},
		{PhaseMerged, "merged"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			if string(tc.phase) != tc.want {
				t.Errorf("phase = %q, want %q", string(tc.phase), tc.want)
			}
		})
	}
}

func TestEmitLoudLogsAndRecords(t *testing.T) {
	// Arrange.
	var logged []string
	sink := &recordingSink{}
	em := &stateEmitter{sink: sink, logf: func(f string, a ...any) {
		logged = append(logged, fmt.Sprintf(f, a...))
	}}

	// Act.
	err := em.emit("ws1", PhaseMergeConflict, "conflict at abc123")

	// Assert.
	if err != nil {
		t.Fatalf("emit() err = %v", err)
	}
	if len(sink.got) != 1 || sink.got[0] != (transition{"ws1", PhaseMergeConflict, "conflict at abc123"}) {
		t.Fatalf("sink recorded %+v, want the single transition", sink.got)
	}
	joined := strings.Join(logged, "\n")
	for _, want := range []string{"ws=ws1", "phase=merge_conflict", "cause=conflict at abc123"} {
		if !strings.Contains(joined, want) {
			t.Errorf("log %q missing %q", joined, want)
		}
	}
}

func TestEmitPropagatesSinkError(t *testing.T) {
	// Arrange.
	sink := &recordingSink{failOn: PhaseMerged}
	em := &stateEmitter{sink: sink, logf: func(string, ...any) {}}

	// Act.
	err := em.emit("ws1", PhaseMerged, "done")

	// Assert.
	if err == nil {
		t.Fatalf("emit() err = nil; want the sink error surfaced")
	}
	if !strings.Contains(err.Error(), string(errFakeSink)) {
		t.Errorf("emit() err = %v; want it to wrap the sink error", err)
	}
}

func TestEmitRejectsUnknownPhase(t *testing.T) {
	// Arrange.
	sink := &recordingSink{}
	em := &stateEmitter{sink: sink, logf: func(string, ...any) {}}

	// Act.
	err := em.emit("ws1", Phase("bogus"), "cause")

	// Assert.
	if err == nil {
		t.Fatalf("emit() err = nil; want an unknown-phase rejection")
	}
	if len(sink.got) != 0 {
		t.Fatalf("emit() recorded %+v for an unknown phase; want the sink untouched", sink.got)
	}
}
