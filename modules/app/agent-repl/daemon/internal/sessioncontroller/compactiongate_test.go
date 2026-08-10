package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/ssm"
)

// The ONE question both daemon compactions ask. The two call sites' behavior on
// each answer is covered where they live (warmcompact_test.go,
// revive_test.go); this covers the shared reading itself, so a third
// daemon-initiated compaction added later inherits a tested answer.

func TestCompactionRedundantAnswersTheGate(t *testing.T) {
	tests := []struct {
		name string
		gate ssm.CompactionGate
		want bool
	}{
		{
			name: "a compaction with nothing said since is redundant",
			gate: ssm.CompactionGate{CompactedAtMs: 200, PromptAtMs: 100},
			want: true,
		},
		{
			name: "a prompt since the compaction is new material",
			gate: ssm.CompactionGate{CompactedAtMs: 200, PromptAtMs: 300},
			want: false,
		},
		{
			name: "a workspace that has never compacted is never redundant",
			gate: ssm.CompactionGate{},
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, applier, _ := newWiredRig(t)
			applier.setCompactionGate("ws", tc.gate)

			// Act.
			redundant, gate, err := m.compactionRedundant("ws")

			// Assert.
			if err != nil {
				t.Fatalf("compactionRedundant: %v", err)
			}
			if redundant != tc.want {
				t.Fatalf("redundant = %v, want %v", redundant, tc.want)
			}
			if gate != tc.gate {
				t.Fatalf("gate = %+v, want the read passed back for the caller's log", gate)
			}
		})
	}
}

// THE READ FAILURE IS RETURNED, never absorbed into a permissive "not
// redundant". A caller that cannot read the gate does not know whether it is
// about to duplicate a whole-conversation model call.
func TestCompactionRedundantSurfacesAFailedRead(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	applier.reconcMutex.Lock()
	applier.compactionGateErr = errors.New("the state store is gone")
	applier.reconcMutex.Unlock()

	// Act.
	redundant, _, err := m.compactionRedundant("ws")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the state store is gone") {
		t.Fatalf("err = %v, want the read failure surfaced", err)
	}
	if redundant {
		t.Fatal("a failed read reported redundant=true; the verdict must come from the gate, never from the failure")
	}
}

// The declined-compaction logs carry the two timestamps the verdict was taken
// from, so a reader can tell a gate that closed an hour ago from one that
// closed a moment ago without correlating another line.
func TestCompactionRedundantDetailNamesBothTimestamps(t *testing.T) {
	got := compactionRedundantDetail(ssm.CompactionGate{CompactedAtMs: 200, PromptAtMs: 100})

	if got != "last_compacted_at_ms=200 last_prompt_at_ms=100" {
		t.Fatalf("detail = %q, want both timestamps named", got)
	}
}
