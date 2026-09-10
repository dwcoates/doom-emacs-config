package classifier

import (
	"context"
	"strings"
)

// FakeInterjectMarker is the `-fake` judge's scripted interject trigger. A
// prompt containing it is classified interject; AGENTS.md fixes the spelling.
const FakeInterjectMarker = "[interject]"

// The evidence the `-fake` judge states, so a tray entry under --fake carries
// a reason like any other.
const (
	fakeMarkerReason = "the scripted classifier saw its interject marker"
	fakeHoldReason   = "the scripted classifier holds every other prompt"
)

// fakeJudge is the deterministic `-fake` classifier AGENTS.md specifies: the
// explicit-interrupt fast path, the `[interject]` marker, and hold_for_turn_end
// for everything else. It invokes nothing, so it needs no guard.
type fakeJudge struct{}

func (fakeJudge) Judge(_ context.Context, _ string, incoming string) (Verdict, error) {
	if ExplicitInterrupt(incoming) {
		return Verdict{Interject: true, Reason: ExplicitInterruptReason, FastPath: true}, nil
	}
	if strings.Contains(strings.ToLower(incoming), FakeInterjectMarker) {
		return Verdict{Interject: true, Reason: fakeMarkerReason}, nil
	}
	return Verdict{Interject: false, Reason: fakeHoldReason}, nil
}
