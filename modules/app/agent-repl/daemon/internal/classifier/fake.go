package classifier

import (
	"context"
	"strings"
)

// FakeInterjectMarker is the `-fake` judge's scripted interrupt trigger. A
// prompt containing it is routed to interrupt; AGENTS.md fixes the spelling.
const FakeInterjectMarker = "[interject]"

// FakeAfterToolCallMarker is the `-fake` judge's scripted after-tool-call
// trigger. A prompt containing it joins the running turn; AGENTS.md fixes the
// spelling.
const FakeAfterToolCallMarker = "[after-tool-call]"

// The evidence the `-fake` judge states, so a tray entry under --fake carries
// a reason like any other.
const (
	fakeMarkerReason        = "the scripted classifier saw its interject marker"
	fakeAfterToolCallReason = "the scripted classifier saw its after-tool-call marker"
	fakeHoldReason          = "the scripted classifier holds every other prompt"
)

// fakeJudge is the deterministic `-fake` classifier AGENTS.md specifies: the
// explicit-interrupt fast path, the `[interject]` and `[after-tool-call]`
// markers, and hold_for_turn_end for everything else. It invokes nothing, so it needs no guard.
type fakeJudge struct{}

func (fakeJudge) Judge(_ context.Context, _ string, incoming string) (Verdict, error) {
	if ExplicitInterrupt(incoming) {
		return Verdict{Route: RouteInterrupt, Reason: ExplicitInterruptReason, FastPath: true}, nil
	}
	lower := strings.ToLower(incoming)
	if strings.Contains(lower, FakeInterjectMarker) {
		return Verdict{Route: RouteInterrupt, Reason: fakeMarkerReason}, nil
	}
	if strings.Contains(lower, FakeAfterToolCallMarker) {
		return Verdict{Route: RouteAfterToolCall, Reason: fakeAfterToolCallReason}, nil
	}
	return Verdict{Route: RouteQueue, Reason: fakeHoldReason}, nil
}
