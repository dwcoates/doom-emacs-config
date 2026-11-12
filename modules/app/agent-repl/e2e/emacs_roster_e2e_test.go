package e2e

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

// AREA E of EMACS-LAYER-SPEC.md: ROSTER PAINT, MODELINE AND ATTENTION.
// Scenarios 26 through 30. EMACS-ONLY: a Connect-dialing client sees the
// roster frames but never sees what Emacs DECIDED to paint from them.
//
// The tab paint itself is SVG sized to the tab-bar line height, which a tty
// frame cannot rasterize. That is not a gap and this file does not pretend
// otherwise: the paint DECISION is data — the arm vocabulary,
// `agent-repl-status-color-table', `agent-repl--color-by-name' and
// `agent-repl-status-blink-schedule' — and the decision is the claim.
// Rasterization is not this layer's.
//
// The one deliberate exception is scenario 30, whose subject IS the rendered
// string: `agent-repl-link-drain-segment' is a mode-line composition, and it
// is asserted as text on purpose.

// ---------------------------------------------------------------------------
// 26. RosterArmsPaintTheTabInOrder
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 27. UnknownRosterArmIsRefusedLoudly
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 28. PermissionAskFiresTheAttentionMarker
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 29. UnfocusedTurnEndRaisesTheDaemonsBanner
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 30. DrainScheduleDrawsTheStandingBanner
// ---------------------------------------------------------------------------

// testEmacsDrainScheduleDrawsTheStandingBanner is scenario 30.
//
// `agent-repl-daemon-shutdown-schedule' prompts for the reason through
// `completing-read', so the pick is stubbed FOR THE DURATION OF THE ONE CALL
// — the standard ERT way, which leaves the command running its own
// argument-collection code path — rather than the command being bypassed.
//
// This scenario's assertion on `agent-repl-link-drain-segment' is a RENDERED
// STRING, deliberately: the segment's own composition is the subject, and it
// is the sanctioned exception to "never scrape human text where a variable
// exists". The variable behind it (`agent-repl-link-drain') is asserted too.
func testEmacsDrainScheduleDrawsTheStandingBanner(t *testing.T) {
	s := newEmacsScenario(t)
	e := s.E

	// Five minutes out: far enough that the daemon is still serving for the
	// whole test and its teardown.
	const drainMinutes = 5
	const reason = "maintenance"

	before := e.EvalInt(`(truncate (* 1000 (float-time)))`)
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(reason) + `)))
              (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(drainMinutes) + `)
              t)`)

	// The daemon echoes the schedule back as a `drain_scheduled' push; the
	// standing schedule is what Emacs then holds.
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs",
		`(and agent-repl-link-drain t)`)

	if arm := e.EvalString(`(format "%s" (plist-get (plist-get agent-repl-link-drain :reason) :arm))`); arm != ":"+reason {
		t.Errorf("the standing drain's reason arm is %q, want %q", arm, ":"+reason)
	}

	atMS := e.EvalInt(`(plist-get agent-repl-link-drain :at-ms)`)
	const minuteMS = 60 * 1000
	if lo, hi := before+drainMinutes*minuteMS, before+drainMinutes*minuteMS+int(DefaultTimeout.Milliseconds()); atMS < lo || atMS > hi {
		t.Errorf("the standing drain fires at %d ms, want it inside [%d, %d]", atMS, lo, hi)
	}

	// The rendered segment. Its shape is "drain HH:MM · <reason>".
	segment := e.EvalString(`(or agent-repl-link-drain-segment "")`)
	if !strings.HasPrefix(segment, "drain ") {
		t.Fatalf("the drain segment is %q, want it to open with \"drain \"", segment)
	}
	if !strings.HasSuffix(segment, "· "+reason) {
		t.Errorf("the drain segment %q does not name the reason %q", segment, reason)
	}

	// And it is ON the mode line, not merely computed: a segment nothing
	// draws is a banner the user never sees.
	if !decodeBool(e.Eval(`(and (memq 'agent-repl-link-drain-segment
                                        (default-value 'global-mode-string))
                                  t)`)) {
		t.Error("agent-repl-link-drain-segment is not in global-mode-string")
	}

}

// decodeBool reads an elisp truth value back. `json-encode' writes nil as
// `null' and t as `true', so anything but a JSON null is truth.
func decodeBool(raw json.RawMessage) bool {
	if isJSONNull(raw) {
		return false
	}
	return string(raw) != "false"
}

// bannersTitled counts the banners among INVOCATIONS whose title is exactly
// TITLE.
func bannersTitled(invocations []harness.Invocation, title string) int {
	n := 0
	for _, inv := range invocations {
		if slices.Contains(inv.Argv, title) {
			n++
		}
	}
	return n
}
