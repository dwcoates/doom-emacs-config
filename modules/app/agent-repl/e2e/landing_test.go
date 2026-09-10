package e2e

import (
	"encoding/json"
	"fmt"
	"strings"
)

// THE LANDING A MINTED WORKSPACE PERFORMS, AND THE ONE WAIT THAT SETTLES IT.
//
// A WORKSPACE THIS EDITOR JUST MINTED COMES UP ON ITS OWN PANEL, by design
// and by three different commands: `agent-repl-add-project-workspace`
// registers a directory, `agent-repl-create-workspace` makes a workspace and
// `agent-repl-fork-workspace` forks one, and all three hand the daemon's
// minted `WorkspaceRef` to `agent-repl-verbs-select-minted`. That landing
// stands on the minted worktree and arms `:pending-show-panels`, so the
// perspective activation drain shows the workspace's own view
// (`agent-repl--arm-landing-panels`, `agent-repl--drain-pending-show-panels`).
//
// EVERY STEP OF IT IS ASYNCHRONOUS. The verb returns as soon as the request
// is on the wire; the daemon's answer comes back on a callback; the tab
// itself arrives on the ROSTER stream, so a landing whose answer beat the
// push waits for the tab (`agent-repl-verbs--pending-landing-fire`, run from
// `agent-repl-roster-update-functions`); and `agent-repl-switch-to-project`
// then defers the switch onto a timer so the perspective change completes
// before any blocking I/O. A test that starts measuring windows as soon as
// the registry holds the name is therefore racing a panel show it never
// asked for -- measured under the panels soak, the show arrived MID-SCENARIO
// and collapsed a work layout that had been arranged before it.
//
// So every caller that mints a workspace waits HERE, and there is one
// predicate rather than a copy per caller.

// landingStateFields are the facts the settle reads, in the order the state
// string carries them.
//
// A STATE STRING RATHER THAN A BARE YES/NO, because a wait that is never
// satisfied must name which half of "settled" is missing: "the landing is
// still pending" and "the panel has no window yet" are different defects and
// a bald `false` tells a reader neither.
//
// `landingStateSeparator` is ` | ` rather than a space because one of the
// fields is a BUFFER NAME, which carries spaces of its own; splitting a
// space-separated line would attribute half a buffer name to the next field.
const landingStateSeparator = " | "

// landingStateForm is the elisp that reads the landing's state for the
// workspace WSFORM answers.
//
// WSFORM is a form rather than a name so the two ways a caller knows the
// workspace -- by the directory it registered, by the name the tab arrived
// under -- reach the SAME predicate.
func landingStateForm(wsForm string) string {
	return `(let* ((ws ` + wsForm + `)
                       (buf (and ws (agent-repl--ws-get ws :frontend-buffer))))
                  (format "ws=%s` + landingStateSeparator + `current=%s` + landingStateSeparator +
		`landing=%s` + landingStateSeparator + `pending=%s` + landingStateSeparator +
		`window=%s` + landingStateSeparator + `webview=%s"
                          ws
                          (agent-repl--ws-current-name)
                          (and agent-repl-verbs--pending-landing t)
                          (and ws (agent-repl--ws-get ws :pending-show-panels))
                          (and (buffer-live-p buf) (window-live-p (get-buffer-window buf)))
                          (and (buffer-live-p buf) (buffer-name buf))))`
}

// parseLandingState splits the state string into its named fields.
func parseLandingState(state string) map[string]string {
	fields := map[string]string{}
	for _, part := range strings.Split(state, landingStateSeparator) {
		key, value, found := strings.Cut(part, "=")
		if !found {
			continue
		}
		fields[key] = value
	}
	return fields
}

// landingSettled answers whether the state string describes a landing that
// has COMPLETED.
//
// Settled is all four of these at once, and each one is here because the
// other three do not imply it:
//
//   - the workspace EXISTS in Emacs's registry (`ws`), so the read below is
//     about a workspace rather than about `nil`;
//   - NOTHING IS WAITING TO LAND (`landing`). A minted ref whose tab has not
//     arrived sits in `agent-repl-verbs--pending-landing` and will move the
//     user the instant the roster push reconciles, which is exactly the
//     mid-scenario switch a playbook must not race;
//   - the landing SELECTED it (`current`), which is what
//     `agent-repl-verbs--land-on` does and therefore what "landed" means;
//   - and its panels are ON THE FRAME: nothing is left armed (`pending`) and
//     the workspace's own frontend buffer has a live window (`window`).
//
// THE WINDOW IS WHAT MAKES THE WAIT SAFE. The flag alone reads as "settled"
// in the moment BEFORE the arm, which is every moment between the verb
// returning and the daemon answering.
func landingSettled(state string) bool {
	fields := parseLandingState(state)
	ws := fields["ws"]
	if ws == "" || ws == "nil" {
		return false
	}
	return fields["landing"] == "nil" &&
		fields["current"] == ws &&
		fields["pending"] == "nil" &&
		fields["window"] == "t"
}

// awaitLanding waits out the landing of the workspace WSFORM answers.
//
// The bound is `emacsVerbBound`, not `panelSettleBound`: this wait spans a
// WHOLE VERB -- the call out of Emacs, the daemon's minted ref, the tab's
// arrival on the roster stream and the landing that tab triggers -- rather
// than a panel's own settling on a frame that already holds the workspace.
func awaitLanding(e *Emacs, what, wsForm string) {
	e.t.Helper()
	e.AwaitEvalFor(emacsVerbBound, what, landingStateForm(wsForm),
		func(raw json.RawMessage) bool {
			var state string
			if err := json.Unmarshal(raw, &state); err != nil {
				return false
			}
			return landingSettled(state)
		})
}

// awaitRegistrationLanding waits out the landing that REGISTERING the
// directory DIR performs, for a caller that knows the workspace by the
// directory it handed the daemon rather than by the name the daemon minted.
func awaitRegistrationLanding(e *Emacs, dir string) {
	e.t.Helper()
	awaitLanding(e, "the registration's landing to put the workspace's panel on the frame",
		`(agent-repl--ws-name-for-dir `+elispString(dir)+`)`)
}

// awaitMintedLanding waits out the landing of the workspace WS, for a caller
// that already read the name off the tab the roster push brought.
func awaitMintedLanding(e *Emacs, ws string) {
	e.t.Helper()
	awaitLanding(e, fmt.Sprintf("the landing of the minted workspace %q to put its panel on the frame", ws),
		elispString(ws))
}
