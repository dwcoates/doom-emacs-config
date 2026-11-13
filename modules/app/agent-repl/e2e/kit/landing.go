package kit

import "strings"

// landingStateFields are the facts the settle reads, in the order the state
// string carries them.
//
// A STATE STRING RATHER THAN A BARE YES/NO, because a wait that is never
// satisfied must name which half of "settled" is missing: "the landing is
// still pending" and "the panel has no window yet" are different defects and
// a bald `false` tells a reader neither.
//
// `LandingStateSeparator` is ` | ` rather than a space because one of the
// fields is a BUFFER NAME, which carries spaces of its own; splitting a
// space-separated line would attribute half a buffer name to the next field.
const LandingStateSeparator = " | "

// ParseLandingState splits the state string into its named fields.
func ParseLandingState(state string) map[string]string {
	fields := map[string]string{}
	for _, part := range strings.Split(state, LandingStateSeparator) {
		key, value, found := strings.Cut(part, "=")
		if !found {
			continue
		}
		fields[key] = value
	}
	return fields
}

// LandingSettled answers whether the state string describes a landing that
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
func LandingSettled(state string) bool {
	fields := ParseLandingState(state)
	ws := fields["ws"]
	if ws == "" || ws == "nil" {
		return false
	}
	return fields["landing"] == "nil" &&
		fields["current"] == ws &&
		fields["pending"] == "nil" &&
		fields["window"] == "t"
}
