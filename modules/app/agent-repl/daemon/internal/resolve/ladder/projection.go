package ladder

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE TWO PROJECTIONS, side by side, so the correspondence between the
// surfaces' arms is read in one place. Every arm either surface can emit maps
// onto exactly one claim; an arm with no row here is one this package has not
// placed on the ladder, which each resolver reports loudly.

// rosterArmClaims places every RosterRow.status arm, by its proto name.
var rosterArmClaims = map[string]Claim{
	"merge_queued":   Merging,
	"merging":        Merging,
	"init":           Disconnected,
	"severed":        Disconnected,
	"dead":           Disconnected,
	"start_failed":   Disconnected,
	"degraded":       Degraded,
	"merge_failed":   MergeFailed,
	"merged":         Merged,
	"vendor_blocked": Blocked,
	"api_retrying":   Blocked,
	"permission":     Waiting,
	"submitting":     Thinking,
	"thinking":       Thinking,
	"clearing":       Thinking,
	"compacting":     Thinking,
	"done":           Idle,
	"interrupted":    Idle,
	"turn_failed":    Idle,
	"idle_async":     Idle,
	"ready":          Idle,
	"none":           Idle,
	"inactive":       Inactive,
}

// RosterArmClaim names the claim a roster status arm makes, and false for an
// arm this package has not placed.
func RosterArmClaim(arm string) (Claim, bool) {
	claim, ok := rosterArmClaims[arm]
	return claim, ok
}

// RosterArms is every roster arm this package places, for the tests that hold
// the placement against the proto's own oneof.
func RosterArms() []string {
	out := make([]string, 0, len(rosterArmClaims))
	for arm := range rosterArmClaims {
		out = append(out, arm)
	}
	return out
}

// FooterClaim names the claim a footer status makes, and false for an unset
// status.
//
// ONE ARM SPLITS ON ITS STEP. `waiting · wakeup` is the fallback the footer
// admits only where it would otherwise read idle — a self-scheduled wakeup is
// the foreground being free — so it projects onto the idle rung, where every
// other waiting step waits on the user.
func FooterClaim(status *frontendv1.FooterStatus) (Claim, bool) {
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Merging:
		return Merging, true
	case *frontendv1.FooterStatus_Disconnected:
		return Disconnected, true
	case *frontendv1.FooterStatus_Closing:
		return Closing, true
	case *frontendv1.FooterStatus_MergeFailed:
		return MergeFailed, true
	case *frontendv1.FooterStatus_Merged:
		return Merged, true
	case *frontendv1.FooterStatus_Blocked:
		return Blocked, true
	case *frontendv1.FooterStatus_Degraded:
		return Degraded, true
	case *frontendv1.FooterStatus_Waiting:
		if arm.Waiting.GetWakeup() != nil {
			return Idle, true
		}
		return Waiting, true
	case *frontendv1.FooterStatus_Working, *frontendv1.FooterStatus_Loading:
		return Thinking, true
	case *frontendv1.FooterStatus_Interrupted, *frontendv1.FooterStatus_Background,
		*frontendv1.FooterStatus_Idle, *frontendv1.FooterStatus_TurnFailed:
		return Idle, true
	default:
		return "", false
	}
}
