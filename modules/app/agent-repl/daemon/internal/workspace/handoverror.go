package workspace

import (
	"context"
	"errors"

	"agent-repld/internal/bounce"
	"agent-repld/internal/bringup"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimclient"
)

// HandedOver answers whether ERR is a HANDOVER rather than a failure: the
// workspace or the bring-up belongs to another daemon now, or this one is
// exiting. Every case is the rollout's ordering, which the caller is answered
// with and acts on; none is something to analyze. Every caller that must tell
// a handover from a failure asks here (a create, a held create's resume, the
// cache keeper's restore), so the set of handover errors cannot drift apart.
func HandedOver(err error) bool {
	if _, refused := StandingRefusal(err); refused {
		return true
	}
	return errors.Is(err, bringup.ErrNotServed) ||
		errors.Is(err, shimclient.ErrStandingDown) ||
		errors.Is(err, rollout.ErrNotYetAdopted) ||
		errors.Is(err, bounce.ErrMovedAway) ||
		errors.Is(err, context.Canceled)
}

// StandingRefusal answers the serving standing a verb refused ERR on: its
// transferring_away and not_yet_adopted arms (verbs.owned), and false for any
// other error, a cancelled context among them. A caller that must tell a
// verb's refusal on the standing from every other failure asks here, so the
// arms it reads are the ones HandedOver reads.
func StandingRefusal(err error) (Standing, bool) {
	var refusal *Refusal
	if !errors.As(err, &refusal) {
		return StandingOwned, false
	}
	switch refusal.Arm {
	case ArmTransferringAway:
		return StandingTransferringAway, true
	case ArmNotYetAdopted:
		return StandingNotYetAdopted, true
	}
	return StandingOwned, false
}
