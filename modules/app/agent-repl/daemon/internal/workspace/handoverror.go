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
	var refusal *Refusal
	if errors.As(err, &refusal) && (refusal.Arm == ArmTransferringAway || refusal.Arm == ArmNotYetAdopted) {
		return true
	}
	return errors.Is(err, bringup.ErrNotServed) ||
		errors.Is(err, shimclient.ErrStandingDown) ||
		errors.Is(err, rollout.ErrNotYetAdopted) ||
		errors.Is(err, bounce.ErrMovedAway) ||
		errors.Is(err, context.Canceled)
}
