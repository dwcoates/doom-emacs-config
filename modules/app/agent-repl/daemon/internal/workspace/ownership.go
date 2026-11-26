package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/rollout"
)

// THE SERVING STANDING COMES FROM THE ROLLOUT.
//
// Only two things make this daemon stop serving a workspace it knows, and the
// rollout controller creates both: a handover transfers it away, and a joining
// successor has not adopted it yet. The durable serving row says who owns a
// workspace but not that a handover is in flight, so the standing is read from
// the controller and translated here rather than re-derived.

// ownership answers Deps.Ownership from the rollout controller's own facts.
type ownership struct{ rollout rollout.Controller }

// NewOwnership adapts the rollout controller to the serving standing every
// per-workspace verb and rpc refuses on.
func NewOwnership(controller rollout.Controller) Ownership {
	return &ownership{rollout: controller}
}

// Standing translates the rollout's standing into the verbs' spelling.
func (o *ownership) Standing(_ context.Context, ws ids.WorkspaceID) (Standing, error) {
	return translateStanding(ws, o.rollout.Standing(ws))
}

// AwaitAdopted waits on the rollout's own adoption latch, within its adoption
// bound, and translates the standing it reached. A bound that fires is the
// rollout's ERROR, returned with the standing translated as well.
func (o *ownership) AwaitAdopted(ctx context.Context, ws ids.WorkspaceID) (Standing, error) {
	reached, err := o.rollout.AwaitAdopted(ctx, ws)
	standing, translateErr := translateStanding(ws, reached)
	if err != nil {
		return standing, err
	}
	return standing, translateErr
}

// translateStanding carries a rollout standing into the verbs' spelling. The
// two enumerations are deliberately separate — the packages do not share a
// vocabulary — so the translation is exhaustive and an unrecognized value is a
// refusal rather than a silent "owned".
func translateStanding(ws ids.WorkspaceID, standing rollout.Standing) (Standing, error) {
	switch standing {
	case rollout.StandingOwned:
		return StandingOwned, nil
	case rollout.StandingTransferringAway:
		return StandingTransferringAway, nil
	case rollout.StandingNotYetAdopted:
		return StandingNotYetAdopted, nil
	default:
		return StandingOwned, fmt.Errorf("workspace: the rollout reported an unrecognized standing %d for %q", int(standing), ws)
	}
}

// Created hands the creation to the rollout, which owns the standing.
func (o *ownership) Created(ws ids.WorkspaceID) { o.rollout.Created(ws) }
