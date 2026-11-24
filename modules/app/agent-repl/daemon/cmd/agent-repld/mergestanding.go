package main

import (
	"context"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/rollout"
	"agent-repld/internal/workspace"
)

// mergeStanding is the rollout controller as the merge queue reads its
// standing (merge.Standing): the controller's queue answers, and each
// workspace's serving standing read from the ownership every workspace verb
// refuses on (workspace.Ownership), so the deploy mediation's gate asks
// exactly what the verbs it guards ask.
type mergeStanding struct {
	rollout.Controller
	ownership workspace.Ownership
}

var _ merge.Standing = mergeStanding{}

// ServingStanding implements merge.Standing.
func (s mergeStanding) ServingStanding(ws ids.WorkspaceID) (merge.ServingStanding, error) {
	standing, err := s.ownership.Standing(context.Background(), ws)
	if err != nil {
		return merge.ServedHere, err
	}
	return mergeServingStanding(standing)
}

// mergeStandingRefusal is merge.Deps.StandingRefusal: the serving standing a
// workspace verb refused ERR on, read from the refusal's typed arm
// (workspace.StandingRefusal), and false for any other error.
func mergeStandingRefusal(err error) (merge.ServingStanding, bool) {
	standing, refused := workspace.StandingRefusal(err)
	if !refused {
		return merge.ServedHere, false
	}
	out, terr := mergeServingStanding(standing)
	if terr != nil {
		// A REFUSAL IS ONLY EVER MADE ON A STANDING NOT OWNED, each of which
		// has its merge queue spelling, so one that has none is a broken
		// invariant.
		panic(fmt.Sprintf("agent-repld: a workspace verb refused on a standing the merge queue cannot spell: %v", terr))
	}
	return out, true
}

// mergeServingStanding spells the workspace verbs' standing as the merge
// queue does. The two enumerations are separate, as merge imports neither
// the rollout nor the verbs, so the translation is exhaustive and an
// unrecognized value is an error, never a silent "served".
func mergeServingStanding(standing workspace.Standing) (merge.ServingStanding, error) {
	switch standing {
	case workspace.StandingOwned:
		return merge.ServedHere, nil
	case workspace.StandingTransferringAway:
		return merge.TransferringAway, nil
	case workspace.StandingNotYetAdopted:
		return merge.NotYetAdopted, nil
	}
	return merge.ServedHere, fmt.Errorf("agent-repld: the serving standing %d has no merge queue spelling", int(standing))
}
