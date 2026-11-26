package topbar

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE PRODUCTION-CHECKS CHIP is the daemon's view of the workspace's
// repository's production checks (agentrepl.v1.UpdateProductionPause). It
// rides the naming (Naming.ProductionChecks) so the strip the naming
// completes already draws it, and a pause or a resume is stated alone
// (SetProductionChecks) for every workspace of the repository, since the
// standing is the repository's.

// SetProductionChecks replaces the workspace's production-checks chip
// standing, leaving the rest of the naming as it is.
func (r *resolver) SetProductionChecks(ws ids.WorkspaceID, chip ProductionChecks) {
	r.mutate(ws, "daemon.topbar.set_production_checks", "the topbar took the workspace's repository's production-check standing",
		dlog.Context{"on": chip.On, "paused": chip.Paused, "mediator": chip.Mediator, "repository": chip.RepositoryDir},
		func(s *wsState) {
			s.naming.ProductionChecks = chip
		})
}

// productionChecksChip projects the standing onto the chip: nil when the
// repository checks no production, else its arm, the repository a click acts
// on, and the hover naming the mediator and what a click does.
func productionChecksChip(chip ProductionChecks) *frontendv1.TopbarProductionChecks {
	if !chip.On {
		return nil
	}
	reviewer := "its production mediator, not made yet"
	if chip.Mediator != "" {
		reviewer = chip.Mediator
	}
	out := &frontendv1.TopbarProductionChecks{
		Repository: &workspacev1.RepositoryRef{Id: string(chip.Repository), Dir: chip.RepositoryDir},
	}
	if chip.Paused {
		out.Standing = &frontendv1.TopbarProductionChecks_Paused{Paused: &frontendv1.TopbarProductionChecksPaused{}}
		out.Tooltip = &frontendv1.TopbarProductionChecksTooltip{Text: fmt.Sprintf("Production checks are paused: %s is told no span · click to resume them", reviewer)}
		return out
	}
	out.Standing = &frontendv1.TopbarProductionChecks_Running{Running: &frontendv1.TopbarProductionChecksRunning{}}
	out.Tooltip = &frontendv1.TopbarProductionChecksTooltip{Text: fmt.Sprintf("Production checks are running, reviewed by %s · click to pause them", reviewer)}
	return out
}
