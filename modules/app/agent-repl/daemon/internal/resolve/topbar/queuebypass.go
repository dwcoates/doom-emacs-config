package topbar

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE QUEUE-BYPASS CHIP is the daemon's view of the workspace's standing
// toward its repository's queue bypass mode (agentrepl.v1.UpdateQueueBypassMode).
// It rides the naming (Naming.QueueBypass) so the strip the naming completes
// already draws it, and a change is stated alone (SetQueueBypass) for every
// workspace of the repository, since a take or a release changes each
// sibling's chip.

// The chip's hover texts.
const (
	queueBypassOffTooltip = "Pause repository merge queue and make all changes in this workspace bypass it"
	queueBypassOnTooltip  = "Queue bypass mode is on: the repository's merge queue is hard-stopped and this workspace lands its changes directly · click to resume the queue"
)

// SetQueueBypass replaces the workspace's queue-bypass chip standing, leaving
// the rest of the naming as it is.
func (r *resolver) SetQueueBypass(ws ids.WorkspaceID, chip QueueBypass) {
	r.mutate(ws, "daemon.topbar.set_queue_bypass", "the topbar took the workspace's queue bypass standing",
		dlog.Context{"offered": chip.Offered, "holds": chip.Holds, "held_by": chip.HeldBy},
		func(s *wsState) {
			s.naming.QueueBypass = chip
		})
}

// queueBypassChip projects the standing onto the chip: nil when it is not
// offered, else its arm and the hover saying what a click does.
func queueBypassChip(chip QueueBypass) *frontendv1.TopbarQueueBypass {
	switch {
	case !chip.Offered:
		return nil
	case chip.Holds:
		return &frontendv1.TopbarQueueBypass{
			Mode:    &frontendv1.TopbarQueueBypass_On{On: &frontendv1.TopbarQueueBypassOn{}},
			Tooltip: &frontendv1.TopbarQueueBypassTooltip{Text: queueBypassOnTooltip},
		}
	case chip.HeldBy != "":
		return &frontendv1.TopbarQueueBypass{
			Mode: &frontendv1.TopbarQueueBypass_HeldElsewhere{HeldElsewhere: &frontendv1.TopbarQueueBypassHeldElsewhere{WorkspaceName: chip.HeldBy}},
			Tooltip: &frontendv1.TopbarQueueBypassTooltip{
				Text: fmt.Sprintf("Queue bypass mode is held by %s; only one workspace of a repository can hold it", chip.HeldBy),
			},
		}
	}
	return &frontendv1.TopbarQueueBypass{
		Mode:    &frontendv1.TopbarQueueBypass_Off{Off: &frontendv1.TopbarQueueBypassOff{}},
		Tooltip: &frontendv1.TopbarQueueBypassTooltip{Text: queueBypassOffTooltip},
	}
}
