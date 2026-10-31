package topbar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE REMOTE-CONTROL CHIP IS A WORKSPACE FACT, never a session one: the mode
// is the daemon's, kept in wsm (wsm.RemoteControl), so every strip draws it,
// shim or no shim. It rides the naming (Naming.RemoteControl) because the
// naming is the readiness gate: no view is ever published before the mode was
// stated, which is what makes the chip ALWAYS SET (topbar.proto). A change is
// stated alone (SetRemoteControl) and publishes at once.

// The chip's hover text, one per mode, composed by the daemon.
const (
	remoteControlOnTooltip  = "Remote control on: this session is reachable from claude.ai, and its shim stays up between keep-alives until the workspace hibernates · click to turn off"
	remoteControlOffTooltip = "Remote control off · click to make this session reachable from claude.ai (keeps its shim up between keep-alives)"
)

// SetRemoteControl replaces the workspace's remote-control mode, leaving the
// rest of the naming as it is. Its sole producer is the fleet, on every
// UpdateRemoteControlMode.
func (r *resolver) SetRemoteControl(ws ids.WorkspaceID, on bool) {
	r.mutate(ws, "daemon.topbar.set_remote_control", "the topbar took the workspace's remote-control mode",
		dlog.Context{"remote_control": on},
		func(s *wsState) {
			s.naming.RemoteControl = on
		})
}

// remoteControlChip projects the mode onto the chip: the arm, and the hover
// text saying what the mode does and what a click does.
func remoteControlChip(on bool) *frontendv1.TopbarRemoteControl {
	if on {
		return &frontendv1.TopbarRemoteControl{
			Mode:    &frontendv1.TopbarRemoteControl_On{On: &frontendv1.TopbarRemoteControlModeOn{}},
			Tooltip: &frontendv1.TopbarRemoteControlTooltip{Text: remoteControlOnTooltip},
		}
	}
	return &frontendv1.TopbarRemoteControl{
		Mode:    &frontendv1.TopbarRemoteControl_Off{Off: &frontendv1.TopbarRemoteControlModeOff{}},
		Tooltip: &frontendv1.TopbarRemoteControlTooltip{Text: remoteControlOffTooltip},
	}
}
