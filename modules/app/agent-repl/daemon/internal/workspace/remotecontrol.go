package workspace

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// REMOTE CONTROL: a workspace's session reachable from claude.ai (and its
// mobile app) through the vendor's Remote Control bridge, which the shim
// drives (shim.v1.SetSessionRemoteControl). The MODE is the daemon's, kept per
// workspace in wsm (wsm.RemoteControl): unset, on for a repository's main
// worktree and off for every other workspace; any explicit set wins (owner
// ruling, 2026-10-31).
//
// THE SHIM HOLDS THE BRIDGE FOR ITS LIFE, NOT THE DAEMON'S. A shim puts the
// request's state in force on every vendor process it opens, so the daemon
// sends it once per shim: at every bring-up whose mode is on (sessionUp, and
// Fleet.Resume for a relaunched shim), and on every change. AN ADOPTED SHIM IS
// SENT NOTHING. A shim a previous daemon started (a boot adoption, a
// handover's successor) kept the bridge that daemon put in force, exactly as
// it kept its effort; the mode cannot have changed meanwhile, because only a
// daemon changes it, and the daemon that changed it sent the change to that
// same shim. A shim that predates the feature is a stale build, and its
// replacement is a relaunch, which comes through Fleet.Resume.
//
// THE SAME claude.ai SESSION ACROSS SHIMS. The bridge session an on answers is
// kept in wsm and handed back on the next shim's request, so claude.ai lists
// one session for the workspace rather than one per shim.
//
// ONE GATE PER WORKSPACE. Every read of the mode that a send acts on, and the
// send, happen under the workspace's remote-control gate, so a change racing a
// bring-up cannot leave the shim in the mode before it: whichever sends last
// read the mode the other wrote.

const (
	opRemoteControl = "daemon.workspace.remote_control"
	// remoteControlWarning keys the warning strip's line for a bridge the
	// vendor would not put in the asked state.
	remoteControlWarning = opRemoteControl
)

// RemoteControlBound bounds ONE SetSessionRemoteControl. It is a PRODUCT
// window, not a bound on the daemon's own work: what it covers is the vendor
// connecting its bridge to claude.ai. No measurement of a healthy connect
// exists yet (the bridge is new), so it is sized at half of
// DefaultStartSessionBound, the window for the vendor work a start covers; a
// re-measure belongs here once the first runs are recorded. An overrun is the
// refusal path: logged at ERROR and stated on the warning strip.
const RemoteControlBound = 30 * time.Second

// UpdateRemoteControlMode records the mode CHANGE leaves on workspace WS and
// puts it in force: on the live shim when one is up, and by starting one when
// the mode turns on with none (a settings visit's start, which the drain never
// stands down while the mode is on). An off with no shim is recorded alone.
func (v *verbs) UpdateRemoteControlMode(ctx context.Context, ws ids.WorkspaceID, change wsm.RemoteControlChange) (bool, error) {
	const rpc = "UpdateRemoteControlMode"
	_, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return false, err
	}
	on, err := v.deps.Sessions.SetRemoteControl(ctx, log, ws, change)
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
		return on, err
	}
	if !on {
		log.Info(opRemoteControl, "no shim holds a session; remote control off is recorded alone", nil)
		return on, nil
	}
	// A MODE TURNED ON WITH NO SHIM STARTS ONE, which every bring-up puts in
	// remote control (reapplyRemoteControl). A shim held with no session yet
	// is left to the start that is coming: the revival finds it up.
	if err := v.deps.Queue.ReviveForSettings(ctx, ws); err != nil {
		log.Error(opRemoteControl, "remote control was turned on and the workspace's shim could not be started", dlog.Context{"cause": err.Error()})
		return on, fmt.Errorf("update remote control on %q: start the shim: %w", ws, err)
	}
	log.Info(opRemoteControl, "no shim holds a session; remote control on is recorded and the session's start puts it in force", nil)
	return on, nil
}

// SetRemoteControl records the mode CHANGE leaves on workspace WS, states it to
// the topbar, and puts it in force on the workspace's live shim. It answers the
// mode recorded, with the shim's no_session refusal when no shim holds a
// session to bridge (the next bring-up puts the mode in force), and the
// vendor's refusal, which also rides the warning strip.
func (f *Fleet) SetRemoteControl(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, change wsm.RemoteControlChange) (bool, error) {
	gate := f.remoteControlGate(ws)
	gate.Lock()
	defer gate.Unlock()
	on, err := f.deps.DB.SetRemoteControlMode(ctx, ws, change)
	if err != nil {
		log.Error(opRemoteControl, "could not record the workspace's remote-control mode", dlog.Context{"change": change.String(), "cause": err.Error()})
		return false, fmt.Errorf("update remote control on %q: %w", ws, err)
	}
	f.deps.Topbar.SetRemoteControl(ws, on)
	fields := dlog.Context{"change": change.String(), "remote_control": on}
	log.Info(opRemoteControl, "recorded the workspace's remote-control mode", fields)
	client, ok := f.Client(ws)
	if !ok {
		return on, &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession, Detail: "the workspace has no live shim"}
	}
	if err := f.applyRemoteControl(ctx, log, ws, client, on); err != nil {
		var refusal *ShimRefusal
		if errors.As(err, &refusal) && refusal.Arm == ArmShimNoSession {
			log.Info(opRemoteControl, "the shim holds no session yet; its session's start puts the mode in force", fields)
			return on, err
		}
		if shimGone(err) {
			log.Info(opRemoteControl, "the shim went under the call; the next bring-up puts the mode in force", dlog.WithCause(fields, err))
			return on, &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession, Detail: err.Error()}
		}
		log.Error(opRemoteControl, "the shim did not put the remote-control mode in force", dlog.WithCause(fields, err))
		f.deps.Topbar.RaiseWarning(ws, remoteControlWarning, fmt.Sprintf("could not turn remote control %s: %v", onOff(on), err))
		return on, err
	}
	log.Info(opRemoteControl, "the shim put the remote-control mode in force", fields)
	return on, nil
}

// reapplyRemoteControl puts a freshly started shim in remote control when the
// workspace's mode is on, read at this moment. An off needs nothing: a new
// shim starts with no bridge.
//
// A REFUSAL IS LOUD, NEVER FATAL. The session runs either way; the chip still
// says the mode is on, so the failure is logged at ERROR and stated on the
// warning strip, the reader's one error surface, as a failed effort put-back
// is.
func (f *Fleet) reapplyRemoteControl(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclientRemoteControl) {
	gate := f.remoteControlGate(ws)
	gate.Lock()
	defer gate.Unlock()
	standing, err := f.deps.DB.RemoteControl(ctx, ws)
	if err != nil {
		log.Error(opRemoteControl, "could not read the workspace's remote-control mode for its new shim", dlog.Context{"cause": err.Error()})
		f.deps.Topbar.RaiseWarning(ws, remoteControlWarning, fmt.Sprintf("could not read whether this session should be reachable from claude.ai: %v", err))
		return
	}
	if !standing.On {
		log.Debug(opRemoteControl, "remote control is off; the new shim is left with no bridge", dlog.Context{"remote_control_recorded": standing.Recorded})
		return
	}
	if err := f.applyRemoteControl(ctx, log, ws, client, true); err != nil {
		if shimGone(err) {
			// THE DEATH IS RECORDED ONCE, by daemon.shimclient.exit, and the
			// shim that replaces it comes up through here again.
			log.Info(opRemoteControl, "the new shim went before its bridge was asked; the next bring-up puts it in force", dlog.Context{"cause": err.Error()})
			return
		}
		log.Error(opRemoteControl, "the new shim could not be put in remote control", dlog.Context{
			"remote_control_recorded": standing.Recorded, "cause": err.Error(),
		})
		f.deps.Topbar.RaiseWarning(ws, remoteControlWarning,
			fmt.Sprintf("the session started and could not be made reachable from claude.ai: %v", err))
		return
	}
	log.Info(opRemoteControl, "put the new shim in remote control", dlog.Context{"remote_control_recorded": standing.Recorded})
}

// shimclientRemoteControl is the slice of the shim client a remote-control
// change uses.
type shimclientRemoteControl interface {
	SetSessionRemoteControl(ctx context.Context, req *shimv1.SetSessionRemoteControlRequest) (*shimv1.SetSessionRemoteControlResponse, error)
}

// applyRemoteControl sends one SetSessionRemoteControl for ON, and keeps the
// bridge session an on answers. An on names the workspace as the sidebar
// draws it and re-binds the bridge session last kept. The caller holds the
// workspace's remote-control gate.
//
// THE CALL OUTLIVES ITS CALLER'S CANCEL, within RemoteControlBound: a bring-up
// may run under an rpc's context, which ends when the rpc answers, and a
// bridge cut there would be a refusal nobody caused.
func (f *Fleet) applyRemoteControl(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclientRemoteControl, on bool) error {
	req := &shimv1.SetSessionRemoteControlRequest{Enabled: on}
	if on {
		record, err := f.deps.DB.Workspace(ctx, ws)
		if err != nil {
			return fmt.Errorf("read the workspace's name for claude.ai: %w", err)
		}
		standing, err := f.deps.DB.RemoteControl(ctx, ws)
		if err != nil {
			return fmt.Errorf("read the bridge session to re-bind: %w", err)
		}
		req.Name = record.Name
		if standing.BridgeSessionID != "" {
			reattach := standing.BridgeSessionID
			req.ReattachBridgeSessionId = &reattach
		}
	}
	bounded, cancel := context.WithTimeout(context.WithoutCancel(ctx), RemoteControlBound)
	defer cancel()
	response, err := client.SetSessionRemoteControl(bounded, req)
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: remoteControlArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	switch {
	case success == nil:
		return fmt.Errorf("the shim answered SetSessionRemoteControl with neither a success nor a failure (response %v)", response)
	case success.GetEnabled() != on:
		return fmt.Errorf("the shim answered SetSessionRemoteControl success with the bridge %s, asked %s", onOff(success.GetEnabled()), onOff(on))
	case !on:
		return nil
	case success.GetBridgeSessionId() == "":
		// The contract: set when enabled. A bridge with no session is a
		// broken shim, and an id nobody stated is never kept.
		return fmt.Errorf("the shim answered SetSessionRemoteControl success with no bridge session")
	}
	// A BRIDGE SESSION THAT CANNOT BE KEPT IS NOT A FAILED BRIDGE: the session
	// is reachable now, and only the next shim lists a new claude.ai session.
	if err := f.deps.DB.SetRemoteControlBridgeSession(ctx, ws, success.GetBridgeSessionId()); err != nil {
		log.Error(opRemoteControl, "could not keep the bridge session; the next shim lists a new claude.ai session", dlog.Context{
			"bridge_session_id": success.GetBridgeSessionId(), "cause": err.Error(),
		})
		return nil
	}
	log.Debug(opRemoteControl, "kept the bridge session for the next shim to re-bind", dlog.Context{
		"bridge_session_id": success.GetBridgeSessionId(), "reattached": req.ReattachBridgeSessionId != nil,
	})
	return nil
}

// remoteControlArm names a SetSessionRemoteControl refusal's arm, spelled as
// UpdateRemoteControlModeError spells the same condition.
func remoteControlArm(failure *shimv1.SetSessionRemoteControlFailure) string {
	switch {
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return ArmShimVendorRefused
	default:
		return ArmShimUnspecified
	}
}

// shimGone reports whether a shim call failed because the shim is gone: it
// died under the call, or this daemon stood it down. Neither is the bridge's
// failure, and the death is recorded where it is observed.
func shimGone(err error) bool {
	return errors.Is(err, shimclient.ErrShimDied) || errors.Is(err, shimclient.ErrStandDownOrdered)
}

// remoteControlGate answers workspace WS's remote-control gate, minting it on
// first use.
func (f *Fleet) remoteControlGate(ws ids.WorkspaceID) *sync.Mutex {
	f.mu.Lock()
	defer f.mu.Unlock()
	gate, ok := f.remoteControlGates[ws]
	if !ok {
		gate = &sync.Mutex{}
		f.remoteControlGates[ws] = gate
	}
	return gate
}

// onOff spells a mode for a sentence.
func onOff(on bool) string {
	if on {
		return "on"
	}
	return "off"
}
