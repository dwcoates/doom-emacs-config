package workspace

import (
	"context"
	"errors"
	"fmt"
	"io"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/shimlife"
)

// A PARKED WORKSPACE IN REMOTE CONTROL STAYS REACHABLE FROM claude.ai (owner
// ruling, 2026-11-02, overturning the 2026-10-27 "a gate runs no shim" ruling
// for this case only). The Remote Control bridge is the vendor CLI's own
// connection, so a gate that runs no shim left a prompt typed on the phone
// reaching nothing on this machine. A workspace whose remote-control mode is
// on therefore keeps, while its gate stands unanswered, ONE shim holding the
// parked conversation's bridge on an idle vendor process
// (shim.v1.HoldParkedBridge); every other gated workspace runs no shim, as
// before.
//
// A REMOTE PROMPT ANSWERS THE GATE PAY, through the one path the editor's
// answer takes. The vendor runs the prompt the moment it arrives, so the cold
// read is spent then; the shim states the prompt on the bridge's stream, and
// the fleet answers the gate pay with agentrepl.v1.AnswerColdGate
// (FleetDeps.PayColdGate). The answer retires the gate, settles the prompts
// held at it and marks the cache, exactly as a click does, and its resume
// PROMOTES the bridge: the shim answers the pay StartSession with the session
// the bridge already opened, whose turn in flight is the remote prompt's.
//
// ONE CHOKE POINT. Every place that used to stand a gate's shim down -- a
// start the shim refused cold, a failed answer's gate raised again, a gate
// carried over a handover -- settles the gate's shim here instead
// (settleGateShim), so no path can park a remote-control workspace
// unreachable. A gate raised from its record with no shim behind it is bridged
// through the same function.

const opParkedBridge = "daemon.workspace.parked_bridge"

// isPay reports whether REMEDIATION is the gate's pay answer.
func isPay(remediation *conversationv1.SessionColdRemediation) bool {
	return remediation.GetPay() != nil
}

// remediationName names REMEDIATION for a record.
func remediationName(remediation *conversationv1.SessionColdRemediation) string {
	switch remediation.GetRemediation().(type) {
	case *conversationv1.SessionColdRemediation_Pay:
		return "pay"
	case *conversationv1.SessionColdRemediation_Clear:
		return "clear"
	case *conversationv1.SessionColdRemediation_Compact:
		return "compact"
	default:
		return "none"
	}
}

// payColdGateAnswer is the gate's pay answer, as the editor's button sends it.
func payColdGateAnswer() *agentreplv1.AnswerColdGateRequest {
	return &agentreplv1.AnswerColdGateRequest{Choice: &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}}}
}

// settleGateShim decides what a standing, unanswered cold gate keeps of its
// shim: none when the workspace's remote control is off (standDownAtGate, run
// here, so a caller under the start gate stands the shim down before it
// returns), and one holding the parked conversation's bridge when it is on.
func (f *Fleet) settleGateShim(ctx context.Context, ws ids.WorkspaceID) {
	f.gateShim(ctx, ws, f.standDownAtGate)
}

// bridgeGateInRemoteControl bridges WS's standing gate when its remote
// control is on, and leaves its shim as it is otherwise: it is for the paths
// that keep a gate's shim held (a relaunch the shim answered cold, a carried
// gate raised over an adopted shim).
func (f *Fleet) bridgeGateInRemoteControl(ctx context.Context, ws ids.WorkspaceID) {
	f.gateShim(ctx, ws, func(context.Context, ids.WorkspaceID) {})
}

// gateShim reads WS's remote-control mode and either runs OFF or holds the
// gate's bridge (holdBridgeAtGate, run detached: it takes the start gate
// itself and watches the bridge for as long as it stands). A mode that cannot
// be read is stated loudly and treated as off.
func (f *Fleet) gateShim(ctx context.Context, ws ids.WorkspaceID, off func(context.Context, ids.WorkspaceID)) {
	log := f.logFor(ctx, ws)
	standing, err := f.deps.DB.RemoteControl(ctx, ws)
	if err != nil {
		log.Error(opParkedBridge, "could not read the parked workspace's remote-control mode; its gate holds no bridge, and claude.ai cannot reach it", dlog.Context{
			"cause": err.Error(),
		})
		f.deps.Topbar.RaiseWarning(ws, remoteControlWarning,
			fmt.Sprintf("could not read whether this parked session should be reachable from claude.ai: %v", err))
		off(ctx, ws)
		return
	}
	if !standing.On {
		off(ctx, ws)
		return
	}
	f.Detach(func(ctx context.Context) { f.holdBridgeAtGate(ctx, f.logFor(ctx, ws), ws) })
}

// holdBridgeAtGate gives WS's standing gate a shim holding its conversation's
// bridge: the shim the gate left held, else one spawned for it. The shim is
// marked bridged BEFORE the bridge is asked for, so an answer racing the
// bridge's opening already knows which shim takes only a pay. The watch runs
// on this goroutine until the bridge's stream ends.
func (f *Fleet) holdBridgeAtGate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	held, req, vendorSessionID, ok := f.bridgedShimAtGate(ctx, log, ws)
	if !ok {
		return
	}
	stream, err := held.client.HoldParkedBridge(ctx, req)
	if err != nil {
		if shimGone(err) || standingDown(ctx, err) {
			log.Info(opParkedBridge, "the parked bridge's shim went before its bridge opened; the gate stands with no bridge", dlog.Context{"cause": err.Error()})
			return
		}
		f.parkedBridgeFailed(ctx, log, ws, "the parked bridge could not be asked of its shim", err.Error())
		return
	}
	f.watchParkedBridge(ctx, log, ws, vendorSessionID, held.client, stream)
}

// bridgedShimAtGate takes, under the workspace's start gate, the shim WS's
// unanswered gate is bridged through, marks it bridged, and builds its
// request. It answers false, having recorded why, when there is nothing to
// bridge or the shim could not be had.
func (f *Fleet) bridgedShimAtGate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (*live, *shimv1.HoldParkedBridgeRequest, string, bool) {
	gate := f.startGate(ws)
	gate.Lock()
	defer gate.Unlock()
	f.mu.RLock()
	standing, gated := f.coldGates[ws]
	f.mu.RUnlock()
	if !gated || standing.answering {
		log.Debug(opParkedBridge, "no unanswered cold gate stands; nothing is bridged", dlog.Context{"gated": gated})
		return nil, nil, "", false
	}
	vendorSessionID := standing.served.VendorSessionID
	if held, ok := f.idleHeldForAnswer(ws); ok && held.bridged {
		log.Debug(opParkedBridge, "the gate's shim already holds its bridge", dlog.Context{"shim_pid": held.client.PID()})
		return nil, nil, "", false
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		f.parkedBridgeFailed(ctx, log, ws, "could not read the parked workspace's record", err.Error())
		return nil, nil, "", false
	}
	previous, _, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		f.parkedBridgeFailed(ctx, log, ws, "could not read the parked workspace's session record", err.Error())
		return nil, nil, "", false
	}
	src := source{VendorSessionID: vendorSessionID}.withRecord(record)
	held, ok := f.idleHeldForAnswer(ws)
	if !ok {
		log.Info(opParkedBridge, "the parked workspace is in remote control and its gate has no shim; spawning the one that holds its bridge", dlog.Context{
			"vendor_session_id": vendorSessionID,
		})
		held, err = f.spawnAtGate(ctx, log, ws, record, previous, src, "HoldParkedBridge")
		if err != nil {
			f.parkedBridgeFailed(ctx, log, ws, "could not spawn the shim that holds the parked workspace's bridge", err.Error())
			return nil, nil, "", false
		}
	}
	start, err := f.startSessionRequest(ctx, log, ws, src, previous, held.configDir)
	if err != nil {
		f.parkedBridgeFailed(ctx, log, ws, "could not build the parked conversation's resume", err.Error())
		return nil, nil, "", false
	}
	bridge, err := f.remoteControlRequest(ctx, ws, true)
	if err != nil {
		f.parkedBridgeFailed(ctx, log, ws, "could not build the parked workspace's bridge request", err.Error())
		return nil, nil, "", false
	}
	if !f.markBridged(ws, held.client) {
		log.Info(opParkedBridge, "the gate's shim was taken before its bridge was asked; nothing is bridged", dlog.Context{"shim_pid": held.client.PID()})
		return nil, nil, "", false
	}
	return held, &shimv1.HoldParkedBridgeRequest{Start: start, RemoteControl: bridge}, vendorSessionID, true
}

// markBridged marks WS's held entry for CLIENT bridged, answering false when
// the entry no longer holds CLIENT.
func (f *Fleet) markBridged(ws ids.WorkspaceID, client shimclient.Client) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	entry, ok := f.sessions[ws]
	if !ok || entry.client != client {
		return false
	}
	entry.bridged = true
	return true
}

// watchParkedBridge reads the parked bridge's stream to its end: it keeps the
// bridge's claude.ai session, answers the gate pay for a remote prompt, and
// stands the shim down when the bridge cannot be held.
func (f *Fleet) watchParkedBridge(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID string, client shimclient.Client, stream shimclient.Stream[*shimv1.HoldParkedBridgeResponse]) {
	defer stream.Close()
	log = log.With(dlog.Context{"vendor_session_id": vendorSessionID})
	for {
		frame, err := stream.Recv()
		if err != nil {
			f.parkedBridgeEnded(ctx, log, ws, client, err)
			return
		}
		switch event := frame.GetEvent().(type) {
		case *shimv1.HoldParkedBridgeResponse_Opened:
			f.parkedBridgeOpened(ctx, log, ws, event.Opened)
		case *shimv1.HoldParkedBridgeResponse_Refused:
			f.parkedBridgeFailed(ctx, log, ws, "the shim could not hold the parked workspace's bridge",
				fmt.Sprintf("%s: %s", refusedBridgeArm(event.Refused), event.Refused.GetDetail()))
			return
		case *shimv1.HoldParkedBridgeResponse_RemotePrompt:
			f.payForRemotePrompt(ctx, log, ws, event.RemotePrompt)
		default:
			// shimclient validates every frame's event set, and the arms above
			// are the contract's every arm.
			log.Error(opParkedBridge, "the parked bridge's stream carried an event no arm reads", dlog.Context{
				"invariant_violation": "HoldParkedBridgeResponse.event names an arm the fleet does not read",
				"frame":               frame.String(),
			})
			return
		}
	}
}

// parkedBridgeOpened keeps the bridge's claude.ai session for the next
// shim to re-bind, and records the bridge open.
func (f *Fleet) parkedBridgeOpened(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, opened *shimv1.HoldParkedBridgeOpened) {
	fields := dlog.Context{"bridge_session_id": opened.GetBridgeSessionId(), "already_open": opened.GetAlreadyOpen()}
	if err := f.deps.DB.SetRemoteControlBridgeSession(ctx, ws, opened.GetBridgeSessionId()); err != nil {
		log.Error(opParkedBridge, "could not keep the parked bridge's session; the next shim lists a new claude.ai session", dlog.WithCause(fields, err))
	}
	log.Info(opParkedBridge, "the parked workspace is reachable from claude.ai; a prompt typed there answers its cold gate pay", fields)
}

// payForRemotePrompt answers WS's gate pay for the prompt typed on claude.ai
// that reached its parked bridge, through the verb the editor's answer takes.
func (f *Fleet) payForRemotePrompt(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, prompt *shimv1.HoldParkedBridgeRemotePrompt) {
	f.mu.RLock()
	detail := f.coldGates[ws].served.Detail
	f.mu.RUnlock()
	fields := dlog.Context{
		"client_uuid": prompt.GetClientUuid(),
		"prompt":      holdfold.SaidText(prompt.GetSaid()),
		"gate":        detail,
		"choice":      "pay",
	}
	log.Info(opParkedBridge, "a prompt typed on claude.ai reached the parked workspace; its cold gate is answered pay", fields)
	if err := f.deps.PayColdGate(ctx, ws, payColdGateAnswer()); err != nil {
		log.Error(opParkedBridge, "the cold gate could not be answered pay for the prompt typed on claude.ai", dlog.WithCause(fields, err))
	}
}

// parkedBridgeEnded records how the bridge's stream ended. Its end after the
// pay answer promoted it, its shim's stand-down and this daemon's own are
// answers; any other break is the bridge's failure.
func (f *Fleet) parkedBridgeEnded(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, err error) {
	switch {
	case errors.Is(err, io.EOF):
		log.Info(opParkedBridge, "the parked bridge's stream ended: the cold gate's pay answer took its session", nil)
	case standingDown(ctx, err) || shimGone(err) || client.StandingDown():
		log.Info(opParkedBridge, "the parked bridge's stream ended with its shim", dlog.Context{"cause": err.Error()})
	default:
		f.parkedBridgeFailed(ctx, log, ws, "the parked bridge's stream broke", err.Error())
	}
}

// standingDown reports whether ERR is this daemon's own context ending under
// the call: judged off the error (ctxend.Is), so a break that merely coincides
// with the stand-down is still the break it is.
func standingDown(ctx context.Context, err error) bool {
	return ctx.Err() != nil && ctxend.Is(err)
}

// parkedBridgeFailed states a bridge that cannot be held: at ERROR and on the
// warning strip, the reader's one error surface. The gate keeps no shim.
func (f *Fleet) parkedBridgeFailed(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, what, cause string) {
	log.Error(opParkedBridge, what+"; claude.ai cannot reach the parked workspace", dlog.Context{"cause": cause})
	f.deps.Topbar.RaiseWarning(ws, remoteControlWarning, fmt.Sprintf("this parked session cannot be reached from claude.ai: %s: %s", what, cause))
	f.standDownAtGate(ctx, ws)
}

// standDownBridge stands down the bridged shim HELD at WS's gate, for an
// answer that runs on a new shim. The caller holds the workspace's start
// gate.
func (f *Fleet) standDownBridge(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, held *live, why string) error {
	f.mu.Lock()
	entry, ok := f.sessions[ws]
	if ok && entry.client == held.client {
		delete(f.sessions, ws)
		f.movedLocked()
	}
	f.mu.Unlock()
	if !ok || entry.client != held.client {
		return ErrShimTaken
	}
	if err := f.teardown(shimlife.WithStandDown(ctx, shimlife.StandDownColdGate), ws, entry, false, why); err != nil {
		log.Error(opParkedBridge, "the parked bridge's shim could not be stood down for the answer", dlog.Context{"why": why, "cause": err.Error()})
		return fmt.Errorf("stand the parked bridge's shim down: %w", err)
	}
	log.Info(opParkedBridge, "stood the parked bridge's shim down: "+why, nil)
	return nil
}

// refusedBridgeArm names a HoldParkedBridge refusal's arm.
func refusedBridgeArm(refused *shimv1.HoldParkedBridgeRefused) string {
	switch refused.GetCause().(type) {
	case *shimv1.HoldParkedBridgeRefused_ResumeFailed:
		return "resume_failed"
	case *shimv1.HoldParkedBridgeRefused_VendorRefused:
		return "vendor_refused"
	case *shimv1.HoldParkedBridgeRefused_AlreadyStarted:
		return "already_started"
	default:
		return "unspecified"
	}
}
