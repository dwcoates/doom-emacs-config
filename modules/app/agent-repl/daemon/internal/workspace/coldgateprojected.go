package workspace

import (
	"context"
	"fmt"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A GATE RAISED THE MOMENT THE CACHE LAPSES (owner ruling, 2026-11-23). The
// cache keeper holds the cold gate each shim projected, and raises it here
// when the cache's instant passes (cachelapse.Keeper), so the sidebar, the
// footer and the composer's gate dock show the gate the moment it stands and
// not at the next prompt. It is the same gate a start's judgment raises
// (askToStartSession): recorded, published, and followed by the park of the
// session it stands over (cachelapse.Keeper.ColdGateRaised).

// RaiseProjectedGate raises WS's cold gate from PROJECTED, the facts its shim
// projected, answering whether it did. It raises nothing, and answers false,
// while a gate already stands, a turn or live detached work runs on the
// session (the shim restates the projection when the cache moves), no
// conversation is recorded to resume, or the projected read is the one the
// user already paid for (paidColdRead). It runs under the workspace's start
// gate, so no start or answer interleaves with the raise.
func (f *Fleet) RaiseProjectedGate(ctx context.Context, ws ids.WorkspaceID, projected *conversationv1.SessionCold) (bool, error) {
	log := f.logFor(ctx, ws)
	gate := f.startGate(ws)
	gate.Lock()
	defer gate.Unlock()

	f.mu.RLock()
	_, standing := f.coldGates[ws]
	f.mu.RUnlock()
	if standing {
		return false, nil
	}
	if running, _ := f.Running(ws); running.Turn != nil || !running.LiveWork.Empty() {
		return false, nil
	}
	session, found, err := f.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opColdGate, "could not read the session a projected cold gate stands over; the gate is not raised",
			dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("raise the projected cold gate of %q: read the session: %w", ws, err)
	}
	if !found || session.VendorSessionID == "" {
		return false, nil
	}
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(opColdGate, "could not read the workspace a projected cold gate stands over; the gate is not raised",
			dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("raise the projected cold gate of %q: read the workspace: %w", ws, err)
	}
	cold := proto.Clone(projected).(*conversationv1.SessionCold)
	cold.RequestedModel = &conversationv1.AgentModel{Name: session.Model}
	// A READ THE USER ALREADY PAID FOR IS NOT ASKED AGAIN (wsm.ColdReadPayment).
	// Paying sends no request, so the projection the pay answer's session
	// restates names the same lapsed read, already due: raised, the gate the
	// user just answered stood again a second later (2026-11-23 22:41:58,
	// merge-queue-stall-triage, twice in a row).
	paid, err := f.paidColdRead(ctx, log, ws, session.VendorSessionID, cold)
	if err != nil {
		return false, fmt.Errorf("raise the projected cold gate of %q: %w", ws, err)
	}
	if paid {
		log.Info(opColdGate, "the projected cold read is the one the user already paid for; no gate is raised",
			coldReadFields(wsm.ColdReadPayment{VendorSessionID: session.VendorSessionID, LastRequestAtMs: cold.GetLastRequestAtMs(), Model: session.Model}))
		return false, nil
	}
	configDir := accountRootFor(f.deps.Accounts, record.Dir, session)
	f.raiseColdGate(ws, session.VendorSessionID, cold, configDir, RaiseJudged)
	f.recordGate(ctx, log, ws, wsm.ColdGateRecord{VendorSessionID: session.VendorSessionID, Cold: cold, ConfigDir: configDir})
	log.Info(opColdGate, "raised the workspace's cold gate from its shim's projection; no session is started until it is answered",
		dlog.Context{"vendor_session_id": session.VendorSessionID, "context_tokens": cold.GetContextTokens()})
	// WHAT THE GATE KEEPS OF ITS SHIM IS SETTLED AS AT EVERY RAISE
	// (parkedbridge.go): an idle shim is stood down, or holds the bridge in
	// remote control, as does a workspace with no shim at all. A session
	// still running is left to its park (cachelapse.Keeper.ColdGateRaised),
	// which bridges the gate once the conversation is free.
	f.settleGateShim(ctx, ws)
	return true, nil
}
