package cachelapse

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE COLD GATE IS RAISED THE MOMENT THE CACHE LAPSES (owner ruling,
// 2026-11-23), not at the next prompt. The gate was the shim's judgment at a
// start, and a start comes with a prompt, so a workspace whose cache had
// lapsed looked ordinary until a prompt was sent. The shim now states, after
// every request that moves the cache, when the cache lapses if no keep-alive
// follows and what the gate will state then (SessionCacheLapseGated). The
// daemon keeps that projection per workspace (wsm projected_cold_gate), arms
// its instant, and raises the gate itself when it passes: the sidebar, the
// footer and the composer's gate dock draw it as any gate, with no shim
// started.
//
// THE PROJECTION IS REPLACED, NEVER CORRECTED: the shim restates it on every
// anchor change, a turn or a keep-alive, so a deadline that moved is simply
// armed again, and an ungated verdict clears it. A gate that was raised by a
// start's own judgment retires the projection too (edgeGateRaised).

const opGate = "daemon.cachelapse.projected_gate"

// projectGate keeps the projection STATED states for WS, or retires the one
// kept when it states none to raise: an ungated verdict, or a gated one from a
// shim build that does not state the gate's last request.
func (k *Keeper) projectGate(ctx context.Context, ws ids.WorkspaceID, stated *conversationv1.SessionCacheLapse) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	gated := stated.GetGated()
	if gated == nil || gated.GetLastRequestAtMs() == 0 {
		k.retireGateProjection(ctx, log, ws, "the shim projects no gate to raise")
		return
	}
	due := time.UnixMilli(stated.GetLapsesAtMs()).UTC()
	cold := &conversationv1.SessionCold{
		ContextTokens:   gated.GetContextTokens(),
		LastRequestAtMs: gated.GetLastRequestAtMs(),
		Reason:          &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: gated.GetCacheTtlMs()}},
	}
	if err := k.deps.DB.SetProjectedColdGate(ctx, ws, due, cold); err != nil {
		if !leaving(ctx, log, opGate, "recording the projected cold gate", err) {
			log.Error(opGate, "could not record the workspace's projected cold gate; its gate is raised at the next prompt as before",
				dlog.Context{"due": due, "context_tokens": cold.GetContextTokens(), "cause": err.Error()})
		}
		delete(k.gateDue, ws)
		return
	}
	k.gateDue[ws] = due
	log.Debug(opGate, "recorded the workspace's projected cold gate", dlog.Context{"due": due, "context_tokens": cold.GetContextTokens()})
}

// retireGateProjection retires WS's projected gate, WHY naming the reason.
func (k *Keeper) retireGateProjection(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, why string) {
	delete(k.gateDue, ws)
	cleared, err := k.deps.DB.ClearProjectedColdGate(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opGate, "retiring the projected cold gate", err) {
			log.Error(opGate, "could not retire the workspace's projected cold gate; it is read again at its instant",
				dlog.Context{"why": why, "cause": err.Error()})
		}
		return
	}
	if cleared {
		log.Debug(opGate, "retired the workspace's projected cold gate: {why}", dlog.Context{"why": why})
	}
}

// gatesDue raises the gate of every projection whose instant has come.
func (k *Keeper) gatesDue(ctx context.Context) {
	now := k.deps.Clock.Now()
	for ws, at := range k.gateDue {
		if now.Before(at) {
			continue
		}
		delete(k.gateDue, ws)
		k.raiseGate(ctx, ws, now)
	}
}

// raiseGate raises WS's projected gate at NOW, read afresh from its row so a
// projection restated meanwhile is honored.
func (k *Keeper) raiseGate(ctx context.Context, ws ids.WorkspaceID, now time.Time) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	projected, found, err := k.deps.DB.ProjectedColdGate(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opGate, "reading the projected cold gate", err) {
			log.Error(opGate, "could not read the workspace's projected cold gate; its gate is not raised until it is projected again",
				dlog.Context{"cause": err.Error()})
		}
		return
	}
	if !found {
		return
	}
	if now.Before(projected.DueAt) {
		k.gateDue[ws] = projected.DueAt
		log.Debug(opGate, "the projected cold gate moved later; it is armed again", dlog.Context{"due": projected.DueAt})
		return
	}
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) || leaving(ctx, log, opGate, "reading a workspace whose projected cold gate came due", err) {
			return
		}
		log.Error(opGate, "could not read a workspace whose projected cold gate came due; its gate is not raised until it is projected again",
			dlog.Context{"cause": err.Error()})
		return
	}
	// A HIDDEN OR CLOSED WORKSPACE'S LAPSE IS THE LAPSE'S (lapse): its session
	// is stood down, and the gate it will ask is shown with the workspace.
	if record.Closed || record.InHiddenSection() {
		log.Debug(opGate, "the workspace is hidden or closed; its cache lapse owns it, and no gate is raised", dlog.Context{"closed": record.Closed})
		return
	}
	if _, standing := k.deps.ColdGate(ws); standing {
		k.retireGateProjection(ctx, log, ws, "a cold gate already stands")
		return
	}
	raised, err := k.deps.RaiseGate(ctx, ws, projected.Cold)
	if err != nil {
		if !leaving(ctx, log, opGate, "raising the projected cold gate", err) {
			log.Error(opGate, "could not raise the workspace's projected cold gate; it is asked again shortly",
				dlog.Context{"due": projected.DueAt, "context_tokens": projected.Cold.GetContextTokens(), "retry_in": BeatRetry.String(), "cause": err.Error()})
			k.gateDue[ws] = now.Add(BeatRetry)
		}
		return
	}
	if !raised {
		log.Info(opGate, "the workspace's projected cold gate is not raised: a turn runs or no session is held; the shim states the next projection when the cache moves",
			dlog.Context{"due": projected.DueAt})
		return
	}
	log.Info(opGate, "raised the workspace's cold gate the moment its cache lapsed", dlog.Context{
		"due": projected.DueAt, "context_tokens": projected.Cold.GetContextTokens(),
	})
}
