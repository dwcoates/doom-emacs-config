package bringup

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// A SHIM COMES BACK AT A BOOT ONLY IF IT WAS UP WHEN THE LAST DAEMON STOPPED
// (owner ruling, 2026-10-28). Each session's record says so already: every
// stand-down this daemon makes on purpose writes its terminal (a keep-alive
// cycle's park, a hibernation, a kill), and a session nothing stood down has
// none. So a workspace whose session is on record with a conversation (a
// vendor session) and no terminal was up, and is brought up again; one parked
// just before the restart, one killed, and one that never started -- no
// record, or the spawn-facts record a create writes before any start -- stay
// as they are, until a prompt (or a
// keep-alive, or a cold gate's answer) brings each up. A boot no longer
// starts every shim it once served.

// SessionLister reads every session record in one read, and every recorded
// cold gate.
type SessionLister interface {
	Sessions(ctx context.Context) ([]wsm.Session, error)
	ColdGateWorkspaces(ctx context.Context) ([]wsm.WorkspaceID, error)
}

// WasUp is what the records say of one workspace's shim when the last daemon
// stopped: the facts OnlyWasUp splits by, kept so each decision can be
// recorded with them.
type WasUp struct {
	// ColdGate reports that the workspace stands at a recorded cold gate.
	ColdGate bool
	// Recorded reports that the workspace has a session record.
	Recorded bool
	// Conversation reports that its session record names a vendor session.
	Conversation bool
	// Live reports that a session record of it has a conversation and no
	// terminal: nothing stood its shim down on purpose.
	Live bool
	// Terminal is the terminal its session record carries, nil when none
	// does: why its shim was stood down (a park, a kill) when one was.
	Terminal *wsm.SessionTerminal
}

// Up reports that the records say the workspace's shim was up when the last
// daemon stopped: its session is live, or it stands at a recorded cold gate.
func (w WasUp) Up() bool { return w.ColdGate || w.Live }

// Context states the facts for a record.
func (w WasUp) Context() dlog.Context {
	ctx := dlog.Context{
		"was_up":       w.Up(),
		"cold_gate":    w.ColdGate,
		"recorded":     w.Recorded,
		"conversation": w.Conversation,
		"live":         w.Live,
	}
	if w.Terminal != nil {
		ctx["terminal"] = w.Terminal.Kind
		ctx["terminal_detail"] = w.Terminal.Detail
		ctx["terminal_at"] = w.Terminal.At.Format(time.RFC3339Nano)
	}
	return ctx
}

// WasUpFacts answers, for each of pending, what the session records and the
// recorded cold gates say of its shim when the last daemon stopped. A read
// that fails answers nothing.
func WasUpFacts(ctx context.Context, sessions SessionLister, pending []wsm.Workspace) (map[wsm.WorkspaceID]WasUp, error) {
	out := make(map[wsm.WorkspaceID]WasUp, len(pending))
	if len(pending) == 0 {
		return out, nil
	}
	all, err := sessions.Sessions(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: read the sessions to tell which shims were up: %w", err)
	}
	gated, err := sessions.ColdGateWorkspaces(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: read the recorded cold gates to tell which workspaces stand at one: %w", err)
	}
	for _, ws := range pending {
		out[ws.ID] = WasUp{}
	}
	for _, id := range gated {
		if f, ok := out[id]; ok {
			f.ColdGate = true
			out[id] = f
		}
	}
	for _, s := range all {
		f, ok := out[s.Workspace]
		if !ok {
			continue
		}
		f.Recorded = true
		f.Conversation = s.VendorSessionID != ""
		f.Terminal = s.Terminal
		f.Live = f.Live || s.Terminal == nil && s.VendorSessionID != ""
		out[s.Workspace] = f
	}
	return out, nil
}

// OnlyWasUp splits pending into the workspaces whose shim was up when the last
// daemon stopped and the ones left as they are (parked, killed, never
// started), each in pending's order. A workspace standing at a recorded cold
// gate is up too: it runs no shim, and its start only raises the gate from the
// record, so the gate shows the moment the daemon serves and not at the next
// prompt. A session read that fails splits nothing:
// waking a parked shim or leaving a live one down are both wrong, so the
// caller states the failure.
func OnlyWasUp(ctx context.Context, sessions SessionLister, pending []wsm.Workspace) (up, left []wsm.Workspace, err error) {
	if len(pending) == 0 {
		return nil, nil, nil
	}
	facts, err := WasUpFacts(ctx, sessions, pending)
	if err != nil {
		return nil, nil, err
	}
	for _, ws := range pending {
		if facts[ws.ID].Up() {
			up = append(up, ws)
			continue
		}
		left = append(left, ws)
	}
	return up, left, nil
}
