package bringup

import (
	"context"
	"fmt"

	"agent-repld/internal/wsm"
)

// A SHIM COMES BACK AT A BOOT ONLY IF IT WAS UP WHEN THE LAST DAEMON STOPPED
// (owner ruling, 2026-10-28). Each session's record says so already: every
// stand-down this daemon makes on purpose writes its terminal (a keep-alive
// cycle's park, a hibernation, a kill), and a session nothing stood down has
// none. So a workspace whose session is on record with no terminal was up,
// and is brought up again; one parked just before the restart, one killed,
// and one that never started stay as they are, until a prompt (or a
// keep-alive, or a cold gate's answer) brings each up. A boot no longer
// starts every shim it once served.

// SessionLister reads every session record in one read.
type SessionLister interface {
	Sessions(ctx context.Context) ([]wsm.Session, error)
}

// OnlyWasUp splits pending into the workspaces whose shim was up when the last
// daemon stopped and the ones left as they are (parked, killed, never
// started), each in pending's order. A session read that fails splits nothing:
// waking a parked shim or leaving a live one down are both wrong, so the
// caller states the failure.
func OnlyWasUp(ctx context.Context, sessions SessionLister, pending []wsm.Workspace) (up, left []wsm.Workspace, err error) {
	if len(pending) == 0 {
		return nil, nil, nil
	}
	all, err := sessions.Sessions(ctx)
	if err != nil {
		return nil, nil, fmt.Errorf("bringup: read the sessions to tell which shims were up: %w", err)
	}
	wasUp := make(map[wsm.WorkspaceID]bool, len(all))
	for _, s := range all {
		wasUp[s.Workspace] = s.Terminal == nil
	}
	for _, ws := range pending {
		if wasUp[ws.ID] {
			up = append(up, ws)
			continue
		}
		left = append(left, ws)
	}
	return up, left, nil
}
