package cachelapse

import "time"

// THE PARK CARRIES ITS NEXT KEEP-ALIVE (owner ruling, 2026-11-09). There is
// one park, whatever stood a session down, and what tells one parked session
// from another is only whether a keep-alive is scheduled for it: a minute
// before it, this Keeper restores the session. The armed beats are the one
// authority on that, so this Keeper states them to the views, and a view's
// park reads its instant from what was stated (wsm.Park).
//
// STATED FOR EVERY WORKSPACE, PARKED OR NOT. A park is raised by the
// stand-down (drain) on its own goroutine; were the instant stated only to a
// standing park, one raised after the statement would carry none. Stated
// always, a park raised at any moment carries the current instant.

// stateNextKeepalives states to the views every workspace's next keep-alive
// that changed since it was last stated: a beat armed or moved states its
// instant, and one disarmed states none (the zero time). It runs at the end
// of every pass of Run, so every change a pass made is stated once.
func (k *Keeper) stateNextKeepalives() {
	for ws, at := range k.beats {
		if stated, ok := k.stated[ws]; ok && stated.Equal(at) {
			continue
		}
		k.stated[ws] = at
		k.deps.NextKeepalive(ws, at)
	}
	for ws := range k.stated {
		if _, armed := k.beats[ws]; armed {
			continue
		}
		delete(k.stated, ws)
		k.deps.NextKeepalive(ws, time.Time{})
	}
}
