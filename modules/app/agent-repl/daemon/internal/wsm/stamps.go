package wsm

import "time"

// Stamp is one instant a workspace record says somebody was there, and which
// field said it.
type Stamp struct {
	At     time.Time
	Signal string
}

// Stamps is every instant the record says somebody was there: its creation,
// and its last activity, last selection, merge and close where set. The close
// counts: closing a workspace is the last thing somebody did with it. It is the one
// reading of a record's activity, shared by every sweep that judges idleness
// (the landed-worktree reaper, the storage sweep).
func (ws Workspace) Stamps() []Stamp {
	stamps := []Stamp{{At: ws.CreatedAt, Signal: "workspace_created"}}
	for _, stamp := range []struct {
		at     *time.Time
		signal string
	}{
		{ws.LastActivityAt, "workspace_activity"},
		{ws.LastSelectedAt, "workspace_selected"},
		{ws.MergedAt, "workspace_merged"},
	} {
		if stamp.at != nil {
			stamps = append(stamps, Stamp{At: *stamp.at, Signal: stamp.signal})
		}
	}
	if ws.Closure != nil {
		stamps = append(stamps, Stamp{At: ws.Closure.At, Signal: "workspace_closed"})
	}
	return stamps
}

// LastTouched is the newest of the record's Stamps.
func (ws Workspace) LastTouched() Stamp {
	var newest Stamp
	for _, stamp := range ws.Stamps() {
		if stamp.At.After(newest.At) {
			newest = stamp
		}
	}
	return newest
}
