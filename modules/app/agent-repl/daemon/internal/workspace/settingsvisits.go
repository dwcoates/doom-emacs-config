package workspace

import (
	"sync"
	"time"

	"agent-repld/internal/ids"
)

// A SETTINGS PICK'S SHIM IS A VISIT (owner request, 2026-10-29). A model,
// permission-mode or effort pick on a workspace with no shim starts one (the
// queue's settings revival), and the switch away from that workspace stands it
// down again unless a turn has run since the pick (drain.VisitDown). The
// queue notes each such start here (promptqueue.Deps.SettingsStarted), and
// every switch takes the visits it left behind (selectCurrent).

// SettingsVisits is the workspaces whose shim a settings pick started, each
// with the instant it did.
type SettingsVisits struct {
	mu     sync.Mutex
	visits map[ids.WorkspaceID]time.Time
}

// NewSettingsVisits builds an empty set.
func NewSettingsVisits() *SettingsVisits {
	return &SettingsVisits{visits: map[ids.WorkspaceID]time.Time{}}
}

// Note records that a settings pick started WS's shim at AT. A later start
// replaces the earlier instant.
func (s *SettingsVisits) Note(ws ids.WorkspaceID, at time.Time) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.visits[ws] = at
}

// LeftFor takes every visit other than CURRENT's, the workspace now in view:
// the ones the switch to it left behind.
func (s *SettingsVisits) LeftFor(current ids.WorkspaceID) map[ids.WorkspaceID]time.Time {
	s.mu.Lock()
	defer s.mu.Unlock()
	left := map[ids.WorkspaceID]time.Time{}
	for ws, at := range s.visits {
		if ws != current {
			left[ws] = at
			delete(s.visits, ws)
		}
	}
	return left
}
