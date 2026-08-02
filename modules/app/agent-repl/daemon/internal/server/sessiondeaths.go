package server

import (
	"fmt"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// This file exposes ONE registry fact the merge pipeline had no way to see: the
// workspace's newest session is terminal BECAUSE THE USER DELETED IT.
//
// merge.SessionBringUp answers "get me a live session", and for a HIBERNATED
// workspace that is the whole answer — hibernation is a deliberate reclamation
// of memory and the record is rehydratable, so a merge brings the session back
// and proceeds. A DELETED session is the opposite fact wearing the same
// clothes: there is no live session either way, but bringing one up would
// resurrect precisely what the user destroyed.
//
// The registry has always known the difference (Record.Terminal together with
// Record.DeathReason == errclass.DeathReasonDeleted). Nothing exposed it to the
// merge path, so a merge of a deleted workspace either silently span up a new
// session or failed with a cause that said only "no live shim" — neither of
// which a user can act on. SessionDeaths is that exposure.

// SessionDeaths reports whether a workspace's NEWEST session record is terminal
// by deletion.
//
// THE NEWEST RECORD IS THE ANSWER, not "any deleted record ever". A workspace
// whose session was deleted and then re-created has a deleted record in its
// history forever, and refusing to merge it would make one deletion permanent.
type SessionDeaths interface {
	// DeletedSession names the deleted session and reports the deletion. It
	// returns ("", false, nil) for a workspace whose newest session is alive,
	// hibernated, or terminal for any OTHER reason: only a deliberate deletion
	// is this port's subject.
	DeletedSession(workspace string) (sessionID string, deleted bool, err error)
}

// RegistrySessionDeaths is the production SessionDeaths, reading the same
// registry every other session fact is resolved from.
type RegistrySessionDeaths struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
}

var _ SessionDeaths = RegistrySessionDeaths{}

// DeletedSession implements SessionDeaths.
//
// A workspace with NO record at all is not a deletion: it is a workspace that
// never had a session, which the bring-up path handles by creating one. Saying
// "deleted" there would refuse a merge of a workspace the user never touched.
func (d RegistrySessionDeaths) DeletedSession(workspace string) (string, bool, error) {
	if d.Reg == nil {
		// A merge decides whether to bring a session up on this answer, and
		// "there is no registry" is not "the session is fine". Reporting the
		// benign answer would resurrect a session the user deleted.
		return "", false, fmt.Errorf("server: no session registry is wired, so the deletion state of workspace %q cannot be read", workspace)
	}
	if workspace == "" {
		return "", false, fmt.Errorf("server: a session-deletion lookup needs a workspace")
	}
	var (
		newest registry.Record
		at     time.Time
		found  bool
	)
	for _, rec := range d.Reg.All() {
		if rec.CWD != workspace {
			continue
		}
		created, err := time.Parse(time.RFC3339, rec.CreatedAt)
		if err != nil {
			created = time.Time{}
		}
		if !found || created.After(at) {
			newest, at, found = rec, created, true
		}
	}
	if !found || !newest.Terminal || newest.DeathReason != errclass.DeathReasonDeleted {
		return "", false, nil
	}
	return newest.SessionID, true, nil
}
