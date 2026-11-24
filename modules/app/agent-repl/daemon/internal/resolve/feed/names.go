package feed

import (
	"fmt"

	"agent-repld/internal/ids"
)

// A ROW THAT NAMES ANOTHER WORKSPACE DRAWS A NAME READ BEFORE THE LOCK
// (storework.go). The registry holds the names, in the state database, so
// they are read off the resolver's mutex wherever a workspace to be named is
// learned, and only looked up under it:
//
//   - every workspace primed (prime.go), until a read answers its name: a
//     read that failed is read again by the next prime;
//   - the senders a replayed page's turns recorded, and the workspace a
//     fork's inherited generation came from, before the replay takes the
//     lock (history.go, book.go);
//   - the sender an entry point names (SenderTurn, the held prompts);
//   - the workspaces the store answers a cross-session message's ends with,
//     before its answer is drawn (peerdelivery.go).
//
// A rename pushes the new name (SetWorkspaceName, from the workspace verbs'
// publishRename), so a name looked up is the registry's.

// nameRead is one workspace's display name as the registry answered it.
type nameRead struct {
	name string
	err  error
}

// readNames reads each of WORKSPACES' display names from the registry into
// the name table, fresh. Called with r.mu NOT held.
func (r *resolver) readNames(workspaces ...ids.WorkspaceID) {
	if r.deps.WorkspaceName == nil {
		return
	}
	for _, ws := range workspaces {
		if ws == "" {
			continue
		}
		name, err := r.deps.WorkspaceName(ws)
		r.names.Store(ws, nameRead{name: name, err: err})
	}
}

// ensureName reads WS's name unless the table holds one a read answered.
// Called with r.mu NOT held.
func (r *resolver) ensureName(ws ids.WorkspaceID) {
	if loaded, ok := r.names.Load(ws); ok && loaded.(nameRead).err == nil {
		return
	}
	r.readNames(ws)
}

// SetWorkspaceName implements Resolver: a rename's new NAME for WS, recorded
// in the name table every later row naming WS draws from.
func (r *resolver) SetWorkspaceName(ws ids.WorkspaceID, name string) {
	r.names.Store(ws, nameRead{name: name})
}

// knownName is WS's name as the table holds it, and the error its read met.
// A workspace whose name was never read panics: a row was about to draw it
// from a read made under the lock (the readers above).
func (r *resolver) knownName(ws ids.WorkspaceID) (string, error) {
	loaded, ok := r.names.Load(ws)
	if !ok {
		panic(fmt.Sprintf("feed: workspace %s was named in a row without its name having been read; every place a workspace to be named is learned reads its name first (names.go)", ws))
	}
	read := loaded.(nameRead)
	return read.name, read.err
}
