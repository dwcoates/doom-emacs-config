package feed

import (
	"context"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/pullrefs"
	"agent-repld/internal/wsm"
)

// A WORKSPACE IS PRIMED BEFORE THE MUTEX IS TAKEN FOR IT (storework.go). The
// facts the feed loads once per workspace, and per process, are read here,
// off the resolver's mutex: its log sink (through the registry), its durable
// rows, its three answered turn sets, and the pull requests its text named.
// Under the mutex they are only installed: logger, state and knownPulls take
// them from the primed entry, and a workspace first needed under the mutex
// without one is an invariant violation that panics, naming it.
//
// EVERY ENTRY POINT NAMING A WORKSPACE PRIMES IT FIRST (pinned by
// TestEveryEntryPointPrimesItsWorkspace), and one naming a second workspace
// it draws in primes that one too.

// primedWorkspace is one workspace's facts as prime read them.
type primedWorkspace struct {
	// once loads the facts read only once per process; a prime that finds
	// another goroutine reading them waits for it.
	once       sync.Once
	durable    []wsm.DurableFeedRow
	durableErr error
	turnSets   map[string]primedTurnSet
	pulls      []pullrefs.Ref
	pullsErr   error
	pullsRead  time.Duration

	// mu guards the logger, which is resolved again by every prime until it
	// resolves for good (a cancelled registry read is retried).
	mu        sync.Mutex
	log       dlog.Logger
	logCached bool
}

// primedTurnSet is one answered turn set as prime read it.
type primedTurnSet struct {
	turns []ids.TurnID
	err   error
}

// The answered turn sets' names in primedWorkspace.turnSets.
const (
	turnSetRolledBack  = "rolled_back"
	turnSetSignedIn    = "signed_in"
	turnSetReconnected = "reconnected"
)

// prime reads every one of WORKSPACES' once-per-process facts not read yet,
// off the resolver's mutex. It is called with the mutex NOT held (the feed
// test harness's store guard fails a prime made under it).
func (r *resolver) prime(workspaces ...ids.WorkspaceID) {
	for _, ws := range workspaces {
		if ws == "" {
			continue
		}
		entry := r.primedEntry(ws)
		entry.once.Do(func() { r.readPrimed(ws, entry) })
		r.primeLogger(ws, entry)
		// ITS OWN NAME, which a row in another workspace draws (names.go),
		// read until a read answers it.
		r.ensureName(ws)
	}
}

// primedEntry answers WS's primed entry, creating it empty.
func (r *resolver) primedEntry(ws ids.WorkspaceID) *primedWorkspace {
	entry, _ := r.primes.LoadOrStore(ws, &primedWorkspace{})
	return entry.(*primedWorkspace)
}

// primedFor answers WS's primed entry for a use under the mutex. A workspace
// needed unprimed panics: its facts would otherwise be read from the state
// database under the mutex every workspace's feed shares.
func (r *resolver) primedFor(ws ids.WorkspaceID) *primedWorkspace {
	if loaded, ok := r.primes.Load(ws); ok {
		entry := loaded.(*primedWorkspace)
		entry.mu.Lock()
		resolved := entry.log != nil
		entry.mu.Unlock()
		if resolved {
			return entry
		}
	}
	panic(fmt.Sprintf("feed: workspace %s was needed by the resolver without being primed; every entry point naming a workspace primes it first (prime.go)", ws))
}

// readPrimed reads WS's once-per-process facts into ENTRY.
func (r *resolver) readPrimed(ws ids.WorkspaceID, entry *primedWorkspace) {
	ctx := context.Background()
	if r.deps.DurableRows != nil {
		entry.durable, entry.durableErr = r.deps.DurableRows.DurableFeedRows(ctx, ws)
	}
	entry.turnSets = map[string]primedTurnSet{}
	for name, store := range map[string]func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error){
		turnSetRolledBack:  r.turnSetLoad(turnSetRolledBack),
		turnSetSignedIn:    r.turnSetLoad(turnSetSignedIn),
		turnSetReconnected: r.turnSetLoad(turnSetReconnected),
	} {
		if store == nil {
			continue
		}
		turns, err := store(ctx, ws)
		entry.turnSets[name] = primedTurnSet{turns: turns, err: err}
	}
	if r.deps.PullReferences != nil {
		asked := r.deps.Now()
		entry.pulls, entry.pullsErr = r.deps.PullReferences.PullReferences(ctx, ws)
		entry.pullsRead = r.deps.Now().Sub(asked)
	}
}

// turnSetLoad answers the load of the turn set NAME, nil when its store is
// not wired.
func (r *resolver) turnSetLoad(name string) func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error) {
	switch name {
	case turnSetRolledBack:
		if r.deps.RolledBack != nil {
			return r.deps.RolledBack.RolledBackTurns
		}
	case turnSetSignedIn:
		if r.deps.SignedIn != nil {
			return r.deps.SignedIn.SignedInTurns
		}
	case turnSetReconnected:
		if r.deps.Reconnected != nil {
			return r.deps.Reconnected.ReconnectedTurns
		}
	}
	return nil
}

// primeLogger resolves WS's durable logger into ENTRY unless it resolved for
// good already. Failing to resolve the workspace is an INVARIANT VIOLATION,
// recorded once at ERROR against the global surface — the record is about
// the resolution failing, not about the workspace, which is why it is the one
// thing that may be global.
func (r *resolver) primeLogger(ws ids.WorkspaceID, entry *primedWorkspace) {
	entry.mu.Lock()
	cached := entry.logCached
	entry.mu.Unlock()
	if cached {
		return
	}
	log, keep := r.resolveLogger(ws)
	entry.mu.Lock()
	defer entry.mu.Unlock()
	if entry.logCached {
		return
	}
	entry.log, entry.logCached = log, keep
}

// resolveLogger reads WS's log sink through the registry and opens it,
// answering whether the answer is kept for the process's life.
func (r *resolver) resolveLogger(ws ids.WorkspaceID) (dlog.Logger, bool) {
	global := r.deps.Log.Global()
	dir, err := r.deps.WorkspaceDir(ws)
	if err != nil {
		if ctxend.Is(err) {
			// THE DAEMON IS GOING AWAY, and the workspace lookup runs under
			// the daemon's own context. A cancelled read is that shutdown, not
			// an unresolvable workspace: it is recorded at INFO, and the global
			// fallback is NOT kept, so a resolver that outlives the
			// cancellation still routes the workspace's records to its own
			// sink, resolved again by the next prime, rather than to the
			// global one forever.
			global.Info("daemon.feed.workspace_lookup_cancelled",
				"a workspace's log sink was not resolved; the daemon's context was cancelled",
				dlog.Context{"workspace": string(ws), "cause": err.Error()})
			return global, false
		}
		global.Error("daemon.feed.workspace_unresolved",
			"the feed resolver could not resolve a workspace's log sink; its records are unroutable",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return global, true
	}
	// A WORKSPACE WHOSE DIRECTORY IS GONE IS ORDINARY, not a broken sink: its
	// feed is still rendered (2026-11-18: a closed workspace's boot restore
	// recorded an ERROR that the footer then teed onto a strip it never
	// bound), and its records go to the central sink (dlog.WorkspaceUnlessGone).
	l, err := dlog.WorkspaceUnlessGone(r.deps.Log, dir)
	if err != nil {
		global.Error("daemon.feed.workspace_sink_unavailable",
			"the feed resolver could not open a workspace's log sink; its records are unroutable",
			dlog.Context{"workspace": string(ws), "dir": dir, "cause": err.Error()})
		return global, true
	}
	return l.With(dlog.Context{"workspace": string(ws)}), true
}
