// Package intakegate decides whether THIS daemon takes the on-disk intakes:
// the command-file ingress and the held-prompt ingress.
//
// ONLY THE DAEMON THAT SERVES TAKES INTAKE. Both ingresses are directories in
// the shared state root, and both call their internal paths directly, so the
// server-level refusals a handover puts in front of every per-workspace rpc
// (transferring_away, not_yet_adopted) do not reach them. During a handover
// two daemons run on one state root: a JOINING successor serves nothing it has
// not adopted, and an incumbent whose handover has begun is handing everything
// away. Either one taking intake would act on a workspace it does not serve --
// a successor reviving a session its incumbent still runs, an incumbent
// queueing a prompt into a workspace it is moving. So each ingress asks the
// gate before every sweep, and the answer is the rollout controller's own
// fact (rollout.Controller.ServesIntake): not joining, and no handover in
// flight. The incumbent stops the instant its handover begins; the successor
// starts the instant it owns everything it was handed. Between the two,
// nothing is taken, and every entry waits on disk.
//
// The gate says which daemon may sweep; it does not make a sweep exclusive.
// A sweep the incumbent began before its handover can still be running when
// the successor's gate opens, so each ingress keeps its own exclusivity per
// entry: the command-file ingress claims a file by rename, and the
// held-prompt ingress sweeps under a kernel lock.
package intakegate

import (
	"sync"

	"claude-repld/internal/dlog"
)

// Gate answers, before each sweep, whether this daemon takes intake now, and
// records every change of that answer once.
type Gate struct {
	serves func() bool
	log    dlog.Logger
	op     string

	mu sync.Mutex
	// open is the last answer; known is whether there has been one.
	open, known bool
}

// New builds a gate over serves, recording under op on log.
func New(serves func() bool, log dlog.Logger, op string) *Gate {
	return &Gate{serves: serves, log: log, op: op}
}

// Admits reports whether this daemon takes intake now. The first answer and
// every change are recorded at INFO -- a daemon that stops or starts taking an
// intake is the one fact that explains why an entry waits -- and an unchanged
// answer is not recorded at all, because the ingress asks on every poll.
func (g *Gate) Admits() bool {
	open := g.serves()
	g.mu.Lock()
	changed := !g.known || g.open != open
	g.open, g.known = open, true
	g.mu.Unlock()
	if changed {
		if open {
			g.log.Info(g.op, "this daemon serves, so it takes the intake", nil)
		} else {
			g.log.Info(g.op, "this daemon does not serve (a successor still joining, or a handover in flight), so it leaves the intake for the daemon that does", nil)
		}
	}
	return open
}
