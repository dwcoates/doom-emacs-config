package cachelapse

import (
	"slices"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A LONG NETWORK OUTAGE PULLS THE NEAR KEEP-ALIVES FORWARD (owner request,
// 2026-11-18). An unstable network can drop again soon after it comes back,
// and a beat that comes due while it is down cannot go out, so a cache close
// to its beat may lapse into its cold gate. So the first time the network is
// restored after an outage that stood longer than OutagePrefetchMinimum,
// every workspace whose next beat is due within OutagePrefetchWindow gets its
// keep-alive at once, while the network is known to be up.
//
// THE PULLED BEAT IS AN ORDINARY BEAT, ONLY EARLY. It is marked on k.early,
// which every plan of the workspace's beat honors (beatPlanFor) until a beat
// is answered, so a restore, a free edge or a reconcile in between never
// re-times it back to its old instant. It goes out through sendBeat like any
// other, the shim judges it the same way, and once sent the next beat is
// timed an interval on from it.
//
// ONLY A WORKSPACE THAT OWES A BEAT IS PULLED. The armed beats are the one
// authority on that: a workspace at its cold gate, backlogged, closed past its
// last round or past the idle cutoff has none armed (beatPlanFor), and the
// beat's own plan is read afresh again when it goes out.

// The outage prefetch's thresholds, both the owner's (2026-11-18).
const (
	// OutagePrefetchMinimum is how long the network must have been down
	// straight before its restoration pulls any keep-alive forward.
	OutagePrefetchMinimum = 5 * time.Minute
	// OutagePrefetchWindow is how close to its scheduled beat a workspace
	// must be for the restoration to pull that beat forward.
	OutagePrefetchWindow = 20 * time.Minute
)

// opOutage is the operation the outage prefetch records under.
const opOutage = "daemon.cachelapse.outage_prefetch"

// NetworkRestored is the network coming back after an outage that stood for
// DOWN (netstate.Monitor.OnRestored). It never blocks.
func (k *Keeper) NetworkRestored(down time.Duration) {
	k.enqueue(edge{kind: edgeNetworkRestored, down: down, why: "the network was restored"})
}

// networkRestored pulls forward every armed beat due within the window, when
// the outage that ended stood longer than the minimum.
func (k *Keeper) networkRestored(down time.Duration) {
	log := k.deps.Log.Global()
	fields := dlog.Context{"down_for": down.String(), "minimum": OutagePrefetchMinimum.String()}
	if down <= OutagePrefetchMinimum {
		log.Info(opOutage, "the network was restored after a short outage; no keep-alive is pulled forward", fields)
		return
	}
	now := k.deps.Clock.Now()
	pulled, left := []string{}, 0
	for ws, due := range k.beats {
		if due.Sub(now) > OutagePrefetchWindow {
			left++
			continue
		}
		k.early[ws] = now
		k.beats[ws] = now
		pulled = append(pulled, string(ws))
	}
	slices.Sort(pulled)
	log.Info(opOutage, "the network was restored after a long outage; every keep-alive due soon is sent now",
		dlog.Merge(fields, dlog.Context{"window": OutagePrefetchWindow.String(), "pulled": pulled, "left_as_scheduled": left}))
}

// pulledForward answers DUE moved to WS's pulled-forward beat, when one stands
// earlier (networkRestored).
func (k *Keeper) pulledForward(ws ids.WorkspaceID, due time.Time) time.Time {
	if early, ok := k.early[ws]; ok && early.Before(due) {
		return early
	}
	return due
}

// beatAnswered ends WS's pulled-forward beat: the shim answered a beat.
func (k *Keeper) beatAnswered(ws ids.WorkspaceID) { delete(k.early, ws) }
