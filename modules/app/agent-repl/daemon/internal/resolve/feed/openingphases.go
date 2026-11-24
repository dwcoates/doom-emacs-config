package feed

import (
	"time"

	"agent-repld/internal/dlog"
)

// openingPhases is where one opening's time went, each wait and step summed
// over the store pages it read (pageStep.reads). It is carried on the
// opening's load plans (loadPlan.phases) and their page loads
// (pageLoad.phases), and reported on its `daemon.feed.opening_reads` record.
//
// AN OPENING SAYS WHAT IT WAITED ON. On the 2026-11-23 15:35 handover
// daily-standup-canonical's open began about 15:35:58.9 and its first feed
// record was 15:36:04.48: some 5.5s in which the opening wrote nothing, so
// whether it queued for the workspace's load lock, the resolver's one mutex,
// the shim's history read or the state database could not be read from the
// logs. Every wait an opening makes is timed here.
type openingPhases struct {
	// loadLockWait is the wait for the workspace's load lock (loadMu).
	loadLockWait time.Duration
	// lockWait is the waits for the resolver's mutex outside the replay:
	// composing each step, and the load's own takes around its read.
	lockWait time.Duration
	// read is the history reads from the session's shim (readPlanned).
	read time.Duration
	// records is the durable turn records read before the replay takes the
	// mutex (replayPage's state-database reads).
	records time.Duration
	// replayLockWait is the replay's wait for the resolver's mutex.
	replayLockWait time.Duration
	// replay is the replay itself, under the mutex.
	replay time.Duration
}

// fields is the phases as a record's context, each in milliseconds.
func (p *openingPhases) fields() dlog.Context {
	return dlog.Context{
		"load_lock_wait_ms":   p.loadLockWait.Milliseconds(),
		"lock_wait_ms":        p.lockWait.Milliseconds(),
		"read_ms":             p.read.Milliseconds(),
		"records_ms":          p.records.Milliseconds(),
		"replay_lock_wait_ms": p.replayLockWait.Milliseconds(),
		"replay_ms":           p.replay.Milliseconds(),
	}
}

// lockTimed takes the resolver's mutex and adds the wait to INTO, when INTO
// is not nil: a take outside an opening is not timed.
func (r *resolver) lockTimed(into *time.Duration) {
	if into == nil {
		r.mu.Lock()
		return
	}
	asked := r.deps.Now()
	r.mu.Lock()
	*into += r.deps.Now().Sub(asked)
}

// since adds the time since FROM to INTO, when INTO is not nil.
func (r *resolver) since(into *time.Duration, from time.Time) {
	if into != nil {
		*into += r.deps.Now().Sub(from)
	}
}

// The phases' selectors, one per field, for phase.
func loadLockWaitOf(p *openingPhases) *time.Duration   { return &p.loadLockWait }
func lockWaitOf(p *openingPhases) *time.Duration       { return &p.lockWait }
func readOf(p *openingPhases) *time.Duration           { return &p.read }
func recordsOf(p *openingPhases) *time.Duration        { return &p.records }
func replayLockWaitOf(p *openingPhases) *time.Duration { return &p.replayLockWait }
func replayOf(p *openingPhases) *time.Duration         { return &p.replay }

// phase answers the field of PHASES that PICK names, nil when PHASES is: the
// one door every timed step goes through, so an untimed load times nothing.
func phase(phases *openingPhases, pick func(*openingPhases) *time.Duration) *time.Duration {
	if phases == nil {
		return nil
	}
	return pick(phases)
}
