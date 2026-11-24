package sidebar

import (
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// THE ROSTER IS RESOLVED ONCE PER BATCH OF CHANGES, NOT ONCE PER CHANGE.
//
// Every change republishes the WHOLE roster, and a roster render walks every
// registered workspace, so a change costs time in proportion to the registry.
// Rendered one change at a time under one lock, N changes arriving together
// cost N whole renders end to end, and the last caller waited for all of them.
// On 2026-11-13 a handover adopted 125 workspaces at once into a 744-workspace
// registry: every adoption's link, live-work and session facts queued on the
// roster's lock behind one another's renders, each caller holding its session
// watcher's lock while it waited, so the watchers stalled past the lockwatch
// threshold (`daemon.lockwatch.stall`, held 6s to 15s), Emacs's 10s
// AdoptHostWorkspace calls timed out, and one adopted shim's facts missed the
// mid-work adoption's bound, which handed the workspace back.
//
// A COMBINED COMMIT bounds every caller's wait by two renders, however many
// changes arrive together. A change is queued; the one caller that finds no
// leader becomes it, takes the whole queue, applies every change in it under
// the lock, and renders and publishes once. Changes queued while it rendered
// form the next batch, led by the first of them. A caller returns only once a
// roster carrying its change has been published and its results and status
// changes have reached their sinks, exactly as a change rendered alone did, so
// a lone change is still rendered alone and returns having been published.
//
// THE POSTING VIEW (Posting) QUEUES A CHANGE AND WAITS FOR NOTHING. A change
// is queued under qmu, which is held only to append to the queue or take it,
// and never across a render, so the change takes its place in line at once;
// when no batch is being led, a goroutine of its own leads it. Every change of
// both views is applied in queue order, so a change posted before a committed
// one is applied before it, and a read waits for every change queued before
// it (awaitApplied).
//
// THE LOCK ORDER this establishes:
//   - qmu is a LEAF: nothing is called and nothing waited on while it is held
//     but the queue itself.
//   - mu (the roster's) is taken only by the leader of a batch and by reads;
//     a render under it takes the footer's lock to read a status (roster,
//     then footer), and calls nothing that takes another resolver's lock.
//   - A caller holding a lock of its own (a session watcher's, its view
//     delivery's, the topbar's) changes the roster only through the posting
//     view, so it reaches qmu alone and never waits on mu: the session
//     watchers' roster sink, the footer's status edge (FooterStatusChanged)
//     and the topbar's context fill are all posted (cmd/agent-repld graph.go).
//
// What a batch gives up is a status arm that was superseded before any roster
// carried it: the roster never drew it, so it records only the arms it
// published (`daemon.sidebar.status_arm_changed` is "the roster published").

// mutation is one queued change to the roster.
type mutation struct {
	operation string
	message   logsite.Message
	ctx       dlog.Context
	// log is the change's sink. A workspace-scoped change (scoped) has it
	// resolved under the lock, from the registry the batch is applied to.
	log    dlog.Logger
	ws     ids.WorkspaceID
	scoped bool
	// touchesOwn marks an unscoped change that moves the version of what it
	// changed itself (a registry install, setRegistry); every other unscoped
	// change moves the epoch (kept.go).
	touchesOwn bool
	// apply makes the change under the lock and answers whether it changed
	// anything; it is handed the change's log sink.
	apply func(dlog.Logger) bool
	// changed is apply's answer, recorded by the leader.
	changed bool
	// posted marks a change the posting view queued, whose caller waits for
	// nothing.
	posted bool
	// batch is the sequence number of the batch that applied the change.
	batch uint64
	// signal carries the one word this change's caller waits for: true when
	// it is to lead the next batch, false when its batch has been published
	// and reported.
	signal chan bool
}

// batchOutcome is what one combined commit leaves to be reported off the
// lock.
type batchOutcome struct {
	applied       bool
	ready         bool
	quiet         bool
	results       []resultChange
	viewed        []viewedChange
	statusChanges []statusChange
	visibilities  map[ids.WorkspaceID]wsm.Visibility
}

// commit queues M and, on the committing view, returns once a roster carrying
// it has been published and reported. On the posting view it returns once M
// is queued: a batch with no leader is led on a goroutine of its own.
func (r *resolver) commit(m *mutation) {
	m.signal = make(chan bool, 1)
	m.posted = r.posting
	r.qmu.Lock()
	r.queue = append(r.queue, m)
	if r.onQueued != nil {
		r.onQueued(len(r.queue))
	}
	lead := !r.combining
	r.combining = true
	r.qmu.Unlock()
	if m.posted {
		if lead {
			inflight.Go(func() { r.leadBatch(m) })
		}
		return
	}
	if !lead && !<-m.signal {
		return
	}
	r.leadBatch(m)
}

// Settle implements Resolver: a change that changes nothing, committed
// behind every change queued before it, which then waits for every batch led
// before its own to be reported too.
func (r *resolver) Settle() {
	m := r.barrier("daemon.sidebar.settle", logsite.Msg("the roster settled"))
	r.core.commitView().commit(m)
	r.qmu.Lock()
	defer r.qmu.Unlock()
	for r.reportsBefore(m.batch) {
		r.reported.Wait()
	}
}

// reportsBefore reports whether a batch led before batch SEQ is still being
// reported. Caller holds qmu.
func (r *resolver) reportsBefore(seq uint64) bool {
	for other := range r.reporting {
		if other < seq {
			return true
		}
	}
	return false
}

// leadBatch takes the whole queue as one batch, applies and publishes it, hands
// the lead to the first change queued meanwhile, and reports the batch. SELF is
// the leader's own change, which is always in its batch.
//
// THE LEAD IS HANDED ON BEFORE THE REPORT, so a sink that changes the roster
// again queues behind the next leader rather than behind this goroutine, which
// is the one running the sink.
//
// THE NEXT LEADER IS THE FIRST CHANGE QUEUED MEANWHILE: its caller, told so,
// when that change was committed, and a goroutine of its own when it was
// posted, since a posting caller waits for nothing.
func (r *resolver) leadBatch(self *mutation) {
	r.qmu.Lock()
	batch := r.queue
	r.queue = nil
	r.batches++
	seq := r.batches
	r.reporting[seq] = struct{}{}
	for _, m := range batch {
		m.batch = seq
	}
	r.qmu.Unlock()

	out := r.applyBatch(batch)

	r.qmu.Lock()
	switch {
	case len(r.queue) == 0:
		r.combining = false
	case r.queue[0].posted:
		next := r.queue[0]
		inflight.Go(func() { r.leadBatch(next) })
	default:
		r.queue[0].signal <- true
	}
	r.qmu.Unlock()

	r.report(batch, out)
	r.qmu.Lock()
	delete(r.reporting, seq)
	r.reported.Broadcast()
	r.qmu.Unlock()
	for _, m := range batch {
		if m != self && !m.posted {
			m.signal <- false
		}
	}
}

// initQueue readies the core's queue.
func (c *core) initQueue() {
	c.reporting = map[uint64]struct{}{}
	c.reported = sync.NewCond(&c.qmu)
}

// awaitApplied returns once every change queued before it has been applied
// and published, so a read that follows sees each of them, posted ones
// included. It does not wait for their reports (Settle does), so a sink
// that reads the roster while its batch is reported cannot wait on itself.
func (r *resolver) awaitApplied() {
	r.core.commitView().commit(r.barrier("daemon.sidebar.await_applied",
		logsite.Msg("the roster applied every change queued before a read")))
}

// barrier is a change that changes nothing: committed, it returns once every
// change queued before it has been applied, and it renders, publishes and
// records nothing of its own.
func (r *resolver) barrier(operation string, message logsite.Message) *mutation {
	return &mutation{
		operation: operation, message: message, log: r.log.Global(),
		apply: func(dlog.Logger) bool { return false },
	}
}

// commitView is the core's committing view, which waits for what it commits.
func (c *core) commitView() *resolver { return &resolver{core: c} }

// postingView is the core's posting view, which waits for nothing it
// commits.
func (c *core) postingView() *resolver { return &resolver{core: c, posting: true} }

// Posting implements Resolver.
func (r *resolver) Posting() Resolver { return r.core.postingView() }
