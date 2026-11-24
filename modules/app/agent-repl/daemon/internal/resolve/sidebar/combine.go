package sidebar

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
	// apply makes the change under the lock and answers whether it changed
	// anything; it is handed the change's log sink.
	apply func(dlog.Logger) bool
	// changed is apply's answer, recorded by the leader.
	changed bool
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

// commit queues M and returns once a roster carrying it has been published
// and reported.
func (r *resolver) commit(m *mutation) {
	m.signal = make(chan bool, 1)
	r.qmu.Lock()
	r.queue = append(r.queue, m)
	if r.onQueued != nil {
		r.onQueued(len(r.queue))
	}
	lead := !r.combining
	r.combining = true
	r.qmu.Unlock()
	if !lead && !<-m.signal {
		return
	}
	r.leadBatch(m)
}

// leadBatch takes the whole queue as one batch, applies and publishes it, hands
// the lead to the first change queued meanwhile, and reports the batch. SELF is
// the leader's own change, which is always in its batch.
//
// THE LEAD IS HANDED ON BEFORE THE REPORT, so a sink that changes the roster
// again queues behind the next leader rather than behind this goroutine, which
// is the one running the sink.
func (r *resolver) leadBatch(self *mutation) {
	r.qmu.Lock()
	batch := r.queue
	r.queue = nil
	r.qmu.Unlock()

	out := r.applyBatch(batch)

	r.qmu.Lock()
	if len(r.queue) > 0 {
		r.queue[0].signal <- true
	} else {
		r.combining = false
	}
	r.qmu.Unlock()

	r.report(batch, out)
	for _, m := range batch {
		if m != self {
			m.signal <- false
		}
	}
}
