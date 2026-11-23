package merge

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// This file is the merge queue's half of SUBSCRIPTIONS
// (agentrepl.v1.UpdateMergeQueueSubscribe): recording a workspace's
// subscription, and REPORTING every change of a queue, with the queue as it
// stands after it, to the observer that tells the subscribers
// (Deps.QueueEvents; internal/mergenotice).
//
// A CHANGE IS REPORTED BY THE WRITE THAT MADE IT, never found by comparing
// two looks at the queue. The orchestrator's DB is wrapped (queueEventsDB),
// as it is for the roster's queue (rosterqueue.go), so every write that
// changes a queue's line reports itself once it has committed. Two daemons
// share the store across a handover; a write is made by exactly one of them,
// so each change is reported exactly once, where two observers comparing
// looks would each report the other's changes. A merge's STEP is not a
// write: it is reported from the one fact publication (publish).

const opQueueEvents = "daemon.merge.queue_events"

// QueueChangeKind names what changed in a merge queue.
type QueueChangeKind int

// The kinds. The zero value is no kind.
const (
	// QueueChangeEnqueued is a merge put in line.
	QueueChangeEnqueued QueueChangeKind = iota + 1
	// QueueChangeRequeued is a merge a restart cut short before it took a
	// step, put back in line at its own place, to run again. It never left the
	// queue.
	QueueChangeRequeued
	// QueueChangeStarted is a merge admitted: it has started.
	QueueChangeStarted
	// QueueChangeStepped is a started merge moving to a new step.
	QueueChangeStepped
	// QueueChangeLanded is a merge that landed and left the queue.
	QueueChangeLanded
	// QueueChangeFailed is a merge that gave up and left the queue.
	QueueChangeFailed
	// QueueChangeDequeued is a merge taken off the queue without landing or
	// failing: evicted, dequeued, or abandoned under its closing workspace or
	// a restart. QueueChange.Cause says which.
	QueueChangeDequeued
	// QueueChangeReordered is the queue's order changed by a move.
	QueueChangeReordered
	// QueueChangePaused is the queue paused.
	QueueChangePaused
	// QueueChangeResumed is the queue resumed.
	QueueChangeResumed
)

// String names the kind in log records.
func (k QueueChangeKind) String() string {
	switch k {
	case QueueChangeEnqueued:
		return "enqueued"
	case QueueChangeRequeued:
		return "requeued"
	case QueueChangeStarted:
		return "started"
	case QueueChangeStepped:
		return "stepped"
	case QueueChangeLanded:
		return "landed"
	case QueueChangeFailed:
		return "failed"
	case QueueChangeDequeued:
		return "dequeued"
	case QueueChangeReordered:
		return "reordered"
	case QueueChangePaused:
		return "paused"
	case QueueChangeResumed:
		return "resumed"
	}
	return fmt.Sprintf("QueueChangeKind(%d)", int(k))
}

// Leaves reports whether the change is a merge LEAVING the queue: the
// queue's advance.
func (k QueueChangeKind) Leaves() bool {
	return k == QueueChangeLanded || k == QueueChangeFailed || k == QueueChangeDequeued
}

// MergeMode is how a merge in line was asked for, as the merge-queue skill
// names its modes.
type MergeMode int

// The modes. The zero value is no mode.
const (
	// MergeModeOneShot is a workspace's own branch merged in place, closing
	// the workspace once it lands.
	MergeModeOneShot MergeMode = iota + 1
	// MergeModeAsync is a branch merged in a hidden merge workspace forked
	// from the workspace that asked, which stays free.
	MergeModeAsync
	// MergeModeInPlace is any other merge run in the workspace that asked,
	// which stays open once it lands.
	MergeModeInPlace
)

// String names the mode as a notice states it.
func (m MergeMode) String() string {
	switch m {
	case MergeModeOneShot:
		return "one-shot"
	case MergeModeAsync:
		return "async"
	case MergeModeInPlace:
		return "in place"
	}
	return fmt.Sprintf("MergeMode(%d)", int(m))
}

// QueueMember is one merge in line.
type QueueMember struct {
	// Workspace is the workspace the merge runs in: the merge workspace of an
	// async merge.
	Workspace ids.WorkspaceID
	// Name is that workspace's name.
	Name string
	// Branch is the branch the merge lands.
	Branch string
	// Mode is how the merge was asked for.
	Mode MergeMode
	// Started reports whether the merge is being worked on: admitted, and
	// not back from a repair out of line waiting to be taken up
	// (backInLine). Only a started merge carries a step.
	Started bool
	// Front reports that the merge asked to be put at the front of its queue.
	Front bool
	// Requester is the workspace that asked for an ASYNC merge, empty for
	// every other mode.
	Requester ids.WorkspaceID
	// Step is a started merge's step, empty while it waits and before its
	// first step is published.
	Step footer.MergeStep
	// Repair is the repair slot a merge out of line holds or awaits, the
	// store's stamped slot as every surface draws it; RepairNone for every
	// merge in line.
	Repair sidebar.QueueRepair
}

// QueueSnapshot is one repository's queue as it stands.
type QueueSnapshot struct {
	// Repo is the queue's key.
	Repo wsm.RepoKey
	// Paused reports whether the queue is paused.
	Paused bool
	// Members are the merges in line, front first. A merge only requested
	// (its asking turn still running) is not in line.
	Members []QueueMember
	// Remediating are the merges out of line (remediate.go), in the order
	// they stepped aside, each with the repair slot it holds or awaits. They
	// are in no place in line.
	Remediating []QueueMember
}

// QueueChange is one change of a merge queue, as its observer is told it.
type QueueChange struct {
	// Kind is what changed.
	Kind QueueChangeKind
	// Subject is the merge the change is about, as it stood when it changed;
	// the zero value for a pause or a resume.
	Subject QueueMember
	// Cause is the recorded reason a failed or dequeued merge left (its
	// queue cause: "failed", "pick_failed", "restart_failed", an eviction, a
	// dequeue); empty for every other kind.
	Cause string
	// Depleted reports that a merge leaving the queue left no merge in line.
	Depleted bool
	// Queue is the queue as it stands after the change.
	Queue QueueSnapshot
	// Seq is the change's place in the one order this daemon makes queue
	// changes and takes subscriptions in (queueEvents.seq): a subscription
	// taken at Seq or later was taken after the change, and is never told
	// it. Zero for a change no queue write made.
	Seq uint64
}

// DeployEnd is how one deploy of the daemon's own checkout ended.
type DeployEnd struct {
	// Failure is why the deploy failed; "" for one whose build now runs,
	// every shim judged against it.
	Failure string
	// Boot marks a fresh boot on a new build (the daemon restarted), rather
	// than a deploy a running daemon carried out.
	Boot bool
	// Landed are the workspaces whose landing this deploy's build carries
	// (or, for a failed deploy, would have carried): the merge workspace of
	// every landing waiting for the deploy, which is the workspace itself for
	// a merge in place, a one-shot's landing as much as an async merge's.
	// Each is told its landing's deploy (WORKSPACE_NOTICE_EVENT_DEPLOYED).
	Landed []ids.WorkspaceID
}

// QueueObserver is told every change of every merge queue this daemon makes,
// in the order it made them (Deps.QueueEvents). QueueChanged must neither
// block nor call back into the orchestrator: it is called from the queue's
// own writes.
type QueueObserver interface {
	QueueChanged(change QueueChange)
	// Subscribed is told every merge queue subscription taken, with the Seq
	// of the last change made before it (THROUGH): the subscription is told
	// only changes after it. A replaced subscription is not told again, since
	// it stood through the changes in between. The same rules as
	// QueueChanged's hold: it is called under the events lock.
	Subscribed(subscriber ids.WorkspaceID, repo wsm.RepoKey, through uint64)
}

// QueueObservers tells every observer each change, in order.
type QueueObservers []QueueObserver

// QueueChanged implements QueueObserver.
func (o QueueObservers) QueueChanged(change QueueChange) {
	for _, observer := range o {
		observer.QueueChanged(change)
	}
}

// Subscribed implements QueueObserver.
func (o QueueObservers) Subscribed(subscriber ids.WorkspaceID, repo wsm.RepoKey, through uint64) {
	for _, observer := range o {
		observer.Subscribed(subscriber, repo, through)
	}
}

// queueEvents serializes every reported write of every queue with its reads,
// so the reports reach the observer in the order the writes were made, and a
// merge leaving the queue judges whether it emptied the line against exactly
// the line its own write left: two merges leaving together can never both
// see an empty line.
//
// IT NEVER NESTS WITH o.mu, in either order: a reported write may be made
// while o.mu is held elsewhere, so nothing done under this lock takes o.mu.
// That is why each started merge's step is kept here (steps) rather than read
// off the published facts.
type queueEvents struct {
	mu sync.Mutex
	// requeued are the merges a restart took out of line only to put them
	// straight back (causeRestartResume): their removal is no departure, and
	// their return is reported as a requeue.
	requeued map[ids.WorkspaceID]bool
	// steps is the step each merge running here last published, present
	// only while its facts say it is merging.
	steps map[ids.WorkspaceID]footer.MergeStep
	// seq is the Seq of the last change this daemon made (QueueChange.Seq):
	// every change and every subscription takes its place in one order
	// under this lock, so a subscription is told exactly the changes made
	// after it, however late the notifier delivers them (2026-10-31: a
	// subscription taken just after a depletion was ended by it).
	seq uint64
}

// stamp gives CHANGE the next place in the order. Called under the lock.
func (e *queueEvents) stamp(change *QueueChange) {
	e.seq++
	change.Seq = e.seq
}

// newQueueEvents builds the empty event state.
func newQueueEvents() *queueEvents {
	return &queueEvents{requeued: map[ids.WorkspaceID]bool{}, steps: map[ids.WorkspaceID]footer.MergeStep{}}
}

// queueEventsDB is the orchestrator's DB, with every write that changes a
// queue's line reporting the change once it has committed. A write that adds
// or removes only a REQUESTED row (RequestMerge, RequestOneShotMerge,
// HandOffDetachedMerge) changes no line and is not wrapped.
type queueEventsDB struct {
	wsm.DB
	o *orchestrator
}

// queueWrite is what one write reports: nothing, or a change of KIND about
// SUBJECT, with the removal's CAUSE for a merge leaving.
type queueWrite struct {
	report  bool
	kind    QueueChangeKind
	subject ids.WorkspaceID
	cause   string
}

// QueueMerge puts a requested merge in line and reports it.
func (d queueEventsDB) QueueMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, ledger wsm.LeaseID) (int, error) {
	var place int
	err := d.reported(ctx, repo, false, func() (queueWrite, error) {
		var err error
		place, err = d.DB.QueueMerge(ctx, repo, id, ledger)
		kind := QueueChangeEnqueued
		if d.o.events.requeued[id] {
			delete(d.o.events.requeued, id)
			kind = QueueChangeRequeued
		}
		return queueWrite{report: true, kind: kind, subject: id}, err
	})
	return place, err
}

// RequeuePausedMerge puts a merge a handover paused back in line at its
// place and reports it requeued.
func (d queueEventsDB) RequeuePausedMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, lease wsm.LeaseID, ledger wsm.LeaseID) error {
	return d.reported(ctx, repo, false, func() (queueWrite, error) {
		return queueWrite{report: true, kind: QueueChangeRequeued, subject: id}, d.DB.RequeuePausedMerge(ctx, repo, id, lease, ledger)
	})
}

// ReturnAdmittedMerge puts an admitted merge with no run back in line at its
// place and reports it requeued.
func (d queueEventsDB) ReturnAdmittedMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, ledger wsm.LeaseID) error {
	return d.reported(ctx, repo, false, func() (queueWrite, error) {
		return queueWrite{report: true, kind: QueueChangeRequeued, subject: id}, d.DB.ReturnAdmittedMerge(ctx, repo, id, ledger)
	})
}

// AdmitMerge starts a merge and reports it.
func (d queueEventsDB) AdmitMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID) error {
	return d.reported(ctx, repo, false, func() (queueWrite, error) {
		return queueWrite{report: true, kind: QueueChangeStarted, subject: id}, d.DB.AdmitMerge(ctx, repo, id)
	})
}

// RemoveMergeQueueEntry drops an entry and reports the merge leaving.
func (d queueEventsDB) RemoveMergeQueueEntry(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, cause string) error {
	return d.reported(ctx, repo, true, func() (queueWrite, error) {
		return d.leaving(id, cause), d.DB.RemoveMergeQueueEntry(ctx, repo, id, cause)
	})
}

// DropAdmittedMerge drops an admitted merge and reports it leaving.
func (d queueEventsDB) DropAdmittedMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, lease wsm.LeaseID, cause string) error {
	return d.reported(ctx, repo, true, func() (queueWrite, error) {
		return d.leaving(id, cause), d.DB.DropAdmittedMerge(ctx, repo, id, lease, cause)
	})
}

// MoveMerge moves a waiting merge and reports the reorder when it moved.
func (d queueEventsDB) MoveMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, move wsm.MergeMove) (wsm.MergeMoveOutcome, error) {
	var outcome wsm.MergeMoveOutcome
	err := d.reported(ctx, repo, false, func() (queueWrite, error) {
		var err error
		outcome, err = d.DB.MoveMerge(ctx, repo, id, move)
		return queueWrite{report: outcome.Moved(), kind: QueueChangeReordered, subject: id}, err
	})
	return outcome, err
}

// MoveMergeTo moves a waiting merge to a place beside another and reports the
// reorder when it moved.
func (d queueEventsDB) MoveMergeTo(ctx context.Context, repo wsm.RepoKey, id, anchor wsm.WorkspaceID, side wsm.MergeSide) (bool, error) {
	var moved bool
	err := d.reported(ctx, repo, false, func() (queueWrite, error) {
		var err error
		moved, err = d.DB.MoveMergeTo(ctx, repo, id, anchor, side)
		return queueWrite{report: moved, kind: QueueChangeReordered, subject: id}, err
	})
	return moved, err
}

// PlaceMerge moves a waiting merge to a place in its queue and reports the
// reorder when it moved.
func (d queueEventsDB) PlaceMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, place wsm.MergePlace) (bool, error) {
	var moved bool
	err := d.reported(ctx, repo, false, func() (queueWrite, error) {
		var err error
		moved, err = d.DB.PlaceMerge(ctx, repo, id, place)
		return queueWrite{report: moved, kind: QueueChangeReordered, subject: id}, err
	})
	return moved, err
}

// PrioritizeMerge moves a waiting merge to the front of the waiting and
// reports the reorder when it moved.
func (d queueEventsDB) PrioritizeMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID) (bool, error) {
	var moved bool
	err := d.reported(ctx, repo, false, func() (queueWrite, error) {
		var err error
		moved, err = d.DB.PrioritizeMerge(ctx, repo, id)
		return queueWrite{report: moved, kind: QueueChangeReordered, subject: id}, err
	})
	return moved, err
}

// SetMergeQueuePaused pauses or resumes a queue and reports it. The
// orchestrator writes only a change (setPaused), so every write is one.
func (d queueEventsDB) SetMergeQueuePaused(ctx context.Context, repo wsm.RepoKey, pause *wsm.MergeQueuePause) error {
	return d.reported(ctx, repo, false, func() (queueWrite, error) {
		kind := QueueChangeResumed
		if pause != nil {
			kind = QueueChangePaused
		}
		return queueWrite{report: true, kind: kind}, d.DB.SetMergeQueuePaused(ctx, repo, pause)
	})
}

// leaving is what a removal reports: a merge leaving for CAUSE, or nothing
// for a merge a restart puts straight back in line. Called under the events
// lock.
func (d queueEventsDB) leaving(id wsm.WorkspaceID, cause string) queueWrite {
	if cause == causeRestartResume {
		d.o.events.requeued[id] = true
		return queueWrite{}
	}
	return queueWrite{report: true, kind: leaveKind(cause), subject: id, cause: cause}
}

// leaveKind classifies a removal's recorded cause.
func leaveKind(cause string) QueueChangeKind {
	switch cause {
	case causeMerged:
		return QueueChangeLanded
	case causeFailed, causeRestartFailed, causePickFailed, causeDetachedForkFailed:
		return QueueChangeFailed
	}
	return QueueChangeDequeued
}

// reported runs WRITE against REPO's queue under the events lock and reports
// what it says it changed. A removal (LEAVES) reads the line BEFORE the write
// too, because the merge leaving is described as it stood in line, and a
// merge that was only requested left no line and is not reported.
//
// A REPORT THAT CANNOT BE READ NEVER FAILS THE WRITE: the write is the
// queue's own work and has committed. It is recorded at ERROR, and the
// subscribers are not told of that change.
func (d queueEventsDB) reported(ctx context.Context, repo wsm.RepoKey, leaves bool, write func() (queueWrite, error)) error {
	d.o.events.mu.Lock()
	var before []wsm.MergeQueueEntry
	var beforeErr error
	if leaves {
		before, beforeErr = d.DB.MergeQueue(ctx, repo)
	}
	w, err := write()
	if err != nil || !w.report {
		d.o.events.mu.Unlock()
		return err
	}
	// THE WRITE IS COUNTED ONCE IT COMMITTED, before any read that could
	// keep its subscribers from hearing of it.
	d.o.deps.Meter.CountQueueChange(w.kind.String(), w.cause)
	fields := dlog.Context{"repo": string(repo), "change": w.kind.String(), "workspace": string(w.subject), "cause": w.cause}
	if beforeErr != nil {
		d.o.events.mu.Unlock()
		recordFailed(d.o.deps.Log.Global(), opQueueEvents, logsite.Msg("could not read the merge queue before a merge left it; its subscribers are not told of the change"),
			logsite.Msg("the merge queue's read before a merge left it ended when its context was cancelled; its subscribers are not told, and the next notice carries its effect"),
			fields, beforeErr)
		return nil
	}
	change, ok, err := d.describe(ctx, repo, w, before)
	if ok {
		d.o.events.fillSteps(&change)
		d.o.events.stamp(&change)
	}
	d.o.events.mu.Unlock()
	if err != nil {
		recordFailed(d.o.deps.Log.Global(), opQueueEvents, logsite.Msg("could not read the merge queue after its change; its subscribers are not told of the change"),
			logsite.Msg("the merge queue's read after its change ended when its context was cancelled; its subscribers are not told, and the next notice carries its effect"),
			fields, err)
		return nil
	}
	if !ok {
		d.o.deps.Log.Global().Debug(opQueueEvents, "a merge only requested left the queue; no line changed, so nothing is reported", fields)
		return nil
	}
	d.o.tell(change)
	return nil
}

// describe reads REPO's queue as W left it and describes the change. It
// answers false for a removal of a merge that was not in line. Called under
// the events lock.
func (d queueEventsDB) describe(ctx context.Context, repo wsm.RepoKey, w queueWrite, before []wsm.MergeQueueEntry) (QueueChange, bool, error) {
	queue, err := d.o.readQueue(ctx, d.DB, repo)
	if err != nil {
		return QueueChange{}, false, err
	}
	change := QueueChange{Kind: w.kind, Queue: queue}
	switch {
	case w.kind.Leaves():
		members, err := d.o.queueMembers(ctx, d.DB, inLine(before))
		if err != nil {
			return QueueChange{}, false, err
		}
		subject, found := memberNamed(members, w.subject)
		if !found {
			return QueueChange{}, false, nil
		}
		change.Subject, change.Depleted = subject, len(queue.Members) == 0
		if w.kind == QueueChangeDequeued || w.kind == QueueChangeFailed {
			change.Cause = w.cause
		}
	case w.subject != "":
		subject, found := memberNamed(queue.Members, w.subject)
		if !found {
			return QueueChange{}, false, fmt.Errorf("merge: the %s merge of %s is not in its queue's line after the write", w.kind, w.subject)
		}
		change.Subject = subject
	}
	return change, true, nil
}

// readQueue reads REPO's queue through DB: its line, whether it is paused,
// and each member as the registry names it. Steps are filled in by tell.
func (o *orchestrator) readQueue(ctx context.Context, db wsm.DB, repo wsm.RepoKey) (QueueSnapshot, error) {
	stored, err := db.MergeQueue(ctx, repo)
	if err != nil {
		return QueueSnapshot{}, err
	}
	paused, err := db.MergeQueuePaused(ctx, repo)
	if err != nil {
		return QueueSnapshot{}, err
	}
	members, err := o.queueMembers(ctx, db, inLine(stored))
	if err != nil {
		return QueueSnapshot{}, err
	}
	var remediating []QueueMember
	if aside := outOfLine(stored); len(aside) > 0 {
		remediating, err = o.queueMembers(ctx, db, aside)
		if err != nil {
			return QueueSnapshot{}, err
		}
		for i := range remediating {
			remediating[i].Repair = rosterRepair(aside[i].Slot)
		}
	}
	return QueueSnapshot{Repo: repo, Paused: paused, Members: members, Remediating: remediating}, nil
}

// queueMembers describes each in-line entry as a member.
func (o *orchestrator) queueMembers(ctx context.Context, db wsm.DB, entries []wsm.MergeQueueEntry) ([]QueueMember, error) {
	members := make([]QueueMember, 0, len(entries))
	for _, entry := range entries {
		record, err := db.Workspace(ctx, entry.Workspace)
		if err != nil {
			return nil, fmt.Errorf("merge: reading the workspace %s in line: %w", entry.Workspace, err)
		}
		member := memberOf(entry, record)
		if member.Started {
			back, err := backInLine(ctx, db, entry.Workspace)
			if err != nil {
				return nil, err
			}
			member.Started = !back
		}
		members = append(members, member)
	}
	return members, nil
}

// backInLine reports whether WS's admitted merge is back in line from its
// repair out of line, waiting to be taken up at the head: its progress record
// says so (progressDoc.BackInLine), the one durable fact the bubble's waiting
// draw reads too (drawBackInLine). Such a merge is admitted but not being
// worked on, so no surface calls it running (2026-11-15: `merge-queue
// status` drew one as running with no step).
func backInLine(ctx context.Context, db wsm.DB, ws ids.WorkspaceID) (bool, error) {
	stored, found, err := db.MergeProgressOf(ctx, ws)
	if err != nil {
		return false, fmt.Errorf("merge: reading the progress of %s's merge in line: %w", ws, err)
	}
	if !found {
		return false, nil
	}
	doc, err := decodeProgress(stored)
	if err != nil {
		return false, err
	}
	return doc.BackInLine, nil
}

// memberOf describes one in-line entry. A request an earlier build recorded
// carries no branch; its own-branch source lands the workspace's branch.
func memberOf(entry wsm.MergeQueueEntry, record wsm.Workspace) QueueMember {
	branch := entry.Source.Branch
	if branch == "" && entry.Source.Kind.ClosesRequester() {
		branch = record.Branch
	}
	member := QueueMember{
		Workspace: entry.Workspace,
		Name:      record.Name,
		Branch:    branch,
		Mode:      modeOf(entry.Source, record),
		Started:   entry.State != wsm.MergeQueued,
		Front:     entry.Source.Front,
	}
	if record.MergeOf != nil {
		member.Requester = record.MergeOf.Requester
	}
	return member
}

// modeOf names how a merge in line was asked for: async when it runs in a
// merge workspace, a one-shot when it lands the workspace's own branch and
// closes it, and in place otherwise.
func modeOf(source wsm.MergeSource, record wsm.Workspace) MergeMode {
	switch {
	case record.MergeOf != nil:
		return MergeModeAsync
	case source.Kind.ClosesRequester() && !source.KeepOpen:
		return MergeModeOneShot
	}
	return MergeModeInPlace
}

// memberNamed finds WS among MEMBERS.
func memberNamed(members []QueueMember, ws ids.WorkspaceID) (QueueMember, bool) {
	for _, member := range members {
		if member.Workspace == ws {
			return member, true
		}
	}
	return QueueMember{}, false
}

// fillSteps sets each started member's step, and the subject's, from the
// steps the merges published. Called under the events lock.
func (e *queueEvents) fillSteps(change *QueueChange) {
	for i := range change.Queue.Members {
		member := &change.Queue.Members[i]
		if member.Started {
			member.Step = e.steps[member.Workspace]
		}
	}
	if change.Subject.Started && !change.Kind.Leaves() {
		change.Subject.Step = e.steps[change.Subject.Workspace]
	}
}

// tell hands one described change to the observer, outside the events lock.
func (o *orchestrator) tell(change QueueChange) {
	o.deps.Log.Global().Debug(opQueueEvents, "reported a merge queue change to its subscribers", dlog.Context{
		"repo": string(change.Queue.Repo), "change": change.Kind.String(), "workspace": string(change.Subject.Workspace),
		"members": len(change.Queue.Members), "depleted": change.Depleted})
	o.deps.QueueEvents.QueueChanged(change)
}

// noteFacts keeps WS's step as its newly published FACTS state it, and
// reports a STARTED MERGE MOVING TO A NEW STEP. A merge whose facts this
// daemon publishes for the first time (HADFACTS false) is a merge it took
// over, from a restart or a handover, whose step its predecessor already
// reported, so it is kept and not reported again.
func (o *orchestrator) noteFacts(ws ids.WorkspaceID, repo wsm.RepoKey, hadFacts bool, facts MergeFacts) {
	ctx := context.Background()
	o.events.mu.Lock()
	prev, had := o.events.steps[ws]
	// A FINISHING DEPLOY IS NOT IN LINE YET (finish.go): its step is no
	// queue's change, so it is reported to no subscriber.
	if facts.State != StateMerging || facts.Step == footer.StepFinishing {
		delete(o.events.steps, ws)
		o.events.mu.Unlock()
		return
	}
	o.events.steps[ws] = facts.Step
	if !hadFacts || (had && prev == facts.Step) {
		o.events.mu.Unlock()
		return
	}
	// A STEP CHANGE NAMES THE STEP IT MOVED TO: a started merge's facts
	// always carry one (setStep is handed its step), so a merge moving to no
	// step is a defect in whatever published it, never a change to tell
	// (2026-10-27: a notice said a merge "moved to its  step").
	if facts.Step == "" {
		o.events.mu.Unlock()
		panic(fmt.Sprintf("merge: %s's merge in %s published a merging state with no step, after step %q", ws, repo, prev))
	}
	fields := dlog.Context{"repo": string(repo), "change": QueueChangeStepped.String(), "workspace": string(ws), "step": string(facts.Step)}
	queue, err := o.readQueue(ctx, o.deps.DB, repo)
	if err != nil {
		o.events.mu.Unlock()
		o.deps.Log.Global().Error(opQueueEvents, "could not read the merge queue after a merge's step changed; its subscribers are not told of the change",
			withField(fields, "error", err.Error()))
		return
	}
	subject, found := memberNamed(queue.Members, ws)
	if !found {
		o.events.mu.Unlock()
		// A MERGE REPAIRING OUT OF LINE STEPS LEGITIMATELY: its resolving
		// rebase or its fixing turn moves its own footer from step to step,
		// while every queue surface draws it by its repair slot alone
		// (QueueSnapshot.Remediating), so its step is no queue change.
		if _, aside := memberNamed(queue.Remediating, ws); aside {
			o.deps.Log.Global().Debug(opQueueEvents, "a merge repairing out of line moved to a new step; its queue's subscribers see its repair slot, not its step, so no change is reported", fields)
			return
		}
		o.deps.Log.Global().Error(opQueueEvents, "a merge published a new step while neither in its queue's line nor repairing out of it; its subscribers are not told of the change", fields)
		return
	}
	change := QueueChange{Kind: QueueChangeStepped, Subject: subject, Queue: queue}
	o.events.fillSteps(&change)
	o.events.stamp(&change)
	o.events.mu.Unlock()
	o.tell(change)
}

// Subscribe implements Orchestrator.
func (o *orchestrator) Subscribe(ctx context.Context, subscriber ids.WorkspaceID, scope *RepositoryScope) (bool, error) {
	const op = "daemon.merge.subscribe"
	repo, err := o.subscriptionScope(ctx, op, "a merge queue subscription", scope)
	if err != nil {
		return false, err
	}
	log := o.log(ctx, subscriber)
	fields := dlog.Context{"subscriber": string(subscriber), "repo": string(repo), "scoped": scope != nil}
	// THE SUBSCRIPTION TAKES ITS PLACE IN THE CHANGES' ORDER, under the
	// events lock, so the notifier tells it only the changes after it.
	o.events.mu.Lock()
	replaced, err := o.deps.DB.SubscribeMergeQueue(ctx, wsm.MergeQueueSubscription{
		Subscriber: subscriber, Repo: repo, SubscribedAt: o.deps.Now(),
	})
	if err == nil && !replaced {
		o.deps.QueueEvents.Subscribed(subscriber, repo, o.events.seq)
	}
	fields["through"] = o.events.seq
	o.events.mu.Unlock()
	if errors.Is(err, wsm.ErrSubscriberClosed) {
		log.Info(op, "refused a merge queue subscription of a closed workspace", withField(fields, "arm", ArmSubscriberClosed))
		return false, &RefusalError{Arm: ArmSubscriberClosed, Workspace: subscriber, Reason: "a closed workspace cannot subscribe to a merge queue"}
	}
	if err != nil {
		log.Error(op, "could not record a merge queue subscription", withField(fields, "error", err.Error()))
		return false, err
	}
	fields["replaced"] = replaced
	log.Info(op, "subscribed the workspace's agent to the merge queue's changes", fields)
	return replaced, nil
}

// Unsubscribe implements Orchestrator.
func (o *orchestrator) Unsubscribe(ctx context.Context, subscriber ids.WorkspaceID, scope *RepositoryScope) error {
	const op = "daemon.merge.unsubscribe"
	repo, err := o.subscriptionScope(ctx, op, "a merge queue unsubscribe", scope)
	if err != nil {
		return err
	}
	log := o.log(ctx, subscriber)
	fields := dlog.Context{"subscriber": string(subscriber), "repo": string(repo), "scoped": scope != nil}
	ended, err := o.deps.DB.UnsubscribeMergeQueue(ctx, subscriber, repo)
	if err != nil {
		log.Error(op, "could not end a merge queue subscription", withField(fields, "error", err.Error()))
		return err
	}
	if !ended {
		log.Info(op, "the workspace holds no merge queue subscription in that scope; nothing ended", withField(fields, "arm", ArmNotSubscribed))
		return &RefusalError{Arm: ArmNotSubscribed, Workspace: subscriber, Reason: "the workspace holds no merge queue subscription in that scope"}
	}
	log.Info(op, "ended the workspace's merge queue subscription", fields)
	return nil
}

// subscriptionScope resolves a subscription's scope: every repository for a
// nil scope, else the one repository's queue key, resolved as a pause's is.
func (o *orchestrator) subscriptionScope(ctx context.Context, op, what string, scope *RepositoryScope) (wsm.RepoKey, error) {
	if scope == nil {
		return wsm.EveryRepository, nil
	}
	return o.queueKeyOf(ctx, op, what, *scope)
}
