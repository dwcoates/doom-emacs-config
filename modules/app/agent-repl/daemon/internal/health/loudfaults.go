package health

import (
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
)

// THE DAEMON'S STANDING LOUD FAULTS, as Emacs is told them (owner request,
// 2026-09-28). A failed deploy is surfaced on every client: the webview draws
// it on its footer and its topbar, whose views already carry it, and Emacs
// echoes it in the minibuffer. Emacs holds no per-workspace view that could
// carry a daemon-scoped fault, so it is told on the one daemon-level stream it
// holds, WatchDaemon, as `faults_standing`.
//
// "Loud" is exactly what the topbar's warning strip carries
// (FaultTopbarLine): one rule decides which daemon-scoped faults every client
// surfaces, so the two editors cannot disagree about what is worth saying.
//
// THE PUBLICATION IS STATE, NOT AN EVENT: the whole standing set, replayed to
// a late subscriber. An Emacs whose stream was severed while a deploy failed
// (a handover, a reconnect) is told what stands the instant it resubscribes,
// and it echoes each fault id once however often it is re-told. It is built
// before the fault hook and before the server, so no open can precede it.

// opLoudFaults is the operation every record here is made under.
const opLoudFaults = "daemon.health.loud_faults"

// LoudFaults holds the standing loud faults and publishes the whole set on
// every change.
type LoudFaults struct {
	log dlog.Logger

	mu sync.Mutex
	// standing are the standing loud faults, oldest first.
	standing []*agentreplv1.DaemonStandingFault
	topic    publish.Topic[*agentreplv1.DaemonFaultsStanding]
}

// NewLoudFaults builds the empty set. Nothing is published until a fault
// opens: a subscriber before then is told nothing, which is the same fact as
// an empty list.
func NewLoudFaults(log dlog.Logger) *LoudFaults {
	return &LoudFaults{log: log}
}

// Topic is the publication the WatchDaemon stream of every Emacs subscribes
// to.
func (l *LoudFaults) Topic() *publish.Topic[*agentreplv1.DaemonFaultsStanding] {
	return &l.topic
}

// Opened adds a standing fault. A line with no topbar sentence is not loud,
// and handing one here is the caller's defect: it is refused at ERROR rather
// than told to Emacs as a blank. An id already standing is restated in place.
func (l *LoudFaults) Opened(line FaultLine) {
	fields := dlog.Context{"fault": string(line.ID), "kind": line.Kind, "line": line.Topbar}
	if line.Topbar == "" {
		l.log.Error(opLoudFaults, "a fault with no topbar line was handed on as loud; Emacs is not told it", fields)
		return
	}
	fault := &agentreplv1.DaemonStandingFault{
		FaultId:    string(line.ID),
		Line:       line.Topbar,
		Fault:      DaemonFaultOf(line.Record),
		OpenedAtMs: line.At.UnixMilli(),
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	replaced := false
	for i, held := range l.standing {
		if held.GetFaultId() == fault.GetFaultId() {
			l.standing[i], replaced = fault, true
		}
	}
	if !replaced {
		l.standing = append(l.standing, fault)
	}
	l.publishLocked()
	fields["standing"] = len(l.standing)
	l.log.Info(opLoudFaults, "a loud fault stands; every Emacs is told", fields)
}

// Closed retracts a standing fault by id. An id that is not standing
// retracts nothing and publishes nothing.
func (l *LoudFaults) Closed(id ids.FaultID) {
	l.mu.Lock()
	defer l.mu.Unlock()
	kept := l.standing[:0:0]
	for _, held := range l.standing {
		if held.GetFaultId() != string(id) {
			kept = append(kept, held)
		}
	}
	if len(kept) == len(l.standing) {
		l.log.Debug(opLoudFaults, "a closed fault was not standing as loud; nothing to retract", dlog.Context{"fault": string(id)})
		return
	}
	l.standing = kept
	l.publishLocked()
	l.log.Info(opLoudFaults, "a loud fault closed; every Emacs is told", dlog.Context{"fault": string(id), "standing": len(l.standing)})
}

// publishLocked publishes a fresh snapshot of the set. The elements are never
// mutated once published, so the snapshot shares them.
func (l *LoudFaults) publishLocked() {
	l.topic.Publish(&agentreplv1.DaemonFaultsStanding{
		Faults: append([]*agentreplv1.DaemonStandingFault(nil), l.standing...),
	})
}
