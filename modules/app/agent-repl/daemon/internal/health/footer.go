package health

import (
	"context"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE FAULT PARTITION — where every recorded fault kind lands on the footer.
//
// The owner's ruling of 2026-09-13 (docs/REALTEST-JUDGEMENT-CALLS.md, "Owner
// rulings, evening batch"): EVERY daemon fault kind reaches the footer. Before
// it, three of the nineteen did, by bespoke paths beside the fault rather than
// derived from it; the other sixteen rode only the host stream Emacs
// subscribes to.
//
// THIS TABLE IS THE MAPPING, and it is the SAME table stated normatively in
// frontend/v1/footer.proto's FooterStatus header. It lives here because here
// is where faults are opened and closed, so a kind cannot be added to the
// vocabulary without a reviewer meeting the question of what the strip says
// about it.
//
// The rulings' three rules (2026-09-13, the domains of 2026-10-02):
//
//  1. STATUS IS THE FAULT'S DOMAIN: `agent_repl_fault` for the faults that
//     mean one of agent-repl's own services cannot serve the workspace (the
//     session cannot be reached, or the daemon cannot serve it);
//     `network_fault` for this machine being offline; `vendor_fault` for the
//     vendor that will not start while agent-repl serves. Precedence
//     agent_repl_fault > network_fault > vendor_fault, which is the ladder's.
//  2. SUBSTATUS IS A BUCKET, onto the activity values — several kinds per
//     bucket, no bucket minted to match one kind.
//  3. ACTIVITY IS THE LEAST GENERAL: the kind's own name and its detail line.
//
// NON-ESCALATING KINDS keep FaultStatusNone: the shim ANSWERED in every one of
// them, so the status is left exactly as it stands and the fault reaches the
// footer as the activity line alone. It is not only a wording question:
// `agent_repl_fault` closes the webapp's composer, so escalating a failed
// classifier run or an abandoned conversation would lock the user out of a
// session that is serving perfectly.

// FaultStatus is the footer STATUS a standing fault claims.
type FaultStatus string

// The claims a fault can make on the status cell, one per fault domain.
const (
	// FaultStatusNone is a NON-ESCALATING fault: the session is serving, so
	// the status stands unchanged and only the activity line is the fault's.
	FaultStatusNone FaultStatus = ""
	// FaultStatusAgentReplFault is a fault that means one of agent-repl's own
	// services cannot serve the session: it cannot be reached, or the daemon
	// cannot serve it.
	FaultStatusAgentReplFault FaultStatus = "agent_repl_fault"
	// FaultStatusNetworkFault is a fault that means this machine cannot reach
	// the network.
	FaultStatusNetworkFault FaultStatus = "network_fault"
	// FaultStatusVendorFault is a fault that means the vendor will not serve
	// while agent-repl does.
	FaultStatusVendorFault FaultStatus = "vendor_fault"
)

// The substatus buckets: footer.proto's FooterStatusAgentReplFault,
// FooterStatusNetworkFault and FooterStatusVendorFault steps.
const (
	// FaultSubStatusStartFailed is a session that never came up.
	FaultSubStatusStartFailed = "start_failed"
	// FaultSubStatusDead is a session whose process or session is gone.
	FaultSubStatusDead = "dead"
	// FaultSubStatusSevered is a link or a handle the two ends disagree about.
	FaultSubStatusSevered = "severed"
	// FaultSubStatusDaemonImpaired is the daemon owing the session a service
	// it cannot give.
	FaultSubStatusDaemonImpaired = "daemon_impaired"
	// FaultSubStatusOffline is this machine unable to reach the network.
	FaultSubStatusOffline = "offline"
	// FaultSubStatusVendorRetry is a vendor that did not start and is being
	// retried.
	FaultSubStatusVendorRetry = "vendor_retry"
	// FaultSubStatusVendorRejection is a vendor that refused the start.
	FaultSubStatusVendorRejection = "vendor_rejection"
	// FaultSubStatusVendorFailed is a vendor that failed to start for the
	// whole retry window.
	FaultSubStatusVendorFailed = "vendor_failed"
)

// KindColdGateReopenFailed is the cold-gate re-open that failed: the user
// answered a standing gate, the daemon carried the remediation out, and the
// session did not come back. It is a bring-up failure by every measure that
// matters to a reader, so it buckets with the other bring-up failures.
const KindColdGateReopenFailed = "cold_gate_reopen_failed"

// FaultCell is the (status, substatus) pair one fault kind claims.
type FaultCell struct {
	// Status is the footer status the fault claims, empty when it claims none.
	Status FaultStatus
	// SubStatus is the bucket within that status, empty when Status is empty.
	SubStatus string
}

// sessionFaultCells is the partition for a WORKSPACE-scoped fault.
var sessionFaultCells = map[string]FaultCell{
	KindShimStartFailed:       {FaultStatusAgentReplFault, FaultSubStatusStartFailed},
	KindResumeFailed:          {FaultStatusAgentReplFault, FaultSubStatusStartFailed},
	KindRelaunchResumeFailed:  {FaultStatusAgentReplFault, FaultSubStatusStartFailed},
	KindAdoptionWindowExpired: {FaultStatusAgentReplFault, FaultSubStatusStartFailed},
	KindColdGateReopenFailed:  {FaultStatusAgentReplFault, FaultSubStatusStartFailed},

	// A VENDOR THAT DID NOT START INSIDE A HEALTHY SHIM is a VENDOR FAULT,
	// never `start_failed`, which names the shim PROCESS (footer.proto,
	// FooterStatusAgentReplFault.substatus).
	KindVendorStartRetrying: {FaultStatusVendorFault, FaultSubStatusVendorRetry},
	KindVendorStartRejected: {FaultStatusVendorFault, FaultSubStatusVendorRejection},
	KindVendorStartFailed:   {FaultStatusVendorFault, FaultSubStatusVendorFailed},

	// THE NETWORK IS UNREACHABLE: the shim told a failure below any answer the
	// vendor could give apart from one the vendor gave.
	KindNetworkUnreachable: {FaultStatusNetworkFault, FaultSubStatusOffline},

	KindShimDied:      {FaultStatusAgentReplFault, FaultSubStatusDead},
	KindBounceDied:    {FaultStatusAgentReplFault, FaultSubStatusDead},
	KindSessionAbsent: {FaultStatusAgentReplFault, FaultSubStatusDead},

	KindLinkSevered:      {FaultStatusAgentReplFault, FaultSubStatusSevered},
	KindWatchOpenRefused: {FaultStatusAgentReplFault, FaultSubStatusSevered},

	KindStateUnreadable: {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},

	// NON-ESCALATING: the shim answered.
	KindShimReported:          {FaultStatusNone, ""},
	KindClassifierFailed:      {FaultStatusNone, ""},
	KindBounceUnknown:         {FaultStatusNone, ""},
	KindConversationAbandoned: {FaultStatusNone, ""},
	// NON-ESCALATING for the same reason and one more: the session is healthy
	// and the answer's prose is on screen. What is missing is the workspace's
	// ability to POINT AT it, and `agent_repl_fault` would close the composer over
	// a session that is serving perfectly. The three cases are told apart by
	// the fault's own `why` evidence, which the arm carries.
	KindFinalAnswerUnresolved: {FaultStatusNone, ""},
}

// daemonFaultCells is the partition for a DAEMON-scoped fault, which stands on
// every workspace's strip because it is every workspace that is owed the
// service. `adoption_window_expired` appears in both tables and means
// different things in each: a session's own bring-up that never happened, or
// this daemon's handover that nobody claimed.
var daemonFaultCells = map[string]FaultCell{
	KindPromptsDirMissing:     {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	KindWsmReadOnly:           {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	KindLogSinkPoisoned:       {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	KindSuccessorSpawnFailed:  {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	KindStateUnreadable:       {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	KindAdoptionWindowExpired: {FaultStatusAgentReplFault, FaultSubStatusDaemonImpaired},
	// NON-ESCALATING: the daemon that ran the deploy keeps serving on the
	// build it already runs. A failed build installed nothing and a failed
	// install restarted nothing, so `agent_repl_fault` would say the daemon cannot
	// serve a session it is serving.
	KindDeployFailed: {FaultStatusNone, ""},
}

// FaultFooterCell answers where one recorded fault lands on the strip, and
// whether it lands at all. `bounce_disposition` is the one kind that answers
// false: it is opened and closed in one breath as per-session accounting and
// never STANDS, which is the same reason the wire has no arm for it.
func FaultFooterCell(kind string, daemonScope bool) (FaultCell, bool) {
	table := sessionFaultCells
	if daemonScope {
		table = daemonFaultCells
	}
	cell, ok := table[kind]
	return cell, ok
}

// FaultLineDetail composes the ONE line the footer draws for a fault, out of
// the fault's own evidence, so the strip and the fault record cannot say
// different things about one failure.
//
// `shim_start_failed` keeps StartFailedDetail: its evidence is an exit code
// and a stderr ring, and the footer's richer start-failed leaf exists for it.
// `deploy_failed` keeps DeployFailedDetail, which names the step it failed at.
// Every other kind says what its `cause` or `detail` evidence says, and falls
// back to the record's own prose.
func FaultLineDetail(f wsm.Fault) string {
	switch f.Kind {
	case KindShimStartFailed:
		return StartFailedDetail(f)
	case KindDeployFailed:
		return DeployFailedDetail(f)
	case KindVendorStartRetrying, KindVendorStartRejected, KindVendorStartFailed:
		return VendorStartLine(f)
	}
	if cause := f.Evidence["cause"]; cause != "" {
		return cause
	}
	if detail := f.Evidence["detail"]; detail != "" {
		return detail
	}
	return f.Detail
}

// FaultTopbarLine answers the line the TOPBAR's warning strip — the webapp's
// one error surface — draws for a fault, and "" for a fault it does not
// carry. A failed deploy stands there as well as on every footer (owner
// ruling, 2026-09-28): it is daemon-scoped, so it lands on every strip. The
// line is the footer's own detail under the kind's name, so the two surfaces
// cannot say different things about one failure.
func FaultTopbarLine(f wsm.Fault, daemonScope bool) string {
	if !daemonScope || f.Kind != KindDeployFailed {
		return ""
	}
	return "deploy failed: " + DeployFailedDetail(f)
}

// FaultLine is one standing fault, as the footer needs it: which cell it
// claims, what it says, and when it began standing.
type FaultLine struct {
	// ID is the fault record's id, which is what closes it again.
	ID ids.FaultID
	// Kind is the fault kind, drawn in the activity cell.
	Kind string
	// Cell is where it lands on the strip.
	Cell FaultCell
	// Detail is the composed line.
	Detail string
	// At is when the fault was opened.
	At time.Time
	// Topbar is the line the topbar's warning strip draws for the fault,
	// empty when the topbar does not carry it (FaultTopbarLine).
	Topbar string
	// Record is the fault as it was recorded, for a surface that renders
	// more of it than the line (the topbar's overlay, Emacs's typed fault).
	Record wsm.Fault
}

// FaultSink is told about every fault the daemon opens and closes. It is the
// ONE hook the footer — and, for the faults it carries, the topbar — hangs off: the ruling asks that every fault reach the
// strip, and per-site plumbing beside each raise is exactly how three kinds
// came to have a footer path and sixteen did not.
//
// A DAEMON-SCOPED fault is delivered with an EMPTY workspace, which the sink
// reads as "every workspace".
type FaultSink interface {
	// FaultOpened states a fault that now stands.
	FaultOpened(ws ids.WorkspaceID, line FaultLine)
	// FaultClosed retracts one by id.
	FaultClosed(ws ids.WorkspaceID, id ids.FaultID)
}

// observedDB is the wsm.DB every fault write passes through, so that opening
// or closing a fault ANYWHERE reaches the sink. It decorates rather than
// replaces: every other method is the wrapped store's.
type observedDB struct {
	wsm.DB
	sink FaultSink
	log  dlog.Surfaces

	mu sync.Mutex
	// opened remembers which workspace each open fault belongs to, because
	// CloseFault is given an id alone and the sink is per-workspace.
	opened map[ids.FaultID]ids.WorkspaceID
}

// ObserveFaults wraps a state client so every fault it opens and closes is
// reported to the sink. Nothing else about the client changes.
func ObserveFaults(db wsm.DB, sink FaultSink, log dlog.Surfaces) wsm.DB {
	if db == nil || sink == nil || log == nil {
		return db
	}
	return &observedDB{DB: db, sink: sink, log: log, opened: map[ids.FaultID]ids.WorkspaceID{}}
}

// OpenFault records the fault and tells the sink where it lands.
func (o *observedDB) OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error) {
	id, err := o.DB.OpenFault(ctx, f)
	if err != nil {
		return id, err
	}
	var ws ids.WorkspaceID
	if f.Workspace != nil {
		ws = *f.Workspace
	}
	cell, drawn := FaultFooterCell(f.Kind, f.Workspace == nil)
	o.mu.Lock()
	o.opened[id] = ws
	o.mu.Unlock()
	if !drawn {
		o.log.Global().Debug(opOpenFault, "the fault kind has no footer cell and stands on no strip",
			dlog.Context{"fault": string(id), "kind": f.Kind})
		return id, nil
	}
	o.sink.FaultOpened(ws, FaultLine{
		ID:     id,
		Kind:   f.Kind,
		Cell:   cell,
		Detail: FaultLineDetail(f),
		At:     f.OpenedAt,
		Topbar: FaultTopbarLine(f, f.Workspace == nil),
		Record: f,
	})
	return id, nil
}

// CloseFault closes the fault and retracts it from the sink.
func (o *observedDB) CloseFault(ctx context.Context, id ids.FaultID, at time.Time) error {
	if err := o.DB.CloseFault(ctx, id, at); err != nil {
		return err
	}
	o.mu.Lock()
	ws, known := o.opened[id]
	delete(o.opened, id)
	o.mu.Unlock()
	if !known {
		// A FAULT THIS PROCESS DID NOT OPEN. It was recorded by a previous
		// daemon and closed by this one, so no strip is drawing it and there
		// is nothing to retract. Recorded rather than silently dropped: a
		// standing fault the footer never learned about would look like this.
		o.log.Global().Debug(opCloseFault, "closed a fault this process never opened; no strip carries it",
			dlog.Context{"fault": string(id)})
		return nil
	}
	o.sink.FaultClosed(ws, id)
	return nil
}
