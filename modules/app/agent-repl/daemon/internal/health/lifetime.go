package health

// EVERY FAULT KIND DECLARES ITS LIFETIME, HERE, IN ONE TABLE (owner-approved
// plan docs/investigations/2026-09-27-footer-fault-lifetimes-plan.md).
//
// A recorded fault stands on the footer, the host view and the health answers
// until something closes it. Before this table, only some kinds had a closing
// edge: a healthy attach closed a hand-listed pair (`linkFaultKinds`), a started
// session closed `resume_failed` by a special case, and `bounce_unknown` and
// `adoption_window_expired` had no closer at all, so their lines stood until
// the next daemon restart whether or not the workspace had recovered.
//
// A kind is either:
//
//   - STANDING until one of its named recovery EDGES: the edge is a moment in
//     the daemon's life (a healthy attach, a started session, the next turn)
//     that proves the condition the fault records is over; or
//   - MOMENTARY: an event report closed the instant it is recorded, or an
//     observation the reporter derives per answer and never records at all.
//
// A recovery edge closes EVERY standing fault whose declared edge it is,
// through CloseOnEdge (faultclose.go). No call site lists kinds by hand: a
// new kind is added here, with its lifetime, or the guard test fails.

// Edge names one recovery edge: a moment that proves a standing fault's
// condition is over.
type Edge string

// The recovery edges. Each is fired by the call sites that observe it, through
// CloseOnEdge or CloseFaultOn, and the guard test fails an edge with none.
const (
	// EdgeRecorded is a MOMENTARY kind's own edge: the fault is closed the
	// instant it is recorded (RecordMomentary).
	EdgeRecorded Edge = "recorded"
	// EdgeHealthyAttach is the daemon attaching a workspace's shim healthy: a
	// bring-up that reached the shim's first healthy diagnostics, a boot's or
	// a successor's adoption of a surviving shim, a relaunch's installed
	// replacement.
	EdgeHealthyAttach Edge = "healthy_attach"
	// EdgeSessionStarted is a StartSession the shim served: the workspace has
	// a live session again.
	EdgeSessionStarted Edge = "session_started"
	// EdgeTurnStarted is the next turn beginning on the workspace.
	EdgeTurnStarted Edge = "turn_started"
	// EdgeAnswerArrived is a response frame on the fold that went silent, or
	// the turn's terminal, which answers every fold at once.
	EdgeAnswerArrived Edge = "answer_arrived"
	// EdgeFeedReset is the workspace's feed being reset to a new
	// conversation: a fault about the old one's turn speaks for nothing.
	EdgeFeedReset Edge = "feed_reset"
	// EdgeSuperseded is a newer fault of the same kind and subject replacing
	// the standing one.
	EdgeSuperseded Edge = "superseded"
	// EdgeShimDeathRecorded is a shim death recorded on the workspace: the
	// severing recorded moments earlier is a CONSEQUENCE of the death.
	EdgeShimDeathRecorded Edge = "shim_death_recorded"
	// EdgeShimDiagnostics is a diagnostics push from the shim: the push is the
	// WHOLE verdict, so it replaces every shim-reported fault standing.
	EdgeShimDiagnostics Edge = "shim_diagnostics"
	// EdgePromptsDirServed is a composed brief read and spliced from the
	// prompts directory: that read is the health probe.
	EdgePromptsDirServed Edge = "prompts_dir_served"
	// EdgeSuccessorServing is a handover's successor proving it serves.
	EdgeSuccessorServing Edge = "successor_serving"
	// EdgeDeployStepSucceeded is a deploy getting through the step a
	// `deploy_failed` fault names.
	EdgeDeployStepSucceeded Edge = "deploy_step_succeeded"
	// EdgeDaemonBoot is a daemon that owns its state booting: what an earlier
	// process recorded about its OWN build, handle or sinks is over, and no
	// strip of the new process draws it.
	EdgeDaemonBoot Edge = "daemon_boot"
)

// Lifetime is how long one fault kind stands.
type Lifetime struct {
	// Momentary is a kind that never stands: it is closed the instant it is
	// recorded, or it is never recorded at all.
	Momentary bool
	// Edges are the recovery edges that close a STANDING kind. Empty for a
	// momentary one, which closes on EdgeRecorded alone.
	Edges []Edge
}

// momentary is the lifetime of a kind that never stands.
var momentary = Lifetime{Momentary: true}

// standing is the lifetime of a kind that stands until one of edges.
func standing(edges ...Edge) Lifetime { return Lifetime{Edges: edges} }

// faultLifetimes is THE table. Every kind in the vocabulary has exactly one
// row, whatever scope it is opened at.
var faultLifetimes = map[string]Lifetime{
	// DAEMON scope.
	//
	// A handover nobody claimed ends when a successor proves it is serving;
	// the SESSION-scoped one (the incumbent took the workspace back) ends when
	// the workspace next attaches or starts healthy.
	KindAdoptionWindowExpired: standing(EdgeHealthyAttach, EdgeSessionStarted, EdgeSuccessorServing),
	// A poisoned sink and a read-only handle are this PROCESS's; the next
	// process opens its own.
	KindLogSinkPoisoned: standing(EdgeDaemonBoot),
	KindWsmReadOnly:     standing(EdgeDaemonBoot),
	// A successor that would not start ends when one proves it is serving,
	// when a later failed handover records its own (the new fault supersedes
	// it, so failures never pile up), or at the next boot: the fault is about
	// the PROCESS that could not hand over, and a fresh daemon is not it.
	KindSuccessorSpawnFailed: standing(EdgeSuccessorServing, EdgeSuperseded, EdgeDaemonBoot),
	// A missing prompts directory ends at the next brief it furnishes.
	KindPromptsDirMissing: standing(EdgePromptsDirServed),
	// A failed deploy step ends when a later deploy gets through that step,
	// or fails it again (the new fault supersedes it), or at the next boot,
	// whose process draws no fault an earlier one opened.
	KindDeployFailed: standing(EdgeDeployStepSucceeded, EdgeSuperseded, EdgeDaemonBoot),

	// SESSION scope.
	//
	// A shim that would not come up, died, or lost its link ends at the next
	// healthy attach; a severing also ends when the death it was a
	// consequence of is recorded.
	KindShimStartFailed:  standing(EdgeHealthyAttach),
	KindShimDied:         standing(EdgeHealthyAttach),
	KindLinkSevered:      standing(EdgeHealthyAttach, EdgeShimDeathRecorded),
	KindWatchOpenRefused: standing(EdgeHealthyAttach),
	// A refused session start ends at the next session the shim serves.
	KindResumeFailed:         standing(EdgeSessionStarted),
	KindRelaunchResumeFailed: standing(EdgeSessionStarted),
	KindColdGateReopenFailed: standing(EdgeSessionStarted),
	// A vendor that did not start ends at the next session the shim serves.
	// The retrying one is also closed by its own run as each attempt
	// replaces it, and as the run gives way to a rejection or exhaustion.
	KindVendorStartRetrying: standing(EdgeSessionStarted),
	KindVendorStartRejected: standing(EdgeSessionStarted),
	KindVendorStartFailed:   standing(EdgeSessionStarted),
	// An undetermined bounce ends when the workspace next attaches or starts
	// healthy.
	KindBounceUnknown: standing(EdgeHealthyAttach, EdgeSessionStarted),
	// A session the bounce meant to preserve whose lock reads free is
	// recorded RESOLVED: the free lock proves the process owns nothing, and
	// the workspace takes the ordinary dead-shim path.
	KindBounceDied: momentary,
	// An ordinary reconciled bounce disposition is per-session accounting.
	KindBounceDisposition: momentary,
	// A shim-reported fault is replaced by the shim's next verdict.
	KindShimReported: standing(EdgeShimDiagnostics),
	// So is the network fault the shim reported: its next verdict says
	// whether the network is still unreachable.
	KindNetworkUnreachable: standing(EdgeShimDiagnostics),
	// A failed classifier run and an abandoned conversation are statements
	// about the conversation as it stood; the next turn moves past them.
	KindClassifierFailed:      standing(EdgeTurnStarted),
	KindConversationAbandoned: standing(EdgeTurnStarted),
	// A turn whose answer did not land ends when the answer arrives, the next
	// turn starts, a later final-answer fault supersedes it, or the feed is
	// reset to another conversation.
	KindFinalAnswerUnresolved: standing(EdgeAnswerArrived, EdgeTurnStarted, EdgeSuperseded, EdgeFeedReset),
	// The reporter's OWN observations, derived per answer and never recorded:
	// they cannot stand, because there is no record to stand.
	KindSessionAbsent:   momentary,
	KindStateUnreadable: momentary,
}

// FaultLifetime answers one kind's declared lifetime, false for a kind the
// table does not declare.
func FaultLifetime(kind string) (Lifetime, bool) {
	lifetime, ok := faultLifetimes[kind]
	return lifetime, ok
}

// ClosesOn reports whether edge is one of kind's declared closing edges. A
// momentary kind closes on EdgeRecorded alone; an undeclared kind closes on
// nothing.
func ClosesOn(kind string, edge Edge) bool {
	lifetime, ok := faultLifetimes[kind]
	if !ok {
		return false
	}
	if lifetime.Momentary {
		return edge == EdgeRecorded
	}
	for _, e := range lifetime.Edges {
		if e == edge {
			return true
		}
	}
	return false
}

// Momentary reports whether kind is declared momentary.
func Momentary(kind string) bool {
	lifetime, ok := faultLifetimes[kind]
	return ok && lifetime.Momentary
}
