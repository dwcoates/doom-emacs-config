package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessionwatcher"
)

// standing is one composed line plus the instant it began standing. Every
// activity kind the footer draws is one of these: the contract ships an
// instant and the client ticks the relative age.
type standing struct {
	// text is the composed line, drawn verbatim.
	text string
	// at is when the line began standing.
	at time.Time
}

// allowanceWindow is ONE drawn allowance's evidence, assembled from two
// different vendor facts per the project lead's sourcing ruling:
//
//   - the FIGURES (utilization, reset) come from SessionUpdate.account_usage,
//     the sampled account usage, which is complete from its first sample;
//   - the VERDICT (the typed status arm) comes from
//     SessionUpdate.rate_limit_status, the vendor's rate-limit event, and
//     stays UNSET until an event for this window has been seen. An unset
//     status oneof is legal and means "no vendor verdict observed yet" — it
//     is never defaulted to "allowed".
//
// A rate-limit event that carries a utilization ALSO supplies the figure, and
// the LAST sighting to arrive is the one drawn. See `fileFigures` for why
// arrival, and not a timestamp, is what orders the two sources.
type allowanceWindow struct {
	// figured reports whether any figure has been observed for this window.
	figured bool
	// utilization is the drawn fraction, 0..1.
	utilization float64
	// resetsAtS is the drawn reset, epoch SECONDS.
	resetsAtS int64
	// sampledAtMs is SessionAccountUsage.observed_at_ms of the newest usage
	// SAMPLE filed for this window, 0 before any sample. It is only ever
	// compared with another sample's, because only samples carry it and only
	// samples are stamped by the shim's clock.
	sampledAtMs int64
	// verdict is the last rate-limit event for this window, nil until one has
	// been seen. Only its status arm is read.
	verdict *conversationv1.SessionRateLimitStatus
}

// observeSampledFigures files a figure sighting read off a usage SAMPLE,
// which carries its own observation instant.
//
// A sample OLDER than the newest sample already on hand is refused. That
// comparison is sound where a cross-source one is not: both stamps are
// SessionAccountUsage.observed_at_ms, so both come from the one shim process's
// clock.
func (w *allowanceWindow) observeSampledFigures(utilizationPercent float64, resetsAtMs, observedAtMs int64) bool {
	if w.sampledAtMs != 0 && observedAtMs < w.sampledAtMs {
		return false
	}
	w.sampledAtMs = observedAtMs
	return w.fileFigures(utilizationPercent, resetsAtMs)
}

// observeEventFigures files a figure sighting read off a rate-limit EVENT.
// SessionRateLimitStatus carries no observation instant at all, so an event is
// ordered by nothing but its arrival.
func (w *allowanceWindow) observeEventFigures(utilizationPercent float64, resetsAtMs int64) bool {
	return w.fileFigures(utilizationPercent, resetsAtMs)
}

// fileFigures draws the sighting it is given, unconditionally: THE LAST
// SIGHTING TO ARRIVE WINS.
//
// This used to compare an `atMs` per sighting and keep the newer, which was
// not a valid ordering. A usage sample's stamp is
// SessionAccountUsage.observed_at_ms, taken by the SHIM; a rate-limit event
// has no stamp in the contract at all (SessionRateLimitStatus declares none),
// so the daemon substituted its own clock at receipt. Two processes' clocks
// are not one timeline, and the shim's stamp is taken before the update has
// crossed the pipe, so a sample that genuinely arrived later routinely lost to
// an event that arrived first — the footer then drew a retired allowance and
// kept drawing it.
//
// Arrival IS a valid total order here, and it is the producer's own order:
// account_usage and rate_limit_status are two arms of ONE SessionUpdate
// stream from ONE shim, and sessionwatcher routes that stream to this sink
// serially under its lock (internal/sessionwatcher/route.go). So the sighting
// that arrives last is the sighting the shim emitted last, with no clock read
// on either side.
//
// The vendor reports utilization as a percentage and resets in millis; the
// contract carries a 0..1 fraction and epoch seconds, so the conversion is the
// daemon's and never the client's.
func (w *allowanceWindow) fileFigures(utilizationPercent float64, resetsAtMs int64) bool {
	w.figured = true
	w.utilization = utilizationPercent / 100
	w.resetsAtS = resetsAtMs / 1000
	return true
}

// rateState is the two drawn allowances. The five-hour window is the session
// allowance and the seven-day window (with its per-model and overage-included
// aliases) is the weekly one.
type rateState struct {
	// session is the rolling five-hour allowance.
	session allowanceWindow
	// weekly is the seven-day allowance.
	weekly allowanceWindow
	// at is when the newest evidence for either window was observed.
	at time.Time
}

// hookState is a hook running right now.
type hookState struct {
	// name is the hook's configured name.
	name string
	// at is when it fired.
	at time.Time
}

// retryState is a vendor call being retried mid-turn.
type retryState struct {
	// attempt is which attempt is running.
	attempt int32
	// status is the vendor's summary of the failure being retried.
	status string
	// at is when the retry evidence arrived.
	at time.Time
}

// wakeupState is a pending self-scheduled wakeup.
type wakeupState struct {
	// wakeAt is when the wakeup fires.
	wakeAt time.Time
	// reason is the agent's sentence, empty when it gave none.
	reason string
	// at is when the schedule was observed.
	at time.Time
}

// blockedKind names why the session cannot proceed.
type blockedKind int

// The blocked kinds, one per FooterStatusBlocked substatus arm.
const (
	blockedAuth blockedKind = iota
	blockedUsageLimit
	blockedVendorError
	blockedBilling
	blockedQueryDied
)

// blockedState is a standing block plus the line it draws.
type blockedState struct {
	// kind is which substatus arm stands.
	kind blockedKind
	// line is the composed activity line, empty when the step has none.
	line string
	// at is when the block was observed.
	at time.Time
}

// interruptedKind names who stopped the turn.
type interruptedKind int

// The interrupted kinds.
const (
	interruptedByUser interruptedKind = iota
	interruptedByHostShutdown
)

// interruptedState is the MOMENTARY interrupted status, retired by the R1
// dwell.
type interruptedState struct {
	// kind is who stopped it.
	kind interruptedKind
	// at is when the stop landed.
	at time.Time
}

// loadingKind names what kind of context injection is running.
type loadingKind int

// The loading kinds, one per FooterStatusLoading substatus arm.
const (
	loadingMemory loadingKind = iota
	loadingInvoked
	loadingDiscovered
	loadingListing
)

// loadingState is the MOMENTARY loading status, retired by the R1 dwell.
type loadingState struct {
	// kind is which injection kind is running.
	kind loadingKind
	// line is the composed item line, which the status REQUIRES.
	line string
	// at is when the injection was observed.
	at time.Time
}

// agentRow is one live agent-spawned subagent with a feed bubble.
type agentRow struct {
	// spawnUnit is the calling agent's activity id for the spawn — half of the
	// bubble's FeedId.
	spawnUnit string
	// createdAgent is the agent the spawn created — the other half.
	createdAgent string
	// label is the subagent type drawn as the row's leading label.
	label string
	// description is the commission's description, empty when none was given.
	description string
	// tokens is the run's running token sum.
	tokens uint64
	// startedAt is when the run began; the ORIGINAL instant, never reset.
	startedAt time.Time
	// order is the spawn order the panel draws in.
	order int
}

// shellRow is one live detached shell.
type shellRow struct {
	// work is the detached work handle, which is the bubble's FeedId key.
	work string
	// command is the command line being run.
	command string
	// startedAt is when the command began; the ORIGINAL instant.
	startedAt time.Time
	// order is the announcement order the panel draws in.
	order int
}

// monitorRow is one live background monitor. Monitors have no feed bubble, so
// the row is not a jump target.
type monitorRow struct {
	// unit is the monitor's activity id, which keys the row.
	unit string
	// description is what is being watched.
	description string
	// persistent reports whether the watch outlives its deadline.
	persistent bool
	// startedAt is when the watch was armed.
	startedAt time.Time
	// order is the arming order the panel draws in.
	order int
}

// taskRow is one tracker task as it currently stands.
type taskRow struct {
	// id is the tracker's identity for the task.
	id string
	// subject is the task's subject line.
	subject string
	// status is the projected checklist status.
	status taskStatus
	// activeForm is the running phrasing, empty when the agent gave none.
	activeForm string
	// order is the tracker order the panel draws in.
	order int
}

// taskStatus is the checklist projection of a tracker status.
type taskStatus int

// The projected task statuses. The tracker's `deleted` status is not one of
// them: a deleted task leaves the list rather than drawing a fourth glyph.
const (
	taskPending taskStatus = iota
	taskRunning
	taskCompleted
)

// cronRow is one scheduled job as the last listing (or a create) stated it.
type cronRow struct {
	// id is the vendor's opaque job id.
	id string
	// cron is the cron expression, verbatim.
	cron string
	// humanSchedule is the vendor's wording of the schedule.
	humanSchedule string
	// prompt is the prompt the job fires.
	prompt string
	// recurring reports whether the job repeats.
	recurring bool
	// durable reports whether the job survives the session.
	durable bool
	// order is the order the panel draws in.
	order int
}

// wsState is one workspace's whole footer accumulation. It is in-memory only:
// a resolver aggregates, it never stores.
type wsState struct {
	// dir is the workspace directory, bound before any frame arrives.
	dir string
	// log is the workspace-bound logger, nil until the directory is bound.
	log dlog.Logger

	// seen reports whether any fact has been observed — the readiness gate.
	seen bool

	// link is the last observed daemon-to-shim link state.
	link sessionwatcher.LinkState
	// linkSeen reports whether any link state has been observed at all.
	linkSeen bool
	// everConnected distinguishes a shim that died from one that never started.
	everConnected bool
	// hostStream and webStream are the OTHER TWO HOPS of connectivity truth
	// (daemon.md invariant 11): the workspace is connected only while its
	// shim.v1 WatchSession, its WatchHostWorkspace and its WatchWebWorkspace
	// are all live. The server states these two on every stream open and close
	// edge; neither is ever inferred from silence.
	hostStream bool
	webStream  bool
	// degraded reports an open degraded window on the last diagnostics push.
	degraded bool

	// turn is the accepted turn in flight, nil when the main thread is idle.
	turn *TurnStarted
	// turnEverRan distinguishes idle·ready from idle·done.
	turnEverRan bool
	// sawActivity reports whether this turn has produced an activity yet,
	// which is what moves `submitting` to `thinking`.
	sawActivity bool
	// compacting reports a vendor-initiated auto-compaction in flight.
	compacting bool

	// interrupting is the registered-interrupt flag SetInterrupting installs.
	interrupting bool
	// coldGate is the standing cold-context gate.
	coldGate ColdGate
	// permissions are the open consent asks, by permission id.
	permissions map[string]string
	// permissionOrder is the order they were opened in.
	permissionOrder []string
	// questions are the open question batches' composed leads, by question id.
	questions map[string]string
	// questionOrder is the order they were opened in.
	questionOrder []string
	// wakeup is the pending self-scheduled wakeup, nil when none is.
	wakeup *wakeupState

	// merge is what the merge orchestrator last told the footer.
	merge MergeFacts
	// closing is the standing close refusal, nil when no close is blocked.
	closing *CloseBlocked
	// blocked is the standing block, nil when nothing blocks the session.
	blocked *blockedState

	// interrupted is the momentary interrupted status, nil when it is not
	// standing.
	interrupted *interruptedState
	// loading is the momentary loading status, nil when it is not standing.
	loading *loadingState
	// momentary cancels an in-flight R1 dwell when its status is superseded.
	momentary Timer

	// notification is the standing agent notification.
	notification *standing
	// contextBudget is the vendor's standing context-budget warning.
	contextBudget *standing
	// rate is the vendor's last rate-limit status per window.
	rate rateState
	// hook is the hook running right now, nil between hooks.
	hook *hookState
	// retrying is the standing mid-turn retry evidence.
	retrying *retryState
	// injected is the standing context-injection line.
	injected *standing
	// blockedOnUser is the vendor's requires_action detail.
	blockedOnUser *standing
	// authLine is the standing auth prompt line.
	authLine *standing
	// mergingCommit is the commit a merge is landing right now.
	mergingCommit *mergingCommit

	// tok is the turn's token accumulation.
	tok tokenState

	// agents are the live subagents with feed bubbles, by spawn unit.
	agents map[string]*agentRow
	// shells are the live detached shells, by work id.
	shells map[string]*shellRow
	// monitors are the live monitors, by activity id.
	monitors map[string]*monitorRow
	// tasks are the tracker's tasks, by task id.
	tasks map[string]*taskRow
	// crons are the scheduled jobs, by job id.
	crons map[string]*cronRow
	// bashUnits are the in-turn shell commands seen this session, by activity
	// id, so a shell that DETACHES from a unit can be described from the unit
	// it detached from (the announcement carries no command of its own).
	bashUnits map[string]*shellRow
	// seq mints the panel orders so a row's place is its arrival order.
	seq int
}

// mergingCommit is the commit a merge is landing right now.
type mergingCommit struct {
	// sha is the commit's abbreviated sha.
	sha string
	// subject is the commit's subject line.
	subject string
	// at is when it began landing.
	at time.Time
}

// newWSState builds an empty accumulation.
func newWSState() *wsState {
	return &wsState{
		permissions: map[string]string{},
		questions:   map[string]string{},
		agents:      map[string]*agentRow{},
		shells:      map[string]*shellRow{},
		monitors:    map[string]*monitorRow{},
		tasks:       map[string]*taskRow{},
		crons:       map[string]*cronRow{},
		bashUnits:   map[string]*shellRow{},
		tok:         newTokenState(),
	}
}

// nextOrder mints the next panel order.
func (s *wsState) nextOrder() int {
	s.seq++
	return s.seq
}
