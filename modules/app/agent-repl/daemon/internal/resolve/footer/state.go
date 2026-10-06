package footer

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
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
	// verdict is the status arm of the last rate-limit event for this window,
	// VerdictNone until one with a status has been seen.
	verdict wsm.AllowanceVerdict
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

// rateState is the drawn allowances. The five-hour window is the session
// allowance, the seven-day window (with its per-model and overage-included
// aliases) is the weekly one, and the vendor's own overage window is the
// third.
type rateState struct {
	// session is the rolling five-hour allowance.
	session allowanceWindow
	// weekly is the seven-day allowance.
	weekly allowanceWindow
	// overage is the allowance the vendor bills beyond the two above. It
	// stays unfigured — and so draws absent — on the accounts that never
	// report an overage window at all.
	overage allowanceWindow
	// at is when the newest evidence for any window was observed.
	at time.Time
}

// retryState is a vendor call being retried mid-turn.
type retryState struct {
	// agent is the agent whose call is being retried: only its own response
	// ends the line.
	agent string
	// attempt is which attempt is running.
	attempt int32
	// status is the vendor's summary of the failure being retried.
	status string
	// at is when the retry evidence arrived.
	at time.Time
	// nextAt is when the vendor said the next attempt starts, nil when it
	// stated no schedule.
	nextAt *time.Time
	// maxAttempt is the last attempt the vendor will make, zero when it stated
	// no schedule.
	maxAttempt int32
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
	// work is the handle the run is addressed by once it has DETACHED, empty
	// while the spawn is still the turn's own progress.
	//
	// It is what makes the row retirable BY ITS OWN IDENTITY: a detached run's
	// terminal is addressed to the work, on whichever stream carries it, and a
	// row with a handle is no longer retired by the spawning call's stream
	// alone. See chips.go OnSubagent.
	work string
	// provenance names what FIRST described this row, so the record a jump
	// writes can say what a row is from the log alone. See rowProvenance.
	provenance rowProvenance
	// spawnedOn is the agent whose stream carried the spawn frame that opened
	// the row, empty when no spawn frame did (an announcement, the live set).
	spawnedOn string
	// jump is the click resolution last recorded for this row.
	jump jumpMemo
}

// rowProvenance is what first described a detached-work row.
type rowProvenance string

// The provenances a row can have.
const (
	// provenanceSpawnFrame: the spawn's own start frame, on the calling
	// agent's stream.
	provenanceSpawnFrame rowProvenance = "spawn_frame"
	// provenanceRunFrame: a frame addressed to the run's detached handle.
	provenanceRunFrame rowProvenance = "detached_run_frame"
	// provenanceAnnouncement: a created-work announcement.
	provenanceAnnouncement rowProvenance = "created_announcement"
	// provenanceLiveWorkSet: the watcher's live-work set listed an id no row
	// stood for, so a MINIMAL row was opened (label "subagent", no tokens, the
	// clock from the instant the footer learned of it) while its descriptive
	// frame was on its way — or never came.
	provenanceLiveWorkSet rowProvenance = "live_work_set"
	// provenanceDetachedAnnouncement: a `detached` announcement named a
	// subagent the footer drew no row for under the unit it detached from --
	// a subagent RESUMED BY A SEND, whose unit is the send -- and the footer
	// had never described that agent either (a daemon that came up after the
	// launch). The announcement's commission describes it (bindDetachedAgent).
	provenanceDetachedAnnouncement rowProvenance = "detached_announcement"
	// provenanceResumeWait: a network-resume wait named work the footer never
	// described, so a MINIMAL row stands for the wait (netresume.go).
	provenanceResumeWait rowProvenance = "network_resume_wait"
)

// jumpMemo is the click resolution last recorded for one row, so the record is
// written when the resolution CHANGES rather than on every push.
type jumpMemo struct {
	recorded bool
	value    string
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
	// jump is the click resolution last recorded for this row.
	jump jumpMemo
}

// monitorRow is one live background monitor. Its jump names the Monitor
// call's tool-call card, which the feed announces by the monitor's unit.
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
	// jump is the click resolution last recorded for this row.
	jump jumpMemo
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

// startFailedState is one standing bring-up failure: why it failed and when
// it began standing. A failed bring-up drops no held prompt (they stay held
// under the reconnect hold), so there is no cost to count.
type startFailedState struct {
	// detail is the composed cause, drawn verbatim.
	detail string
	// at is when the failure began standing.
	at time.Time
	// announced reports that the info record for this failure's line has
	// already been written, so the line is recorded ONCE rather than on every
	// republication of a view that keeps drawing it.
	announced bool
}

// wsState is one workspace's whole footer accumulation. It is in-memory only:
// a resolver aggregates, it never stores.
type wsState struct {
	// id is the workspace this accumulation belongs to, stated once at its
	// creation. It is what a resolver-wide fact keyed by workspace (a
	// deploy's per-workspace notes) is read against.
	id ids.WorkspaceID
	// dir is the workspace directory, bound before any frame arrives.
	dir string
	// log is the workspace-bound logger, nil until the directory is bound.
	log dlog.Logger
	// unboundReported latches that a record for this workspace already
	// arrived unbound and the invariant violation was stated at ERROR.
	unboundReported bool

	// seen reports whether any fact has been observed — the readiness gate.
	seen bool

	// link is the last observed daemon-to-shim link state.
	link sessionwatcher.LinkState
	// linkSeen reports whether any link state has been observed at all.
	linkSeen bool
	// everConnected distinguishes a shim that died from one that never started.
	everConnected bool
	// parked reports that the idle sweep stood this workspace's shim down on
	// purpose and recorded the `hibernated` session terminal. It is the SAME
	// fact the roster keys its idle arm on (resolve/sidebar/status.go's
	// `parked`), handed to both surfaces from the one site that writes the
	// terminal, so the dot and the strip cannot disagree about the same park.
	//
	// It is cleared by the next link state of ANY kind: the watcher latches a
	// dead link and publishes nothing further on it, so the next OnLink this
	// workspace sees belongs to the revival's own spawn, and a spawn that then
	// FAILS must read `dead` like any other.
	parked bool
	// hostStream and webStream are the OTHER TWO HOPS of connectivity truth
	// (daemon.md invariant 11): the workspace is connected only while its
	// shim.v1 WatchSession, its WatchHostWorkspace and its WatchWebWorkspace
	// are all live. The server states these two on every stream open and close
	// edge; neither is ever inferred from silence.
	hostStream bool
	webStream  bool
	// degraded reports an open degraded window on the last diagnostics push.
	degraded bool
	// stateUnreported reports a shim this daemon took back after a failed
	// handover that has not re-reported its session state. It stands from
	// the take-back's bounded wait running out until the next session start.
	stateUnreported bool

	// turn is the accepted turn in flight, nil when the main thread is idle.
	turn *TurnStarted
	// sessionStarted reports that the session has announced itself, which on
	// a route never seen is the bring-up window (ladder.AwaitingBringUp).
	sessionStarted bool
	// turnEverRan distinguishes idle·ready from idle·done.
	turnEverRan bool
	// pendingEnding is how the turn in flight failed, recorded from its
	// terminal or the query's death, for the turn's close to raise its fault
	// from (turnfault.go). The next turn resets it.
	pendingEnding *pendingEnding
	// turnFault is the standing TURN FAULT the last turn's close raised,
	// nil when none stands. The next turn resets it (owner ruling,
	// 2026-10-06: a turn fault stands until the next turn starts).
	turnFault *turnFaultState
	// turnRefused reports that a response in the turn in flight ended on the
	// vendor's refusal: the refusal's only witness
	// (turnfault.RefusedResponse). The next turn resets it.
	turnRefused bool
	// sawActivity reports whether this turn has produced an activity yet,
	// which is what moves `submitting` to a working step.
	sawActivity bool
	// mainAgent is the session's main agent as the watcher named it, empty
	// until named. Only its items name the working step.
	mainAgent string
	// motion is the feed's items as the working step reads them
	// (workstep.go).
	motion feedMotion
	// compacting reports a vendor-initiated auto-compaction in flight.
	compacting bool
	// compaction is the compaction's own progress line, from whichever
	// producer is compacting — the vendor's auto-compaction or the cold gate's
	// answered remediation. Nil when nothing is compacting. It is only ever
	// stood and ended through compaction.go's standCompaction/endCompaction,
	// which bind it to the act it narrates.
	compaction *standing
	// concludedCompactions are the identities of the latest compactions whose
	// cut this footer took, newest last (at most maxConcludedCompactions). A
	// compaction's start signal and its cut travel on different streams with
	// no order between them, so a start signal can arrive AFTER its own cut;
	// one naming a concluded compaction is stale, never a new compaction.
	concludedCompactions []string

	// interrupting is the registered-interrupt flag SetInterrupting installs.
	interrupting bool
	// interruptingAt is when the interrupt registered: the instant its
	// salient line began standing.
	interruptingAt time.Time
	// coldGate is the standing cold-context gate.
	coldGate ColdGate
	// coldGateAt is when the standing gate began standing.
	coldGateAt time.Time
	// coldAnswer is the gate answer in flight, nil when none is being spent.
	// It OUTRANKS coldGate: the gate stays standing until the re-open lands,
	// and drawing the question over the answer is what made the answer look
	// like it had done nothing.
	coldAnswer *ColdGateAnswer
	// permissions are the open consent asks' composed lines and the instant
	// each opened, by permission id.
	permissions map[string]standing
	// permissionOrder is the order they were opened in.
	permissionOrder []string
	// questions are the open question batches' composed leads and the instant
	// each opened, by question id.
	questions map[string]standing
	// questionOrder is the order they were opened in.
	questionOrder []string
	// wakeup is the pending self-scheduled wakeup, nil when none is.
	wakeup *wakeupState

	// merge is what the merge orchestrator last told the footer.
	merge MergeFacts
	// closing is the standing close refusal, nil when no close is blocked.
	closing *CloseBlocked
	// closingAt is when the standing refusal began standing.
	closingAt time.Time
	// startFailed is the standing bring-up failure, nil when none stands. It
	// is installed by the site that opens the `shim_start_failed` fault and
	// cleared by the next successful link edge.
	startFailed *startFailedState
	// faults are this workspace's OWN standing faults, in the order they were
	// opened. The strongest of them, folded together with the resolver's
	// daemon-scoped ones, is what the strip draws.
	faults []Fault
	// blocked is the standing VENDOR OR ACCOUNT block, nil when nothing
	// blocks the session. It is raised and lifted by exactly the facts the
	// roster's vendor_blocked is (ladder.ClassifyFailure, ladder.RateLimitBlocks,
	// a session start, a turn start), so the strip and the dot move together.
	blocked *blockedState

	// interrupted is the momentary interrupted status, nil when it is not
	// standing.
	interrupted *interruptedState
	// loading is the momentary loading status, nil when it is not standing.
	loading *loadingState
	// momentary cancels an in-flight R1 dwell when its status is superseded.
	momentary Timer

	// transient is THE NEWEST TRANSIENT: the one slot every transient source
	// fills, each raise replacing the last (transient.go). It carries its own
	// event instant and expiry, and nothing ever clears it — the client's
	// clock retires it at its expiry. Nil until the first transient.
	transient *frontendv1.FooterActivityTransient
	// account is the account root (Claude config dir) the workspace's
	// session spends from, empty until SetAccount binds it.
	account string
	// usage is the ACCOUNT'S usage evidence, shared by every workspace bound
	// to the same root (account.go). Never nil: an unbound workspace holds a
	// private one until SetAccount binds it.
	usage *accountUsage
	// notification is the agent's standing push notification, nil when none
	// stands. It stands until the next prompt (salient.go).
	notification *standing
	// contextWindow is the main agent's last readable context-usage report,
	// nil until one arrives. The enduring line draws it.
	// retrying is the standing mid-turn retry evidence. It stands until the
	// retried call's response lands (ladder.RetryAnswered), the turn ends, or
	// the next turn opens; while it stands with a turn in flight the status is
	// `blocked · api_retrying` (retryBlocks).
	retrying *retryState
	// authLine is the standing auth prompt line.
	authLine *standing
	// queryDied is the standing dead-query line. It is SESSION-scoped, not
	// part of the block: the vendor's query death and the turn's terminal are
	// two facts about the same event arriving separately, and a terminal whose
	// failure does not say the query died respells the block without knowing
	// it — so keeping the line on the block let such a terminal erase the only
	// sentence the strip had about the death. Either statement of the death
	// stands it (the push, or the terminal's query_died arm), and it stands
	// until the next prompt opens a turn.
	queryDied *standing

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
	// liveWork is the watcher's AUTHORITATIVE live-work set: the one party
	// that reaps each detached item's watch at its terminal, and therefore the
	// only one that can say a detached item has ENDED. It governs which
	// detached rows stand and whether the strip's `background` arm may be
	// raised at all; the frames the footer reads supply a row's DESCRIPTION,
	// never its liveness.
	liveWork sessionwatcher.LiveWorkSet
	// liveWorkSeen reports whether the watcher has stated a set at all. Until
	// it has, the announcement ledger is all the footer has; once it has, the
	// set is the authority.
	liveWorkSeen bool
	// lastArm is the status arm the last published view carried, so a CHANGE
	// of arm is recorded once rather than on every push.
	lastArm string
	// lastLine is the activity line the last published view carried, so a
	// CHANGE to it can be recorded and a push that leaves it standing is not.
	lastLine activityLine

	// resumeWaits are the background subagents the shim is waiting to resume
	// after a network outage, in the order the shim stated them: the LAST
	// standing set, restated whole on every change (netresume.go). They are
	// read by the agents chip and panel ONLY — no status, freeness or drain
	// rule reads them (owner ruling, 2026-09-28: visibility only).
	resumeWaits []resumeWait
	// retiredRows are the detached subagent rows retired at their terminal, by
	// every identity the row is addressed by, so a wait that opens after the
	// failure terminal took the row away can still draw it (label,
	// description, tokens, jump). Kept for the same lifetime as retiredWork.
	retiredRows map[string]*agentRow

	// retiredWork are the detached handles that have already reached a
	// terminal, so a REPLAY of the run's opening frames cannot count it live
	// again.
	//
	// WHY A SET AND NOT JUST THE MAP. A detached run's frames reach this
	// daemon on BOTH books -- the spawning agent's, where the producer settles
	// the unit, and the run's own, which the watcher opened at the
	// announcement -- and the two deliveries are not ordered against each
	// other. Measured in the G50 playbook: `AgentSubagent_Success` for a
	// handle arrived on the caller's book and retired the row, and 80ms later
	// the same handle's `AgentSubagent_Start` arrived on the run's own book
	// and RE-OPENED it, so the ⚙ chip read 3 beside two settled placements and
	// one live one, for the rest of the session. A handle is minted once and
	// its terminal is final, so a start after a terminal is a second telling
	// of a finished run rather than a new one.
	retiredWork map[string]struct{}
	// adoptedWork are the detached items the watcher took up by ADOPTION and
	// the set has not listed yet, so the set change that lists them does not
	// read as a launch. See markAdopted.
	adoptedWork map[string]struct{}
	// workOwners are the owners recorded for detached-work-capable items, by
	// every identity each is addressed by, so drawsWork can tell the main
	// agent's work from a subagent's (owner.go). Kept for the session's life,
	// as retiredWork is, because a wait can name work long after it retired.
	workOwners map[string]*ownerRecord
	// unownedReported are the drawsWork refusals already recorded at ERROR,
	// so a render repeating one does not repeat its record.
	unownedReported map[string]struct{}
	// entries are the FeedIds the feed drew each detached-work-capable entry
	// under, keyed by unit (a subagent's spawn unit, a shell's work id, a
	// monitor's unit), as the feed resolver announced them (OnEntryPlaced). They are what a jump
	// row names: the address ON THE FEED THAT DRAWS THE ENTRY, never a guess.
	entries map[string]*frontendv1.FeedId
	// jumpNotes are the jump-resolution records a render produced, written
	// once the lock is released.
	jumpNotes []dlog.Context
	// focus is the expanded panel the last launch of detached work named, and
	// the generation it was minted under. See mintFocus.
	focus focusState
	// seq mints the panel orders so a row's place is its arrival order.
	seq int
}

// newWSState builds an empty accumulation.
func newWSState() *wsState {
	return &wsState{
		usage:           &accountUsage{},
		permissions:     map[string]standing{},
		questions:       map[string]standing{},
		retiredRows:     map[string]*agentRow{},
		agents:          map[string]*agentRow{},
		shells:          map[string]*shellRow{},
		monitors:        map[string]*monitorRow{},
		tasks:           map[string]*taskRow{},
		crons:           map[string]*cronRow{},
		bashUnits:       map[string]*shellRow{},
		retiredWork:     map[string]struct{}{},
		adoptedWork:     map[string]struct{}{},
		workOwners:      map[string]*ownerRecord{},
		unownedReported: map[string]struct{}{},
		entries:         map[string]*frontendv1.FeedId{},
		tok:             newTokenState(),
		motion:          newFeedMotion(),
	}
}

// detachedLive reports whether DETACHED work is running right now, which is
// what the strip's `background` arm means. The watcher's set is the authority
// once it has stated one; until then the footer's own rows are all it has.
//
// THE ROW COUNT IS NOT THE ANSWER, and that was the defect: a row whose
// terminal never reached the footer in a form its ledger matched stood for the
// rest of the session, and the strip reported a background task the roster —
// which reads the watcher's set — said was over.
func (s *wsState) detachedLive() bool {
	return s.detachedCount() > 0
}

// detachedCount is how many detached items are running right now, read from
// the same authority detachedLive reads: the watcher's set once it has stated
// one, the footer's own rows until then. IT COUNTS EVERY OWNER'S WORK, a
// subagent's included, though only the main agent's is drawn (owner.go): the
// `background` arm and the deploy's wait both say work is RUNNING, and the
// roster and the drain count the same items.
func (s *wsState) detachedCount() int {
	if s.liveWorkSeen {
		return len(s.liveWork.Agents) + len(s.liveWork.Shells) + len(s.liveWork.Monitors)
	}
	return len(s.agents) + len(s.shells) + len(s.monitors)
}

// observeArm folds the published view's status arm in, answering the arm and
// whether it CHANGED. The previous arm is answered too, so the record of a
// change carries both ends of it.
func (s *wsState) observeArm(view *frontendv1.FooterView) (arm string, changed bool, previous string) {
	arm = statusName(view.GetStrip().GetStatus())
	previous = s.lastArm
	if previous == "" {
		previous = "none"
	}
	if arm == s.lastArm {
		return arm, false, previous
	}
	s.lastArm = arm
	return arm, true, previous
}

// observeLine folds the published view's activity line in, answering the line,
// whether it CHANGED, and the line it replaced.
func (s *wsState) observeLine(view *frontendv1.FooterView) (line activityLine, changed bool, previous activityLine) {
	line = activityLineOf(view.GetStrip().GetStatus())
	previous = s.lastLine
	if line == previous {
		return line, false, previous
	}
	s.lastLine = line
	return line, true, previous
}

// nextOrder mints the next panel order.
func (s *wsState) nextOrder() int {
	s.seq++
	return s.seq
}

// retryBlocks reports a standing API retry that blocks the workspace: the
// vendor is retrying a call and a turn is in flight to be held by it.
func (s *wsState) retryBlocks() bool {
	return s.retrying != nil && s.turn != nil
}
