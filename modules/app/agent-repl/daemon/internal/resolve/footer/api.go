// Package footer is the footer resolver.
//
// It publishes the footer view WHOLE and only when complete: the tokens cell
// and its panels are ALWAYS populated. R1 momentary statuses (`interrupted`,
// `loading`) are retired by a DAEMON-SIDE one-shot successor push — nothing on
// the wire ticks. See ARCHITECTURE.md "resolvers".
//
// READINESS: a workspace's footer becomes complete the moment the resolver has
// observed ANY fact for it — a sink frame or a setter call. Every field of the
// view resolves from the accumulated state alone (the status tree bottoms out
// at idle, the tokens cell and all six panel lines are always populated, and
// an empty panel is a legal empty row list), so there is no partial state to
// ship. Before that first fact nothing is published, which is the contract's
// legal "not yet resolved" state.
package footer

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/vocab"
)

// MergeFacts is what the merge orchestrator tells the footer and the sidebar
// about a workspace's merge. ARCHITECTURE.md does not fix its fields; the
// minimum the contract implies is the state, its queue position and the
// evidence a parked merge needs.
type MergeFacts struct {
	// State names the merge's standing: "none", "enqueuing", "queued",
	// "merging", "parked", "conflict", "failed", "merged".
	State string
	// QueuePosition is the workspace's place in its repo's queue, zero when it
	// is not queued.
	QueuePosition int
	// QueueDepth is how many runs are queued in the same repo, zero when the
	// workspace is not queued.
	QueueDepth int
	// Round is the merge's current round number, zero before the first.
	Round int
	// Detail is the evidence a conflicted or failed merge carries.
	Detail string
	// ParkedLine is the merge orchestrator's COMPOSED standing line for a
	// parked merge — the same sentence the bubble's parked tab shows. The
	// footer draws it verbatim as FooterSubStatusMergingParked.line, and never
	// composes one of its own: a parked merge's account is the orchestrator's
	// fact. Empty while the merge is not parked.
	ParkedLine string
	// ActiveTab is the label of the merge bubble's FRONT entry's active tab
	// ("pre_prompt", "merge", "testing", "fixes", "conflicts", "post_prompt").
	// It is what refines the `merging` state into the phase substatus the
	// footer draws, so the footer never guesses a phase from a state word.
	// Empty means the state alone decides.
	ActiveTab string
}

// CloseBlocked is why a close was refused: a close requires quiet, and the
// refusal MANIFESTS IN THE FOOTER rather than only in the rpc's answer.
type CloseBlocked struct {
	// Reason names what is not quiet: "turn_in_flight", "live_work",
	// "held_prompts", "merge_queued".
	Reason string
	// Detail is the human-readable sentence the footer draws. It is also the
	// `summary` CloseWorkspaceBlocked carries: ONE composed sentence, so a
	// caller with no footer reads exactly what the footer's activity line
	// shows (landing 7).
	Detail string
	// The evidence the quiet check computed, whole, so the refusal states it
	// rather than only naming the first blocker it hit. Every field is filled
	// on every blocker.
	TurnInFlight bool
	// LiveWork counts the detached agents and shells still live.
	LiveWork uint32
	// HeldPrompts counts the undelivered held prompts.
	HeldPrompts uint32
	// MergeQueued reports a merge that still owes this workspace work.
	MergeQueued bool
}

// StartFailed is a session bring-up that FAILED, which the footer alone
// carries: the owner's ruling of 2026-09-12 is that a bring-up failure writes
// no feed row, so the disconnected arm's `start_failed` step and this line are
// the whole account of it.
type StartFailed struct {
	// Detail is WHY the bring-up failed, composed at the site that opened the
	// `shim_start_failed` fault out of that fault's own evidence, so the strip
	// and the fault cannot say different things about one failure.
	Detail string
}

// Fault is one standing daemon fault, as the footer draws it. The daemon's
// health package decides which cell a kind claims (THE FAULT PARTITION, stated
// in internal/health/footer.go and normatively in footer.proto); the resolver
// takes the verdict and draws it, deriving nothing.
type Fault struct {
	// ID is the fault record's id; CloseFault retracts it by this.
	ID string
	// Kind is the fault kind, spelled as the daemon's fault vocabulary spells
	// it. It is drawn in the activity cell.
	Kind string
	// Status is the footer status the fault CLAIMS: "disconnected", "blocked",
	// or EMPTY for a non-escalating fault, which leaves the status exactly as
	// it stands and takes the activity cell alone.
	Status string
	// SubStatus is the bucket within that status, empty when Status is.
	SubStatus string
	// Detail is the composed line, drawn verbatim. Empty is legal.
	Detail string
	// At is when the fault began standing.
	At time.Time
}

// ColdGate is the standing cold-context gate, which owns the composer while it
// stands.
type ColdGate struct {
	// Standing reports whether a gate is open.
	Standing bool
	// Detail is what was refused cold.
	Detail string
}

// ColdGateAnswer is a standing cold gate's answer BEING SPENT — the whole
// stretch from the click to the outcome, which before 2026-09-14 was invisible:
// the owner answered a gate, the shim compacted for a minute, the session came
// back, and the strip said nothing at any point of it (owner's report,
// 2026-09-14).
//
// IT REUSES THE EXISTING VOCABULARY AND ADDS NONE (owner ruling, 2026-09-14):
// the status is `thinking`, the step is the one the chosen remediation already
// has — `compacting` for a compaction, `clearing` for a clear, `submitting` for
// a paid resume — and the line is the daemon's own progress sentence for the
// phase the producer last stated. It OUTRANKS the standing gate it is
// answering, because the gate is not lifted until the re-open succeeds and the
// user must see the answer being spent rather than the question again.
type ColdGateAnswer struct {
	// Choice is the remediation being spent: "pay", "clear" or "compact".
	Choice string
	// Text is the daemon's COMPOSED progress line for where the answer has got
	// to. Never empty: an answer with nothing to say about itself is the state
	// this type exists to end.
	//
	// WHEN it began standing is the RESOLVER's stamp, taken from the resolver's
	// own clock like every other `at` on this strip, so a caller cannot hand
	// the footer an instant from a different clock than the one the view is
	// rendered against.
	Text string
}

// The cold-gate remediations, spelled once. The verb that answers a gate and
// the footer that draws the answer must not spell them differently.
const (
	// ChoicePay is the gate's "pay and resume".
	ChoicePay = "pay"
	// ChoiceClear is the gate's "clear and start fresh".
	ChoiceClear = "clear"
	// ChoiceCompact is the gate's "compact and resume".
	ChoiceCompact = "compact"
)

// SessionAct is what a turn was started to do. A prompt is the ordinary case;
// the two session acts have their own thinking substatus, and the daemon is
// the only thing that knows which act it submitted.
type SessionAct int

// The session acts a turn can carry.
const (
	// ActPrompt is an ordinary prompt delivery.
	ActPrompt SessionAct = iota
	// ActClear is a /clear being applied.
	ActClear
	// ActCompact is a compaction being run.
	ActCompact
)

// TurnStarted is the daemon's own fact that StartTurn was ACCEPTED. Nothing on
// the shim's streams states it — the first frame of a turn is an activity, by
// which time `submitting` is already over — so the footer is told directly.
type TurnStarted struct {
	// At is when the turn was accepted; the strip's clock ticks from it.
	At time.Time
	// Act is what the turn carries.
	Act SessionAct
}

// Resolver is the footer's whole surface.
type Resolver interface {
	sessionwatcher.FooterSink
	// Sink is the ONE entry point a deploy's progress reaches the footer
	// through: the update line, stood on every workspace's strip (update.go).
	deployprogress.Sink

	// SetParticipants states the OTHER TWO HOPS of connectivity truth: whether
	// this workspace's WatchHostWorkspace and WatchWebWorkspace streams are
	// held right now. The server calls it on every open and close edge of
	// either stream. Per daemon.md invariant 11 the workspace is connected
	// only while all three hops are live, so a hop down is drawn as not
	// connected however healthy the shim link is.
	SetParticipants(ws ids.WorkspaceID, host, web bool)
	// SetWorkspaceDir binds the workspace's directory, which is what resolves
	// its durable log sink. The daemon calls it at registration, BEFORE any
	// frame can arrive; a frame for an unbound workspace is an invariant
	// violation the resolver records loudly rather than writing globally by
	// default.
	SetWorkspaceDir(ws ids.WorkspaceID, dir string) error
	// SetTurn installs the accepted turn, nil when no turn is in flight. It is
	// what raises `thinking · submitting` the instant StartTurn is accepted
	// and what starts the strip's clock.
	SetTurn(ws ids.WorkspaceID, turn *TurnStarted)
	// SetMerge installs the merge facts the footer draws.
	SetMerge(ws ids.WorkspaceID, facts MergeFacts)
	// SetParked states that the idle sweep stood this workspace's shim down on
	// purpose and recorded the `hibernated` session terminal. While it stands,
	// a DEAD link is not a fault: the daemon put the route down and a prompt
	// brings it straight back, so the strip keeps an idle status instead of
	// `disconnected · dead`. It is lifted by the next link state of any kind,
	// which belongs to the revival's own spawn.
	SetParked(ws ids.WorkspaceID, parked bool)
	// SetStateUnreported installs, or lifts, the fact that a shim taken back
	// after a failed handover has not re-reported its session state: the
	// degraded rung, drawn `degraded · state_unreported`.
	SetStateUnreported(ws ids.WorkspaceID, unreported bool)
	// SetClosing installs a close refusal, nil to clear it.
	SetClosing(ws ids.WorkspaceID, blocked *CloseBlocked)
	// SetColdGate installs the standing cold gate.
	SetColdGate(ws ids.WorkspaceID, gate ColdGate)
	// SetColdGateAnswer installs the gate answer in flight, nil to clear it.
	// The verb that spends an answer calls it ONCE AT THE CLICK, before it
	// dials the shim, again for every phase the producer relays, and once more
	// with nil when the answer has landed or failed.
	SetColdGateAnswer(ws ids.WorkspaceID, answer *ColdGateAnswer)
	// SetInterrupting fires the waiting-interrupting status the MOMENT an
	// interrupt registers, before the real turn end arrives.
	SetInterrupting(ws ids.WorkspaceID, on bool)
	// OpenFault installs one standing daemon fault. An EMPTY workspace is a
	// DAEMON-SCOPED fault, which stands on every workspace's strip because it
	// is every workspace that is owed the service the daemon cannot give.
	//
	// It is driven from the ONE place faults are opened — health.ObserveFaults
	// — never from the raise sites: per-site plumbing is exactly how three
	// fault kinds came to have a footer path and sixteen did not.
	OpenFault(ws ids.WorkspaceID, fault Fault)
	// CloseFault retracts a standing fault by its record id. An empty
	// workspace retracts a daemon-scoped one.
	CloseFault(ws ids.WorkspaceID, id string)
	// SetStartFailed installs the standing bring-up failure whose line the
	// `disconnected · start_failed` step exists to explain, nil to clear it.
	// The next successful link edge clears it on its own.
	SetStartFailed(ws ids.WorkspaceID, failure *StartFailed)
	// AddDroppedPrompts adds to the count of held prompts the STANDING
	// bring-up failure dropped. The drop is decided by the prompt queue, after
	// the failure is installed, so the count arrives second and accrues onto
	// the failure already standing.
	AddDroppedPrompts(ws ids.WorkspaceID, n uint32)
	// Prime publishes the workspace's CURRENT footer view at registration, so
	// the per-workspace footer topic holds a complete view for a
	// (re)connecting subscriber to replay even before any live session fact
	// arrives — the same register-time prime the topbar takes through
	// SetNaming.
	//
	// THE FOOTER TOPIC IS PER-WORKSPACE, and unlike the GLOBAL roster (which
	// always retains a current value) it is empty after a daemon restart
	// rebuilds the resolver. An idle session then produces no fresh live edge,
	// so serveTopic and Republish would have nothing to hand a reconnecting
	// subscriber and the footer would stay blank until the next live change —
	// which for an idle session may never come. Priming at registration —
	// the one edge every workspace passes through on a reconnect (Emacs
	// re-announces every workspace it holds) — is what keeps the topic
	// current. The view is drawn from the accumulated state, so a prime on a
	// live daemon whose footer already stands renders the same view and is a
	// no-op through the topic's value dedup; it never regresses a live footer.
	Prime(ws ids.WorkspaceID)
	// Topic is the workspace's footer publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView]
	// OnEntryPlaced records where the feed drew one detached-work-capable
	// entry (a subagent bubble by its spawn unit, a shell head by its work
	// id). It is the feed resolver's Deps.EntryPlaced, and it is what a jump
	// row names: FooterJump.entry, never an address the footer guessed.
	OnEntryPlaced(ws ids.WorkspaceID, unit string, row *frontendv1.FeedId)
	// OnItemDrawn records where the feed drew one activity unit's row, and
	// whether it is on the root feed. It is the feed resolver's
	// Deps.ItemDrawn, and it is what ends a quiet stretch: the stretch ends
	// when the next feed item is DRAWN, and the ended line is stated with a
	// root-feed row (FooterStatusQuietStretchEnding).
	OnItemDrawn(ws ids.WorkspaceID, unit string, row *frontendv1.FeedId, onRoot bool)
}

// Option adjusts the resolver's injectable knobs. The defaults are the
// production values; tests inject a clock and compressed thresholds.
type Option func(*options)

// options are the resolver's knobs.
type options struct {
	clock         Clock
	dwell         time.Duration
	alarmTokens   uint64
	rateNewsworth float64
}

// WithClock injects the clock the dwell and every `at` stamp are taken from.
func WithClock(c Clock) Option { return func(o *options) { o.clock = c } }

// WithMomentaryDwell sets how long a momentary status (`interrupted`,
// `loading`) stands before the R1 successor push retires it.
func WithMomentaryDwell(d time.Duration) Option { return func(o *options) { o.dwell = d } }

// WithTokenAlarmThreshold sets the uncached-input figure a turn must exceed
// for the expensive-turn alarm to trip.
func WithTokenAlarmThreshold(n uint64) Option { return func(o *options) { o.alarmTokens = n } }

// WithRateLimitNewsworthyThreshold sets the utilization (0..1) an allowance
// must reach before it is reported as newsworthy.
func WithRateLimitNewsworthyThreshold(f float64) Option {
	return func(o *options) { o.rateNewsworth = f }
}

// Defaults for the injectable knobs.
const (
	// DefaultMomentaryDwell is how long `interrupted` and `loading` stand.
	DefaultMomentaryDwell = 1500 * time.Millisecond
	// DefaultTokenAlarmThreshold is the uncached-input figure that trips the
	// expensive-turn alarm.
	DefaultTokenAlarmThreshold uint64 = 20_000
	// DefaultRateLimitNewsworthyThreshold is the utilization an allowance must
	// reach to be worth saying anything about.
	DefaultRateLimitNewsworthyThreshold = 0.8
	// DefaultWarningRowWidth is how wide a composed panel row line may be
	// before the daemon truncates it.
	DefaultWarningRowWidth = 120
)

// New builds the footer resolver. It takes the render-colors vocabulary so the
// footer_status and footer_allowance tables are asserted against the arms this
// resolver emits, at boot, rather than drawing an unpainted state later.
func New(colors vocab.RenderColors, log dlog.Surfaces, opts ...Option) (Resolver, error) {
	return newResolver(colors, log, opts...)
}
