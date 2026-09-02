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

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
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

// ColdGate is the standing cold-context gate, which owns the composer while it
// stands.
type ColdGate struct {
	// Standing reports whether a gate is open.
	Standing bool
	// Detail is what was refused cold.
	Detail string
}

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
	// SetClosing installs a close refusal, nil to clear it.
	SetClosing(ws ids.WorkspaceID, blocked *CloseBlocked)
	// SetColdGate installs the standing cold gate.
	SetColdGate(ws ids.WorkspaceID, gate ColdGate)
	// SetInterrupting fires the waiting-interrupting status the MOMENT an
	// interrupt registers, before the real turn end arrives.
	SetInterrupting(ws ids.WorkspaceID, on bool)
	// Topic is the workspace's footer publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView]
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
	encodeFeedID  func(feedid.Ref) *frontendv1.FeedId
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

// WithFeedIDEncoder injects the FeedId encoder the chips' jump targets are
// minted with. Production uses feedid.Encode.
func WithFeedIDEncoder(f func(feedid.Ref) *frontendv1.FeedId) Option {
	return func(o *options) { o.encodeFeedID = f }
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

// New builds the footer resolver.
func New(log dlog.Surfaces, opts ...Option) (Resolver, error) {
	return newResolver(log, opts...)
}
