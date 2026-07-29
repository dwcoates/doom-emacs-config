// Package progress resolves the consolidated progress footer's single input:
// one latest-wins agentshim.frontend.v1.ProgressView per workspace, pushed on
// change and carried in the connect StateSnapshot.
//
// It is a SIBLING of internal/ssm, not a layer over it. The SSM resolves the
// closed render-state vocabulary from a persisted signal log; this resolver
// folds the SAME event stream — plus the ephemeral relays and the daemon's own
// local counters — into the ephemeral progress facts that have no home in the
// SSM's durable model: the turn clock, the live tickers, the activity windows,
// and the pending/queued counts. Nothing here is persisted: a progress view is
// a statement about RIGHT NOW, and a restart legitimately starts blank.
//
// The two are wired together at the seam rather than merged: the SSM's resolved
// WorkspaceState is fed in via ObserveWorkspaceState, which supplies the
// live-task count so this resolver never re-derives it.
//
// THE PHASE IS NOT HERE. This resolver used to keep a copy of the SSM's verdict
// so the footer had one self-sufficient frame, and the copy went stale exactly
// as a second copy of an authoritative fact always does — it refreshed on this
// resolver's triggers rather than on the authority's. The footer reads the
// phase off the WorkspaceState instead, which is the same message the sidebar
// dot and the tab bar read, delivered in the same sequenced order.
//
// COALESCING. The ticker inputs (thinking tokens, per-request input usage) fire
// far faster than any frontend can paint. They mark the workspace dirty and a
// single deferred flush pushes at most one frame per coalescing window.
// STRUCTURAL changes (a phase transition, a window opening or closing, a turn
// boundary, an error, a count) bypass the window and push at once, because
// their latency is what the footer exists to show. A structural push also
// flushes whatever ticker movement was pending, so a coalesced value is never
// stranded behind a quiet period.
package progress

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// subBufferSize bounds each subscriber's channel. A subscriber this far behind
// is a slow consumer: the push is dropped (loudly), never blocked on, and the
// consumer recovers from the next Snapshot. Mirrors the SSM's contract.
const subBufferSize = 64

// DefaultCoalesceWindow is how long ticker movement is held before it is
// pushed. Chosen so a chatty thinking-token stream costs at most ~8 frames a
// second rather than one frame per token estimate.
const DefaultCoalesceWindow = 120 * time.Millisecond

// Scheduler defers a func. Injected so the coalescing window is testable
// without wall-clock waits: a test scheduler captures the callback and fires it
// on demand.
type Scheduler interface {
	// AfterFunc runs f once after d, returning a stop func that reports
	// whether it prevented the run (the time.Timer.Stop contract).
	AfterFunc(d time.Duration, f func()) (stop func() bool)
}

// realScheduler is time.AfterFunc.
type realScheduler struct{}

func (realScheduler) AfterFunc(d time.Duration, f func()) func() bool {
	t := time.AfterFunc(d, f)
	return t.Stop
}

// Options configure a Manager.
type Options struct {
	// Logf is the loud anomaly logger. Required.
	Logf dlog.Logf
	// Clock returns wall-clock unix millis. Nil uses time.Now.
	Clock func() int64
	// Sched defers coalesced flushes. Nil uses time.AfterFunc.
	Sched Scheduler
	// CoalesceWindow holds ticker movement before pushing. Zero uses
	// DefaultCoalesceWindow; negative disables coalescing entirely (every
	// ticker update pushes at once), which is what most tests want.
	CoalesceWindow time.Duration
}

// Manager resolves and fans out per-workspace ProgressViews.
type Manager struct {
	logf   dlog.Logf
	clock  func() int64
	sched  Scheduler
	window time.Duration

	mu      sync.Mutex
	views   map[string]*workspaceProgress
	subs    map[int]chan *frontendv1.ProgressView
	nextSub int
	// dirty names the workspaces with coalesced ticker movement awaiting a
	// flush. A structural push clears a workspace's entry as it goes out.
	dirty map[string]struct{}
	// stopFlush cancels the pending deferred flush, if any.
	stopFlush func() bool
	closed    bool
}

// workspaceProgress is one workspace's mutable resolution: the view itself plus
// the bookkeeping that produces it but does not belong on the wire.
type workspaceProgress struct {
	view *frontendv1.ProgressView
	// turnOpen is whether a turn is in flight as far as THIS resolver has
	// observed. It makes the turn-start reset idempotent: a second start signal
	// for a turn already running must not clear the tokens accumulated so far.
	turnOpen bool
	// countedUsage dedupes per-request input usage. The same assistant message
	// reaches the daemon on BOTH observation planes (the live stream and the
	// on-disk transcript), so counting every sighting would double the turn's
	// input figure. Reset at each turn start.
	countedUsage map[string]struct{}
	// retryDetailRich marks the retrying window's detail as having come from a
	// data.ApiRetry rather than the poorer ApiErrorLine retry fields.
	//
	// The two are TWINS describing the same backoff on different planes: the
	// stream's api_retry (which carries the backoff delay and the HTTP status)
	// and the transcript's api_error (which carries attempt counts only). The
	// disk twin generally lands second, so without this flag it would overwrite
	// the richer live detail with its own. Cleared whenever the window closes,
	// so the ApiErrorLine fallback still works for a plane or a CLI version
	// that emits no api_retry at all.
	retryDetailRich bool
}

// New builds a Manager.
func New(opts Options) *Manager {
	if opts.Logf == nil {
		panic("progress: Options.Logf is required")
	}
	logf := opts.Logf
	clock := opts.Clock
	if clock == nil {
		clock = func() int64 { return time.Now().UnixMilli() }
	}
	var sched Scheduler = opts.Sched
	if sched == nil {
		sched = realScheduler{}
	}
	window := opts.CoalesceWindow
	if window == 0 {
		window = DefaultCoalesceWindow
	}
	return &Manager{
		logf:   logf,
		clock:  clock,
		sched:  sched,
		window: window,
		views:  map[string]*workspaceProgress{},
		subs:   map[int]chan *frontendv1.ProgressView{},
		dirty:  map[string]struct{}{},
	}
}

// ---------------------------------------------------------------------------
// Inputs
// ---------------------------------------------------------------------------

// ObserveWorkspaceState adopts the live-task count and session identity off the
// SSM's resolved WorkspaceState.
//
// IT NO LONGER MIRRORS THE PHASE. It used to copy the SSM's verdict into
// `view.State` so the footer had one self-sufficient input, and the copy was
// the defect: it refreshed only on THIS resolver's triggers, so a workspace
// that acquired a progress fact before a state reached here kept the blank
// view's INIT seed forever. That is why the footer read "starting" against an
// already-green tab until the first prompt moved the phase.
//
// A second copy of an authoritative fact has no correct refresh policy, so
// there is no longer a second copy: the footer reads the phase off the
// WorkspaceState the SSM pushes, which every frontend already receives, and
// which the delivery sequencer orders against every other surface.
//
// A nil state is a programmer error at the call site, surfaced rather than
// silently ignored.
func (m *Manager) ObserveWorkspaceState(ws *frontendv1.WorkspaceState) error {
	if ws == nil {
		return fmt.Errorf("progress: ObserveWorkspaceState got a nil state")
	}
	name := ws.GetWorkspace()
	if name == "" {
		return fmt.Errorf("progress: ObserveWorkspaceState got a state with no workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(name)
	if sid := ws.GetSessionId(); sid != "" {
		wp.view.SessionId = sid
	}
	wp.view.LiveTaskCount = ws.GetLiveTaskCount()
	m.pushLocked(name, wp)
	return nil
}

// NoteTurnAccepted opens the turn clock when the daemon ACCEPTS a prompt for
// immediate submission.
//
// WHY THIS EXISTS rather than relying on the TurnStarted event alone: live
// TurnStarted events do not currently reach the daemon (a known defect being
// fixed separately), so the submit-accept is the earliest turn-start signal the
// daemon actually observes today. Both paths funnel into the same idempotent
// open (see openTurnLocked), so when TurnStarted starts arriving it simply
// becomes the earlier of the two and nothing double-resets.
//
// A prompt that the daemon QUEUES rather than submits must NOT call this: the
// turn it would be reporting is the one already running.
func (m *Manager) NoteTurnAccepted(workspace, sessionID string) {
	if workspace == "" {
		m.logf("progress: NoteTurnAccepted with no workspace (session=%s); ignoring", sessionID)
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(workspace)
	if sessionID != "" {
		wp.view.SessionId = sessionID
	}
	if !m.openTurnLocked(wp, m.clock()) {
		m.logf("progress: turn accepted IDEMPOTENT ws=%s session=%s turn_open=true interrupt_outcome=%s",
			workspace, sessionID, wp.view.GetInterrupt().GetOutcome())
		return
	}
	m.logf("progress: turn accepted OPENED ws=%s session=%s turn_started_at_ms=%d interrupt_cleared=true",
		workspace, sessionID, wp.view.GetTurnStartedAtMs())
	m.pushLocked(workspace, wp)
}

// NoteTurnRejected closes the turn clock NoteTurnAccepted opened for a prompt
// the shim then refused to take.
//
// The accept is optimistic by design (see ssm.MarkPromptAccepted): it starts
// the footer clock the moment the daemon commits to the submit, so the elapsed
// time the user reads counts from when they pressed send. A failed submit means
// no turn is running behind that clock, and a footer counting up against
// nothing is a worse report than no footer at all.
//
// Callers must first confirm the state retraction actually happened, so a turn
// that started for some other reason between the accept and the failure keeps
// its clock. An already-closed clock is an idempotent no-op.
func (m *Manager) NoteTurnRejected(workspace, sessionID string) {
	if workspace == "" {
		m.logf("progress: NoteTurnRejected with no workspace (session=%s); ignoring", sessionID)
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(workspace)
	if !wp.turnOpen {
		m.logf("progress: turn rejected IDEMPOTENT ws=%s session=%s turn_open=false", workspace, sessionID)
		return
	}
	wp.turnOpen = false
	wp.view.TurnStartedAtMs = 0
	wp.view.Retrying = nil
	wp.retryDetailRich = false
	m.logf("progress: turn rejected CLOSED ws=%s session=%s — the shim never took the prompt whose accept opened this clock",
		workspace, sessionID)
	m.pushLocked(workspace, wp)
}

// NoteInterrupt opens the INTERRUPT WINDOW on the shim's ack of a
// USER-COMMANDED stop (I1).
//
// The outcome is decided ATOMICALLY on that ack (core.proto InterruptOutcome:
// the shim's single-threaded liveness check IS the answer), so the window
// opens already carrying its verdict and there is no outcome-pending phase to
// represent. All three outcomes open it: ALREADY_COMPLETE and FAILED move no
// workspace phase, and the window is the only surface that reports them at
// all.
//
// IT CLEARS WHEN THE NEXT TURN STARTS, never on a timer — see
// openTurnLocked, which is the one place that clears it.
//
// ONLY THE COMMAND PATH CALLS THIS. The queue's interject sends the same
// Interrupt to the same shim as machinery (a held prompt asked to run sooner,
// not a user asking for the turn to stop), and it reaches this resolver from
// nowhere: sessioncontroller's interject calls the shim client directly, while the
// frontend command's Interrupt is the sole caller here.
func (m *Manager) NoteInterrupt(workspace, sessionID string, outcome corev1.InterruptOutcome) {
	if workspace == "" {
		m.logf("progress: NoteInterrupt with no workspace (session=%s outcome=%s); ignoring", sessionID, outcome)
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(workspace)
	if sessionID != "" {
		wp.view.SessionId = sessionID
	}
	wp.view.Interrupt = &frontendv1.InterruptWindow{
		Active:  true,
		SinceMs: m.clock(),
		Outcome: outcome,
	}
	m.logf("progress: interrupt window OPENED ws=%s session=%s outcome=%s since_ms=%d turn_started_at_ms=%d",
		workspace, sessionID, outcome, wp.view.Interrupt.GetSinceMs(), wp.view.GetTurnStartedAtMs())
	// STRUCTURAL: a stop landing is exactly the kind of latency the footer
	// exists to show, so it goes out at once rather than riding a window.
	m.pushLocked(workspace, wp)
}

// LiveTasks returns the workspace's live subagent-task count as this resolver
// last adopted it from the SSM's WorkspaceState, and whether the workspace has
// a view at all.
//
// The miss is reported rather than defaulted because the two answers differ:
// "this workspace has no live tasks" and "nothing has ever told this resolver
// about this workspace" are different facts, and the interrupt confirm gate
// (the caller) must be able to say which one it acted on.
func (m *Manager) LiveTasks(workspace string) (int64, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	wp, ok := m.views[workspace]
	if !ok {
		return 0, false
	}
	return wp.view.GetLiveTaskCount(), true
}

// SetCounts adopts the daemon-local ephemeral counters: how many permission
// prompts are waiting on the user and how deep the held-prompt queue is.
// Neither is a store fact — both live only in the daemon — so they are set
// rather than folded.
func (m *Manager) SetCounts(workspace string, pendingPermissions, queueDepth int64) {
	if workspace == "" {
		m.logf("progress: SetCounts with no workspace; ignoring")
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(workspace)
	if wp.view.GetPendingPermissions() == pendingPermissions && wp.view.GetQueueDepth() == queueDepth {
		return
	}
	wp.view.PendingPermissions = pendingPermissions
	wp.view.QueueDepth = queueDepth
	m.pushLocked(workspace, wp)
}

// Apply folds one store event into the workspace's progress view.
//
// The fold is deliberately NARROW: it consumes exactly the payloads that carry
// ephemeral progress facts and reports every other payload as untouched (ok
// false), so a caller can tell "folded" from "nothing here for me" without this
// package pretending to have handled something it did not.
//
// A nil event or an empty workspace is a programmer error, surfaced loudly.
func (m *Manager) Apply(workspace string, ev *corev1.Event) error {
	if ev == nil {
		return fmt.Errorf("progress: Apply got a nil event")
	}
	if workspace == "" {
		return fmt.Errorf("progress: Apply got an event with no workspace (session %s seq %d)",
			ev.GetSessionId(), ev.GetSeq())
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	wp := m.forLocked(workspace)
	if sid := ev.GetSessionId(); sid != "" {
		wp.view.SessionId = sid
	}

	at := ev.GetProducedAtMs()
	if at == 0 {
		at = m.clock()
	}

	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		if m.openTurnLocked(wp, at) {
			m.pushLocked(workspace, wp)
		}
	case *corev1.Event_TurnEnded:
		m.closeTurnLocked(wp, p.TurnEnded)
		m.pushLocked(workspace, wp)
	case *corev1.Event_MessageLatency:
		m.applyLatencyLocked(workspace, wp, p.MessageLatency)
	case *corev1.Event_Vendor:
		if err := m.applyVendorLocked(workspace, wp, p.Vendor, at); err != nil {
			return err
		}
	default:
		// Every other payload is somebody else's fact: task lifecycle reaches the
		// footer as the SSM's live_task_count (ObserveWorkspaceState), content
		// deltas are the typing relay's, and heartbeats are the HeartbeatView
		// relay's. Nothing to fold, and nothing lost.
	}
	return nil
}

// applyLatencyLocked adopts a streamed message's first-token latency.
//
// This is the LIVE ttft source. The same number reaches the daemon a second
// time on the turn's terminal result, but only once the turn is over — useless
// to a footer whose whole job is to report the turn in flight. The shim's delta
// bypass relays the vendor's mid-stream stamp as an EPHEMERAL MessageLatency,
// and this is its one consumer.
//
// LATEST-WINS per message, matching the field's meaning ("first-token latency
// of the current message"): a turn streams one message per API request, each
// stamped with its own latency, and the footer reports the newest. Structural
// rather than coalesced, because it moves at most once per message.
//
// A missing or unusable stamp never reaches here (the relay declines to emit
// one), so a zero arriving anyway is a producer bug rather than absence — but
// it is still refused rather than allowed to blank a figure already standing,
// which is the EPHEMERAL contract's absence-tolerance.
func (m *Manager) applyLatencyLocked(workspace string, wp *workspaceProgress, ml *corev1.MessageLatency) {
	next := ml.GetTtftMs()
	if next <= 0 {
		m.logf("progress: MessageLatency with no usable ttft ws=%s uuid=%s ttft=%d; ignored",
			workspace, ml.GetUuid(), next)
		return
	}
	if wp.view.GetTtftMs() == next {
		return
	}
	wp.view.TtftMs = next
	m.pushLocked(workspace, wp)
}

// ---------------------------------------------------------------------------
// The vendor fold
// ---------------------------------------------------------------------------

// applyVendorLocked folds a vendor payload's progress facts. It hard-errors on
// an Any that cannot be unmarshaled — that is a genuine anomaly, distinct from
// a known payload this resolver has nothing to take from.
func (m *Manager) applyVendorLocked(workspace string, wp *workspaceProgress, a *anypb.Any, atMs int64) error {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return fmt.Errorf("progress: unmarshal vendor Any (type=%q): %w", a.GetTypeUrl(), err)
	}
	switch v := msg.(type) {
	case *datav1.ClaudeStreamMessage:
		m.applyStreamLocked(workspace, wp, v, atMs)
	case *datav1.AssistantMessage:
		m.applyAssistantLocked(workspace, wp, v.GetUuid(), v.GetMessage())
	case *datav1.TranscriptLine:
		m.applyTranscriptLocked(workspace, wp, v, atMs)
	default:
		// A known data.v1 message carrying no progress fact.
	}
	return nil
}

// applyStreamLocked folds the live stream plane's progress-bearing arms.
func (m *Manager) applyStreamLocked(workspace string, wp *workspaceProgress, csm *datav1.ClaudeStreamMessage, atMs int64) {
	switch inner := csm.GetMsg().(type) {
	case *datav1.ClaudeStreamMessage_ThinkingTokens:
		// A TICKER: coalesced. The estimate climbs continuously through a long
		// reasoning block and no frontend needs every step of it.
		next := inner.ThinkingTokens.GetEstimatedTokens()
		if wp.view.GetThinkingTokens() == next {
			return
		}
		wp.view.ThinkingTokens = next
		m.markDirtyLocked(workspace)
	case *datav1.ClaudeStreamMessage_Status:
		// STRUCTURAL: the compaction window. An empty status is the vendor's
		// null — the window closing — not an absent field.
		m.setWindowLocked(workspace, wp, &wp.view.Compacting,
			inner.Status.GetStatus() == "compacting", atMs, inner.Status.GetStatus())
	case *datav1.ClaudeStreamMessage_Assistant:
		m.applyAssistantLocked(workspace, wp, inner.Assistant.GetUuid(), inner.Assistant.GetMessage())
	case *datav1.ClaudeStreamMessage_HookStarted:
		// STRUCTURAL: a hook is running. Its response closes the window.
		m.setWindowLocked(workspace, wp, &wp.view.Hook, true, atMs, hookDetail(inner.HookStarted))
	case *datav1.ClaudeStreamMessage_HookResponse:
		m.setWindowLocked(workspace, wp, &wp.view.Hook, false, atMs, "")
	case *datav1.ClaudeStreamMessage_AuthStatus:
		m.setWindowLocked(workspace, wp, &wp.view.Authenticating,
			inner.AuthStatus.GetIsAuthenticating(), atMs, authDetail(inner.AuthStatus))
	case *datav1.ClaudeStreamMessage_RateLimitEvent:
		m.applyRateLimitLocked(workspace, wp, inner.RateLimitEvent.GetRateLimitInfo())
	case *datav1.ClaudeStreamMessage_ApiRetry:
		// STRUCTURAL: the AUTHORITATIVE retrying source. It carries what the
		// transcript twin cannot — the backoff delay and the HTTP status — so it
		// claims the window's detail and marks it rich.
		m.setWindowLocked(workspace, wp, &wp.view.Retrying, true, atMs, apiRetryDetail(inner.ApiRetry))
		wp.retryDetailRich = true
	case *datav1.ClaudeStreamMessage_SessionStateChanged:
		m.applySessionStateLocked(workspace, wp, inner.SessionStateChanged.GetState(), atMs)
	default:
		// A known stream arm carrying no progress fact of its own.
	}
}

// applySessionStateLocked folds the session's own idle/running/blocked report.
//
// It drives EXACTLY ONE thing: the `blocked` window. It is deliberately NOT a
// phase source — the SSM's WorkspaceState is the workspace's only phase,
// because two independent phase authorities is precisely the drift the SSM
// exists to prevent. What this adds instead is a fact the daemon cannot
// otherwise see:
// `requires_action` means the session is parked on the USER, and it covers
// interactions the daemon holds no count for, so `pending_permissions` alone
// under-reports "waiting on you".
//
// `idle` and `running` both close the window: neither is blocked, and the
// difference between them is the SSM's business, not this window's. An
// unrecognized state is loud-logged and changes nothing rather than being
// guessed at.
func (m *Manager) applySessionStateLocked(workspace string, wp *workspaceProgress, state string, atMs int64) {
	switch state {
	case "requires_action":
		m.setWindowLocked(workspace, wp, &wp.view.Blocked, true, atMs, "waiting on you")
	case "idle", "running":
		m.setWindowLocked(workspace, wp, &wp.view.Blocked, false, atMs, "")
	default:
		m.logf("progress: unknown session state %q ws=%s; blocked window unchanged", state, workspace)
	}
}

// weeklyRateLimitTypes are the vendor's `rate_limit_type` values that bill
// against the SEVEN-DAY allowance. The per-model variants and the
// overage-included one are the same weekly allowance reported with a narrower
// scope, so they share its window rather than each claiming one.
//
// "overage" is weekly too: overage is the allowance a user buys once the week's
// is spent, and it resets on the week's own deadline.
var weeklyRateLimitTypes = map[string]bool{
	"seven_day":                  true,
	"seven_day_opus":             true,
	"seven_day_sonnet":           true,
	"seven_day_overage_included": true,
	"overage":                    true,
}

// sessionRateLimitTypes are the values that bill against the rolling FIVE-HOUR
// session allowance. The empty string is here because a vendor that reports no
// type at all is reporting the session window — that is what the field's
// absence meant before the type was modeled, and it is the reading every
// display carried until now.
var sessionRateLimitTypes = map[string]bool{
	"five_hour": true,
	"":          true,
}

// applyRateLimitLocked folds a rate-limit report into ITS OWN allowance's
// window — the five-hour session one or the seven-day weekly one, per the
// vendor's `rate_limit_type`.
//
// The two are kept apart because they are separate facts: separate deadlines,
// separate severities, and separate remedies. One shared window meant the last
// event won, so a weekly figure deep into its allowance was rendered under the
// session's name.
//
// The window is PRESENT for as long as the vendor has reported its allowance,
// including a plain "allowed" — that figure is not news on its own, but it is
// exactly what a reader needs beside the allowance that IS news, and it is the
// only way the footer can name both. `active` carries the narrower newsworthy
// claim (any status other than a plain "allowed"), which is what still decides
// whether the rung outranks the activity the footer would otherwise show.
//
// An unrecognized type is loud-logged and folded into the session window rather
// than dropped: the daemon does not know which allowance it bills against, but
// a rate-limit report the reader never sees is strictly worse than one filed
// under the wrong heading, and the log says which happened.
func (m *Manager) applyRateLimitLocked(workspace string, wp *workspaceProgress, info *datav1.RateLimitInfo) {
	limitType := info.GetRateLimitType()
	weekly := weeklyRateLimitTypes[limitType]
	if !weekly && !sessionRateLimitTypes[limitType] {
		m.logf("progress: unknown rate_limit_type %q ws=%s; filed under the session allowance",
			limitType, workspace)
	}
	status := info.GetStatus()
	next := &frontendv1.RateLimitWindow{
		// Anything other than a plain "allowed" is news — UNCHANGED from when
		// this was the whole gate on the window existing. An empty status is
		// the vendor reporting no verdict rather than a bad one, so it stays
		// quiet here exactly as it always did.
		Active:      status != "" && status != "allowed",
		ResetsAt:    info.GetResetsAt(),
		Utilization: info.GetUtilization(),
		Status:      status,
	}
	slot := &wp.view.RateLimited
	if weekly {
		slot = &wp.view.RateLimitedWeekly
	}
	if proto.Equal(*slot, next) {
		return
	}
	*slot = next
	m.pushLocked(workspace, wp)
}

// applyTranscriptLocked folds the on-disk plane's progress-bearing lines: the
// API error/retry family, and the assistant lines whose usage feeds the
// turn-scoped input figure.
func (m *Manager) applyTranscriptLocked(workspace string, wp *workspaceProgress, tl *datav1.TranscriptLine, atMs int64) {
	switch line := tl.GetLine().(type) {
	case *datav1.TranscriptLine_Assistant:
		al := line.Assistant
		m.applyAssistantLocked(workspace, wp, al.GetEnvelope().GetUuid(), al.GetMessage())
	case *datav1.TranscriptLine_System:
		if ae := line.System.GetApiError(); ae != nil {
			m.applyApiErrorLocked(workspace, wp, line.System.GetEnvelope().GetUuid(), ae, atMs)
		}
	}
}

// applyApiErrorLocked splits the ApiErrorLine family into its two meanings.
//
// A line WITH retries remaining is the SDK saying "this failed and I am going
// again": the retrying window opens and no error is reported, because the turn
// has not failed. This is the FALLBACK half of the retry sourcing — a live
// data.ApiRetry says the same thing with a backoff delay and an HTTP status
// attached, so once one has spoken for this window its detail stands and this
// line does not downgrade it (see `retryDetailRich`).
//
// A line with retries EXHAUSTED is the TERMINAL account of the failure, which
// is this family's own job and which api_retry never reports: the window
// closes and the classified failure takes over, addressed to the failure
// CARD (never the raw line, which renders nothing a user can act on) so the
// footer's error row can scroll the feed to it.
func (m *Manager) applyApiErrorLocked(workspace string, wp *workspaceProgress, uuid string, ae *datav1.ApiErrorLine, atMs int64) {
	if errclass.Retrying(ae) {
		if wp.retryDetailRich {
			return // api_retry already said it better
		}
		m.setWindowLocked(workspace, wp, &wp.view.Retrying, true, atMs, retryDetail(ae))
		return
	}
	m.setWindowLocked(workspace, wp, &wp.view.Retrying, false, atMs, "")
	wp.retryDetailRich = false
	failure := errclass.APIError(ae, frontend.FailureUUID(uuid))
	if proto.Equal(wp.view.Failure, failure) {
		return
	}
	wp.view.Failure = failure
	m.pushLocked(workspace, wp)
}

// applyAssistantLocked adds one assistant message's INPUT usage to the current
// turn's running total: uncached input plus both cache halves, exactly the
// figure the design's token cell names. Output tokens are deliberately absent —
// the footer never shows a running output figure.
//
// Deduped per message so the twin observation planes cannot double-count, and
// skipped entirely off-turn: the figure is turn-scoped, so usage arriving with
// no turn open belongs to no turn and is not attributed to the next one.
func (m *Manager) applyAssistantLocked(workspace string, wp *workspaceProgress, uuid string, msg *datav1.ApiAssistantMessage) {
	u := msg.GetUsage()
	if u == nil {
		return
	}
	if !wp.turnOpen {
		return
	}
	key := uuid
	if key == "" {
		// No message uuid to dedupe on. Counting it is the honest choice (the
		// tokens were really spent), but it is loud because a re-observation on
		// the other plane WILL double-count it.
		m.logf("progress: assistant usage with no uuid ws=%s; counted undeduped", workspace)
	} else {
		if _, seen := wp.countedUsage[key]; seen {
			return
		}
		wp.countedUsage[key] = struct{}{}
	}
	add := u.GetInputTokens() + u.GetCacheReadInputTokens() + u.GetCacheCreationInputTokens()
	if add == 0 {
		return
	}
	// A TICKER: coalesced. Requests land in bursts through a tool-heavy turn.
	wp.view.InputTokens += add
	m.markDirtyLocked(workspace)
}

// ---------------------------------------------------------------------------
// Turn lifecycle
// ---------------------------------------------------------------------------

// openTurnLocked starts the turn clock and resets every turn-scoped figure,
// reporting whether anything changed. Idempotent: a second start signal for a
// turn already in flight is a no-op, so the accumulated tokens survive it.
func (m *Manager) openTurnLocked(wp *workspaceProgress, atMs int64) bool {
	if wp.turnOpen {
		return false
	}
	wp.turnOpen = true
	wp.countedUsage = map[string]struct{}{}
	wp.view.TurnStartedAtMs = atMs
	wp.view.InputTokens = 0
	wp.view.ThinkingTokens = 0
	wp.view.TtftMs = 0
	// The failure persists until the NEXT turn starts (design decision), so
	// this is exactly where it clears.
	wp.view.Failure = nil
	wp.view.Retrying = nil
	wp.retryDetailRich = false
	// The interrupt window persists until the NEXT turn starts, for the same
	// reason the failure does: it reports what happened to the turn that just
	// stopped, and a new turn beginning is the moment that report stops being
	// the news. Nothing else clears it — never a timer.
	wp.view.Interrupt = nil
	return true
}

// closeTurnLocked stops the turn clock and records an errored end. The turn's
// token figures are left standing: the idle footer shows the last turn's
// summary rather than blanking the moment the turn lands.
func (m *Manager) closeTurnLocked(wp *workspaceProgress, te *corev1.TurnEnded) {
	wp.turnOpen = false
	wp.view.TurnStartedAtMs = 0
	wp.view.Retrying = nil
	wp.retryDetailRich = false
	if te.GetIsError() {
		// A terminal ApiErrorLine earlier in the turn is the more specific
		// account (it addresses its own card) and is left standing rather
		// than overwritten by the generic turn-end account.
		if wp.view.Failure == nil {
			// TurnEnd returns nil for a conclusion the SSM does not treat as
			// blocking, so the footer and the workspace color agree on what
			// "the turn failed" means instead of each deciding for itself.
			wp.view.Failure = errclass.TurnEnd(te)
		}
	}
}

// setWindowLocked opens, refreshes, or closes an activity window, pushing only
// on a real change. since_ms is stamped when the window OPENS and preserved
// across detail refreshes, so a window's age counts from its start rather than
// restarting on every update.
func (m *Manager) setWindowLocked(workspace string, wp *workspaceProgress, slot **frontendv1.ProgressWindow, active bool, atMs int64, detail string) {
	cur := *slot
	if !active {
		if cur == nil || !cur.GetActive() {
			return
		}
		*slot = nil
		m.pushLocked(workspace, wp)
		return
	}
	if cur != nil && cur.GetActive() && cur.GetDetail() == detail {
		return
	}
	since := atMs
	if cur != nil && cur.GetActive() {
		since = cur.GetSinceMs()
	}
	*slot = &frontendv1.ProgressWindow{Active: true, SinceMs: since, Detail: detail}
	m.pushLocked(workspace, wp)
}

// ---------------------------------------------------------------------------
// Reads and fan-out
// ---------------------------------------------------------------------------

// Current returns the workspace's ProgressView, and whether one exists. An
// absent workspace is an explicit miss, never a zero-value default.
func (m *Manager) Current(workspace string) (*frontendv1.ProgressView, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	wp, ok := m.views[workspace]
	if !ok {
		return nil, false
	}
	return cloneView(wp.view), true
}

// Snapshot returns every workspace's current ProgressView in stable workspace
// order, for a frontend's connect/resync StateSnapshot.
func (m *Manager) Snapshot() []*frontendv1.ProgressView {
	m.mu.Lock()
	defer m.mu.Unlock()
	names := make([]string, 0, len(m.views))
	for ws := range m.views {
		names = append(names, ws)
	}
	sort.Strings(names)
	out := make([]*frontendv1.ProgressView, 0, len(names))
	for _, ws := range names {
		out = append(out, cloneView(m.views[ws].view))
	}
	return out
}

// Subscribe registers a push channel for progress changes and returns it with
// an idempotent unsubscribe.
func (m *Manager) Subscribe() (<-chan *frontendv1.ProgressView, func()) {
	m.mu.Lock()
	defer m.mu.Unlock()
	id := m.nextSub
	m.nextSub++
	ch := make(chan *frontendv1.ProgressView, subBufferSize)
	m.subs[id] = ch
	var once sync.Once
	return ch, func() {
		once.Do(func() {
			m.mu.Lock()
			defer m.mu.Unlock()
			if c, ok := m.subs[id]; ok {
				delete(m.subs, id)
				close(c)
			}
		})
	}
}

// Close cancels any pending flush and closes every subscriber channel.
func (m *Manager) Close() error {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.closed {
		return nil
	}
	m.closed = true
	if m.stopFlush != nil {
		m.stopFlush()
		m.stopFlush = nil
	}
	for id, ch := range m.subs {
		delete(m.subs, id)
		close(ch)
	}
	return nil
}

// Flush pushes any coalesced ticker movement immediately. The deferred flush
// calls it; callers with their own cadence (a test, a shutdown) may too.
func (m *Manager) Flush() {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.flushLocked()
}

// ---------------------------------------------------------------------------
// internals
// ---------------------------------------------------------------------------

// forLocked returns the workspace's resolution, creating a blank one on first
// sight. Caller holds mu.
func (m *Manager) forLocked(workspace string) *workspaceProgress {
	if wp, ok := m.views[workspace]; ok {
		return wp
	}
	// A fresh view carries NO phase. It used to be seeded INIT so the frame's
	// phase mirror always named something renderable, and that seed was the
	// stale value the footer showed against an already-green tab: a workspace
	// can acquire a progress fact (a pending permission, a queued prompt)
	// before any state reaches this resolver, and nothing ever corrected the
	// seed afterwards. The phase now comes from the WorkspaceState the SSM
	// pushes, so a blank view is blank about the phase too.
	wp := &workspaceProgress{
		view:         &frontendv1.ProgressView{Workspace: workspace},
		countedUsage: map[string]struct{}{},
	}
	m.views[workspace] = wp
	return wp
}

// markDirtyLocked records coalesced ticker movement and arms the deferred
// flush. A negative coalescing window means "do not coalesce": the frame goes
// out at once. Caller holds mu.
func (m *Manager) markDirtyLocked(workspace string) {
	if m.window < 0 {
		if wp, ok := m.views[workspace]; ok {
			m.pushLocked(workspace, wp)
		}
		return
	}
	m.dirty[workspace] = struct{}{}
	if m.stopFlush != nil {
		return // a flush is already armed; this movement rides it
	}
	m.stopFlush = m.sched.AfterFunc(m.window, m.Flush)
}

// flushLocked pushes every dirty workspace and disarms the deferred flush.
// Caller holds mu.
func (m *Manager) flushLocked() {
	if m.stopFlush != nil {
		m.stopFlush()
		m.stopFlush = nil
	}
	if len(m.dirty) == 0 {
		return
	}
	names := make([]string, 0, len(m.dirty))
	for ws := range m.dirty {
		names = append(names, ws)
	}
	sort.Strings(names)
	for _, ws := range names {
		delete(m.dirty, ws)
		if wp, ok := m.views[ws]; ok {
			m.broadcastLocked(ws, wp)
		}
	}
}

// pushLocked broadcasts a STRUCTURAL change at once, taking any pending ticker
// movement for this workspace out with it so a coalesced value is never
// stranded. Caller holds mu.
func (m *Manager) pushLocked(workspace string, wp *workspaceProgress) {
	delete(m.dirty, workspace)
	m.broadcastLocked(workspace, wp)
}

// broadcastLocked fans one view out to every subscriber. A full subscriber
// channel is a slow consumer: dropped loudly (it recovers from the next
// Snapshot), never blocked on. Caller holds mu.
func (m *Manager) broadcastLocked(workspace string, wp *workspaceProgress) {
	if len(m.subs) == 0 {
		return
	}
	msg := cloneView(wp.view)
	for id, ch := range m.subs {
		select {
		case ch <- msg:
		default:
			m.logf("progress: subscriber %d slow; dropped ws=%s (will resync via Snapshot)", id, workspace)
		}
	}
}

// cloneView deep-copies a view so a subscriber can never observe a later
// mutation of the resolver's own state.
func cloneView(v *frontendv1.ProgressView) *frontendv1.ProgressView {
	return proto.Clone(v).(*frontendv1.ProgressView)
}

// retryDetail renders a mid-backoff retry as the footer's activity detail:
// which attempt, of how many, and the failure it is retrying.
func retryDetail(ae *datav1.ApiErrorLine) string {
	detail := fmt.Sprintf("attempt %d/%d", ae.GetRetryAttempt(), ae.GetMaxRetries())
	if msg := errorText(ae); msg != "" {
		detail += " · " + msg
	}
	return detail
}

// apiRetryDetail renders a data.ApiRetry as the footer's activity detail: the
// attempt, how long until the next one, and what failed.
//
// The failure is named by the HTTP status when there was a response and by the
// typed error enum otherwise — which is exactly the distinction `error_status_set`
// exists to carry, since a connection error that never got a response has no
// status to print and a bare 0 would read as one.
func apiRetryDetail(r *datav1.ApiRetry) string {
	parts := []string{fmt.Sprintf("attempt %d/%d", r.GetAttempt(), r.GetMaxRetries())}
	if d := r.GetRetryDelayMs(); d > 0 {
		parts = append(parts, "next in "+formatDelay(d))
	}
	if cause := retryCause(r); cause != "" {
		parts = append(parts, cause)
	}
	return strings.Join(parts, " · ")
}

// retryCause names what the retry is retrying: the HTTP status when the
// request got a response, else the typed error family.
func retryCause(r *datav1.ApiRetry) string {
	if r.GetErrorStatusSet() {
		return strconv.FormatInt(r.GetErrorStatus(), 10)
	}
	return assistantErrorName(r.GetError())
}

// retryShortNames are the footer's TERSE labels for a retry cause, keyed by
// the shared failure type rather than by the SDK enum.
//
// The enum -> type mapping used to live here, as a switch that existed only
// to build this one detail string. It now lives in errclass, which is the
// vocabulary every surface shares; what remains here is genuinely a footer
// concern — a detail row has room for "auth failed", not for the card's full
// sentence.
var retryShortNames = map[errclass.Type]string{
	errclass.TypeAPIAuthenticationFailed: "auth failed",
	errclass.TypeAPIBillingError:         "billing",
	errclass.TypeAPIRateLimit:            "rate limit",
	errclass.TypeAPIInvalidRequest:       "invalid request",
	errclass.TypeAPIServerError:          "server error",
	errclass.TypeAPIUnknown:              "unknown error",
	errclass.TypeAPIOAuthOrgNotAllowed:   "org not allowed",
	errclass.TypeAPIOverloaded:           "overloaded",
	errclass.TypeAPIModelNotFound:        "model not found",
	errclass.TypeAPIMaxOutputTokens:      "max output tokens",
}

// assistantErrorName is the short human token for an AssistantMessageError, or
// "" when the enum is unset (nothing useful to add to the detail line).
func assistantErrorName(e datav1.AssistantMessageError) string {
	t, ok := errclass.Assistant(e)
	if !ok {
		return ""
	}
	return retryShortNames[t]
}

// formatDelay renders a backoff in the coarsest unit that still reads: whole
// seconds once past a second, millis below it.
func formatDelay(ms int64) string {
	if ms < 1000 {
		return fmt.Sprintf("%dms", ms)
	}
	return fmt.Sprintf("%ds", (ms+999)/1000)
}

// errorText is the ApiErrorLine's own human account of the failure.
func errorText(ae *datav1.ApiErrorLine) string {
	return ae.GetError().GetMessage()
}

// hookDetail names the running hook: its configured name, falling back to the
// event it fired on when the name is absent.
func hookDetail(hs *datav1.HookStarted) string {
	if n := hs.GetHookName(); n != "" {
		return n
	}
	return hs.GetHookEvent()
}

// authDetail is the auth window's line: the failure when there is one, else
// the newest line of the auth prompt's own output.
func authDetail(as *datav1.AuthStatus) string {
	if e := as.GetError(); e != "" {
		return e
	}
	out := as.GetOutput()
	if len(out) == 0 {
		return ""
	}
	return out[len(out)-1]
}
