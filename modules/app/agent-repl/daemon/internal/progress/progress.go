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
// WorkspaceState is fed in via ObserveWorkspaceState, which supplies the phase
// mirror and the live-task count so this resolver never re-derives either.
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
	"log"
	"sort"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"

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
	// Logf is the loud anomaly logger. Nil defaults to log.Printf.
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
}

// New builds a Manager.
func New(opts Options) *Manager {
	logf := opts.Logf
	if logf == nil {
		logf = log.Printf
	}
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

// ObserveWorkspaceState adopts the SSM's resolved WorkspaceState as the
// footer's phase mirror and live-task count. This is the seam between the two
// resolvers: the progress view REPEATS the SSM's verdict rather than deriving a
// second opinion from the same events, so the footer's phase and the sidebar's
// can never disagree.
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
	// An UNSPECIFIED state is the SSM saying nothing, not a state to mirror:
	// adopting it would blank a phase the footer already had.
	if st := ws.GetState(); st != frontendv1.RenderState_RENDER_STATE_UNSPECIFIED {
		wp.view.State = st
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
		return
	}
	m.pushLocked(workspace, wp)
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
	default:
		// A known stream arm carrying no progress fact of its own.
	}
}

// applyRateLimitLocked folds a rate-limit report into its window.
//
// The window is open whenever the vendor reports a status OTHER than a plain
// "allowed": an "allowed_warning" (approaching the cap) or an outright limit is
// news the footer should carry, while a bare "allowed" is the vendor saying
// everything is fine and closes the window.
func (m *Manager) applyRateLimitLocked(workspace string, wp *workspaceProgress, info *datav1.RateLimitInfo) {
	status := info.GetStatus()
	active := status != "" && status != "allowed"
	cur := wp.view.GetRateLimited()
	if !active {
		if cur == nil || !cur.GetActive() {
			return
		}
		wp.view.RateLimited = nil
		m.pushLocked(workspace, wp)
		return
	}
	next := &frontendv1.RateLimitWindow{
		Active:      true,
		ResetsAt:    info.GetResetsAt(),
		Utilization: info.GetUtilization(),
		Status:      status,
	}
	if proto.Equal(cur, next) {
		return
	}
	wp.view.RateLimited = next
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
// again": the retrying window opens (or refreshes its detail) and no error is
// reported, because the turn has not failed. A line with retries EXHAUSTED is
// the terminal account of the failure: the window closes and the error summary
// takes over, addressed to the line's own item so the footer's error row can
// scroll the feed to it.
func (m *Manager) applyApiErrorLocked(workspace string, wp *workspaceProgress, uuid string, ae *datav1.ApiErrorLine, atMs int64) {
	attempt, max := ae.GetRetryAttempt(), ae.GetMaxRetries()
	retrying := max > 0 && attempt < max
	if retrying {
		m.setWindowLocked(workspace, wp, &wp.view.Retrying, true, atMs, retryDetail(ae))
		return
	}
	m.setWindowLocked(workspace, wp, &wp.view.Retrying, false, atMs, "")
	summary := errorSummary(ae)
	if wp.view.GetErrorSummary() == summary && wp.view.GetErrorItemUuid() == uuid {
		return
	}
	wp.view.ErrorSummary = summary
	wp.view.ErrorItemUuid = uuid
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
	// The error line persists until the NEXT turn starts (design decision), so
	// this is exactly where it clears.
	wp.view.ErrorSummary = ""
	wp.view.ErrorItemUuid = ""
	wp.view.Retrying = nil
	return true
}

// closeTurnLocked stops the turn clock and records an errored end. The turn's
// token figures are left standing: the idle footer shows the last turn's
// summary rather than blanking the moment the turn lands.
func (m *Manager) closeTurnLocked(wp *workspaceProgress, te *corev1.TurnEnded) {
	wp.turnOpen = false
	wp.view.TurnStartedAtMs = 0
	wp.view.Retrying = nil
	if te.GetIsError() {
		// A turn-end error carries no ApiErrorLine of its own, so the summary has
		// no addressable feed item — the uuid stays empty rather than pointing at
		// the previous error's item. An ApiErrorLine summary already standing is
		// the more specific account and is left alone.
		if wp.view.GetErrorSummary() == "" {
			wp.view.ErrorSummary = turnErrorSummary(te)
			wp.view.ErrorItemUuid = ""
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
	// A fresh view is seeded INIT, not UNSPECIFIED. A workspace can acquire
	// progress facts (a pending permission, a queued prompt) BEFORE the SSM has
	// resolved a render state for it, and INIT is the honest name for "this
	// session exists and nothing has resolved yet" — it is already in the closed
	// vocabulary. Leaving it UNSPECIFIED would push a frame whose phase mirror
	// names nothing renderable, which frontends reject.
	wp := &workspaceProgress{
		view: &frontendv1.ProgressView{
			Workspace: workspace,
			State:     frontendv1.RenderState_RENDER_STATE_INIT,
		},
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

// errorSummary renders an exhausted-retry failure as the footer's error row.
func errorSummary(ae *datav1.ApiErrorLine) string {
	msg := errorText(ae)
	if msg == "" {
		msg = "api request failed"
	}
	if n := ae.GetMaxRetries(); n > 0 {
		return fmt.Sprintf("%s (after %d attempts)", msg, n)
	}
	return msg
}

// errorText is the ApiErrorLine's own human account of the failure.
func errorText(ae *datav1.ApiErrorLine) string {
	return ae.GetError().GetMessage()
}

// turnErrorSummary is the footer's account of an errored turn end. TurnEnded
// carries no message of its own, so the stop reason is the most specific thing
// there is to say.
func turnErrorSummary(te *corev1.TurnEnded) string {
	if r := te.GetStopReason(); r != "" {
		return "turn ended in error: " + r
	}
	return "turn ended in error"
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
