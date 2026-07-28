package sessiondrv

import (
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

// Pusher is the slice of the frontend server the driver pushes to. Satisfied by
// *frontend.Server; an interface so the sink translation is unit-testable with
// a recording fake.
type Pusher interface {
	PushConversationDelta(*frontendv1.ConversationDelta)
	PushTypingDelta(*frontendv1.TypingDelta)
	PushTaskCatalog(*frontendv1.TaskCatalog)
	PushWorkspaceState(*frontendv1.WorkspaceState)
	PushSessionInitView(*frontendv1.SessionInitView)
	PushHeartbeatView(*frontendv1.HeartbeatView)
	PushQueueView(*frontendv1.QueueView)
}

// StateApplier is the slice of the SSM the driver feeds lifecycle events to.
// Satisfied by *ssm.Manager.
type StateApplier interface {
	Apply(ev *corev1.Event) error
	// ReconcileTasks adopts an AUTHORITATIVE live-task set for the session's
	// workspace, so live_task_count becomes exactly len(liveTaskIDs). Fed from
	// data.BackgroundTasksChanged, the only event carrying the whole live set.
	ReconcileTasks(sessionID string, liveTaskIDs []string) error
	// ApplyBackfillState records the workspace's transcript-backfill outcome
	// on the SSM's backfill axis. A FAILED backfill compromises the route
	// and resolves the workspace blue: an incomplete history cannot be the
	// basis of a "ready" claim.
	ApplyBackfillState(workspace, state string) error
	// ApplyConnectionDegraded records the daemon's OWN observation that the
	// shim transport went quiet, or came back (F4). The shim's DegradedState
	// events already reached the degraded axis; this transport-level miss did
	// not, so it produced a banner and no workspace color at all.
	ApplyConnectionDegraded(workspace string, degraded bool, reason string) error
	// MarkTurnInterrupted records that a USER-COMMANDED stop was delivered to
	// the workspace's running turn, so that turn's own TurnEnded reports
	// `interrupted` instead of `done` or `vendor_blocked` (I1). Fed ONLY from
	// the frontend interrupt command path — the queue's interject sends the
	// same Interrupt as machinery and must paint no outcome.
	MarkTurnInterrupted(workspace string) error
	// ApplySessionRotated reconciles the agent axis across a VENDOR SESSION
	// UUID ROTATION: the turn in flight when the uuid changed can never report
	// its end under the retired identity, so a `thinking` row held for it has
	// nothing arriving to supersede it. Fed only from the shim handshake that
	// announces the new uuid.
	ApplySessionRotated(workspace, previous, next string) error
}

// ProgressResolver is the slice of the progress-footer resolver (F1) the driver
// feeds. Satisfied by *progress.Manager.
//
// The driver feeds it the SAME event stream it feeds the SSM, plus the two
// daemon-local facts no store event carries: how many permission prompts are
// waiting on the user, and how deep the held-prompt queue is. Its Apply takes
// the workspace explicitly because a progress view is workspace-keyed and this
// resolver, unlike the SSM, holds no session→workspace binding of its own.
type ProgressResolver interface {
	Apply(workspace string, ev *corev1.Event) error
	SetCounts(workspace string, pendingPermissions, queueDepth int64)
	NoteTurnAccepted(workspace, sessionID string)
	// NoteInterrupt opens the interrupt window on the shim's ack of a
	// USER-COMMANDED stop (I1), carrying the ack's outcome verbatim. Fed ONLY
	// from the frontend interrupt command path, for the same reason
	// MarkTurnInterrupted is: an interject's stop is machinery, not a user
	// action, and opening a window for it would report a stop nobody asked
	// for.
	NoteInterrupt(workspace, sessionID string, outcome corev1.InterruptOutcome)
}

// ClearCompactStore persists the newest CLEAR-OR-COMPACTION seq per
// conversation — the frontend REPLAY FLOOR. Satisfied by
// *server.RegistrySeqStore, the same registry adapter that persists
// last_seen_seq, because the floor mark is the same kind of fact: a
// per-conversation seq high-water that must outlive the daemon.
//
// It is a store rather than a field on the live driver on purpose. The daemon
// re-Subscribes each shim from its high-water mark, so a clear or compaction
// observed before a restart is never re-delivered; an in-memory floor would
// quietly reset to "nothing seen" on every restart and hand the frontend back
// the entire conversation that clear or compaction discarded, which is the
// class of bug the floor exists to end.
//
// It deliberately does NOT reach into the shim-store. The store's API is
// contractually tiny — schema, seq, dedup, fan-out — and the daemon already
// sees every clear and every compaction as it flows, so tracking the newest one
// costs an existing registry write and asks the store for nothing.
type ClearCompactStore interface {
	// NewestClearOrCompactSeq returns the newest clear-or-compaction seq for the
	// session's conversation, or 0 when neither has ever been observed on it.
	NewestClearOrCompactSeq(sessionID string) uint64
	// SetNewestClearOrCompactSeq records seq as the newest clear or compaction.
	// Monotonic: an older seq never lowers a conversation's floor.
	SetNewestClearOrCompactSeq(sessionID string, seq uint64)
}

// noopProgress is the ProgressResolver a driver built without one falls back
// to. It exists so the progress feed is OPTIONAL for a test harness that does
// not care about it, without every feed site growing a nil check.
type noopProgress struct{}

func (noopProgress) Apply(string, *corev1.Event) error                     { return nil }
func (noopProgress) SetCounts(string, int64, int64)                        {}
func (noopProgress) NoteTurnAccepted(string, string)                       {}
func (noopProgress) NoteInterrupt(string, string, corev1.InterruptOutcome) {}

// ringCap bounds the per-session retained event ring the daemon keeps for the
// live TaskCatalog rebuild and for resync replay. It is a bounded window: older
// history is served by the store-backed replay on the next Subscribe (the store
// is the durable record), so dropping the oldest here loses nothing durable.
const ringCap = 4096

// consumer is one session's translation of the merged shim event stream into
// frontend pushes and SSM state. It implements shimclient's StateSink,
// FrameSink and DegradedReporter for a single session bound to one workspace.
// All three sink methods run on the shimclient demux goroutine in strict
// arrival order; the ring mutex guards only the retained-events slice (touched
// by both the demux and a concurrent resync).
type consumer struct {
	workspace string
	sessionID string
	push      Pusher
	ssm       StateApplier
	// prog is the progress-footer resolver, fed the same stream as the SSM.
	// Never nil (noopProgress stands in), so the feed sites stay unconditional.
	prog ProgressResolver
	// floors persists the newest clear-or-compaction seq (the replay floor).
	// Required: Config validation rejects a Manager built without one, so a
	// clear or a compaction can never be observed and silently forgotten.
	floors ClearCompactStore
	logf   func(string, ...any)
	now    func() int64
	// onSessionStarted fires when a SessionStarted event arrives, letting the
	// driver arm the metaprompt re-fire for a RESUME/COMPACT_CONTINUE session.
	onSessionStarted func(*corev1.SessionStarted)
	// onTurn reports an observed turn boundary (true = TurnStarted, false =
	// TurnEnded). It drives the prompt queue's interception and drain (E4).
	// Called on the shim read-loop goroutine, so the handler must not block on
	// anything that needs that loop to make progress — notably an Ack-awaiting
	// send back to the same shim.
	onTurn func(active bool)
	// onBackfill reports a never-blue backfill transition (F2), once per
	// distinct state. The driver persists it and re-pushes the SessionView.
	onBackfill func(state string)
	// onSessionEnded reports that the session's shim reported it is over (F4).
	// Nothing marked a record terminal on shim death before this, so
	// RENDER_STATE_DEAD and death_reason sat on two disconnected axes: the
	// workspace went dead with no account of why, and the one death reason the
	// registry documented was never written by anything.
	onSessionEnded func()

	mu   sync.Mutex
	ring []*corev1.Event
	// systemInit is the last SDK system:init snapshot seen on this session's
	// stream (a data.v1 SystemInit inside a vendor event). It backs the daemon's
	// HTTP /status and /commands routes now that the L2 translator that used to
	// cache it is gone. Nil until the first init lands (honest empty).
	systemInit *datav1.SystemInit
	// permItems retains the LATEST permission ConversationItem per request_id,
	// in first-seen order, so a resync replays each permission's current
	// resolution (S8). The retained ring holds core.v1.Events; a permission item
	// is a daemon-composed frontend item with no store seq, so it lives here
	// beside the ring and is replayed on every resync.
	permItems map[string]*frontendv1.ConversationItem
	permOrder []string
	// failItems retains the LATEST system-failure ConversationItem per uuid,
	// in first-seen order, on the same footing as permItems and for the same
	// reason (F4). A WINDOW-shaped failure is re-sent under its opening uuid
	// with resolved_at_ms set, so the retained copy is the SETTLED card and a
	// resync replays that rather than re-opening the alarm.
	failItems map[string]*frontendv1.ConversationItem
	failOrder []string
	// backfill is the last never-blue state reported for this session (F2).
	// In-memory latch: it is what keeps a long transcript from writing the
	// registry record once per line.
	backfill string
}

func newConsumer(workspace, sessionID string, push Pusher, applier StateApplier, prog ProgressResolver, floors ClearCompactStore, logf func(string, ...any), onSessionStarted func(*corev1.SessionStarted), onTurn func(active bool), onBackfill func(state string), onSessionEnded func()) *consumer {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	if prog == nil {
		prog = noopProgress{}
	}
	return &consumer{
		workspace:        workspace,
		sessionID:        sessionID,
		push:             push,
		ssm:              applier,
		prog:             prog,
		floors:           floors,
		logf:             logf,
		now:              func() int64 { return time.Now().UnixMilli() },
		onSessionStarted: onSessionStarted,
		onTurn:           onTurn,
		onBackfill:       onBackfill,
		onSessionEnded:   onSessionEnded,
	}
}

// retain appends ev to the bounded ring, dropping the oldest past ringCap.
func (c *consumer) retain(ev *corev1.Event) {
	c.mu.Lock()
	c.ring = append(c.ring, ev)
	if len(c.ring) > ringCap {
		// Drop the oldest quarter in one shift so the trim is amortized O(1).
		drop := ringCap / 4
		c.ring = append(c.ring[:0], c.ring[drop:]...)
	}
	c.mu.Unlock()
}

// snapshotRing returns a shallow copy of the retained events for catalog
// rebuilds and resync, taken under the lock so a concurrent retain cannot race
// the read.
func (c *consumer) snapshotRing() []*corev1.Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*corev1.Event, len(c.ring))
	copy(out, c.ring)
	return out
}

// latestSystemInit returns the last SDK system:init snapshot seen on this
// session's stream, or nil before the first init lands.
func (c *consumer) latestSystemInit() *datav1.SystemInit {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.systemInit
}

// systemInitFromVendor decodes a vendor event's Any into its SystemInit arm, or
// nil when the Any is not a ClaudeStreamMessage carrying a system:init (every
// vendor event shares the same Any type URL; the inner oneof is the
// discriminator).
func systemInitFromVendor(a *anypb.Any) *datav1.SystemInit {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return csm.GetSystemInit()
}

// commandsChangedFromVendor decodes a vendor event's Any into its
// CommandsChanged arm, or nil when it is anything else.
func commandsChangedFromVendor(a *anypb.Any) *datav1.CommandsChanged {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return csm.GetCommandsChanged()
}

// applyCommandsChanged folds a system/commands_changed push into the retained
// SystemInit and returns the refreshed snapshot to re-publish, or nil when
// there is nothing to fold it into yet.
//
// The SDK's contract for this message is REPLACE, not merge: the payload is
// the complete current command list after a mid-session change (a skill
// discovered as the agent moves into a subdirectory, a plugin installed). So
// the retained slash-command list is overwritten wholesale.
//
// The retained SystemInit is CLONED rather than mutated: the very same pointer
// was already handed to frontends in an earlier SessionInitView, and editing
// it in place would retroactively rewrite a snapshot they are still holding.
//
// A push arriving before any init has nothing to update. That is not an error
// worth failing on — the init that follows carries the current list anyway —
// but it IS worth saying out loud rather than silently discarding a command
// list the user's menu is waiting for.
func (c *consumer) applyCommandsChanged(cc *datav1.CommandsChanged) *datav1.SystemInit {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.systemInit == nil {
		c.logf("sessiondrv: commands_changed before system:init session=%s commands=%d; dropped (the next init carries the current list)",
			c.sessionID, len(cc.GetCommands()))
		return nil
	}
	names := make([]string, 0, len(cc.GetCommands()))
	for _, cmd := range cc.GetCommands() {
		names = append(names, cmd.GetName())
	}
	refreshed, _ := proto.Clone(c.systemInit).(*datav1.SystemInit)
	refreshed.SlashCommands = names
	c.systemInit = refreshed
	return refreshed
}

// Apply feeds a lifecycle event to the SSM and refreshes the TaskCatalog on
// task-lifecycle transitions (design step 1). It also fires onSessionStarted so
// the driver can arm the metaprompt re-fire. An SSM apply error is loud-logged,
// never swallowed — but it does not stop the stream (the SSM has already logged
// its own cause).
func (c *consumer) Apply(ev *corev1.Event) {
	c.retain(ev)
	if ss := ev.GetSessionStarted(); ss != nil && c.onSessionStarted != nil {
		c.onSessionStarted(ss)
	}
	// The prompt queue keys entirely off the observed turn boundary (E4): a
	// prompt is intercepted while a turn is running, and delivered when one
	// actually ends. Reading it from the event stream — rather than from the
	// SSM's derived turn_active, or from "we just sent an interrupt" — is what
	// makes the interject sequence evented instead of a race against teardown.
	if c.onTurn != nil {
		switch ev.GetPayload().(type) {
		case *corev1.Event_TurnStarted:
			c.onTurn(true)
		case *corev1.Event_TurnEnded:
			c.onTurn(false)
		}
	}
	if err := c.ssm.Apply(ev); err != nil {
		c.logf("sessiondrv: ssm apply failed session=%s seq=%d kind=%s: %v",
			c.sessionID, ev.GetSeq(), stateKind(ev), err)
	}
	c.applyProgress(ev)
	switch ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted, *corev1.Event_TaskEnded:
		catalog := frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.snapshotRing())
		c.logf("sessiondrv: task catalog push session=%s ws=%s seq=%d event=%s tasks=%d",
			c.sessionID, c.workspace, ev.GetSeq(), stateKind(ev), len(catalog.GetTasks()))
		c.push.PushTaskCatalog(catalog)
	case *corev1.Event_TaskProgress:
		// TaskProgress can fire hundreds of times per second, but TaskCatalog has
		// no progress fields and BuildTaskCatalog deliberately ignores it. Do not
		// log or broadcast this hot no-change path.
	case *corev1.Event_SessionEnded:
		// The SAME event the SSM resolves to RENDER_STATE_DEAD also records
		// WHY, so the color and its account cannot disagree. Before this the
		// SSM went dead and the record stayed silent.
		if c.onSessionEnded != nil {
			c.onSessionEnded()
		}
	}
}

// Consume translates a data/ephemeral event into a frontend push (design step
// 1): complete vendor messages become a ConversationDelta stamped with
// through_seq; ContentDelta and HeartbeatProgress become ephemeral TypingDelta
// relays. A vendor payload that cannot be translated is a loud error, never a
// silent drop.
func (c *consumer) Consume(ev *corev1.Event) {
	c.retain(ev)
	c.applyProgress(ev)
	c.observeBackfill(ev)
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_ContentDelta:
		if td := frontend.TypingDeltaFromContentDelta(c.workspace, c.sessionID, p.ContentDelta); td != nil {
			c.push.PushTypingDelta(td)
		}
	case *corev1.Event_HeartbeatProgress:
		// E4: relayed as HeartbeatView. Under S9 this was a schema-forced DROP —
		// TypingDelta carries only a ContentDelta and there was no other arm to
		// put a tool-progress heartbeat in. HeartbeatView is that arm, so the
		// liveness signal now reaches the frontend instead of being logged away.
		if hv := frontend.HeartbeatViewFromProgress(c.workspace, c.sessionID, p.HeartbeatProgress); hv != nil {
			c.push.PushHeartbeatView(hv)
		}
	case *corev1.Event_Vendor:
		if si := systemInitFromVendor(p.Vendor); si != nil {
			c.mu.Lock()
			c.systemInit = si
			c.mu.Unlock()
			// The session's retained SystemInit just became available (attach or a
			// fresh init): push it as a SessionInitView so frontends can source
			// their slash-command/tools/model menus from it (S9), replacing the
			// Emacs GET /commands HTTP menu.
			c.push.PushSessionInitView(&frontendv1.SessionInitView{
				Workspace: c.workspace,
				SessionId: c.sessionID,
				Init:      si,
			})
		}
		// system/commands_changed is the SDK's mid-session command-list push
		// (a skill discovered as the agent moves, a plugin installed). Before
		// this arm existed the menu froze at whatever the session's one init
		// reported, and only an explicit refresh could unstick it. Folding it
		// into the retained SystemInit and re-publishing means the frontend's
		// menu simply follows, through the SessionInitView it already consumes.
		if cc := commandsChangedFromVendor(p.Vendor); cc != nil {
			if refreshed := c.applyCommandsChanged(cc); refreshed != nil {
				c.push.PushSessionInitView(&frontendv1.SessionInitView{
					Workspace: c.workspace,
					SessionId: c.sessionID,
					Init:      refreshed,
				})
			}
		}
		// The SDK's full live-task set. It is the one event that can CLOSE the
		// ghost class rather than adding to it, so it drives both task planes
		// (the SSM counter and the roster) as an authority.
		if btc := frontend.BackgroundTasksFromVendor(p.Vendor); btc != nil {
			c.reconcileTasks(ev, btc)
		}
		c.pushConversation(ev, true)
	case *corev1.Event_ContextCleared, *corev1.Event_ContextCompacted:
		// A clear and a compaction each do two things at once: they render as
		// their own bubble, and they RAISE this conversation's replay floor so no
		// reconnecting frontend is ever served the history they discarded. The
		// floor is recorded FIRST — one pushed but not recorded would be drawn
		// once and then buried under a replay of everything above it.
		c.noteClearOrCompact(ev)
		c.pushConversation(ev, true)
	default:
		// UnparsedEvent / empty payloads carry no conversation content of their
		// own; the demux already loud-logged them. Nothing to push.
	}
}

// noteClearOrCompact records a clear or a compaction as the conversation's
// newest replay floor.
//
// A seq-less one is NOT recorded: seq 0 means the event never reached the store
// (producer → store, pre-ingest), and a floor derived from it would be no floor
// at all. It is loud-logged instead of being taken as "nothing happened", since
// a clear or compaction the daemon saw but cannot position is a real anomaly.
func (c *consumer) noteClearOrCompact(ev *corev1.Event) {
	seq := ev.GetSeq()
	logf := dlog.Tag(dlog.Logf(c.logf),
		"session", c.sessionID, "ws", c.workspace, "kind", stateKind(ev),
		"seq", seq, "dedup_key", ev.GetDedupKey())
	if seq == 0 {
		logf("sessiondrv: replay floor NOT raised — this clear or compaction carries no store seq, so it has no position to floor at")
		return
	}
	logf("sessiondrv: replay floor raised to this clear or compaction")
	c.floors.SetNewestClearOrCompactSeq(c.sessionID, seq)
}

// reconcileTasks adopts a `BackgroundTasksChanged` snapshot as the
// authoritative live-task set on BOTH task planes:
//
//   - the SSM's live_task_count, which appends reconciliation rows so the
//     derived count equals the list exactly (it is the only thing that can
//     settle the `IMPOSSIBLE live_task_count=-N` class, where replayed
//     historical task_ended events arrive with no logged task_started); and
//   - the frontend TaskCatalog, rebuilt from the ring — which already holds
//     THIS event, since Consume retains before it translates — so the roster
//     sweeps its ghosts in the same push.
//
// An SSM failure is loud-logged and does not stop the roster refresh: the two
// planes are independent, and losing both over one failure would be worse.
func (c *consumer) reconcileTasks(ev *corev1.Event, btc *datav1.BackgroundTasksChanged) {
	ids := make([]string, 0, len(btc.GetTasks()))
	for _, ref := range btc.GetTasks() {
		if ref.GetTaskId() != "" {
			ids = append(ids, ref.GetTaskId())
		}
	}
	c.logf("sessiondrv: authoritative live-task set session=%s ws=%s seq=%d tasks=%d",
		c.sessionID, c.workspace, ev.GetSeq(), len(ids))
	// Keyed by the EVENT's session id: the SSM resolves a workspace from
	// whichever identity the event carries, and a store event carries the
	// vendor uuid rather than the daemon's s_ id.
	if err := c.ssm.ReconcileTasks(ev.GetSessionId(), ids); err != nil {
		c.logf("sessiondrv: task reconciliation failed session=%s seq=%d: %v", c.sessionID, ev.GetSeq(), err)
	}
	c.push.PushTaskCatalog(frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.snapshotRing()))
}

// Backfill states persisted on the registry record and mapped onto
// frontendv1.BackfillState by the server. Strings (not the enum) because the
// registry is JSON on disk and a numeric enum there would be unreadable.
const (
	BackfillPending = "pending"
	BackfillDone    = "done"
	BackfillFailed  = "failed"
)

// sidecarProducer is the UnparsedEvent producer tag the shim-claude-sidecar
// stamps on a transcript line it could not read. It is the ONLY sidecar
// failure that reaches the daemon durably (see frontendv1.BackfillState).
const sidecarProducer = "shim-claude-sidecar"

// noteBackfill records a backfill transition for this session, ONCE.
//
// Called on every qualifying event, so the in-memory latch is what keeps a
// long transcript from writing the registry record per line. FAILED is
// terminal for the session: a transcript the sidecar could not fully read does
// not become readable by reading more of it, and letting a later good line
// flip it back to DONE would hide exactly the partial-history case this signal
// exists to surface.
func (c *consumer) noteBackfill(state string) {
	c.mu.Lock()
	if c.backfill == state || c.backfill == BackfillFailed {
		c.mu.Unlock()
		return
	}
	c.backfill = state
	c.mu.Unlock()
	c.logf("sessiondrv: backfill %s session=%s ws=%s", state, c.sessionID, c.workspace)
	if c.onBackfill != nil {
		c.onBackfill(state)
	}
}

// settleBackfillFromStore settles the backfill for a session whose history is
// ALREADY in the store, using the durable high-water as the evidence.
//
// THE REOPEN WEDGE this exists to close: observeBackfill can only ever see a
// backfill happen — it flips DONE when a TranscriptLine ARRIVES. On reopening
// a session whose transcript was fully ingested in an earlier run, the
// sidecar's cursor already sits at the tail of that file, so no new line is
// ever written and no new event ever arrives. The backfill was complete before
// this daemon started, and waiting for evidence of it happening again would
// wait forever — leaving a workspace with a complete, replayable history stuck
// reporting "still starting" and therefore stuck BLUE, permanently, on the
// most ordinary action there is.
//
// A nonzero high-water is proof of exactly the right fact: the daemon has
// durably observed store events for this session, which means the file plane
// really did deliver into the store. It is the same conclusion
// observeBackfill draws, read from the accumulated record rather than from a
// live arrival.
//
// It never overwrites a state already known — in particular never a FAILED,
// which is terminal (a transcript the sidecar could not fully read does not
// become readable by reopening it).
func (c *consumer) settleBackfillFromStore(highWater uint64) {
	if highWater == 0 {
		// No durable events for this session: genuinely nothing ingested
		// yet, so the live path is the one that will settle it.
		return
	}
	c.mu.Lock()
	already := c.backfill != ""
	c.mu.Unlock()
	if already {
		return
	}
	c.logf("sessiondrv: backfill settled from store high-water session=%s ws=%s seq=%d (history already ingested; no new line will arrive)",
		c.sessionID, c.workspace, highWater)
	c.noteBackfill(BackfillDone)
}

// observeBackfill derives the never-blue backfill signal from the two pieces
// of evidence that actually reach the daemon (the sidecar reports nothing of
// its own — see frontendv1.BackfillState):
//
//   - a data.v1.TranscriptLine vendor event means the FILE plane delivered
//     into the store, which is what "backfilled" means;
//   - an UnparsedEvent stamped by the sidecar means it hit a line of that
//     transcript it could not read.
//
// A stream-plane event proves nothing about the file plane and is ignored
// here: a live turn writes ClaudeStreamMessage events for a session whose
// history never arrived, which is precisely the blue-but-live case.
//
// This sees only backfills that HAPPEN. A session reopened with its history
// already ingested is settled by settleBackfillFromStore instead.
func (c *consumer) observeBackfill(ev *corev1.Event) {
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_Unparsed:
		if p.Unparsed.GetProducer() == sidecarProducer {
			c.logf("sessiondrv: sidecar could not read transcript line session=%s path=%s offset=%d: %s",
				c.sessionID, p.Unparsed.GetSourcePath(), p.Unparsed.GetByteOffset(), p.Unparsed.GetError())
			c.noteBackfill(BackfillFailed)
		}
	case *corev1.Event_Vendor:
		if isTranscriptLine(p.Vendor) {
			c.noteBackfill(BackfillDone)
		}
	}
}

// isTranscriptLine reports whether a vendor Any carries a file-plane
// TranscriptLine. Matched on the Any's type identity rather than by
// unmarshaling it: this runs on every vendor event, and which ARM it is is the
// entire question being asked.
func isTranscriptLine(a *anypb.Any) bool {
	return a != nil && a.MessageIs((*datav1.TranscriptLine)(nil))
}

// applyProgress folds an event into the progress-footer resolver. Both event
// planes route through here — the lifecycle plane (Apply) carries the turn
// boundaries and the data plane (Consume) carries the tickers and windows — so
// the resolver sees the whole stream exactly once per event.
//
// A fold failure is loud-logged, never swallowed, and never stops the stream:
// the footer degrading is not a reason to stop delivering conversation.
func (c *consumer) applyProgress(ev *corev1.Event) {
	if err := c.prog.Apply(c.workspace, ev); err != nil {
		c.logf("sessiondrv: progress apply failed session=%s seq=%d kind=%s: %v",
			c.sessionID, ev.GetSeq(), stateKind(ev), err)
	}
}

// userTurnReceipt extracts the round-trip receipt a pushed delta carries for a
// USER PROMPT: the request id and total prompt-text length across its
// user_message items. textLen 0 means the delta carries no prompt — a pure
// tool-result feedback message rides the user_message arm too, and logging
// every tool result would bury the one receipt per prompt this exists for.
func userTurnReceipt(cd *frontendv1.ConversationDelta) (requestID string, textLen int) {
	for _, it := range cd.GetItems() {
		um := it.GetUserMessage()
		if um == nil {
			continue
		}
		n := 0
		switch content := um.GetContent().(type) {
		case *datav1.ApiUserMessage_ContentString:
			n = len(content.ContentString)
		case *datav1.ApiUserMessage_ContentBlocks:
			for _, b := range content.ContentBlocks.GetBlocks() {
				if t := b.GetText(); t != nil {
					n += len(t.GetText())
				}
			}
		}
		if n > 0 {
			requestID = it.GetRequestId()
			textLen += n
		}
	}
	return requestID, textLen
}

// pushConversation converts a vendor event to a ConversationDelta and pushes it,
// loud-logging (never swallowing) a translation failure.
func (c *consumer) pushConversation(ev *corev1.Event, live bool) {
	cd, err := frontend.ConversationDeltaFromEvent(c.workspace, ev)
	if err != nil {
		c.logf("sessiondrv: conversation translate failed session=%s seq=%d: %v", c.sessionID, ev.GetSeq(), err)
		return
	}
	if cd == nil {
		return // known-but-non-conversational vendor payload
	}
	c.push.PushConversationDelta(cd)
	// The prompt round-trip receipt: one line per LIVE user prompt reaching
	// the frontend push, closing the gap between "control request acked" (the
	// shim took the prompt) and the webapp's own mount log. Live only — a
	// resync/repull replays thousands of historical user turns, and a receipt
	// per replayed prompt would bury the one that answers "did MY prompt come
	// back".
	if live {
		if requestID, textLen := userTurnReceipt(cd); textLen > 0 {
			c.logf("sessiondrv: user turn pushed ws=%q session=%s seq=%d request_id=%s len=%d",
				c.workspace, c.sessionID, ev.GetSeq(), requestID, textLen)
		}
	}
}

// connectionComponent names the daemon's own transport in a degraded card.
const connectionComponent = "shim-connection"

// Degraded surfaces a shim-sourced DegradedState as a self-resolving failure
// card (F4).
//
// It used to push a DegradedNotice: chrome that scrolled away, carried no
// correlation id, and threw dropped_count away in translation. A user whose
// workspace changed color needs to find out why from the conversation itself,
// so the account lives there now.
func (c *consumer) Degraded(_ string, ds *corev1.DegradedState) {
	item := frontend.SystemFailureItemFromDegradedState(ds, c.now())
	if item == nil {
		return
	}
	c.pushFailure(c.degradedUUID(ds.GetComponent()), item)
}

// ConnectionDegraded surfaces a transport-level missed-heartbeat window: the
// SSM's degraded axis PLUS the opening edge of a failure card.
//
// The SSM row is the half that never existed. The missed-heartbeat window
// called this sink and nothing appended a state row, so the transport going
// quiet produced a banner and no workspace color at all — the ambience the
// banner was standing in for had no other home.
func (c *consumer) ConnectionDegraded(_ string, reason string) {
	c.applyConnectionDegraded(true, reason)
	c.pushFailure(c.degradedUUID(connectionComponent), errclass.ConnectionDegraded(reason))
}

// ConnectionRecovered closes the window: the SSM's degraded axis clears and
// the SAME card is re-sent with resolved_at_ms stamped.
//
// Re-sending under the opening card's uuid is what makes it ONE card that
// settles rather than two cards that accumulate — the recovery report used to
// carry neither a reason nor any correlation to what it was recovering from.
func (c *consumer) ConnectionRecovered(_ string) {
	c.applyConnectionDegraded(false, "")
	item := errclass.ConnectionDegraded("")
	item.ResolvedAtMs = c.now()
	c.pushFailure(c.degradedUUID(connectionComponent), item)
}

// applyConnectionDegraded moves the SSM's degraded axis, loud-logging a
// failure rather than swallowing it: a workspace whose color silently failed
// to move is the exact misreport this axis exists to prevent.
func (c *consumer) applyConnectionDegraded(degraded bool, reason string) {
	if err := c.ssm.ApplyConnectionDegraded(c.workspace, degraded, reason); err != nil {
		c.logf("sessiondrv: applying connection degraded=%v to the SSM (ws %q): %v", degraded, c.workspace, err)
	}
}

// degradedUUID is the STABLE card identity for one component's degraded
// window on this session. Both edges of the window derive the same id, which
// is what lets the closing edge reconcile the opening card in place.
func (c *consumer) degradedUUID(component string) string {
	if component == "" {
		component = connectionComponent
	}
	return "degraded:" + c.sessionID + ":" + component
}

// resync replays the retained conversation deltas from fromSeq (0 = from the
// start of the retained window) via the normal PushConversationDelta path. It
// is idempotent by construction: the frontends reconcile by through_seq/uuid,
// so re-pushing already-seen items REPLACES rather than duplicates them.
//
// It returns the ring's FLOOR: the oldest seq this replay could possibly have
// covered. A caller asking below the floor was answered incompletely, and the
// floor is what tells it so — the bounded store re-pull (repull.go) closes the
// remainder. Before the floor was reported, "older than the retained window"
// was answered with silence, which is what left a freshly-mounted GUI blank
// after a daemon restart (the ring is empty then, so EVERY request is
// below-floor).
func (c *consumer) resync(fromSeq uint64) (floor uint64, haveFloor bool) {
	for _, ev := range c.snapshotRing() {
		if ev.GetSeq() < fromSeq {
			continue
		}
		// INCLUSIVE of fromSeq. When the caller raised it to a clear or a
		// compaction, that event is the FIRST thing replayed: a frontend that
		// discards its history at the floor and never receives the clear or the
		// compaction has nothing to draw and no reason to have discarded.
		//
		// Every retained event is offered to the curator, which decides what
		// carries conversation content. Filtering to vendor payloads here is
		// what kept the first-class clear and compaction out of every replay.
		c.pushConversation(ev, false)
	}
	// Replay the retained permission items too: they carry no store seq (they
	// are daemon-composed, not store events), so a pending or resolved
	// permission is re-presented on reconnect regardless of fromSeq. Idempotent
	// by uuid (the permission request_id) — a re-push REPLACES.
	for _, item := range c.snapshotPermItems() {
		c.pushPermissionDelta(item)
	}
	// Same for the retained failure cards (F4): they carry no store seq
	// either, and a reconnecting frontend that could not see WHY its
	// workspace is off-green is the gap the cards exist to close.
	for _, item := range c.snapshotFailItems() {
		c.pushPermissionDelta(item)
	}
	return c.ringFloor()
}

// ringFloor reports the oldest store seq the retained ring still holds — the
// first seq a resync replay can cover — and whether the ring holds one at all.
//
// ok=false means the ring carries no seq-bearing event: it was emptied by the
// cap, or (the case that matters) this is a freshly restarted daemon whose ring
// has not filled yet. A caller cannot derive the floor from the ring then, and
// must fall back to the DURABLE last_seen_seq — see Manager.Resync.
//
// Reporting 0 in that case would claim the ring covers all of history, which is
// the silent answer this whole mechanism exists to replace.
func (c *consumer) ringFloor() (uint64, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, ev := range c.ring {
		if seq := ev.GetSeq(); seq > 0 {
			return seq, true
		}
	}
	return 0, false
}

// pushPermission retains and pushes a permission ConversationItem, keyed by its
// uuid (the permission request_id) so a resync replays the latest resolution.
// A same-uuid push REPLACES the retained item, tracking the resolution
// lifecycle (PENDING -> ALLOWED/DENIED/ABANDONED). This is the S8 permission
// surface pushed through the NORMAL retained pusher path so resync replays it.
func (c *consumer) pushPermission(item *frontendv1.ConversationItem) {
	c.mu.Lock()
	if c.permItems == nil {
		c.permItems = map[string]*frontendv1.ConversationItem{}
	}
	if _, seen := c.permItems[item.GetUuid()]; !seen {
		c.permOrder = append(c.permOrder, item.GetUuid())
	}
	c.permItems[item.GetUuid()] = item
	c.mu.Unlock()
	c.pushPermissionDelta(item)
}

// pushPermissionDelta wraps a single permission item in a ConversationDelta and
// pushes it (no store seq: through_seq stays 0, daemon-local).
func (c *consumer) pushPermissionDelta(item *frontendv1.ConversationItem) {
	c.push.PushConversationDelta(&frontendv1.ConversationDelta{
		Workspace: c.workspace,
		SessionId: c.sessionID,
		Items:     []*frontendv1.ConversationItem{item},
	})
}

// pushFailure retains and pushes a system-failure ConversationItem under uuid,
// on the same retained-and-replayed path permissions use (F4).
//
// Keying by uuid is what makes a WINDOW-shaped failure one card: the closing
// edge REPLACES the retained item with its resolved twin, so a resync replays
// the settled card rather than re-opening an alarm about something that
// already ended.
func (c *consumer) pushFailure(uuid string, failure *frontendv1.SystemFailureItem) {
	item := &frontendv1.ConversationItem{
		Uuid: uuid,
		TsMs: c.now(),
		Item: &frontendv1.ConversationItem_SystemFailure{SystemFailure: failure},
	}
	// The failure carries its own addressing so a surface reached OUTSIDE the
	// feed (a footer row) can still scroll to this card.
	failure.ItemUuid = uuid

	c.mu.Lock()
	if c.failItems == nil {
		c.failItems = map[string]*frontendv1.ConversationItem{}
	}
	if _, seen := c.failItems[uuid]; !seen {
		c.failOrder = append(c.failOrder, uuid)
	}
	c.failItems[uuid] = item
	c.mu.Unlock()

	c.logf("sessiondrv: system failure session=%s uuid=%s type=%s resolved=%v: %s",
		c.sessionID, uuid, failure.GetErrorType(), failure.GetResolvedAtMs() != 0, failure.GetSourceDetail())
	c.pushPermissionDelta(item)
}

// snapshotFailItems returns the retained failure items in first-seen order,
// taken under the lock so a concurrent pushFailure cannot race the read.
func (c *consumer) snapshotFailItems() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.failOrder))
	for _, id := range c.failOrder {
		out = append(out, c.failItems[id])
	}
	return out
}

// snapshotPermItems returns the retained permission items in first-seen order,
// taken under the lock so a concurrent pushPermission cannot race the read.
func (c *consumer) snapshotPermItems() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.permOrder))
	for _, id := range c.permOrder {
		out = append(out, c.permItems[id])
	}
	return out
}

// permissionItem composes a permission ConversationItem: the request plus its
// resolution, keyed by the request_id as the item uuid (the reconciliation key
// frontends replace on). denyMessage is set only on RESOLUTION_DENIED.
func permissionItem(req *corev1.PermissionRequest, res corev1.PermissionItem_Resolution, denyMessage string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid: req.GetRequestId(),
		Item: &frontendv1.ConversationItem_Permission{Permission: &corev1.PermissionItem{
			Request:     req,
			Resolution:  res,
			DenyMessage: denyMessage,
		}},
	}
}

// stateKind names a lifecycle event's payload for logging.
func stateKind(ev *corev1.Event) string {
	switch ev.GetPayload().(type) {
	case *corev1.Event_SessionStarted:
		return "session_started"
	case *corev1.Event_SessionEnded:
		return "session_ended"
	case *corev1.Event_TurnStarted:
		return "turn_started"
	case *corev1.Event_TurnEnded:
		return "turn_ended"
	case *corev1.Event_TaskStarted:
		return "task_started"
	case *corev1.Event_TaskProgress:
		return "task_progress"
	case *corev1.Event_TaskEnded:
		return "task_ended"
	default:
		return "other"
	}
}
