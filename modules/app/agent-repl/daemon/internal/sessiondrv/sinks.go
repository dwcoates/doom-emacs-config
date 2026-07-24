package sessiondrv

import (
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"

	"google.golang.org/protobuf/types/known/anypb"
)

// Pusher is the slice of the frontend server the driver pushes to. Satisfied by
// *frontend.Server; an interface so the sink translation is unit-testable with
// a recording fake.
type Pusher interface {
	PushConversationDelta(*frontendv1.ConversationDelta)
	PushTypingDelta(*frontendv1.TypingDelta)
	PushTaskCatalog(*frontendv1.TaskCatalog)
	PushDegradedNotice(*frontendv1.DegradedNotice)
	PushWorkspaceState(*frontendv1.WorkspaceState)
	PushSessionInitView(*frontendv1.SessionInitView)
}

// StateApplier is the slice of the SSM the driver feeds lifecycle events to.
// Satisfied by *ssm.Manager.
type StateApplier interface {
	Apply(ev *corev1.Event) error
}

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
	logf      func(string, ...any)
	now       func() int64
	// onSessionStarted fires when a SessionStarted event arrives, letting the
	// driver arm the metaprompt re-fire for a RESUME/COMPACT_CONTINUE session.
	onSessionStarted func(*corev1.SessionStarted)

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
}

func newConsumer(workspace, sessionID string, push Pusher, applier StateApplier, logf func(string, ...any), onSessionStarted func(*corev1.SessionStarted)) *consumer {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &consumer{
		workspace:        workspace,
		sessionID:        sessionID,
		push:             push,
		ssm:              applier,
		logf:             logf,
		now:              func() int64 { return time.Now().UnixMilli() },
		onSessionStarted: onSessionStarted,
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
	if err := c.ssm.Apply(ev); err != nil {
		c.logf("sessiondrv: ssm apply failed session=%s seq=%d kind=%s: %v",
			c.sessionID, ev.GetSeq(), stateKind(ev), err)
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted, *corev1.Event_TaskProgress, *corev1.Event_TaskEnded:
		c.push.PushTaskCatalog(frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.snapshotRing()))
	}
}

// Consume translates a data/ephemeral event into a frontend push (design step
// 1): complete vendor messages become a ConversationDelta stamped with
// through_seq; ContentDelta and HeartbeatProgress become ephemeral TypingDelta
// relays. A vendor payload that cannot be translated is a loud error, never a
// silent drop.
func (c *consumer) Consume(ev *corev1.Event) {
	c.retain(ev)
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_ContentDelta:
		if td := frontend.TypingDeltaFromContentDelta(c.workspace, c.sessionID, p.ContentDelta); td != nil {
			c.push.PushTypingDelta(td)
		}
	case *corev1.Event_HeartbeatProgress:
		// The S9 TypingDelta carries only a core.v1.ContentDelta; a tool-progress
		// heartbeat is not a ContentDelta and has no other frontend.v1 arm, so it
		// is intentionally not relayed (a schema-forced drop, loud-logged once so
		// the gap is visible rather than silent). It stays an EPHEMERAL event the
		// store never persists, so nothing durable is lost.
		c.logf("sessiondrv: heartbeat progress not relayed (no frontend.v1 arm under S9) session=%s tool=%s",
			c.sessionID, p.HeartbeatProgress.GetToolUseId())
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
		c.pushConversation(ev)
	default:
		// UnparsedEvent / empty payloads carry no conversation content of their
		// own; the demux already loud-logged them. Nothing to push.
	}
}

// pushConversation converts a vendor event to a ConversationDelta and pushes it,
// loud-logging (never swallowing) a translation failure.
func (c *consumer) pushConversation(ev *corev1.Event) {
	cd, err := frontend.ConversationDeltaFromEvent(c.workspace, ev)
	if err != nil {
		c.logf("sessiondrv: conversation translate failed session=%s seq=%d: %v", c.sessionID, ev.GetSeq(), err)
		return
	}
	if cd == nil {
		return // known-but-non-conversational vendor payload
	}
	c.push.PushConversationDelta(cd)
}

// Degraded surfaces a shim-sourced DegradedState as a frontend DegradedNotice.
func (c *consumer) Degraded(_ string, ds *corev1.DegradedState) {
	if n := frontend.DegradedNoticeFromState(ds, c.now()); n != nil {
		c.push.PushDegradedNotice(n)
	}
}

// ConnectionDegraded surfaces a transport-level missed-heartbeat window as a
// DegradedNotice (component shim-connection), honest reporting of a stale
// display — not a fallback.
func (c *consumer) ConnectionDegraded(_ string, reason string) {
	c.push.PushDegradedNotice(&frontendv1.DegradedNotice{
		Component: "shim-connection",
		Reason:    reason,
		Recovered: false,
		AtMs:      c.now(),
	})
}

// ConnectionRecovered clears the shim-connection degraded notice.
func (c *consumer) ConnectionRecovered(_ string) {
	c.push.PushDegradedNotice(&frontendv1.DegradedNotice{
		Component: "shim-connection",
		Recovered: true,
		AtMs:      c.now(),
	})
}

// resync replays the retained conversation deltas from fromSeq (0 = from the
// start of the retained window) via the normal PushConversationDelta path. It
// is idempotent by construction: the frontends reconcile by through_seq/uuid,
// so re-pushing already-seen items REPLACES rather than duplicates them. This
// is the simplest honest mechanism (task step 5): the daemon replays its
// bounded retained ring; history older than the window is recovered by the
// store-backed Subscribe replay on reconnect.
func (c *consumer) resync(fromSeq uint64) {
	for _, ev := range c.snapshotRing() {
		if ev.GetSeq() < fromSeq {
			continue
		}
		if ev.GetVendor() != nil {
			c.pushConversation(ev)
		}
	}
	// Replay the retained permission items too: they carry no store seq (they
	// are daemon-composed, not store events), so a pending or resolved
	// permission is re-presented on reconnect regardless of fromSeq. Idempotent
	// by uuid (the permission request_id) — a re-push REPLACES.
	for _, item := range c.snapshotPermItems() {
		c.pushPermissionDelta(item)
	}
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
