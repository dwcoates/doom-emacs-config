// Package sessionwatcher owns every shim watch for one live workspace, and is
// the daemon's source of connectivity truth for the daemon-to-shim hop.
//
// One instance per live workspace. It opens WatchSession immediately, opens
// WatchAgent for the turn in flight, opens one watch per live-work item and
// one for every detached-work announcement, and reaps each at its terminal.
// Frames flow straight to their consumers: nothing is relayed module by
// module. FREENESS — no in-flight turn AND an empty live-work set — is
// answered here. See ARCHITECTURE.md "sessionwatcher".
package sessionwatcher

import (
	"context"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// LinkState is the daemon-to-shim hop of connectivity truth, as the watcher
// republishes it. It aliases the shim client's spelling so the two never
// drift; the render-colors topbar_connectivity table is keyed by it.
type LinkState = shimclient.LinkState

// TurnClose is how a turn ended. It aliases wsm's spelling so the durable
// record and the lifecycle notification cannot disagree.
type TurnClose = wsm.TurnClose

// OutputAddress is where a lease holder wants this session's rows to land. It
// aliases wsm's spelling for the same reason.
type OutputAddress = wsm.OutputAddress

// LiveWorkSet is the set of detached work items currently live on a session:
// detached subagents and detached shells. An EMPTY set plus no turn in flight
// is what freeness means.
type LiveWorkSet struct {
	// Agents are the live detached subagents.
	Agents []*conversationv1.AgentId
	// Shells are the live detached shells.
	Shells []*conversationv1.DetachedWorkId
}

// Empty reports whether any detached work is live.
func (s LiveWorkSet) Empty() bool { return len(s.Agents) == 0 && len(s.Shells) == 0 }

// HostNotification is a notification bound for the workspace's host stream:
// what raises the roster's attention marker.
type HostNotification struct {
	// Text is the notification line.
	Text string
	// At is when it was raised.
	At time.Time
	// Kind names it: "agent_addressed", or "permission_requested" with the
	// tool named.
	Kind string
	// ToolName is set for a permission notification, empty otherwise.
	ToolName string
}

// FeedSink receives everything the feed resolver draws a row from. Every
// method carries the OutputAddress in force so the resolver places the row
// without asking anyone.
type FeedSink interface {
	// OnPrompt is a prompt one agent addressed to another.
	OnPrompt(ws ids.WorkspaceID, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt, addr OutputAddress)
	// OnActivity is one unit of a turn's synchronous progress.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, addr OutputAddress)
	// OnQuestion is the agent blocking on a choice.
	OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion, addr OutputAddress)
	// OnPermission is the agent blocking on consent.
	OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission, addr OutputAddress)
	// OnContextCut is the AgentUpdate.context_cut page line — /clear, a
	// compaction, or a compaction that failed. The feed draws the separation
	// divider from it. Instantaneous: one frame, no lifecycle.
	OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut, addr OutputAddress)
	// OnApiError is the AgentUpdate.api_error page line: a vendor request that
	// failed MID-TURN and the turn went on. EVIDENCE, never a terminal — the
	// turn's end is the frame-level failure arm and nothing else.
	OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, addr OutputAddress)
	// OnAgentTerminal is how one agent's stream ended: exactly one of success
	// and failure is set, and turn is set when the agent belonged to a turn.
	OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure, addr OutputAddress)
	// OnDetachedWork is work leaving the stream, which is what makes a bubble
	// outlive its turn.
	OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, addr OutputAddress)
	// OnBash is one detached shell's progress.
	OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash, addr OutputAddress)
	// OnSessionUpdate is a session-scoped fact that changes rows —
	// query_died, compacting, identity_rotated.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnHistoryPage is a watch's opening catch-up page.
	OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, addr OutputAddress)
}

// FooterSink receives what the footer's status tree, live-work chips and
// tokens cell resolve from.
type FooterSink interface {
	// OnActivity advances the status tree and the activity line.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity)
	// OnQuestion moves the footer to waiting.
	OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion)
	// OnPermission moves the footer to waiting.
	OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission)
	// OnApiError is mid-turn evidence the footer draws as a retry notice.
	OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed)
	// OnAgentTerminal retires an agent from the status tree.
	OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure)
	// OnDetachedWork adds or updates a live-work chip.
	OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork)
	// OnBash advances a shell chip.
	OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash)
	// OnSessionUpdate carries context usage, the budget warning and the
	// terminals the footer reflects.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnLink is the connectivity change the footer reflects.
	OnLink(ws ids.WorkspaceID, link LinkState)
}

// TopbarSink receives what the topbar's title, model selector, connectivity
// glyph, warnings and context chip resolve from.
type TopbarSink interface {
	// OnSessionStarted carries the session's identity and spawn facts.
	OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted)
	// OnSessionUpdate carries model changes, diagnostics, context usage,
	// account usage and session faults.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnActivity is here only for the unmodeled-activity warning: an activity
	// the schema does not model is a warning the topbar shows.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity)
	// OnLink drives the connectivity glyph and its tone.
	OnLink(ws ids.WorkspaceID, link LinkState)
}

// SidebarSink receives what a roster row's status arm resolves from.
type SidebarSink interface {
	// OnSessionStarted marks the row live.
	OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted)
	// OnAgentTerminal retires the row's thinking state.
	OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure)
	// OnActivity moves the row to thinking.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity)
	// OnDetachedWork moves the row to background.
	OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork)
	// OnPermission moves the row to waiting.
	OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission)
	// OnSessionUpdate carries the terminals and faults the row reflects.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnLink drives the severed and dead arms.
	OnLink(ws ids.WorkspaceID, link LinkState)
}

// HoldsSink is deliberately empty of watch-driven methods: the hold tray is
// fed by the prompt queue and the merge orchestrator, never by the watcher.
// It exists so the watcher's constructor takes the same shape as the other
// four sinks and so the tray's owner is named in one place.
type HoldsSink interface{}

// LifecycleSink receives the facts that drive the daemon's own machinery
// rather than a view.
type LifecycleSink interface {
	// OnTurnEnded is what pops the prompt queue and releases a
	// hold-for-turn-end.
	OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how TurnClose)
	// OnLiveWorkChanged republishes the live-work set; combined with the
	// in-flight turn it is the freeness answer every lease holder waits on.
	OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet)
	// OnNotification raises a host notification and the roster's attention
	// marker.
	OnNotification(ws ids.WorkspaceID, note HostNotification)
}

// Sinks is the set a watcher routes into.
type Sinks struct {
	Feed      FeedSink
	Footer    FooterSink
	Topbar    TopbarSink
	Sidebar   SidebarSink
	Holds     HoldsSink
	Lifecycle LifecycleSink
}

// Watcher is one live workspace's watch fleet.
type Watcher interface {
	// Connected reports whether the daemon-to-shim link is serving.
	Connected() bool
	// Link is the current link state.
	Link() LinkState
	// LiveWork is the current live-work set.
	LiveWork() LiveWorkSet
	// TurnInFlight reports the open turn, nil when none is.
	TurnInFlight() *ids.TurnID
	// Free reports freeness: no turn in flight AND an empty live-work set.
	// Never judged except while holding the workspace's lease.
	Free() bool
	// SetOutputAddress installs the address a lease holder wants this
	// session's rows stamped with; nil restores the root feed.
	SetOutputAddress(addr *OutputAddress)
	// Close tears down every watch this workspace owns.
	Close() error
}

// Start builds and starts one workspace's watcher against its shim client.
func Start(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, sinks Sinks, log dlog.Logger) (Watcher, error) {
	return nil, notimpl.Err
}
