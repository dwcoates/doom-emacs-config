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
	// Monitors are the live background watchers. FOOTER-ONLY — a monitor
	// opens no stream of its own (the contract gives it none), so the watcher
	// tracks its liveness from the announcement and from the monitor
	// activity's own terminal. It counts toward freeness like any other
	// detached item: a session with a live monitor is not free.
	Monitors []*conversationv1.DetachedWorkId
}

// Empty reports whether any detached work is live.
func (s LiveWorkSet) Empty() bool {
	return len(s.Agents) == 0 && len(s.Shells) == 0 && len(s.Monitors) == 0
}

// NotificationKind names what a host notification is about. A typed spelling
// rather than a bare string so a new kind is a compiler-visible addition
// rather than a literal invented at a call site.
type NotificationKind string

// The notification kinds.
const (
	// NotificationAgentAddressed is an agent addressing the user — today a
	// blocked question, whose header is the notification's text.
	NotificationAgentAddressed NotificationKind = "agent_addressed"
	// NotificationPermissionRequested is an agent blocked on consent, with the
	// gated call's tool named.
	NotificationPermissionRequested NotificationKind = "permission_requested"
	// NotificationQuestionAsked is a question batch blocking the agent, which
	// gets a permission ask's attention treatment. It carries the first
	// question's chip label, as HostNotificationQuestionAsked.header does.
	NotificationQuestionAsked NotificationKind = "question_asked"
)

// HostNotification is a notification bound for the workspace's host stream:
// what raises the roster's attention marker.
type HostNotification struct {
	// Text is the notification line.
	Text string
	// At is when it was raised.
	At time.Time
	// Kind names it.
	Kind NotificationKind
	// ToolName is set for a permission notification, empty otherwise. It is
	// HostNotificationPermissionRequested.tool_name.
	ToolName string
	// Header is set for a question notification, empty otherwise. It is
	// HostNotificationQuestionAsked.header: the first question's chip label.
	Header string
}

// FeedSink receives everything the feed resolver draws a row from. Every
// method carries the OutputAddress in force so the resolver places the row
// without asking anyone.
type FeedSink interface {
	// OnTurnOpened is the TURN-OPEN EDGE: the prompt queue's accepted turn,
	// handed over by the watcher. Nothing on the shim's streams states it for
	// a turn the DAEMON opened -- StartTurn answers with the prompt rather
	// than echoing it on the agent's stream -- so without this edge the feed
	// does not know which turn is running, and a session-scoped death
	// (query_died) has no turn to draw a terminal for.
	OnTurnOpened(ws ids.WorkspaceID, turn ids.TurnID)
	// OnPrompt is a prompt one agent addressed to another.
	OnPrompt(ws ids.WorkspaceID, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt, addr OutputAddress)
	// OnPeerMessage is a message ANOTHER Claude session sent into this
	// conversation — an inter-session peer message or a subagent hand-back. It
	// is never a person's prompt and never this agent's own work; the feed draws
	// it as the abbreviated, right-aligned, purple, expandable peer bubble.
	OnPeerMessage(ws ids.WorkspaceID, peer *conversationv1.PeerMessage, addr OutputAddress)
	// OnActivity is one unit of a turn's synchronous progress.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, addr OutputAddress)
	// OnQuestion is the agent blocking on a choice.
	OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion, addr OutputAddress)
	// OnPermission is the agent blocking on consent.
	OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission, addr OutputAddress)
	// OnContextCut is the AgentUpdate.context_cut page line — /clear, a
	// compaction, or a compaction that failed. The feed draws the separation
	// divider from it. Instantaneous: one frame, no lifecycle.
	//
	// `at` IS THE CUT'S IDENTITY. A cut reaches this daemon once per plane —
	// the shim's stream and the sidecar's file both write the same store entry
	// — so the feed keys its divider on the entry's own stable position rather
	// than on a count of arrivals. See feed.drawContextCut.
	OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut, at *conversationv1.HistoryPointer, addr OutputAddress)
	// OnApiError is the AgentUpdate.api_error page line: a vendor request that
	// failed MID-TURN and the turn went on. EVIDENCE, never a terminal — the
	// turn's end is the frame-level failure arm and nothing else.
	OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, addr OutputAddress)
	// OnAgentTerminal is how one agent's stream ended: exactly one of success
	// and failure is set, and turn is set when the agent belonged to a turn.
	OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure, addr OutputAddress)
	// OnMainAgent names the session's MAIN agent: the one whose work is drawn
	// on the root feed. It is stated before any frame that agent's watch
	// carries is routed, and again whenever the naming changes. Nothing else
	// puts an agent on the root: an agent the feed has not been told is the
	// main one and never saw created is unplaceable, never defaulted.
	OnMainAgent(ws ids.WorkspaceID, agent *conversationv1.AgentId)
	// OnDetachedWork is work leaving the stream, which is what makes a bubble
	// outlive its turn.
	OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, addr OutputAddress)
	// OnBash is one detached shell's progress.
	OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash, addr OutputAddress)
	// OnLiveWorkChanged republishes the AUTHORITATIVE live-work set. A
	// detached shell that leaves it with no terminal of its own will never
	// report again, so its bubble must not go on drawing it running.
	OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet)
	// OnSessionUpdate is a session-scoped fact that changes rows —
	// query_died, compacting, identity_rotated.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnHistoryPage is a watch's opening catch-up page.
	OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, addr OutputAddress)
}

// FooterSink receives what the footer's status tree, live-work chips and
// tokens cell resolve from.
type FooterSink interface {
	// OnTurnOpened is the TURN-OPEN EDGE: the prompt queue's accepted turn,
	// handed over by the watcher. Nothing on the shim's streams states it —
	// the first frame of a turn is an activity, by which time `submitting` is
	// already over — so the footer is told here, and this is what raises
	// `thinking submitting` and starts the strip's clock.
	OnTurnOpened(ws ids.WorkspaceID, turn ids.TurnID)
	// OnActivity advances the status tree and the activity line.
	OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity)
	// OnQuestion moves the footer to waiting.
	OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion)
	// OnPermission moves the footer to waiting.
	OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission)
	// OnApiError is mid-turn evidence the footer draws as a retry notice.
	OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed)
	// OnContextCut is the cut's END SIGNAL. SessionUpdate.compacting says a
	// compaction BEGAN and nothing upstream says it finished, so this record
	// is what clears the footer's compacting and clearing states — which is
	// why the footer sees a page line the feed also draws. A FAILED compaction
	// ends the status too: nothing was cut, so the session is idle again, and
	// the producer's account becomes the footer's evidence rather than passing
	// silently.
	OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut)
	// OnAgentTerminal retires an agent from the status tree.
	OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure)
	// OnDetachedWork adds or updates a live-work chip.
	OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork)
	// OnBash advances a shell chip.
	OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash)
	// OnSubagent advances a DETACHED subagent's chip and retires it at that
	// run's own terminal. The counterpart of OnBash: a detached run is
	// addressed by its HANDLE, so its terminal retires the chip whichever
	// stream carried the frame, and the spawning call's stream is never read
	// as a statement about a run that has already left the turn.
	OnSubagent(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, sub *conversationv1.AgentSubagent)
	// OnContextBudgetWarning is the vendor's own context-budget warning. It
	// is an AGENT-PLANE fact (a page line of the agent's book, sidecar-
	// produced), never a session-stream event, so it arrives addressed to the
	// agent whose transcript carried it.
	OnContextBudgetWarning(ws ids.WorkspaceID, agent *conversationv1.AgentId, w *conversationv1.ContextBudgetWarning)
	// OnSessionUpdate carries context usage, the rate-limit status and the
	// terminals the footer reflects.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnHistoryPage is a watch's opening catch-up page — the frame a RESUMED
	// session's whole prior conversation arrives as.
	//
	// The footer reads ONE thing out of it: that this conversation has
	// already run turns, so an idle strip reads `done` rather than `ready`.
	// Nothing else can state that after a daemon relaunch — the turn-open
	// edge and the turn's terminal both belong to a turn some EARLIER daemon
	// process watched — and a rehydrated feed under a `ready` footer says the
	// conversation never happened.
	OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage)
	// OnLink is the connectivity change the footer reflects.
	OnLink(ws ids.WorkspaceID, link LinkState)
	// OnLiveWorkChanged republishes the AUTHORITATIVE live-work set, which is
	// what the footer's `background` arm and its live-work chips mean by LIVE.
	// The same set the roster takes: the watcher reaps each item's watch at
	// its terminal, so it is the one party that knows an item has ENDED, and
	// a footer that decided liveness from its own frame ledger could — and
	// did — report a background task for work that had finished while the
	// roster said ready.
	OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet)
}

// TopbarSink receives what the topbar's title, model selector, connectivity
// glyph, warnings and context chip resolve from.
type TopbarSink interface {
	// OnSessionStarted carries the session's identity and spawn facts.
	OnSessionStarted(ws ids.WorkspaceID, started *conversationv1.SessionStarted)
	// OnSessionUpdate carries model changes, diagnostics, context usage,
	// account usage and session faults.
	OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate)
	// OnContextCut is the cut's END SIGNAL — /clear or a compaction — and the
	// chip's evidence that its last `context_usage` no longer describes the
	// context. A clear or a completed compaction discards the transcript the
	// stale total was read off, so the chip drops that figure rather than
	// stating it until the next reading; a FAILED compaction cut nothing, so
	// the chip keeps the total it already had.
	OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut)
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
	// OnLiveWorkChanged republishes the AUTHORITATIVE live-work set. The
	// roster's `idle_async` arm is what detached work makes a row say, and an
	// announcement alone can only ever raise it: nothing on the agent's stream
	// states that a detached item has ENDED. The watcher owns that fact — it
	// reaps each item's watch at its terminal — so the set is routed to the
	// roster too, and the arm retires the moment the last item ends rather
	// than at the next turn.
	OnLiveWorkChanged(ws ids.WorkspaceID, live LiveWorkSet)
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
	// OnFree is the FREENESS EDGE: the workspace just went from having work in
	// flight (a turn, or live detached work) to having none. It is told OFF the
	// watcher's lock, on a goroutine of its own that Close joins, because the
	// sink is the prompt queue's bounce registry, which reads this watcher
	// under its own delivery lock. It is told once per transition, never while
	// the workspace simply stays free.
	OnFree(ws ids.WorkspaceID)
	// OnNotification raises a host notification and the roster's attention
	// marker.
	OnNotification(ws ids.WorkspaceID, note HostNotification)
	// OnAsksSettled reports that every ask whose notification raised this
	// workspace's attention marker has SETTLED — answered by the user, decided
	// without them, or failed to be put at all. It CLEARS the marker: the
	// marker means an UNSEEN notification, and an ask that is over is not one.
	//
	// It is the counterpart of OnNotification rather than a second spelling of
	// SelectWorkspace's clear: a user who answers a card has looked at the
	// workspace whether or not they ever selected it, and a workspace that is
	// never selected would otherwise wear its marker forever.
	OnAsksSettled(ws ids.WorkspaceID)
	// OnLinkFault reports a link this daemon LOST, as evidence rather than as
	// a view fact: a severed standing stream, or a reaped shim process with
	// its exit code. Each becomes a per-session fault record, which is what
	// SessionHealth answers with — the liveness probe's two booleans cannot
	// carry an exit code and cannot tell a process that died from a stream
	// that broke.
	OnLinkFault(ws ids.WorkspaceID, fault LinkFault)
	// OnWatchOpenRefused reports a watch open the shim refused for a handle
	// NOTHING announced. A refusal on a handle the daemon legitimately
	// expects is retried and never reaches here: only an unexpected one is
	// evidence, and it is evidence of a daemon/shim disagreement about what
	// exists rather than of a broken link.
	OnWatchOpenRefused(ws ids.WorkspaceID, refusal WatchOpenRefusal)
	// OnLinkChanged reports the shim link's attachment. The VIEWS take the
	// link on their own sinks; this arm exists because the HOST view's
	// `shim_attached` is composed by the server, which cannot see the edge.
	OnLinkChanged(ws ids.WorkspaceID, attached bool)
	// OnSessionDiagnostics is the shim's own health verdict, handed at the
	// daemon's machinery as well as at the topbar: the health reporter's
	// per-session faults are what SessionHealth answers with, and a verdict
	// that only reached a view would never reach that answer. The push is the
	// WHOLE current verdict, so a healthy one retracts the standing faults.
	OnSessionDiagnostics(ws ids.WorkspaceID, diagnostics *conversationv1.SessionDiagnostics)
}

// LinkFaultKind names how the daemon-to-shim link was lost.
type LinkFaultKind string

const (
	// LinkFaultSevered is a standing stream that ended while the shim process
	// is still alive. The daemon redials.
	LinkFaultSevered LinkFaultKind = "severed"
	// LinkFaultDead is a shim process that is gone. Redial stops here.
	LinkFaultDead LinkFaultKind = "dead"
)

// WatchOpenRefusal is a watch OPEN the shim REFUSED before any frame: a
// not_found or failed_precondition answer to the Watch call itself. It is not
// a lost link -- the transport is serving, the shim simply has no such handle
// yet -- so it travels on its own arm and never as a LinkFault.
type WatchOpenRefusal struct {
	// Operation is the rpc whose open was refused ("watch_agent",
	// "watch_bash").
	Operation string
	// Handle is what the open addressed: an AgentId.value, a
	// DetachedWorkId.value, or empty for the main agent's unset target.
	Handle string
	// Detail is the sentence the fault record carries.
	Detail string
}

// LinkFault is one lost link, with whatever evidence the loss carried.
type LinkFault struct {
	// Kind is how the link was lost.
	Kind LinkFaultKind
	// ExitCode is the reaped process's decoded exit status. It is set only on
	// LinkFaultDead, and only when the reap actually decoded one: presence,
	// never a sentinel zero.
	ExitCode *int32
	// Detail is the sentence the fault record carries.
	Detail string
}

// Sinks is the set a watcher routes into.
type Sinks struct {
	Feed      FeedSink
	Footer    FooterSink
	Topbar    TopbarSink
	Sidebar   SidebarSink
	Holds     HoldsSink
	Lifecycle LifecycleSink
	// Title is the synthesized-title trigger sink. OPTIONAL: nil disables title
	// synthesis, which is what every test that does not exercise it leaves it.
	// Production always wires it. Its calls are non-blocking (the synthesizer
	// dispatches its own goroutine), so it rides the watcher's stream goroutine
	// without holding it up.
	Title TitleSink
}

// TitleSink is the synthesized-title synthesizer, seen from the watcher: the
// four occasions on which a workspace's own title may need (re)making or
// dropping. It is a SEPARATE sink from TopbarSink because it is not a view — it
// is the daemon deciding, from these edges, whether to spend a cheap model call
// on a title. Every method is keyed by workspace alone; the synthesizer
// resolves the shim, the account and the digest itself.
type TitleSink interface {
	// OnSessionStarted is a session naming itself: a resumed or adopted
	// conversation may already carry prompts and no vendor title.
	OnSessionStarted(ws ids.WorkspaceID)
	// OnTurnEnded is a turn completing: a new prompt has changed the digest.
	OnTurnEnded(ws ids.WorkspaceID)
	// OnVendorTitle is the vendor stating its own ai-title: synthesis stops,
	// because the vendor's title always wins.
	OnVendorTitle(ws ids.WorkspaceID)
	// OnContextReset is a /clear or a completed /compact: the boundary moved,
	// so the last synthesis no longer describes the conversation.
	OnContextReset(ws ids.WorkspaceID)
}

// Watcher is one live workspace's watch fleet.
type Watcher interface {
	// Connected reports whether the daemon-to-shim link is serving.
	Connected() bool
	// Link is the current link state.
	Link() LinkState
	// LiveWork is the current live-work set.
	LiveWork() LiveWorkSet
	// Pointers is the newest pointer this watcher was served on each watch. It
	// is what a SUCCESSOR watcher of the same conversation resumes from
	// (ResumeFrom), and it stays readable after Close for exactly that reason.
	Pointers() Pointers
	// MainKnownThrough is the newest pointer the MAIN agent's watch was served,
	// nil when it was served none. StartTurn states it as known_through, so the
	// accepted turn's opening page carries only what the turn itself wrote.
	MainKnownThrough() *conversationv1.HistoryPointer
	// TurnInFlight reports the open turn, nil when none is.
	TurnInFlight() *ids.TurnID
	// Free reports freeness: no turn in flight AND an empty live-work set.
	// Never judged except while holding the workspace's lease.
	Free() bool
	// AwaitFree blocks until the workspace is free, or until ctx ends. It is
	// driven by the stream edges — OnTurnEnded and OnLiveWorkChanged — so a
	// lease holder waits on an EVENT rather than on a poll. A watcher closed
	// under a standing wait answers ErrWatcherClosed.
	AwaitFree(ctx context.Context) error
	// AwaitTurnEnd blocks until the named turn ends and reports how. A turn
	// that ended just before the call is answered from the watcher's memory of
	// recently closed turns, so the caller cannot miss the edge it submitted.
	AwaitTurnEnd(ctx context.Context, turn ids.TurnID) (TurnClose, error)
	// SetOutputAddress installs the address a lease holder wants this
	// session's rows stamped with; nil restores the root feed.
	SetOutputAddress(addr *OutputAddress)
	// SetMainAgent names the session's main agent — the WatchAgent address the
	// turn runs under. The prompt queue calls it with
	// StartTurnSuccess.prompt.agent after every accepted turn; it is the
	// AUTHORITATIVE source, and the only other one is the main watch's opening
	// history page (an AgentPrompt names its recipient), which is what an
	// adoption with a turn already in flight has to go on. Until one of the
	// two has named it, a terminal cannot be attributed to the main agent and
	// OnTurnEnded is withheld rather than guessed.
	SetMainAgent(agent *conversationv1.AgentId)
	// OnTurnOpening records the turn a caller is ABOUT to hand to the shim,
	// before StartTurn is dispatched. It exists because the shim can put the
	// turn's first frames — its terminal included — on the agent stream
	// before StartTurn's response has been processed here: a terminal routed
	// while no turn is recorded in flight is attributable to nothing and the
	// turn's end is lost, hanging every AwaitTurnEnd on it forever. Recording
	// the turn first makes the attribution independent of that ordering.
	//
	// The caller MUST pair it with either OnTurnOpened (the shim accepted the
	// turn) or OnTurnOpenFailed (it did not).
	OnTurnOpening(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnOpenFailed retires a turn recorded by OnTurnOpening that the shim
	// then refused, so a turn that never started does not stand as in flight.
	OnTurnOpenFailed(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnOpened is the prompt queue handing over an accepted turn: the
	// prompt as StartTurn delivered it, and the opening page
	// StartTurnSuccess now carries. It is the ONE entry point for a turn the
	// watcher did not see opened on a stream — it names the main agent,
	// records the turn a terminal will be attributed to, and feeds the page
	// through the same history-page path a watch's own opening page takes.
	//
	// IT DOES NOT MIRROR THE PROMPT: the queue draws the accepted prompt's
	// row itself, and the prompt also arrives as a history entry on the main
	// watch. Two feed rows for one prompt is what routing it here would cost.
	OnTurnOpened(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, page *conversationv1.HistoryPage)
	// SessionEnding records that THIS DAEMON is ending the session, before the
	// verb that ends it is dispatched.
	//
	// The shim closes its standing streams as the session goes, and a watcher
	// that has not been told reads the daemon's own act as a transport fault:
	// it records a severing at ERROR, marks the link degraded, and redials
	// watches at a shim the same call is about to stop. Only the caller knows
	// the difference, so only the caller can say.
	SessionEnding(reason string)
	// Close tears down every watch this workspace owns.
	Close() error
}

// Session is what the caller learned when it opened the session, and how the
// watcher's watches begin. The watcher needs both: the SessionStarted states
// the LEVEL it must open watches for (the turn in flight and every live
// detached item), and the Opening decides whether each opening page is the
// first page (a replay) or a catch-up from a predecessor's pointers. See
// opening.go.
type Session struct {
	// Started is StartSession's success, whole.
	Started *conversationv1.SessionStarted
	// Opening is REQUIRED: Start refuses the zero value.
	Opening Opening
}

// Start builds and starts one workspace's watcher against its shim client.
func Start(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, session Session, sinks Sinks, log dlog.Logger) (Watcher, error) {
	return start(ctx, ws, client, session, sinks, log)
}
