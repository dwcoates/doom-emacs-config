// Package promptqueue is the ONE delivery path for prompts.
//
// It owns holds, the classifier call, interjection, the parked ledger and the
// drain at turn end. It never imports the merge orchestrator or the drain
// controller; the three meet at the WSM lease and at the shim client. See
// ARCHITECTURE.md "promptqueue".
package promptqueue

import (
	"context"
	"errors"
	"io/fs"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/lockwatch"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/startslots"
	"agent-repld/internal/wsm"
)

// The typed refusals this package answers with. Each is a fact about the
// SUBMISSION, never about the queue's contents: a hold is an answer, not one of
// these.
//
// THE SERVER MAPS THESE ONTO THE LANDED ERROR ARMS, one for one, and this is
// the one place the mapping is written down:
//
//	ErrMerging           → SubmitPromptError.merging
//	ErrNoSession         → SubmitPromptError.no_session
//	ErrColdGate          → SubmitPromptError.cold_gate
//	ErrNoSuchHold        → UpdateHeldPromptError.no_such_hold
//	ErrAlreadyDelivered  → UpdateHeldPromptError.already_delivered
//	ErrReleaseRefused    → UpdateHeldPromptError.release_refused
//	ErrNoSuchHold        → EditHeldPromptError.no_such_hold
//	ErrNotHeld           → EditHeldPromptError.not_held
//	ErrAlreadyDelivered  → EditHeldPromptError.already_delivered
//	ErrBeingEdited       → EditHeldPromptError.being_edited
//	ErrNotEditing        → EditHeldPromptError.not_editing
//	ErrNoEditor          → EditHeldPromptError.no_editor
//	ErrNoSuchHold        → FoldHeldPromptError.no_such_hold
//	ErrNotHeld           → FoldHeldPromptError.not_held
//	ErrNotAPrompt        → FoldHeldPromptError.not_a_prompt
//	ErrAboveMoved        → FoldHeldPromptError.above_moved (AboveMovedError)
//	ErrAboveNotAPrompt   → FoldHeldPromptError.above_not_a_prompt
//	ErrAboveOtherAuthor  → FoldHeldPromptError.above_other_author
//	ErrBeingEdited       → FoldHeldPromptError.being_edited (BeingEditedError)
var (
	// ErrMerging is a submission that arrived AFTER a merge began. It is
	// refused outright rather than held: a merged workspace closes, so work a
	// post-merge-start prompt produced would be orphaned.
	ErrMerging = errors.New("promptqueue: a merge is in flight for this workspace")
	// ErrHoldsChanged is a rollback whose planned held prompts are no longer
	// the ones standing: the plan no longer describes what would be dropped,
	// so nothing was performed.
	ErrHoldsChanged = errors.New("promptqueue: the held prompts changed since the rollback was planned")
	// ErrNoSession is a submission to a workspace with no live shim.
	ErrNoSession = errors.New("promptqueue: the workspace has no session to submit to")
	// ErrShimHasNoSession is the SHIM's own refusal of a delivery because it
	// holds no session (StartTurnFailure.no_session). The sender's typed
	// refusal answers errors.Is against it. A delivery refused so is HELD
	// under the reconnect hold rather than lost (feed_held.proto,
	// HeldPromptReconnectHold).
	ErrShimHasNoSession = errors.New("promptqueue: the shim holds no session")
	// ErrAskOpen is the SHIM's own refusal of a prompt sent to join the
	// running turn because an ask blocks the vendor (StartTurnFailure.ask_open):
	// the vendor would take the prompt in only once the user answered. The
	// sender's typed refusal answers errors.Is against it, and the prompt stays
	// held for the running turn's end.
	ErrAskOpen = errors.New("promptqueue: an ask blocks the vendor")
	// ErrShimDied is a delivery that failed because the shim DIED under it:
	// the session read live when the prompt arrived, and the shim was gone
	// by the time StartTurn reached it. The sender's error answers errors.Is
	// against it. Like a no_session refusal it is HELD under the reconnect
	// hold, never refused: the death starts the session's revival, whose
	// coming up delivers it.
	ErrShimDied = errors.New("promptqueue: the shim died under the delivery")
	// ErrSessionNotWatched is a delivery the sender would not send because no
	// watch stands to see the turn: the session's watcher is gone, or closed
	// while the turn waited for its main watch. A turn sent then writes frames
	// nothing receives, so, like a no_session refusal, it is HELD under the
	// reconnect hold, and the session's next coming up delivers it.
	ErrSessionNotWatched = errors.New("promptqueue: no watch stands to see the turn")
	// ErrColdGate is a submission to a workspace whose session is PARKED AT
	// ITS COLD GATE. It is deliberately NOT ErrNoSession: a session exists —
	// the shim is up and serving, which is exactly why the gate could be
	// raised — and it takes no prompt until the user answers the gate in the
	// panel. The two were one answer until 2026-09-14, and every client
	// consequently told the user the wrong thing about a session that was up.
	ErrColdGate = errors.New("promptqueue: the session is parked at its cold gate")
	// ErrNoSuchHold names a turn the queue holds nothing under.
	ErrNoSuchHold = errors.New("promptqueue: no hold stands under that turn")
	// ErrAlreadyDelivered is an action on a hold that already went to the shim.
	ErrAlreadyDelivered = errors.New("promptqueue: that hold was already delivered")
	// ErrBeingDelivered is an action on a hold whose delivery call to the
	// shim is in flight (call.go): it is neither held nor delivered yet, and
	// once the call settles it is either delivered or back in the tray.
	ErrBeingDelivered = errors.New("promptqueue: that hold is being delivered to the shim")
	// ErrReleaseRefused is a force-through on an uninterruptible verdict or a
	// session_starting hold: there is nothing delivery could do yet.
	ErrReleaseRefused = errors.New("promptqueue: this hold cannot be released through")
	// ErrNotHeld is an edit of a prompt that WAS held and no longer is: it
	// was dropped.
	ErrNotHeld = errors.New("promptqueue: that prompt is no longer held")
	// ErrBeingEdited is a begin while an edit already stands on the
	// workspace, or a fold naming the prompt an edit stands on.
	// BeingEditedError carries it with the turn being edited.
	ErrBeingEdited = errors.New("promptqueue: a held prompt is already being edited on this workspace")
	// ErrNotEditing is a commit or cancel naming a prompt no edit stands on.
	ErrNotEditing = errors.New("promptqueue: no edit stands on that prompt")
	// ErrNoEditor is a begin with no editor's host stream to edit in.
	ErrNoEditor = errors.New("promptqueue: no editor is attached to this workspace")
	// ErrPromptNotRecorded is a rewind of a turn whose prompt the vendor
	// never recorded (shim.v1 RollBackSessionPromptNotRecorded): there is
	// nothing in the vendor conversation to cut.
	ErrPromptNotRecorded = errors.New("promptqueue: the vendor never recorded the prompt")
)

// The session-act kinds the one delivery path carries. They are constants
// because internal/workspace spells the same two at its call sites and the two
// spellings must not drift.
const (
	// ActClear is /clear: the context cut with nothing left in its place.
	ActClear = "clear"
	// ActCompact is /compact: the context cut that summarizes.
	ActCompact = "compact"
	// ActSetModel is a model change, from /model <arg> or from the picker.
	ActSetModel = "set_model"
	// ActSetPermissionMode is a permission-mode change.
	ActSetPermissionMode = "set_permission_mode"
)

// Submission is one prompt entering the queue.
type Submission struct {
	// WS is the workspace it belongs to.
	WS ids.WorkspaceID
	// Turn is the turn the daemon minted for it.
	Turn ids.TurnID
	// Said is the composed prompt — one canonical form client to daemon to
	// tray to shim to record.
	Said *conversationv1.UserSaid
	// Origin is the send site, REQUIRED: an UNSPECIFIED origin is refused at
	// submission, and the accepted value is persisted onto the turn via
	// StartTurn.
	Origin conversationv1.PromptOrigin
	// SentBy is the workspace whose agent sent the prompt, set exactly when
	// Origin is PROMPT_ORIGIN_AGENT_SENT: Submit refuses a submission breaking
	// either half. It rides the hold and the turn record (wsm), so the
	// recipient's feed heads the prompt with it.
	SentBy *ids.WorkspaceID
	// Target, when set, is the bubble composer's addressed feed row — the
	// prompt goes to THAT agent through UpdateAgent.prompt.
	Target *feedid.Ref
	// Delivery is how the prompt asked to be delivered (agentrepl.v1
	// SubmitPromptDelivery). A DEFERRED prompt held behind a running turn is
	// never classified, so it never interjects: it waits for the turn's end
	// and runs as its own turn. It is stored on the hold, so every later
	// judgement of the hold honors it.
	Delivery wsm.Delivery
	// Act, when set, makes the submission a held SESSION ACT (a model or
	// permission-mode change) rather than a prompt: never classified, and run
	// as the act when what is ahead of it ends.
	Act *wsm.HeldAct
	// Ask, when set, makes the submission a zettel THREAD QUESTION: where it
	// was asked from rides the hold, and the turn it is delivered as records
	// the question whole (wsm.ThreadQuestion), so the answer can be recorded in
	// zettel. A thread question is never classified, never interjected and
	// never folded: one turn is one question.
	Ask *wsm.ThreadAsk

	// interjected reports a held prompt delivered because its verdict
	// interrupted the running turn: its StartTurn carries the interruption
	// note. Set by the delivery of a hold, never by a caller.
	interjected bool
	// fromHold reports a submission delivered from its standing hold, which
	// the shim call delivering it claims (shimCall.holds). Set by the
	// delivery of a hold, never by a caller.
	fromHold bool
}

// claims answers the standing holds a call delivering SUB claims: its own
// hold when it is delivered from one, none otherwise.
func (sub Submission) claims() []ids.TurnID {
	if !sub.fromHold {
		return nil
	}
	return []ids.TurnID{sub.Turn}
}

// Disposition is what became of a submission. A hold is an ANSWER, not a
// failure.
type Disposition struct {
	// Delivered reports the prompt went to the shim at once.
	Delivered bool
	// Held is the DAEMON-SIDE condition it was parked under, nil when no such
	// condition applies.
	Held *wsm.HoldKind
	// Classification is the verdict the prompt was parked under, nil when it
	// was not parked by one.
	//
	// SEAM ADDITION (recorded): the skeleton described a hold with `Held`
	// alone, but the two facts are orthogonal in wsm and a classification hold
	// carries no HoldKind at all — with `Held` as the only signal, a hold for
	// a running turn was indistinguishable from a delivery.
	Classification *wsm.Classification
	// RefusedArm names the refusal when the submission was rejected outright
	// (the merge lease's refuse policy), empty otherwise.
	RefusedArm string
}

// Parked reports that the submission was held rather than delivered or
// refused. It is the one place the three-way answer is read off the fields, so
// no caller re-derives it.
func (d Disposition) Parked() bool {
	return !d.Delivered && d.RefusedArm == ""
}

// Act is a session-level action that travels the same one delivery path as a
// prompt, so it cannot overtake a queued prompt.
type Act struct {
	// Kind names it: ActClear, ActCompact, ActSetModel, ActSetPermissionMode.
	Kind string
	// Value is the act's argument — the model id, the permission mode — empty
	// for the argument-less acts.
	Value string
	// Turn is the turn a context-cutting act runs as, minted by the caller so
	// SubmitPrompt can acknowledge it. Empty means the queue mints one, which
	// is what the picker-driven setter acts do.
	//
	// SEAM ADDITION (recorded): /clear and /compact reach the vendor as turns,
	// and SubmitPromptSuccess.turn is what the composer matches its own row
	// against.
	Turn ids.TurnID
	// Origin is the send site of a context-cutting act, which is delivered as
	// a turn and so needs the same durable origin a prompt does. It is unused
	// by the two setter acts.
	//
	// SEAM ADDITION (recorded): /clear and /compact reach the vendor as
	// StartTurn calls, and StartTurn.origin is required.
	Origin conversationv1.PromptOrigin
}

// Queue is the delivery path's whole surface.
type Queue interface {
	// ReviveForSettings starts WS's shim for an effort pick, which holds no
	// row (a model or permission-mode pick is held by SubmitSessionAct and
	// starts it there). It returns at once; a shim already up is no change,
	// and a revival in flight is joined. ErrNoSession when the queue has no
	// revival wired.
	ReviveForSettings(ctx context.Context, ws ids.WorkspaceID) error
	// HeldSince answers the held prompts queued at or after a time, in queue
	// order: what a rollback to a prompt sent then drops. Session acts are
	// never included.
	HeldSince(ctx context.Context, ws ids.WorkspaceID, since time.Time) ([]ids.TurnID, error)
	// RollBack runs a rollback while owning the workspace's turn sequence
	// (under the delivery lock), dropping the planned held prompts only once
	// it succeeds; ErrHoldsChanged when they are no longer the ones standing.
	RollBack(ctx context.Context, ws ids.WorkspaceID, since time.Time, drop []ids.TurnID, perform func(context.Context) error) error
	// Exclusively runs PERFORM as the workspace's one standing shim call: no
	// prompt is delivered to the workspace until it returns, and a prompt
	// submitted meanwhile is held and delivered once it has. WHAT names the
	// call for the record.
	Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error
	// Submit runs one submission through recognition-free delivery: the lease
	// policy, the classifier, the hold decision, and the shim call. The
	// disposition is the answer.
	Submit(ctx context.Context, sub Submission) (Disposition, error)
	// ReleaseReconnectHolds is told that a session has come up on the
	// workspace, however it came up (a start, a retried vendor start, a
	// relaunch's resume, a cold-gate re-open): it un-stamps every reconnect
	// hold and delivers the next entry down the ordinary path.
	ReleaseReconnectHolds(ws ids.WorkspaceID)
	// ColdGateAnswered settles what a cold gate's answer does to the prompts
	// held at the gate (gatefate.go): discarded, kept for the user, or sent
	// once the remediated session is up. The answer calls it BEFORE it brings
	// the session back, and an error refuses the answer.
	ColdGateAnswered(ctx context.Context, ws ids.WorkspaceID, fate GateFate) error
	// OnVendorServes is told that the workspace's mid-session vendor block
	// stopped standing (footer.WithVendorServes): off the caller's goroutine,
	// it releases the prompts held after reconnect while the block stood,
	// classifies them and delivers them in order (vendorblock.go).
	OnVendorServes(ws ids.WorkspaceID)
	// Release delivers a held prompt now (UpdateHeldPrompt.release).
	Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Drop discards a held prompt (UpdateHeldPrompt.drop).
	Drop(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Supersede retires a held prompt the daemon submitted, because a newer
	// one says everything it said, and answers whether it was retired: false
	// for a prompt already told (holdactions.go).
	Supersede(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error)
	// BeginEdit claims a held prompt as being edited (EditHeldPrompt.begin).
	// While the claim stands, the prompt and every prompt queued after it are
	// withheld from delivery. It refuses a turn nothing was held under
	// (ErrNoSuchHold), a dropped prompt (ErrNotHeld), a delivered one
	// (ErrAlreadyDelivered), a second edit on the workspace (ErrBeingEdited)
	// and a workspace whose editor probe answers false (ErrNoEditor).
	BeginEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, editor EditorProbe) error
	// CommitEdit replaces the edited prompt's content, discards its verdict,
	// retires the claim and reclassifies the prompt through the ordinary
	// classifier path (EditHeldPrompt.commit).
	CommitEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) error
	// CancelEdit retires the claim with the content unchanged and resumes the
	// queue (EditHeldPrompt.cancel).
	CancelEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Fold folds the held prompt TURN into ABOVE, the entry the client saw
	// directly ahead of it (FoldHeldPrompt): ABOVE takes TURN's content after
	// a blank line, keeps its place, has its verdict discarded and is
	// reclassified, and TURN leaves the queue, in one store transaction. It
	// refuses, folding nothing, a turn nothing was held under (ErrNoSuchHold),
	// one no longer held (ErrNotHeld), a session act (ErrNotAPrompt), an ABOVE
	// that is no longer directly ahead (AboveMovedError), a session act ahead
	// (ErrAboveNotAPrompt), and either entry being edited (BeingEditedError).
	Fold(ctx context.Context, ws ids.WorkspaceID, turn, above ids.TurnID) error
	// EditorGone retires the workspace's claim, as a cancel would, because no
	// editor's host stream stands for it any more. The server calls it on the
	// last host stream's close; a workspace with no claim is a no-op.
	EditorGone(ws ids.WorkspaceID)
	// Editing answers the workspace's standing edit, false when none stands.
	Editing(ws ids.WorkspaceID) (Edit, bool)
	// SubmitSessionAct sends a session act down the same path.
	SubmitSessionAct(ctx context.Context, ws ids.WorkspaceID, act Act) error
	// OnTurnEnded is the LifecycleSink's turn end: pop the queue and deliver
	// the next prompt.
	OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose)
	// OnTurnAdopted is the LifecycleSink's vendor-started turn: its durable
	// row is recorded and the roster takes its turn fact, exactly as for a
	// turn the queue delivered, so its end closes through the one door.
	OnTurnAdopted(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnsEndedUnobserved is the LifecycleSink's adoption reconciliation:
	// each turn ended while no daemon was watching, so its durable row is
	// closed as orphaned. Nothing is popped or delivered: none of them was the
	// adopted session's turn in flight.
	OnTurnsEndedUnobserved(ws ids.WorkspaceID, turns []ids.TurnID)
	// CloseOrphans closes, in one transaction, every turn of the workspace
	// that has no terminal, as orphaned, and draws each one's ending in the
	// feed. It is THE DOOR for a boot's and a teardown's close (turnclose.go):
	// no other production code closes a turn row.
	CloseOrphans(ctx context.Context, ws ids.WorkspaceID, at time.Time) (wsm.OrphanReport, error)
	// ClaimDisplacedTurn takes a displaced turn's mark exclusively (the
	// database arbitrates between the merge's release and the boot sweep) and
	// reports whether this caller took it. A turn the claim closes — its
	// capture's kill never produced a terminal — is closed as orphaned and its
	// ending drawn, through the same door.
	ClaimDisplacedTurn(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error)
	// OnLeaseChanged re-evaluates every hold against the new lease policy.
	OnLeaseChanged(ws ids.WorkspaceID)
	// RequestBounce asks the per-workspace BOUNCE REGISTRY to replace what
	// serves a workspace (bounce.go): at once when nothing is in flight or the
	// request is forced, else when the workspace's work ends. The queue owns
	// the decision because it owns dispatch: both are taken under one
	// per-workspace lock, and a decided bounce DRAINS the workspace — nothing is
	// dispatched until it has finished, and what was queued is then delivered
	// to the new shim. Queued prompts never block a bounce.
	RequestBounce(ctx context.Context, ws ids.WorkspaceID, req bounce.Request) (bounce.Decision, error)
	// EndKeptDrain ends the drain a KeepDraining bounce left standing after
	// it ran -- a handover transfer whose workspace this daemon then took back
	// -- and resumes dispatch on what serves the workspace now. A workspace
	// with no kept drain is success.
	EndKeptDrain(ws ids.WorkspaceID)
	// SealMove is a running DISPATCH-QUIET move's seal (handoff.go): it takes
	// what the queue holds for the workspace only in memory -- the queued
	// acts, the running cut, the semantic head -- and the shim replacements
	// the move is carrying, supersedes every verdict still being judged, and
	// refuses every later request with bounce.ErrMovedAway. Only the move's
	// own action calls it, once, before it releases the workspace.
	SealMove(ctx context.Context, ws ids.WorkspaceID) (bounce.Handoff, []bounce.Request, error)
	// UnsealMove puts back what SealMove took, for a move that did not land.
	UnsealMove(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error
	// AdoptHandoff installs what the previous daemon's queue held in memory
	// for a workspace this daemon is adopting mid-work, BEFORE the adopted
	// shim is dialed.
	AdoptHandoff(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error
	// RejudgeHeld re-judges, against the adopted shim's running turn, every
	// held prompt whose verdict the previous daemon's seal superseded.
	RejudgeHeld(ctx context.Context, ws ids.WorkspaceID) error
	// FlushHeld sends every held prompt to the shim at once, in order, to join
	// the running turn with nothing interrupted (flush.go): what an in-place
	// merge the agent's turn asked for does, so the user's prompts are
	// answered before the merge drives the session. It answers an error only
	// when the queue's state could not be read.
	FlushHeld(ctx context.Context, ws ids.WorkspaceID) error
	// OnFree is the watcher's freeness edge: the last turn or detached item
	// ended. It takes a registered bounce.
	OnFree(ws ids.WorkspaceID)
	// OnDeparted is the watcher's DEPARTURE edge: the shim it watched is gone,
	// and all of that shim's in-flight work ended with it. A bounce registered
	// behind that work is decided at once -- taken, or unregistered when it
	// would replace a shim nothing is left to replace (bounce.Request's
	// ReplacesShim) -- under the same delivery lock OnFree decides under, so
	// the two can never both take it. It never blocks: the decision runs on a
	// goroutine of its own, which Drain joins.
	OnDeparted(ws ids.WorkspaceID, departed Watcher, departure sessionwatcher.Departure)
	// Reviving reports whether a background revival this queue started for
	// the workspace is still in flight: from before its bring-up spawns a shim
	// until the prompt it holds has been handed to that shim. The idle sweep
	// reads it so it never hibernates the session a prompt is reviving.
	Reviving(ws ids.WorkspaceID) bool
	// RestoreHolds reloads every standing hold at boot, ALL-OR-NOTHING: a
	// corrupt row fails the restore and nothing is loaded.
	RestoreHolds(ctx context.Context) error
	// Drain waits, BOUNDED, for the queue's own background goroutines — the
	// asynchronous classification verdicts and the background revivals — to
	// finish, and reports whether they all did.
	//
	// THE ORDERLY EXIT CALLS IT BEFORE THE STATE CLIENT CLOSES. Both of those
	// goroutines read and write that client off their own goroutine, so a
	// SIGTERM landing inside one left `daemon.promptqueue.tray: could not read
	// the standing holds — sql: database is closed` and
	// `daemon.promptqueue.classify: the verdict was recorded but the tray was
	// not republished` in the log of an ORDERLY exit.
	Drain(bound time.Duration) bool
}

// TurnEdges is told a workspace's turn edges. Both return at once.
type TurnEdges interface {
	TurnStarted(ws ids.WorkspaceID)
	TurnEnded(ws ids.WorkspaceID)
	TurnEndedUnobserved(ws ids.WorkspaceID)
}

// LeaseWatcher is told each lease change of a workspace (Deps.Leases).
type LeaseWatcher interface {
	LeaseChanged(ws ids.WorkspaceID)
}

// MergeRequests answers whether a turn is the one that asked for a merge
// (merge.Orchestrator.TakeRequestingTurn, Deps.Merges).
type MergeRequests interface {
	// TakeRequestingTurn reports whether turn of ws is the turn that asked
	// for a merge of ws, and forgets it, so a turn is asked about once.
	TakeRequestingTurn(ws ids.WorkspaceID, turn ids.TurnID) bool
}

// Deps are the queue's collaborators.
type Deps struct {
	// DB is the durable hold store and the lease's policy metadata.
	DB wsm.DB
	// Leases is told each lease change of a workspace, once the queue has
	// re-judged its holds (OnLeaseChanged, the one sink every lease change
	// reaches): a merge waiting for a workspace another holder held takes it
	// from here (merge.Orchestrator.LeaseChanged). REQUIRED.
	Leases LeaseWatcher
	// Merges says which turn asked for a merge, so that turn's end raises no
	// banner: the merge queue's own banners announce it. REQUIRED.
	Merges MergeRequests
	// Judge classifies interjection.
	Judge classifier.Judge
	// Feed mirrors an accepted prompt as a user_prompt row.
	Feed feed.Resolver
	// Footer carries the waiting-interrupting status, which fires the MOMENT
	// an interrupt registers rather than when the turn actually ends.
	Footer footer.Resolver
	// Sidebar carries the roster's own turn facts. The roster draws
	// `submitting` and `thinking` from the SAME edge the footer does — the
	// daemon's acceptance of a turn — because nothing on the shim's streams
	// states it, and a roster left to infer it reads `ready` while a turn runs.
	// REQUIRED.
	Sidebar sidebar.Resolver
	// Edges is told each turn's start, where the shim took it, and its end,
	// where the queue closes it (wsedges.Hub): the one origin of the turn
	// edges agents are told of.
	Edges TurnEdges
	// Holds is the feed's held section, which the queue states its standing
	// holds and edit claim to.
	Holds holds.Sink
	// Client resolves a workspace's shim client; the queue never dials one
	// itself.
	Client ClientFunc
	// Revive brings a parked workspace's session back up so a submission to it
	// is DELIVERED rather than refused. A hibernated session is idle, not
	// dead: the prompt is the revival, exactly as mounting the frontend is.
	// Nil means a workspace with no live session simply refuses.
	Revive ReviveFunc
	// SettingsStarted is told when a settings pick starts a workspace's shim
	// (a model, permission-mode or effort pick with no shim), with the
	// instant: the workspace verbs stand that shim down again on the switch
	// away unless a turn has run since (owner request, 2026-10-29). Nil
	// follows no such start.
	SettingsStarted func(ws ids.WorkspaceID, at time.Time)
	// Watcher resolves a workspace's session watcher, which is what answers
	// the in-flight turn and what an accepted turn is handed over to.
	Watcher WatcherFunc
	// RestartSlots is the daemon-wide cap on vendor starts in flight, the
	// SAME pool the fleet's starts take their slot from (startslots). A shim
	// replacement takes its slot here before it drains anything and holds it
	// for the whole replacement, so a deploy restarts a few shims at a time.
	// Nil admits every replacement at once.
	RestartSlots *startslots.Pool
	// SessionStarted reports whether the workspace's installed shim holds a
	// STARTED session. A client is not a session: a relaunch installs its
	// shim before the resume, and a vendor start being retried (or one that
	// failed) leaves a shim with none. A submission there is held under the
	// reconnect hold rather than sent to be refused `no_session`.
	SessionStarted func(ws ids.WorkspaceID) bool
	// SessionAbsent reports that the workspace's installed shim is KNOWN to
	// hold no session: this daemon prelaunched it and its StartSession has not
	// succeeded (being retried, failed, or not yet sent). It is NOT the
	// negation of SessionStarted: an adopted shim whose facts have not arrived
	// is neither started nor absent, it is UNKNOWN, and unknown is never free.
	// A shim known to hold no session runs no turn and holds no live work, so
	// it never holds a bounce (inFlight).
	SessionAbsent func(ws ids.WorkspaceID) bool
	// ColdGate answers the STANDING cold gate's own account for a workspace,
	// false when no gate stands. A gate is a refusal with a name of its own,
	// and this is what lets the queue give it rather than reporting a missing
	// session. Nil means no workspace is ever read as gated.
	ColdGate ColdGateFunc
	// ColdGateHeld is told, on every republish of the hold tray, how many
	// prompts are held at a cold gate (gatefate.go), so the standing gate
	// offers its compact-and-resume answer exactly while there is something to
	// send. nil tells nobody.
	ColdGateHeld func(ws ids.WorkspaceID, held int)
	// DrainRefusals records each submission the drain lease refused or held.
	// The controller rate-limits its own record; the queue only reports.
	DrainRefusals RefusalNoter
	// StandingDown answers whether this daemon's own stand-down is under way
	// (the shim supervisor's latch, which never clears). A turn that closes
	// interrupted while it stands is a cut the daemon ordered and is owed a
	// resumption, and nothing held is delivered into a shim about to go
	// (restartredrive.go). REQUIRED.
	StandingDown func() bool
	// Resumptions is told what every session-up release did about the
	// resumption a stand-down owes the workspace (restartredrive.go), so an
	// editor's open of the workspace waits for a resumed turn to have started
	// before it answers the workspace open (opentabs). nil tells nobody.
	Resumptions func(ws ids.WorkspaceID, report ResumeReport)
	// StripSentinels removes the host's metaprompt sentinel spans from the
	// MIRRORED text; the full text stays on the durable record and on the
	// prompt the shim receives. nil is the identity.
	StripSentinels func(string) string
	// ResolveImage turns an attached image's reference into a src the webview
	// can load, for the MIRRORED row. It is the SAME resolver the feed
	// resolver holds: the mirror and the replayed draw are one row under one
	// key, so they resolve an image the same way or they disagree about what
	// the person attached. REQUIRED -- a nil default here is what made an
	// attached image invisible for a whole live session.
	ResolveImage feed.ImageResolver
	// Lifetime is the daemon's serving lifetime, which a bounce the registry
	// runs is bounded by. nil leaves it bounded by the process alone.
	Lifetime context.Context
	// Now supplies the instants the queue stamps. nil means time.Now.
	Now func() time.Time
	// Stat reads a workspace directory before a dead shim's session is
	// brought back: a workspace whose directory is gone has nothing to serve.
	// nil means os.Stat.
	Stat func(name string) (fs.FileInfo, error)
	// PublishHost republishes a workspace's host view, which carries the
	// standing edit the editor fills its input from. REQUIRED.
	PublishHost func(ws ids.WorkspaceID)
	// TurnBanners raises the desktop banner a turn end earns. It is told on
	// THIS edge — the one that installs the turn's close on the roster and
	// draws its ending in the feed — so a banner is raised for exactly the
	// turns whose end the clients are shown. REQUIRED.
	TurnBanners TurnBanners
	// Commits recounts a workspace's branch commits. THE DOOR tells it every
	// turn close, so the roster's detail panel states the count as of the
	// workspace's last turn. REQUIRED.
	Commits CommitRecounter
	// CacheClocks is the keep-alive keeper (cachelapse.Keeper.TurnClosed).
	// THE DOOR tells it every turn close AFTER the durable write, because
	// the cache clock it times beats from is read off the closed rows: the
	// session watcher's free edge comes before that write, so a keeper
	// timing the beat from the free edge alone could find no turn ended
	// (2026-10-26). REQUIRED.
	CacheClocks CacheClocks
	// Log is the queue's logger.
	Log dlog.Surfaces
	// Stalls is the lock stall watchdog. OPTIONAL: nil leaves the queue's
	// locks unwatched, which is what every test that does not exercise it
	// leaves it. Production always wires it, because Submit takes the queue's
	// mutex and then the workspace's delivery lock, so a hold past Emacs's
	// unary timeout on either is a SubmitPrompt that never answers.
	Stalls lockwatch.Registry
}

// CommitRecounter owes a workspace a recount of its branch commits
// (branchcommits.Recounter.TurnClosed). It never blocks the door.
type CommitRecounter interface {
	TurnClosed(ws ids.WorkspaceID)
}

// CacheClocks re-times a workspace's next keep-alive once a turn's close is
// on the record (cachelapse.Keeper.TurnClosed). It never blocks.
type CacheClocks interface {
	TurnClosed(ws ids.WorkspaceID)
}

// TurnBanners raises a turn end's desktop banner
// (desktopnotify.TurnBanners.OnTurnEnded).
type TurnBanners interface {
	OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how wsm.TurnClose)
}

// ColdGateRefusal is ErrColdGate carrying the GATE'S OWN sentence, which is
// what the `cold_gate` arm's `detail` field is filled from. It is a type
// rather than a wrapped error string so the arm carries the gate's account
// alone, never this package's name prefixed to it.
type ColdGateRefusal struct {
	// Detail is the standing gate's own account of what was refused cold.
	Detail string
}

// Error names the refusal, the gate's own sentence included.
func (e *ColdGateRefusal) Error() string {
	if e.Detail == "" {
		return ErrColdGate.Error()
	}
	return ErrColdGate.Error() + ": " + e.Detail
}

// Unwrap answers the sentinel, so `errors.Is(err, ErrColdGate)` holds.
func (e *ColdGateRefusal) Unwrap() error { return ErrColdGate }

// ColdGateFunc answers the standing cold gate's detail for a workspace,
// reporting false when no gate stands. workspace.Fleet.ColdGateDetail is it.
type ColdGateFunc func(ws ids.WorkspaceID) (string, bool)

// ClientFunc resolves a workspace's live shim client, reporting false when the
// workspace has none. It is injected so the queue does not own the fleet.
type ClientFunc func(ws ids.WorkspaceID) (Sender, bool)

// ReviveFunc brings one workspace's session up. workspace.Fleet.Start
// satisfies it, and it is idempotent for a session that is already live.
type ReviveFunc func(ctx context.Context, ws ids.WorkspaceID) error

// SentBy is the workspace whose agent sent a prompt, as the shim declares it
// to the vendor (shim.v1 StartTurnSentBy): the prompt reaches the recipient's
// agent as that workspace's message, declaring the SENDER's own permission
// mode, never the recipient's.
type SentBy struct {
	// Workspace is the sending workspace. Never empty.
	Workspace ids.WorkspaceID
	// Name is the sending workspace's name, "" when the daemon can no longer
	// name it (a sender forgotten since it sent).
	Name string
	// PermissionMode is the sending workspace's recorded permission mode, ""
	// when the daemon holds no session record for it.
	PermissionMode string
}

// Sender is the slice of the shim client the queue uses. Keeping it narrow is
// what lets the queue be tested against a fake without a whole shim.
type Sender interface {
	// StartTurn opens a turn with its minted id and required origin. It
	// answers the shim's success, whose `prompt` names the main agent and
	// whose `page` is the opening page.
	//
	// SEAM CHANGE (recorded): the skeleton returned only an error. The queue
	// must hand StartTurnSuccess.prompt.agent to the watcher's SetMainAgent
	// and the pair to OnTurnOpened — ARCHITECTURE names the queue as the
	// authoritative source of both — so the success has to come back here.
	//
	// SENTBY names the workspace whose agent sent the prompt, set exactly
	// when origin is PROMPT_ORIGIN_AGENT_SENT (StartTurnRequest.sent_by).
	StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *SentBy) (*shimv1.StartTurnSuccess, error)
	// JoinRunningTurn sends the prompt to join the daemon's turn in flight
	// after its current tool call, with nothing interrupted
	// (StartTurnRequest.join_running_turn). Answered as StartTurn is.
	JoinRunningTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *SentBy) (*shimv1.StartTurnSuccess, error)
	// StartInterjection opens the turn of a prompt that interrupted the
	// running one, with NOTE for the agent alone (StartTurnRequest.vendor_note).
	// Answered as StartTurn is.
	StartInterjection(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *SentBy, note string) (*shimv1.StartTurnSuccess, error)
	// PromptAgent delivers a bubble-composer prompt to one agent.
	PromptAgent(ctx context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error
	// KillTurn interrupts the open turn for an interjection. commandedBy is
	// HOW the person commanded the stop, relayed to the shim as
	// KillTurnRequest.commanded_by and recorded verbatim as the interrupted
	// terminal's `by_user` cause; nil states none.
	KillTurn(ctx context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error
	// SetModel switches the session's model. It rides this interface because a
	// model change resolves at the turn boundary and respects the lease like
	// any other delivery.
	SetModel(ctx context.Context, model string) error
	// SetPermissionMode switches the session's permission mode, for the same
	// reason.
	SetPermissionMode(ctx context.Context, mode string) error
	// RollBackTurn cuts one turn's prompt, and everything after it, out of the
	// vendor conversation, files kept (shim.v1 RollBackSession). A prompt the
	// vendor never recorded answers ErrPromptNotRecorded.
	RollBackTurn(ctx context.Context, turn ids.TurnID) error
}

// WatcherFunc resolves a workspace's session watcher, reporting false when the
// workspace has none. sessionwatcher.Watcher satisfies Watcher.
type WatcherFunc func(ws ids.WorkspaceID) (Watcher, bool)

// Watcher is the slice of the session watcher the queue uses.
type Watcher interface {
	// TurnInFlight reports the open turn, nil when none is.
	TurnInFlight() *ids.TurnID
	// Departed reports that the watched shim is gone. A departed watcher's
	// turn and live work ended with the shim, so a bounce never waits on them.
	Departed() (sessionwatcher.Departure, bool)
	// ShimGone reports that the watched shim is gone: departed, or reaped
	// with its departure still on its way (sessionwatcher.Watcher.ShimGone).
	ShimGone() bool
	// LiveWork is the live detached work: what a bounce registered on the
	// workspace waits on besides the turn.
	LiveWork() sessionwatcher.LiveWorkSet
	// Free is the watcher's own freeness judgement: its facts are in, and
	// they leave no turn in flight and no live work. A pure attach whose
	// facts have not arrived is NOT free, whatever TurnInFlight answers.
	Free() bool
	// SessionFactsIn reports, without waiting, whether the session facts are
	// in. Before they are, TurnInFlight's nil does NOT mean no turn runs: an
	// adopted shim's turn is not yet known.
	SessionFactsIn() bool
	// AwaitSessionFacts blocks until the session facts are in, or ctx ends.
	AwaitSessionFacts(ctx context.Context) error
	// SetMainAgent names the session's main agent from an accepted turn.
	SetMainAgent(agent *conversationv1.AgentId)
	// OnTurnOpening records a turn BEFORE StartTurn is dispatched, so a
	// terminal that arrives on the agent stream ahead of StartTurn's response
	// is still attributable to it.
	OnTurnOpening(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnJoining records a turn about to be sent to join the turn in
	// flight, waiting behind it; false, recording nothing, when the turn in
	// flight is one the vendor started. See sessionwatcher.Watcher.
	OnTurnJoining(ws ids.WorkspaceID, turn ids.TurnID) bool
	// OnTurnOpenFailed retires a turn recorded by OnTurnOpening or
	// OnTurnJoining that the shim then refused.
	OnTurnOpenFailed(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnOpened hands an accepted turn over: the prompt as delivered and
	// the opening page StartTurnSuccess carried.
	OnTurnOpened(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, page *conversationv1.HistoryPage)
}

// RefusalNoter records one drain-refused submission. drain.Controller
// satisfies it.
type RefusalNoter interface {
	// NoteRefusal records that one submission was refused or held under the
	// drain lease.
	NoteRefusal(ws ids.WorkspaceID)
}

// TurnStops is the queue's part in a turn stop: the accepted turn a stop
// lands on may still be the queue's, waiting on a bring-up or on its way to
// the shim. It is kept apart from Queue so the interrupt verb depends on these
// calls alone.
type TurnStops interface {
	// WithdrawRevivalTurn is a turn stop that lands while the workspace's
	// session is still coming up: it retires the accepted turn the status
	// surfaces already show (the first prompt held for the revival), so the
	// stop they offer is honored rather than answered "nothing running" and
	// then run anyway. It reports false, and retires nothing, when no revival
	// is in flight or nothing waits on it, which is when the turn (if any) is
	// already the session's own to kill.
	WithdrawRevivalTurn(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// AwaitTurnStarted returns once no call that opens TURN on WS stands: the
	// shim has the turn, or its start failed. A kill for the turn waits on it,
	// so the kill can never reach the shim ahead of the start and find no
	// turn open. It answers ctx's error when ctx ends first.
	AwaitTurnStarted(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
}

// Service is everything the built queue answers: the delivery path and its
// part in a turn stop.
type Service interface {
	Queue
	TurnStops
	HeldRollback
}

// HeldRollback is the stop key's take-back of a held prompt: the one part of
// the queue the server's Interrupt needs beyond Queue.
type HeldRollback interface {
	// RollBackHeld takes back the newest prompt the user sent that stands
	// held while a turn runs, when nothing answering has been drawn in the
	// root feed since it was queued: it is retired as rolled back and
	// answered with ok true. ok is false, and nothing changes, otherwise.
	RollBackHeld(ctx context.Context, ws ids.WorkspaceID) (held wsm.HeldPrompt, ok bool, err error)
}

// New builds the queue.
func New(deps Deps) (Service, error) {
	return newQueue(deps)
}
