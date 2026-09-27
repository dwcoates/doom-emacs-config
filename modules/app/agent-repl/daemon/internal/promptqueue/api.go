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

	"claude-repld/internal/bounce"
	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
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
//	ErrAcceptNotApplicable → UpdateHeldPromptError.accept_not_applicable
//	ErrReleaseRefused    → UpdateHeldPromptError.release_refused
//	ErrNoSuchHold        → EditHeldPromptError.no_such_hold
//	ErrNotHeld           → EditHeldPromptError.not_held
//	ErrAlreadyDelivered  → EditHeldPromptError.already_delivered
//	ErrBeingEdited       → EditHeldPromptError.being_edited
//	ErrNotEditing        → EditHeldPromptError.not_editing
//	ErrNoEditor          → EditHeldPromptError.no_editor
var (
	// ErrMerging is a submission that arrived AFTER a merge began. It is
	// refused outright rather than held: a merged workspace closes, so work a
	// post-merge-start prompt produced would be orphaned.
	ErrMerging = errors.New("promptqueue: a merge is in flight for this workspace")
	// ErrNoSession is a submission to a workspace with no live shim.
	ErrNoSession = errors.New("promptqueue: the workspace has no session to submit to")
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
	// ErrAcceptNotApplicable is an accept on a verdict other than
	// hold_for_turn_end. The landed contract makes accept legal only there.
	ErrAcceptNotApplicable = errors.New("promptqueue: accept is legal only on a hold_for_turn_end verdict")
	// ErrReleaseRefused is a force-through on an uninterruptible verdict or a
	// session_starting hold: there is nothing delivery could do yet.
	ErrReleaseRefused = errors.New("promptqueue: this hold cannot be released through")
	// ErrNotHeld is an edit of a prompt that WAS held and no longer is: it
	// was dropped.
	ErrNotHeld = errors.New("promptqueue: that prompt is no longer held")
	// ErrBeingEdited is a begin while an edit already stands on the
	// workspace. BeingEditedError carries it with the turn being edited.
	ErrBeingEdited = errors.New("promptqueue: a held prompt is already being edited on this workspace")
	// ErrNotEditing is a commit or cancel naming a prompt no edit stands on.
	ErrNotEditing = errors.New("promptqueue: no edit stands on that prompt")
	// ErrNoEditor is a begin with no editor's host stream to edit in.
	ErrNoEditor = errors.New("promptqueue: no editor is attached to this workspace")
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
	// Target, when set, is the bubble composer's addressed feed row — the
	// prompt goes to THAT agent through UpdateAgent.prompt.
	Target *feedid.Ref
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
	// Submit runs one submission through recognition-free delivery: the lease
	// policy, the classifier, the hold decision, and the shim call. The
	// disposition is the answer.
	Submit(ctx context.Context, sub Submission) (Disposition, error)
	// Release delivers a held prompt now (UpdateHeldPrompt.release).
	Release(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Drop discards a held prompt (UpdateHeldPrompt.drop).
	Drop(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
	// Accept flips a hold_for_turn_end verdict to accepted and re-pushes the
	// tray (UpdateHeldPrompt.accept). It is LEGAL ONLY on a hold_for_turn_end
	// verdict; every other hold refuses.
	Accept(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error
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

// Deps are the queue's collaborators.
type Deps struct {
	// DB is the durable hold store and the lease's policy metadata.
	DB wsm.DB
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
	Sidebar sidebar.Resolver
	// Holds is the tray the queue publishes its holds to.
	Holds holds.Resolver
	// Client resolves a workspace's shim client; the queue never dials one
	// itself.
	Client ClientFunc
	// Revive brings a parked workspace's session back up so a submission to it
	// is DELIVERED rather than refused. A hibernated session is idle, not
	// dead: the prompt is the revival, exactly as mounting the frontend is.
	// Nil means a workspace with no live session simply refuses.
	Revive ReviveFunc
	// Watcher resolves a workspace's session watcher, which is what answers
	// the in-flight turn and what an accepted turn is handed over to.
	Watcher WatcherFunc
	// ColdGate answers the STANDING cold gate's own account for a workspace,
	// false when no gate stands. A gate is a refusal with a name of its own,
	// and this is what lets the queue give it rather than reporting a missing
	// session. Nil means no workspace is ever read as gated.
	ColdGate ColdGateFunc
	// ParkedRoute delivers a submission that arrived under a PARKED merge
	// lease to the resolution agent as guidance. It is a FUNCTION, not the
	// orchestrator, because the queue never imports merge.
	ParkedRoute ParkedRouter
	// DrainRefusals records each submission the drain lease refused or held.
	// The controller rate-limits its own record; the queue only reports.
	DrainRefusals RefusalNoter
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
	// Log is the queue's logger.
	Log dlog.Surfaces
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
	StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error)
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
	// LiveWork is the live detached work: what a bounce registered on the
	// workspace waits on besides the turn.
	LiveWork() sessionwatcher.LiveWorkSet
	// SetMainAgent names the session's main agent from an accepted turn.
	SetMainAgent(agent *conversationv1.AgentId)
	// OnTurnOpening records a turn BEFORE StartTurn is dispatched, so a
	// terminal that arrives on the agent stream ahead of StartTurn's response
	// is still attributable to it.
	OnTurnOpening(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnOpenFailed retires a turn recorded by OnTurnOpening that the shim
	// then refused.
	OnTurnOpenFailed(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnOpened hands an accepted turn over: the prompt as delivered and
	// the opening page StartTurnSuccess carried.
	OnTurnOpened(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, page *conversationv1.HistoryPage)
}

// ParkedRouter delivers one parked submission to the resolution agent as
// guidance, addressed at the parked tab. It answers with the turn the guidance
// runs as. It matches merge.ParkedRouter so the wiring is a direct assignment.
type ParkedRouter func(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) (ids.TurnID, error)

// RefusalNoter records one drain-refused submission. drain.Controller
// satisfies it.
type RefusalNoter interface {
	// NoteRefusal records that one submission was refused or held under the
	// drain lease.
	NoteRefusal(ws ids.WorkspaceID)
}

// New builds the queue.
func New(deps Deps) (Queue, error) {
	return newQueue(deps)
}
