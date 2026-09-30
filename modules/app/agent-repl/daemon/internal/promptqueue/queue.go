package promptqueue

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/wsm"
)

// The queue's operation names. Every logical branch records under one of them,
// per the logging contract.
const (
	opNew         = "daemon.promptqueue.new"
	opSubmit      = "daemon.promptqueue.submit"
	opDeliver     = "daemon.promptqueue.deliver"
	opHold        = "daemon.promptqueue.hold"
	opClassify    = "daemon.promptqueue.classify"
	opInterject   = "daemon.promptqueue.interject"
	opRelease     = "daemon.promptqueue.release"
	opDrop        = "daemon.promptqueue.drop"
	opAccept      = "daemon.promptqueue.accept"
	opAct         = "daemon.promptqueue.session_act"
	opTurnEnded   = "daemon.promptqueue.turn_ended"
	opTurnAdopted = "daemon.promptqueue.turn_adopted"
	opLeaseChange = "daemon.promptqueue.lease_changed"
	opRestore     = "daemon.promptqueue.restore_holds"
	opRevive      = "daemon.promptqueue.revive"
	opTray        = "daemon.promptqueue.tray"
)

// wsState is the queue's in-memory memory of one workspace: the semantic head
// an interjection installed, the running turn's uninterruptibility, and the
// session acts waiting behind the queue.
//
// NONE OF IT IS DURABLE, deliberately. The head is a within-turn ordering fact
// that a restart resolves by re-reading the durable holds; the pending acts are
// a within-process ordering fact whose loss costs a re-issued model change, not
// a lost prompt.
type wsState struct {
	// drain serializes the two events that DELIVER a standing hold — a turn's
	// end and a lease change — for one workspace. Without it a handover's
	// quiesce (a lease change) runs concurrently with the turn end that freed
	// the workspace, and the held intake leaves in whichever order the two
	// races settle in rather than in arrival order.
	//
	// It is NOT q.mu: a delivery is an rpc to the shim, and holding the
	// queue's own mutex across it would wedge every other workspace.
	drain lockwatch.Mutex

	// reviving reports that a BACKGROUND revival goroutine is already running,
	// so a second submission joins it rather than spawning a second one.
	reviving bool

	// bringUps counts the revivals actually in flight for this workspace,
	// from every caller -- the background one and the in-line one a hold's
	// delivery takes. A release that lands while one is running is answered
	// "the session is still coming up", never "there is no session".
	bringUps int

	// unattendedRevival reports that this queue already brought the session
	// back after a shim died on its own and no turn has ended since. A second
	// death in that state is a shim that cannot hold a session, and it is
	// left down for the next prompt rather than respawned in a loop. Guarded
	// by q.mu; reset by every turn end (OnTurnEnded).
	unattendedRevival bool

	// bounce is the workspace's standing bounce: registered while work is in
	// flight, DRAINING once decided (bounce.go). nil when none stands. Guarded
	// by q.mu; DECIDED only under drain.
	bounce *pendingBounce

	head         *ids.TurnID
	interrupting bool

	// joining is the prompt the queue sent to join the running turn after
	// its current tool call (join.go), nil when none waits. While it stands,
	// the vendor has not yet folded it in nor run it as its own turn, and no
	// prompt behind it is classified. Guarded by q.mu; set under drain,
	// cleared by the prompt's own turn ending, folded or run.
	joining *joiningPrompt

	// edit is the workspace's standing held-prompt edit, nil when none
	// stands. It is WRITTEN only while `drain` is held — the lock every
	// delivery of a standing hold is decided under — and under mu as well,
	// so a reader that decides nothing (the host view, the tray) takes mu
	// alone. See edit.go.
	edit *editClaim
	// watched reports that drain and verdicts are registered with the stall
	// watchdog. Guarded by q.mu.
	watched bool
	// verdicts serializes a classifier verdict's SETTLING (its record and, on
	// an interject, the interrupt) against an edit's commit replacing the
	// content it judged; epochs counts each turn's content replacements under
	// it. A verdict captured at one epoch settles only while that epoch still
	// stands, so a judge that was already in flight when the content changed
	// can never stamp — or interject — the new content with the old verdict.
	// Lock order: drain, then verdicts; nothing holding verdicts takes drain.
	verdicts lockwatch.Mutex
	epochs   map[ids.TurnID]uint64
	// cut is the context cut — /clear or /compact — that is the running
	// turn, nil when none is. It is THE QUEUE'S KNOWLEDGE THAT THE RUNNING
	// TURN IS A SESSION ACT: every path that could interrupt or overtake the
	// running turn (a verdict, an interjection, a release, the turn end's
	// pop) reads it. Guarded by q.mu; set by runContextCut, retired by the
	// turn's end or a refused start.
	cut *runningCut
}

// runningCut is a context cut running as the session's turn.
type runningCut struct {
	// turn is the cut's own turn.
	turn ids.TurnID
	// command is /clear or /compact.
	command conversationv1.SessionCommand
}

// runningCut answers the context cut that is the workspace's running turn. It
// takes q.mu alone and waits on nothing else.
func (q *queue) runningCut(ws ids.WorkspaceID) (runningCut, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	if !ok || state.cut == nil {
		return runningCut{}, false
	}
	return *state.cut, true
}

// queue is the one delivery path.
type queue struct {
	deps Deps

	mu     lockwatch.Mutex
	states map[ids.WorkspaceID]*wsState

	// classifying tracks the in-flight classification goroutines. It is a
	// WaitGroup rather than a sleep so a test can join them.
	classifying sync.WaitGroup

	// reviving tracks the in-flight background revivals. It is a WaitGroup
	// rather than a sleep so a test can join them.
	reviving sync.WaitGroup

	// bouncing tracks the bounces running on their own goroutines, so Drain
	// joins them rather than closing the state client under one.
	bouncing sync.WaitGroup

	// departing tracks the departure decisions (OnDeparted) running on their
	// own goroutines, so Drain joins them too.
	departing sync.WaitGroup
	// exiting is set by Drain, under mu, before it joins: a departure told
	// after it (the exit's own watcher closes run after the drain) decides
	// nothing, because the state client it would read is about to close and
	// every registered bounce ends with the process anyway.
	exiting bool

	// editSeq mints each edit's identity; guarded by mu.
	editSeq uint64
}

// newQueue validates the dependencies and builds the queue. Every collaborator
// the queue cannot invent is REQUIRED: a nil one is a wiring defect, and a
// queue that discovered it at the first submission would lose that prompt.
func newQueue(deps Deps) (*queue, error) {
	switch {
	case deps.Log == nil:
		return nil, fmt.Errorf("the prompt queue needs log surfaces")
	case deps.DB == nil:
		return nil, fmt.Errorf("the prompt queue needs a state client")
	case deps.Judge == nil:
		return nil, fmt.Errorf("the prompt queue needs a classifier")
	case deps.Feed == nil:
		return nil, fmt.Errorf("the prompt queue needs the feed resolver")
	case deps.TurnBanners == nil:
		return nil, fmt.Errorf("the prompt queue needs the turn banners")
	case deps.Footer == nil:
		return nil, fmt.Errorf("the prompt queue needs the footer resolver")
	case deps.Holds == nil:
		return nil, fmt.Errorf("the prompt queue needs the holds resolver")
	case deps.Client == nil:
		return nil, fmt.Errorf("the prompt queue needs a shim client resolver")
	case deps.Watcher == nil:
		return nil, fmt.Errorf("the prompt queue needs a session watcher resolver")
	case deps.ResolveImage == nil:
		return nil, fmt.Errorf("the prompt queue needs an image resolver for the rows it mirrors")
	case deps.PublishHost == nil:
		return nil, fmt.Errorf("the prompt queue needs the host view's publisher for the edits it claims")
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	if deps.Stat == nil {
		deps.Stat = os.Stat
	}
	if deps.StripSentinels == nil {
		deps.StripSentinels = func(s string) string { return s }
	}
	q := &queue{
		deps:   deps,
		states: make(map[ids.WorkspaceID]*wsState),
	}
	// THE QUEUE'S OWN MUTEX IS DAEMON-WIDE: every workspace's Submit takes
	// it, so its stall is recorded in the run log. It lives as long as the
	// process, so it is never unwatched.
	if deps.Stalls != nil {
		deps.Stalls.Watch(&q.mu, stallQueueLock, "", deps.Log.Global())
	}
	deps.Log.Global().Debug(opNew, "the prompt queue is wired", nil)
	return q, nil
}

// The names the stall watchdog records the queue's locks under.
const (
	stallQueueLock    = "promptqueue.queue"
	stallDrainLock    = "promptqueue.drain"
	stallVerdictsLock = "promptqueue.verdicts"
)

// state returns a workspace's memory, minting it on first sight.
func (q *queue) state(ws ids.WorkspaceID) *wsState {
	q.mu.Lock()
	defer q.mu.Unlock()
	return q.stateLocked(ws)
}

// stateLocked is state with q.mu held.
func (q *queue) stateLocked(ws ids.WorkspaceID) *wsState {
	s, ok := q.states[ws]
	if !ok {
		s = &wsState{}
		q.states[ws] = s
	}
	return s
}

// watchLocks registers a workspace's delivery and verdict locks with the stall
// watchdog, once, with the workspace's own logger: a stall on them is the
// workspace's record. It runs where that logger is first resolved, because
// the memory is minted under q.mu where no logger can be (resolving one reads
// the state client). Every entry point resolves the logger before it takes
// drain; the one that does not, a departure's decision, acts only on a bounce
// RequestBounce registered, which did. The memory lives as long as the process, so the locks are never unwatched.
func (q *queue) watchLocks(ws ids.WorkspaceID, log dlog.Logger) {
	if q.deps.Stalls == nil {
		return
	}
	q.mu.Lock()
	s := q.stateLocked(ws)
	first := !s.watched
	s.watched = true
	q.mu.Unlock()
	if !first {
		return
	}
	q.deps.Stalls.Watch(&s.drain, stallDrainLock, ws, log)
	q.deps.Stalls.Watch(&s.verdicts, stallVerdictsLock, ws, log)
}

// logger resolves a workspace's durable logger. Failing to resolve the
// workspace is an invariant violation, never a reason to write globally.
func (q *queue) logger(ctx context.Context, ws ids.WorkspaceID) (dlog.Logger, error) {
	record, err := q.deps.DB.Workspace(ctx, ws)
	if err != nil {
		q.deps.Log.Global().Error(opSubmit, "could not resolve the workspace", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return nil, fmt.Errorf("resolve workspace %q: %w", ws, err)
	}
	// RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION, and a prompt is
	// never lost over WHERE its narration is written. A directory that cannot
	// host a durable sink routes to the central sink carrying
	// `unroutable_workspace'; only the WORKSPACE READ above can refuse.
	log := q.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)})
	q.watchLocks(ws, log)
	return log, nil
}

// pushTray republishes a workspace's standing holds, whole. The queue is the
// tray's only author for prompts, so every state change ends here.
func (q *queue) pushTray(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) error {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opTray, "could not read the standing holds", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	q.deps.Holds.SetHeldPrompts(ws, standing)
	log.Debug(opTray, "republished the hold tray", dlog.Context{"holds": len(standing)})
	return nil
}

// standingHold finds one workspace's standing hold by turn.
func (q *queue) standingHold(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.HeldPrompt, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return wsm.HeldPrompt{}, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	for _, h := range standing {
		if h.Turn == turn {
			if h.Tombstone != nil {
				return wsm.HeldPrompt{}, ErrAlreadyDelivered
			}
			return h, nil
		}
	}
	return wsm.HeldPrompt{}, ErrNoSuchHold
}

// waitForClassifications joins every in-flight classification goroutine. It is
// unexported and exists for the package's own tests, which must observe a
// verdict without sleeping for it.
func (q *queue) waitForClassifications() { q.classifying.Wait() }

// Drain implements Queue: a BOUNDED join of the classification verdicts, the
// background revivals, and the bounces the registry is running. It reports
// whether they all left, so the caller decides what an overrun means rather
// than this package guessing — and it is bounded because an unbounded wait is
// a daemon that does not exit.
func (q *queue) Drain(bound time.Duration) bool {
	q.mu.Lock()
	q.exiting = true
	q.mu.Unlock()
	left := make(chan struct{})
	go func() {
		q.classifying.Wait()
		q.reviving.Wait()
		// A departure decision can START a bounce, so it is joined first.
		q.departing.Wait()
		q.bouncing.Wait()
		close(left)
	}()
	select {
	case <-left:
		return true
	case <-time.After(bound):
		return false
	}
}

// saidText renders a submission's text for the classifier and the durable turn
// record: the text blocks, joined. Images carry no text and contribute none.
func saidText(said *conversationv1.UserSaid) string {
	out := ""
	for _, block := range said.GetContent().GetBlocks() {
		if text, ok := block.GetBlock().(*conversationv1.UserContentBlock_Text); ok {
			if out != "" {
				out += "\n"
			}
			out += text.Text.GetText()
		}
	}
	return out
}
