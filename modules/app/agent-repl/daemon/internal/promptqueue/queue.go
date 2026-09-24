package promptqueue

import (
	"context"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
	opLeaseChange = "daemon.promptqueue.lease_changed"
	opRestore     = "daemon.promptqueue.restore_holds"
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
	drain sync.Mutex

	// reviving reports that a BACKGROUND revival goroutine is already running,
	// so a second submission joins it rather than spawning a second one.
	reviving bool

	// bringUps counts the revivals actually in flight for this workspace,
	// from every caller -- the background one and the in-line one a hold's
	// delivery takes. A release that lands while one is running is answered
	// "the session is still coming up", never "there is no session".
	bringUps int

	// bounce is the workspace's standing bounce: registered while work is in
	// flight, DRAINING once decided (bounce.go). nil when none stands. Guarded
	// by q.mu; DECIDED only under drain.
	bounce *pendingBounce

	head         *ids.TurnID
	interrupting bool

	// edit is the workspace's standing held-prompt edit, nil when none
	// stands. It is WRITTEN only while `drain` is held — the lock every
	// delivery of a standing hold is decided under — and under mu as well,
	// so a reader that decides nothing (the host view, the tray) takes mu
	// alone. See edit.go.
	edit *editClaim
	// verdicts serializes a classifier verdict's SETTLING (its record and, on
	// an interject, the interrupt) against an edit's commit replacing the
	// content it judged; epochs counts each turn's content replacements under
	// it. A verdict captured at one epoch settles only while that epoch still
	// stands, so a judge that was already in flight when the content changed
	// can never stamp — or interject — the new content with the old verdict.
	// Lock order: drain, then verdicts; nothing holding verdicts takes drain.
	verdicts        sync.Mutex
	epochs          map[ids.TurnID]uint64
	uninterruptible conversationv1.SessionCommand
	acts            []Act
}

// queue is the one delivery path.
type queue struct {
	deps Deps

	mu     sync.Mutex
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
	if deps.StripSentinels == nil {
		deps.StripSentinels = func(s string) string { return s }
	}
	q := &queue{
		deps:   deps,
		states: make(map[ids.WorkspaceID]*wsState),
	}
	deps.Log.Global().Debug(opNew, "the prompt queue is wired", nil)
	return q, nil
}

// state returns a workspace's memory, minting it on first sight.
func (q *queue) state(ws ids.WorkspaceID) *wsState {
	q.mu.Lock()
	defer q.mu.Unlock()
	s, ok := q.states[ws]
	if !ok {
		s = &wsState{}
		q.states[ws] = s
	}
	return s
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
	log := q.deps.Log.WorkspaceOrCentral(record.Dir)
	return log.With(dlog.Context{"workspace": string(ws)}), nil
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
	left := make(chan struct{})
	go func() {
		q.classifying.Wait()
		q.reviving.Wait()
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
