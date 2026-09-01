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
	opFinish      = "daemon.promptqueue.one_shot_finish"
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
	head            *ids.TurnID
	interrupting    bool
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
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	if deps.StripSentinels == nil {
		deps.StripSentinels = func(s string) string { return s }
	}
	q := &queue{deps: deps, states: make(map[ids.WorkspaceID]*wsState)}
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
	log, err := q.deps.Log.Workspace(record.Dir)
	if err != nil {
		q.deps.Log.Global().Error(opSubmit, "could not open the workspace log sink", dlog.Context{
			"workspace": string(ws), "dir": record.Dir, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open the log sink for %q: %w", ws, err)
	}
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
