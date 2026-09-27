package prompthandler

import (
	"context"
	"errors"
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/wsm"
)

// This file holds the prompt handler's fakes. Nothing here reaches a database,
// a shim or the vendor: the queue is a recorder and the panels are scripted.

// theWorkspace is the workspace every subject acts on.
const theWorkspace ids.WorkspaceID = "ws-1"

// fakeDB is the slice of wsm the handler uses, in memory. It embeds wsm.DB so
// anything else panics loudly rather than answering a zero value.
type fakeDB struct {
	wsm.DB

	mu sync.Mutex
	// claimed maps an idempotency key to the claim standing on it. It is the
	// DURABLE state: a subject simulates a restart by building a second
	// handler over the same fakeDB.
	claimed map[string]*fakeClaim
	// claimErr fails the claim when set.
	claimErr error
	// acceptErr fails the acceptance stamp when set.
	acceptErr error
	// standing, when set, overrides the standing every claim answers.
	standing *wsm.ClaimStanding
	// workspaceErr fails the workspace resolution when set.
	workspaceErr error
}

// fakeClaim is one key's durable claim: the turn it binds and whether the
// queue accepted that turn's submission.
type fakeClaim struct {
	turn     ids.TurnID
	accepted bool
}

func newFakeDB() *fakeDB { return &fakeDB{claimed: map[string]*fakeClaim{}} }

// claimOn reads back the claim standing on key, zero when there is none.
func (d *fakeDB) claimOn(key string) fakeClaim {
	d.mu.Lock()
	defer d.mu.Unlock()
	if c, ok := d.claimed[key]; ok {
		return *c
	}
	return fakeClaim{}
}

func (d *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	if d.workspaceErr != nil {
		return wsm.Workspace{}, d.workspaceErr
	}
	return wsm.Workspace{ID: id, Dir: "/tmp/ws-1"}, nil
}

// ClaimIdempotencyKey models wsm's three standings: minted, accepted, and
// re-driven (rebound to the offered turn).
func (d *fakeDB) ClaimIdempotencyKey(_ context.Context, _ ids.WorkspaceID, key string, turn ids.TurnID) (wsm.IdempotencyClaim, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.claimErr != nil {
		return wsm.IdempotencyClaim{}, d.claimErr
	}
	if d.standing != nil {
		return wsm.IdempotencyClaim{Standing: *d.standing, Turn: turn}, nil
	}
	existing, ok := d.claimed[key]
	switch {
	case !ok:
		d.claimed[key] = &fakeClaim{turn: turn}
		return wsm.IdempotencyClaim{Standing: wsm.ClaimMinted, Turn: turn}, nil
	case existing.accepted:
		return wsm.IdempotencyClaim{Standing: wsm.ClaimAccepted, Turn: existing.turn}, nil
	default:
		abandoned := existing.turn
		existing.turn = turn
		return wsm.IdempotencyClaim{Standing: wsm.ClaimRedriven, Turn: turn, Abandoned: abandoned}, nil
	}
}

// AcceptIdempotencyKey stamps the claim bound to turn accepted, refusing a key
// not bound to it, as wsm does.
func (d *fakeDB) AcceptIdempotencyKey(_ context.Context, _ ids.WorkspaceID, key string, turn ids.TurnID) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.acceptErr != nil {
		return d.acceptErr
	}
	existing, ok := d.claimed[key]
	if !ok || existing.turn != turn || existing.accepted {
		return errors.New("fakeDB: no unaccepted claim binds the key to that turn")
	}
	existing.accepted = true
	return nil
}

// fakeQueue records every forward the handler makes.
type fakeQueue struct {
	promptqueue.Queue

	mu sync.Mutex

	submissions []promptqueue.Submission
	acts        []promptqueue.Act
	disposition promptqueue.Disposition
	submitErr   error
	actErr      error

	// entered, when set, receives once each time Submit is entered, before
	// it waits on gate.
	entered chan struct{}
	// gate, when set, holds Submit until it is closed: the hang a deadlocked
	// queue produces. It deliberately ignores the caller's context, as the
	// deadlock did.
	gate chan struct{}
}

func (q *fakeQueue) Submit(_ context.Context, sub promptqueue.Submission) (promptqueue.Disposition, error) {
	if q.entered != nil {
		q.entered <- struct{}{}
	}
	if q.gate != nil {
		<-q.gate
	}
	q.mu.Lock()
	defer q.mu.Unlock()
	if q.submitErr != nil {
		return promptqueue.Disposition{}, q.submitErr
	}
	q.submissions = append(q.submissions, sub)
	return q.disposition, nil
}

func (q *fakeQueue) SubmitSessionAct(_ context.Context, _ ids.WorkspaceID, act promptqueue.Act) error {
	q.mu.Lock()
	defer q.mu.Unlock()
	if q.actErr != nil {
		return q.actErr
	}
	q.acts = append(q.acts, act)
	return nil
}

func (q *fakeQueue) forwarded() []promptqueue.Submission {
	q.mu.Lock()
	defer q.mu.Unlock()
	out := make([]promptqueue.Submission, len(q.submissions))
	copy(out, q.submissions)
	return out
}

func (q *fakeQueue) sessionActs() []promptqueue.Act {
	q.mu.Lock()
	defer q.mu.Unlock()
	out := make([]promptqueue.Act, len(q.acts))
	copy(out, q.acts)
	return out
}

// fakeFeed records the non-durable command rows the handler mirrors.
type fakeFeed struct {
	feed.Resolver

	mu       sync.Mutex
	panels   []*frontendv1.FeedCommandPanel
	refusals []refusal
}

// refusal is one mirrored refusal card.
type refusal struct {
	Command    string
	Reason     string
	AddSupport bool
}

func (f *fakeFeed) UpsertCommandPanel(_ ids.WorkspaceID, panel *frontendv1.FeedCommandPanel) *frontendv1.FeedId {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.panels = append(f.panels, panel)
	return &frontendv1.FeedId{}
}

func (f *fakeFeed) UpsertCommandRefused(_ ids.WorkspaceID, command, reason string, addSupport bool) *frontendv1.FeedId {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.refusals = append(f.refusals, refusal{Command: command, Reason: reason, AddSupport: addSupport})
	return &frontendv1.FeedId{}
}

func (f *fakeFeed) mirroredPanels() []*frontendv1.FeedCommandPanel {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]*frontendv1.FeedCommandPanel, len(f.panels))
	copy(out, f.panels)
	return out
}

func (f *fakeFeed) mirroredRefusals() []refusal {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]refusal, len(f.refusals))
	copy(out, f.refusals)
	return out
}

// harness is one wired handler and every fake behind it.
type harness struct {
	h     *handler
	db    *fakeDB
	queue *fakeQueue
	feed  *fakeFeed

	// asked records every panel command the source was asked for.
	askedMu  sync.Mutex
	asked    []conversationv1.SessionCommand
	panel    *agentreplv1.SubmitPromptCommandPanel
	panelErr error

	// minted is the turn the handler mints, so a subject can assert on it.
	minted ids.TurnID
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{
		db:     newFakeDB(),
		queue:  &fakeQueue{},
		feed:   &fakeFeed{},
		minted: "minted-turn",
		panel: &agentreplv1.SubmitPromptCommandPanel{
			Panel: &agentreplv1.SubmitPromptCommandPanel_Status{Status: &frontendv1.StatusPanelView{}},
		},
	}
	built, err := newHandler(Deps{
		DB:    h.db,
		Queue: h.queue,
		Feed:  h.feed,
		Panels: func(_ context.Context, _ ids.WorkspaceID, command conversationv1.SessionCommand) (*agentreplv1.SubmitPromptCommandPanel, error) {
			h.askedMu.Lock()
			h.asked = append(h.asked, command)
			h.askedMu.Unlock()
			return h.panel, h.panelErr
		},
		MintTurn: func() ids.TurnID { return h.minted },
		Log:      dlog.NewTestSurfaces(),
	})
	if err != nil {
		t.Fatalf("newHandler: %v", err)
	}
	h.h = built
	return h
}

// respawn replaces the handler with a fresh one over the SAME durable state and
// queue, as a daemon killed mid-submit and started again would be: nothing of
// the old process's in-memory state survives, the claims do.
func (h *harness) respawn(t *testing.T) {
	t.Helper()
	built, err := newHandler(h.h.deps)
	if err != nil {
		t.Fatalf("newHandler: %v", err)
	}
	h.h = built
}

// panelsAsked reads back the panel commands the source was asked for.
func (h *harness) panelsAsked() []conversationv1.SessionCommand {
	h.askedMu.Lock()
	defer h.askedMu.Unlock()
	out := make([]conversationv1.SessionCommand, len(h.asked))
	copy(out, h.asked)
	return out
}

// submit runs one ordinary user submission of text.
func (h *harness) submit(text string) (Outcome, error) {
	return h.h.Submit(context.Background(), theWorkspace, userSaid(text), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil)
}

// userSaid composes a one-block text submission.
func userSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// bubbleRef addresses a subagent bubble's composer in a workspace.
func bubbleRef(ws ids.WorkspaceID) *feedid.Ref {
	return &feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub-agent"}},
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1", Sub: "sub-agent"},
	}
}

// errPanel is the panel-source failure a subject injects.
var errPanel = errors.New("the status panel could not be assembled")
