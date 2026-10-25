// Package chat is the daemon's chat service: each zettel chat thread's view
// (frontend.v1.ChatView), the gestures that move it, and the recording of the
// thread's answered questions in zettel.
//
// A THREAD IS A WORKSPACE (wsm.WorkspaceKindThread), so its session, turns and
// feed are the daemon's ordinary ones. This package adds what a chat has and
// an agent workspace does not: the graph of questions asked of answers, held
// by the zettel service, and the per-visit choice of which branch is shown.
//
// WHERE EACH FACT COMES FROM:
//   - the nodes: zettel (Thread, then every node this service records);
//   - the question in flight: the daemon's own prompt records (an open turn
//     carrying its wsm.ThreadQuestion, or a held prompt carrying its
//     wsm.ThreadAsk), read again at every edge that can change them;
//   - the answer in flight: the feed resolver, which folds the session's prose
//     and names the concluded answer (feed.ChatSink, which Service is);
//   - the shown path, the picks and the highlight: this service, in memory.
//     They describe what one person is looking at, and a restarted daemon
//     starts a fresh visit.
//
// THE FEED SINK IS CALLED UNDER THE FEED RESOLVER'S LOCK, so the three sink
// methods only touch this service's memory; every read of zettel or the
// database they cause runs as a job (Deps.Go).
package chat

import (
	"context"
	"errors"
	"slices"
	"sort"
	"strconv"
	"sync"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/wsm"
	"agent-repld/internal/zettelclient"
)

// NodeID is a zettel node's id.
type NodeID string

// Direction is which way a sibling switch moves.
type Direction int

// The directions.
const (
	// Previous moves to the sibling created before the shown one, wrapping.
	Previous Direction = iota
	// Next moves to the sibling created after the shown one, wrapping.
	Next
)

// The refusals the gestures answer with.
var (
	// ErrUnknownNode is a gesture naming a node this thread does not hold.
	ErrUnknownNode = errors.New("chat: the node is not in this thread")
	// ErrAwaitingFirstAnswer is a question asked while the thread's first
	// question is still being answered: there is no node to ask it of yet.
	ErrAwaitingFirstAnswer = errors.New("chat: the thread's first question is still being answered")
)

// EmptyHint is the line an empty thread draws.
const EmptyHint = "Ask your first question in the composer."

// Highlight is the words held highlighted in one answer.
type Highlight struct {
	// Node is the answer's node.
	Node NodeID
	// Quote is the highlighted words and their context.
	Quote *zettelv1.NodeQuote
}

// Turns is the daemon's prompt records, as this service reads them (wsm.DB).
type Turns interface {
	Turn(ctx context.Context, id ids.WorkspaceID, turn ids.TurnID) (wsm.Turn, error)
	OpenTurns(ctx context.Context, id ids.WorkspaceID) ([]wsm.Turn, error)
	HeldPrompts(ctx context.Context, id ids.WorkspaceID) ([]wsm.HeldPrompt, error)
}

// Deps are the service's collaborators. Every field is required but Go.
type Deps struct {
	// Zettel holds the graph.
	Zettel zettelclient.Client
	// Turns reads the threads' prompt records.
	Turns Turns
	// Log answers a thread's logger.
	Log func(ids.WorkspaceID) dlog.Logger
	// Go runs a job the feed sink caused, off the feed's lock. nil runs it on
	// a new goroutine; a test runs it inline.
	Go func(func())
	// Context bounds every job: the daemon's lifetime.
	Context context.Context
}

// Service is the chat service.
type Service struct {
	deps    Deps
	mu      sync.Mutex
	threads map[ids.WorkspaceID]*thread
}

// New builds the service.
func New(deps Deps) *Service {
	if deps.Zettel == nil || deps.Turns == nil || deps.Log == nil || deps.Context == nil {
		panic("chat: Zettel, Turns, Log and Context are required")
	}
	if deps.Go == nil {
		deps.Go = func(f func()) { go f() }
	}
	return &Service{deps: deps, threads: map[ids.WorkspaceID]*thread{}}
}

// thread is one thread's state. Every field is read and written under
// Service.mu.
type thread struct {
	id    ids.WorkspaceID
	topic *publish.Topic[*frontendv1.ChatView]
	// loaded marks that nodes holds the thread's graph; until then the thread
	// has no view and the feed's edges about it are not kept.
	loaded bool
	nodes  map[NodeID]*zettelv1.Node
	// order is every node, oldest first.
	order []NodeID
	// picks is, per node on the shown path, which child is shown below it:
	// decided once per visit and forgotten when the node leaves the path.
	picks     map[NodeID]NodeID
	highlight *Highlight
	// open and held are the thread's questions in flight, as last read.
	open []wsm.Turn
	held []wsm.HeldPrompt
	// prose is each in-flight turn's answer so far; answered is a turn whose
	// answer is known and is being recorded; ended is a turn the feed saw
	// end, which the prompt records may not yet say.
	prose    map[ids.TurnID]string
	answered map[ids.TurnID]string
	ended    map[ids.TurnID]bool
	// path is the shown path as last composed, root first.
	path []NodeID
}

// state answers a thread's state, creating it. Called under s.mu.
func (s *Service) state(id ids.WorkspaceID) *thread {
	t, ok := s.threads[id]
	if !ok {
		t = &thread{
			id: id, topic: publish.NewLatestOnly[*frontendv1.ChatView](),
			nodes: map[NodeID]*zettelv1.Node{}, picks: map[NodeID]NodeID{},
			prose: map[ids.TurnID]string{}, answered: map[ids.TurnID]string{}, ended: map[ids.TurnID]bool{},
		}
		s.threads[id] = t
	}
	return t
}

// Topic is a thread's chat view topic. It holds no value until Open loads the
// thread.
func (s *Service) Topic(id ids.WorkspaceID) *publish.Topic[*frontendv1.ChatView] {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.state(id).topic
}

// Open loads a thread (once) and publishes its view: what a watch does before
// it subscribes.
func (s *Service) Open(ctx context.Context, id ids.WorkspaceID) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	return s.refresh(ctx, id)
}

// Asked is told a question was submitted, asked from ASKED (what Ask
// answered): the highlight it quoted is consumed, and the thread's questions
// in flight are read again.
func (s *Service) Asked(ctx context.Context, id ids.WorkspaceID, asked *zettelv1.NodeParent) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	s.mu.Lock()
	t := s.state(id)
	if h := t.highlight; h != nil && asked.GetNode().GetQuote() != nil &&
		string(h.Node) == asked.GetNode().GetNode().GetId() && proto.Equal(h.Quote, asked.GetNode().GetQuote()) {
		t.highlight = nil
	}
	s.mu.Unlock()
	return s.refresh(ctx, id)
}

// ensureLoaded reads the thread's graph from zettel the first time.
func (s *Service) ensureLoaded(ctx context.Context, id ids.WorkspaceID) error {
	s.mu.Lock()
	loaded := s.state(id).loaded
	s.mu.Unlock()
	if loaded {
		return nil
	}
	nodes, err := s.deps.Zettel.Thread(ctx, id)
	if err != nil {
		s.deps.Log(id).Error("daemon.chat.load_failed", "the thread's graph could not be read from zettel", dlog.Context{
			"thread": string(id), "cause": err.Error(),
		})
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	if t.loaded {
		return nil
	}
	for _, n := range nodes {
		t.add(n)
	}
	t.loaded = true
	s.publish(t)
	s.deps.Log(id).Debug("daemon.chat.loaded", "the thread's graph was read from zettel", dlog.Context{
		"thread": string(id), "nodes": len(nodes),
	})
	return nil
}

// refresh re-reads the thread's questions in flight and publishes.
func (s *Service) refresh(ctx context.Context, id ids.WorkspaceID) error {
	open, err := s.deps.Turns.OpenTurns(ctx, id)
	if err != nil {
		s.deps.Log(id).Error("daemon.chat.refresh_failed", "the thread's open turns could not be read", dlog.Context{
			"thread": string(id), "cause": err.Error(),
		})
		return err
	}
	held, err := s.deps.Turns.HeldPrompts(ctx, id)
	if err != nil {
		s.deps.Log(id).Error("daemon.chat.refresh_failed", "the thread's held prompts could not be read", dlog.Context{
			"thread": string(id), "cause": err.Error(),
		})
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	t.open = t.open[:0]
	stillOpen := map[ids.TurnID]bool{}
	for _, turn := range open {
		if turn.Question != nil {
			t.open = append(t.open, turn)
			stillOpen[turn.ID] = true
		}
	}
	sort.SliceStable(t.open, func(i, j int) bool { return t.open[i].StartedAt.Before(t.open[j].StartedAt) })
	t.held = t.held[:0]
	for _, h := range held {
		if h.Ask != nil && h.Tombstone == nil {
			t.held = append(t.held, h)
		}
	}
	// A TURN THE RECORDS NO LONGER CALL OPEN needs no memory of its end or its
	// prose: the records now say what the feed said first.
	for turn := range t.ended {
		if !stillOpen[turn] {
			delete(t.ended, turn)
		}
	}
	for turn := range t.prose {
		if !stillOpen[turn] {
			delete(t.prose, turn)
		}
	}
	s.publish(t)
	return nil
}

// ---- The feed sink (feed.ChatSink) ----

// AnswerProse keeps a loaded thread's in-flight answer as it grows.
func (s *Service) AnswerProse(ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	s.mu.Lock()
	t, ok := s.threads[ws]
	if !ok || !t.loaded {
		s.mu.Unlock()
		return
	}
	// A HELD QUESTION WAS DELIVERED when its turn is not one the records last
	// said was open, so they are read again.
	stale := !t.isOpen(turn)
	t.prose[turn] = markdown
	s.publish(t)
	s.mu.Unlock()
	if stale {
		s.deps.Go(func() { _ = s.refresh(s.deps.Context, ws) })
	}
}

// TurnAnswered records a turn's answer in zettel when the turn was a thread
// question. It is told for every workspace and every plane, so the turn's own
// record decides: a turn with no question records nothing.
func (s *Service) TurnAnswered(ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	s.mu.Lock()
	if t, ok := s.threads[ws]; ok && t.loaded {
		t.answered[turn] = markdown
		s.publish(t)
	}
	s.mu.Unlock()
	s.deps.Go(func() { s.record(s.deps.Context, ws, turn, markdown) })
}

// TurnEnded notes a loaded thread's turn ended and re-reads its questions in
// flight.
func (s *Service) TurnEnded(ws ids.WorkspaceID, turn ids.TurnID) {
	s.mu.Lock()
	t, ok := s.threads[ws]
	if !ok || !t.loaded {
		s.mu.Unlock()
		return
	}
	t.ended[turn] = true
	s.publish(t)
	s.mu.Unlock()
	s.deps.Go(func() { _ = s.refresh(s.deps.Context, ws) })
}

// record records one answered turn as a node, when it was a thread question.
func (s *Service) record(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	log := s.deps.Log(ws)
	row, err := s.deps.Turns.Turn(ctx, ws, turn)
	if err != nil {
		log.Error("daemon.chat.record_failed", "an answered turn's record could not be read", dlog.Context{
			"thread": string(ws), "turn": string(turn), "cause": err.Error(),
		})
		s.forgetAnswer(ws, turn)
		return
	}
	if row.Question == nil {
		log.Debug("daemon.chat.not_a_question", "an answered turn asked no thread question; nothing is recorded", dlog.Context{
			"workspace": string(ws), "turn": string(turn),
		})
		s.forgetAnswer(ws, turn)
		return
	}
	node, err := s.deps.Zettel.RecordExchange(ctx, ws, turn, *row.Question, markdown)
	if err != nil {
		log.Error("daemon.chat.record_failed", "an answered thread question could not be recorded in zettel", dlog.Context{
			"thread": string(ws), "turn": string(turn), "parent": row.Question.Parent.String(), "cause": err.Error(),
		})
		s.forgetAnswer(ws, turn)
		return
	}
	log.Info("daemon.chat.recorded", "an answered thread question was recorded in zettel", dlog.Context{
		"thread": string(ws), "turn": string(turn), "node": node.GetId().GetId(),
	})
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.threads[ws]
	if !ok || !t.loaded {
		return
	}
	known := t.nodes[NodeID(node.GetId().GetId())] != nil
	t.add(node)
	// THE NEW ANSWER IS SHOWN when it was asked of the path's end, which is
	// where the question in flight was drawn.
	if parent := parentOf(node); !known && parent != "" && len(t.path) > 0 && t.path[len(t.path)-1] == parent {
		t.picks[parent] = NodeID(node.GetId().GetId())
	}
	delete(t.answered, turn)
	s.publish(t)
}

// forgetAnswer drops an answer that will not be recorded, so it is no longer
// drawn as in flight.
func (s *Service) forgetAnswer(ws ids.WorkspaceID, turn ids.TurnID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if t, ok := s.threads[ws]; ok && t.loaded {
		if _, held := t.answered[turn]; held {
			delete(t.answered, turn)
			s.publish(t)
		}
	}
}

// ---- Gestures ----

// Ask decides where the thread's next question is asked from: the
// highlighted answer when there is one, else the end of the shown path, else
// the start of the thread. The highlight stands until Asked is told the
// question was submitted.
func (s *Service) Ask(ctx context.Context, id ids.WorkspaceID) (*zettelv1.NodeParent, error) {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return nil, err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	if h := t.highlight; h != nil {
		return askedOf(t.nodes[h.Node], h.Quote), nil
	}
	if len(t.path) > 0 {
		return askedOf(t.nodes[t.path[len(t.path)-1]], nil), nil
	}
	if len(t.order) > 0 {
		// A LOADED THREAD WITH NODES ALWAYS HAS A PATH, because publish runs
		// after every change; an empty one here is a bug.
		panic("chat: a thread with nodes composed no path")
	}
	if _, pending := t.pending(); pending {
		return nil, ErrAwaitingFirstAnswer
	}
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}, nil
}

// SwitchSibling shows the sibling before or after a shown node.
func (s *Service) SwitchSibling(ctx context.Context, id ids.WorkspaceID, shown NodeID, way Direction) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	node, ok := t.nodes[shown]
	if !ok {
		return ErrUnknownNode
	}
	parent := parentOf(node)
	siblings := t.children(parent)
	if parent == "" || len(siblings) < 2 {
		return nil
	}
	at := indexOf(siblings, shown)
	step := 1
	if way == Previous {
		step = -1
	}
	t.picks[parent] = siblings[(at+step+len(siblings))%len(siblings)]
	s.publish(t)
	return nil
}

// Select holds a highlight in a thread, or clears it (nil).
func (s *Service) Select(ctx context.Context, id ids.WorkspaceID, h *Highlight) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	t := s.state(id)
	if h != nil && t.nodes[h.Node] == nil {
		return ErrUnknownNode
	}
	t.highlight = h
	s.publish(t)
	return nil
}

// Delete destroys a thread's graph in zettel and forgets every view and
// question in flight held of it (workspace.ThreadGraphs). A thread zettel
// holds nothing of deletes to nothing.
func (s *Service) Delete(ctx context.Context, id ids.WorkspaceID) error {
	log := s.deps.Log(id)
	deleted, err := s.deps.Zettel.DeleteThread(ctx, id)
	if err != nil {
		log.Error("daemon.chat.delete_failed", "the thread's graph could not be deleted in zettel; nothing of it was forgotten", dlog.Context{
			"thread": string(id), "cause": err.Error(),
		})
		return err
	}
	s.mu.Lock()
	delete(s.threads, id)
	s.mu.Unlock()
	log.Info("daemon.chat.deleted", "the thread's graph was deleted", dlog.Context{"thread": string(id), "nodes_deleted": deleted})
	return nil
}

// Search answers every thread whose questions or answers say QUERY's words,
// the best match first, as zettel answers it. A failure is the caller's to
// record, with the request it was serving.
func (s *Service) Search(ctx context.Context, query string) ([]*zettelv1.SearchThreadsMatch, error) {
	return s.deps.Zettel.SearchThreads(ctx, query)
}

// Sightings counts one sighting per named node, all or nothing.
func (s *Service) Sightings(ctx context.Context, id ids.WorkspaceID, nodes []NodeID) error {
	if err := s.ensureLoaded(ctx, id); err != nil {
		return err
	}
	s.mu.Lock()
	t := s.state(id)
	named := make([]*zettelv1.NodeId, 0, len(nodes))
	for _, n := range nodes {
		if t.nodes[n] == nil {
			s.mu.Unlock()
			return ErrUnknownNode
		}
		named = append(named, &zettelv1.NodeId{Id: string(n)})
	}
	s.mu.Unlock()
	if err := s.deps.Zettel.RecordSightings(ctx, named); err != nil {
		s.deps.Log(id).Error("daemon.chat.sightings_failed", "the thread's sightings could not be recorded in zettel", dlog.Context{
			"thread": string(id), "nodes": len(nodes), "cause": err.Error(),
		})
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	for _, n := range nodes {
		if node := t.nodes[n]; node != nil {
			if node.Sightings == nil {
				node.Sightings = &zettelv1.NodeSightings{}
			}
			node.Sightings.Count++
		}
	}
	return nil
}

// ---- The thread's graph ----

// add keeps one node, once.
func (t *thread) add(n *zettelv1.Node) {
	id := NodeID(n.GetId().GetId())
	if _, ok := t.nodes[id]; ok {
		t.nodes[id] = n
		return
	}
	t.nodes[id] = n
	t.order = append(t.order, id)
}

// children answers a node's children, oldest first ("" answers the roots).
func (t *thread) children(parent NodeID) []NodeID {
	var out []NodeID
	for _, id := range t.order {
		if parentOf(t.nodes[id]) == parent {
			out = append(out, id)
		}
	}
	return out
}

// isOpen reports whether a turn is one the records last said was open.
func (t *thread) isOpen(turn ids.TurnID) bool {
	for _, o := range t.open {
		if o.ID == turn {
			return true
		}
	}
	return false
}

// recordedTurn reports whether a turn is already a node.
func (t *thread) recordedTurn(turn ids.TurnID) bool {
	for _, n := range t.nodes {
		if ids.TurnID(n.GetTurn().GetTurnId()) == turn {
			return true
		}
	}
	return false
}

// parentOf answers the node a node was asked of, "" for a thread's root.
func parentOf(n *zettelv1.Node) NodeID {
	return NodeID(n.GetParent().GetNode().GetNode().GetId())
}

// askedOf is a question's parent: asked of node's current revision, quoting
// quote when set.
func askedOf(node *zettelv1.Node, quote *zettelv1.NodeQuote) *zettelv1.NodeParent {
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node:     node.GetId(),
		Revision: node.GetAnswer().GetNumber(),
		Quote:    quote,
	}}}
}

// indexOf is a node's place among its siblings, which always holds it.
func indexOf(list []NodeID, id NodeID) int {
	if i := slices.Index(list, id); i >= 0 {
		return i
	}
	panic("chat: a node is missing from its parent's children: " + string(id))
}

// siblingLabel is the sibling control's label.
func siblingLabel(at, of int) string {
	return strconv.Itoa(at+1) + " of " + strconv.Itoa(of)
}

// saidText is a held prompt's question text.
func saidText(h wsm.HeldPrompt) string { return holdfold.SaidText(h.Said) }
