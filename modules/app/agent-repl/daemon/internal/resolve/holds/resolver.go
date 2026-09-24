package holds

import (
	"fmt"
	"sync"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/wsm"
)

// wsState is one workspace's whole tray accumulation. It is in-memory only: a
// resolver aggregates, it never stores. WSM is the durable hold store and the
// prompt queue is what reads it back.
type wsState struct {
	// dir is the workspace directory, bound before any hold arrives.
	dir string
	// log is the workspace-bound logger, nil until the directory is bound.
	log dlog.Logger
	// held are the standing holds as the prompt queue last stated them.
	held []wsm.HeldPrompt
	// offer is the parked question, nil when none is posed.
	offer *frontendv1.HeldOffer
	// editing is the held prompt being edited, empty when none is.
	editing ids.TurnID
}

// resolver is the hold-tray resolver. One instance serves every workspace;
// each workspace owns one accumulation and one topic.
type resolver struct {
	log dlog.Surfaces

	mu     sync.Mutex
	states map[ids.WorkspaceID]*wsState
	topics map[ids.WorkspaceID]*publish.Topic[*frontendv1.DaemonHoldTray]
}

// newResolver builds the resolver.
func newResolver(log dlog.Surfaces) (*resolver, error) {
	if log == nil {
		return nil, fmt.Errorf("holds resolver needs log surfaces")
	}
	return &resolver{
		log:    log,
		states: map[ids.WorkspaceID]*wsState{},
		topics: map[ids.WorkspaceID]*publish.Topic[*frontendv1.DaemonHoldTray]{},
	}, nil
}

// Topic is the workspace's tray publication.
func (r *resolver) Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.DaemonHoldTray] {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.topicLocked(ws)
}

// topicLocked resolves the workspace's topic under the resolver's lock.
func (r *resolver) topicLocked(ws ids.WorkspaceID) *publish.Topic[*frontendv1.DaemonHoldTray] {
	t, ok := r.topics[ws]
	if !ok {
		t = &publish.Topic[*frontendv1.DaemonHoldTray]{}
		r.topics[ws] = t
	}
	return t
}

// stateLocked resolves the workspace's accumulation under the lock.
func (r *resolver) stateLocked(ws ids.WorkspaceID) *wsState {
	s, ok := r.states[ws]
	if !ok {
		s = &wsState{}
		r.states[ws] = s
	}
	return s
}

// SetWorkspaceDir binds the workspace's directory so its records reach that
// workspace's own sink.
func (r *resolver) SetWorkspaceDir(ws ids.WorkspaceID, dir string) error {
	log, err := r.log.Workspace(dir)
	if err != nil {
		return fmt.Errorf("bind holds resolver to workspace %s at %q: %w", ws, dir, err)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.stateLocked(ws)
	s.dir = dir
	s.log = log.With(dlog.Context{"workspace_id": string(ws)})
	s.log.Debug("daemon.holds.bind", "the hold tray bound a workspace to its log sink",
		dlog.Context{"workspace_dir": dir})
	view := r.render(s, s.log)
	topic := r.topicLocked(ws)
	r.mu.Unlock()
	// THE EMPTY TRAY IS A COMPLETE ANSWER, and binding is when it can first be
	// given: a subscriber that opens before any hold exists is otherwise
	// handed nothing at all, because a Topic replays only what was published.
	topic.Publish(view)
	r.mu.Lock()
	return nil
}

// logOf answers the workspace's logger. An unbound workspace is an invariant
// violation, recorded as one on the only sink that exists before a directory is
// known rather than silently dropped.
func (r *resolver) logOf(ws ids.WorkspaceID, s *wsState) dlog.Logger {
	if s.log != nil {
		return s.log
	}
	return r.log.Global().With(dlog.Context{
		"workspace_id":        string(ws),
		"invariant_violation": "hold-tray fact for a workspace with no bound directory",
		"remediation":         "call SetWorkspaceDir at registration",
	})
}

// mutate runs one accumulation change under the lock and republishes the whole
// tray. Every setter goes through it, so there is exactly one publication site.
//
// The tray has NO readiness gate: an empty tray is a complete answer, so the
// first fact a workspace takes already publishes one.
func (r *resolver) mutate(ws ids.WorkspaceID, operation, message string, ctx dlog.Context, apply func(*wsState)) {
	r.mu.Lock()
	s := r.stateLocked(ws)
	apply(s)
	log := r.logOf(ws, s)
	tray := r.render(s, log)
	topic := r.topicLocked(ws)
	r.mu.Unlock()

	if ctx == nil {
		ctx = dlog.Context{}
	}
	ctx["items"] = len(tray.GetItems())
	log.Debug(operation, message, ctx)
	topic.Publish(tray)
}

// SetHeldPrompts installs a workspace's standing holds, whole.
func (r *resolver) SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt) {
	r.mutate(ws, "daemon.holds.set_held_prompts", "the hold tray took the standing holds",
		dlog.Context{"held": len(held)}, func(s *wsState) {
			s.held = held
		})
}

// SetEditing names the held prompt being edited, or clears it with "".
func (r *resolver) SetEditing(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mutate(ws, "daemon.holds.set_editing", "the hold tray took the edit standing",
		dlog.Context{"editing_turn": string(turn)}, func(s *wsState) {
			s.editing = turn
		})
}

// SetOffer installs the parked question the tray poses, or clears it.
//
// The offer arrives ALREADY COMPOSED (MergeDequeueOffer is its one author). An
// offer whose arm is unset is a contract breach the resolver records loudly and
// still publishes: dropping it would hide a question the user is being asked.
func (r *resolver) SetOffer(ws ids.WorkspaceID, offer *frontendv1.HeldOffer) {
	posed := offer != nil
	r.mutate(ws, "daemon.holds.set_offer", "the hold tray took the parked offer",
		dlog.Context{"posed": posed}, func(s *wsState) {
			s.offer = offer
		})
	if posed && offer.GetOffer() == nil {
		r.mu.Lock()
		log := r.logOf(ws, r.stateLocked(ws))
		r.mu.Unlock()
		log.Error("daemon.holds.set_offer", "an offer arrived with no arm set and was published as given",
			dlog.Context{
				"invariant_violation": "HeldOffer.offer is unset",
				"remediation":         "raise the offer through holds.MergeDequeueOffer",
			})
	}
}

// render builds the whole tray: every held item, in
// display order.
func (r *resolver) render(s *wsState, log dlog.Logger) *frontendv1.DaemonHoldTray {
	items := make([]*frontendv1.DaemonHoldItem, 0, len(s.held)+1)
	for _, h := range orderedHolds(s.held) {
		prompt := heldPrompt(h, s.editing != "" && h.Turn == s.editing, log)
		if prompt == nil {
			continue
		}
		items = append(items, &frontendv1.DaemonHoldItem{
			Item: &frontendv1.DaemonHoldItem_Prompt{Prompt: prompt},
		})
	}
	if s.offer != nil {
		items = append(items, &frontendv1.DaemonHoldItem{
			Item: &frontendv1.DaemonHoldItem_Offer{Offer: s.offer},
		})
	}
	return &frontendv1.DaemonHoldTray{
		Items: items,
	}
}
