package feed

import (
	"context"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A FORK'S INHERITED PAST.
//
// A fork's vendor session resumes a COPY of its parent's transcript, and the
// file plane ingests that copy into the fork's own book WHILE THE FORK IS
// ALREADY RUNNING: the conversation's past reaches the fork's watch as live
// entries, oldest first, for as long as the ingestion takes, interleaved with
// the fork's own first turn — and on every later replay it sits in the book
// AROUND the fork's first prompt, because the book orders by first insert.
// Drawn by arrival, the past sorted below the fork's own prompt, its prompts
// became the turn in flight, its context cuts bounded the fork's own rows (the
// fork's first prompt was withheld and its responses scattered through the
// replay), and every row above each later cut was pushed and then truncated
// (ship-gns, 2026-09-27).
//
// WHAT THE FORK ITSELF PRODUCED IS KNOWN EXACTLY. Every turn the daemon opens
// is recorded against its workspace before it is delivered (wsm turns), and
// every entry a producing session writes within an open turn carries that
// turn's stamp — the file plane's copy of a turn the stream plane stamped keeps
// the stream plane's stamp. So ON A FORK, a main-agent entry stamped with a
// turn the workspace recorded is the fork's own, and every other main-agent
// entry — stamped with a turn the fork never opened, or unstamped — is the
// inherited past. A subagent's entries ride its own watch and are never the
// copied transcript, so they are never classified.
//
// THE INHERITED PAST RIDES ITS OWN PLANE (planeInherited): below everything the
// fork has of its own, whichever plane that arrived on, and in the order the
// copy arrives, which is the copied transcript's own order. The delivery bound
// needs no special case: a cut in the inherited plane withholds the inherited
// rows drawn before it, and by boundHides' plane rule it can never hide a row
// of a later plane — nothing the fork produced.
//
// IT IS DRAWN IN A STANCE OF ITS OWN (inheritedStance), the way a history page
// is: an inherited prompt never becomes the turn in flight, an inherited
// terminal never ends the fork's turn, and the fork's own positional
// attribution is untouched by it.
//
// IT IS NEVER PUSHED LIVE (withheldFromPush). A tail appends by arrival and an
// inherited row sorts above the fork's own rows: appended, it would land below
// them where nothing retracts it, and an inherited cut appended after the
// fork's own prompt would truncate that prompt off the reader's screen. The
// inherited past is served by pages, where the order is the resolver's.

// lineage is what this resolver has learned about one workspace's descent: the
// facts are durable (a workspace is a fork from its creation; a turn is or is
// not its own forever), so an answer, once read, is never read again.
type lineage struct {
	// known says the fork fact has been read.
	known bool
	// fork says the workspace carries a ported conversation, which is what a
	// fork is.
	fork bool
	// owned answers, for each turn asked about, whether the workspace recorded
	// it. A turn absent from the map has not been asked about, or its read
	// failed and will be asked again.
	owned map[ids.TurnID]bool
	// failures are read failures not yet recorded: the reads run outside the
	// resolver's mutex, and the workspace's logger is resolved under it.
	failures []lineageFailure
}

// lineageFailure is one read the lineage could not answer.
type lineageFailure struct {
	operation string
	message   string
	context   dlog.Context
}

// lineages is the resolver's lineage cache, under a mutex of its own: it is
// read and written OUTSIDE the resolver's mutex, where the database reads that
// fill it belong.
type lineages struct {
	mu sync.Mutex
	by map[ids.WorkspaceID]*lineage
}

// entry answers a workspace's lineage record, creating it on first sight.
// Called with mu held.
func (l *lineages) entry(ws ids.WorkspaceID) *lineage {
	if l.by == nil {
		l.by = map[ids.WorkspaceID]*lineage{}
	}
	got, ok := l.by[ws]
	if !ok {
		got = &lineage{owned: map[ids.TurnID]bool{}}
		l.by[ws] = got
	}
	return got
}

// learnLineage reads whatever the entries about to be drawn need to be
// classified and the cache does not hold yet: the fork fact, once, and — on a
// fork — the ownership of every turn not yet asked about. It runs BEFORE the
// resolver's mutex is taken: no read of a database belongs inside it.
//
// A FAILED READ IS RECORDED, NEVER SWALLOWED (recordLineageFailures), and it is
// not cached: the entry is drawn as the fork's own — what the feed did before
// it could tell the two apart — and the next entry asks again.
func (r *resolver) learnLineage(ws ids.WorkspaceID, turns ...ids.TurnID) {
	r.lineage.mu.Lock()
	l := r.lineage.entry(ws)
	needFork := !l.known
	r.lineage.mu.Unlock()

	if needFork {
		r.learnFork(ws)
	}

	r.lineage.mu.Lock()
	l = r.lineage.entry(ws)
	if !l.fork || r.deps.OwnedTurns == nil {
		r.lineage.mu.Unlock()
		return
	}
	var unasked []ids.TurnID
	seen := map[ids.TurnID]bool{}
	for _, turn := range turns {
		if _, asked := l.owned[turn]; turn == "" || asked || seen[turn] {
			continue
		}
		seen[turn] = true
		unasked = append(unasked, turn)
	}
	r.lineage.mu.Unlock()
	if len(unasked) == 0 {
		return
	}

	owned, err := r.deps.OwnedTurns(context.Background(), ws, unasked)
	r.lineage.mu.Lock()
	defer r.lineage.mu.Unlock()
	l = r.lineage.entry(ws)
	if err != nil {
		l.failures = append(l.failures, lineageFailure{
			operation: "daemon.feed.turn_ownership_unreadable",
			message:   "which turns a fork opened itself could not be read; their entries are drawn as the fork's own",
			context:   dlog.Context{"turns": len(unasked), "cause": err.Error()},
		})
		return
	}
	for _, turn := range unasked {
		l.owned[turn] = owned[turn]
	}
}

// learnFork reads whether a workspace is a fork: it carries a ported
// conversation exactly when it was forked from one.
func (r *resolver) learnFork(ws ids.WorkspaceID) {
	if r.deps.PortedPrompts == nil {
		r.lineage.mu.Lock()
		l := r.lineage.entry(ws)
		l.known, l.fork = true, false
		r.lineage.mu.Unlock()
		return
	}
	ported, err := r.deps.PortedPrompts(context.Background(), ws)
	r.lineage.mu.Lock()
	defer r.lineage.mu.Unlock()
	l := r.lineage.entry(ws)
	if err != nil {
		l.failures = append(l.failures, lineageFailure{
			operation: "daemon.feed.fork_unreadable",
			message:   "whether the workspace is a fork could not be read; its entries are drawn as its own",
			context:   dlog.Context{"cause": err.Error()},
		})
		return
	}
	l.known, l.fork = true, len(ported) > 0
}

// ownTurn records a turn the daemon just opened as the workspace's own, so an
// entry of it is never asked about.
func (r *resolver) ownTurn(ws ids.WorkspaceID, turn ids.TurnID) {
	r.lineage.mu.Lock()
	defer r.lineage.mu.Unlock()
	r.lineage.entry(ws).owned[turn] = true
}

// recordLineageFailures writes the lineage reads that failed since the last
// call to the workspace's own sink. Called with the resolver's mutex held.
func (r *resolver) recordLineageFailures(s *wsState) {
	r.lineage.mu.Lock()
	l := r.lineage.entry(s.id)
	failures := l.failures
	l.failures = nil
	r.lineage.mu.Unlock()
	for _, failure := range failures {
		r.logger(s.id).Error(failure.operation, failure.message, failure.context)
	}
}

// inherits reports whether an entry of AGENT stamped with TURN ("" when
// unstamped) is the fork's inherited past. Called with the resolver's mutex
// held, after learnLineage.
func (r *resolver) inherits(s *wsState, agent *conversationv1.AgentId, turn ids.TurnID) bool {
	r.recordLineageFailures(s)
	if s.mainAgent != "" && agent.GetValue() != s.mainAgent {
		return false
	}
	r.lineage.mu.Lock()
	defer r.lineage.mu.Unlock()
	l := r.lineage.entry(s.id)
	if !l.fork || r.deps.OwnedTurns == nil {
		return false
	}
	if turn == "" {
		return true
	}
	owned, asked := l.owned[turn]
	return asked && !owned
}

// inheritedStance is the attribution the inherited past is drawn under,
// carried from one inherited entry to the next the way a page's replay stance
// is carried through its page.
type inheritedStance struct {
	turnInFlight      *ids.TurnID
	turnStamp         *ids.TurnID
	replayTurn        *ids.TurnID
	replayPromptDrawn bool
}

// drawInherited draws one inherited entry in the inherited plane, under the
// inherited stance, and restores everything it displaced — the plane and the
// fork's own attribution — afterwards.
func (r *resolver) drawInherited(s *wsState, draw func()) {
	plane := s.plane
	own := inheritedStance{
		turnInFlight:      s.turnInFlight,
		turnStamp:         s.turnStamp,
		replayTurn:        s.replayTurn,
		replayPromptDrawn: s.replayPromptDrawn,
	}
	atFloor, closes := s.replayAtFloor, s.replayCloses

	s.plane = planeInherited
	s.turnInFlight = s.inherited.turnInFlight
	s.turnStamp = s.inherited.turnStamp
	s.replayTurn = s.inherited.replayTurn
	s.replayPromptDrawn = s.inherited.replayPromptDrawn
	// The inherited past reaches no floor this feed can claim, and none of its
	// turns has a recorded close in THIS workspace.
	s.replayAtFloor, s.replayCloses = false, nil

	draw()

	s.inherited = inheritedStance{
		turnInFlight:      s.turnInFlight,
		turnStamp:         s.turnStamp,
		replayTurn:        s.replayTurn,
		replayPromptDrawn: s.replayPromptDrawn,
	}
	s.plane = plane
	s.turnInFlight = own.turnInFlight
	s.turnStamp = own.turnStamp
	s.replayTurn = own.replayTurn
	s.replayPromptDrawn = own.replayPromptDrawn
	s.replayAtFloor, s.replayCloses = atFloor, closes
}

// entryTurns answers every turn a page's entries name — each entry's stamp and
// each prompt's own turn — for learnLineage.
func entryTurns(page *conversationv1.HistoryPage) []ids.TurnID {
	var out []ids.TurnID
	for _, at := range page.GetEntries() {
		if turn := at.GetTurn().GetValue(); turn != "" {
			out = append(out, ids.TurnID(turn))
		}
		if turn := at.GetEntry().GetUserPrompt().GetId().GetValue(); turn != "" {
			out = append(out, ids.TurnID(turn))
		}
	}
	return out
}

// entryClass answers the agent and turn an entry is classified by: the
// recipient and own turn of a prompt, the frame's own agent and stamp, the
// recipient and stamp of a peer message.
func entryClass(at *conversationv1.HistoryEntryAt, pageAgent *conversationv1.AgentId) (*conversationv1.AgentId, ids.TurnID) {
	entry := at.GetEntry()
	stamp := ids.TurnID(at.GetTurn().GetValue())
	switch {
	case entry.GetUserPrompt() != nil:
		prompt := entry.GetUserPrompt()
		agent := prompt.GetAgent()
		if agent.GetValue() == "" {
			agent = pageAgent
		}
		return agent, ids.TurnID(prompt.GetId().GetValue())
	case entry.GetAgentFrame() != nil:
		return entry.GetAgentFrame().GetAgentId(), stamp
	case entry.GetPeerMessage() != nil:
		return entry.GetPeerMessage().GetAgent(), stamp
	}
	return pageAgent, stamp
}
