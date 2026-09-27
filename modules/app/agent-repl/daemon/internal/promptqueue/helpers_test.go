package promptqueue

import (
	"context"
	"errors"
	"io/fs"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

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

// This file holds the prompt queue's fakes. No test spawns a process, calls
// git, or reaches the vendor: the judge is a scripted verdict and the shim is a
// recording fake.

// instant is the fixed instant every stamp is taken from; no test reads the
// wall clock.
var instant = time.Date(2026, 9, 1, 9, 0, 0, 0, time.UTC)

// theWorkspace is the workspace every subject acts on.
const theWorkspace ids.WorkspaceID = "ws-1"

// fakeDB is the durable state, in memory. It embeds wsm.DB so the fake declares
// only what the queue actually calls: anything else panics loudly rather than
// quietly answering a zero value.
type fakeDB struct {
	wsm.DB

	mu sync.Mutex

	// engagements counts the delivery-time engagement stamps.
	engagements int

	workspaces map[ids.WorkspaceID]wsm.Workspace
	leases     map[ids.WorkspaceID]wsm.Lease
	held       map[ids.TurnID]*wsm.HeldPrompt
	order      []ids.TurnID
	turns      map[ids.TurnID]*wsm.Turn
	schedule   *wsm.DrainSchedule

	// closedTurns records every CloseTurn, and orphaned every CloseOrphans.
	closedTurns map[ids.TurnID]wsm.TurnClose
	orphaned    []ids.WorkspaceID

	// putHeldErr, tombstoneErr and acceptErr fail their write when set.
	putHeldErr   error
	tombstoneErr error
	acceptErr    error
	// allHeldErr fails the boot restore's all-or-nothing read.
	allHeldErr error
	// openTurnsErr fails the open-turns read the judge compares against.
	openTurnsErr error
	// closeTurnErrs fails one turn's close each.
	closeTurnErrs map[ids.TurnID]error
	// orphansErr fails CloseOrphans, and claimErr ClaimDisplacedTurn.
	orphansErr error
	claimErr   error
	// byTurnErr fails the one-hold read an edit resolves its prompt through,
	// and replaceErr fails an edit's content replacement.
	byTurnErr  error
	replaceErr error
	// workspaceErr fails the workspace read when set.
	workspaceErr error
}

func newFakeDB() *fakeDB {
	return &fakeDB{
		workspaces:  map[ids.WorkspaceID]wsm.Workspace{theWorkspace: {ID: theWorkspace, Dir: "/tmp/ws-1"}},
		leases:      map[ids.WorkspaceID]wsm.Lease{},
		held:        map[ids.TurnID]*wsm.HeldPrompt{},
		turns:       map[ids.TurnID]*wsm.Turn{},
		closedTurns: map[ids.TurnID]wsm.TurnClose{},
	}
}

// TouchEngagement records the engagement stamp every delivery writes.
func (d *fakeDB) TouchEngagement(_ context.Context, _ ids.WorkspaceID, _ time.Time) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.engagements++
	return nil
}

func (d *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.workspaceErr != nil {
		return wsm.Workspace{}, d.workspaceErr
	}
	record, ok := d.workspaces[id]
	if !ok {
		return wsm.Workspace{}, errors.New("no such workspace")
	}
	return record, nil
}

func (d *fakeDB) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	out := make([]wsm.Workspace, 0, len(d.workspaces))
	for _, w := range d.workspaces {
		out = append(out, w)
	}
	return out, nil
}

func (d *fakeDB) Lease(_ context.Context, id ids.WorkspaceID) (wsm.Lease, bool, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	lease, ok := d.leases[id]
	return lease, ok, nil
}

func (d *fakeDB) DrainSchedule(context.Context) (*wsm.DrainSchedule, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.schedule, nil
}

func (d *fakeDB) PutHeldPrompt(_ context.Context, h wsm.HeldPrompt) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.putHeldErr != nil {
		return d.putHeldErr
	}
	copied := h
	d.held[h.Turn] = &copied
	d.order = append(d.order, h.Turn)
	return nil
}

func (d *fakeDB) UpdateHeldPromptClassification(_ context.Context, turn ids.TurnID, c wsm.Classification) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	h, ok := d.held[turn]
	if !ok {
		return errors.New("no such hold")
	}
	copied := c
	h.Classification = &copied
	return nil
}

func (d *fakeDB) UpdateHeldPromptHold(_ context.Context, turn ids.TurnID, kind *wsm.HoldKind, scheduleID string) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	h, ok := d.held[turn]
	if !ok {
		return errors.New("no such hold")
	}
	h.Hold, h.ScheduleID = kind, scheduleID
	return nil
}

func (d *fakeDB) SetHeldPromptAccepted(_ context.Context, turn ids.TurnID) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.acceptErr != nil {
		return d.acceptErr
	}
	h, ok := d.held[turn]
	if !ok {
		return errors.New("no such hold")
	}
	h.Accepted = true
	return nil
}

func (d *fakeDB) TombstoneHeldPrompt(_ context.Context, turn ids.TurnID, why wsm.Tombstone) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.tombstoneErr != nil {
		return d.tombstoneErr
	}
	h, ok := d.held[turn]
	if !ok {
		return errors.New("no such hold")
	}
	copied := why
	h.Tombstone = &copied
	return nil
}

// HeldPromptByTurn answers one hold, retired or not.
func (d *fakeDB) HeldPromptByTurn(_ context.Context, turn ids.TurnID) (wsm.HeldPrompt, bool, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.byTurnErr != nil {
		return wsm.HeldPrompt{}, false, d.byTurnErr
	}
	h, ok := d.held[turn]
	if !ok {
		return wsm.HeldPrompt{}, false, nil
	}
	return *h, true, nil
}

// ReplaceHeldPromptSaid replaces a standing hold's content and discards its
// verdict, as the store does.
func (d *fakeDB) ReplaceHeldPromptSaid(_ context.Context, turn ids.TurnID, said *conversationv1.UserSaid) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.replaceErr != nil {
		return d.replaceErr
	}
	h, ok := d.held[turn]
	if !ok || h.Tombstone != nil {
		return errors.New("no such standing hold")
	}
	h.Said = said
	h.Classification = nil
	h.Accepted = false
	return nil
}

// retired answers one turn's tombstone, which HeldPrompts hides.
func (d *fakeDB) retired(turn ids.TurnID) *wsm.Tombstone {
	d.mu.Lock()
	defer d.mu.Unlock()
	if h, ok := d.held[turn]; ok {
		return h.Tombstone
	}
	return nil
}

func (d *fakeDB) HeldPrompts(_ context.Context, id ids.WorkspaceID) ([]wsm.HeldPrompt, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	out := []wsm.HeldPrompt{}
	for _, turn := range d.order {
		h := d.held[turn]
		if h != nil && h.Workspace == id && h.Tombstone == nil {
			out = append(out, *h)
		}
	}
	return out, nil
}

func (d *fakeDB) AllHeldPrompts(context.Context) ([]wsm.HeldPrompt, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.allHeldErr != nil {
		return nil, d.allHeldErr
	}
	out := []wsm.HeldPrompt{}
	for _, turn := range d.order {
		if h := d.held[turn]; h != nil && h.Tombstone == nil {
			out = append(out, *h)
		}
	}
	return out, nil
}

func (d *fakeDB) PutTurn(_ context.Context, t wsm.Turn) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	copied := t
	d.turns[t.ID] = &copied
	return nil
}

func (d *fakeDB) CloseTurn(_ context.Context, turn ids.TurnID, _ time.Time, how wsm.TurnClose) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if err := d.closeTurnErrs[turn]; err != nil {
		return err
	}
	d.closedTurns[turn] = how
	if t, ok := d.turns[turn]; ok {
		t.Close = &how
	}
	return nil
}

func (d *fakeDB) OpenTurns(_ context.Context, id ids.WorkspaceID) ([]wsm.Turn, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.openTurnsErr != nil {
		return nil, d.openTurnsErr
	}
	out := []wsm.Turn{}
	for _, t := range d.turns {
		if t.Workspace == id && t.Close == nil {
			out = append(out, *t)
		}
	}
	return out, nil
}

func (d *fakeDB) CloseOrphans(_ context.Context, id ids.WorkspaceID, at time.Time) (wsm.OrphanReport, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.orphansErr != nil {
		return wsm.OrphanReport{}, d.orphansErr
	}
	d.orphaned = append(d.orphaned, id)
	report := wsm.OrphanReport{At: at}
	for _, t := range d.turns {
		if t.Workspace == id && t.Close == nil {
			how := wsm.CloseOrphaned
			t.Close = &how
			report.Turns = append(report.Turns, t.ID)
		}
	}
	return report, nil
}

// ClaimDisplacedTurn takes a marked turn, closing it as orphaned when it is
// still open, as the store's one transaction does.
func (d *fakeDB) ClaimDisplacedTurn(_ context.Context, turn ids.TurnID, _ time.Time) (wsm.DisplacedClaim, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.claimErr != nil {
		return wsm.DisplacedClaim{}, d.claimErr
	}
	t, ok := d.turns[turn]
	if !ok || !t.Displaced {
		return wsm.DisplacedClaim{}, nil
	}
	t.Displaced = false
	claim := wsm.DisplacedClaim{Claimed: true, Closed: t.Close == nil}
	if claim.Closed {
		how := wsm.CloseOrphaned
		t.Close = &how
	}
	return claim, nil
}

// hold reads back one recorded hold.
func (d *fakeDB) hold(turn ids.TurnID) wsm.HeldPrompt {
	d.mu.Lock()
	defer d.mu.Unlock()
	if h, ok := d.held[turn]; ok {
		return *h
	}
	return wsm.HeldPrompt{}
}

// closedTurn reads back how a turn was closed, if it was.
func (d *fakeDB) closedTurn(turn ids.TurnID) (wsm.TurnClose, bool) {
	d.mu.Lock()
	defer d.mu.Unlock()
	how, ok := d.closedTurns[turn]
	return how, ok
}

// startedTurn reads back one recorded turn.
func (d *fakeDB) startedTurn(turn ids.TurnID) (wsm.Turn, bool) {
	d.mu.Lock()
	defer d.mu.Unlock()
	t, ok := d.turns[turn]
	if !ok {
		return wsm.Turn{}, false
	}
	return *t, true
}

// fakeSender records every shim call the queue makes.
type fakeSender struct {
	mu sync.Mutex

	turns   []ids.TurnID
	said    []*conversationv1.UserSaid
	origins []conversationv1.PromptOrigin
	agents  []*conversationv1.AgentId
	kills   []ids.TurnID
	// killForces is each recorded kill's force flag, index-aligned with
	// kills, so a test can assert an interrupt never forced one.
	killForces []bool
	// killCommands is each recorded kill's commanded_by, index-aligned with
	// kills, so a test can assert HOW the stop was stated.
	killCommands []*conversationv1.AgentInterruptedByUser
	models       []string
	modes        []string
	startErr     error
	killErr      error
	promptErr    error
	setModelErr  error
	mainAgent    string
	// startHook runs inside StartTurn, so a test can observe what the queue
	// holds while a delivery is in flight.
	startHook func()
	// attempts counts every StartTurn call, so a test can assert the queue
	// delivered a prompt exactly once.
	attempts int
}

func newFakeSender() *fakeSender { return &fakeSender{mainAgent: "main-agent"} }

func (s *fakeSender) StartTurn(_ context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.attempts++
	if s.startHook != nil {
		s.startHook()
	}
	if s.startErr != nil {
		return nil, s.startErr
	}
	s.turns = append(s.turns, turn)
	s.said = append(s.said, said)
	s.origins = append(s.origins, origin)
	return &shimv1.StartTurnSuccess{
		Prompt: &conversationv1.AgentPrompt{
			Id:    &conversationv1.TurnId{Value: string(turn)},
			Agent: &conversationv1.AgentId{Value: s.mainAgent},
			Said:  said,
		},
		Page: &conversationv1.HistoryPage{},
	}, nil
}

func (s *fakeSender) PromptAgent(_ context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.promptErr != nil {
		return s.promptErr
	}
	s.agents = append(s.agents, agent)
	s.said = append(s.said, said)
	return nil
}

func (s *fakeSender) KillTurn(_ context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.killErr != nil {
		return s.killErr
	}
	s.kills = append(s.kills, turn)
	s.killForces = append(s.killForces, force)
	s.killCommands = append(s.killCommands, commandedBy)
	return nil
}

func (s *fakeSender) killedForces() []bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]bool, len(s.killForces))
	copy(out, s.killForces)
	return out
}

func (s *fakeSender) killedCommands() []*conversationv1.AgentInterruptedByUser {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]*conversationv1.AgentInterruptedByUser, len(s.killCommands))
	copy(out, s.killCommands)
	return out
}

func (s *fakeSender) SetModel(_ context.Context, model string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.setModelErr != nil {
		return s.setModelErr
	}
	s.models = append(s.models, model)
	return nil
}

func (s *fakeSender) SetPermissionMode(_ context.Context, mode string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.modes = append(s.modes, mode)
	return nil
}

func (s *fakeSender) started() []ids.TurnID {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]ids.TurnID, len(s.turns))
	copy(out, s.turns)
	return out
}

func (s *fakeSender) modelsSet() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]string, len(s.models))
	copy(out, s.models)
	return out
}

func (s *fakeSender) startAttempts() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.attempts
}

func (s *fakeSender) killed() []ids.TurnID {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]ids.TurnID, len(s.kills))
	copy(out, s.kills)
	return out
}

// fakeWatcher answers the in-flight turn and records the handover.
type fakeWatcher struct {
	mu sync.Mutex

	inFlight   *ids.TurnID
	live       sessionwatcher.LiveWorkSet
	mainAgents []string
	opened     []*conversationv1.AgentPrompt
	opening    []ids.TurnID
	openFailed []ids.TurnID
	// departure is the watched shim's departure, nil while it runs.
	departure *sessionwatcher.Departure
}

func (w *fakeWatcher) Departed() (sessionwatcher.Departure, bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.departure == nil {
		return sessionwatcher.Departure{}, false
	}
	return *w.departure, true
}

// depart records the watched shim as gone, as the real watcher does on its
// dead link or its close.
func (w *fakeWatcher) depart(d sessionwatcher.Departure) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.departure = &d
}

func (w *fakeWatcher) TurnInFlight() *ids.TurnID {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.inFlight
}

func (w *fakeWatcher) LiveWork() sessionwatcher.LiveWorkSet {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.live
}

// detached sets the live detached work the watcher reports.
func (w *fakeWatcher) detached(live sessionwatcher.LiveWorkSet) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.live = live
}

func (w *fakeWatcher) SetMainAgent(agent *conversationv1.AgentId) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.mainAgents = append(w.mainAgents, agent.GetValue())
}

func (w *fakeWatcher) OnTurnOpening(_ ids.WorkspaceID, turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.opening = append(w.opening, turn)
}

func (w *fakeWatcher) OnTurnOpenFailed(_ ids.WorkspaceID, turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.openFailed = append(w.openFailed, turn)
}

func (w *fakeWatcher) OnTurnOpened(_ ids.WorkspaceID, prompt *conversationv1.AgentPrompt, _ *conversationv1.HistoryPage) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.opened = append(w.opened, prompt)
}

func (w *fakeWatcher) running(turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.inFlight = &turn
}

func (w *fakeWatcher) idle() {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.inFlight = nil
}

func (w *fakeWatcher) handovers() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return len(w.opened)
}

// fakeFeed records the synthesized rows.
type fakeFeed struct {
	feed.Resolver
	mu              sync.Mutex
	rows            []*frontendv1.FeedRow
	address         *sessionwatcher.OutputAddress
	clearReceived   []ids.TurnID
	compactReceived []ids.TurnID
	cutAborted      []ids.TurnID
	// closed are the door's feed tells, in order.
	closed []closedTell
}

// closedTell is one OnTurnClosed the door made.
type closedTell struct {
	turn ids.TurnID
	how  wsm.TurnClose
}

// OnTurnClosed records the door telling the feed a turn closed.
func (f *fakeFeed) OnTurnClosed(_ ids.WorkspaceID, turn ids.TurnID, close wsm.RecordedClose) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.closed = append(f.closed, closedTell{turn: turn, how: close.How})
}

// closedTells answers the door's feed tells, in order.
func (f *fakeFeed) closedTells() []closedTell {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]closedTell, len(f.closed))
	copy(out, f.closed)
	return out
}

// OnClearReceived records the turns a /clear drew its optimistic divider for.
func (f *fakeFeed) OnClearReceived(_ ids.WorkspaceID, turn ids.TurnID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.clearReceived = append(f.clearReceived, turn)
}

// OnCompactReceived records the turns registered as a /compact directive.
func (f *fakeFeed) OnCompactReceived(_ ids.WorkspaceID, turn ids.TurnID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.compactReceived = append(f.compactReceived, turn)
}

// OnContextCutAborted records the turns whose optimistic divider was retired.
func (f *fakeFeed) OnContextCutAborted(_ ids.WorkspaceID, turn ids.TurnID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.cutAborted = append(f.cutAborted, turn)
}

// clearReceivedTurns answers the turns OnClearReceived was called for.
func (f *fakeFeed) clearReceivedTurns() []ids.TurnID {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]ids.TurnID, len(f.clearReceived))
	copy(out, f.clearReceived)
	return out
}

// compactReceivedTurns answers the turns OnCompactReceived was called for.
func (f *fakeFeed) compactReceivedTurns() []ids.TurnID {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]ids.TurnID, len(f.compactReceived))
	copy(out, f.compactReceived)
	return out
}

// cutAbortedTurns answers the turns OnContextCutAborted was called for.
func (f *fakeFeed) cutAbortedTurns() []ids.TurnID {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]ids.TurnID, len(f.cutAborted))
	copy(out, f.cutAborted)
	return out
}

func (f *fakeFeed) UpsertSynthesized(_ ids.WorkspaceID, _ feedid.Feed, row *frontendv1.FeedRow) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rows = append(f.rows, row)
}

// UpsertAtOutputAddress records the row the way the resolver does: the id is
// composed from the recorded address (root when none stands) and the row key.
func (f *fakeFeed) UpsertAtOutputAddress(ws ids.WorkspaceID, key feedid.RowKey, row *frontendv1.FeedRow) {
	f.mu.Lock()
	defer f.mu.Unlock()
	feedAt := feedid.Feed{Root: true}
	if f.address != nil {
		feedAt = f.address.Feed
	}
	row.Id = feedid.Encode(feedid.Ref{WS: ws, Feed: feedAt, Row: key})
	f.rows = append(f.rows, row)
}

// SetOutputAddress records the standing output address the mirror lands at.
func (f *fakeFeed) SetOutputAddress(_ ids.WorkspaceID, addr *sessionwatcher.OutputAddress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.address = addr
}

func (f *fakeFeed) mirrored() []*frontendv1.FeedRow {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]*frontendv1.FeedRow, len(f.rows))
	copy(out, f.rows)
	return out
}

// fakeFooter records the waiting-interrupting status.
// fakeSidebar records the roster's own turn facts.
type fakeSidebar struct {
	sidebar.Resolver
	mu    sync.Mutex
	turns []*footer.TurnStarted
	ends  []sessionwatcher.TurnClose
}

func (f *fakeSidebar) SetTurn(_ ids.WorkspaceID, turn *footer.TurnStarted) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.turns = append(f.turns, turn)
}

func (f *fakeSidebar) AckTurn(_ ids.WorkspaceID) {}

func (f *fakeSidebar) SetTurnEnded(_ ids.WorkspaceID, how sessionwatcher.TurnClose) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ends = append(f.ends, how)
}

// rosterTurns answers the recorded roster turn facts.
func (f *fakeSidebar) rosterTurns() []*footer.TurnStarted {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*footer.TurnStarted(nil), f.turns...)
}

// rosterEnds answers the recorded turn-end facts.
func (f *fakeSidebar) rosterEnds() []sessionwatcher.TurnClose {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]sessionwatcher.TurnClose(nil), f.ends...)
}

type fakeFooter struct {
	footer.Resolver
	mu           sync.Mutex
	interrupting []bool
	turns        []*footer.TurnStarted
	dropped      []uint32
}

// SetTurn records what the queue told the footer a turn carries.
func (f *fakeFooter) SetTurn(_ ids.WorkspaceID, turn *footer.TurnStarted) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.turns = append(f.turns, turn)
}

// startedTurns answers the recorded turn facts.
func (f *fakeFooter) startedTurns() []*footer.TurnStarted {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*footer.TurnStarted(nil), f.turns...)
}

func (f *fakeFooter) SetInterrupting(_ ids.WorkspaceID, on bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.interrupting = append(f.interrupting, on)
}

// AddDroppedPrompts records what the queue told the footer a failed bring-up
// cost in held prompts.
func (f *fakeFooter) AddDroppedPrompts(_ ids.WorkspaceID, n uint32) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.dropped = append(f.dropped, n)
}

// droppedPrompts answers the recorded drop counts.
func (f *fakeFooter) droppedPrompts() []uint32 {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]uint32(nil), f.dropped...)
}

func (f *fakeFooter) interruptions() []bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]bool, len(f.interrupting))
	copy(out, f.interrupting)
	return out
}

// fakeHolds records the tray pushes.
type fakeHolds struct {
	holds.Resolver
	mu     sync.Mutex
	pushes [][]wsm.HeldPrompt
	// editing records every SetEditing, in order.
	editing []ids.TurnID
}

func (h *fakeHolds) SetEditing(_ ids.WorkspaceID, turn ids.TurnID) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.editing = append(h.editing, turn)
}

// editingMarks answers every recorded SetEditing.
func (h *fakeHolds) editingMarks() []ids.TurnID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]ids.TurnID(nil), h.editing...)
}

func (h *fakeHolds) SetHeldPrompts(_ ids.WorkspaceID, held []wsm.HeldPrompt) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.pushes = append(h.pushes, held)
}

func (h *fakeHolds) last() []wsm.HeldPrompt {
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.pushes) == 0 {
		return nil
	}
	return h.pushes[len(h.pushes)-1]
}

func (h *fakeHolds) pushCount() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.pushes)
}

// scriptedJudge answers with the verdict the test set, or its error.
type scriptedJudge struct {
	verdict classifier.Verdict
	err     error
	mu      sync.Mutex
	asked   [][2]string
	// gate, when set, blocks every Judge call until it is closed.
	gate chan struct{}
	// entered, when set, is closed by the first Judge call to reach the gate,
	// so a test can act while a verdict is known to be IN the model's hands.
	entered     chan struct{}
	enteredOnce sync.Once
}

func (j *scriptedJudge) Judge(_ context.Context, running, incoming string) (classifier.Verdict, error) {
	j.mu.Lock()
	j.asked = append(j.asked, [2]string{running, incoming})
	gate, entered := j.gate, j.entered
	j.mu.Unlock()
	if entered != nil {
		j.enteredOnce.Do(func() { close(entered) })
	}
	// THE GATE IS A RENDEZVOUS, NOT A SLEEP: a test that needs a verdict still
	// IN FLIGHT closes it when it is done, and every other test leaves it nil.
	if gate != nil {
		<-gate
	}
	return j.verdict, j.err
}

// hold makes every later Judge call block until the returned release is
// called, so a test can observe the queue with a classification in flight.
func (j *scriptedJudge) hold() func() {
	gate := make(chan struct{})
	j.mu.Lock()
	j.gate = gate
	j.entered = make(chan struct{})
	j.mu.Unlock()
	var once sync.Once
	return func() { once.Do(func() { close(gate) }) }
}

// asking answers a channel closed once a Judge call has reached the gate hold
// installed.
func (j *scriptedJudge) asking() <-chan struct{} {
	j.mu.Lock()
	defer j.mu.Unlock()
	return j.entered
}

func (j *scriptedJudge) questions() [][2]string {
	j.mu.Lock()
	defer j.mu.Unlock()
	out := make([][2]string, len(j.asked))
	copy(out, j.asked)
	return out
}

// noteRecorder records the drain refusals reported to the controller.
type noteRecorder struct {
	mu    sync.Mutex
	noted []ids.WorkspaceID
}

func (n *noteRecorder) NoteRefusal(ws ids.WorkspaceID) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.noted = append(n.noted, ws)
}

func (n *noteRecorder) count() int {
	n.mu.Lock()
	defer n.mu.Unlock()
	return len(n.noted)
}

// harness is one wired queue and every fake behind it.
type harness struct {
	// statErr is what reading any workspace directory answers.
	statErr error
	q       *queue
	db      *fakeDB
	sender  *fakeSender
	watcher *fakeWatcher
	feed    *fakeFeed
	footer  *fakeFooter
	sidebar *fakeSidebar
	holds   *fakeHolds
	judge   *scriptedJudge
	drain   *noteRecorder
	log     *dlog.TestSurfaces

	// parked records every parked route, parkedTurns the turn each was routed
	// under, and parkedErr fails it.
	parked      []*conversationv1.UserSaid
	parkedTurns []ids.TurnID
	parkedErr   error

	// noSession, when set, makes the client resolver report no session.
	revivals   int
	reviveErr  error
	reviveHook func()
	noSession  bool
	// coldGate is the standing gate's own account, empty when no gate stands.
	coldGate string

	// hostPublishes counts the host-view republications the queue asked for.
	hostMu        sync.Mutex
	hostPublishes int
}

// hostPublished answers how many host-view republications the queue asked for.
func (h *harness) hostPublished() int {
	h.hostMu.Lock()
	defer h.hostMu.Unlock()
	return h.hostPublishes
}

// waitRevivals joins every background revival the queue started.
func (h *harness) waitRevivals() { h.q.reviving.Wait() }

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{
		db:      newFakeDB(),
		sender:  newFakeSender(),
		watcher: &fakeWatcher{},
		feed:    &fakeFeed{},
		footer:  &fakeFooter{},
		sidebar: &fakeSidebar{},
		holds:   &fakeHolds{},
		judge:   &scriptedJudge{},
		drain:   &noteRecorder{},
		log:     dlog.NewTestSurfaces(),
	}
	q, err := newQueue(Deps{
		// The harness's image resolver is a plain naming of the path, so a
		// mirrored image block is legible in an assertion; every test whose
		// SUBJECT is resolution overrides it.
		ResolveImage: func(b *conversationv1.ImageBlock) (string, string, error) {
			return "src:" + b.GetLocation().(*conversationv1.ImageBlock_Path).Path.GetPath(), "alt", nil
		},
		DB:      h.db,
		Judge:   h.judge,
		Feed:    h.feed,
		Footer:  h.footer,
		Sidebar: h.sidebar,
		Holds:   h.holds,
		Client:  func(ids.WorkspaceID) (Sender, bool) { return h.sender, !h.noSession },
		Revive: func(context.Context, ids.WorkspaceID) error {
			h.revivals++
			if h.reviveErr != nil {
				return h.reviveErr
			}
			if h.reviveHook != nil {
				h.reviveHook()
			}
			return nil
		},
		Watcher: func(ids.WorkspaceID) (Watcher, bool) { return h.watcher, !h.noSession },
		ColdGate: func(ids.WorkspaceID) (string, bool) {
			return h.coldGate, h.coldGate != ""
		},
		ParkedRoute: func(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) error {
			h.parked = append(h.parked, said)
			h.parkedTurns = append(h.parkedTurns, turn)
			return h.parkedErr
		},
		DrainRefusals: h.drain,
		PublishHost: func(ids.WorkspaceID) {
			h.hostMu.Lock()
			defer h.hostMu.Unlock()
			h.hostPublishes++
		},
		Now: func() time.Time { return instant },
		// Every workspace directory exists unless a test says otherwise.
		Stat: func(string) (fs.FileInfo, error) { return nil, h.statErr },
		Log:  h.log,
	})
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = q
	return h
}

// newHarnessWithoutRevival is the harness with NO revival wired at all, which
// is the "a workspace with no live session simply refuses" configuration
// (Deps.Revive's own doc).
func newHarnessWithoutRevival(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	deps := h.q.deps
	deps.Revive = nil
	q, err := newQueue(deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = q
	return h
}

// beginCut records a context cut as the running turn, under the queue's lock
// exactly as runContextCut does, and tells the watcher it is in flight.
func (h *harness) beginCut(turn ids.TurnID, command conversationv1.SessionCommand) {
	h.q.mu.Lock()
	if _, ok := h.q.states[theWorkspace]; !ok {
		h.q.states[theWorkspace] = &wsState{}
	}
	h.q.states[theWorkspace].cut = &runningCut{turn: turn, command: command}
	h.q.mu.Unlock()
	h.watcher.running(turn)
}

// lease installs an occupancy lease with a policy.
func (h *harness) lease(holder wsm.LeaseHolder, policy wsm.LeasePolicy) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.leases[theWorkspace] = wsm.Lease{
		ID: "lease-1", Workspace: theWorkspace, Holder: holder, Policy: policy, AcquiredAt: instant,
	}
}

// clearLease releases the standing lease.
func (h *harness) clearLease() {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	delete(h.db.leases, theWorkspace)
}

// submission composes one ordinary user submission.
func submission(turn ids.TurnID, text string) Submission {
	return Submission{
		WS:     theWorkspace,
		Turn:   turn,
		Said:   userSaid(text),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}
}

// userSaid composes a one-block text submission.
func userSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// userSaidWithImage composes a submission of words plus one attached image,
// in the order the composer sends them.
func userSaidWithImage(text, path string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
			{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
				Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}},
				MediaType: "image/png",
			}}},
		},
	}}
}

// idsTurn spells a turn id in tests, so a subject reads as prose rather than as
// a conversion.
func idsTurn(v string) ids.TurnID { return ids.TurnID(v) }
