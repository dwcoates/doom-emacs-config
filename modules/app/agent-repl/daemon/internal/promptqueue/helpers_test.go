package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/pullrefs"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
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

	// engagements counts the delivery-time engagement stamps, and
	// engagementCtxErr is the context error the last one was written under.
	engagements      int
	engagementCtxErr error

	workspaces map[ids.WorkspaceID]wsm.Workspace
	leases     map[ids.WorkspaceID]wsm.Lease
	held       map[ids.TurnID]*wsm.HeldPrompt
	// classificationErr, when set, is what every verdict write answers.
	classificationErr error
	order             []ids.TurnID
	turns             map[ids.TurnID]*wsm.Turn
	// owed is the resumptions a stand-down owes, by workspace.
	owed     map[ids.WorkspaceID]map[ids.TurnID]bool
	schedule *wsm.DrainSchedule

	// closedTurns records every CloseTurn, and orphaned every CloseOrphans.
	closedTurns map[ids.TurnID]wsm.TurnClose
	orphaned    []ids.WorkspaceID

	// putHeldErr and tombstoneErr fail their write when set.
	putHeldErr   error
	tombstoneErr error
	// allHeldErr fails the boot restore's all-or-nothing read.
	allHeldErr error
	// heldErr fails every standing-holds read.
	heldErr error
	// leaseErr fails every lease read.
	leaseErr error
	// openTurnsErr fails the open-turns read the judge compares against.
	openTurnsErr error
	// closeTurnErrs fails one turn's close each.
	closeTurnErrs map[ids.TurnID]error
	// putTurnErr fails every turn-row write, and turnErr every one-turn read.
	putTurnErr error
	turnErr    error
	// orphansErr fails CloseOrphans, and claimErr ClaimDisplacedTurn.
	orphansErr error
	claimErr   error
	// resumeClaimErr fails ClaimResumptions.
	resumeClaimErr error
	// byTurnErr fails the one-hold read an edit resolves its prompt through,
	// and replaceErr fails an edit's content replacement.
	byTurnErr  error
	replaceErr error
	// coalesceErr fails a coalescence whole; coalescences counts attempts.
	coalesceErr  error
	coalescences int
	// workspaceErr fails the workspace read when set, and workspaceErrFor
	// fails the read of one workspace only.
	workspaceErr    error
	workspaceErrFor map[ids.WorkspaceID]error
	// sessions is each workspace's recorded session, and sessionErr fails
	// every session read when set.
	sessions   map[ids.WorkspaceID]wsm.Session
	sessionErr error
	// holdErr fails every hold restamp (UpdateHeldPromptHold).
	holdErr error
	// beforeHoldWrite, when set, runs under the fake's lock as a held prompt
	// is written or restamped: the instant a merge's release can win the race.
	beforeHoldWrite func(d *fakeDB)
}

// requireMergeLease stands for wsm.bindMergeHold's refusal: a merge hold is
// written only while the workspace's merge lease stands. The caller holds the
// lock.
func (d *fakeDB) requireMergeLease(ws ids.WorkspaceID, kind *wsm.HoldKind) error {
	if kind == nil || *kind != wsm.HoldMerge {
		return nil
	}
	if lease, ok := d.leases[ws]; ok && lease.Holder == wsm.HolderMerge {
		return nil
	}
	return fmt.Errorf("a merge hold on %s: %w", ws, wsm.ErrMergeLeaseGone)
}

func newFakeDB() *fakeDB {
	return &fakeDB{
		workspaces:  map[ids.WorkspaceID]wsm.Workspace{theWorkspace: {ID: theWorkspace, Dir: "/tmp/ws-1"}},
		leases:      map[ids.WorkspaceID]wsm.Lease{},
		held:        map[ids.TurnID]*wsm.HeldPrompt{},
		turns:       map[ids.TurnID]*wsm.Turn{},
		closedTurns: map[ids.TurnID]wsm.TurnClose{},
		sessions:    map[ids.WorkspaceID]wsm.Session{},
	}
}

// TouchEngagement records the engagement stamp every delivery writes.
func (d *fakeDB) TouchEngagement(ctx context.Context, _ ids.WorkspaceID, _ time.Time) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.engagements++
	d.engagementCtxErr = ctx.Err()
	return nil
}

func (d *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.workspaceErr != nil {
		return wsm.Workspace{}, d.workspaceErr
	}
	if err := d.workspaceErrFor[id]; err != nil {
		return wsm.Workspace{}, err
	}
	record, ok := d.workspaces[id]
	if !ok {
		return wsm.Workspace{}, fmt.Errorf("wsm: workspace %s: %w", id, wsm.ErrNotFound)
	}
	return record, nil
}

// Session answers a workspace's recorded session, as wsm does.
func (d *fakeDB) Session(_ context.Context, id ids.WorkspaceID) (wsm.Session, bool, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.sessionErr != nil {
		return wsm.Session{}, false, d.sessionErr
	}
	session, ok := d.sessions[id]
	return session, ok, nil
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
	if d.leaseErr != nil {
		return wsm.Lease{}, false, d.leaseErr
	}
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
	if d.beforeHoldWrite != nil {
		d.beforeHoldWrite(d)
	}
	if err := d.requireMergeLease(h.Workspace, h.Hold); err != nil {
		return err
	}
	// AN UPSERT BY TURN, as the real store's: a re-put keeps the entry's place,
	// and a retired hold never resurrects.
	prior, exists := d.held[h.Turn]
	if exists && prior.Tombstone != nil {
		return errors.New("the hold is retired")
	}
	copied := h
	d.held[h.Turn] = &copied
	if !exists {
		d.order = append(d.order, h.Turn)
	}
	return nil
}

func (d *fakeDB) UpdateHeldPromptClassification(_ context.Context, turn ids.TurnID, c wsm.Classification) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.classificationErr != nil {
		return d.classificationErr
	}
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
	if d.holdErr != nil {
		return d.holdErr
	}
	if d.beforeHoldWrite != nil {
		d.beforeHoldWrite(d)
	}
	if err := d.requireMergeLease(h.Workspace, kind); err != nil {
		return err
	}
	h.Hold, h.ScheduleID = kind, scheduleID
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

// TombstoneHeldPrompts retires several holds all or nothing, as the store does.
func (d *fakeDB) TombstoneHeldPrompts(_ context.Context, turns []ids.TurnID, why wsm.Tombstone) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.tombstoneErr != nil {
		return d.tombstoneErr
	}
	for _, turn := range turns {
		if _, ok := d.held[turn]; !ok {
			return errors.New("no such hold")
		}
	}
	for _, turn := range turns {
		copied := why
		d.held[turn].Tombstone = &copied
	}
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
	return nil
}

// CoalesceHeldPrompts folds FROM into INTO atomically, as the store does:
// coalesceErr fails it whole, and nothing is written.
func (d *fakeDB) CoalesceHeldPrompts(_ context.Context, c wsm.Coalescence) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.coalescences++
	if d.coalesceErr != nil {
		return d.coalesceErr
	}
	into, okInto := d.held[c.Into]
	from, okFrom := d.held[c.From]
	if !okInto || !okFrom || into.Tombstone != nil || from.Tombstone != nil || c.Into == c.From {
		return errors.New("no such pair of standing holds")
	}
	into.Said = c.Said
	into.Coalesced = true
	if c.DiscardVerdict {
		into.Classification = nil
	}
	retired := c.Retired
	from.Tombstone = &retired
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
	if d.heldErr != nil {
		return nil, d.heldErr
	}
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
	if d.putTurnErr != nil {
		return d.putTurnErr
	}
	copied := t
	d.turns[t.ID] = &copied
	return nil
}

func (d *fakeDB) Turn(_ context.Context, id ids.WorkspaceID, turn ids.TurnID) (wsm.Turn, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.turnErr != nil {
		return wsm.Turn{}, d.turnErr
	}
	t, ok := d.turns[turn]
	if !ok || t.Workspace != id {
		return wsm.Turn{}, fmt.Errorf("fake turn %s of %s: %w", turn, id, wsm.ErrNotFound)
	}
	return *t, nil
}

// OweResumption records a stand-down's debt.
func (d *fakeDB) OweResumption(_ context.Context, ws ids.WorkspaceID, turn ids.TurnID, _ time.Time) error {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.owed == nil {
		d.owed = map[ids.WorkspaceID]map[ids.TurnID]bool{}
	}
	if d.owed[ws] == nil {
		d.owed[ws] = map[ids.TurnID]bool{}
	}
	d.owed[ws][turn] = true
	return nil
}

// ClaimResumptions takes every debt ws is owed.
func (d *fakeDB) ClaimResumptions(_ context.Context, ws ids.WorkspaceID) (int, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.resumeClaimErr != nil {
		return 0, d.resumeClaimErr
	}
	n := len(d.owed[ws])
	delete(d.owed, ws)
	return n, nil
}

// OwedTurns answers the debts ws is owed, unclaimed.
func (d *fakeDB) OwedTurns(_ context.Context, ws ids.WorkspaceID) ([]ids.TurnID, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	var out []ids.TurnID
	for turn := range d.owed[ws] {
		out = append(out, turn)
	}
	return out, nil
}

// owedCount answers how many resumptions ws is owed.
func (d *fakeDB) owedCount(ws ids.WorkspaceID) int {
	d.mu.Lock()
	defer d.mu.Unlock()
	return len(d.owed[ws])
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
	// callHook runs inside every OTHER call (StartInterjection,
	// JoinRunningTurn, PromptAgent, KillTurn, SetModel, SetPermissionMode),
	// named, so a test can act on the queue while that call is in flight.
	callHook func(call string)
	// attempts counts every StartTurn call, so a test can assert the queue
	// delivered a prompt exactly once.
	attempts int
	// joins is every turn sent to join the running turn, in order.
	joins []ids.TurnID
	// notes is the vendor note of every StartInterjection, in order.
	notes []string
	// joinErr, when set, refuses every JoinRunningTurn.
	joinErr error
	// sentBys is the sender every StartTurn, StartInterjection and
	// JoinRunningTurn was handed, nil for none, in call order.
	sentBys []*SentBy
	// rollBacks is every turn RollBackTurn was asked to cut, in order.
	rollBacks []ids.TurnID
	// rollBackErr, when set, is every RollBackTurn's answer.
	rollBackErr error
}

// RollBackTurn records the cut and answers rollBackErr.
func (s *fakeSender) RollBackTurn(_ context.Context, turn ids.TurnID) error {
	if hook := s.hookFor(); hook != nil {
		hook("rollback")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.rollBacks = append(s.rollBacks, turn)
	return s.rollBackErr
}

// rolledBack answers every turn RollBackTurn was asked to cut.
func (s *fakeSender) rolledBack() []ids.TurnID {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]ids.TurnID(nil), s.rollBacks...)
}

func newFakeSender() *fakeSender { return &fakeSender{mainAgent: "main-agent"} }

// handedSentBys answers the sender every turn-opening call was handed.
func (s *fakeSender) handedSentBys() []*SentBy {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]*SentBy(nil), s.sentBys...)
}

// hookFor reads the call hook under the sender's lock, for the calls that run
// their hook before taking it.
func (s *fakeSender) hookFor() func(string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.callHook
}

func (s *fakeSender) StartInterjection(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *SentBy, note string) (*shimv1.StartTurnSuccess, error) {
	if hook := s.hookFor(); hook != nil {
		hook("StartInterjection")
	}
	s.mu.Lock()
	s.notes = append(s.notes, note)
	s.mu.Unlock()
	return s.StartTurn(ctx, turn, said, origin, sentBy)
}

func (s *fakeSender) JoinRunningTurn(_ context.Context, turn ids.TurnID, said *conversationv1.UserSaid, _ conversationv1.PromptOrigin, sentBy *SentBy) (*shimv1.StartTurnSuccess, error) {
	if hook := s.hookFor(); hook != nil {
		hook("JoinRunningTurn")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.sentBys = append(s.sentBys, sentBy)
	if s.joinErr != nil {
		return nil, s.joinErr
	}
	s.joins = append(s.joins, turn)
	s.said = append(s.said, said)
	return &shimv1.StartTurnSuccess{
		Prompt: &conversationv1.AgentPrompt{
			Id:    &conversationv1.TurnId{Value: string(turn)},
			Agent: &conversationv1.AgentId{Value: s.mainAgent},
			Said:  said,
		},
		Page: &conversationv1.HistoryPage{},
	}, nil
}

func (s *fakeSender) StartTurn(_ context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *SentBy) (*shimv1.StartTurnSuccess, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.attempts++
	s.sentBys = append(s.sentBys, sentBy)
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
	if s.callHook != nil {
		s.callHook("PromptAgent")
	}
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
	if s.callHook != nil {
		s.callHook("KillTurn")
	}
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
	if s.callHook != nil {
		s.callHook("SetModel")
	}
	if s.setModelErr != nil {
		return s.setModelErr
	}
	s.models = append(s.models, model)
	return nil
}

func (s *fakeSender) SetPermissionMode(_ context.Context, mode string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.callHook != nil {
		s.callHook("SetPermissionMode")
	}
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
	// joining is every turn recorded as joining the turn in flight.
	joining []ids.TurnID
	// refuseJoins makes OnTurnJoining report false, as a vendor-started turn
	// in flight does.
	refuseJoins bool
	// departure is the watched shim's departure, nil while it runs.
	departure *sessionwatcher.Departure
	// reaped reports the shim's client reaped while its departure has not
	// been recorded yet.
	reaped bool
	// factsPending reports a pure attach whose session facts have not
	// arrived: the watcher then answers not free, whatever it holds.
	factsPending bool
	// factsIn closes when takeFacts takes the session facts up; nil until a
	// wait asks for it.
	factsIn chan struct{}
	// standOnOpening makes the fake stand an opening turn in flight, and
	// retire it on a refused open, as the real watcher does (OnTurnOpening,
	// OnTurnOpenFailed).
	standOnOpening bool
}

// SessionFactsIn answers whether the facts are in, as the real watcher does.
func (w *fakeWatcher) SessionFactsIn() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return !w.factsPending
}

// AwaitSessionFacts waits for takeFacts, or answers at once when the facts are
// already in.
func (w *fakeWatcher) AwaitSessionFacts(ctx context.Context) error {
	w.mu.Lock()
	if !w.factsPending {
		w.mu.Unlock()
		return nil
	}
	if w.factsIn == nil {
		w.factsIn = make(chan struct{})
	}
	ch := w.factsIn
	w.mu.Unlock()
	select {
	case <-ch:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// takeFacts takes the session facts up, with RUNNING as the turn they name
// in flight (nil for none), and releases every wait on them: the real
// watcher's facts-in edge, in one step.
func (w *fakeWatcher) takeFacts(running *ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.inFlight = running
	w.factsPending = false
	if w.factsIn == nil {
		w.factsIn = make(chan struct{})
	}
	close(w.factsIn)
}

// Free answers the real watcher's judgement: facts in, no turn, no live work.
func (w *fakeWatcher) Free() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return !w.factsPending && w.inFlight == nil && len(w.live.Agents)+len(w.live.Shells)+len(w.live.Monitors) == 0
}

func (w *fakeWatcher) Departed() (sessionwatcher.Departure, bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.departure == nil {
		return sessionwatcher.Departure{}, false
	}
	return *w.departure, true
}

// ShimGone answers departed, or reaped with the departure still to come.
func (w *fakeWatcher) ShimGone() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.departure != nil || w.reaped
}

// reap marks the watched shim's client reaped, its departure not yet
// recorded: the gap between the client's reap and the watcher's dead-link
// edge.
func (w *fakeWatcher) reap() {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.reaped = true
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
	if w.standOnOpening {
		w.inFlight = &turn
	}
}

func (w *fakeWatcher) OnTurnJoining(_ ids.WorkspaceID, turn ids.TurnID) bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.refuseJoins {
		return false
	}
	w.joining = append(w.joining, turn)
	return true
}

func (w *fakeWatcher) OnTurnOpenFailed(_ ids.WorkspaceID, turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.openFailed = append(w.openFailed, turn)
	if w.standOnOpening && w.inFlight != nil && *w.inFlight == turn {
		w.inFlight = nil
	}
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
// fakeRecounter records every workspace the door owed a branch recount.
type fakeRecounter struct {
	mu   sync.Mutex
	owed []ids.WorkspaceID
}

func (r *fakeRecounter) TurnClosed(ws ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.owed = append(r.owed, ws)
}

// fakeCacheClocks records every workspace the door told its cache clock
// moved, and, when probe is set, what probe answered at that moment.
type fakeCacheClocks struct {
	mu     sync.Mutex
	told   []ids.WorkspaceID
	probe  func(ids.WorkspaceID) bool
	probed []bool
}

func (c *fakeCacheClocks) TurnClosed(ws ids.WorkspaceID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.told = append(c.told, ws)
	if c.probe != nil {
		c.probed = append(c.probed, c.probe(ws))
	}
}

func (c *fakeCacheClocks) snapshot() ([]ids.WorkspaceID, []bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]ids.WorkspaceID(nil), c.told...), append([]bool(nil), c.probed...)
}

func (r *fakeRecounter) recounts() []ids.WorkspaceID {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]ids.WorkspaceID(nil), r.owed...)
}

// fakeTurnBanners records every turn end the queue raised a banner for.
type fakeTurnBanners struct {
	mu    sync.Mutex
	ended []bannerEnd
	// onEnded, when set, runs as a banner is raised, which is where a test
	// reads what the door had already done.
	onEnded func()
}

type bannerEnd struct {
	WS   ids.WorkspaceID
	Turn ids.TurnID
	How  wsm.TurnClose
}

func (b *fakeTurnBanners) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how wsm.TurnClose) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.ended = append(b.ended, bannerEnd{ws, turn, how})
	if b.onEnded != nil {
		b.onEnded()
	}
}

func (b *fakeTurnBanners) raised() []bannerEnd {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]bannerEnd(nil), b.ended...)
}

type fakeFeed struct {
	feed.Resolver
	mu sync.Mutex
	// rolledBack is every turn RollBackTurns removed, in order.
	rolledBack []ids.TurnID
	rows       []*frontendv1.FeedRow
	address    *sessionwatcher.OutputAddress
	// turnAddrs are the addresses AddressTurn handed over, by turn; nil
	// entries are turns handed over with no address.
	turnAddrs map[ids.TurnID]*sessionwatcher.OutputAddress
	// mirror is the standing output mirror; turnMirrors are the mirrors
	// MirrorTurn handed over, by turn.
	mirror      *wsm.MirrorAddress
	turnMirrors map[ids.TurnID]*wsm.MirrorAddress
	// turnSenders are the senders SenderTurn handed over, by turn; nil entries
	// are turns handed over with no sender.
	turnSenders     map[ids.TurnID]*ids.WorkspaceID
	clearReceived   []ids.TurnID
	compactReceived []ids.TurnID
	cutAborted      []ids.TurnID
	// promptsRetired are the turns whose mirrored prompt row was taken down.
	promptsRetired []ids.TurnID
	// closed are the door's feed tells, in order.
	closed []closedTell
	// pullTarget is the pull request target PullTarget answers for every
	// workspace, nil for none.
	pullTarget *pullrefs.Target
	// answeredAt is when the fixture's root feed last drew agent output, as
	// the real resolver records it (AnsweredSince); zero for never.
	answeredAt time.Time
}

// AnsweredSince answers whether the fixture drew agent output at or after
// AT, as the real resolver does.
func (f *fakeFeed) AnsweredSince(_ ids.WorkspaceID, at time.Time) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return !f.answeredAt.IsZero() && !f.answeredAt.Before(at)
}

// PullTarget answers the fixture's pull request target.
func (f *fakeFeed) PullTarget(ids.WorkspaceID) *pullrefs.Target { return f.pullTarget }

// closedTell is one OnTurnClosed the door made.
type closedTell struct {
	turn ids.TurnID
	how  wsm.TurnClose
}

// OnPromptRetired records a mirrored prompt row taken back down.
// RollBackTurns records the turns removed from the feed for good.
func (f *fakeFeed) RollBackTurns(_ ids.WorkspaceID, turns []ids.TurnID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rolledBack = append(f.rolledBack, turns...)
	return nil
}

// rolledBackTurns answers every turn RollBackTurns removed.
func (f *fakeFeed) rolledBackTurns() []ids.TurnID {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ids.TurnID(nil), f.rolledBack...)
}

func (f *fakeFeed) OnPromptRetired(_ ids.WorkspaceID, prompt *conversationv1.AgentPrompt) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.promptsRetired = append(f.promptsRetired, ids.TurnID(prompt.GetId().GetValue()))
}

// retiredPrompts answers the turns whose mirrored row was taken down.
func (f *fakeFeed) retiredPrompts() []ids.TurnID {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ids.TurnID(nil), f.promptsRetired...)
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

// UpsertAtTurnAddress records the row the way the resolver does: the id is
// composed from the address the turn was handed over with (root when none)
// and the row key.
func (f *fakeFeed) UpsertAtTurnAddress(ws ids.WorkspaceID, turn ids.TurnID, key feedid.RowKey, row *frontendv1.FeedRow) {
	f.mu.Lock()
	defer f.mu.Unlock()
	feedAt := feedid.Feed{Root: true}
	if addr := f.turnAddrs[turn]; addr != nil {
		feedAt = addr.Feed
	}
	row.Id = feedid.Encode(feedid.Ref{WS: ws, Feed: feedAt, Row: key})
	f.rows = append(f.rows, row)
}

// OutputAddress answers a copy of the standing output address, as the resolver
// does.
func (f *fakeFeed) OutputAddress(_ ids.WorkspaceID) *sessionwatcher.OutputAddress {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.address == nil {
		return nil
	}
	copied := *f.address
	return &copied
}

// AddressTurn records the address a turn was handed over with.
func (f *fakeFeed) AddressTurn(_ ids.WorkspaceID, turn ids.TurnID, addr *sessionwatcher.OutputAddress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.turnAddrs == nil {
		f.turnAddrs = map[ids.TurnID]*sessionwatcher.OutputAddress{}
	}
	f.turnAddrs[turn] = addr
}

// turnAddress answers the address a turn was handed over with, and whether
// it was handed over at all.
func (f *fakeFeed) turnAddress(turn ids.TurnID) (*sessionwatcher.OutputAddress, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	addr, ok := f.turnAddrs[turn]
	return addr, ok
}

// SetOutputMirror records the standing output mirror an async merge's own
// turns are recorded with.
func (f *fakeFeed) SetOutputMirror(_ ids.WorkspaceID, m *wsm.MirrorAddress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.mirror = m
}

// OutputMirror answers a copy of the standing output mirror.
func (f *fakeFeed) OutputMirror(_ ids.WorkspaceID) *wsm.MirrorAddress {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.mirror == nil {
		return nil
	}
	copied := *f.mirror
	return &copied
}

// MirrorTurn records the mirror a turn was handed over with.
func (f *fakeFeed) MirrorTurn(_ ids.WorkspaceID, turn ids.TurnID, m *wsm.MirrorAddress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.turnMirrors == nil {
		f.turnMirrors = map[ids.TurnID]*wsm.MirrorAddress{}
	}
	f.turnMirrors[turn] = m
}

// turnMirror answers the mirror a turn was handed over with, and whether it
// was handed over at all.
func (f *fakeFeed) turnMirror(turn ids.TurnID) (*wsm.MirrorAddress, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	m, ok := f.turnMirrors[turn]
	return m, ok
}

// SetOutputAddress records the standing output address a merge's own turns
// are recorded at.
func (f *fakeFeed) SetOutputAddress(_ ids.WorkspaceID, addr *sessionwatcher.OutputAddress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.address = addr
}

func (f *fakeFeed) SenderTurn(_ ids.WorkspaceID, turn ids.TurnID, sender *ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.turnSenders == nil {
		f.turnSenders = map[ids.TurnID]*ids.WorkspaceID{}
	}
	f.turnSenders[turn] = sender
}

// turnSender answers the sender SenderTurn handed over for TURN, and whether
// it handed one over at all.
func (f *fakeFeed) turnSender(turn ids.TurnID) (*ids.WorkspaceID, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	sender, ok := f.turnSenders[turn]
	return sender, ok
}

// PromptHeader heads an agent-sent turn's prompt by the sender SenderTurn
// handed over, named by its id, as the resolver heads it by its name.
func (f *fakeFeed) PromptHeader(_ ids.WorkspaceID, turn ids.TurnID, origin conversationv1.PromptOrigin) *frontendv1.FeedAgentPromptAddress {
	if origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT {
		return nil
	}
	sender, _ := f.turnSender(turn)
	name := "no sender"
	if sender != nil {
		name = string(*sender)
	}
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: name},
		Direction: &frontendv1.FeedAgentPromptAddress_Received{Received: &frontendv1.FeedAgentPromptAddressReceived{
			Sender: &frontendv1.FeedAgentPromptAddressReceived_Workspace{Workspace: &frontendv1.FeedAgentPromptAddressWorkspace{}},
		}},
	}
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
	submissions  []footer.Submission
	retrying     bool
	// vendorBlock is the standing mid-session vendor block the test stood
	// with standVendorBlock, empty when none stands.
	vendorBlock string
	// ends are the turn closes the footer was told, in order.
	ends []wsm.TurnClose
	// acks counts the turn acks the footer was told.
	acks int
}

// AckTurn records the shim's taking of a turn the footer was told.
func (f *fakeFooter) AckTurn(ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.acks++
}

// turnAcks answers how many turn acks the footer was told.
func (f *fakeFooter) turnAcks() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.acks
}

// SetTurnEnded records the turn close the footer was told.
func (f *fakeFooter) SetTurnEnded(_ ids.WorkspaceID, how wsm.TurnClose) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ends = append(f.ends, how)
}

// turnEnds answers the turn closes the footer was told.
func (f *fakeFooter) turnEnds() []wsm.TurnClose {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]wsm.TurnClose(nil), f.ends...)
}

// VendorBlock answers the block the test stood with standVendorBlock.
func (f *fakeFooter) VendorBlock(ids.WorkspaceID) (string, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.vendorBlock, f.vendorBlock != ""
}

// standVendorBlock stands (or, with "", lifts) a mid-session vendor block.
func (f *fakeFooter) standVendorBlock(block string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.vendorBlock = block
}

// RetryStanding answers the retry the test stood with standRetry.
func (f *fakeFooter) RetryStanding(ids.WorkspaceID) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.retrying
}

// standRetry states that the vendor is retrying the running turn's call.
func (f *fakeFooter) standRetry() {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.retrying = true
}

// OnSubmission records a move of a prompt's delivery the queue reported.
func (f *fakeFooter) OnSubmission(_ ids.WorkspaceID, sub footer.Submission) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.submissions = append(f.submissions, sub)
}

// submissionStages answers the recorded submissions.
func (f *fakeFooter) submissionStages() []footer.Submission {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]footer.Submission(nil), f.submissions...)
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

func (f *fakeFooter) interruptions() []bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]bool, len(f.interrupting))
	copy(out, f.interrupting)
	return out
}

// fakeHolds records the tray pushes.
type fakeHolds struct {
	holds.Sink
	mu     sync.Mutex
	pushes [][]wsm.HeldPrompt
	// folds records the fold every push made, in push order: the zero Fold
	// for a SetHeldPrompts push, which folded nothing.
	folds []holds.Fold
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
	h.folds = append(h.folds, holds.Fold{})
}

// FoldHeldPrompts records the push as SetHeldPrompts does, and the fold it made.
func (h *fakeHolds) FoldHeldPrompts(_ ids.WorkspaceID, held []wsm.HeldPrompt, fold holds.Fold) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.pushes = append(h.pushes, held)
	h.folds = append(h.folds, fold)
}

// pushedFolds answers every fold a push named, in order, skipping the
// pushes that folded nothing.
func (h *fakeHolds) pushedFolds() []holds.Fold {
	h.mu.Lock()
	defer h.mu.Unlock()
	var out []holds.Fold
	for _, f := range h.folds {
		if f != (holds.Fold{}) {
			out = append(out, f)
		}
	}
	return out
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
	// onJudge, when set, runs as each verdict is reached, which is where a
	// test changes the session under a verdict in flight.
	onJudge func()
}

func (j *scriptedJudge) Judge(_ context.Context, running, incoming string) (classifier.Verdict, error) {
	j.mu.Lock()
	j.asked = append(j.asked, [2]string{running, incoming})
	gate, entered, onJudge := j.gate, j.entered, j.onJudge
	j.mu.Unlock()
	if onJudge != nil {
		onJudge()
	}
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
	// standingDown is the supervisor's stand-down latch the queue reads.
	standingDown atomic.Bool
	// statErr is what reading any workspace directory answers.
	statErr error
	q       *queue
	db      *fakeDB
	sender  *fakeSender
	watcher *fakeWatcher
	feed    *fakeFeed
	footer  *fakeFooter
	sidebar *fakeSidebar
	edges   *fakeEdges
	leases  *fakeLeases
	holds   *fakeHolds
	judge   *scriptedJudge
	// elapsed advances the queue's clock past instant; zero unless a test
	// moves it (the classifier's measured latency).
	elapsed atomic.Int64
	drain   *noteRecorder
	banners *fakeTurnBanners
	commits *fakeRecounter
	clocks  *fakeCacheClocks
	log     *dlog.TestSurfaces

	// noSession, when set, makes the client resolver report no session.
	revivals   int
	reviveErr  error
	reviveHook func()
	noSession  bool
	// notStarted keeps the client and the watcher while the shim holds no
	// started session: a relaunch's installed shim, a vendor start retried.
	notStarted bool
	// sessionAbsent states the shim is KNOWN to hold no session.
	sessionAbsent bool
	// clientReaped answers no client while the watcher stays: the shim a
	// revival brought up has since died and been reaped.
	clientReaped bool
	// coldGate is the standing gate's own account, empty when no gate stands.
	coldGate string
	// noWatcher keeps the shim client while the session has no watcher: a shim
	// parked at its cold gate, or a session between its start and its watch.
	noWatcher bool

	// gateCounts records every count of prompts held at a cold gate the queue
	// reported (Deps.ColdGateHeld), in order.
	gateMu     sync.Mutex
	gateCounts []int

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
		edges:   &fakeEdges{},
		leases:  &fakeLeases{},
		holds:   &fakeHolds{},
		judge:   &scriptedJudge{},
		drain:   &noteRecorder{},
		banners: &fakeTurnBanners{},
		commits: &fakeRecounter{},
		clocks:  &fakeCacheClocks{},
		log:     dlog.NewTestSurfaces(),
	}
	q, err := newQueue(Deps{
		TurnBanners: h.banners,
		Commits:     h.commits,
		CacheClocks: h.clocks,
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
		Edges:   h.edges,
		Leases:  h.leases,
		Holds:   h.holds,
		Client:  func(ids.WorkspaceID) (Sender, bool) { return h.sender, !h.noSession && !h.clientReaped },
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
		Watcher:        func(ids.WorkspaceID) (Watcher, bool) { return h.watcher, !h.noSession && !h.noWatcher },
		SessionStarted: func(ids.WorkspaceID) bool { return !h.noSession && !h.notStarted },
		SessionAbsent:  func(ids.WorkspaceID) bool { return h.sessionAbsent },
		StandingDown:   h.standingDown.Load,
		ColdGate: func(ids.WorkspaceID) (string, bool) {
			return h.coldGate, h.coldGate != ""
		},
		ColdGateHeld: func(_ ids.WorkspaceID, held int) {
			h.gateMu.Lock()
			defer h.gateMu.Unlock()
			h.gateCounts = append(h.gateCounts, held)
		},
		DrainRefusals: h.drain,
		PublishHost: func(ids.WorkspaceID) {
			h.hostMu.Lock()
			defer h.hostMu.Unlock()
			h.hostPublishes++
		},
		Now: func() time.Time { return instant.Add(time.Duration(h.elapsed.Load())) },
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

// theSender is the workspace agentSubmission's prompts are sent by.
const theSender ids.WorkspaceID = "ws-sender"

// agentSubmission is submission's prompt as theSender's agent sent it.
func agentSubmission(turn ids.TurnID, text string) Submission {
	sub := submission(turn, text)
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
	sender := theSender
	sub.SentBy = &sender
	return sub
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

// fakeEdges records every turn edge, as "<edge>:<workspace>".
type fakeEdges struct {
	mu  sync.Mutex
	got []string
}

func (e *fakeEdges) TurnStarted(ws ids.WorkspaceID) { e.record("started:" + string(ws)) }
func (e *fakeEdges) TurnEnded(ws ids.WorkspaceID)   { e.record("ended:" + string(ws)) }
func (e *fakeEdges) TurnEndedUnobserved(ws ids.WorkspaceID) {
	e.record("ended_unobserved:" + string(ws))
}

func (e *fakeEdges) record(edge string) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.got = append(e.got, edge)
}

// edgesTold answers every turn edge told so far.
func (e *fakeEdges) edgesTold() []string {
	e.mu.Lock()
	defer e.mu.Unlock()
	return append([]string(nil), e.got...)
}

// productionFuncs parses every production source of this package
// (sourcescan.Production) and answers each function declaration with the file
// set that positions it: what a structural rule over the package's own code
// walks.
func productionFuncs(t *testing.T) (*token.FileSet, []*ast.FuncDecl) {
	t.Helper()
	fset := token.NewFileSet()
	var funcs []*ast.FuncDecl
	for _, file := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok {
				funcs = append(funcs, fn)
			}
		}
	}
	return fset, funcs
}

// fakeLeases records every workspace whose lease change the queue passed on.
type fakeLeases struct {
	mu      sync.Mutex
	changed []ids.WorkspaceID
}

func (l *fakeLeases) LeaseChanged(ws ids.WorkspaceID) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.changed = append(l.changed, ws)
}

func (l *fakeLeases) told() []ids.WorkspaceID {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]ids.WorkspaceID(nil), l.changed...)
}
