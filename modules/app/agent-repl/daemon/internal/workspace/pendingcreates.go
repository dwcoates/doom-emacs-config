package workspace

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/netstate"
	"agent-repld/internal/wsm"
)

// A CREATE IS PENDING from the moment it is asked for until its outcome is
// known: while its attempt runs, and while it is HELD for the network (owner
// ruling, 2026-10-27). Every pending create lives in ONE set, keyed by its
// NORMALIZED REQUEST (createKey), and the set is the only thing that starts a
// create. So a request identical to a pending create cannot start a second
// one: entering the set either makes the create or joins the one standing,
// under the set's one lock. A held create is also a durable row keyed the
// same way (wsm.QueuedCreate), so a successor daemon holds the same create
// under the same key and the same pending ref.
//
// A HELD CREATE RESUMES ON A LATCH, never on a timer: the network-restored
// signal (netstate.Monitor's Restored latch) for a create held for the
// network, or its requester's next turn end (turnlatch) for a workspace that
// forked itself from inside its conversation's first turn. It resumes from the
// start of the create and completes exactly as it would have; an attempt that
// meets the same wait again is held again. A held create holds no create-gate
// ticket while it waits, so a stand-down never waits on it; each resumed
// attempt holds one. A previous daemon's held row is resumed by the next
// daemon as a create held for the network (RecoverQueuedCreates): with the
// network up that latch is already closed, so the attempt runs at once and,
// should its requester's turn still be running, is held for that turn again.

// opPendingCreate is the operation the pending-create set's records carry.
const opPendingCreate = "daemon.workspace.pending_create"

// BriefCreateDequeued is the notice a held create's requester is told its
// outcome by (prompts/workspace-create-dequeued.md).
const BriefCreateDequeued = "workspace-create-dequeued"

// PendingCreateID is a pending create's identity
// (agentrepl.v1.PendingCreateRef.id).
type PendingCreateID string

// newPendingCreateID mints a pending create's identity.
func newPendingCreateID() PendingCreateID {
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		panic(fmt.Sprintf("workspace: the system's random source failed: %v", err))
	}
	return PendingCreateID("create-" + hex.EncodeToString(raw[:]))
}

// NetworkWaitError is a create held until the network is restored. It is the
// create's ANSWER, never its failure: no create-failure report is made, and a
// caller says the create waits.
type NetworkWaitError struct {
	// ID is the held create.
	ID PendingCreateID
	// Evidence is what decided the network was down.
	Evidence netstate.Evidence
}

func (e *NetworkWaitError) Error() string {
	return fmt.Sprintf("the create %s waits for the network (%s)", e.ID, e.Evidence)
}

// IsNetworkWait reports whether ERR is a create held for the network.
func IsNetworkWait(err error) bool {
	var wait *NetworkWaitError
	return errors.As(err, &wait)
}

// RequesterTurnWaitError is a create held until its requesting workspace's
// turn ends: the requester forked its own conversation from inside that
// conversation's first turn, which has nothing finished to carry yet. Like
// NetworkWaitError it is the create's ANSWER, never its failure.
type RequesterTurnWaitError struct {
	// ID is the held create.
	ID PendingCreateID
	// Requester is the workspace whose turn the create waits for.
	Requester ids.WorkspaceID
}

func (e *RequesterTurnWaitError) Error() string {
	return fmt.Sprintf("the create %s waits for the turn of its requester %s to end", e.ID, e.Requester)
}

// IsRequesterTurnWait reports whether ERR is a create held for its
// requester's turn.
func IsRequesterTurnWait(err error) bool {
	var wait *RequesterTurnWaitError
	return errors.As(err, &wait)
}

// requesterTurnInFlight is a create whose requester forks itself from inside
// its conversation's first turn (forkableParentSession): the create's attempt
// ends with it, and the set holds the create until that turn ends.
type requesterTurnInFlight struct {
	requester ids.WorkspaceID
}

func (e *requesterTurnInFlight) Error() string {
	return fmt.Sprintf("the requester %q forks itself from inside its conversation's first turn, which has nothing finished to carry yet", e.requester)
}

// isHeldWait reports whether ERR ends an attempt with a wait the set holds the
// create for, rather than with a failure.
func isHeldWait(err error) bool {
	var network *namingNetworkError
	var turn *requesterTurnInFlight
	return errors.As(err, &network) || errors.As(err, &turn)
}

// namingNetworkError is a naming call that could not run because the network
// is down: the create's attempt ends with it, and the set holds the create.
type namingNetworkError struct {
	verdict netstate.Verdict
	// cause is the headless cause the call failed with (timeout,
	// exit_status), kept for the record.
	cause string
}

func (e *namingNetworkError) Error() string {
	return fmt.Sprintf("the naming call could not reach the network (%s: %s; the call failed with %s)", e.verdict.Evidence, e.verdict.Detail, e.cause)
}

// CreateTicket is one caller's place in a pending create: the create it made,
// or the one it joined.
type CreateTicket struct {
	entry    *pendingCreate
	spec     CreateSpec
	joined   bool
	progress CreateProgress
}

// NewCreateTicket is a ticket for SPEC naming pending create ID, JOINED or
// not: what a stand-in for the verbs answers EnterCreate with. The verbs mint
// their own tickets; nothing else runs one.
func NewCreateTicket(spec CreateSpec, id PendingCreateID, joined bool) *CreateTicket {
	progress := spec.Progress
	spec.Progress = nil
	return &CreateTicket{entry: &pendingCreate{id: id}, spec: spec, joined: joined, progress: progress}
}

// Spec is the request the ticket was entered with, its reporter included.
func (t *CreateTicket) Spec() CreateSpec {
	spec := t.spec
	spec.Progress = t.progress
	return spec
}

// Joined reports whether the request joined a create already pending.
func (t *CreateTicket) Joined() bool { return t.joined }

// ID is the pending create the ticket names; empty for a merge workspace,
// which is never pending (its name was reserved by its request).
func (t *CreateTicket) ID() PendingCreateID {
	if t.entry == nil {
		return ""
	}
	return t.entry.id
}

// pendingCreate is one create in the set.
type pendingCreate struct {
	id  PendingCreateID
	key string
	// spec is the request the create runs, with no reporter of its own.
	spec CreateSpec
	// attempt is closed when the attempt in flight ends; nil while held.
	attempt chan struct{}
	// result is the ended attempt's outcome, read once attempt is closed.
	result attemptResult
	// held is true while the create waits for what it was held for.
	held bool
	// tellHeld says to a joining caller's reporter that the create waits, as
	// the reason it was last held for words it (holdReason.tell).
	tellHeld func(r CreateProgress, id PendingCreateID)
	// heldFor is the reason it was last held for (holdReason.cause), which
	// its requester's notice names.
	heldFor string
	// persisted is true while its durable row stands.
	persisted bool
	// waiters are the reporters of the callers waiting on the attempt in
	// flight, who are answered by RunCreate's return.
	waiters []CreateProgress
	// subscribers are the reporters told the outcome once the create was
	// held: their callers were already answered that it waits.
	subscribers []CreateProgress
	// wasHeld is true once the create was ever held: its outcome is then told
	// to its requester, who was answered only that it waits.
	wasHeld bool
}

type attemptResult struct {
	created wsm.Workspace
	err     error
}

// pendingCreates is the set: the ONE owner of every pending create.
type pendingCreates struct {
	mu    sync.Mutex
	byKey map[string]*pendingCreate
}

// EnterCreate puts SPEC's request in the pending-create set: it makes the
// create, or joins the identical one already pending.
func (v *verbs) EnterCreate(spec CreateSpec) *CreateTicket {
	progress := spec.Progress
	spec.Progress = nil
	ticket := &CreateTicket{spec: spec, progress: progress}
	// A MERGE WORKSPACE IS NEVER PENDING: its request reserved its name, so
	// two of them are never identical, and it never names itself.
	if spec.MergeOf != nil {
		return ticket
	}
	key := createKey(spec)
	v.pending.mu.Lock()
	defer v.pending.mu.Unlock()
	if v.pending.byKey == nil {
		v.pending.byKey = map[string]*pendingCreate{}
	}
	entry, standing := v.pending.byKey[key]
	if !standing {
		entry = &pendingCreate{id: newPendingCreateID(), key: key, spec: spec, attempt: make(chan struct{})}
		v.pending.byKey[key] = entry
	}
	ticket.entry, ticket.joined = entry, standing
	if progress != nil {
		if entry.held {
			entry.subscribers = appendReporter(entry.subscribers, progress)
		} else {
			entry.waiters = appendReporter(entry.waiters, progress)
		}
	}
	return ticket
}

// RunCreate runs the create TICKET names to its answer: the workspace, its
// failure, or a *NetworkWaitError once it is held.
func (v *verbs) RunCreate(ctx context.Context, ticket *CreateTicket) (wsm.Workspace, error) {
	entry := ticket.entry
	if entry == nil {
		return v.attemptCreate(ctx, ticket.spec)
	}
	log := v.deps.Log.Global().With(dlog.Context{"create_id": string(entry.id), "repo_dir": entry.spec.RepoDir})
	if ticket.joined {
		v.pending.mu.Lock()
		held, attempt, standingResult, tellHeld := entry.held, entry.attempt, entry.result, entry.tellHeld
		v.pending.mu.Unlock()
		log.Info(opPendingCreate, "the request is identical to a pending create; it joined that create", dlog.Context{
			"held": held, "requester": ids.Optional(ticket.spec.Requester),
		})
		if held {
			if ticket.progress != nil {
				tellHeld(ticket.progress, entry.id)
			}
			return wsm.Workspace{}, standingResult.err
		}
		select {
		case <-attempt:
		case <-ctx.Done():
			return wsm.Workspace{}, ctx.Err()
		}
		v.pending.mu.Lock()
		result := entry.result
		v.pending.mu.Unlock()
		return result.created, result.err
	}
	spec := entry.spec
	spec.Progress = entryProgress{set: &v.pending, entry: entry}
	created, err := v.attemptCreate(ctx, spec)
	result := v.settle(ctx, log, entry, created, err)
	return result.created, result.err
}

// attemptCreate is one run of the create.
func (v *verbs) attemptCreate(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	return v.attemptHeldCreate(ctx, spec)
}

// attemptHeldCreate is one run of the create, under a create-gate ticket. A
// failure is reported to the repository's main worktree; a create held for
// the network is not a failure and is not reported.
func (v *verbs) attemptHeldCreate(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	ctx, release, err := v.deps.Creates.Hold(ctx)
	if err != nil {
		v.deps.Log.Global().Info(opCreate, "the daemon stood down before the create could start; nothing was named or built", dlog.Context{
			"repo_dir": spec.RepoDir, "cause": err.Error(),
		})
		return wsm.Workspace{}, err
	}
	defer release()
	created, err := v.create(ctx, spec)
	if err != nil && !isHeldWait(err) {
		v.reportCreateFailure(ctx, spec, err)
	}
	return created, err
}

// holdReason is what a held create waits for.
type holdReason struct {
	// cause names the wait in records: "network" or "requester_turn".
	cause string
	// fields are the wait's own diagnostic context.
	fields dlog.Context
	// latch answers the latch the create resumes on. It is taken BEFORE
	// anyone is told the create waits, so a release after that answer is
	// never missed.
	latch func(ctx context.Context, log dlog.Logger) <-chan struct{}
	// answer is the create's answer while it waits.
	answer func(id PendingCreateID) error
	// tell says to one reporter that the create waits.
	tell func(r CreateProgress, id PendingCreateID)
	// waits is the record a hold is announced with.
	waits string
}

// networkHold is the wait for the network a naming call failed on.
func (v *verbs) networkHold(network *namingNetworkError) holdReason {
	return holdReason{
		cause: "network",
		fields: dlog.Context{
			"evidence": string(network.verdict.Evidence), "detail": network.verdict.Detail, "naming_cause": network.cause,
		},
		latch: func(context.Context, dlog.Logger) <-chan struct{} { return v.deps.Network.Restored() },
		answer: func(id PendingCreateID) error {
			return &NetworkWaitError{ID: id, Evidence: network.verdict.Evidence}
		},
		tell:  func(r CreateProgress, id PendingCreateID) { r.WaitingForNetwork(id) },
		waits: "the create waits for the network; it resumes from the naming call when the network is restored",
	}
}

// requesterTurnHold is the wait for the requester's turn a self-fork met.
func (v *verbs) requesterTurnHold(turn *requesterTurnInFlight) holdReason {
	return holdReason{
		cause:  "requester_turn",
		fields: dlog.Context{"requester": string(turn.requester)},
		latch: func(ctx context.Context, log dlog.Logger) <-chan struct{} {
			// THE LATCH IS TAKEN, THEN THE TURN IS LOOKED AT AGAIN: a turn
			// that ended after the attempt looked, and before the latch was
			// taken, would otherwise never release it.
			latch := v.deps.TurnEnds.Next(turn.requester)
			if err := v.deps.Sessions.ProbeForkPoint(ctx, log, turn.requester); !errors.Is(err, ErrForkNothingFinished) {
				log.Info(opPendingCreate, "the requester's first turn ended before the create was held; it resumes at once", dlog.Context{
					"requester": string(turn.requester), "probe": errText(err),
				})
				done := make(chan struct{})
				close(done)
				return done
			}
			return latch
		},
		answer: func(id PendingCreateID) error {
			return &RequesterTurnWaitError{ID: id, Requester: turn.requester}
		},
		tell:  func(r CreateProgress, id PendingCreateID) { r.WaitingForRequesterTurn(id) },
		waits: "the create waits for its requester's turn to end; it resumes from the start once that turn has ended",
	}
}

// heldReason answers the wait ERR ends an attempt with, and whether it is one.
func (v *verbs) heldReason(err error) (holdReason, bool) {
	var network *namingNetworkError
	if errors.As(err, &network) {
		return v.networkHold(network), true
	}
	var turn *requesterTurnInFlight
	if errors.As(err, &turn) {
		return v.requesterTurnHold(turn), true
	}
	return holdReason{}, false
}

// settle ends ENTRY's attempt with its outcome: a network failure holds the
// create, durably, and starts its wait for the network; anything else ends
// the create and takes it out of the set.
func (v *verbs) settle(ctx context.Context, log dlog.Logger, entry *pendingCreate, created wsm.Workspace, err error) attemptResult {
	if why, waits := v.heldReason(err); waits {
		held, holdErr := v.hold(ctx, log, entry, why)
		if holdErr == nil {
			return held
		}
		// A CREATE THAT CANNOT BE HELD HAS FAILED: it would otherwise wait in
		// memory alone, which nothing outlives.
		err = holdErr
		v.reportCreateFailure(ctx, entry.spec, err)
	}
	v.pending.mu.Lock()
	delete(v.pending.byKey, entry.key)
	entry.result = attemptResult{created: created, err: err}
	subscribers, persisted, wasHeld, heldFor := entry.subscribers, entry.persisted, entry.wasHeld, entry.heldFor
	close(entry.attempt)
	v.pending.mu.Unlock()
	if persisted {
		if delErr := v.deps.DB.DeleteQueuedCreate(context.WithoutCancel(ctx), entry.key); delErr != nil {
			log.Error(opPendingCreate, "the ended create's held row could not be released; the next daemon will run it again", dlog.Context{
				"cause": delErr.Error(), "outcome_error": errText(err),
			})
		}
	}
	// THE ENDING IS RECORDED AND THE REQUESTER TOLD BEFORE ANY OP IS: a
	// caller that learns the outcome on its op can then read both.
	if wasHeld {
		fields := dlog.Context{"subscribers": len(subscribers)}
		if err != nil {
			fields["cause"] = err.Error()
			log.Info(opPendingCreate, "the held create ended without a workspace", fields)
		} else {
			fields["workspace"] = string(created.ID)
			log.Info(opPendingCreate, "the held create was created", fields)
		}
		v.tellRequesterDequeued(context.WithoutCancel(ctx), log, entry.spec, heldFor, created, err)
	}
	for _, s := range subscribers {
		if err != nil {
			s.Failed(err)
		} else {
			s.Created(created)
		}
	}
	return entry.result
}

// hold records ENTRY as waiting for WHY, durably, answers every caller waiting
// on its attempt that it waits, and starts its wait.
func (v *verbs) hold(ctx context.Context, log dlog.Logger, entry *pendingCreate, why holdReason) (attemptResult, error) {
	v.pending.mu.Lock()
	reporters := append(append([]CreateProgress(nil), entry.subscribers...), entry.waiters...)
	v.pending.mu.Unlock()
	encoded, err := encodeHeldSpec(entry.spec)
	if err != nil {
		log.Error(opPendingCreate, "the held create could not be encoded; it fails instead", dlog.Context{"cause": err.Error(), "held_for": why.cause})
		return attemptResult{}, fmt.Errorf("hold the create for %s: %w", why.cause, err)
	}
	if err := v.deps.DB.PutQueuedCreate(context.WithoutCancel(ctx), wsm.QueuedCreate{
		Key: entry.key, ID: string(entry.id), Spec: encoded, OpIDs: opIDsOf(reporters), QueuedAt: v.now(),
	}); err != nil {
		log.Error(opPendingCreate, "the held create could not be recorded; it fails instead", dlog.Context{"cause": err.Error(), "held_for": why.cause})
		return attemptResult{}, fmt.Errorf("hold the create for %s: %w", why.cause, err)
	}
	// THE LATCH IS TAKEN BEFORE ANYONE IS TOLD THE CREATE WAITS, so a
	// release that comes after that answer is never missed.
	released := why.latch(context.WithoutCancel(ctx), log)
	wait := why.answer(entry.id)
	v.pending.mu.Lock()
	entry.held, entry.persisted, entry.wasHeld = true, true, true
	entry.tellHeld, entry.heldFor = why.tell, why.cause
	entry.subscribers, entry.waiters = mergeReporters(entry.subscribers, entry.waiters), nil
	entry.result = attemptResult{err: wait}
	attempt := entry.attempt
	entry.attempt = nil
	v.pending.mu.Unlock()
	log.Info(opPendingCreate, why.waits, dlog.Merge(why.fields, dlog.Context{
		"cause": why.cause, "subscribers": len(reporters),
	}))
	for _, r := range reporters {
		why.tell(r, entry.id)
	}
	close(attempt)
	v.resumeOn(entry, released, why.cause)
	return entry.result, nil
}

// resumeOn waits, on its own goroutine, for RELEASED -- the latch of what the
// create was held for (CAUSE), taken when it was held -- then runs ENTRY
// again. It ends with the daemon, leaving the held row for the next one.
func (v *verbs) resumeOn(entry *pendingCreate, released <-chan struct{}, cause string) {
	life := v.deps.Lifetime
	inflight.Standing(func() {
		log := v.deps.Log.Global().With(dlog.Context{"create_id": string(entry.id), "repo_dir": entry.spec.RepoDir, "held_for": cause})
		select {
		case <-released:
		case <-life.Done():
			log.Info(opPendingCreate, "the daemon ended while the create was held; the next daemon resumes it", nil)
			return
		}
		v.pending.mu.Lock()
		entry.held = false
		entry.attempt = make(chan struct{})
		v.pending.mu.Unlock()
		log.Info(opPendingCreate, "what the create was held for is over; the held create resumes from the start", nil)
		spec := entry.spec
		spec.Progress = entryProgress{set: &v.pending, entry: entry}
		created, err := v.attemptCreate(life, spec)
		if err != nil && createHandedOver(err) {
			// THE DAEMON IS ENDING: the create is the next daemon's, from its
			// held row, so it is neither failed nor released here.
			v.pending.mu.Lock()
			entry.held = true
			attempt := entry.attempt
			entry.attempt = nil
			entry.subscribers, entry.waiters = mergeReporters(entry.subscribers, entry.waiters), nil
			v.pending.mu.Unlock()
			close(attempt)
			log.Info(opPendingCreate, "the daemon ended before the resumed create could run; the next daemon resumes it", dlog.Context{"cause": err.Error()})
			return
		}
		v.settle(life, log, entry, created, err)
	})
}

// RecoverQueuedCreates takes up every create a previous daemon held for the
// network: each joins the set under its own key and pending ref, and waits
// for the network as it did there. A held create whose request is already
// pending here (an identical request this daemon took first) is merged into
// that one, so it can never run twice.
func (v *verbs) RecoverQueuedCreates(ctx context.Context) error {
	log := v.deps.Log.Global()
	rows, err := v.deps.DB.QueuedCreates(ctx)
	if err != nil {
		log.Error(opPendingCreate, "the creates held for the network could not be read", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("recover the held creates: %w", err)
	}
	for _, row := range rows {
		fields := dlog.Context{"create_id": row.ID, "op_ids": len(row.OpIDs)}
		spec, err := decodeHeldSpec(row.Spec)
		if err != nil {
			log.Error(opPendingCreate, "a held create's request is unreadable; it is left held and not resumed", dlog.WithCause(fields, err))
			continue
		}
		var reporters []CreateProgress
		for _, opID := range row.OpIDs {
			reporters = appendReporter(reporters, v.deps.CreateProgressFor(opID))
		}
		v.pending.mu.Lock()
		if v.pending.byKey == nil {
			v.pending.byKey = map[string]*pendingCreate{}
		}
		if standing, ok := v.pending.byKey[row.Key]; ok {
			standing.persisted, standing.wasHeld = true, true
			standing.subscribers = mergeReporters(standing.subscribers, reporters)
			v.pending.mu.Unlock()
			log.Info(opPendingCreate, "a held create is identical to one already pending here; it joined that one", dlog.Merge(fields, dlog.Context{"pending": string(standing.id)}))
			continue
		}
		entry := &pendingCreate{
			id: PendingCreateID(row.ID), key: row.Key, spec: spec,
			held: true, persisted: true, wasHeld: true, subscribers: reporters,
			tellHeld: func(r CreateProgress, id PendingCreateID) { r.WaitingForNetwork(id) }, heldFor: "network",
		}
		v.pending.byKey[row.Key] = entry
		v.pending.mu.Unlock()
		restored := v.deps.Network.Restored()
		log.Info(opPendingCreate, "took up a create a previous daemon held", dlog.Merge(fields, dlog.Context{
			"queued_at": row.QueuedAt, "repo_dir": spec.RepoDir,
		}))
		v.resumeOn(entry, restored, "network")
	}
	return nil
}

// heldWhy words, for the requester's notice, why a create was held for
// HELDFOR (holdReason.cause).
func heldWhy(heldFor string) string {
	if heldFor == "requester_turn" {
		return "because it forked your own conversation from inside its first turn, which had nothing finished to carry yet, so agent-repl held it until that turn ended"
	}
	return "because this machine could not reach the network, so agent-repl held it until the network was back"
}

// tellRequesterDequeued tells a held create's requesting workspace how it
// ended, held for HELDFOR: the requester was answered only that it waits. A
// requester that has closed since is told nothing: nothing of it reads the
// notice.
func (v *verbs) tellRequesterDequeued(ctx context.Context, log dlog.Logger, spec CreateSpec, heldFor string, created wsm.Workspace, cause error) {
	if spec.Requester == nil {
		return
	}
	log = log.With(dlog.Context{"requester": string(*spec.Requester), "held_for": heldFor})
	requester, err := v.deps.DB.Workspace(ctx, *spec.Requester)
	if err != nil {
		log.Error(opPendingCreate, "the held create's requester could not be read; it is not told how the create ended", dlog.Context{"cause": err.Error()})
		return
	}
	if requester.Closed {
		log.Info(opPendingCreate, "the held create's requester has closed; it is not told how the create ended", nil)
		return
	}
	outcome := fmt.Sprintf("It has now been created as `%s`, in `%s`.", created.Name, created.Dir)
	if cause != nil {
		outcome = "It could not be created after all:\n\n" + cause.Error()
	}
	notice, err := v.composeBrief(ctx, log, opPendingCreate, BriefCreateDequeued, map[string]string{
		"request": describeCreateRequest(spec),
		"outcome": outcome,
		"why":     heldWhy(heldFor),
	})
	if err != nil {
		return
	}
	submission := deferredBrief(*spec.Requester, notice, conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE)
	if _, err := v.queueBrief(ctx, log, opPendingCreate, "the requesting workspace's", "the held create's outcome", submission); err != nil {
		return
	}
	log.Info(opPendingCreate, "told the requesting workspace how the held create ended", dlog.Context{"turn": string(submission.Turn)})
}

// entryProgress relays a pending create's stages to every caller attached to
// it: the ones waiting on its attempt and the ones it was held for.
type entryProgress struct {
	set   *pendingCreates
	entry *pendingCreate
}

func (p entryProgress) reporters() []CreateProgress {
	p.set.mu.Lock()
	defer p.set.mu.Unlock()
	return mergeReporters(p.entry.subscribers, p.entry.waiters)
}

func (p entryProgress) Stage(stage CreateStage) {
	for _, r := range p.reporters() {
		r.Stage(stage)
	}
}

func (p entryProgress) WaitingForNetwork(id PendingCreateID) {
	for _, r := range p.reporters() {
		r.WaitingForNetwork(id)
	}
}

func (p entryProgress) WaitingForRequesterTurn(id PendingCreateID) {
	for _, r := range p.reporters() {
		r.WaitingForRequesterTurn(id)
	}
}

func (p entryProgress) Created(created wsm.Workspace) {
	for _, r := range p.reporters() {
		r.Created(created)
	}
}

func (p entryProgress) Failed(err error) {
	for _, r := range p.reporters() {
		r.Failed(err)
	}
}

// OpID is empty: the relay is no caller's op.
func (p entryProgress) OpID() string { return "" }

// appendReporter adds R unless a reporter of its op already stands.
func appendReporter(rs []CreateProgress, r CreateProgress) []CreateProgress {
	for _, have := range rs {
		if have.OpID() == r.OpID() {
			return rs
		}
	}
	return append(rs, r)
}

// mergeReporters answers A with every reporter of B whose op A lacks.
func mergeReporters(a, b []CreateProgress) []CreateProgress {
	out := append([]CreateProgress(nil), a...)
	for _, r := range b {
		out = appendReporter(out, r)
	}
	return out
}

// opIDsOf answers the ops of RS, in order.
func opIDsOf(rs []CreateProgress) []string {
	out := make([]string, 0, len(rs))
	for _, r := range rs {
		out = append(out, r.OpID())
	}
	return out
}

// createKey is a create's NORMALIZED REQUEST: two requests are the same create
// exactly when every fact that shapes what is built is equal -- the
// repository, the topology (owner, cut from origin), the context
// and fork sources, one-shot, the name, the model, the priority, the
// permission mode and its consent, the deploy hooks, and the initial prompt
// after whitespace trimming. Who asked is not one of them.
func createKey(spec CreateSpec) string {
	priority := ""
	if spec.Priority != nil {
		priority = fmt.Sprint(int(*spec.Priority))
	}
	key := struct {
		RepoDir, Parent, ContextWorkspace, ForkFrom, Landing   string
		ContextSelection                                       *heldSelection
		FromOrigin, OneShot                                    bool
		Name, Model, Priority, PermissionMode, Consent, Prompt string
		SlackThread, Originator                                string
		PreDeploy, PostDeploy                                  []string
		Hidden                                                 bool
		Placement                                              *heldPlacement
	}{
		Hidden: spec.Hidden, Placement: heldPlacementOf(spec.Placement),
		// THE KEY SPELLS THE KIND BY NUMBER, so a spec that names none still
		// keys, and is refused by the create's validation rather than here.
		SlackThread: spec.SlackThread, Originator: fmt.Sprint(int(spec.Originator)),
		RepoDir: filepath.Clean(spec.RepoDir), Parent: ids.Optional(spec.Parent),
		Landing:          landingKey(spec.Landing),
		ContextWorkspace: ids.Optional(spec.ContextWorkspace), ForkFrom: ids.Optional(spec.ForkFrom),
		ContextSelection: heldSelectionOf(spec.ContextSelection),
		FromOrigin:       spec.FromOrigin, OneShot: spec.OneShot,
		Name: strings.TrimSpace(spec.Name), Model: spec.Model, Priority: priority,
		PermissionMode: spec.PermissionMode, Consent: spec.ConsentedUngatedMode,
		Prompt:    strings.TrimSpace(spec.InitialPrompt),
		PreDeploy: hookBytes(spec.DeployHooks.PreDeploy), PostDeploy: hookBytes(spec.DeployHooks.PostDeploy),
	}
	encoded, err := json.Marshal(key)
	if err != nil {
		panic(fmt.Sprintf("workspace: a create's key will not encode: %v", err))
	}
	sum := sha256.Sum256(encoded)
	return hex.EncodeToString(sum[:])
}

// hookBytes is each hook's deterministic wire encoding, hex.
func hookBytes(hooks []*repoconfigv1.Hook) []string {
	out := make([]string, 0, len(hooks))
	for _, h := range hooks {
		b, err := proto.MarshalOptions{Deterministic: true}.Marshal(h)
		if err != nil {
			panic(fmt.Sprintf("workspace: a deploy hook will not encode: %v", err))
		}
		out = append(out, hex.EncodeToString(b))
	}
	return out
}

// heldSpec is a held create's request as its durable row stores it. Every
// field of CreateSpec a held create can carry is here; a merge workspace is
// never held, so MergeOf and MergeName are not.
type heldSpec struct {
	RepoDir          string `json:"repo_dir"`
	InitialPrompt    string `json:"initial_prompt,omitempty"`
	OneShot          bool   `json:"one_shot,omitempty"`
	FromOrigin       bool   `json:"from_origin,omitempty"`
	ContextWorkspace string `json:"context_workspace,omitempty"`
	// ContextSelection is the context workspace's selected bubble, absent
	// when nothing was selected.
	ContextSelection     *heldSelection `json:"context_selection,omitempty"`
	BaseRef              string         `json:"base_ref,omitempty"`
	Parent               string         `json:"parent,omitempty"`
	ForkFrom             string         `json:"fork_from,omitempty"`
	Requester            string         `json:"requester,omitempty"`
	ConsentedUngatedMode string         `json:"consented_ungated_mode,omitempty"`
	PreDeploy            []string       `json:"pre_deploy,omitempty"`
	PostDeploy           []string       `json:"post_deploy,omitempty"`
	Name                 string         `json:"name,omitempty"`
	Model                string         `json:"model,omitempty"`
	PermissionMode       string         `json:"permission_mode,omitempty"`
	Priority             *int           `json:"priority,omitempty"`
	SlackThread          string         `json:"slack_thread,omitempty"`
	// Originator is the spec's kind (lifecycleconfig.Originator.String). A
	// create held by a build before originators were recorded carries none,
	// and resumes as user_or_workspace: that build had no emoji-ack kind.
	Originator string `json:"originator,omitempty"`
	// Landing is the explicit landing target, absent for the relationship's
	// default; a held row written before landing targets existed has none,
	// which is what its create meant.
	Landing *heldLanding `json:"landing,omitempty"`
	// Hidden is the born-hidden request, and Placement the creator's filing
	// of the workspace; a row written before either existed has neither.
	Hidden    bool           `json:"hidden,omitempty"`
	Placement *heldPlacement `json:"placement,omitempty"`
}

// heldPlacement is a held create's CreatePlacement.
type heldPlacement struct {
	Subsection    string   `json:"subsection,omitempty"`
	NewSubsection string   `json:"new_subsection,omitempty"`
	HasNew        bool     `json:"has_new,omitempty"`
	Gather        []string `json:"gather,omitempty"`
}

// heldPlacementOf is PLACEMENT as a held row stores it, nil for none.
func heldPlacementOf(p *CreatePlacement) *heldPlacement {
	if p == nil {
		return nil
	}
	held := &heldPlacement{Subsection: p.Subsection}
	if p.NewSubsection != nil {
		held.HasNew, held.NewSubsection, held.Gather = true, p.NewSubsection.Name, p.NewSubsection.Gather
	}
	return held
}

// createPlacementOf reads a held row's placement back, nil for none.
func createPlacementOf(held *heldPlacement) *CreatePlacement {
	if held == nil {
		return nil
	}
	out := &CreatePlacement{Subsection: held.Subsection}
	if held.HasNew {
		out.NewSubsection = &wsm.NewSubsectionPlacement{Name: held.NewSubsection, Gather: held.Gather}
	}
	return out
}

// heldSelection is a held create's context selection (ContextSelection).
type heldSelection struct {
	Kind     string `json:"kind"`
	Row      string `json:"row"`
	Markdown string `json:"markdown"`
}

// heldSelectionOf is SELECTED as a held row stores it, nil for none.
func heldSelectionOf(selected *ContextSelection) *heldSelection {
	if selected == nil {
		return nil
	}
	return &heldSelection{Kind: string(selected.Kind), Row: selected.Row, Markdown: selected.Markdown}
}

// contextSelectionOf reads a held row's context selection back, nil for
// none.
func contextSelectionOf(held *heldSelection) *ContextSelection {
	if held == nil {
		return nil
	}
	return &ContextSelection{Kind: ContextSelectionKind(held.Kind), Row: held.Row, Markdown: held.Markdown}
}

// heldLanding is a held create's explicit landing target.
type heldLanding struct {
	// Into is the workspace landed into, empty for the primary branch.
	Into string `json:"into,omitempty"`
}

// landingKey spells an explicit landing target for a create's key: empty for
// the relationship's default, so a default and an explicit landing that
// happen to agree stay distinct requests.
func landingKey(landing *wsm.LandingChoice) string {
	if landing == nil {
		return ""
	}
	return "lands:" + ids.Optional(landing.Into)
}

// encodeHeldSpec encodes SPEC for its durable row.
func encodeHeldSpec(spec CreateSpec) (string, error) {
	if spec.MergeOf != nil {
		panic("workspace: a merge workspace's create is never held")
	}
	held := heldSpec{
		RepoDir: spec.RepoDir, InitialPrompt: spec.InitialPrompt, OneShot: spec.OneShot, FromOrigin: spec.FromOrigin,
		ContextWorkspace: ids.Optional(spec.ContextWorkspace), ContextSelection: heldSelectionOf(spec.ContextSelection),
		BaseRef: spec.BaseRef, Parent: ids.Optional(spec.Parent),
		ForkFrom: ids.Optional(spec.ForkFrom), Requester: ids.Optional(spec.Requester),
		ConsentedUngatedMode: spec.ConsentedUngatedMode, PreDeploy: hookBytes(spec.DeployHooks.PreDeploy),
		PostDeploy: hookBytes(spec.DeployHooks.PostDeploy), Name: spec.Name, Model: spec.Model, PermissionMode: spec.PermissionMode,
		SlackThread: spec.SlackThread, Originator: spec.Originator.String(),
		Hidden: spec.Hidden, Placement: heldPlacementOf(spec.Placement),
	}
	if spec.Priority != nil {
		p := int(*spec.Priority)
		held.Priority = &p
	}
	if spec.Landing != nil {
		held.Landing = &heldLanding{Into: ids.Optional(spec.Landing.Into)}
	}
	out, err := json.Marshal(held)
	if err != nil {
		return "", err
	}
	return string(out), nil
}

// decodeHeldSpec reads a held create's request back.
func decodeHeldSpec(encoded string) (CreateSpec, error) {
	var held heldSpec
	if err := json.Unmarshal([]byte(encoded), &held); err != nil {
		return CreateSpec{}, fmt.Errorf("decode a held create's request: %w", err)
	}
	pre, err := decodeHooks(held.PreDeploy)
	if err != nil {
		return CreateSpec{}, err
	}
	post, err := decodeHooks(held.PostDeploy)
	if err != nil {
		return CreateSpec{}, err
	}
	spec := CreateSpec{
		RepoDir: held.RepoDir, InitialPrompt: held.InitialPrompt, OneShot: held.OneShot, FromOrigin: held.FromOrigin,
		ContextWorkspace: optionalID(held.ContextWorkspace), ContextSelection: contextSelectionOf(held.ContextSelection),
		BaseRef: held.BaseRef, Parent: optionalID(held.Parent),
		ForkFrom: optionalID(held.ForkFrom), Requester: optionalID(held.Requester),
		ConsentedUngatedMode: held.ConsentedUngatedMode, DeployHooks: wsm.DeployHooks{PreDeploy: pre, PostDeploy: post},
		Name: held.Name, Model: held.Model, PermissionMode: held.PermissionMode, SlackThread: held.SlackThread,
		Originator: lifecycleconfig.OriginatorUserOrWorkspace,
		Hidden:     held.Hidden, Placement: createPlacementOf(held.Placement),
	}
	if held.Originator != "" {
		if spec.Originator, err = lifecycleconfig.ParseOriginator(held.Originator); err != nil {
			return CreateSpec{}, fmt.Errorf("decode a held create's request: %w", err)
		}
	}
	if held.Priority != nil {
		p := wsm.Priority(*held.Priority)
		spec.Priority = &p
	}
	if held.Landing != nil {
		spec.Landing = &wsm.LandingChoice{Into: optionalID(held.Landing.Into)}
	}
	return spec, nil
}

func decodeHooks(encoded []string) ([]*repoconfigv1.Hook, error) {
	var out []*repoconfigv1.Hook
	for _, e := range encoded {
		raw, err := hex.DecodeString(e)
		if err != nil {
			return nil, fmt.Errorf("decode a held create's deploy hook: %w", err)
		}
		hook := &repoconfigv1.Hook{}
		if err := proto.Unmarshal(raw, hook); err != nil {
			return nil, fmt.Errorf("decode a held create's deploy hook: %w", err)
		}
		out = append(out, hook)
	}
	return out, nil
}

func optionalID(s string) *ids.WorkspaceID {
	if s == "" {
		return nil
	}
	id := ids.WorkspaceID(s)
	return &id
}
