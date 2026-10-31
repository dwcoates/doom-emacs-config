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
// A HELD CREATE RESUMES ON THE NETWORK-RESTORED SIGNAL (netstate.Monitor's
// Restored latch), never on a timer. It resumes from the start of the create,
// whose first expensive step is the naming call, and completes exactly as it
// would have. A held create holds no create-gate ticket while it waits, so a
// stand-down never waits on the network; each resumed attempt holds one.

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
	// held is true while the create waits for the network.
	held bool
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
		held, attempt, standingResult := entry.held, entry.attempt, entry.result
		v.pending.mu.Unlock()
		log.Info(opPendingCreate, "the request is identical to a pending create; it joined that create", dlog.Context{
			"held": held, "requester": ids.Optional(ticket.spec.Requester),
		})
		if held {
			if ticket.progress != nil {
				ticket.progress.WaitingForNetwork(entry.id)
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

// attemptCreate is one run of the create. A workspace created for a Slack
// message then takes its thread up (adoptSlackOrigin), once the create-gate
// ticket is released, so a slow post never holds a stand-down back.
func (v *verbs) attemptCreate(ctx context.Context, spec CreateSpec) (wsm.Workspace, error) {
	created, err := v.attemptHeldCreate(ctx, spec)
	if err == nil {
		v.adoptSlackOrigin(ctx, created, spec.SlackThread)
	}
	return created, err
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
	var network *namingNetworkError
	if err != nil && !errors.As(err, &network) {
		v.reportCreateFailure(ctx, spec, err)
	}
	return created, err
}

// settle ends ENTRY's attempt with its outcome: a network failure holds the
// create, durably, and starts its wait for the network; anything else ends
// the create and takes it out of the set.
func (v *verbs) settle(ctx context.Context, log dlog.Logger, entry *pendingCreate, created wsm.Workspace, err error) attemptResult {
	var network *namingNetworkError
	if errors.As(err, &network) {
		held, holdErr := v.hold(ctx, log, entry, network)
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
	subscribers, persisted, wasHeld := entry.subscribers, entry.persisted, entry.wasHeld
	close(entry.attempt)
	v.pending.mu.Unlock()
	if persisted {
		if delErr := v.deps.DB.DeleteQueuedCreate(context.WithoutCancel(ctx), entry.key); delErr != nil {
			log.Error(opPendingCreate, "the ended create's held row could not be released; the next daemon will run it again", dlog.Context{
				"cause": delErr.Error(), "outcome_error": errText(err),
			})
		}
	}
	for _, s := range subscribers {
		if err != nil {
			s.Failed(err)
		} else {
			s.Created(created)
		}
	}
	if wasHeld {
		fields := dlog.Context{"subscribers": len(subscribers)}
		if err != nil {
			fields["cause"] = err.Error()
			log.Info(opPendingCreate, "the create held for the network ended without a workspace", fields)
		} else {
			fields["workspace"] = string(created.ID)
			log.Info(opPendingCreate, "the create held for the network was created", fields)
		}
		v.tellRequesterDequeued(context.WithoutCancel(ctx), log, entry.spec, created, err)
	}
	return entry.result
}

// hold records ENTRY as waiting for the network, durably, answers every
// caller waiting on its attempt that it waits, and starts its wait.
func (v *verbs) hold(ctx context.Context, log dlog.Logger, entry *pendingCreate, network *namingNetworkError) (attemptResult, error) {
	v.pending.mu.Lock()
	reporters := append(append([]CreateProgress(nil), entry.subscribers...), entry.waiters...)
	v.pending.mu.Unlock()
	encoded, err := encodeHeldSpec(entry.spec)
	if err != nil {
		log.Error(opPendingCreate, "the create held for the network could not be encoded; it fails instead", dlog.Context{"cause": err.Error()})
		return attemptResult{}, fmt.Errorf("hold the create for the network: %w", err)
	}
	if err := v.deps.DB.PutQueuedCreate(context.WithoutCancel(ctx), wsm.QueuedCreate{
		Key: entry.key, ID: string(entry.id), Spec: encoded, OpIDs: opIDsOf(reporters), QueuedAt: v.now(),
	}); err != nil {
		log.Error(opPendingCreate, "the create held for the network could not be recorded; it fails instead", dlog.Context{"cause": err.Error()})
		return attemptResult{}, fmt.Errorf("hold the create for the network: %w", err)
	}
	// THE LATCH IS TAKEN BEFORE ANYONE IS TOLD THE CREATE WAITS, so a
	// restoration that comes after that answer is never missed.
	restored := v.deps.Network.Restored()
	wait := &NetworkWaitError{ID: entry.id, Evidence: network.verdict.Evidence}
	v.pending.mu.Lock()
	entry.held, entry.persisted, entry.wasHeld = true, true, true
	entry.subscribers, entry.waiters = mergeReporters(entry.subscribers, entry.waiters), nil
	entry.result = attemptResult{err: wait}
	attempt := entry.attempt
	entry.attempt = nil
	v.pending.mu.Unlock()
	log.Info(opPendingCreate, "the create waits for the network; it resumes from the naming call when the network is restored", dlog.Context{
		"cause": "network", "evidence": string(network.verdict.Evidence), "detail": network.verdict.Detail,
		"naming_cause": network.cause, "subscribers": len(reporters),
	})
	for _, r := range reporters {
		r.WaitingForNetwork(entry.id)
	}
	close(attempt)
	v.resumeOnRestore(entry, restored)
	return entry.result, nil
}

// resumeOnRestore waits, on its own goroutine, for RESTORED -- the
// network-restored latch, taken when the create was held -- then runs ENTRY
// again. It ends with the daemon, leaving the held row for the next one.
func (v *verbs) resumeOnRestore(entry *pendingCreate, restored <-chan struct{}) {
	life := v.deps.Lifetime
	go func() {
		log := v.deps.Log.Global().With(dlog.Context{"create_id": string(entry.id), "repo_dir": entry.spec.RepoDir})
		select {
		case <-restored:
		case <-life.Done():
			log.Info(opPendingCreate, "the daemon ended while the create waited for the network; the next daemon resumes it", nil)
			return
		}
		v.pending.mu.Lock()
		entry.held = false
		entry.attempt = make(chan struct{})
		v.pending.mu.Unlock()
		log.Info(opPendingCreate, "the network is restored; the held create resumes from the naming call", nil)
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
	}()
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
		}
		v.pending.byKey[row.Key] = entry
		v.pending.mu.Unlock()
		restored := v.deps.Network.Restored()
		log.Info(opPendingCreate, "took up a create a previous daemon held for the network", dlog.Merge(fields, dlog.Context{
			"queued_at": row.QueuedAt, "repo_dir": spec.RepoDir,
		}))
		v.resumeOnRestore(entry, restored)
	}
	return nil
}

// tellRequesterDequeued tells a held create's requesting workspace how it
// ended: the requester was answered only that it waits.
func (v *verbs) tellRequesterDequeued(ctx context.Context, log dlog.Logger, spec CreateSpec, created wsm.Workspace, cause error) {
	if spec.Requester == nil {
		return
	}
	outcome := fmt.Sprintf("It has now been created as `%s`, in `%s`.", created.Name, created.Dir)
	if cause != nil {
		outcome = "It could not be created after all:\n\n" + cause.Error()
	}
	log = log.With(dlog.Context{"requester": string(*spec.Requester)})
	notice, err := v.composeBrief(ctx, log, opPendingCreate, BriefCreateDequeued, map[string]string{
		"request": describeCreateRequest(spec),
		"outcome": outcome,
	})
	if err != nil {
		return
	}
	submission := deferredBrief(*spec.Requester, notice, conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE)
	if _, err := v.queueBrief(ctx, log, opPendingCreate, "the requesting workspace's", "the held create's outcome", submission); err != nil {
		return
	}
	log.Info(opPendingCreate, "told the requesting workspace how the create held for the network ended", dlog.Context{"turn": string(submission.Turn)})
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
// repository, the topology (owner, owner notice, cut from origin), the context
// and fork sources, one-shot, the name, the model, the priority, the
// permission mode and its consent, the deploy hooks, and the initial prompt
// after whitespace trimming. Who asked is not one of them.
func createKey(spec CreateSpec) string {
	priority := ""
	if spec.Priority != nil {
		priority = fmt.Sprint(int(*spec.Priority))
	}
	key := struct {
		RepoDir, Parent, OwnerNotice, ContextWorkspace, ForkFrom, Landing string
		FromOrigin, OneShot                                               bool
		Name, Model, Priority, PermissionMode, Consent, Prompt            string
		SlackThread                                                       string
		PreDeploy, PostDeploy                                             []string
	}{
		SlackThread: spec.SlackThread,
		RepoDir:     filepath.Clean(spec.RepoDir), Parent: ids.Optional(spec.Parent), OwnerNotice: strings.TrimSpace(spec.OwnerNotice),
		Landing:          landingKey(spec.Landing),
		ContextWorkspace: ids.Optional(spec.ContextWorkspace), ForkFrom: ids.Optional(spec.ForkFrom),
		FromOrigin: spec.FromOrigin, OneShot: spec.OneShot,
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
func hookBytes(hooks []*repoconfigv1.DeployHook) []string {
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
	RepoDir              string   `json:"repo_dir"`
	InitialPrompt        string   `json:"initial_prompt,omitempty"`
	OneShot              bool     `json:"one_shot,omitempty"`
	FromOrigin           bool     `json:"from_origin,omitempty"`
	ContextWorkspace     string   `json:"context_workspace,omitempty"`
	BaseRef              string   `json:"base_ref,omitempty"`
	Parent               string   `json:"parent,omitempty"`
	ForkFrom             string   `json:"fork_from,omitempty"`
	Requester            string   `json:"requester,omitempty"`
	OwnerNotice          string   `json:"owner_notice,omitempty"`
	ConsentedUngatedMode string   `json:"consented_ungated_mode,omitempty"`
	PreDeploy            []string `json:"pre_deploy,omitempty"`
	PostDeploy           []string `json:"post_deploy,omitempty"`
	Name                 string   `json:"name,omitempty"`
	Model                string   `json:"model,omitempty"`
	PermissionMode       string   `json:"permission_mode,omitempty"`
	Priority             *int     `json:"priority,omitempty"`
	SlackThread          string   `json:"slack_thread,omitempty"`
	// Landing is the explicit landing target, absent for the relationship's
	// default; a held row written before landing targets existed has none,
	// which is what its create meant.
	Landing *heldLanding `json:"landing,omitempty"`
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
		ContextWorkspace: ids.Optional(spec.ContextWorkspace), BaseRef: spec.BaseRef, Parent: ids.Optional(spec.Parent),
		ForkFrom: ids.Optional(spec.ForkFrom), Requester: ids.Optional(spec.Requester), OwnerNotice: spec.OwnerNotice,
		ConsentedUngatedMode: spec.ConsentedUngatedMode, PreDeploy: hookBytes(spec.DeployHooks.PreDeploy),
		PostDeploy: hookBytes(spec.DeployHooks.PostDeploy), Name: spec.Name, Model: spec.Model, PermissionMode: spec.PermissionMode,
		SlackThread: spec.SlackThread,
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
		ContextWorkspace: optionalID(held.ContextWorkspace), BaseRef: held.BaseRef, Parent: optionalID(held.Parent),
		ForkFrom: optionalID(held.ForkFrom), Requester: optionalID(held.Requester), OwnerNotice: held.OwnerNotice,
		ConsentedUngatedMode: held.ConsentedUngatedMode, DeployHooks: wsm.DeployHooks{PreDeploy: pre, PostDeploy: post},
		Name: held.Name, Model: held.Model, PermissionMode: held.PermissionMode, SlackThread: held.SlackThread,
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

func decodeHooks(encoded []string) ([]*repoconfigv1.DeployHook, error) {
	var out []*repoconfigv1.DeployHook
	for _, e := range encoded {
		raw, err := hex.DecodeString(e)
		if err != nil {
			return nil, fmt.Errorf("decode a held create's deploy hook: %w", err)
		}
		hook := &repoconfigv1.DeployHook{}
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
