package create

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"sync"
	"time"
)

// Config supplies the daemon-owned creation collaborators.  They are all
// interfaces because the manager is orchestration, not git, shim, or frontend
// plumbing; the daemon stitch point supplies concrete adapters later.
type Config struct {
	Store       JobStore
	Planner     WorktreePlanner
	Worktrees   WorktreeCreator
	Geometry    WorkspaceGeometryRecorder
	Sessions    SessionCreator
	Health      SessionHealthChecker
	Prompts     InitialPromptSubmitter
	Available   WorkspaceAvailablePublisher
	Releases    SessionPublicationReleaser
	Publication SessionPublicationPreparer
	HostActions HostActionSink
	Logf        func(string, ...any)
	// Errorf is the same canonical logger at ERROR severity.  It is REQUIRED
	// and separate because a workspace the host has silently refused to
	// materialize is a fault, and a fault emitted at info is a fault nobody
	// finds: the incident that produced this escalation buried its own cause
	// under 1592 info lines.
	Errorf func(string, ...any)
	// Now is the manager's clock, used only by the awaiting-host sweep.  Tests
	// supply a controlled one so a cadence can be exercised without waiting on
	// wall time; production leaves it nil and gets time.Now.
	Now func() time.Time
	// MaterializationRequestInterval bounds how often one job's unanswered
	// materialization request is re-sent.  Zero takes the package default.
	MaterializationRequestInterval time.Duration
	// MaterializationHeldDeadline bounds how long a job may wait on the host
	// before the wait is escalated.  Zero takes the package default.
	MaterializationHeldDeadline time.Duration
}

// routedJobBuffer is how many job ids the creation worker may fall behind by
// before the router stops naming them individually and falls back to the
// coalesced store sweep.  It is a throughput knob and nothing more: the durable
// job store is the source of truth, so a dropped id costs a re-list and never a
// lost workspace.
const routedJobBuffer = 256

// defaultMaterializationRequestInterval is how long an unanswered
// materialization request is left standing before the daemon asks again.
//
// THE ORIGINAL REQUEST IS NOT A DELIVERY GUARANTEE.  It is a host-only frame
// broadcast to whoever is connected at that instant, and a daemon that bounced
// out from under a running Emacs has no host client at all for as long as the
// editor takes to notice and reconnect — every request issued in that window is
// dropped with nobody to receive it.  The only other path to the host is the
// snapshot a host connect replays, so before this cadence existed a workspace
// created during that window waited for an EDITOR RESTART.  One did, for
// sixteen minutes, while the daemon logged the same held-publication line 1592
// times and never once said the request had reached no one.
const defaultMaterializationRequestInterval = 15 * time.Second

// defaultMaterializationHeldDeadline is how long a workspace may sit waiting on
// the host before the daemon stops treating the wait as normal.
//
// SOMETHING MUST NOTICE.  A held job costs a worktree, a registered session and
// a live shim's half-gigabyte, and it holds back the user's initial prompt; two
// such jobs sat unnoticed for DAYS across daemon restarts because nothing ever
// looked at how old a wait was.  Crossing this deadline is not a cleanup
// trigger — the workspace is real and a host that comes back still materializes
// it — it is a REPORT: one error-level record and one user-visible failure
// card, both emitted exactly once per job.
const defaultMaterializationHeldDeadline = 2 * time.Minute

// HostActionTypeWorkspaceMaterializationHeld is the escalation's user-visible
// arm.  It rides the create-failed action so the host renders it through the
// path it already has for a creation that went wrong, and its ID is derived
// from the job's so a second escalation for the same job is impossible.
const HostActionTypeWorkspaceMaterializationHeld = HostActionTypeWorkspaceCreateFailed

// Manager drives durable workspace creation jobs to the next safe boundary.
// It has no dependency on frontend/server or sessioncontroller so ownership remains
// unambiguous: this package owns creation; other packages supply adapters.
//
// EXECUTION IS OWNED BY WORKERS, NEVER BY A CALLER.  Every path that wants a
// job advanced — the command-file router, an interactive create, a
// materialization ACK, boot resume — ROUTES the job's id and returns.
// RunCreationWorker is the single goroutine that runs the state machine, so a
// job wedged inside Process can only ever stall creation, never the ingress
// that feeds it.  Publication of host actions has its own worker for the same
// reason.
type Manager struct {
	cfg Config

	// jobs carries routed job IDS ONLY.  The durable store stays the source of
	// truth: a bounce replays from disk, so nothing but an id is ever in
	// flight.
	jobs chan string
	// sweep is the coalesced "re-list the non-terminal jobs" signal.  It is
	// what makes routing total without ever blocking: when jobs is full the
	// router raises this instead, and the worker recovers the ids from the
	// store.  Capacity one because the worker re-lists after every wake, so a
	// coalesced pair of signals loses nothing.
	sweep chan struct{}
	// hostWake is the coalesced "there are host actions to publish" signal,
	// consumed by RunHostActionWorker.  Same shape and same reasoning as sweep.
	hostWake chan struct{}

	// running serializes every durable transition for one job.  Inbox polling,
	// interactive submission, restart recovery, and a materialization ACK can
	// all arrive concurrently; allowing two of them to drive the same state
	// machine would duplicate effects between checkpoints.
	mu      sync.Mutex
	running map[string]bool
}

func NewManager(cfg Config) (*Manager, error) {
	switch {
	case cfg.Store == nil:
		return nil, fmt.Errorf("workspace create: manager needs a JobStore")
	case cfg.Planner == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorktreePlanner")
	case cfg.Worktrees == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorktreeCreator")
	case cfg.Geometry == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorkspaceGeometryRecorder (without it a created workspace has no recorded merge geometry and can never be merged)")
	case cfg.Sessions == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionCreator")
	case cfg.Health == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionHealthChecker")
	case cfg.Prompts == nil:
		return nil, fmt.Errorf("workspace create: manager needs an InitialPromptSubmitter")
	case cfg.Available == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorkspaceAvailablePublisher")
	case cfg.Releases == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionPublicationReleaser")
	case cfg.Publication == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionPublicationPreparer")
	case cfg.HostActions == nil:
		return nil, fmt.Errorf("workspace create: manager needs a HostActionSink")
	case cfg.Logf == nil:
		return nil, fmt.Errorf("workspace create: manager needs a logger")
	case cfg.Errorf == nil:
		return nil, fmt.Errorf("workspace create: manager needs an error-level logger (without it a workspace the host never materialized is reported at the same severity as routine progress and nobody finds it)")
	}
	return &Manager{
		cfg:      cfg,
		jobs:     make(chan string, routedJobBuffer),
		sweep:    make(chan struct{}, 1),
		hostWake: make(chan struct{}, 1),
		running:  map[string]bool{},
	}, nil
}

// RouteJob hands one job id to the creation worker.  IT NEVER BLOCKS, which is
// the whole point: its callers are ingress paths (the command-file router, a
// frontend command, a materialization ACK) and a creation worker stuck on a
// wedged job must not be able to stop them.
//
// A full buffer degrades to the coalesced sweep rather than to a drop: the id
// is recoverable from the durable store, and the worker re-lists every
// non-terminal job when it sees the signal.
func (m *Manager) RouteJob(id string) {
	if id == "" {
		m.cfg.Logf("workspace-create: ROUTE REFUSED empty job id")
		return
	}
	select {
	case m.jobs <- id:
		m.cfg.Logf("workspace-create: routed job id=%s queued=%d", id, len(m.jobs))
	default:
		m.cfg.Logf("workspace-create: creation worker BEHIND buffer=%d id=%s; falling back to a store sweep", cap(m.jobs), id)
		m.RouteSweep()
	}
}

// RouteSweep asks the creation worker to re-derive its work from the durable
// store.  Nonblocking and coalescing.
func (m *Manager) RouteSweep() {
	select {
	case m.sweep <- struct{}{}:
	default:
	}
}

// RouteHostActions asks the host-action worker to publish whatever the store
// holds.  Nonblocking and coalescing, so neither the command-file router nor
// the creation worker can be stalled by a slow or absent host.
func (m *Manager) RouteHostActions() {
	select {
	case m.hostWake <- struct{}{}:
	default:
	}
}

// RunCreationWorker is THE owner of Process.  It resumes every non-terminal job
// through the same channel a fresh command uses, then serves routed ids until
// CTX ends.
//
// One poisoned job is contained here and nowhere else: ErrJobFailed is already
// durable, logged, and surfaced to the host, so the worker takes the next id.
// A structural failure cannot be handed anywhere either — this goroutine has no
// caller to return it to — so it is logged loudly and the worker keeps serving,
// because a daemon that stops creating workspaces silently is strictly worse
// than one that keeps trying and says why each time.
func (m *Manager) RunCreationWorker(ctx context.Context) error {
	m.cfg.Logf("workspace-create: creation worker STARTED buffer=%d", cap(m.jobs))
	m.resumeAtBoot()
	for {
		for m.drainOnce(ctx) {
		}
		select {
		case <-ctx.Done():
			m.cfg.Logf("workspace-create: creation worker STOPPED: %v", ctx.Err())
			return ctx.Err()
		case id := <-m.jobs:
			m.runRouted(ctx, id)
		case <-m.sweep:
			m.sweepStore(ctx)
		}
	}
}

// RunHostActionWorker is THE owner of DrainHostActions.  Publication is
// deliberately off both the command-file router and the creation worker: a host
// that is slow to accept an action must not delay the next command file or the
// next workspace.
func (m *Manager) RunHostActionWorker(ctx context.Context) error {
	m.cfg.Logf("workspace-create: host-action worker STARTED")
	m.RouteHostActions()
	for {
		for m.drainHostOnce(ctx) {
		}
		select {
		case <-ctx.Done():
			m.cfg.Logf("workspace-create: host-action worker STOPPED: %v", ctx.Err())
			return ctx.Err()
		case <-m.hostWake:
			m.publishHostActions(ctx)
		}
	}
}

// drainOnce serves at most one pending creation signal WITHOUT blocking,
// reporting whether it found work.  RunCreationWorker loops it to quiescence
// before parking; tests loop it to run the worker's exact behavior
// deterministically on the calling goroutine.
func (m *Manager) drainOnce(ctx context.Context) bool {
	select {
	case id := <-m.jobs:
		m.runRouted(ctx, id)
		return true
	default:
	}
	select {
	case <-m.sweep:
		m.sweepStore(ctx)
		return true
	default:
	}
	return false
}

// drainHostOnce is drainHostActions' nonblocking half; see drainOnce.
func (m *Manager) drainHostOnce(ctx context.Context) bool {
	select {
	case <-m.hostWake:
		m.publishHostActions(ctx)
		return true
	default:
	}
	return false
}

// runRouted advances one routed job and CONTAINS its failure.  A job-level
// failure is already durable and surfaced; a structural one is logged loudly.
// Neither may end the worker, because the next id in the channel belongs to a
// different workspace that has nothing to do with this one.
func (m *Manager) runRouted(ctx context.Context, id string) {
	err := m.Process(ctx, id)
	switch {
	case err == nil:
	case errors.Is(err, ErrJobFailed):
		m.cfg.Logf("workspace-create: CONTAINED job failure id=%s error=%v; creation worker continuing", id, err)
	default:
		m.cfg.Logf("workspace-create: creation worker STRUCTURAL FAILURE id=%s error=%v; the job stays in the store and the worker continues", id, err)
	}
}

// resumeAtBoot restarts every non-terminal job by ROUTING its id, so recovery
// and fresh ingestion travel one path.  The external collaborators are
// idempotent by job ID, which closes the crash gap between performing an effect
// and persisting its succeeding state transition.
func (m *Manager) resumeAtBoot() {
	ids, err := m.resumableIDs()
	if err != nil {
		m.cfg.Logf("workspace-create: BOOT RESUME FAILED error=%v; no job is lost (the store still holds them) but none is resumed until the next sweep", err)
		return
	}
	m.cfg.Logf("workspace-create: boot resume routing jobs=%d", len(ids))
	for _, id := range ids {
		m.RouteJob(id)
	}
	// Retained host work is published by its own worker, which signals itself
	// at start; the creation worker deliberately does not reach into it.
	m.RouteHostActions()
}

// sweepStore recovers the work a full route buffer could not name individually.
func (m *Manager) sweepStore(ctx context.Context) {
	ids, err := m.resumableIDs()
	if err != nil {
		m.cfg.Logf("workspace-create: SWEEP FAILED error=%v; re-raising the sweep so the work is not stranded", err)
		m.RouteSweep()
		return
	}
	m.cfg.Logf("workspace-create: sweep processing jobs=%d", len(ids))
	for _, id := range ids {
		m.runRouted(ctx, id)
	}
}

// resumableIDs lists the ids of every job that is not resting on a terminal or
// host-owned boundary.
func (m *Manager) resumableIDs() ([]string, error) {
	jobs, err := m.cfg.Store.List()
	if err != nil {
		return nil, fmt.Errorf("workspace create: list jobs for resume: %w", err)
	}
	ids := make([]string, 0, len(jobs))
	for _, job := range jobs {
		if job.State == StateReady || job.State == StateFailed || job.State == StateAwaitingEmacs {
			continue
		}
		m.cfg.Logf("workspace-create: resumable job id=%s state=%s name=%q", job.ID, job.State, job.Request.Name)
		ids = append(ids, job.ID)
	}
	return ids, nil
}

// publishHostActions runs one drain and contains its failure: a retained action
// stays pending in the store, so the next signal redelivers it.
//
// It also runs the awaiting-host sweep, because re-asking the host to
// materialize a workspace IS host publication and belongs on the worker that
// owns it: the creation worker must not be able to stall behind a slow host,
// and a second goroutine publishing to the same host would be a second owner of
// the same channel.
func (m *Manager) publishHostActions(ctx context.Context) {
	if err := m.DrainHostActions(ctx); err != nil {
		m.cfg.Logf("workspace-create: host-action publication FAILED error=%v; the actions stay pending and are redelivered on the next signal", err)
	}
	if err := m.SweepAwaitingHost(ctx); err != nil {
		m.cfg.Logf("workspace-create: awaiting-host sweep FAILED error=%v; every held job stays held and the next signal sweeps again", err)
	}
}

func (m *Manager) now() time.Time {
	if m.cfg.Now != nil {
		return m.cfg.Now()
	}
	return time.Now()
}

func (m *Manager) materializationRequestInterval() time.Duration {
	if m.cfg.MaterializationRequestInterval > 0 {
		return m.cfg.MaterializationRequestInterval
	}
	return defaultMaterializationRequestInterval
}

func (m *Manager) materializationHeldDeadline() time.Duration {
	if m.cfg.MaterializationHeldDeadline > 0 {
		return m.cfg.MaterializationHeldDeadline
	}
	return defaultMaterializationHeldDeadline
}

// escalateHeldMaterialization reports one job whose wait on the host has run
// past the deadline: an error-level record and a retained user-visible failure,
// each exactly once.
//
// IT DESTROYS NOTHING.  The worktree, the branch, the session and the shim are
// all real and all still the user's; the only thing wrong is that no host ever
// answered.  So the job STAYS on awaiting_emacs, and the next host connect
// still materializes it from the reconnect snapshot.  The escalation is the
// report, not the remedy.
//
// The durable latch is written FIRST.  A daemon that dies between the report
// and the latch would re-report on every sweep forever, and an escalation that
// repeats is one nobody reads.
func (m *Manager) escalateHeldMaterialization(ctx context.Context, job Job, heldMs int64) {
	latched, err := m.cfg.Store.Update(job.ID, func(j *Job) error {
		j.MaterializationEscalated = true
		return nil
	})
	if err != nil {
		m.cfg.Logf("workspace-create: held-materialization escalation could not latch id=%s error=%v; nothing is reported this sweep and the next one retries", job.ID, err)
		return
	}
	m.cfg.Errorf("workspace-create: MATERIALIZATION HELD PAST DEADLINE id=%s name=%q worktree=%s session=%s held_ms=%d requests=%d deadline_ms=%d — no host ever acknowledged this workspace; it exists on disk with a live session and its initial prompt is undelivered, and it will materialize on the next host connect",
		latched.ID, latched.FinalName, latched.WorktreePath, latched.SessionID, heldMs, latched.MaterializationRequests, m.materializationHeldDeadline().Milliseconds())
	payload, err := json.Marshal(WorkspaceCreateFailure{
		JobID:         latched.ID,
		RequestedName: latched.Request.Name,
		Error: fmt.Sprintf("workspace %q was built but no editor materialized it after %ds and %d requests; it is intact on disk at %s and appears when the editor reconnects",
			firstNonEmptyName(latched.FinalName, latched.Request.Name), heldMs/1000, latched.MaterializationRequests, latched.WorktreePath),
	})
	if err != nil {
		m.cfg.Errorf("workspace-create: HELD MATERIALIZATION NOT SURFACED id=%s reason=payload error=%v", latched.ID, err)
		return
	}
	notice := HostAction{ID: latched.ID + ":materialization-held", SourceFile: latched.SourceFile, SourceIndex: latched.SourceIndex, Type: HostActionTypeWorkspaceMaterializationHeld, Payload: payload}
	if _, _, err := m.cfg.Store.EnqueueHostAction(notice); err != nil {
		m.cfg.Errorf("workspace-create: HELD MATERIALIZATION NOT SURFACED id=%s reason=enqueue error=%v", latched.ID, err)
		return
	}
	m.cfg.Logf("workspace-create: held materialization SURFACED id=%s host_action=%s name=%q", latched.ID, notice.ID, latched.FinalName)
	if err := m.DrainHostActions(ctx); err != nil {
		m.cfg.Logf("workspace-create: held-materialization notice not delivered yet id=%s host_action=%s error=%v retained=yes", latched.ID, notice.ID, err)
	}
}

func firstNonEmptyName(values ...string) string {
	for _, value := range values {
		if value != "" {
			return value
		}
	}
	return "(unnamed)"
}

// SweepAwaitingHost re-asks the host to materialize every job parked on
// awaiting_emacs whose last request has gone unanswered past the re-request
// interval.
//
// IT IS THE ONLY REDELIVERY THE DAEMON OWNS.  A host connect replays the whole
// awaiting_emacs set in its snapshot, but that path belongs to the host: it
// fires when the editor decides to reconnect and never otherwise, so a job
// created while no host was connected was previously unreachable until someone
// restarted Emacs.  Re-requesting on a bounded interval makes materialization a
// thing the daemon keeps asking for rather than a thing it announced once.
//
// A failed re-request is CONTAINED per job: the next job's workspace has nothing
// to do with this one, and the store still holds every one of them.
func (m *Manager) SweepAwaitingHost(ctx context.Context) error {
	jobs, err := m.cfg.Store.List()
	if err != nil {
		return fmt.Errorf("workspace create: list jobs for the awaiting-host sweep: %w", err)
	}
	nowMs := m.now().UnixMilli()
	interval := m.materializationRequestInterval().Milliseconds()
	deadline := m.materializationHeldDeadline().Milliseconds()
	for _, job := range jobs {
		if job.State != StateAwaitingEmacs {
			continue
		}
		// A job persisted before this cadence existed, or one whose park was
		// never stamped, is dated from the first sweep that sees it. Dating it
		// from zero instead would report an age measured from the epoch.
		if job.AwaitingEmacsSinceMs == 0 {
			updated, err := m.cfg.Store.Update(job.ID, func(j *Job) error {
				j.AwaitingEmacsSinceMs = nowMs
				return nil
			})
			if err != nil {
				m.cfg.Logf("workspace-create: awaiting-host sweep could not stamp id=%s error=%v; the job stays held and the next sweep retries", job.ID, err)
				continue
			}
			job = updated
		}
		heldMs := nowMs - job.AwaitingEmacsSinceMs
		if heldMs >= deadline && !job.MaterializationEscalated {
			m.escalateHeldMaterialization(ctx, job, heldMs)
			continue
		}
		// AN ESCALATED JOB STOPS BEING RE-ASKED.  The host has had every
		// request the deadline allows for, the wait is now on the record as a
		// fault, and a workspace nobody is coming back for must not go on
		// republishing forever.  Recovery is still total: a host connect
		// replays every awaiting_emacs job in its snapshot.
		if job.MaterializationEscalated {
			continue
		}
		if job.MaterializationLastRequestMs != 0 && nowMs-job.MaterializationLastRequestMs < interval {
			continue
		}
		available := Available{JobID: job.ID, Name: job.FinalName, Branch: job.Branch, BaseCommit: job.ResolvedBaseCommit, WorktreePath: job.WorktreePath, SessionID: job.SessionID, Request: job.Request}
		m.cfg.Logf("workspace-create: RE-REQUESTING materialization id=%s name=%q worktree=%s session=%s held_ms=%d attempt=%d", job.ID, job.FinalName, job.WorktreePath, job.SessionID, heldMs, job.MaterializationRequests+1)
		if err := m.cfg.Available.PublishWorkspaceAvailable(ctx, available); err != nil {
			m.cfg.Logf("workspace-create: materialization RE-REQUEST FAILED id=%s held_ms=%d error=%v; the job stays held and the next sweep asks again", job.ID, heldMs, err)
			continue
		}
		if _, err := m.cfg.Store.Update(job.ID, func(j *Job) error {
			j.MaterializationRequests++
			j.MaterializationLastRequestMs = nowMs
			return nil
		}); err != nil {
			m.cfg.Logf("workspace-create: materialization re-request checkpoint FAILED id=%s error=%v; the request was sent and the next sweep may repeat it (the host acknowledgement is idempotent)", job.ID, err)
		}
	}
	return nil
}

// EnqueueInteractiveCreate durably accepts one frontend create command before
// any git/session mutation.  REQUESTID is the frontend's idempotency key: a
// reconnect retry gets the same job instead of a second worktree.
func (m *Manager) EnqueueInteractiveCreate(requestID string, request Request) (Job, bool, error) {
	if requestID == "" {
		return Job{}, false, fmt.Errorf("workspace create: interactive request id is required")
	}
	job := Job{
		ID:          "interactive:" + requestID,
		SourceFile:  "interactive",
		SourceIndex: 0,
		Request:     request,
		State:       StateQueued,
	}
	persisted, inserted, err := m.cfg.Store.Enqueue(job)
	if err != nil {
		return Job{}, false, err
	}
	m.cfg.Logf("workspace-create: interactive enqueue request=%s job=%s inserted=%t name=%q", requestID, persisted.ID, inserted, persisted.Request.Name)
	return persisted, inserted, nil
}

// StartInteractiveCreate queues the job durably, then ROUTES it to the creation
// worker.  It does not spawn a goroutine of its own: creation has exactly one
// owner, and a frontend command that started a second one would race the
// worker for the same state machine.  An HTTP/UDS ACK returns immediately
// either way.
func (m *Manager) StartInteractiveCreate(_ context.Context, requestID string, request Request) (Job, bool, error) {
	job, inserted, err := m.EnqueueInteractiveCreate(requestID, request)
	if err != nil || !inserted {
		return job, inserted, err
	}
	m.cfg.Logf("workspace-create: interactive create routed id=%s request=%s", job.ID, requestID)
	m.RouteJob(job.ID)
	return job, true, nil
}

// Process advances one job until it is waiting on Emacs, ready, or failed.
// It never creates an implicit alternate path: every missing prerequisite is
// terminally recorded and returned to the caller.
//
// CALL IT FROM THE CREATION WORKER ONLY.  Every other path routes an id (see
// RouteJob); the per-id running guard below is a second line of defense, not an
// invitation to add owners.
func (m *Manager) Process(ctx context.Context, id string) error {
	m.mu.Lock()
	if m.running[id] {
		m.mu.Unlock()
		m.cfg.Logf("workspace-create: process already active id=%s; leaving durable worker to finish", id)
		return nil
	}
	m.running[id] = true
	m.mu.Unlock()
	defer func() {
		m.mu.Lock()
		delete(m.running, id)
		m.mu.Unlock()
	}()
	return m.process(ctx, id)
}

func (m *Manager) process(ctx context.Context, id string) error {
	for {
		job, ok, err := m.cfg.Store.Get(id)
		if err != nil {
			return err
		}
		if !ok {
			return fmt.Errorf("workspace create: process unknown job %q", id)
		}
		switch job.State {
		case StateQueued, StateWorktreeCreating:
			if job.State == StateQueued {
				if _, err := m.transition(id, StateWorktreeCreating, ""); err != nil {
					return err
				}
				continue
			}
			// The plan/create/record-geometry sequence is SHARED with the
			// one-off `create-workspace` command through WorktreeStage, so a
			// command-line workspace resolves its name, branch and base
			// exactly as a daemon-created one does and carries the same merge
			// geometry. The checkpoint below is this caller's durability
			// boundary: the plan is persisted before any git mutation.
			if _, err := m.worktreeStage().Materialize(ctx, job, func(_ context.Context, result WorktreeResult) (Job, error) {
				return m.cfg.Store.Update(id, func(j *Job) error {
					j.WorktreePath = result.Path
					j.FinalName = result.FinalName
					j.Branch = result.Branch
					j.ResolvedBaseCommit = result.BaseCommit
					j.Request.ForkSessionID = result.ForkSessionID
					j.LastError = ""
					return nil
				})
			}); err != nil {
				var stageErr *StageError
				if errors.As(err, &stageErr) {
					return m.fail(ctx, id, stageErr.Action, stageErr.Err)
				}
				// Anything else came from the store update above, which is
				// structural: nothing about THIS job is wrong.
				return err
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.State = StateWorktreeReady
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
		case StateWorktreeReady, StateSessionCreating:
			if job.State == StateWorktreeReady {
				if resolver, ok := m.cfg.Sessions.(SessionMetadataResolver); ok {
					request, err := resolver.ResolveSessionMetadata(ctx, job)
					if err != nil {
						return m.fail(ctx, id, "resolve session metadata", err)
					}
					if _, err := m.cfg.Store.Update(id, func(j *Job) error {
						j.Request = request
						j.LastError = ""
						return nil
					}); err != nil {
						return err
					}
				}
				if _, err := m.transition(id, StateSessionCreating, ""); err != nil {
					return err
				}
				continue
			}
			m.cfg.Logf("workspace-create: preparing session publication id=%s worktree=%q session=%q", job.ID, job.WorktreePath, job.SessionID)
			if err := m.cfg.Publication.PrepareSessionPublication(ctx, job); err != nil {
				return m.fail(ctx, id, "prepare session publication", err)
			}
			sessionID, err := m.cfg.Sessions.EnsureSession(ctx, job)
			if err != nil {
				return m.fail(ctx, id, "ensure session", err)
			}
			if sessionID == "" {
				return m.fail(ctx, id, "ensure session", fmt.Errorf("creator returned an empty session id"))
			}
			updated, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.SessionID = sessionID
				j.State = StateSessionReady
				j.LastError = ""
				return nil
			})
			if err != nil {
				return err
			}
			m.cfg.Logf("workspace-create: binding session publication id=%s worktree=%q session=%q", updated.ID, updated.WorktreePath, updated.SessionID)
			if err := m.cfg.Publication.PrepareSessionPublication(ctx, updated); err != nil {
				return m.fail(ctx, id, "bind session publication", err)
			}
		case StateSessionReady:
			if err := m.cfg.Health.AwaitHealthy(ctx, job); err != nil {
				return m.fail(ctx, id, "await session health", err)
			}
			if _, err := m.transition(id, StateSessionHealthy, ""); err != nil {
				return err
			}
		case StateSessionHealthy:
			// Persist the visible descriptor before publishing it.  A fast host ACK
			// can arrive synchronously from the publisher; it must see an
			// awaiting-emacs job instead of being rejected as premature.
			parkedMs := m.now().UnixMilli()
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.State = StateAwaitingEmacs
				j.AwaitingEmacsSinceMs = parkedMs
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
			job, _, err = m.cfg.Store.Get(id)
			if err != nil {
				return err
			}
			available := Available{JobID: job.ID, Name: job.FinalName, Branch: job.Branch, BaseCommit: job.ResolvedBaseCommit, WorktreePath: job.WorktreePath, SessionID: job.SessionID, Request: job.Request}
			m.cfg.Logf("workspace-create: publishing available id=%s name=%q branch=%q base=%q worktree=%s session=%s", job.ID, job.FinalName, job.Branch, job.ResolvedBaseCommit, job.WorktreePath, job.SessionID)
			if err := m.cfg.Available.PublishWorkspaceAvailable(ctx, available); err != nil {
				return m.fail(ctx, id, "publish workspace available", err)
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.AvailablePublished = true
				j.MaterializationRequests++
				j.MaterializationLastRequestMs = parkedMs
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
			// A reentrant ACK may have advanced state while Publish returned.  Read
			// it again and continue directly to prompt delivery when that happened.
			continue
		case StateAwaitingEmacs, StateReady, StateFailed:
			return nil
		case StateEmacsMaterialized, StatePromptSubmitting:
			if job.Request.Prompt == "" {
				if _, err := m.transition(id, StateReady, ""); err != nil {
					return err
				}
				return nil
			}
			if job.State == StateEmacsMaterialized {
				if _, err := m.transition(id, StatePromptSubmitting, ""); err != nil {
					return err
				}
				continue
			}
			// Delivery is intentionally at-least-once: if the process dies after
			// the shim accepts this submit and before the following store update,
			// recovery can repeat the prompt.  The inverse (marking it first) can
			// lose a prompt, so it is forbidden.
			m.cfg.Logf("workspace-create: submitting initial prompt id=%s name=%q session=%s prompt_len=%d origin=workspace-create:%s delivery=at-least-once checkpoint=after-shim-ack crash_window=may-duplicate-never-lose", job.ID, job.Request.Name, job.SessionID, len(job.Request.Prompt), job.ID)
			// A FAILED SUBMIT CAN LEAVE A TURN CLAIM OPEN, and this job does NOT
			// close it. Delivery is at-least-once, so the shim may have accepted
			// the prompt and opened `workspace-create:<id>` before the error came
			// back, and nothing here would ever observe that turn's end.
			//
			// The edge was considered and deliberately not taken. Its honest form
			// is the FAILURE edge only — reaching `StateReady` is the instant the
			// initial prompt's turn legitimately BEGINS, so a close on settle
			// would kill live work — which leaves one call site to justify a new
			// port through `InitialPromptSubmitter`. The coverage that actually
			// closes the observed leak lives in the shim instead: its
			// turn-liveness watchdog (`agent-shim/claude/shim/src/uds/uds-session.ts`,
			// `checkTurnQuiet`) writes the terminal for ANY turn whose SDK query
			// died without producing one, whatever opened it. That is what
			// retired the `workspace-create:…:0` claim observed open for four
			// hours, and it needs no per-origin port at all.
			if err := m.cfg.Prompts.SubmitInitialPrompt(ctx, job); err != nil {
				return m.fail(ctx, id, "submit initial prompt", err)
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.PromptDelivered = true
				j.State = StateReady
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
			m.cfg.Logf("workspace-create: checkpointed initial prompt delivery id=%s session=%s", job.ID, job.SessionID)
			return nil
		default:
			return m.fail(ctx, id, "read job state", fmt.Errorf("unknown state %q", job.State))
		}
	}
}

// MarkMaterialized records Emacs' acknowledgement and ROUTES the job onward.
// Calling it repeatedly is safe: a replayed ACK cannot resubmit an
// already-delivered initial prompt.  The ACK's own transition is durable before
// this returns; advancing past it belongs to the creation worker, so a frontend
// command is never the thread that submits an initial prompt.
func (m *Manager) MarkMaterialized(ctx context.Context, id string) error {
	job, ok, err := m.cfg.Store.Get(id)
	if err != nil {
		return err
	}
	if !ok {
		return fmt.Errorf("workspace create: materialization ack for unknown job %q", id)
	}
	if job.State == StateAwaitingEmacs {
		if _, err := m.cfg.Store.Update(id, func(j *Job) error {
			j.State = StateEmacsMaterialized
			j.Materialized = true
			j.LastError = ""
			return nil
		}); err != nil {
			return err
		}
	} else if job.State != StateEmacsMaterialized && job.State != StatePromptSubmitting && job.State != StateReady && job.State != StateFailed {
		return fmt.Errorf("workspace create: materialization ack for job %q in state %s", id, job.State)
	}
	job, ok, err = m.cfg.Store.Get(id)
	if err != nil {
		return err
	}
	if !ok {
		return fmt.Errorf("workspace create: materialization ack lost job %q after transition", id)
	}
	if !job.Materialized {
		return fmt.Errorf("workspace create: materialization ack for job %q did not persist the publication latch", id)
	}
	if !job.PublicationReleased {
		decision := PublicationDecision{JobID: job.ID, WorktreePath: job.WorktreePath, SessionID: job.SessionID, Materialized: true}
		m.cfg.Logf("workspace-create: releasing session publication id=%s worktree=%q session=%q state=%s prompt_len=%d", decision.JobID, decision.WorktreePath, decision.SessionID, job.State, len(job.Request.Prompt))
		if err := m.cfg.Releases.ReleaseSessionPublication(ctx, decision); err != nil {
			return fmt.Errorf("workspace create: release session publication id=%s worktree=%q session=%q: %w", decision.JobID, decision.WorktreePath, decision.SessionID, err)
		}
		if _, err := m.cfg.Store.Update(id, func(j *Job) error {
			j.PublicationReleased = true
			return nil
		}); err != nil {
			return err
		}
		m.cfg.Logf("workspace-create: checkpointed session publication release id=%s worktree=%q session=%q", decision.JobID, decision.WorktreePath, decision.SessionID)
	}
	if job.State == StateReady || job.State == StateFailed {
		return nil
	}
	return m.Process(ctx, id)
}

// DrainHostActions asks the host to apply every persisted UI-only action.  The
// sink deduplicates by action ID because the action is deliberately marked only
// after its publish returns successfully.
func (m *Manager) DrainHostActions(ctx context.Context) error {
	actions, err := m.cfg.Store.HostActionsForDelivery()
	if err != nil {
		return err
	}
	for _, action := range actions {
		m.cfg.Logf("workspace-create: publishing host action id=%s type=%s", action.ID, action.Type)
		if err := m.cfg.HostActions.PublishHostAction(ctx, action); err != nil {
			return fmt.Errorf("workspace create: publish host action %s: %w", action.ID, err)
		}
		if err := m.cfg.Store.MarkHostActionPublished(action.ID); err != nil {
			return err
		}
	}
	return nil
}

// CompleteHostAction records the host's terminal verdict. ANY verdict — ok or
// failure — releases the action from redelivery and from the reconnect
// snapshot; a failure keeps its diagnostic text durably and is logged loudly.
// Completion is idempotent.
func (m *Manager) CompleteHostAction(id string, ok bool, failure string) error {
	if err := m.cfg.Store.CompleteHostAction(id, ok, failure); err != nil {
		return err
	}
	m.cfg.Logf("workspace-create: completed host action id=%s ok=%t failure=%q", id, ok, failure)
	return nil
}

// worktreeStage binds the manager's configured collaborators into the shared
// worktree stage. It is built per call rather than cached because Config is
// immutable after NewManager and the value is three interface words wide.
func (m *Manager) worktreeStage() WorktreeStage {
	return WorktreeStage{Planner: m.cfg.Planner, Worktrees: m.cfg.Worktrees, Geometry: m.cfg.Geometry, Logf: m.cfg.Logf}
}

func (m *Manager) transition(id string, state JobState, lastError string) (Job, error) {
	return m.cfg.Store.Update(id, func(j *Job) error {
		j.State = state
		j.LastError = lastError
		return nil
	})
}

// SurfaceBootSweepVerdict hands ONE boot-sweep verdict to the host as a
// retained action.  It is the jobless sibling of surfaceFailure: no creation
// job produced it, so it carries no source file and no job id, but it rides the
// identical envelope — persisted before delivery, replayed on reconnect, and
// released only by the host's acknowledgement.
//
// THE ID IS THE DEDUPLICATOR AND IT IS DELIBERATELY STABLE.  Keying on the
// session and the verdict means a daemon restarted twice over an unchanged
// situation does not nag twice about the same conclusion, while a DIFFERENT
// conclusion about the same session — the probe that failed last boot now says
// the shim is simply gone — is a different action and is delivered.  This
// matches the SSM half exactly, which no-ops on an already-standing verdict for
// the same reason.
func (m *Manager) SurfaceBootSweepVerdict(ctx context.Context, workspace, sessionID, verdict, reason string) error {
	switch {
	case workspace == "":
		return fmt.Errorf("workspace create: boot-sweep verdict %q needs a workspace", verdict)
	case sessionID == "":
		return fmt.Errorf("workspace create: boot-sweep verdict %q for workspace %q needs a session id", verdict, workspace)
	case verdict == "":
		return fmt.Errorf("workspace create: boot-sweep verdict for workspace %q session %q is empty", workspace, sessionID)
	case reason == "":
		return fmt.Errorf("workspace create: boot-sweep verdict %q for workspace %q session %q has no display reason; the host renders it verbatim and has nothing to render",
			verdict, workspace, sessionID)
	}
	payload, err := json.Marshal(BootSweepSessionUnwired{Workspace: workspace, SessionID: sessionID, Verdict: verdict, Reason: reason})
	if err != nil {
		return fmt.Errorf("workspace create: marshal boot-sweep verdict %q for workspace %q session %q: %w", verdict, workspace, sessionID, err)
	}
	notice := HostAction{
		ID:      fmt.Sprintf("boot-sweep:%s:%s", sessionID, verdict),
		Type:    HostActionTypeBootSweepSessionUnwired,
		Payload: payload,
	}
	_, inserted, err := m.cfg.Store.EnqueueHostAction(notice)
	if err != nil {
		return fmt.Errorf("workspace create: enqueue boot-sweep verdict host action %s: %w", notice.ID, err)
	}
	if !inserted {
		m.cfg.Logf("workspace-create: BOOT-SWEEP VERDICT ALREADY RECORDED host_action=%s ws=%s session=%s verdict=%s — the same conclusion is already standing against this session, so it is not re-announced",
			notice.ID, workspace, sessionID, verdict)
		return nil
	}
	m.cfg.Logf("workspace-create: BOOT-SWEEP VERDICT SURFACED host_action=%s ws=%s session=%s verdict=%s reason=%q", notice.ID, workspace, sessionID, verdict, reason)
	if err := m.DrainHostActions(ctx); err != nil {
		// NOT LOST, and not an error to the caller: the action is persisted and
		// the next drain or reconnect snapshot delivers it. The undelivered
		// attempt is still recorded, because a host that never comes back is a
		// verdict nobody reads and the log is the only place that shows it.
		m.cfg.Logf("workspace-create: boot-sweep verdict not delivered yet host_action=%s ws=%s session=%s verdict=%s error=%v retained=yes",
			notice.ID, workspace, sessionID, verdict, err)
	}
	return nil
}

// fail records one job's terminal failure durably, logs it loudly, and hands
// it to the host as a retained HostAction.  The returned error wraps
// ErrJobFailed so callers can step over a contained job failure without
// mistaking it for a structural one; a failure to RECORD the failure is
// structural and deliberately does not carry the sentinel.
func (m *Manager) fail(ctx context.Context, id, action string, cause error) error {
	err := fmt.Errorf("workspace create: job %s %s: %w: %w", id, action, ErrJobFailed, cause)
	m.cfg.Logf("workspace-create: FAILED id=%s action=%s error=%v", id, action, cause)
	if _, updateErr := m.transition(id, StateFailed, err.Error()); updateErr != nil {
		return fmt.Errorf("workspace create: job %s %s: %w (and record failure: %v)", id, action, cause, updateErr)
	}
	m.surfaceFailure(ctx, id, action, cause)
	return err
}

// surfaceFailure enqueues the durable host notification for a failed job.  A
// publish that cannot reach a host right now is not lost: the action stays
// pending in the store and the next Resume/DrainHostActions delivers it, and
// the reconnect snapshot carries it regardless.
func (m *Manager) surfaceFailure(ctx context.Context, id, action string, cause error) {
	job, ok, err := m.cfg.Store.Get(id)
	if err != nil || !ok {
		m.cfg.Logf("workspace-create: FAILURE NOT SURFACED id=%s action=%s reason=job-unreadable error=%v found=%t", id, action, err, ok)
		return
	}
	payload, err := json.Marshal(WorkspaceCreateFailure{JobID: id, RequestedName: job.Request.Name, Error: fmt.Sprintf("%s: %v", action, cause)})
	if err != nil {
		m.cfg.Logf("workspace-create: FAILURE NOT SURFACED id=%s action=%s reason=payload error=%v", id, action, err)
		return
	}
	notice := HostAction{ID: id + ":failed", SourceFile: job.SourceFile, SourceIndex: job.SourceIndex, Type: HostActionTypeWorkspaceCreateFailed, Payload: payload}
	if _, _, err := m.cfg.Store.EnqueueHostAction(notice); err != nil {
		m.cfg.Logf("workspace-create: FAILURE NOT SURFACED id=%s action=%s reason=enqueue error=%v", id, action, err)
		return
	}
	m.cfg.Logf("workspace-create: FAILURE SURFACED id=%s action=%s host_action=%s name=%q", id, action, notice.ID, job.Request.Name)
	if err := m.DrainHostActions(ctx); err != nil {
		m.cfg.Logf("workspace-create: failure notice not delivered yet id=%s host_action=%s error=%v retained=yes", id, notice.ID, err)
	}
}
