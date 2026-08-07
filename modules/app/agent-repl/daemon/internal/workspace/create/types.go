// Package create owns durable agent-repl workspace creation.  It accepts the
// command files emitted by skills, persists each command before acknowledging
// its file, and materializes a worktree/session/shim before asking a host UI
// to render the workspace.
package create

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
)

// ErrJobFailed marks an error that has ALREADY been recorded durably against
// one job.  It is the single classification line between a failure that
// belongs to a job and a failure that belongs to the subsystem: a job-level
// failure is contained (recorded, logged, surfaced to the host, and stepped
// over), while anything not wrapping this sentinel — an unreadable inbox
// directory, a broken JobStore — is structural and keeps propagating.
var ErrJobFailed = errors.New("workspace create: job failed")

// HostActionTypeWorkspaceCreateFailed is the daemon-minted host action that
// carries a durably-failed job to the Emacs host.  It never originates from a
// command file: the inbox rejects unknown command types, so this type can only
// be produced by the manager's own failure path.
const HostActionTypeWorkspaceCreateFailed = "workspace-create-failed"

// WorkspaceCreateFailure is the payload of a HostActionTypeWorkspaceCreateFailed
// action.  It names the job and the error so the host can say exactly which
// creation died and why.
type WorkspaceCreateFailure struct {
	JobID         string `json:"job_id"`
	RequestedName string `json:"requested_name"`
	Error         string `json:"error"`
}

// JobState is the durable lifecycle of one create command.  State is persisted
// before every effect so a restarted daemon can resume through idempotent
// collaborators instead of guessing what the previous process did.
type JobState string

const (
	StateQueued            JobState = "queued"
	StateWorktreeCreating  JobState = "worktree_creating"
	StateWorktreeReady     JobState = "worktree_ready"
	StateSessionCreating   JobState = "session_creating"
	StateSessionReady      JobState = "session_ready"
	StateSessionHealthy    JobState = "session_healthy"
	StateAwaitingEmacs     JobState = "awaiting_emacs"
	StateEmacsMaterialized JobState = "emacs_materialized"
	StatePromptSubmitting  JobState = "prompt_submitting"
	StateReady             JobState = "ready"
	StateFailed            JobState = "failed"
)

// Priority is a workspace priority label ("p05", "p1", "p2", "p3").  It
// decodes from either a JSON string or a bare JSON number, because the
// command-file writers (the Emacs host and the generation skill) have both
// spellings in the wild, and it always re-encodes as a string.  Holding it
// decoded is what lets the daemon hand the label to the host verbatim: a
// `json.RawMessage' would carry the quotes into `WorkspaceAvailable.priority'
// and the host would look up an image named "\"p1\"".
type Priority string

func (p *Priority) UnmarshalJSON(data []byte) error {
	if string(data) == "null" {
		*p = ""
		return nil
	}
	var s string
	if err := json.Unmarshal(data, &s); err == nil {
		*p = Priority(s)
		return nil
	}
	var n json.Number
	if err := json.Unmarshal(data, &n); err != nil {
		return fmt.Errorf("workspace create: priority must be a string or a number, got %s", data)
	}
	*p = Priority(n.String())
	return nil
}

// Request is the complete creation payload.  The fields mirror the current
// create-or-update-workspace JSON contract; Extra retains new skill metadata
// until a dedicated daemon consumer is introduced rather than dropping it.
type Request struct {
	Name                 string          `json:"name"`
	GitRoot              string          `json:"git_root"`
	Prompt               string          `json:"prompt,omitempty"`
	Priority             Priority        `json:"priority,omitempty"`
	ForkFrom             string          `json:"fork_from,omitempty"`
	ForkSessionID        string          `json:"fork_session_id,omitempty"`
	SourceWorkspace      string          `json:"source_workspace,omitempty"`
	SourceDir            string          `json:"source_dir,omitempty"`
	BaseCommit           string          `json:"base_commit,omitempty"`
	Model                string          `json:"model,omitempty"`
	ConfigDir            string          `json:"config_dir,omitempty"`
	PermissionMode       string          `json:"permission_mode,omitempty"`
	AllowUngated         bool            `json:"allow_ungated,omitempty"`
	PostprocessingPrompt string          `json:"postprocessing_prompt,omitempty"`
	BeforeWSMerge        string          `json:"before_ws_merge,omitempty"`
	Extra                json.RawMessage `json:"extra,omitempty"`
}

func (r Request) validate() error {
	if r.Name == "" {
		return fmt.Errorf("workspace create: name is required")
	}
	if r.GitRoot == "" {
		return fmt.Errorf("workspace create %q: git_root is required", r.Name)
	}
	return nil
}

// Job is the durable record for one create array element.  ID is derived from
// its command-file UUID and array position, making inbox ingestion idempotent.
type Job struct {
	ID                 string   `json:"id"`
	SourceFile         string   `json:"source_file"`
	SourceIndex        int      `json:"source_index"`
	Request            Request  `json:"request"`
	State              JobState `json:"state"`
	WorktreePath       string   `json:"worktree_path,omitempty"`
	FinalName          string   `json:"final_name,omitempty"`
	Branch             string   `json:"branch,omitempty"`
	ResolvedBaseCommit string   `json:"resolved_base_commit,omitempty"`
	SessionID          string   `json:"session_id,omitempty"`
	AvailablePublished bool     `json:"available_published,omitempty"`
	// Materialized is the durable host acknowledgement that releases every
	// session-scoped frontend publication for this job.  It is deliberately a
	// fact separate from State: prompt delivery can advance or fail after the
	// acknowledgement, but neither outcome may re-close the publication gate.
	Materialized        bool   `json:"materialized,omitempty"`
	PublicationReleased bool   `json:"publication_released,omitempty"`
	PromptDelivered     bool   `json:"prompt_delivered,omitempty"`
	LastError           string `json:"last_error,omitempty"`
	// AwaitingEmacsSinceMs is when this job first parked on the host, unix
	// millis.  It is the clock the re-request cadence and the held-job
	// escalation are both measured against, and it is DURABLE because the wait
	// outlives the daemon: a bounce mid-wait must not reset a job's age and hide
	// how long the user has been staring at a workspace that never appeared.
	AwaitingEmacsSinceMs int64 `json:"awaiting_emacs_since_ms,omitempty"`
	// MaterializationRequests counts how many times the daemon has asked the
	// host to materialize this workspace, and
	// MaterializationLastRequestMs when it last asked.  Together they are the
	// re-request cadence's whole state: an unanswered request is re-sent on a
	// bounded interval rather than once, because the single original request is
	// dropped outright when no host client is connected to receive it.
	MaterializationRequests      int   `json:"materialization_requests,omitempty"`
	MaterializationLastRequestMs int64 `json:"materialization_last_request_ms,omitempty"`
	// MaterializationEscalated latches the one report the daemon makes about a
	// wait that ran past its deadline.  It is durable so a restart cannot
	// re-report an escalation the user has already been shown, and an
	// escalation that repeats every sweep is one nobody reads.
	MaterializationEscalated bool `json:"materialization_escalated,omitempty"`
}

// PublicationDecision is the durable creation job's verdict about whether a
// session-scoped frontend frame may be emitted.  A matching job is the sole
// authority for a newly-created workspace, so a session cannot publish before
// its WorkspaceMaterialized acknowledgement has been checkpointed.
type PublicationDecision struct {
	JobID        string
	WorktreePath string
	SessionID    string
	Materialized bool
}

// SessionPublicationDecision returns the durable publication verdict for one
// workspace/session pair.  No matching create job means the session was not
// created through this pending-materialization lifecycle and is publishable.
// More than one matching job is an invariant violation: one live session must
// have exactly one creation job identity.
func SessionPublicationDecision(store JobStore, worktreePath, sessionID string) (PublicationDecision, error) {
	if store == nil {
		return PublicationDecision{}, fmt.Errorf("workspace create: session publication gate needs a job store")
	}
	if worktreePath == "" {
		return PublicationDecision{}, fmt.Errorf("workspace create: session publication gate needs a worktree path")
	}
	jobs, err := store.List()
	if err != nil {
		return PublicationDecision{}, fmt.Errorf("workspace create: read publication gate jobs for worktree=%q session=%q: %w", worktreePath, sessionID, err)
	}
	var matches []*Job
	for i := range jobs {
		job := &jobs[i]
		if job.WorktreePath != worktreePath {
			continue
		}
		matches = append(matches, job)
	}
	var match *Job
	for _, candidate := range matches {
		if candidate.SessionID == sessionID {
			match = candidate
			break
		}
	}
	if match == nil && len(matches) == 1 {
		match = matches[0]
	}
	if match == nil {
		unmaterialized := make([]*Job, 0, len(matches))
		for _, candidate := range matches {
			if !candidate.Materialized {
				unmaterialized = append(unmaterialized, candidate)
			}
		}
		switch len(unmaterialized) {
		case 0:
			return PublicationDecision{WorktreePath: worktreePath, SessionID: sessionID, Materialized: true}, nil
		case 1:
			match = unmaterialized[0]
		default:
			ids := make([]string, 0, len(unmaterialized))
			for _, candidate := range unmaterialized {
				ids = append(ids, candidate.ID)
			}
			return PublicationDecision{}, fmt.Errorf("workspace create: publication gate cannot resolve worktree=%q frame_session=%q across unmaterialized_job_ids=%q", worktreePath, sessionID, ids)
		}
	}
	if match == nil {
		return PublicationDecision{WorktreePath: worktreePath, SessionID: sessionID, Materialized: true}, nil
	}
	return PublicationDecision{JobID: match.ID, WorktreePath: worktreePath, SessionID: sessionID, Materialized: match.Materialized}, nil
}

func (j Job) validate() error {
	if j.ID == "" {
		return fmt.Errorf("workspace create: job id is required")
	}
	if err := j.Request.validate(); err != nil {
		return err
	}
	return nil
}

// Available is the daemon's authoritative descriptor for a workspace which
// can now be rendered.  Publisher implementations must deduplicate by JobID:
// a crash after the frame reaches Emacs but before its durable state update is
// an at-least-once delivery.
type Available struct {
	JobID        string
	Name         string
	Branch       string
	BaseCommit   string
	WorktreePath string
	SessionID    string
	Request      Request
}

// WorktreeResult is the authoritative identity chosen by the daemon.  A
// requested name may collide, so the resolved name and branch must be durable
// facts rather than an unrecorded choice inside a git adapter.
type WorktreeResult struct {
	Path          string
	FinalName     string
	Branch        string
	BaseCommit    string
	ForkSessionID string
}

// HostAction preserves a non-create command for the host frontend.  It is
// opaque on purpose: UI-only semantics stay out of workspace materialization.
type HostAction struct {
	ID          string          `json:"id"`
	SourceFile  string          `json:"source_file"`
	SourceIndex int             `json:"source_index"`
	Type        string          `json:"type"`
	Payload     json.RawMessage `json:"payload"`
	Published   bool            `json:"published,omitempty"`
	Completed   bool            `json:"completed,omitempty"`
	Failure     string          `json:"failure,omitempty"`
}

// WorktreePlanner resolves an immutable worktree identity before any git
// mutation.  The manager checkpoints its result before asking WorktreeCreator
// to add the worktree, so a crash cannot turn the job's own branch/path into a
// fresh collision on recovery.
type WorktreePlanner interface {
	PlanWorktree(context.Context, Job) (WorktreeResult, error)
}

// WorktreeCreator must make the job's already-persisted worktree identity
// exist.  It receives the job ID so real adapters can make the git operation
// idempotent across a crash after `git worktree add` succeeds.
type WorktreeCreator interface {
	EnsureWorktree(context.Context, Job) error
}

// WorkspaceGeometryRecorder durably records the created workspace's MERGE
// GEOMETRY — its branch, its own worktree, and the worktree its commits will
// later land in.
//
// It runs at the worktree stage on purpose: that is the single moment the three
// coordinates are OBSERVED FACTS rather than a later reconstruction. Emacs used
// to carry them on every merge command, and two owners of one map is how a
// merge landed against a target the daemon had never heard of.
//
// It receives the whole Job because path spelling and the parent-worktree
// choice are the adapter's business, exactly as they are for WorktreeCreator.
// A recording failure FAILS THE JOB: a workspace materialized without geometry
// is a workspace nobody can ever merge, and discovering that at merge time is
// strictly worse than discovering it at creation.
type WorkspaceGeometryRecorder interface {
	RecordWorkspaceGeometry(context.Context, Job) error
}

// SessionCreator registers the session and starts its waiting shim.  Every
// create job uses it, including jobs with no initial prompt.
type SessionCreator interface {
	EnsureSession(context.Context, Job) (string, error)
}

// SessionMetadataResolver derives account/permission metadata from an
// explicitly nominated live source workspace.  The manager checkpoints the
// returned request before CreateSession, so a restart releases the prompt and
// advertises exactly the metadata that created the session.
type SessionMetadataResolver interface {
	ResolveSessionMetadata(context.Context, Job) (Request, error)
}

// SessionHealthChecker verifies the complete daemon-to-shim health chain.
type SessionHealthChecker interface {
	AwaitHealthy(context.Context, Job) error
}

// InitialPromptSubmitter submits a job's held initial prompt after Emacs has
// acknowledged materialization.  Job.ID must be carried into the submit's
// origin for diagnosis. Delivery is at-least-once: a crash after the shim has
// accepted a prompt but before PromptDelivered is checkpointed can repeat it,
// but the manager must never checkpoint delivery before the submit succeeds.
type InitialPromptSubmitter interface {
	SubmitInitialPrompt(context.Context, Job) error
}

// WorkspaceAvailablePublisher delivers an available descriptor to the Emacs
// host channel.  It is intentionally independent of frontend/server.
type WorkspaceAvailablePublisher interface {
	PublishWorkspaceAvailable(context.Context, Available) error
}

// SessionPublicationReleaser is notified after the host materialization
// acknowledgement is durably checkpointed and before initial-prompt delivery
// may begin.  Its publication is at-least-once for the same crash boundary as
// WorkspaceAvailable: an uncheckpointed notification is retried by an ACK.
type SessionPublicationReleaser interface {
	ReleaseSessionPublication(context.Context, PublicationDecision) error
}

// SessionPublicationPreparer invalidates any historical in-memory gate verdict
// before a new session can emit startup state for this durable worktree job.
type SessionPublicationPreparer interface {
	PrepareSessionPublication(context.Context, Job) error
}

// HostActionSink delivers durable UI-only command records to the host.  It
// must deduplicate by action ID for the same crash boundary as available.
type HostActionSink interface {
	PublishHostAction(context.Context, HostAction) error
}
