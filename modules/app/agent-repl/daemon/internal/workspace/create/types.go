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

// HostActionTypeBootSweepSessionUnwired is the daemon-minted host action that
// carries ONE boot-sweep verdict to the Emacs host.  Like the failure notice
// above it never originates from a command file: the inbox rejects unknown
// command types, so only the daemon's own boot reconciliation can produce it.
//
// It lives in this package because this package owns the retained-until-
// completed host-action envelope — the durable store, the delivery drain, the
// reconnect snapshot and the acknowledgement — and a second, parallel envelope
// for one more daemon-minted notice would be a second owner of one mechanism.
const HostActionTypeBootSweepSessionUnwired = "boot-sweep-session-unwired"

// BootSweepSessionUnwired is the payload of a
// HostActionTypeBootSweepSessionUnwired action.  Reason is a display-ready
// sentence composed by the sweep and rendered verbatim by the host.
type BootSweepSessionUnwired struct {
	Workspace string `json:"workspace"`
	SessionID string `json:"session_id"`
	Verdict   string `json:"verdict"`
	Reason    string `json:"reason"`
}

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
	Name            string   `json:"name"`
	GitRoot         string   `json:"git_root"`
	Prompt          string   `json:"prompt,omitempty"`
	Priority        Priority `json:"priority,omitempty"`
	ForkFrom        string   `json:"fork_from,omitempty"`
	ForkSessionID   string   `json:"fork_session_id,omitempty"`
	SourceWorkspace string   `json:"source_workspace,omitempty"`
	SourceDir       string   `json:"source_dir,omitempty"`
	BaseCommit      string   `json:"base_commit,omitempty"`
	Model           string   `json:"model,omitempty"`
	// ConfigDir is the account this workspace RESOLVED to — a derived value the
	// daemon computes, never something an emitter may choose. Anything a
	// command file puts here is overwritten.
	ConfigDir string `json:"config_dir,omitempty"`
	// ConfigDirOverride is an account SELECTION this workspace inherited from
	// the source workspace it was created from, empty when it inherited none.
	// It is persisted so a restart resumes the create with the same selection,
	// and it travels onto the new session's record so the choice keeps
	// following that workspace's own children.
	ConfigDirOverride    string          `json:"config_dir_override,omitempty"`
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
	// PublicationAbandoned is the TERMINAL DISPOSITION of a hold that will
	// never be released.
	//
	// The publication gate holds a worktree's session frames until its creation
	// job reports Materialized, and "not materialized" used to be the only other
	// answer the gate could give — a WAIT, forever, with no state that could end
	// it. A job that died before it ever published its WorkspaceAvailable never
	// reaches the host, so its Materialized latch can never be written by
	// anything: the wait had no terminating event at all. Five such jobs held
	// their worktrees across a three-hour daemon lifetime and produced 43550
	// hold records, one per gated frame per snapshot, while the workspaces they
	// gated rendered blank.
	//
	// Abandonment is that missing terminal state. It DESTROYS NOTHING — the
	// worktree, the branch and the session are all real and all still the
	// user's — it only records that no acknowledgement is coming, so the gate
	// stops holding frames for a workspace nobody will ever materialize. It is
	// durable because the conclusion outlives the daemon and must be reported
	// exactly once, and it is written only against a job that is already
	// terminal or whose worktree is already gone.
	PublicationAbandoned bool `json:"publication_abandoned,omitempty"`
	// PublicationAbandonedReason names WHY the hold was abandoned, so the
	// durable record answers the question the log line answered at the instant
	// it happened.
	PublicationAbandonedReason string `json:"publication_abandoned_reason,omitempty"`
}

// PublicationAbandonReason names one terminal disposition of a held session
// publication. Each is a different fault with a different remedy, so they are
// never reported in the same words.
type PublicationAbandonReason string

const (
	// AbandonTerminalFailure is a creation job that reached StateFailed without
	// ever being materialized. Nothing will ever acknowledge it.
	AbandonTerminalFailure PublicationAbandonReason = "terminal_failure_before_materialization"
	// AbandonWorktreeGone is a job still parked on the host whose worktree no
	// longer exists on disk. Materializing it would ask the editor to render a
	// directory that is not there.
	AbandonWorktreeGone PublicationAbandonReason = "worktree_no_longer_exists"
)

// publicationAbandonable reports a job whose hold has a terminal disposition
// available RIGHT NOW, and names it.
//
// A job with no worktree path is deliberately not abandonable: it holds nothing,
// because the gate is keyed by worktree and has nothing to match it against.
// An already-materialized job is not abandonable either — its gate is open, so
// there is no hold to dispose of.
func publicationAbandonable(job Job, worktreeExists func(string) bool) (PublicationAbandonReason, bool) {
	if job.WorktreePath == "" || job.Materialized || job.PublicationAbandoned {
		return "", false
	}
	if job.State == StateFailed {
		return AbandonTerminalFailure, true
	}
	// A LIVENESS CHECK, and it is what keeps boot replay from resurrecting a
	// dead job: every awaiting_emacs job rides the host's connect snapshot, so
	// a job whose worktree was deleted while the daemon was down would be
	// re-requested on every host connect for the rest of time.
	if job.State == StateAwaitingEmacs && worktreeExists != nil && !worktreeExists(job.WorktreePath) {
		return AbandonWorktreeGone, true
	}
	return "", false
}

// PublicationDecision is the durable creation job's verdict about whether a
// session-scoped frontend frame may be emitted.  A matching job is the sole
// authority for a newly-created workspace, so a session cannot publish before
// its WorkspaceMaterialized acknowledgement has been checkpointed.
// SessionID IS THE JOB'S OWN SESSION, never the session id the asking frame
// carried. The distinction is the whole reason a workspace once held its
// frames forever: the gate's caller is any frame for the worktree, including
// frames that arrive before the creation job has a session at all (session
// id ""), and a decision that echoed the ASKING frame's id got memoized under
// that empty value. The release then compared the job's real session against
// the memoized "" , found them unequal, refused to open the gate, and left a
// permanently-closed hold in front of a workspace the durable store had
// already marked materialized. Sourcing the field from the matched job makes
// that mismatch unrepresentable rather than merely unlikely.
type PublicationDecision struct {
	JobID        string
	WorktreePath string
	SessionID    string
	Materialized bool
	// Abandoned reports that Materialized is true because the hold was given a
	// TERMINAL DISPOSITION rather than because the host ever acknowledged the
	// workspace. The frames pass either way — the difference is what the daemon
	// is entitled to say about the workspace, and a gate that could not tell
	// the two apart would report an abandoned creation as a successful one.
	Abandoned bool
	// AbandonedReason carries the durable reason behind Abandoned.
	AbandonedReason string
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
			if publicationHeld(*candidate) {
				unmaterialized = append(unmaterialized, candidate)
			}
		}
		switch len(unmaterialized) {
		case 0:
			return PublicationDecision{WorktreePath: worktreePath, Materialized: true}, nil
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
		return PublicationDecision{WorktreePath: worktreePath, Materialized: true}, nil
	}
	return PublicationDecision{
		JobID:           match.ID,
		WorktreePath:    worktreePath,
		SessionID:       match.SessionID,
		Materialized:    !publicationHeld(*match),
		Abandoned:       match.PublicationAbandoned,
		AbandonedReason: match.PublicationAbandonedReason,
	}, nil
}

// publicationHeld reports a job whose worktree's session frames are still
// gated. A hold ends in exactly two ways — the host acknowledges the workspace,
// or the daemon abandons the wait — and naming both here is what keeps every
// reader of the gate agreeing on when a hold is over.
func publicationHeld(job Job) bool {
	return !job.Materialized && !job.PublicationAbandoned
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
