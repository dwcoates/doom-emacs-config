// Package create owns durable agent-repl workspace creation.  It accepts the
// command files emitted by skills, persists each command before acknowledging
// its file, and materializes a worktree/session/shim before asking a host UI
// to render the workspace.
package create

import (
	"context"
	"encoding/json"
	"fmt"
)

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

// Request is the complete creation payload.  The fields mirror the current
// create-or-update-workspace JSON contract; Extra retains new skill metadata
// until a dedicated daemon consumer is introduced rather than dropping it.
type Request struct {
	Name                 string          `json:"name"`
	GitRoot              string          `json:"git_root"`
	Prompt               string          `json:"prompt,omitempty"`
	Priority             json.RawMessage `json:"priority,omitempty"`
	ForkFrom             string          `json:"fork_from,omitempty"`
	BaseCommit           string          `json:"base_commit,omitempty"`
	Model                string          `json:"model,omitempty"`
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
	SessionID          string   `json:"session_id,omitempty"`
	AvailablePublished bool     `json:"available_published,omitempty"`
	PromptDelivered    bool     `json:"prompt_delivered,omitempty"`
	LastError          string   `json:"last_error,omitempty"`
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
	WorktreePath string
	SessionID    string
	Request      Request
}

// HostAction preserves a non-create command for the host frontend.  It is
// opaque on purpose: UI-only semantics stay out of workspace materialization.
type HostAction struct {
	ID          string          `json:"id"`
	SourceFile  string          `json:"source_file"`
	SourceIndex int             `json:"source_index"`
	Type        string          `json:"type"`
	Payload     json.RawMessage `json:"payload"`
	Delivered   bool            `json:"delivered,omitempty"`
}

// WorktreeCreator must make the requested worktree exist.  It receives the
// job ID so real adapters can make this operation idempotent across a crash.
type WorktreeCreator interface {
	EnsureWorktree(context.Context, Job) (string, error)
}

// SessionCreator registers the session and starts its waiting shim.  Every
// create job uses it, including jobs with no initial prompt.
type SessionCreator interface {
	EnsureSession(context.Context, Job) (string, error)
}

// SessionHealthChecker verifies the complete daemon-to-shim health chain.
type SessionHealthChecker interface {
	AwaitHealthy(context.Context, Job) error
}

// InitialPromptSubmitter submits a job's held initial prompt after Emacs has
// acknowledged materialization.  Implementations deduplicate by Job.ID.
type InitialPromptSubmitter interface {
	SubmitInitialPrompt(context.Context, Job) error
}

// WorkspaceAvailablePublisher delivers an available descriptor to the Emacs
// host channel.  It is intentionally independent of frontend/server.
type WorkspaceAvailablePublisher interface {
	PublishWorkspaceAvailable(context.Context, Available) error
}

// HostActionSink delivers durable UI-only command records to the host.  It
// must deduplicate by action ID for the same crash boundary as available.
type HostActionSink interface {
	PublishHostAction(context.Context, HostAction) error
}
