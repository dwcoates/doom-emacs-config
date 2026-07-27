package create

import (
	"context"
	"fmt"
)

// Config supplies the daemon-owned creation collaborators.  They are all
// interfaces because the manager is orchestration, not git, shim, or frontend
// plumbing; the daemon stitch point supplies concrete adapters later.
type Config struct {
	Store       JobStore
	Worktrees   WorktreeCreator
	Sessions    SessionCreator
	Health      SessionHealthChecker
	Prompts     InitialPromptSubmitter
	Available   WorkspaceAvailablePublisher
	HostActions HostActionSink
	Logf        func(string, ...any)
}

// Manager drives durable workspace creation jobs to the next safe boundary.
// It has no dependency on frontend/server or sessiondrv so ownership remains
// unambiguous: this package owns creation; other packages supply adapters.
type Manager struct {
	cfg Config
}

func NewManager(cfg Config) (*Manager, error) {
	switch {
	case cfg.Store == nil:
		return nil, fmt.Errorf("workspace create: manager needs a JobStore")
	case cfg.Worktrees == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorktreeCreator")
	case cfg.Sessions == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionCreator")
	case cfg.Health == nil:
		return nil, fmt.Errorf("workspace create: manager needs a SessionHealthChecker")
	case cfg.Prompts == nil:
		return nil, fmt.Errorf("workspace create: manager needs an InitialPromptSubmitter")
	case cfg.Available == nil:
		return nil, fmt.Errorf("workspace create: manager needs a WorkspaceAvailablePublisher")
	case cfg.HostActions == nil:
		return nil, fmt.Errorf("workspace create: manager needs a HostActionSink")
	case cfg.Logf == nil:
		return nil, fmt.Errorf("workspace create: manager needs a logger")
	}
	return &Manager{cfg: cfg}, nil
}

// Resume restarts every non-terminal job after daemon startup.  The external
// collaborators are idempotent by job ID, which closes the crash gap between
// performing an effect and persisting its succeeding state transition.
func (m *Manager) Resume(ctx context.Context) error {
	jobs, err := m.cfg.Store.List()
	if err != nil {
		return fmt.Errorf("workspace create: list jobs for resume: %w", err)
	}
	for _, job := range jobs {
		if job.State == StateReady || job.State == StateFailed || job.State == StateAwaitingEmacs {
			continue
		}
		m.cfg.Logf("workspace-create: resuming job id=%s state=%s name=%q", job.ID, job.State, job.Request.Name)
		if err := m.Process(ctx, job.ID); err != nil {
			return err
		}
	}
	return m.DrainHostActions(ctx)
}

// Process advances one job until it is waiting on Emacs, ready, or failed.
// It never creates an implicit alternate path: every missing prerequisite is
// terminally recorded and returned to the caller.
func (m *Manager) Process(ctx context.Context, id string) error {
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
			path, err := m.cfg.Worktrees.EnsureWorktree(ctx, job)
			if err != nil {
				return m.fail(id, "ensure worktree", err)
			}
			if path == "" {
				return m.fail(id, "ensure worktree", fmt.Errorf("creator returned an empty worktree path"))
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.WorktreePath = path
				j.State = StateWorktreeReady
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
		case StateWorktreeReady, StateSessionCreating:
			if job.State == StateWorktreeReady {
				if _, err := m.transition(id, StateSessionCreating, ""); err != nil {
					return err
				}
				continue
			}
			sessionID, err := m.cfg.Sessions.EnsureSession(ctx, job)
			if err != nil {
				return m.fail(id, "ensure session", err)
			}
			if sessionID == "" {
				return m.fail(id, "ensure session", fmt.Errorf("creator returned an empty session id"))
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.SessionID = sessionID
				j.State = StateSessionReady
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
		case StateSessionReady:
			if err := m.cfg.Health.AwaitHealthy(ctx, job); err != nil {
				return m.fail(id, "await session health", err)
			}
			if _, err := m.transition(id, StateSessionHealthy, ""); err != nil {
				return err
			}
		case StateSessionHealthy:
			available := Available{JobID: job.ID, Name: job.Request.Name, WorktreePath: job.WorktreePath, SessionID: job.SessionID, Request: job.Request}
			m.cfg.Logf("workspace-create: publishing available id=%s name=%q worktree=%s session=%s", job.ID, job.Request.Name, job.WorktreePath, job.SessionID)
			if err := m.cfg.Available.PublishWorkspaceAvailable(ctx, available); err != nil {
				return m.fail(id, "publish workspace available", err)
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.AvailablePublished = true
				j.State = StateAwaitingEmacs
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
			return nil
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
			m.cfg.Logf("workspace-create: submitting initial prompt id=%s name=%q session=%s prompt_len=%d", job.ID, job.Request.Name, job.SessionID, len(job.Request.Prompt))
			if err := m.cfg.Prompts.SubmitInitialPrompt(ctx, job); err != nil {
				return m.fail(id, "submit initial prompt", err)
			}
			if _, err := m.cfg.Store.Update(id, func(j *Job) error {
				j.PromptDelivered = true
				j.State = StateReady
				j.LastError = ""
				return nil
			}); err != nil {
				return err
			}
			return nil
		default:
			return m.fail(id, "read job state", fmt.Errorf("unknown state %q", job.State))
		}
	}
}

// MarkMaterialized records Emacs' acknowledgement.  Calling it repeatedly is
// safe: a replayed ACK cannot resubmit an already-delivered initial prompt.
func (m *Manager) MarkMaterialized(ctx context.Context, id string) error {
	job, ok, err := m.cfg.Store.Get(id)
	if err != nil {
		return err
	}
	if !ok {
		return fmt.Errorf("workspace create: materialization ack for unknown job %q", id)
	}
	if job.State == StateReady {
		return nil
	}
	if job.State == StateAwaitingEmacs {
		if _, err := m.transition(id, StateEmacsMaterialized, ""); err != nil {
			return err
		}
	} else if job.State != StateEmacsMaterialized && job.State != StatePromptSubmitting {
		return fmt.Errorf("workspace create: materialization ack for job %q in state %s", id, job.State)
	}
	return m.Process(ctx, id)
}

// DrainHostActions asks the host to apply every persisted UI-only action.  The
// sink deduplicates by action ID because the action is deliberately marked only
// after its publish returns successfully.
func (m *Manager) DrainHostActions(ctx context.Context) error {
	actions, err := m.cfg.Store.PendingHostActions()
	if err != nil {
		return err
	}
	for _, action := range actions {
		m.cfg.Logf("workspace-create: publishing host action id=%s type=%s", action.ID, action.Type)
		if err := m.cfg.HostActions.PublishHostAction(ctx, action); err != nil {
			return fmt.Errorf("workspace create: publish host action %s: %w", action.ID, err)
		}
		if err := m.cfg.Store.MarkHostActionDelivered(action.ID); err != nil {
			return err
		}
	}
	return nil
}

func (m *Manager) transition(id string, state JobState, lastError string) (Job, error) {
	return m.cfg.Store.Update(id, func(j *Job) error {
		j.State = state
		j.LastError = lastError
		return nil
	})
}

func (m *Manager) fail(id, action string, cause error) error {
	err := fmt.Errorf("workspace create: job %s %s: %w", id, action, cause)
	m.cfg.Logf("workspace-create: FAILED id=%s action=%s error=%v", id, action, cause)
	if _, updateErr := m.transition(id, StateFailed, err.Error()); updateErr != nil {
		return fmt.Errorf("%w (and record failure: %v)", err, updateErr)
	}
	return err
}
