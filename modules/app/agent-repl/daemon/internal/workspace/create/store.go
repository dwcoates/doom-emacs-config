package create

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
)

// JobStore is deliberately separate from the session registry.  The registry
// answers which sessions exist; this store answers which workspace creation
// effects are pending and which source command caused them.
type JobStore interface {
	Enqueue(Job) (job Job, inserted bool, err error)
	Get(string) (Job, bool, error)
	List() ([]Job, error)
	Update(string, func(*Job) error) (Job, error)
	EnqueueHostAction(HostAction) (action HostAction, inserted bool, err error)
	HostActionsForDelivery() ([]HostAction, error)
	PendingHostActions() ([]HostAction, error)
	MarkHostActionPublished(string) error
	CompleteHostAction(id string, ok bool, failure string) error
}

type diskShape struct {
	Version     int                   `json:"version"`
	Jobs        map[string]Job        `json:"jobs"`
	HostActions map[string]HostAction `json:"host_actions"`
}

// FileJobStore is a small write-through, atomically replaced durable store.
// The create queue is low-volume and each transition needs a readable durable
// checkpoint, so a JSON document is less surprising than sharing the session
// registry or introducing lifecycle state into the shim store.
type FileJobStore struct {
	path string
	logf func(string, ...any)
	mu   sync.Mutex
	doc  diskShape
}

// OpenJobStore opens a durable creation store.  A missing file is the first
// boot case; malformed state is an error so startup cannot silently lose jobs.
func OpenJobStore(path string, logf func(string, ...any)) (*FileJobStore, error) {
	if path == "" {
		return nil, fmt.Errorf("workspace create: job store path is required")
	}
	if logf == nil {
		return nil, fmt.Errorf("workspace create: job store logger is required")
	}
	s := &FileJobStore{path: path, logf: logf, doc: diskShape{Version: 1, Jobs: map[string]Job{}, HostActions: map[string]HostAction{}}}
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return s, nil
		}
		return nil, fmt.Errorf("workspace create: read job store %s: %w", path, err)
	}
	if err := json.Unmarshal(data, &s.doc); err != nil {
		return nil, fmt.Errorf("workspace create: parse job store %s: %w", path, err)
	}
	if s.doc.Version != 1 {
		return nil, fmt.Errorf("workspace create: unsupported job store version %d", s.doc.Version)
	}
	if s.doc.Jobs == nil || s.doc.HostActions == nil {
		return nil, fmt.Errorf("workspace create: job store %s has missing required collections", path)
	}
	for id, job := range s.doc.Jobs {
		if id != job.ID {
			return nil, fmt.Errorf("workspace create: job store key %q does not match job id %q", id, job.ID)
		}
		if err := job.validate(); err != nil {
			return nil, fmt.Errorf("workspace create: invalid persisted job %q: %w", id, err)
		}
	}
	return s, nil
}

func (s *FileJobStore) Enqueue(job Job) (Job, bool, error) {
	if err := job.validate(); err != nil {
		return Job{}, false, err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if old, ok := s.doc.Jobs[job.ID]; ok {
		return old, false, nil
	}
	s.doc.Jobs[job.ID] = job
	if err := s.saveLocked(); err != nil {
		delete(s.doc.Jobs, job.ID)
		return Job{}, false, err
	}
	s.logf("workspace-create: persisted job id=%s state=%s name=%q", job.ID, job.State, job.Request.Name)
	return job, true, nil
}

func (s *FileJobStore) Get(id string) (Job, bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	job, ok := s.doc.Jobs[id]
	return job, ok, nil
}

func (s *FileJobStore) List() ([]Job, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	jobs := make([]Job, 0, len(s.doc.Jobs))
	for _, job := range s.doc.Jobs {
		jobs = append(jobs, job)
	}
	sort.Slice(jobs, func(i, j int) bool { return jobs[i].ID < jobs[j].ID })
	return jobs, nil
}

func (s *FileJobStore) Update(id string, change func(*Job) error) (Job, error) {
	if change == nil {
		return Job{}, fmt.Errorf("workspace create: update %q has nil change", id)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	job, ok := s.doc.Jobs[id]
	if !ok {
		return Job{}, fmt.Errorf("workspace create: unknown job %q", id)
	}
	old := job
	if err := change(&job); err != nil {
		return Job{}, err
	}
	if err := job.validate(); err != nil {
		return Job{}, err
	}
	s.doc.Jobs[id] = job
	if err := s.saveLocked(); err != nil {
		s.doc.Jobs[id] = old
		return Job{}, err
	}
	s.logf("workspace-create: persisted transition id=%s state=%s session=%s worktree=%s prompt_delivered=%t", job.ID, job.State, job.SessionID, job.WorktreePath, job.PromptDelivered)
	return job, nil
}

func (s *FileJobStore) EnqueueHostAction(action HostAction) (HostAction, bool, error) {
	if action.ID == "" || action.Type == "" || len(action.Payload) == 0 {
		return HostAction{}, false, fmt.Errorf("workspace create: host action requires id, type, and payload")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if old, ok := s.doc.HostActions[action.ID]; ok {
		return old, false, nil
	}
	s.doc.HostActions[action.ID] = action
	if err := s.saveLocked(); err != nil {
		delete(s.doc.HostActions, action.ID)
		return HostAction{}, false, err
	}
	s.logf("workspace-create: persisted host action id=%s type=%s", action.ID, action.Type)
	return action, true, nil
}

func (s *FileJobStore) HostActionsForDelivery() ([]HostAction, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	actions := make([]HostAction, 0, len(s.doc.HostActions))
	for _, action := range s.doc.HostActions {
		if !action.Published && !action.Completed {
			actions = append(actions, action)
		}
	}
	sort.Slice(actions, func(i, j int) bool { return actions[i].ID < actions[j].ID })
	return actions, nil
}

// PendingHostActions returns actions that still require a host result.  It is
// the reconnect snapshot source: a previously published but uncompleted action
// remains visible until the host explicitly reports success or failure.
func (s *FileJobStore) PendingHostActions() ([]HostAction, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	actions := make([]HostAction, 0, len(s.doc.HostActions))
	for _, action := range s.doc.HostActions {
		if !action.Completed {
			actions = append(actions, action)
		}
	}
	sort.Slice(actions, func(i, j int) bool { return actions[i].ID < actions[j].ID })
	return actions, nil
}

func (s *FileJobStore) MarkHostActionPublished(id string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	action, ok := s.doc.HostActions[id]
	if !ok {
		return fmt.Errorf("workspace create: unknown host action %q", id)
	}
	if action.Published {
		return nil
	}
	action.Published = true
	s.doc.HostActions[id] = action
	if err := s.saveLocked(); err != nil {
		action.Published = false
		s.doc.HostActions[id] = action
		return err
	}
	s.logf("workspace-create: marked host action published id=%s type=%s", id, action.Type)
	return nil
}

func (s *FileJobStore) CompleteHostAction(id string, ok bool, failure string) error {
	if ok && failure != "" {
		return fmt.Errorf("workspace create: completed host action %q cannot carry a failure", id)
	}
	if !ok && failure == "" {
		return fmt.Errorf("workspace create: failed host action %q requires failure text", id)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	action, exists := s.doc.HostActions[id]
	if !exists {
		return fmt.Errorf("workspace create: unknown host action %q", id)
	}
	if action.Completed {
		if !ok {
			return fmt.Errorf("workspace create: host action %q was already completed successfully", id)
		}
		return nil
	}
	old := action
	if ok {
		action.Completed = true
		action.Failure = ""
	} else if action.Failure == failure {
		// The host refused this action with the IDENTICAL failure it already
		// reported: a deterministic refusal (e.g. "unsupported type"), not a
		// transient one. Re-publishing it on the drain tick would loop the
		// host through the same refusal every second — which is exactly what
		// stalled Emacs startup on a stuck set-view action. The action is
		// PARKED instead: it stays incomplete with its durable failure, and
		// the next frontend reconnect snapshot re-surfaces it once, so the
		// evidence is never hidden and a host that has since learned the type
		// can still complete it.
		s.logf("workspace-create: host action PARKED after repeated identical refusal id=%s type=%s failure=%q — active redelivery stops; the reconnect snapshot re-surfaces it", id, action.Type, failure)
	} else {
		// A failed host action deliberately remains incomplete and becomes
		// publishable again. Its failure is durable diagnostic evidence, while
		// clearing Published makes the active daemon drain retry it rather than
		// waiting indefinitely for a frontend reconnect snapshot.
		action.Failure = failure
		action.Published = false
	}
	s.doc.HostActions[id] = action
	if err := s.saveLocked(); err != nil {
		s.doc.HostActions[id] = old
		return err
	}
	s.logf("workspace-create: host action completion id=%s type=%s ok=%t failure=%q", id, action.Type, ok, failure)
	return nil
}

func (s *FileJobStore) saveLocked() error {
	if err := os.MkdirAll(filepath.Dir(s.path), 0o755); err != nil {
		return fmt.Errorf("workspace create: create job store directory: %w", err)
	}
	payload, err := json.MarshalIndent(s.doc, "", "  ")
	if err != nil {
		return fmt.Errorf("workspace create: marshal job store: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(s.path), ".workspace-create-*.json")
	if err != nil {
		return fmt.Errorf("workspace create: create temporary job store: %w", err)
	}
	tmpPath := tmp.Name()
	defer os.Remove(tmpPath)
	if _, err := tmp.Write(payload); err != nil {
		tmp.Close()
		return fmt.Errorf("workspace create: write temporary job store: %w", err)
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return fmt.Errorf("workspace create: sync temporary job store: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("workspace create: close temporary job store: %w", err)
	}
	if err := os.Rename(tmpPath, s.path); err != nil {
		return fmt.Errorf("workspace create: replace job store: %w", err)
	}
	return nil
}
