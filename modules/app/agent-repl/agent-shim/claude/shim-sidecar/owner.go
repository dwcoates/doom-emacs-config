package main

import (
	"path/filepath"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// OwnerSource identifies the evidence that binds a task spool to a session.
// Sources are recorded rather than inferred so diagnostic state can explain
// exactly why an association is authoritative.
type OwnerSource string

const (
	OwnerSourceTarget          OwnerSource = "target-session"
	OwnerSourceLiveLaunch      OwnerSource = "live-launch"
	OwnerSourceDurableOpenTask OwnerSource = "durable-open-task"
)

// OwnerResolutionOutcome states why resolving a discovered target succeeded or
// failed. Consumers must not turn an unresolved outcome into an association.
type OwnerResolutionOutcome string

const (
	OwnerResolvedPath            OwnerResolutionOutcome = "resolved-exact-output-path"
	OwnerResolvedTask            OwnerResolutionOutcome = "resolved-unique-task"
	OwnerUnresolvedAwaitingOwner OwnerResolutionOutcome = "unresolved-awaiting-owner"
	OwnerUnresolvedConflict      OwnerResolutionOutcome = "unresolved-conflicting-owner"
	OwnerUnresolvedInvalid       OwnerResolutionOutcome = "unresolved-invalid-target"
)

// OwnerResolution is the complete owner lookup result consumed by spool held
// lifecycle. OutputPath is normalized whenever target metadata carried a path.
type OwnerResolution struct {
	SessionID  string
	TaskID     string
	OutputPath string
	Source     OwnerSource
	Outcome    OwnerResolutionOutcome
}

func (r OwnerResolution) Resolved() bool {
	return r.Outcome == OwnerResolvedPath || r.Outcome == OwnerResolvedTask
}

// MayArrive reports whether a later authoritative observation can resolve this
// target. A conflicted task may later gain an exact output-path observation;
// malformed metadata is the only terminally non-retryable result here.
func (r OwnerResolution) MayArrive() bool {
	return r.Outcome == OwnerUnresolvedAwaitingOwner || r.Outcome == OwnerUnresolvedConflict
}

type ownerRecord struct {
	taskID     string
	sessionID  string
	outputPath string
	source     OwnerSource
}

// normalizeOwnerOutputPath produces the identity under which an owner output
// path is recorded and compared. Two spellings of one file — notably macOS's
// /tmp symlink onto /private/tmp — must collapse to one key, so symlinks are
// resolved rather than merely cleaned.
//
// A spool is observed both before and after it exists, and filepath.EvalSymlinks
// fails on a missing path, so resolution walks up to the deepest existing
// ancestor, resolves that, and rejoins the not-yet-created suffix. When nothing
// on the path exists the cleaned path is returned unchanged; normalization never
// fails and never drops a path.
func normalizeOwnerOutputPath(path string) string {
	if path == "" {
		return ""
	}
	cleaned := filepath.Clean(path)
	var suffix []string
	current := cleaned
	for {
		if resolved, err := filepath.EvalSymlinks(current); err == nil {
			for i := len(suffix) - 1; i >= 0; i-- {
				resolved = filepath.Join(resolved, suffix[i])
			}
			return resolved
		}
		parent := filepath.Dir(current)
		if parent == current {
			return cleaned
		}
		suffix = append(suffix, filepath.Base(current))
		current = parent
	}
}

// resolveOwnerResult resolves only a target's explicit session, an exact
// normalized output path, or an unambiguous task mapping. Filename similarity
// is deliberately not evidence and is never consulted.
func (s *sidecar) resolveOwnerResult(tgt discover.Target) OwnerResolution {
	outputPath := normalizeOwnerOutputPath(tgt.Path)
	s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: tgt.SessionID, Task: tgt.TaskID}).LogVerbose("owner resolution entered target_session=%t task_id=%q", tgt.SessionID != "", tgt.TaskID)
	if tgt.SessionID != "" {
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: tgt.SessionID, Task: tgt.TaskID}).LogVerbose("owner resolution selected target session")
		return OwnerResolution{SessionID: tgt.SessionID, TaskID: tgt.TaskID, OutputPath: outputPath, Source: OwnerSourceTarget, Outcome: OwnerResolvedPath}
	}
	if tgt.TaskID == "" || outputPath == "" {
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Task: tgt.TaskID, Level: "error"}).Log("owner resolution rejected invalid spool target")
		return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedInvalid}
	}
	if s.ownerPathConflicts[outputPath] {
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Task: tgt.TaskID, Level: "error"}).Log("owner resolution rejected conflicting exact output path")
		return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedConflict}
	}
	if record, ok := s.ownerByOutput[outputPath]; ok {
		if record.taskID != tgt.TaskID {
			s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: record.sessionID, Task: tgt.TaskID, Level: "error"}).Log("owner resolution rejected exact output path task mismatch recorded_task=%s", record.taskID)
			return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedConflict}
		}
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: record.sessionID, Task: tgt.TaskID}).Log("owner resolution selected exact output path source=%s recorded_task=%s", record.source, record.taskID)
		return OwnerResolution{SessionID: record.sessionID, TaskID: tgt.TaskID, OutputPath: outputPath, Source: record.source, Outcome: OwnerResolvedPath}
	}
	if s.ownerConflicts[tgt.TaskID] {
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Task: tgt.TaskID, Level: "error"}).Log("owner resolution rejected conflicting task ownership")
		return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedConflict}
	}
	if session, ok := s.owners[tgt.TaskID]; ok {
		if recordedPath := s.ownerTaskOutput[tgt.TaskID]; recordedPath != "" && recordedPath != outputPath {
			s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: session, Task: tgt.TaskID, Level: "error"}).Log("owner resolution rejected task-only association with different authoritative output path recorded_path=%s", recordedPath)
			return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedConflict}
		}
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Session: session, Task: tgt.TaskID}).Log("owner resolution selected unique task association")
		return OwnerResolution{SessionID: session, TaskID: tgt.TaskID, OutputPath: outputPath, Source: s.ownerSource[tgt.TaskID], Outcome: OwnerResolvedTask}
	}
	s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: outputPath, Task: tgt.TaskID}).LogVerbose("owner resolution awaiting authoritative observation")
	return OwnerResolution{TaskID: tgt.TaskID, OutputPath: outputPath, Outcome: OwnerUnresolvedAwaitingOwner}
}

// observeOwner records one authoritative association. The output path index is
// authoritative only for an exact cleaned path; task-only resolution remains
// unavailable once two sessions claim the same task identifier.
func (s *sidecar) observeOwner(taskID, sessionID, outputPath string, source OwnerSource) bool {
	outputPath = normalizeOwnerOutputPath(outputPath)
	if taskID == "" || sessionID == "" {
		s.log.With(logging.Context{Operation: "record-spool-owner", Path: outputPath, Session: sessionID, Task: taskID, Level: "error"}).Log("owner observation rejected missing task or session source=%s", source)
		return false
	}
	s.log.With(logging.Context{Operation: "record-spool-owner", Path: outputPath, Session: sessionID, Task: taskID}).LogVerbose("owner observation entered source=%s", source)
	if prior, ok := s.owners[taskID]; ok && prior != sessionID {
		s.ownerConflicts[taskID] = true
		s.log.With(logging.Context{Operation: "record-spool-owner", Path: outputPath, Session: prior, Task: taskID, Level: "error"}).Log("owner observation CONFLICTING owner source=%s existing_session=%s incoming_session=%s", source, prior, sessionID)
	}
	added := false
	if outputPath != "" {
		if prior, ok := s.ownerByOutput[outputPath]; ok && (prior.sessionID != sessionID || prior.taskID != taskID) {
			delete(s.ownerByOutput, outputPath)
			s.ownerPathConflicts[outputPath] = true
			s.log.With(logging.Context{Operation: "record-spool-owner", Path: outputPath, Session: prior.sessionID, Task: taskID, Level: "error"}).Log("owner observation conflicts exact output path source=%s existing_task=%s existing_session=%s incoming_task=%s incoming_session=%s", source, prior.taskID, prior.sessionID, taskID, sessionID)
			return false
		}
		if !s.ownerPathConflicts[outputPath] {
			if _, exists := s.ownerByOutput[outputPath]; !exists {
				added = true
			}
			s.ownerByOutput[outputPath] = ownerRecord{taskID: taskID, sessionID: sessionID, outputPath: outputPath, source: source}
		}
	}
	if _, ok := s.owners[taskID]; ok {
		return added
	}
	s.owners[taskID] = sessionID
	s.ownerSource[taskID] = source
	s.ownerTaskOutput[taskID] = outputPath
	s.log.With(logging.Context{Operation: "record-spool-owner", Path: outputPath, Session: sessionID, Task: taskID}).Log("owner observation recorded source=%s", source)
	return true
}

// resetOwners drops connection-scoped durable observations before the next
// recovery snapshot is seeded. A closed task from a prior connection must not
// remain capable of claiming a newly discovered spool.
func (s *sidecar) resetOwners() {
	s.log.With(logging.Context{Operation: "reset-spool-owners"}).Log("owner index reset before authoritative recovery")
	s.owners = map[string]string{}
	s.ownerSource = map[string]OwnerSource{}
	s.ownerTaskOutput = map[string]string{}
	s.ownerByOutput = map[string]ownerRecord{}
	s.ownerConflicts = map[string]bool{}
	s.ownerPathConflicts = map[string]bool{}
	s.openTasks = map[string]bool{}
}

// markTaskOpen records authoritative live/recovered lifecycle state separately
// from ownership. An open task can still lack a usable owner when its durable
// launch metadata is malformed, and must remain retryable rather than becoming
// terminal merely because its spool is old.
func (s *sidecar) markTaskOpen(taskID string, source OwnerSource) {
	if taskID == "" {
		s.log.With(logging.Context{Operation: "record-open-task", Level: "error"}).Log("open-task observation rejected missing task id source=%s", source)
		return
	}
	s.openTasks[taskID] = true
	s.log.With(logging.Context{Operation: "record-open-task", Task: taskID}).LogVerbose("open-task observation recorded source=%s", source)
}

func (s *sidecar) markTaskClosed(taskID string) {
	if taskID == "" {
		s.log.With(logging.Context{Operation: "record-closed-task", Level: "error"}).Log("closed-task observation rejected missing task id")
		return
	}
	delete(s.openTasks, taskID)
	s.log.With(logging.Context{Operation: "record-closed-task", Task: taskID}).LogVerbose("closed-task observation recorded")
}

func (s *sidecar) taskOpen(taskID string) bool {
	return s.openTasks[taskID]
}

// seedOwners seeds the spool-owner index from the store's authoritative
// open-task snapshot.
//
// IT CAN NO LONGER SEED ANYTHING. `OpenTaskState.started` carried the record
// that opened a task — the task id, the session that launched it, and the output
// path its spool lives at — and it was retired with no successor. What remains
// says only when a task was last active, which names no task, no session and no
// path, so there is no association to record.
//
// WHAT THIS SEED EXISTED TO PREVENT NOW HAPPENS. A /tmp spool carries no session
// of its own, and the launch line naming its owner may sit far behind this
// connection's resumed cursor — so a restart leaves a LIVE task's spool
// unattributed, and therefore untailed, until its transcript happens to be
// re-read. The persisted open tasks were exactly that mapping, already fetched.
//
// The spool is HELD rather than guessed at, which is the same behavior an
// unattributed spool has always had: inventing a session, or reading the /tmp
// path's runtime id as an identity, are the two things this system refuses.
func (s *sidecar) seedOwners(states []*agentshimv1.OpenTaskState) int {
	if len(states) == 0 {
		s.log.With(logging.Context{Operation: "seed-spool-owners"}).LogVerbose("owner seed entered with no persisted open tasks")
		return 0
	}
	s.log.With(logging.Context{Operation: "seed-spool-owners", Level: "error"}).Log(
		"owner seed produced nothing from %d persisted open task(s): OpenTaskState carries no task, session or output path to seed from; "+
			"live tasks' spools stay held and unread until their transcripts are re-read", len(states))
	return 0
}

func (s *sidecar) noteTaskOwner(taskID, session, outputPath string, source OwnerSource) bool {
	return s.observeOwner(taskID, session, outputPath, source)
}
