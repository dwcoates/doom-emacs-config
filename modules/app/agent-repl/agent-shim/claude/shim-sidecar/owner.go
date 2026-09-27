// owner.go resolves WHICH CALL a task spool belongs to.
//
// A SPOOL PATH IS A LOCATION, NEVER AN IDENTITY (see internal/discover): the
// spool layout embeds the harness's RUNTIME session id, which disagrees with the
// transcript's whenever a session was resumed. So a spool's owner is looked up
// by task id against the spawning call the converter read out of a tool result,
// and nothing else — filename similarity is deliberately not evidence and is
// never consulted.
//
// THE CONVERTER IS WHERE THE EVIDENCE ARRIVES, and it reports it through one
// callback per observation (Observer, below) rather than through a map two
// packages share. Owner resolution itself lives here, in the root package,
// because it is a fact about FILES rather than about records.
package main

import (
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Observer is the callback interface the converter uses to report what it
// learns from a tool result: which task a spawning call opened, which agent the
// spawn created, and where that task's output is being written.
//
// ONE CALL PER OBSERVATION, NO SHARED MUTABLE STATE. The converter never sees
// this index and the index never sees a record; the only thing crossing the
// seam is the observation itself.
type Observer interface {
	// TaskSpawned reports a spawning call and the task it opened. toolUseID is
	// the spawning call's activity id — which IS the created agent's AgentId
	// under the cross-plane minting rule, so no separate id is reported —
	// ownerAgentID the agent whose book the spawn happened in, outputPath the
	// spool the task writes to when the vendor named one, and backgrounded
	// whether the spawn went to the background.
	//
	// BACKGROUNDED TRAVELS RATHER THAN BEING RE-DERIVED. Only the converter sees
	// the launch result that states it, and the reader's own alternative — an a*
	// task id, or a spool target carrying a task id — is a second reading of the
	// same fact, which is how the two halves of the seam come to disagree.
	TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)

	// TaskStopped reports that a person stopped a task. The reader owns what
	// that means: the terminal is minted by the spool's handler, which is the
	// only thing holding the output the terminal owes.
	TaskStopped(taskID string)
}

var _ Observer = (*sidecar)(nil)

// TaskSpawned implements Observer for the sidecar.
func (s *sidecar) TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string) {
	if workspaceDir == "" || workspaceID == "" || claudeSessionID == "" {
		s.log.With(logging.Context{
			Operation: "record-spawn", TaskID: taskID, ActivityID: toolUseID,
			AgentID: ownerAgentID, Path: outputPath, Level: "error",
		}).Log("spawn observation rejected: workspace directory, workspace id, and transcript session are all required")
		return
	}
	normalizedOutput := discover.Normalize(outputPath)
	if normalizedOutput != "" {
		s.log.RegisterFile(logging.Context{
			Path: normalizedOutput, WorkspaceDir: workspaceDir,
			WorkspaceID: workspaceID, ClaudeSessionID: claudeSessionID,
		})
	}
	s.owners.observe(observation{
		taskID:          taskID,
		activityID:      toolUseID,
		agentID:         ownerAgentID,
		mainAgentID:     ownerAgentID,
		outputPath:      normalizedOutput,
		backgrounded:    backgrounded,
		workspaceDir:    workspaceDir,
		workspaceID:     workspaceID,
		claudeSessionID: claudeSessionID,
	})
	// A SPAWN IS ONE OF THE TWO FACTS A HELD STOP WAITS FOR, so it retries the
	// stop exactly as a spool's first durable batch does.
	//
	// WITHOUT THIS EDGE THE STOP WAS NEVER RETRIED AT ALL in the shape it is
	// most often held by. applyStop is otherwise reached only after a batch of
	// the run's spool commits — and a run a person stopped has, by definition,
	// stopped writing, so on a reader that learned of the stop before the launch
	// (a restart catching up on a large transcript) no further batch ever
	// arrived and the run stayed open forever. Now the launch itself closes it.
	s.applyStop(taskID)
}

// TaskStopped implements Observer for the sidecar.
//
// A STOP IS A FACT ABOUT A RUN, AND A RUN IS A FILE HERE. So the stop is routed
// to that file's reader if it has one, and REMEMBERED against the task if it
// does not: a spool is frequently written before the transcript line naming it,
// and a stop arriving in that window must not be dropped just because the spool
// is still held. One pending value per task, applied when the spool is claimed.
func (s *sidecar) TaskStopped(taskID string) {
	if taskID == "" {
		s.log.With(logging.Context{Operation: "task-stopped", Level: "error"}).
			Log("task stop reported with no task id; it names no run and cannot be attributed")
		return
	}
	s.stopped[taskID] = s.now().UnixMilli()
	s.log.With(logging.Context{Operation: "task-stopped", TaskID: taskID}).
		Log("task stop recorded; the run's terminal is minted by its spool's reader")
	s.applyStop(taskID)
}

// TaskConcluded records that a transcript settled a backgrounded agent run
// itself (its task notification, or an agent TaskStop). A concluded run is
// never later adjudicated LOST: its spool is settled now if it is being read,
// and when it is claimed otherwise.
func (s *sidecar) TaskConcluded(taskID string) {
	if taskID == "" {
		s.log.With(logging.Context{Operation: "task-concluded", Level: "error"}).
			Log("run conclusion reported with no task id; it names no run and cannot be attributed")
		return
	}
	s.concluded[taskID] = struct{}{}
	s.applyConclusion(taskID)
}

// applyConclusion untracks a concluded run's spool from the LOST policy, if the
// spool is being read and still tracked.
func (s *sidecar) applyConclusion(taskID string) {
	if _, ok := s.concluded[taskID]; !ok {
		return
	}
	path, watched := s.spoolForTask(taskID)
	if !watched || !s.tracker.Open(path) {
		return
	}
	s.tracker.Settle(path)
	s.log.With(logging.Context{Operation: "run-concluded", TaskID: taskID, Path: path}).
		Log("the run was settled by its transcript; it can no longer be concluded LOST")
}

// applyStop mints and writes the cancelled terminal for a stopped task, if its
// spool is being read. A task whose spool is not watched yet keeps its pending
// stop and is retried when the spool is claimed.
//
// THE PENDING STOP IS RETIRED ONLY ON A DURABLE WRITE, and the run is untracked
// at the same moment: a cancelled run must never be restated LOST, and a stop
// forgotten against a refused write would be a run that is neither.
func (s *sidecar) applyStop(taskID string) {
	stoppedAt, pending := s.stopped[taskID]
	if !pending {
		return
	}
	path, watched := s.spoolForTask(taskID)
	if !watched {
		s.log.With(logging.Context{Operation: "cancel-terminal", TaskID: taskID}).
			LogVerbose("the stopped task's spool is not being read yet; the stop is held until it is claimed")
		return
	}
	bound := s.log.With(logging.Context{Operation: "cancel-terminal", TaskID: taskID, Path: path})
	// A STOP WAITING ON ITS SPAWNING CALL IS HELD, NOT REFUSED. The terminal is
	// keyed on the spawning call's activity id and nothing else, so a stop read
	// before the launch line naming that call has no unit to settle yet — which
	// is the ORDINARY shape of a restart with a backlog, not a failure: the
	// reader joins a 50 MB transcript at the cursor the store holds and the
	// launch is tens of megabytes behind it, still to be caught up on. It is the
	// same "not known yet" as the unwatched-spool branch above and it is held the
	// same way, on the same pending stop, and retried the moment TaskSpawned
	// names the call. Reaching the sink here instead turned each one into an
	// ERROR that no operator could act on (31 of them on the owner's machine,
	// one per distinct b* run) while leaving the run open anyway.
	run := s.owners.activityFor(taskID)
	if run == "" {
		bound.LogVerbose("the stopped task's spawning call is not known yet; the stop is held until a launch names it")
		return
	}
	sink, ok := s.watchers[path].tailer.Handler().(cancelTerminalSink)
	if !ok {
		bound.With(logging.Context{Level: "error"}).Log(
			"no terminal for the stopped run: the %s converter implements no CancelTerminal, so a run a person stopped stays open in every reader downstream",
			s.watchers[path].target.Kind)
		return
	}
	entries := sink.CancelTerminal(taskID, run, s.owners.agentFor(taskID), stoppedAt)
	if len(entries) == 0 {
		// CancelTerminal already stated why it refused.
		return
	}
	skips, err := s.storeWrite("cancelled terminal", &storev1.EntryBatch{Entries: entries})
	if err != nil {
		if s.interrupted(err) {
			// THE LAST storeWrite CALLER WITHOUT THIS GUARD. A shutdown
			// withdrawing the write is not the store failing: storeWrite has
			// already stated the one INFO `shutdown` record, the stop stays
			// pending, and the next boot re-reads the same bytes and re-applies
			// it. Its two siblings in cycle.go already returned quietly here;
			// this one accused the store instead.
			return
		}
		bound.With(logging.Context{Level: "error"}).Log(
			"the cancelled terminal was not committed; the stop stays pending and is restated on the next cycle: %v", err)
		return
	}
	// A cancelled terminal is an inferred record naming no file: a book-conflict
	// skip here is unexpected and warned, never folded into a catch-up summary.
	s.warnUnexpectedSkips("cancelled terminal", skips)
	delete(s.stopped, taskID)
	s.tracker.Settle(path)
	bound.With(logging.Context{ActivityID: run}).Log("cancelled terminal minted and committed entries=%d", len(entries))
}

// spoolForTask answers the watched file that IS a task's run.
//
// BY THE OWNER'S AUTHORITATIVE OUTPUT PATH FIRST, and by the watcher's own task
// id otherwise — both single indexed lookups, and neither of them a guess from
// filename similarity, which owner.go's header rules out as evidence.
func (s *sidecar) spoolForTask(taskID string) (string, bool) {
	if path := s.owners.outputFor(taskID); path != "" {
		if _, ok := s.watchers[path]; ok {
			return path, true
		}
	}
	for path, w := range s.watchers {
		if w.target.TaskID == taskID && w.target.SessionID == "" {
			return path, true
		}
	}
	return "", false
}

// observation is one authoritative spawn, as reported by the converter.
type observation struct {
	taskID     string
	activityID string
	// agentID is the agent whose book the spawn happened in — the SPAWNER. The
	// spawned agent's identity is activityID, so it is not stored twice.
	agentID    string
	outputPath string
	// mainAgentID is the agent whose stream carried the spawn.
	mainAgentID string
	// backgrounded reports that the spawn ran in the background, which is what
	// makes a subagent its own top_level rather than the spawner's.
	backgrounded    bool
	workspaceDir    string
	workspaceID     string
	claudeSessionID string
}

// ownerIndex maps a task id to the call that spawned it.
//
// It holds ONE entry per open spawn, keyed by task id, plus the same entry
// reachable by output path when the vendor named one. Both are single indexed
// lookups; nothing here walks a lineage or accumulates per-record state.
type ownerIndex struct {
	byTask   map[string]observation
	byOutput map[string]string // resolved output path -> task id
	// backgroundedCalls remembers, by the SPAWNING CALL's activity id, that the
	// spawn ran in the background.
	//
	// A BACKGROUNDED SUBAGENT IS SEEN TWICE, and only one of the two sightings
	// names a task. Its transcript arrives as an `a*` task SPOOL, which is keyed
	// by task id, AND as a `subagents/agent-<id>.jsonl` SIDECHAIN, which names
	// no task at all — its only identity is the spawning call the meta file
	// states. A backgrounded flag reachable only by task id therefore answered
	// false for the sidechain, and the two planes' writes for ONE agent carried
	// DIFFERENT top_level: the subagent itself from the spool, the session's
	// main agent from the sidechain. This index is what makes the two agree.
	backgroundedCalls map[string]bool
	// conflicts records a task id two different spawns claimed. A conflicted
	// task resolves to nothing: guessing between two claims is how one run's
	// output lands in another run's card.
	conflicts map[string]bool
	// refused remembers, by spool path, the refusal already stated for it.
	//
	// A REFUSAL IS A CONDITION, NOT AN EVENT. A refused spool is never read, so
	// every rescan resolves it again; without this each pass restated the same
	// ERROR for as long as the file existed.
	refused map[string]string
	log     *logging.Bound
}

func newOwnerIndex(log *logging.Bound) *ownerIndex {
	return &ownerIndex{
		byTask:            map[string]observation{},
		byOutput:          map[string]string{},
		backgroundedCalls: map[string]bool{},
		conflicts:         map[string]bool{},
		refused:           map[string]string{},
		log:               log,
	}
}

// observe records one spawn.
//
// A CONFLICT IS ITS OWN OPERATION (`record-spawn-conflict`): it is the branch
// that makes a task permanently unresolvable, and sharing `record-spawn` with an
// ordinary rejection left the two findable only by their prose.
func (o *ownerIndex) observe(obs observation) {
	bound := o.log.With(logging.Context{
		Operation: "record-spawn", TaskID: obs.taskID, ActivityID: obs.activityID,
		AgentID: obs.agentID, Path: obs.outputPath,
		WorkspaceDir: obs.workspaceDir, WorkspaceID: obs.workspaceID,
		ClaudeSessionID: obs.claudeSessionID,
	})
	if obs.taskID == "" || obs.activityID == "" {
		bound.With(logging.Context{Level: "error"}).Log("spawn observation rejected: it names no task or no spawning call")
		return
	}
	if prior, ok := o.byTask[obs.taskID]; ok {
		if prior.activityID != obs.activityID {
			o.conflicts[obs.taskID] = true
			bound.With(logging.Context{Operation: "record-spawn-conflict", Level: "error"}).Log(
				"CONFLICTING spawn observation: task already claimed by call %s; it resolves to nothing rather than to a guess", prior.activityID)
			return
		}
		bound.LogVerbose("spawn observation re-reported by the same call")
		return
	}
	o.byTask[obs.taskID] = obs
	if obs.backgrounded {
		o.backgroundedCalls[obs.activityID] = true
	}
	if obs.outputPath != "" {
		o.byOutput[obs.outputPath] = obs.taskID
	}
	bound.Log("spawn recorded: the task's output belongs to this call")
}

// resolve returns the spawn that owns a spool, and whether it is known.
//
// THE TWO REFUSALS ARE DISTINCT BRANCHES: `resolve-spool-owner-conflicted` is a
// task two calls claim, `resolve-spool-owner-path-mismatch` is a spool whose
// authoritative output is a different file. They are fixed differently, so a
// reader must be able to filter for one without reading sentences.
func (o *ownerIndex) resolve(target discover.Target) (observation, bool) {
	bound := o.log.With(logging.Context{Operation: "resolve-spool-owner", Path: target.Path, TaskID: target.TaskID})
	if o.conflicts[target.TaskID] {
		o.refuse(bound, target.Path, "resolve-spool-owner-conflicted", "owner resolution refused: two calls claim this task")
		return observation{}, false
	}
	// An exact output path is the strongest evidence: the vendor named this
	// file, so no id comparison is needed at all.
	if taskID, ok := o.byOutput[target.Path]; ok {
		if taskID != target.TaskID {
			o.refuse(bound, target.Path, "resolve-spool-owner-path-mismatch",
				fmt.Sprintf("owner resolution refused: this exact output path is recorded for task %s", taskID))
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by exact output path")
		return o.byTask[taskID], true
	}
	if obs, ok := o.byTask[target.TaskID]; ok {
		if obs.outputPath != "" && obs.outputPath != target.Path {
			o.refuse(bound, target.Path, "resolve-spool-owner-path-mismatch",
				fmt.Sprintf("owner resolution refused: the task's authoritative output path is %s", obs.outputPath))
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by task id")
		return obs, true
	}
	bound.LogVerbose("owner unknown: no spawn has been observed for this task yet")
	return observation{}, false
}

// refuse states a refusal at ERROR the first time it is true of a path, and
// verbosely on every rescan that finds it true again.
func (o *ownerIndex) refuse(bound *logging.Bound, path, operation, message string) {
	if o.refused[path] == operation {
		bound.With(logging.Context{Operation: operation}).LogVerbose("%s; already stated for this spool", message)
		return
	}
	o.refused[path] = operation
	bound.With(logging.Context{Operation: operation, Level: "error"}).Log("%s", message)
}

// repoint moves a task's authoritative output path to where its file now is.
func (o *ownerIndex) repoint(taskID, path string) {
	obs, ok := o.byTask[taskID]
	if !ok {
		panic("sidecar: repoint of a task no spawn was observed for")
	}
	delete(o.byOutput, obs.outputPath)
	delete(o.refused, path)
	obs.outputPath = path
	o.byTask[taskID] = obs
	o.byOutput[path] = taskID
}

// followRename re-points a claimed run at its spool's new path when the file
// found there IS the file the run's reader has been reading.
//
// A RENAMED SPOOL IS THE SAME RUN IN A NEW PLACE. The vendor may move a task's
// output under a new runtime-session segment; the owner index still names the
// old path, so resolution refuses the new one as a mismatch — and a refused
// spool is never read, which would strand the rest of a rendered run's output.
// The file's own dev:inode identity is the evidence, exactly as it is for the
// cursor: the same identity as the watched old path is the same file, never a
// guess from its name.
func (s *sidecar) followRename(target discover.Target) {
	obs, ok := s.owners.byTask[target.TaskID]
	if !ok || s.owners.conflicts[target.TaskID] || obs.outputPath == "" || obs.outputPath == target.Path {
		return
	}
	old, watched := s.watchers[obs.outputPath]
	if !watched || old.tailer.FileID() == "" {
		return
	}
	identity, err := tail.Identity(target.Path)
	if err != nil {
		s.log.With(logging.Context{Operation: "spool-rename", Path: target.Path, TaskID: target.TaskID}).
			LogVerbose("whether this spool is the claimed run's renamed file could not be read: %v", err)
		return
	}
	if identity != old.tailer.FileID() {
		return
	}
	s.owners.repoint(target.TaskID, target.Path)
	s.log.With(logging.Context{
		Operation: "spool-rename", Path: target.Path, TaskID: target.TaskID, FileID: identity, ActivityID: obs.activityID,
	}).Log("the claimed run's spool was renamed from %s; it is the same file, so the run is read at its new path", obs.outputPath)
}

// agentFor returns the agent whose book a task's spawn happened in, when it is
// known — the run's owner, which is what its frames are attributed to.
func (o *ownerIndex) agentFor(taskID string) string { return o.byTask[taskID].agentID }

// activityFor returns the spawning call's activity id, when one is known.
func (o *ownerIndex) activityFor(taskID string) string { return o.byTask[taskID].activityID }

// outputFor returns the authoritative output path the vendor named for a task,
// when it named one.
func (o *ownerIndex) outputFor(taskID string) string { return o.byTask[taskID].outputPath }

// backgroundedFor reports whether the spawn behind a watched file ran in the
// background, by whichever identity that file actually carries.
//
// A SPOOL NAMES A TASK; A SIDECHAIN TRANSCRIPT NAMES THE SPAWNING CALL. Both are
// the SAME backgrounded subagent, and `top_level` must be the same identity on
// both — the subagent itself, because its stream outlives the turn that spawned
// it. Asking only by task id made the sidechain's writes name the session's main
// agent instead, so one agent's two planes disagreed about where its work
// belongs and no consumer could reconcile them.
func (o *ownerIndex) backgroundedFor(target discover.Target) bool {
	// AN OBSERVED SPAWN IS THE STRONGEST ANSWER, and only a spool's TaskID is a
	// harness task id the launch named. A sidechain transcript's TaskID is its
	// `agent-<id>` LOCATOR, which no launch ever mentions, so the lookup misses
	// and the fallback below is what answers for it.
	if obs, ok := o.byTask[target.TaskID]; ok {
		return obs.backgrounded
	}
	// A sidechain transcript's AgentID is the `toolUseId` its meta file states,
	// which IS the spawning call's activity id (the cross-plane minting rule).
	return o.backgroundedCalls[target.AgentID]
}

// mainAgentFor returns the main agent whose work a file belongs to.
//
// A SESSION TRANSCRIPT'S MAIN AGENT IS ITS OWN FILE NAME. The file's session
// uuid is the identity; the per-record `sessionId` field diverges from it in a
// fifth of records and is never read as one. A subagent transcript belongs to
// the session directory it sits under, and a spool to the agent that spawned it.
func (o *ownerIndex) mainAgentFor(target discover.Target) string {
	if target.SessionID != "" {
		return target.SessionID
	}
	if obs, ok := o.byTask[target.TaskID]; ok {
		return obs.mainAgentID
	}
	return ""
}
