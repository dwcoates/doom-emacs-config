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
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
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
	// ownerAgentID the agent whose book the spawn happened in, and outputPath the
	// spool the task writes to when the vendor named one.
	TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string)

	// TaskStopped reports that a person stopped a task. The reader owns what
	// that means: the terminal is minted by the spool's handler, which is the
	// only thing holding the output the terminal owes.
	TaskStopped(taskID string)
}

var _ Observer = (*sidecar)(nil)

// TaskSpawned implements Observer for the sidecar.
func (s *sidecar) TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string) {
	s.owners.observe(observation{
		taskID:      taskID,
		activityID:  toolUseID,
		agentID:     ownerAgentID,
		mainAgentID: ownerAgentID,
		outputPath:  discover.Normalize(outputPath),
	})
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
	sink, ok := s.watchers[path].tailer.Handler().(cancelTerminalSink)
	if !ok {
		bound.With(logging.Context{Level: "error"}).Log(
			"no terminal for the stopped run: the %s converter implements no CancelTerminal, so a run a person stopped stays open in every reader downstream",
			s.watchers[path].target.Kind)
		return
	}
	run := s.owners.activityFor(taskID)
	entries := sink.CancelTerminal(taskID, run, s.owners.agentFor(taskID), stoppedAt)
	if len(entries) == 0 {
		// CancelTerminal already stated why it refused.
		return
	}
	if err := s.storeWrite("cancelled terminal", &storev1.EntryBatch{Entries: entries}); err != nil {
		bound.With(logging.Context{Level: "error"}).Log(
			"the cancelled terminal was not committed; the stop stays pending and is restated on the next cycle: %v", err)
		return
	}
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
	backgrounded bool
}

// ownerIndex maps a task id to the call that spawned it.
//
// It holds ONE entry per open spawn, keyed by task id, plus the same entry
// reachable by output path when the vendor named one. Both are single indexed
// lookups; nothing here walks a lineage or accumulates per-record state.
type ownerIndex struct {
	byTask   map[string]observation
	byOutput map[string]string // resolved output path -> task id
	// conflicts records a task id two different spawns claimed. A conflicted
	// task resolves to nothing: guessing between two claims is how one run's
	// output lands in another run's card.
	conflicts map[string]bool
	log       *logging.Bound
}

func newOwnerIndex(log *logging.Bound) *ownerIndex {
	return &ownerIndex{
		byTask:    map[string]observation{},
		byOutput:  map[string]string{},
		conflicts: map[string]bool{},
		log:       log,
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
		bound.With(logging.Context{Operation: "resolve-spool-owner-conflicted", Level: "error"}).Log("owner resolution refused: two calls claim this task")
		return observation{}, false
	}
	// An exact output path is the strongest evidence: the vendor named this
	// file, so no id comparison is needed at all.
	if taskID, ok := o.byOutput[target.Path]; ok {
		if taskID != target.TaskID {
			bound.With(logging.Context{Operation: "resolve-spool-owner-path-mismatch", Level: "error"}).Log(
				"owner resolution refused: this exact output path is recorded for task %s", taskID)
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by exact output path")
		return o.byTask[taskID], true
	}
	if obs, ok := o.byTask[target.TaskID]; ok {
		if obs.outputPath != "" && obs.outputPath != target.Path {
			bound.With(logging.Context{Operation: "resolve-spool-owner-path-mismatch", Level: "error"}).Log(
				"owner resolution refused: the task's authoritative output path is %s", obs.outputPath)
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by task id")
		return obs, true
	}
	bound.LogVerbose("owner unknown: no spawn has been observed for this task yet")
	return observation{}, false
}

// agentFor returns the agent whose book a task's spawn happened in, when it is
// known — the run's owner, which is what its frames are attributed to.
func (o *ownerIndex) agentFor(taskID string) string { return o.byTask[taskID].agentID }

// activityFor returns the spawning call's activity id, when one is known.
func (o *ownerIndex) activityFor(taskID string) string { return o.byTask[taskID].activityID }

// outputFor returns the authoritative output path the vendor named for a task,
// when it named one.
func (o *ownerIndex) outputFor(taskID string) string { return o.byTask[taskID].outputPath }

// spawnBackgrounded reports whether a task's spawn ran in the background.
func (o *ownerIndex) spawnBackgrounded(taskID string) bool { return o.byTask[taskID].backgrounded }

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
