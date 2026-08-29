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
	// the spawning call's activity id, agentID the created agent's id (empty for
	// a shell run, which creates no agent), and outputPath the spool the task
	// writes to when the vendor named one (empty when it did not).
	TaskSpawned(taskID, toolUseID, agentID, outputPath string)
}

var _ Observer = (*sidecar)(nil)

// TaskSpawned implements Observer for the sidecar.
func (s *sidecar) TaskSpawned(taskID, toolUseID, agentID, outputPath string) {
	s.owners.observe(observation{
		taskID:     taskID,
		activityID: toolUseID,
		agentID:    agentID,
		outputPath: discover.Normalize(outputPath),
	})
}

// observation is one authoritative spawn, as reported by the converter.
type observation struct {
	taskID     string
	activityID string
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
			bound.With(logging.Context{Level: "error"}).Log(
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
func (o *ownerIndex) resolve(target discover.Target) (observation, bool) {
	bound := o.log.With(logging.Context{Operation: "resolve-spool-owner", Path: target.Path, TaskID: target.TaskID})
	if o.conflicts[target.TaskID] {
		bound.With(logging.Context{Level: "error"}).Log("owner resolution refused: two calls claim this task")
		return observation{}, false
	}
	// An exact output path is the strongest evidence: the vendor named this
	// file, so no id comparison is needed at all.
	if taskID, ok := o.byOutput[target.Path]; ok {
		if taskID != target.TaskID {
			bound.With(logging.Context{Level: "error"}).Log(
				"owner resolution refused: this exact output path is recorded for task %s", taskID)
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by exact output path")
		return o.byTask[taskID], true
	}
	if obs, ok := o.byTask[target.TaskID]; ok {
		if obs.outputPath != "" && obs.outputPath != target.Path {
			bound.With(logging.Context{Level: "error"}).Log(
				"owner resolution refused: the task's authoritative output path is %s", obs.outputPath)
			return observation{}, false
		}
		bound.LogVerbose("owner resolved by task id")
		return obs, true
	}
	bound.LogVerbose("owner unknown: no spawn has been observed for this task yet")
	return observation{}, false
}

// agentFor returns the agent a task's spawn created, when one is known.
func (o *ownerIndex) agentFor(taskID string) string { return o.byTask[taskID].agentID }

// activityFor returns the spawning call's activity id, when one is known.
func (o *ownerIndex) activityFor(taskID string) string { return o.byTask[taskID].activityID }

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
