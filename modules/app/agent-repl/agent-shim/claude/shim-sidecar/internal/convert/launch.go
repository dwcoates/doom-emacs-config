package convert

// launch.go — WHAT A LAUNCH TELLS OWNER RESOLUTION.
//
// The converter does not resolve owners: which spool belongs to which call is IO
// and policy, and lives in the root package. But the LAUNCH RESULT is the only
// place the vendor states the mapping, and only this package reads it — so the
// fact is handed over through the Observer callback, one call per observation,
// with no shared mutable map across the package boundary.

import (
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// reportLaunch reads a tool result's launch signature and reports it.
//
// A result that is not a launch reports nothing. The classification is by the
// SIGNATURE KEYS the harness's launch results carry, most specific first.
func (c *Converter) reportLaunch(call openCall, result map[string]any, at Attribution) {
	if result == nil {
		return
	}
	taskID, output := "", ""
	switch {
	case has(result, "isAsync"):
		// A backgrounded subagent. Its transcript is the a* spool, and its
		// prose reaches no stream at all — the path is the only place the work
		// exists. The vendor's `agentId` is that FILE's locator, never the
		// created agent's identity: the identity is this call (the cross-plane
		// minting rule) and the reader derives it from the call id below.
		taskID = str(pick(result, "agentId", "agent_id"))
		output = str(pick(result, "outputFile", "output_file"))
	case str(pick(result, "backgroundTaskId", "background_task_id")) != "":
		// A detached shell run. Its spool is the b* file.
		taskID = str(pick(result, "backgroundTaskId", "background_task_id"))
		output = str(pick(result, "outputFile", "output_file"))
	case has(result, "runId"):
		// A workflow run. Discovered and cursor-tailed, converted only to
		// residue this wave.
		taskID = firstNonEmpty(str(result["runId"]), str(pick(result, "taskId", "task_id")))
		output = str(pick(result, "transcriptDir", "transcript_dir"))
	default:
		return
	}
	if taskID == "" {
		// A launch the harness did not name. Nothing can be attributed to it,
		// and a spool that later appears will be ingested as residue rather than
		// attached to an invented owner.
		c.log.With(at.ctxWarn("launch")).With(logging.Context{ActivityID: call.activityID}).
			Log("detached launch carries no task identity; its spool cannot be attributed to this call")
		return
	}
	c.log.With(at.ctxFor("launch")).With(logging.Context{TaskID: taskID, ActivityID: call.activityID, BookAgentID: call.agentID}).
		Log("detached launch observed with output path %q", output)
	// Remembered for THIS file's own conversions: a TaskStop result names only
	// the task, and the run it cancels is the call recorded here.
	c.spawnedRuns[taskID] = call.activityID
	// THE THIRD FACT IS THE SPAWNER, NOT THE SPAWNED. A created agent's id is
	// the spawning call's id and needs no separate report; what the reader
	// genuinely cannot derive is WHOSE book the spawn happened in, which is what
	// a detached run's frames are attributed to.
	c.observer.TaskSpawned(taskID, call.activityID, call.agentID, output)
}
