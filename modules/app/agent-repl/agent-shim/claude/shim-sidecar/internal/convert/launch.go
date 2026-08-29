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
	taskID, agent, output := "", "", ""
	switch {
	case has(result, "isAsync"):
		// A backgrounded subagent. Its transcript is the a* spool, and its
		// prose reaches no stream at all — the path is the only place the work
		// exists.
		taskID = str(pick(result, "agentId", "agent_id"))
		agent = taskID
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
	c.log.With(at.ctxFor("launch")).With(logging.Context{TaskID: taskID, ActivityID: call.activityID, BookAgentID: agent}).
		Log("detached launch observed with output path %q", output)
	c.observer.TaskSpawned(taskID, call.activityID, agent, output)
}
