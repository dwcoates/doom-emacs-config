package convert

// launch.go — WHAT A LAUNCH TELLS OWNER RESOLUTION.
//
// The converter does not resolve owners: which spool belongs to which call is IO
// and policy, and lives in the root package. But the LAUNCH RESULT is the only
// place the vendor states the mapping, and only this package reads it — so the
// fact is handed over through the Observer callback, one call per observation,
// with no shared mutable map across the package boundary.

import (
	"regexp"

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
	// A BACKGROUNDED SPAWN IS ONLY THE isAsync BRANCH. A detached shell run and a
	// workflow run are backgrounded WORK, but neither is an AGENT, so neither
	// makes a subagent its own top level.
	backgrounded := false
	switch {
	case has(result, "isAsync"):
		backgrounded = true
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
	if backgrounded {
		c.spawns[call.activityID] = spawnRecord{prompt: subagentPrompt(call, result)}
	}
	// THE THIRD FACT IS THE SPAWNER, NOT THE SPAWNED. A created agent's id is
	// the spawning call's id and needs no separate report; what the reader
	// genuinely cannot derive is WHOSE book the spawn happened in, which is what
	// a detached run's frames are attributed to.
	c.observer.TaskSpawned(taskID, call.activityID, call.agentID, output, backgrounded)
}

// backgroundSentence is the vendor's own account of a shell it launched into the
// background, as the RESULT TEXT the model reads states it:
//
//	Command running in background with ID: bmo77o6cu. Output is being written
//	to: /private/tmp/…/tasks/bmo77o6cu.output. …
//
// The shim's stream plane reads the same sentence for the output path
// (outputPathFromProse); this is its file-plane twin for the task id.
var backgroundSentence = regexp.MustCompile(`Command running in background with ID: ([A-Za-z0-9_-]+)\.`)

// backgroundLaunchFromProse restates a shell result's backgrounding SENTENCE as
// the structured launch the vendor writes beside it everywhere else, answering
// false when the result text states no launch.
//
// A SUBAGENT'S TRANSCRIPT SOMETIMES OMITS `toolUseResult` OUTRIGHT (measured on
// one session, 2026-09-23: 17 of 1409 backgrounded shell results in subagent
// transcripts, none in the main transcript). The sentence is then the ONLY
// statement that the call's work left rather than ended, and without it the
// launch was never reported: the spool sat unclaimed until its hold expired and
// was ingested as residue, so the store held no rows for the run and every
// WatchBash for it was refused — while the call itself settled as a success
// whose output was the sentence.
//
// IT IS THE VENDOR'S STATEMENT, NOT A GUESS: the id is read from the sentence
// that names it, and a result that does not carry the sentence reports nothing.
// The restated shape is the one the vendor writes for this launch (empty
// output, the task id), so every reader downstream treats both alike.
func backgroundLaunchFromProse(block map[string]any) (map[string]any, bool) {
	match := backgroundSentence.FindStringSubmatch(resultText(block["content"]))
	if match == nil {
		return nil, false
	}
	return map[string]any{
		"stdout":           "",
		"stderr":           "",
		"interrupted":      false,
		"backgroundTaskId": match[1],
	}, true
}
