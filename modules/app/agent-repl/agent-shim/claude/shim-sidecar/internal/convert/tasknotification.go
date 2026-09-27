package convert

// tasknotification.go — A BACKGROUND TASK'S NOTIFICATION IS ITS CONCLUSION,
// NEVER A PROMPT.
//
// When backgrounded work ends, the vendor delivers a `<task-notification>` into
// the conversation as a user-role record (`origin.kind: "task-notification"`),
// because that is how the model is told. No person typed it. The STREAM plane
// sees the same fact as the task stream's `system/task_notification` and
// settles the spawn from it (shim/src/convert/detached.ts,
// subagentTerminalEntries): `completed` → AgentSubagentSuccess carrying the
// summary as its report and the token total as `total_only`, `failed` →
// AgentSubagentFailure carrying the summary as its error content, `stopped` →
// AgentSubagentFailure.stopped_by_user — each on the SPAWN's own
// `activity:<tool_use_id>` key, restating the commission and the created agent.
//
// THIS PLANE WRITES THE SAME FRAME UNDER THE SAME KEY, so the two planes'
// settles of one run converge on one row. It settles ONLY a backgrounded agent
// spawn THIS stream read the launch of, the same refusal taskStopTerminal makes:
// the commission a settle restates is the launch's, and a settle restating an
// empty one over a row the other plane filled would erase it.
//
// Everything else a notification can say is DOCUMENTED RESIDUE
// (`user/task_notification`), never a prompt:
//   - a SHELL run's end: its terminal is the spool's own `EXIT=` line, which
//     carries the output this record does not (detached.go);
//   - a run whose launch this stream never read (a different file's, or one
//     behind this reader's cursor);
//   - a notification naming no spawning call (a monitor's event, the vendor's
//     account of runs a previous session left behind);
//   - a status the stream plane has no arm for.

import (
	"strconv"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// The statuses a notification settles a spawn with, spelled as the vendor
// writes them.
const (
	taskStatusCompleted = "completed"
	taskStatusFailed    = "failed"
	taskStatusStopped   = "stopped"
)

// taskNotice is what one notification states, read off its elements.
type taskNotice struct {
	taskID      string
	toolUseID   string
	status      string
	summary     string
	totalTokens *uint64
}

// readTaskNotice reads a notification's elements. An element the vendor did not
// write stays empty; the token total stays UNSET, because an unreported total is
// never zero.
func readTaskNotice(text string) taskNotice {
	notice := taskNotice{
		taskID:    betweenTags(text, taskIDOpen, taskIDClose),
		toolUseID: betweenTags(text, "<tool-use-id>", "</tool-use-id>"),
		status:    betweenTags(text, "<status>", "</status>"),
		summary:   betweenTags(text, "<summary>", "</summary>"),
	}
	usage := betweenTags(text, "<usage>", "</usage>")
	if raw := betweenTags(usage, "<subagent_tokens>", "</subagent_tokens>"); raw != "" {
		if total, err := strconv.ParseUint(raw, 10, 64); err == nil {
			notice.totalTokens = &total
		}
	}
	return notice
}

// taskNotification converts a notification record.
func (c *Converter) taskNotification(record map[string]any, text string, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	notice := readTaskNotice(text)
	bound := c.log.With(at.ctxFor("task-notification")).With(logging.Context{TaskID: notice.taskID, ActivityID: notice.toolUseID})
	residue := func(why string) []*storev1.StoreEntry {
		bound.LogVerbose("task notification (status=%q) withheld as vendor_specific, never a prompt: %s", notice.status, why)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, kindUserTaskNotification, record)}
	}
	if notice.toolUseID == "" {
		return residue("it names no spawning call, so no unit is settled by it")
	}
	spawn, launched := c.spawns[notice.toolUseID]
	if !launched {
		if _, shell := c.spawnedRuns[notice.taskID]; shell {
			return residue("the run is a detached shell, whose terminal is its spool's EXIT line carrying the output this record lacks")
		}
		return residue("no backgrounded agent launch on this stream opened the call it names; its launch was written elsewhere or lies behind this reader's cursor")
	}
	var activity *conversationv1.AgentActivity
	switch notice.status {
	case taskStatusCompleted:
		activity = item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				CreatedAgentId: agentID(notice.toolUseID),
				Prompt:         c.spawnPrompt(notice.toolUseID),
				Report:         &conversationv1.AgentSubagentReport{Prose: &conversationv1.AgentResponseProse{Markdown: notice.summary}},
				Totals: &conversationv1.AgentSubagentTotals{Usage: &conversationv1.AgentSubagentTotals_TotalOnly{
					TotalOnly: &conversationv1.AgentSubagentAsyncUsage{TotalTokens: notice.totalTokens},
				}},
				SettledAt: settledAt(env.timestampMs, spawn.startedAtMs),
			}},
		}})
	case taskStatusFailed:
		failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(env.timestampMs, spawn.startedAtMs)}
		if notice.summary != "" {
			failure.Content = &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
				Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: notice.summary}},
			}}}
		}
		activity = item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Error:          failure,
				Prompt:         c.spawnPrompt(notice.toolUseID),
				CreatedAgentId: agentID(notice.toolUseID),
			}},
		}})
	case taskStatusStopped:
		activity = item(&conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Cause:          &conversationv1.AgentSubagentFailure_StoppedByUser{StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{}},
				Prompt:         c.spawnPrompt(notice.toolUseID),
				CreatedAgentId: agentID(notice.toolUseID),
			}},
		}})
	default:
		return residue("the status is not one the stream plane settles a spawn with")
	}
	activity.ActivityId = activityID(notice.toolUseID)
	bound.With(logging.Context{UpsertKey: ActivityKey(notice.toolUseID)}).
		LogVerbose("task notification (status=%q) settles its backgrounded spawn, as the stream plane's task_notification does", notice.status)
	return []*storev1.StoreEntry{c.settledEntry(at, agent, notice.toolUseID, activity)}
}
