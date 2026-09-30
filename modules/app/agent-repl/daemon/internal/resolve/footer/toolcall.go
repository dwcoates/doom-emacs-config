package footer

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/sessionwatcher"
)

// THE TOOL-CALL LINE: every tool call's START raises the transient `tool_call`
// line, naming the tool as the vendor names it and a one-line gist composed
// from the call's input. The tool's name is read from the ONE helper that names
// a unit's tool (sessionwatcher.ActivityToolName), so the strip and the
// permission notifications can never name one call two ways.
//
// The arms with transient kinds of their own — a hook, a task-tracker act, a
// push notification, injected context — and the streamed reasoning and prose
// are not tool-call lines: each raises its own kind.

// toolCallStart answers the `tool_call` line for an activity that is a tool
// call's START, and false for every other frame.
func toolCallStart(act *conversationv1.AgentActivity) (*frontendv1.FooterActivityTransientToolCall, bool) {
	summary, started := toolCallSummary(act)
	if !started {
		return nil, false
	}
	name := sessionwatcher.ActivityToolName(act)
	if name == "" {
		return nil, false
	}
	call := &frontendv1.FooterActivityTransientToolCall{Tool: name}
	if gist := firstLine(summary); gist != "" {
		gist = truncate(gist, DefaultWarningRowWidth)
		call.Summary = &gist
	}
	return call, true
}

// toolCallSummary answers the call's gist and whether the frame is a tool
// call's start at all. The gist is EMPTY when the input has nothing worth a
// line, which draws the tool's name alone.
func toolCallSummary(act *conversationv1.AgentActivity) (string, bool) {
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Read:
		start := item.Read.GetStart()
		return start.GetPath().GetPath(), start != nil
	case *conversationv1.AgentActivity_Write:
		start := item.Write.GetStart()
		return start.GetPath().GetPath(), start != nil
	case *conversationv1.AgentActivity_Edit:
		start := item.Edit.GetStart()
		return start.GetPath().GetPath(), start != nil
	case *conversationv1.AgentActivity_Grep:
		start := item.Grep.GetStart()
		return start.GetQuery().GetPattern(), start != nil
	case *conversationv1.AgentActivity_Glob:
		start := item.Glob.GetStart()
		return start.GetQuery().GetPattern(), start != nil
	case *conversationv1.AgentActivity_Bash:
		start := item.Bash.GetStart()
		return start.GetCommand().GetLine(), start != nil
	case *conversationv1.AgentActivity_Subagent:
		start := item.Subagent.GetStart()
		return start.GetPrompt().GetDescription(), start != nil
	case *conversationv1.AgentActivity_SkillUse:
		start := item.SkillUse.GetStart()
		return start.GetSkill().GetName(), start != nil
	case *conversationv1.AgentActivity_WebFetch:
		start := item.WebFetch.GetStart()
		return start.GetTarget().GetUrl(), start != nil
	case *conversationv1.AgentActivity_WebSearch:
		start := item.WebSearch.GetStart()
		return start.GetQuery().GetTerms(), start != nil
	case *conversationv1.AgentActivity_SendMessage:
		start := item.SendMessage.GetStart()
		if text := start.GetSummary().GetText(); text != "" {
			return text, start != nil
		}
		return start.GetAddressedTo(), start != nil
	case *conversationv1.AgentActivity_Monitor:
		start := item.Monitor.GetStart()
		return start.GetDescription(), start != nil
	case *conversationv1.AgentActivity_ScheduleWakeup:
		start := item.ScheduleWakeup.GetStart()
		return start.GetSchedule().GetReason(), start != nil
	case *conversationv1.AgentActivity_Artifact:
		start := item.Artifact.GetStart()
		return start.GetPublish().GetFilePath(), start != nil
	case *conversationv1.AgentActivity_Cron:
		start := item.Cron.GetStart()
		return start.GetCreate().GetCron(), start != nil
	case *conversationv1.AgentActivity_McpToolCall:
		start := item.McpToolCall.GetStart()
		return start.GetTool().GetAddress().GetServer(), start != nil
	case *conversationv1.AgentActivity_PlanMode:
		return "", item.PlanMode.GetStart() != nil
	case *conversationv1.AgentActivity_ReportFindings:
		return "", item.ReportFindings.GetStart() != nil
	case *conversationv1.AgentActivity_Worktree:
		return "", item.Worktree.GetStart() != nil
	case *conversationv1.AgentActivity_SubagentHandback:
		return "", item.SubagentHandback.GetStart() != nil
	case *conversationv1.AgentActivity_Unmodeled:
		return "", item.Unmodeled.GetStart() != nil
	default:
		// Thinking, Response, Hook, TaskAct, PushNotification and
		// ContextInjected raise kinds of their own.
		return "", false
	}
}
