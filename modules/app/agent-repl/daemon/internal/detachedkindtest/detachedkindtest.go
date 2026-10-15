// Package detachedkindtest builds the conversation.v1.DetachedWorkKind every
// detached-work announcement carries, for the tests of the views that take
// those announcements (the feed, the footer and the roster).
//
// The kind is REQUIRED (conversation.v1.AgentDetachedWork.kind): the session
// watcher refuses an announcement without one before any view takes it, and
// each view fails hard on reaching one. So a test fixture states the kind of
// the work it announces, through these, and never omits it.
package detachedkindtest

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// Subagent is a subagent's kind, running as AGENT.
func Subagent(agent *conversationv1.AgentId) *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Subagent{
		Subagent: &conversationv1.DetachedWorkKindSubagent{AgentId: agent},
	}}
}

// Bash is a shell's kind.
func Bash() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Bash{
		Bash: &conversationv1.DetachedWorkKindBash{},
	}}
}

// Monitor is a monitor's kind.
func Monitor() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Monitor{
		Monitor: &conversationv1.DetachedWorkKindMonitor{},
	}}
}

// Workflow is a workflow's kind.
func Workflow() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Workflow{
		Workflow: &conversationv1.DetachedWorkKindWorkflow{},
	}}
}
