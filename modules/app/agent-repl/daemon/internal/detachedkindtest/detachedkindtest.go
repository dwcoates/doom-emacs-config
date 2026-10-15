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

// CreatedSubagent announces a subagent spawned detached outright (the
// `created` origin), owned by OWNER, under the handle WORK, running as the
// agent CREATED with SUBAGENTTYPE, started at STARTEDATMS. Its kind and the
// start it describes name the one agent, as the session watcher requires of
// a created announcement (resolveDetachedLocked refuses two that disagree).
func CreatedSubagent(owner *conversationv1.AgentId, work, created, subagentType string, startedAtMs int64) *conversationv1.AgentDetachedWork {
	agent := &conversationv1.AgentId{Value: created}
	return &conversationv1.AgentDetachedWork{
		Kind:  Subagent(agent),
		Owner: owner,
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{
				Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
					Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
						CreatedAgentId: agent,
						Prompt:         &conversationv1.AgentSubagentPrompt{SubagentType: &subagentType},
						StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: startedAtMs},
					}},
				}},
			},
		}},
	}
}
