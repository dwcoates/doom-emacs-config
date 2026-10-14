// Package mergetab names, once, which merge tabs are AGENTIC: the tabs whose
// content is a session's conversation (frontend.v1.FeedMergeTab — pick,
// pre-prompt, a commit subtab whose conflict was handed to the session, a
// fixing sub-subtab, post-prompt). A merge-tab interrupt
// (agentrepl.v1.InterruptRequest.merge_tab) is honoured only for one of them,
// so the set of agentic arms is spelled in one place.
package mergetab

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// Agentic reports whether TAB is an agentic tab. A commit subtab is one only
// once it carries a conflict section: a commit that replayed cleanly, or
// whose conflict a recorded resolution settled, holds no agent stream.
func Agentic(tab *frontendv1.FeedMergeTab) bool {
	switch arm := tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_Resolving,
		*frontendv1.FeedMergeTab_PrePrompt,
		*frontendv1.FeedMergeTab_TestsRunFixing,
		*frontendv1.FeedMergeTab_PostPrompt:
		return true
	case *frontendv1.FeedMergeTab_RebaseCommit:
		return arm.RebaseCommit.GetConflict() != nil
	}
	return false
}
