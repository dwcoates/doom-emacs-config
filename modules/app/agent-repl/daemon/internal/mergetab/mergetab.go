// Package mergetab names, once, which merge tabs are AGENTIC: the tabs whose
// content is a session's conversation (frontend.v1.FeedMergeTab — pick,
// pre-prompt, conflicts, fixes, post-prompt). A merge-tab interrupt
// (agentrepl.v1.InterruptRequest.merge_tab) is honoured only for one of them,
// so the set of agentic arms is spelled in one place.
package mergetab

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// Agentic reports whether TAB is an agentic tab.
func Agentic(tab *frontendv1.FeedMergeTab) bool {
	switch tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_Pick,
		*frontendv1.FeedMergeTab_PrePrompt,
		*frontendv1.FeedMergeTab_Conflicts,
		*frontendv1.FeedMergeTab_Fixes,
		*frontendv1.FeedMergeTab_PostPrompt:
		return true
	}
	return false
}
