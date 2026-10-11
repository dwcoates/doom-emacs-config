// Package mergetab names, once, which merge tabs are AGENTIC: the tabs whose
// content is a session's conversation, and so the tabs that carry a switch to
// the merge workspace in the requesting workspace's copy of a detached merge's
// bubble (frontend.v1.FeedMergeTabWorkspaceSwitch). The merge run sets that
// switch and the feed resolver restates it; both reach it here, so the set of
// agentic arms is spelled in one place.
package mergetab

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// Switch answers an agentic tab's switch, nil for a tab that carries none and
// for every resolved tab.
func Switch(tab *frontendv1.FeedMergeTab) *frontendv1.FeedMergeTabWorkspaceSwitch {
	switch arm := tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_PrePrompt:
		return arm.PrePrompt.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_Conflicts:
		return arm.Conflicts.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_Fixes:
		return arm.Fixes.GetWorkspaceSwitch()
	case *frontendv1.FeedMergeTab_PostPrompt:
		return arm.PostPrompt.GetWorkspaceSwitch()
	}
	return nil
}

// SetSwitch sets SW as an agentic tab's switch and reports whether the tab is
// agentic; a resolved tab is left untouched.
func SetSwitch(tab *frontendv1.FeedMergeTab, sw *frontendv1.FeedMergeTabWorkspaceSwitch) bool {
	switch arm := tab.GetKind().(type) {
	case *frontendv1.FeedMergeTab_PrePrompt:
		arm.PrePrompt.WorkspaceSwitch = sw
	case *frontendv1.FeedMergeTab_Conflicts:
		arm.Conflicts.WorkspaceSwitch = sw
	case *frontendv1.FeedMergeTab_Fixes:
		arm.Fixes.WorkspaceSwitch = sw
	case *frontendv1.FeedMergeTab_PostPrompt:
		arm.PostPrompt.WorkspaceSwitch = sw
	default:
		return false
	}
	return true
}
