package scenariogolden

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
)

// nop is every session watcher sink but the feed's, drawing nothing: a
// projection golden reads the root feed alone.
type nop struct{}

func (nop) OnTurnRunningAtAttach(ids.WorkspaceID, ids.TurnID, *time.Time)                      {}
func (nop) OnTurnOpened(ids.WorkspaceID, ids.TurnID)                                           {}
func (nop) OnMainAgent(ids.WorkspaceID, *conversationv1.AgentId)                               {}
func (nop) OnActivity(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.AgentActivity) {}
func (nop) OnQuestion(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.AgentQuestion) {}
func (nop) OnPermission(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.AgentPermission) {
}
func (nop) OnApiError(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.ApiRequestFailed) {
}
func (nop) OnContextCut(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.ContextCut) {}
func (nop) OnContextCutRestated(ids.WorkspaceID, *conversationv1.ContextCut)                  {}
func (nop) OnAgentTerminal(ids.WorkspaceID, *conversationv1.AgentId, *ids.TurnID, *conversationv1.AgentSuccess, *conversationv1.AgentFailure) {
}
func (nop) OnDetachedWork(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.AgentDetachedWork) {
}
func (nop) OnBash(ids.WorkspaceID, *conversationv1.DetachedWorkId, *conversationv1.AgentBash) {}
func (nop) OnSubagent(ids.WorkspaceID, *conversationv1.DetachedWorkId, *conversationv1.AgentSubagent) {
}
func (nop) OnSessionUpdate(ids.WorkspaceID, *conversationv1.SessionUpdate)                      {}
func (nop) OnSessionStarted(ids.WorkspaceID, *conversationv1.SessionStarted)                    {}
func (nop) OnHistoryPage(ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPage) {}
func (nop) OnLink(ids.WorkspaceID, sessionwatcher.LinkState)                                    {}
func (nop) OnLiveWorkChanged(ids.WorkspaceID, sessionwatcher.LiveWorkSet)                       {}
func (nop) OnTurnEnded(ids.WorkspaceID, ids.TurnID, sessionwatcher.TurnClose)                   {}
func (nop) OnTurnAdopted(ids.WorkspaceID, sessionwatcher.AdoptedTurn)                           {}
func (nop) OnTurnsEndedUnobserved(ids.WorkspaceID, []ids.TurnID)                                {}
func (nop) OnQueryDied(ids.WorkspaceID)                                                         {}
func (nop) OnSkillsChanged(ids.WorkspaceID, []string)                                           {}
func (nop) OnVendorSessionID(ids.WorkspaceID, string)                                           {}
func (nop) OnFree(ids.WorkspaceID, bool)                                                        {}
func (nop) OnNotification(ids.WorkspaceID, sessionwatcher.HostNotification)                     {}
func (nop) OnAsksSettled(ids.WorkspaceID)                                                       {}
func (nop) OnLinkFault(ids.WorkspaceID, sessionwatcher.LinkFault)                               {}
func (nop) OnDeparted(ids.WorkspaceID, sessionwatcher.Watcher, sessionwatcher.Departure)        {}
func (nop) OnWatchOpenRefused(ids.WorkspaceID, sessionwatcher.WatchOpenRefusal)                 {}
func (nop) OnWatchOpened(ids.WorkspaceID)                                                       {}
func (nop) OnLinkRestored(ids.WorkspaceID)                                                      {}
func (nop) OnLinkChanged(ids.WorkspaceID, bool)                                                 {}
func (nop) OnSessionDiagnostics(ids.WorkspaceID, *conversationv1.SessionDiagnostics)            {}
func (nop) OnCommandCatalog(ids.WorkspaceID, *conversationv1.SessionCommandCatalog)             {}
func (nop) OnCacheLapse(ids.WorkspaceID, *conversationv1.SessionCacheLapse)                     {}
func (nop) OnKeepaliveFailed(ids.WorkspaceID, *conversationv1.SessionKeepaliveFailed)           {}
