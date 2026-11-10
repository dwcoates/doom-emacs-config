package server

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
)

// ComposerMentions is each workspace's record of the names its own final
// responses mentioned (internal/composermentions), which the host stream
// serves to Emacs as its `mentions` arm so the composer orders its
// completions by them.
type ComposerMentions interface {
	// Topic answers the workspace's record, holding the record in force so a
	// fresh subscriber is handed it first.
	Topic(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.HostWorkspaceMentions]
}
