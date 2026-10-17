package feed

import (
	"agent-repld/internal/prose"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// parsedProse is TEXT parsed for the webapp (internal/prose) at STAGE: every
// drawn text that can hold a tree is served this way, and never as a string,
// so the webapp parses nothing. A failure is recorded on S's workspace logger
// before it panics.
func (r *resolver) parsedProse(s *wsState, text string, stage prose.Stage) *frontendv1.ParsedProse {
	return prose.Parse(r.logger(s.id), text, stage)
}
