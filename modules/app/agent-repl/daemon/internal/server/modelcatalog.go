package server

import (
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// SessionModelCatalogs holds query-owned model menus for the daemon lifetime.
// A shim republishes its menu on every gated attach, so this need not become a
// registry field; retaining it here still makes frontend reconnect snapshots
// and ordinary SessionView pushes agree while this daemon serves the session.
type SessionModelCatalogs struct {
	mu   sync.RWMutex
	byID map[string][]*frontendv1.ModelOption
}

func NewSessionModelCatalogs() *SessionModelCatalogs {
	return &SessionModelCatalogs{byID: make(map[string][]*frontendv1.ModelOption)}
}

func (c *SessionModelCatalogs) Set(sessionID string, models []*corev1.ModelOption) {
	copyModels := make([]*frontendv1.ModelOption, 0, len(models))
	for _, model := range models {
		if model == nil || model.GetValue() == "" {
			continue
		}
		copyModels = append(copyModels, &frontendv1.ModelOption{
			Value:       model.GetValue(),
			DisplayName: model.GetDisplayName(),
			Description: model.GetDescription(),
		})
	}
	c.mu.Lock()
	c.byID[sessionID] = copyModels
	c.mu.Unlock()
}

func (c *SessionModelCatalogs) Get(sessionID string) []*frontendv1.ModelOption {
	if c == nil {
		return nil
	}
	c.mu.RLock()
	models := c.byID[sessionID]
	c.mu.RUnlock()
	return append([]*frontendv1.ModelOption(nil), models...)
}
