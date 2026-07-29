package server

import (
	"fmt"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
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

func (c *SessionModelCatalogs) Set(sessionID string, models []*corev1.ModelOption) error {
	if c == nil {
		return fmt.Errorf("model catalog store is nil")
	}
	if sessionID == "" {
		return fmt.Errorf("model catalog has an empty session id")
	}
	copyModels := make([]*frontendv1.ModelOption, 0, len(models))
	seen := make(map[string]struct{}, len(models))
	for index, model := range models {
		if model == nil {
			return fmt.Errorf("model catalog session=%s has nil option at index=%d", sessionID, index)
		}
		value := registry.NormalizeModel(model.GetValue())
		if value == "" {
			return fmt.Errorf("model catalog session=%s has empty or <synthetic> option at index=%d", sessionID, index)
		}
		if _, duplicate := seen[value]; duplicate {
			return fmt.Errorf("model catalog session=%s has duplicate option value=%q", sessionID, value)
		}
		seen[value] = struct{}{}
		copyModels = append(copyModels, &frontendv1.ModelOption{
			Value:       value,
			DisplayName: model.GetDisplayName(),
			Description: model.GetDescription(),
		})
	}
	c.mu.Lock()
	c.byID[sessionID] = copyModels
	c.mu.Unlock()
	return nil
}

func (c *SessionModelCatalogs) Get(sessionID string) []*frontendv1.ModelOption {
	c.mu.RLock()
	models := c.byID[sessionID]
	c.mu.RUnlock()
	copyModels := make([]*frontendv1.ModelOption, 0, len(models))
	for _, model := range models {
		copyModels = append(copyModels, &frontendv1.ModelOption{
			Value:       model.GetValue(),
			DisplayName: model.GetDisplayName(),
			Description: model.GetDescription(),
		})
	}
	return copyModels
}
