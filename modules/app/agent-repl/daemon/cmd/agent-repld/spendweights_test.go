package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/vendorspend"
)

// The skill's weights are read here, not in vendorspend: the test roster
// grants this package skills/.
func TestTheSkillsWeightsStateTheCachePrices(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile(filepath.Join("..", "..", "..", "skills", "agent-repl-metrics", "spend-weights.json"))
	if err != nil {
		t.Fatalf("read the weights: %v", err)
	}
	var weights struct {
		Kinds map[string]float64 `json:"kinds"`
	}
	if err := json.Unmarshal(raw, &weights); err != nil {
		t.Fatalf("parse the weights: %v", err)
	}
	// Assert
	if got, want := weights.Kinds[vendorspend.KindCacheRead], float64(vendorspend.CacheReadPriceTenths)/10; got != want {
		t.Errorf("the weights read %s at %v, the daemon at %v", vendorspend.KindCacheRead, got, want)
	}
	if got, want := weights.Kinds[vendorspend.KindCacheWrite1h], float64(vendorspend.CacheWritePriceTenths)/10; got != want {
		t.Errorf("the weights write %s at %v, the daemon at %v", vendorspend.KindCacheWrite1h, got, want)
	}
	for _, kind := range vendorspend.Kinds {
		if _, ok := weights.Kinds[kind]; !ok {
			t.Errorf("the weights weigh no %s", kind)
		}
	}
}
