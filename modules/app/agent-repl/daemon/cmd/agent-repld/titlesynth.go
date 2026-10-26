package main

import (
	"context"
	"errors"
	"sync"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// digestForwarder carries the title synthesizer's GatherTitleDigest call to the
// workspace fleet, which is built AFTER the synthesizer it feeds (the
// synthesizer rides the fleet's own session-watch sinks). It is bound once the
// fleet exists, exactly like verbsForwarder and healthForwarder.
type digestForwarder struct {
	mu    sync.RWMutex
	fleet *workspace.Fleet
}

func (f *digestForwarder) bind(fleet *workspace.Fleet) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.fleet = fleet
}

// GatherTitleDigest asks the workspace's live shim for its title digest. A
// workspace with no live shim (hibernated, cold-gated, or never started) has no
// transcript to read here, which is not a fault — the synthesizer keeps the
// workspace name.
func (f *digestForwarder) GatherTitleDigest(ctx context.Context, ws ids.WorkspaceID) (*shimv1.GatherTitleDigestResponse, error) {
	f.mu.RLock()
	fleet := f.fleet
	f.mu.RUnlock()
	if fleet == nil {
		return nil, errors.New("titlesynth: the fleet is not bound yet")
	}
	client, ok := fleet.Client(ws)
	if !ok {
		return nil, errors.New("titlesynth: no live shim for the workspace")
	}
	return client.GatherTitleDigest(ctx, &shimv1.GatherTitleDigestRequest{})
}
