package main

import (
	"context"
	"errors"
	"fmt"
	"sync"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
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

// titleConfigDirs resolves a workspace's account config dir for the synthesized
// title's headless call, so its tokens are billed to the SAME account the
// session spends as. It reuses the daemon's one workspace-dir lookup and the
// account resolver's one repo-under-root rule, so the title call and the
// session can never disagree about which account a workspace spends from.
type titleConfigDirs struct {
	workspaceDir func(context.Context, ids.WorkspaceID) (string, error)
	accounts     account.Resolver
}

// ConfigDirFor answers the workspace's account root, or the error that kept
// the workspace's directory from being read (a workspace forgotten
// mid-flight, the caller's context ending as the daemon stands down). Each
// caller records the failure at the level its own outcome warrants.
func (c titleConfigDirs) ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	dir, err := c.workspaceDir(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("read the directory of workspace %q: %w", ws, err)
	}
	return c.accounts.ConfigDirFor(dir), nil
}
