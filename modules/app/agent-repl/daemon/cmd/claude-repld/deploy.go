package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/buildid"
	"claude-repld/internal/deploy"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// THE DAEMON'S OWN DEPLOY (internal/deploy), wired against the checkout this
// binary was deployed from, the launchd services in the user's GUI domain, and
// the connected clients the server holds.

// The deploy's operator contracts. Each exists so that a harness can keep a
// deploy away from the host: no test ever builds for real or reaches the live
// launchd.
const (
	// envDeployBuilder names ONE executable a deploy runs as
	// `<exe> --out <staging>` in place of the real build.
	envDeployBuilder = "AGENT_REPL_DEPLOY_BUILDER"
	// envLaunchctl is the launchctl a service restart drives, the same
	// contract bin/store-reset.sh honors.
	envLaunchctl = "AGENT_REPL_LAUNCHCTL"
	// envLaunchAgentsDir is where the services' plists are installed, the same
	// contract bin/store-reset.sh honors.
	envLaunchAgentsDir = "AGENT_REPL_LAUNCH_AGENTS_DIR"
)

// deployerParams are what buildDeployer needs from the composition root.
type deployerParams struct {
	Surfaces dlog.Surfaces
	// Checkout is the agent-repl module root the daemon was deployed from.
	Checkout string
	// ShimMain and WebappDist are where the daemon runs the shim bundle and
	// serves the webapp from.
	ShimMain, WebappDist string
	// StateDir is the state root; staging and build logs live under it.
	StateDir string
	// SelfExe is this daemon's binary, whose content hash is its build.
	SelfExe string
	Bundle  *buildid.ShimBundle
	Rollout deploy.Rollout
	Clients deploy.Clients
	Runner  deploy.Runner
	// Store is the store's socket, which a store restart awaits.
	Store string
	// Workspace answers every workspace with a live session.
	Workspace func() []ids.WorkspaceID
	Getenv    func(string) string
}

// deployPaths are the host locations a deploy works against.
type deployPaths struct {
	cacheBin, plistDir, storeLog, reportDir string
}

// resolveDeployPaths answers the host locations, the operator overrides first.
func resolveDeployPaths(getenv func(string) string) (deployPaths, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return deployPaths{}, fmt.Errorf("claude-repld: resolve the home directory the services live under: %w", err)
	}
	reportDir, err := buildreport.ResolveDir(getenv)
	if err != nil {
		return deployPaths{}, fmt.Errorf("claude-repld: resolve the services' build-report directory: %w", err)
	}
	return deployPaths{
		cacheBin:  filepath.Join(home, ".cache", "agent-repl", "bin"),
		plistDir:  firstNonEmpty(getenv(envLaunchAgentsDir), filepath.Join(home, "Library", "LaunchAgents")),
		storeLog:  filepath.Join(home, ".cache", "agent-repl", "log", "shim-store.err.log"),
		reportDir: reportDir,
	}, nil
}

// buildDeployer builds the daemon's deploy. The daemon's own build is the
// content hash of the binary it was exec'd from, taken HERE, at boot: an
// install replaces the file under it, and the running process is still the
// old build.
func buildDeployer(ctx context.Context, p deployerParams) (*deploy.Deployer, error) {
	log := p.Surfaces.Global()
	selfBuild, err := buildid.File(p.SelfExe)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: hash this daemon's own binary %s: %w", p.SelfExe, err)
	}
	where, err := resolveDeployPaths(p.Getenv)
	if err != nil {
		return nil, err
	}
	deployDir := filepath.Join(p.StateDir, "deploy")
	builder := &deploy.ScriptBuilder{
		ModuleRoot: p.Checkout,
		LogDir:     filepath.Join(deployDir, "logs"),
		Override:   p.Getenv(envDeployBuilder),
		Runner:     p.Runner,
		Clock:      rollout.SystemClock{},
		Log:        log,
	}
	restarter := &deploy.Restarter{
		Launchd: &deploy.Launchctl{
			Binary: firstNonEmpty(p.Getenv(envLaunchctl), "launchctl"),
			UID:    os.Getuid(),
			Dir:    p.Checkout,
			Runner: p.Runner,
		},
		PlistDir:    where.plistDir,
		StoreSocket: p.Store,
		StoreLog:    where.storeLog,
		Windows:     deploy.DefaultServiceWindows,
		Clock:       rollout.SystemClock{},
		Log:         log,
	}
	deployer, err := deploy.New(deploy.Deps{
		Live: deploy.Live{
			ModuleRoot: p.Checkout, CacheBin: where.cacheBin,
			Shim: p.ShimMain, Webapp: p.WebappDist,
		},
		StagingRoot: filepath.Join(deployDir, "staging"),
		Builder:     builder,
		Bundle:      p.Bundle,
		DaemonBuild: selfBuild,
		Rollout:     p.Rollout,
		Workspaces: func(context.Context) ([]ids.WorkspaceID, error) {
			return p.Workspace(), nil
		},
		Clients:   p.Clients,
		Services:  restarter,
		ReportDir: where.reportDir,
		Lifetime:  ctx,
		Log:       p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the deploy: %w", err)
	}
	log.Info(graphOperation, "the deploy is wired", dlog.Context{
		"daemon_build": selfBuild, "checkout": p.Checkout, "staging": filepath.Join(deployDir, "staging"),
		"builder_override": builder.Override != "", "report_dir": where.reportDir,
	})
	return deployer, nil
}

// rolloutForwarder carries the session watcher's build reports to the rollout
// controller, which is built after the fleet the watchers live in.
type rolloutForwarder struct {
	mu     sync.RWMutex
	target rollout.Controller
}

func (f *rolloutForwarder) bind(target rollout.Controller) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *rolloutForwarder) controller() (rollout.Controller, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.target, f.target != nil
}

// deployClientsForwarder carries the deploy's reads of the connected clients'
// builds, and its reload pushes, to the server, which is built after the
// deploy. With no server yet no client can be connected, so the reads answer
// none — the truth rather than a stand-in for it — and a push reaches nobody.
type deployClientsForwarder struct {
	mu     sync.RWMutex
	target deploy.Clients
}

var _ deploy.Clients = (*deployClientsForwarder)(nil)

func (f *deployClientsForwarder) bind(target deploy.Clients) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *deployClientsForwarder) bound() (deploy.Clients, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.target, f.target != nil
}

func (f *deployClientsForwarder) EmacsBuilds() []deploy.EmacsClient {
	if target, ok := f.bound(); ok {
		return target.EmacsBuilds()
	}
	return nil
}

func (f *deployClientsForwarder) PushReloadElisp(streams []string, moduleRoot, build string) int {
	if target, ok := f.bound(); ok {
		return target.PushReloadElisp(streams, moduleRoot, build)
	}
	return 0
}

func (f *deployClientsForwarder) WebviewBuilds() map[ids.WorkspaceID][]string {
	if target, ok := f.bound(); ok {
		return target.WebviewBuilds()
	}
	return nil
}

func (f *deployClientsForwarder) PushReloadWebapp(ws ids.WorkspaceID) {
	if target, ok := f.bound(); ok {
		target.PushReloadWebapp(ws)
	}
}
