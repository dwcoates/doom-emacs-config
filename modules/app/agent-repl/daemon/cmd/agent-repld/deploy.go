package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sync"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/sockethold"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/buildid"
	"agent-repld/internal/deploy"
	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/holderensure"
	"agent-repld/internal/ids"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/rollout"
)

// THE DAEMON'S OWN DEPLOY (internal/deploy), wired against the checkout this
// binary was deployed from, the services in the user's own service manager
// (launchd's GUI domain on macOS, systemd's user manager on Linux), and the
// connected clients the server holds.

// The deploy's operator contracts. Each exists so that a harness can keep a
// deploy away from the host: no test ever builds for real or reaches a live
// service manager.
const (
	// envDeployBuilder names ONE executable a deploy runs as
	// `<exe> --out <staging>` in place of the real build.
	envDeployBuilder = "AGENT_REPL_DEPLOY_BUILDER"
	// envRequirementsScript names the executable a deploy runs in place of
	// the checkout's bin/requirements.sh, to write a service's definition
	// and to install a socket holder.
	envRequirementsScript = "AGENT_REPL_REQUIREMENTS_SCRIPT"
	// envLaunchctl is the launchctl a service restart drives, the same
	// contract bin/store-reset.sh honors.
	envLaunchctl = "AGENT_REPL_LAUNCHCTL"
	// envLaunchAgentsDir is where the services' plists are installed, the same
	// contract bin/store-reset.sh honors.
	envLaunchAgentsDir = "AGENT_REPL_LAUNCH_AGENTS_DIR"
	// envSystemctl is the systemctl a service restart drives on Linux.
	envSystemctl = "AGENT_REPL_SYSTEMCTL"
	// envSystemdUserDir is where the services' units are installed on Linux.
	envSystemdUserDir = "AGENT_REPL_SYSTEMD_USER_DIR"
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
	// Reports are the landing reports a deploy covers and pays.
	Reports deploy.OwedReports
	Clients deploy.Clients
	Runner  deploy.Runner
	// WifiTools is the daemon's one persistent Wi-Fi tool set
	// (persistentwifi.ConfigFromEnv), so the deploy stops the helper's asks
	// with the tools every other Wi-Fi step uses: a harness's fakes under
	// test, never the host's own pkill.
	WifiTools persistentwifi.Tools
	// Store is the store's socket, which a store restart awaits.
	Store string
	// Workspace answers every workspace with a live session.
	Workspace func() []ids.WorkspaceID
	// Progress is the footer's update line, where the deploy states each phase.
	Progress deployprogress.Sink
	// Faults is the OBSERVED state client a failed deploy stands on as a
	// fault, so the footer draws it.
	Faults deploy.Faults
	// Joining is a successor still joining: its state handle is read-only, so
	// it leaves the standing deploy faults alone.
	Joining bool
	Getenv  func(string) string
	// Deployed is told each time a deploy gets all the way through.
	Deployed func()
	// Restarts records how long each deploy phase and service restart took.
	Restarts restarttiming.Recorder
	// Reconnects times each reloaded webview's return.
	Reconnects *restarttiming.Reconnects
}

// deployPaths are the host locations a deploy works against.
type deployPaths struct {
	cacheBin, plistDir, unitDir, storeLog, reportDir string
	// wifiApp is where the deploy installs the Wi-Fi helper app; empty on a
	// platform with no helper (wifiAppOn).
	wifiApp string
}

// wifiAppOn is where a deploy on GOOS installs the Wi-Fi helper app under
// HOME: the failover's own (persistentwifi.WifiAppIn) on macOS, the one
// platform the helper is built for, and nowhere elsewhere.
func wifiAppOn(goos, home string) string {
	if goos != "darwin" {
		return ""
	}
	return persistentwifi.WifiAppIn(home)
}

// resolveDeployPaths answers the host locations, the operator overrides first.
func resolveDeployPaths(getenv func(string) string) (deployPaths, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return deployPaths{}, fmt.Errorf("agent-repld: resolve the home directory the services live under: %w", err)
	}
	reportDir, err := buildreport.ResolveDir(getenv)
	if err != nil {
		return deployPaths{}, fmt.Errorf("agent-repld: resolve the services' build-report directory: %w", err)
	}
	return deployPaths{
		cacheBin:  filepath.Join(home, ".cache", "agent-repl", "bin"),
		plistDir:  firstNonEmpty(getenv(envLaunchAgentsDir), filepath.Join(home, "Library", "LaunchAgents")),
		unitDir:   firstNonEmpty(getenv(envSystemdUserDir), filepath.Join(firstNonEmpty(getenv("XDG_CONFIG_HOME"), filepath.Join(home, ".config")), "systemd", "user")),
		storeLog:  filepath.Join(home, ".cache", "agent-repl", "log", "shim-store.err.log"),
		reportDir: reportDir,
		wifiApp:   wifiAppOn(runtime.GOOS, home),
	}, nil
}

// serviceManagerFor is the service manager the services live in on goos:
// launchd on macOS, systemd's user manager on Linux. Any other system has none
// agent-repl knows, which the boot refuses loudly.
func serviceManagerFor(goos string, where deployPaths, p deployerParams) (deploy.ServiceManager, error) {
	switch goos {
	case "darwin":
		return &deploy.Launchctl{
			Binary:   firstNonEmpty(p.Getenv(envLaunchctl), "launchctl"),
			UID:      os.Getuid(),
			PlistDir: where.plistDir,
			Dir:      p.Checkout,
			Runner:   p.Runner,
		}, nil
	case "linux":
		return &deploy.SystemdUser{
			Binary:  firstNonEmpty(p.Getenv(envSystemctl), "systemctl"),
			UnitDir: where.unitDir,
			Dir:     p.Checkout,
			Runner:  p.Runner,
		}, nil
	default:
		return nil, fmt.Errorf("agent-repld: %s has no service manager agent-repl knows (launchd on darwin, systemd on linux) to run the store, sidecar and zettel under", goos)
	}
}

// buildDeployer builds the daemon's deploy. The daemon's own build is the
// content hash of the binary it was exec'd from, taken HERE, at boot: an
// install replaces the file under it, and the running process is still the
// old build.
// buildDeployer also answers its service restarter, which the boot's bring-up
// ensures the launchd services with.
func buildDeployer(ctx context.Context, p deployerParams) (*deploy.Deployer, *deploy.Restarter, error) {
	log := p.Surfaces.Global()
	selfBuild, err := buildid.FileCached(p.SelfExe)
	if err != nil {
		return nil, nil, fmt.Errorf("agent-repld: hash this daemon's own binary %s: %w", p.SelfExe, err)
	}
	where, err := resolveDeployPaths(p.Getenv)
	if err != nil {
		return nil, nil, err
	}
	deployDir := filepath.Join(p.StateDir, "deploy")
	builder := &deploy.ScriptBuilder{
		ModuleRoot: p.Checkout,
		LogDir:     filepath.Join(deployDir, "logs"),
		Override:   p.Getenv(envDeployBuilder),
		WifiHelper: where.wifiApp != "",
		Runner:     p.Runner,
		Clock:      rollout.SystemClock{},
		Log:        log,
	}
	services, err := serviceManagerFor(runtime.GOOS, where, p)
	if err != nil {
		return nil, nil, err
	}
	requirements := deploy.ScriptRequirements{ModuleRoot: p.Checkout, Script: p.Getenv(envRequirementsScript), Runner: p.Runner}
	home, err := os.UserHomeDir()
	if err != nil {
		return nil, nil, fmt.Errorf("agent-repld: the home directory a holder's control socket expands against: %w", err)
	}
	holders, err := holderensure.Declared(p.Checkout, home)
	if err != nil {
		return nil, nil, fmt.Errorf("agent-repld: the socket holders the deploy brings up: %w", err)
	}
	// The store takes its sockets from the holder that holds its socket, if
	// one is declared (holderensure.HolderOfSocket).
	storeHolder, _ := holderensure.HolderOfSocket(holders, p.Store)
	ensurer := &holderensure.Ensurer{Holders: holders, Probe: sockethold.Probe, Install: requirements.EnsureHolder, Log: log}
	restarter := &deploy.Restarter{
		Services:      services,
		StoreSocket:   p.Store,
		StoreLog:      where.storeLog,
		Windows:       deploy.DefaultServiceWindows,
		Clock:         rollout.SystemClock{},
		Log:           log,
		CacheBin:      where.cacheBin,
		ReportDir:     where.reportDir,
		Restarts:      p.Restarts,
		Define:        requirements.Define,
		EnsureHolder:  ensurer.EnsureID,
		HolderServing: ensurer.ServingID,
		StoreHolder:   storeHolder,
	}
	deployer, err := deploy.New(deploy.Deps{
		Restarts:   p.Restarts,
		Reconnects: p.Reconnects,
		Live: deploy.Live{
			ModuleRoot: p.Checkout, CacheBin: where.cacheBin,
			Shim: p.ShimMain, Webapp: p.WebappDist, WifiApp: where.wifiApp,
		},
		StagingRoot: filepath.Join(deployDir, "staging"),
		Builder:     builder,
		Bundle:      p.Bundle,
		DaemonBuild: selfBuild,
		Rollout:     p.Rollout,
		Reports:     p.Reports,
		Workspaces: func(context.Context) ([]ids.WorkspaceID, error) {
			return p.Workspace(), nil
		},
		Clients:  p.Clients,
		Services: restarter,
		StopWifiAsks: func(ctx context.Context) (bool, error) {
			tools := p.WifiTools
			tools.WifiApp = where.wifiApp
			return persistentwifi.StopAsks(ctx, p.Runner, tools)
		},
		ReportDir: where.reportDir,
		Lifetime:  ctx,
		Progress:  p.Progress,
		Faults:    p.Faults,
		Deployed:  p.Deployed,
		Log:       p.Surfaces,
	})
	if err != nil {
		return nil, nil, fmt.Errorf("agent-repld: build the deploy: %w", err)
	}
	if p.Joining {
		log.Debug(graphOperation, "a joining successor leaves the standing deploy faults to the incumbent that opened them", nil)
	} else {
		deployer.CloseEarlierFailures(ctx)
	}
	log.Info(graphOperation, "the deploy is wired", dlog.Context{
		"daemon_build": selfBuild, "checkout": p.Checkout, "staging": filepath.Join(deployDir, "staging"),
		"builder_override": builder.Override != "", "report_dir": where.reportDir,
	})
	return deployer, restarter, nil
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

func (f *deployClientsForwarder) PushReloadWebappBuild(ws ids.WorkspaceID, build string) {
	if target, ok := f.bound(); ok {
		target.PushReloadWebappBuild(ws, build)
	}
}

func (f *deployClientsForwarder) WebhostPages() []*agentreplv1.WebhostPage {
	if target, ok := f.bound(); ok {
		return target.WebhostPages()
	}
	return nil
}

func (f *deployClientsForwarder) PushReloadWebhost(build string) int {
	if target, ok := f.bound(); ok {
		return target.PushReloadWebhost(build)
	}
	return 0
}
