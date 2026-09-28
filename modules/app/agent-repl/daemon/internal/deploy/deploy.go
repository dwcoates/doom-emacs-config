// Package deploy is how the DAEMON deploys (owner design, 2026-09-23). There
// is no deploy script: a caller asks (the Deploy rpc, which the
// `claude-repld deploy` verb and Emacs's `agent-repl-deploy` both call), or a
// complete change lands on master through the daemon's own merge, and the
// daemon
//
//  1. BUILDS every component into a staging directory (a failure deploys
//     nothing and is the caller's answer),
//  2. decides what is OUT OF DATE by content hash — each running process's
//     reported build against the fresh one, never "did this build change the
//     file",
//  3. INSTALLS the staged artifacts, and
//  4. restarts what is out of date, each WHEN it may: the store and sidecar at
//     once in the recorded safe order; the daemon by the blue-green handover;
//     each stale shim through the prompt queue's bounce registry (at once when
//     free, when its work ends otherwise); elisp and the webapp by a pushed
//     reload.
//
// AN UNFORCED DEPLOY ENDS NO TURN. A forced one does not wait.
package deploy

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"syscall"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/buildid"
	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

// The deploy's operations. Every decision — built, stale, bounced now,
// registered, restarted, pushed, handed over, forced — is recorded under one
// of them with the component and the builds compared.
const (
	opDeploy  = "daemon.deploy.run"
	opDecide  = "daemon.deploy.decide"
	opLanding = "daemon.deploy.landing"
)

// Component is a deployable part of the stack.
type Component string

// The components, in the order a deploy reports them.
const (
	ComponentStore   Component = "store"
	ComponentSidecar Component = "sidecar"
	ComponentElisp   Component = "elisp"
	ComponentDaemon  Component = "daemon"
	ComponentShim    Component = "shim"
	ComponentWebapp  Component = "webapp"
)

// Arm names a component on the wire.
func (c Component) Arm() agentreplv1.DeployComponent {
	switch c {
	case ComponentDaemon:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON
	case ComponentShim:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM
	case ComponentWebapp:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP
	case ComponentStore:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE
	case ComponentSidecar:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR
	case ComponentElisp:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP
	default:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_UNSPECIFIED
	}
}

// OutcomeKind is what a deploy decided for one component.
type OutcomeKind string

// The decisions.
const (
	UpToDate            OutcomeKind = "up_to_date"
	Restarted           OutcomeKind = "restarted"
	HandingOver         OutcomeKind = "handing_over"
	ShimsBouncing       OutcomeKind = "shims"
	ReloadPushed        OutcomeKind = "reload_pushed"
	DeferredToSuccessor OutcomeKind = "deferred_to_successor"
	// RestartingAcrossLayout is a stale daemon whose fresh build writes a
	// DIFFERENT state layout: it is rolled out stop-then-start
	// (rollout.Controller.Restart), never handed over, because a joining
	// successor cannot carry an older layout forward on its read-only handle.
	RestartingAcrossLayout OutcomeKind = "restarting_across_layout"
)

// Outcome is one component's decision.
type Outcome struct {
	Component Component
	// Build is the fresh build's content hash.
	Build string
	Kind  OutcomeKind
	// Handover is set for HandingOver and RestartingAcrossLayout.
	Handover rollout.HandoverAcceptance
	// Layouts is set for RestartingAcrossLayout: the running and the fresh
	// state layout.
	Layouts LayoutChange
	// Shims is set for ShimsBouncing: one entry per stale shim.
	Shims []ShimBounce
	// Recipients is set for ReloadPushed.
	Recipients int
}

// LayoutChange is the state layout a running daemon writes and the one its
// fresh build writes.
type LayoutChange struct {
	Running, Fresh int
}

// ShimBounce is one stale shim and the registry's decision for it.
type ShimBounce struct {
	Workspace ids.WorkspaceID
	Decision  bounce.Decision
}

// Result is a whole deploy's decisions, one per component.
type Result struct {
	Outcomes []Outcome
}

// ErrAlreadyDeploying refuses a deploy asked for while one runs.
var ErrAlreadyDeploying = errors.New("deploy: a deploy is already running")

// ServiceRestartFailed is a launchd service that did not come back onto the
// fresh build. Its artifacts ARE installed.
type ServiceRestartFailed struct {
	Component Component
	Detail    string
}

func (e *ServiceRestartFailed) Error() string {
	return fmt.Sprintf("deploy: restart the %s: %s", e.Component, e.Detail)
}

// Rollout is the slice of the rollout controller a deploy acts through.
type Rollout interface {
	HandOver(ctx context.Context, force bool) (rollout.HandoverAcceptance, error)
	Restart(ctx context.Context, force bool) (rollout.HandoverAcceptance, error)
	CheckStaleness(ctx context.Context, ws ids.WorkspaceID, force bool) (rollout.StaleCheck, error)
	Joining() bool
	RollingOut() ([]ids.WorkspaceID, bool)
}

// EmacsClient is one open Emacs WatchDaemon stream and the elisp it reported.
type EmacsClient struct {
	ID    string
	Build string
}

// Clients are the connected clients whose builds a deploy compares: every
// Emacs states its elisp on WatchDaemon, every webview its webapp on
// WatchWebWorkspace.
type Clients interface {
	// EmacsBuilds answers every open Emacs stream's reported elisp build.
	EmacsBuilds() []EmacsClient
	// PushReloadElisp pushes reload_elisp to the named Emacs streams and
	// answers how many it reached.
	PushReloadElisp(streams []string, moduleRoot, build string) int
	// WebviewBuilds answers, per workspace, the webapp build every open
	// webview of it reported.
	WebviewBuilds() map[ids.WorkspaceID][]string
	// PushReloadWebapp pushes reload_webapp on one workspace's host stream.
	PushReloadWebapp(ws ids.WorkspaceID)
}

// Services restarts the launchd services.
type Services interface {
	RestartStore(ctx context.Context) error
	RestartSidecar(ctx context.Context) error
}

// Clock is the deploy's view of time.
type Clock = rollout.Clock

// Deps are a Deployer's collaborators.
type Deps struct {
	// Live names the installed artifacts.
	Live Live
	// StagingRoot is where each deploy's staging directory is made.
	StagingRoot string
	Builder     Builder
	// Bundle guards the installed shim bundle: it is replaced only while no
	// spawn holds it.
	Bundle *buildid.ShimBundle
	// DaemonBuild is THIS process's build: the content hash of the binary it
	// was exec'd from, taken at boot.
	DaemonBuild string
	Rollout     Rollout
	// Workspaces answers every registered workspace; the staleness check
	// skips the ones with no live shim.
	Workspaces func(ctx context.Context) ([]ids.WorkspaceID, error)
	Clients    Clients
	Services   Services
	// ReportDir is where the services write their build reports.
	ReportDir string
	// Alive reports whether a process is running; nil is the kernel's answer.
	Alive func(pid int) bool
	// Lifetime bounds the deploys a landing starts; nil leaves them bounded by
	// the process alone.
	Lifetime context.Context
	// StateLayout answers the state layout a daemon binary writes; nil is
	// BinaryLayout, which asks the binary itself.
	StateLayout func(ctx context.Context, bin string) (int, error)
	// RunningLayout is the state layout THIS process writes; zero is
	// wsm.LayoutVersion, which is what this binary was built with.
	RunningLayout int
	Clock         Clock
	// Progress is where the deploy states each phase it enters: the footer's
	// update line on every workspace's strip (owner request, 2026-09-27). It
	// is the ONE entry point for that line.
	Progress deployprogress.Sink
	// Faults is the state client a failed build, install or service restart
	// is recorded through as the daemon-scoped `deploy_failed` fault, and
	// through which a step a later deploy gets through closes it. It must be
	// the OBSERVED client (health.ObserveFaults), so the footer draws it.
	Faults Faults
	Log    dlog.Surfaces
}

// Deployer runs deploys, one at a time.
type Deployer struct {
	deps Deps
	log  dlog.Logger

	mu sync.Mutex
	// running is true while a deploy runs.
	running bool
	// landed is a landing no deploy has covered yet: it arrived while no
	// deploy was running and none could start, or while one was already past
	// its start.
	landed bool
	// landings joins the deploys landings started.
	landings sync.WaitGroup
}

// New builds a Deployer.
func New(deps Deps) (*Deployer, error) {
	switch {
	case deps.Log == nil:
		return nil, errors.New("deploy: log surfaces are required")
	case deps.Builder == nil:
		return nil, errors.New("deploy: a builder is required")
	case deps.Bundle == nil:
		return nil, errors.New("deploy: the shim bundle guard is required")
	case deps.Rollout == nil:
		return nil, errors.New("deploy: the rollout controller is required")
	case deps.Workspaces == nil:
		return nil, errors.New("deploy: the workspace list is required")
	case deps.Clients == nil:
		return nil, errors.New("deploy: the connected clients are required")
	case deps.Services == nil:
		return nil, errors.New("deploy: the service restarter is required")
	case deps.Progress == nil:
		return nil, errors.New("deploy: the progress sink is required")
	case deps.Faults == nil:
		return nil, errors.New("deploy: the fault recorder is required")
	case deps.DaemonBuild == "":
		return nil, errors.New("deploy: this daemon's own build is required")
	case deps.StagingRoot == "" || deps.ReportDir == "":
		return nil, errors.New("deploy: the staging root and the report directory are required")
	}
	if deps.Clock == nil {
		deps.Clock = rollout.SystemClock{}
	}
	if deps.Alive == nil {
		deps.Alive = processAlive
	}
	if deps.StateLayout == nil {
		deps.StateLayout = BinaryLayout
	}
	if deps.RunningLayout == 0 {
		deps.RunningLayout = wsm.LayoutVersion
	}
	return &Deployer{deps: deps, log: deps.Log.Global()}, nil
}

// Deploy builds, installs and puts into service what is out of date. See the
// package doc. It answers the DECISIONS, not their completion.
func (d *Deployer) Deploy(ctx context.Context, force bool) (result Result, err error) {
	fields := dlog.Context{"forced": force, "daemon_build": d.deps.DaemonBuild}
	if !d.begin() {
		d.log.Info(opDeploy, "refused a deploy while one is running", fields)
		return Result{}, ErrAlreadyDeploying
	}
	defer d.end()

	if d.deps.Rollout.Joining() {
		d.log.Info(opDeploy, "refused a deploy asked of a successor still joining", fields)
		return Result{}, rollout.ErrJoining
	}
	if waiting, rolling := d.deps.Rollout.RollingOut(); rolling {
		d.log.Info(opDeploy, "refused a deploy while a handover is in flight", merge(fields, dlog.Context{"waiting_on": wsNames(waiting)}))
		return Result{}, &rollout.ErrAlreadyRollingOut{WaitingOn: waiting}
	}

	// FROM HERE THE LINE IS THIS DEPLOY'S. A deploy that stops short of the
	// phase that ends it (updated here, or a handover whose successor says it)
	// takes its line down, and a build, an install or a service restart that
	// failed stands as the `deploy_failed` fault line: the rpc's answer
	// reaches only its caller, and a landing's deploy has none. A handover
	// whose successor would not start is the rollout's own fault.
	d.progress(&deployprogress.Progress{Phase: deployprogress.Building, Components: builtComponents})
	defer func() {
		if err != nil {
			d.progress(nil)
			d.recordFailure(ctx, err)
		}
	}()

	nonce := nonceOf(d.deps.Clock.Now().UnixNano())
	staging := filepath.Join(d.deps.StagingRoot, nonce)
	if err := os.MkdirAll(staging, 0o755); err != nil {
		d.log.Error(opDeploy, "could not create the staging directory; nothing was built", withCause(merge(fields, dlog.Context{"staging": staging}), err))
		return Result{}, &BuildFailed{Step: "setup", Detail: err.Error(), Log: staging}
	}
	defer func() {
		if err := os.RemoveAll(staging); err != nil {
			d.log.Error(opDeploy, "could not remove the staging directory", withCause(dlog.Context{"staging": staging}, err))
		}
	}()
	fields["staging"] = staging
	d.log.Info(opDeploy, "deploying: building every component into staging", fields)

	if err = d.deps.Builder.Build(ctx, staging); err != nil {
		var failed *BuildFailed
		if !errors.As(err, &failed) {
			failed = &BuildFailed{Step: "build", Detail: err.Error()}
		}
		d.log.Error(opDeploy, "the build failed; NOTHING WAS DEPLOYED", merge(fields, dlog.Context{
			"step": failed.Step, "detail": failed.Detail, "log": failed.Log,
		}))
		return Result{}, failed
	}

	fresh, err := d.hashStaged(Staged{Dir: staging})
	if err != nil {
		d.log.Error(opDeploy, "a staged artifact could not be hashed; NOTHING WAS DEPLOYED", withCause(fields, err))
		return Result{}, err
	}
	d.log.Info(opDeploy, "built", merge(fields, dlog.Context{"builds": fresh.fields()}))
	d.stepSucceeded(ctx, health.DeployStepBuild)

	d.progress(&deployprogress.Progress{Phase: deployprogress.Installing})
	if err = d.install(Staged{Dir: staging}, fresh, nonce); err != nil {
		d.log.Error(opDeploy, "the staged build could not be installed; nothing was restarted", withCause(fields, err))
		return Result{}, err
	}
	d.stepSucceeded(ctx, health.DeployStepInstall)

	services, err := d.services(ctx, fresh)
	result.Outcomes = append(result.Outcomes, services...)
	if err != nil {
		return result, err
	}
	d.stepSucceeded(ctx, health.DeployStepRestartServices)
	result.Outcomes = append(result.Outcomes, d.elisp(fresh))
	rest, err := d.daemonShimWebapp(ctx, Staged{Dir: staging}, fresh, force)
	result.Outcomes = append(result.Outcomes, rest...)
	if err != nil {
		return result, err
	}
	d.log.Info(opDeploy, "deployed: every component decided", merge(fields, dlog.Context{"decisions": summarize(result)}))
	if !movesDaemon(result) {
		// THIS DAEMON STAYS, so it ends its own story. A daemon that moves
		// leaves `updated` to its successor, whose streams are the ones left.
		d.progress(&deployprogress.Progress{Phase: deployprogress.Updated, Notes: deferredNotes(result)})
	}
	return result, nil
}

// builtComponents are the running components a deploy's build stages, named
// on the building line. shim-lock rides with the shim (see Targets).
var builtComponents = []deployprogress.Component{
	deployprogress.Shim, deployprogress.Webapp, deployprogress.Daemon,
	deployprogress.Store, deployprogress.Sidecar,
}

// progress states one phase on the update line.
func (d *Deployer) progress(p *deployprogress.Progress) {
	d.deps.Progress.SetDeployProgress(p)
}

// movesDaemon reports whether the deploy handed this daemon over or restarted
// it, in which case its successor says `updated`.
func movesDaemon(r Result) bool {
	for _, o := range r.Outcomes {
		if o.Kind == HandingOver || o.Kind == RestartingAcrossLayout {
			return true
		}
	}
	return false
}

// deferredNotes are the per-workspace notes a deploy that stays leaves: a
// stale shim REGISTERED behind its work is replaced when the session is idle.
func deferredNotes(r Result) map[ids.WorkspaceID][]deployprogress.Note {
	notes := map[ids.WorkspaceID][]deployprogress.Note{}
	for _, o := range r.Outcomes {
		for _, shim := range o.Shims {
			if !shim.Decision.Now {
				notes[shim.Workspace] = append(notes[shim.Workspace], deployprogress.ShimWhenIdle)
			}
		}
	}
	return notes
}

// Landed is the merge orchestrator's hook: ONE COMPLETE CHANGE landed on
// master through the daemon's own merge — however many commits it carries —
// and it deploys ONCE. A landing that arrives while a deploy runs is covered by
// ONE follow-up deploy after it, however many landings that is: every landing
// is deployed, and nothing deploys per commit.
func (d *Deployer) Landed(ctx context.Context, commits []gitclient.Commit) {
	fields := dlog.Context{"commits": len(commits)}
	if len(commits) > 0 {
		fields["landed"] = commits[len(commits)-1].SHA
	}
	d.mu.Lock()
	d.landed = true
	running := d.running
	d.mu.Unlock()
	if running {
		d.log.Info(opLanding, "a change landed while a deploy runs; one deploy follows it", fields)
		return
	}
	d.log.Info(opLanding, "a change landed; deploying it once", fields)
	d.deployLanding(ctx)
}

// deployLanding runs one landing's deploy off the caller, joinably.
func (d *Deployer) deployLanding(ctx context.Context) {
	lifetime := d.deps.Lifetime
	if lifetime == nil {
		lifetime = context.WithoutCancel(ctx)
	}
	d.landings.Add(1)
	go func() {
		defer d.landings.Done()
		result, err := d.Deploy(lifetime, false)
		switch {
		case errors.Is(err, ErrAlreadyDeploying):
			// THE LANDING IS NOT LOST: it is still marked, and the deploy
			// holding the slot starts one more when it ends.
			d.log.Debug(opLanding, "another deploy holds the slot; it deploys the landing when it ends", nil)
		case err != nil:
			d.log.Error(opLanding, "the landing's deploy failed", withCause(nil, err))
		default:
			d.log.Info(opLanding, "the landing is deployed", dlog.Context{"decisions": summarize(result)})
		}
	}()
}

// WaitLandings joins every deploy a landing started.
func (d *Deployer) WaitLandings() { d.landings.Wait() }

// begin takes the one deploy slot. A deploy that starts COVERS every landing
// before it: it builds the checkout as it stands.
func (d *Deployer) begin() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.running {
		return false
	}
	d.running = true
	d.landed = false
	return true
}

// end releases the slot, and starts the one deploy owed to landings that
// arrived while this one ran.
func (d *Deployer) end() {
	d.mu.Lock()
	d.running = false
	again := d.landed
	d.mu.Unlock()
	if again {
		d.log.Info(opLanding, "changes landed during the deploy that just ended; deploying them once", nil)
		d.deployLanding(context.Background())
	}
}

// builds are the fresh builds of one deploy.
type builds struct {
	shim, webapp, daemon, store, sidecar, lock, elisp string
}

func (b builds) fields() dlog.Context {
	return dlog.Context{
		"shim": b.shim, "webapp": b.webapp, "daemon": b.daemon,
		"store": b.store, "sidecar": b.sidecar, "lock": b.lock, "elisp": b.elisp,
	}
}

// hashStaged names every staged artifact by content hash, and the checkout's
// elisp by its module-set hash.
func (d *Deployer) hashStaged(s Staged) (builds, error) {
	var b builds
	var err error
	step := func(component string, value *string, fn func() (string, error)) {
		if err != nil {
			return
		}
		v, hashErr := fn()
		if hashErr != nil {
			err = &BuildFailed{Step: component, Detail: hashErr.Error()}
			return
		}
		*value = v
	}
	step("shim", &b.shim, func() (string, error) { return buildid.File(s.ShimMain()) })
	step("webapp", &b.webapp, func() (string, error) { return buildid.Webapp(s.WebappDist()) })
	step("daemon", &b.daemon, func() (string, error) { return buildid.File(s.DaemonBin()) })
	step("store", &b.store, func() (string, error) { return buildid.File(s.CacheBin(buildreport.ServiceStore)) })
	step("sidecar", &b.sidecar, func() (string, error) { return buildid.File(s.CacheBin(buildreport.ServiceSidecar)) })
	step("lock", &b.lock, func() (string, error) { return buildid.File(s.CacheBin("shim-lock")) })
	step("elisp", &b.elisp, func() (string, error) { return buildid.Elisp(d.deps.Live.ModuleRoot) })
	return b, err
}

// install puts every staged artifact whose content differs from the installed
// one into place. Nothing has been restarted when it runs, and a failure stops
// the deploy before anything is.
func (d *Deployer) install(s Staged, fresh builds, nonce string) error {
	live := d.deps.Live
	in := installer{nonce: nonce, log: d.log}
	type artifact struct {
		component Component
		name      string
		fresh     string
		installed func() (string, error)
		install   func() error
	}
	cacheBin := func(name string) func() error {
		return func() error {
			return in.file(s.CacheBin(name), live.CacheBinPath(name),
				stampsBeside(filepath.Dir(s.CacheBin(name)), live.CacheBin, name+"."))
		}
	}
	artifacts := []artifact{
		{ComponentShim, "shim", fresh.shim, d.deps.Bundle.Build, func() error {
			// THE BUNDLE IS REPLACED ONLY WHILE NO SPAWN HOLDS IT: a spawn
			// states the hash it read, and node must run those very bytes.
			return d.deps.Bundle.Replace(func() error {
				return in.file(s.ShimMain(), live.ShimMain(),
					stampsBeside(filepath.Dir(s.ShimMain()), filepath.Dir(live.ShimMain()), ""))
			})
		}},
		{ComponentWebapp, "webapp", fresh.webapp, func() (string, error) { return buildid.Webapp(live.WebappDist()) }, func() error {
			return in.dir(s.WebappDist(), live.WebappDist())
		}},
		{ComponentDaemon, "daemon", fresh.daemon, func() (string, error) { return buildid.File(live.DaemonBin()) }, func() error {
			return in.file(s.DaemonBin(), live.DaemonBin(),
				stampsBeside(filepath.Dir(s.DaemonBin()), filepath.Dir(live.DaemonBin()), ""))
		}},
		{ComponentStore, buildreport.ServiceStore, fresh.store, func() (string, error) {
			return buildid.File(live.CacheBinPath(buildreport.ServiceStore))
		}, cacheBin(buildreport.ServiceStore)},
		{ComponentSidecar, buildreport.ServiceSidecar, fresh.sidecar, func() (string, error) {
			return buildid.File(live.CacheBinPath(buildreport.ServiceSidecar))
		}, cacheBin(buildreport.ServiceSidecar)},
		{ComponentShim, "shim-lock", fresh.lock, func() (string, error) {
			return buildid.File(live.CacheBinPath("shim-lock"))
		}, cacheBin("shim-lock")},
	}
	for _, a := range artifacts {
		fields := dlog.Context{"artifact": a.name, "fresh": a.fresh}
		installed, err := a.installed()
		if err == nil && installed == a.fresh {
			d.log.Debug(opInstall, "the installed artifact is already the fresh build", fields)
			continue
		}
		if err != nil {
			// AN UNREADABLE OR ABSENT INSTALLED ARTIFACT IS REPLACED: there is
			// nothing standing that the fresh build could be the same as.
			fields["installed_unreadable"] = err.Error()
		} else {
			fields["installed"] = installed
		}
		if err := a.install(); err != nil {
			d.log.Error(opInstall, "could not install the fresh build", withCause(fields, err))
			return &InstallFailed{Component: a.component, Detail: err.Error()}
		}
		d.log.Info(opInstall, "installed the fresh build", fields)
	}
	return nil
}

// services restarts the store and the sidecar when the processes launchd runs
// report a build that is not the fresh one. They hold no turn state, so
// nothing is waited on.
func (d *Deployer) services(ctx context.Context, fresh builds) ([]Outcome, error) {
	storeStale := d.serviceStale(ComponentStore, buildreport.ServiceStore, fresh.store)
	sidecarStale := d.serviceStale(ComponentSidecar, buildreport.ServiceSidecar, fresh.sidecar)
	store := Outcome{Component: ComponentStore, Build: fresh.store, Kind: UpToDate}
	sidecar := Outcome{Component: ComponentSidecar, Build: fresh.sidecar, Kind: UpToDate}
	switch {
	case storeStale:
		d.progress(&deployprogress.Progress{Phase: deployprogress.RestartingServices,
			Components: []deployprogress.Component{deployprogress.Store, deployprogress.Sidecar}})
		// A STORE RESTART ALWAYS RESTARTS THE SIDECAR: its socket is out while
		// the store restarts, and a fresh pair is the known-good state.
		if err := d.deps.Services.RestartStore(ctx); err != nil {
			d.log.Error(opDecide, "the store restart failed", withCause(dlog.Context{"component": string(ComponentStore)}, err))
			return []Outcome{store, sidecar}, &ServiceRestartFailed{Component: ComponentStore, Detail: err.Error()}
		}
		store.Kind, sidecar.Kind = Restarted, Restarted
		d.log.Info(opDecide, "restarted the store and the sidecar onto the fresh build", dlog.Context{
			"store_build": fresh.store, "sidecar_build": fresh.sidecar,
		})
	case sidecarStale:
		d.progress(&deployprogress.Progress{Phase: deployprogress.RestartingServices,
			Components: []deployprogress.Component{deployprogress.Sidecar}})
		if err := d.deps.Services.RestartSidecar(ctx); err != nil {
			d.log.Error(opDecide, "the sidecar restart failed", withCause(dlog.Context{"component": string(ComponentSidecar)}, err))
			return []Outcome{store, sidecar}, &ServiceRestartFailed{Component: ComponentSidecar, Detail: err.Error()}
		}
		sidecar.Kind = Restarted
		d.log.Info(opDecide, "restarted the sidecar onto the fresh build", dlog.Context{"sidecar_build": fresh.sidecar})
	}
	return []Outcome{store, sidecar}, nil
}

// serviceStale reports whether the process launchd runs for a service is not
// running the fresh build: no report, a report whose process is gone, or a
// different build are all stale. A report that cannot be read is stale too —
// and said so loudly — because nothing can prove the service current.
func (d *Deployer) serviceStale(component Component, service, fresh string) bool {
	fields := dlog.Context{"component": string(component), "fresh": fresh}
	report, found, err := buildreport.Read(d.deps.ReportDir, service)
	switch {
	case err != nil:
		d.log.Error(opDecide, "the service's build report is unreadable; it cannot be proven current, so it is restarted", withCause(fields, err))
		return true
	case !found:
		d.log.Info(opDecide, "the service reports no build; restarting it", fields)
		return true
	case !d.deps.Alive(report.PID):
		d.log.Info(opDecide, "the process that reported the service's build is gone; restarting it", merge(fields, dlog.Context{"pid": report.PID}))
		return true
	case report.Build != fresh:
		d.log.Info(opDecide, "the service runs an older build; restarting it", merge(fields, dlog.Context{"running": report.Build, "pid": report.PID}))
		return true
	default:
		d.log.Debug(opDecide, "the service runs the fresh build", fields)
		return false
	}
}

// elisp pushes reload_elisp to every Emacs whose loaded elisp is not the
// checkout's.
func (d *Deployer) elisp(fresh builds) Outcome {
	out := Outcome{Component: ComponentElisp, Build: fresh.elisp, Kind: UpToDate}
	var stale []string
	for _, client := range d.deps.Clients.EmacsBuilds() {
		if client.Build != fresh.elisp {
			d.log.Info(opDecide, "an Emacs runs older elisp; pushing it the reload", dlog.Context{
				"stream": client.ID, "running": client.Build, "fresh": fresh.elisp,
			})
			stale = append(stale, client.ID)
		}
	}
	if len(stale) == 0 {
		d.log.Debug(opDecide, "every connected Emacs runs the checkout's elisp", dlog.Context{"fresh": fresh.elisp})
		return out
	}
	out.Kind = ReloadPushed
	out.Recipients = d.deps.Clients.PushReloadElisp(stale, d.deps.Live.ModuleRoot, fresh.elisp)
	return out
}

// daemonShimWebapp decides the three components the daemon itself serves. A
// stale daemon is HANDED OVER, and its successor takes the shims and the
// webviews onto the fresh build; otherwise each stale shim goes to the bounce
// registry and each stale webview is told to reload.
func (d *Deployer) daemonShimWebapp(ctx context.Context, staged Staged, fresh builds, force bool) ([]Outcome, error) {
	daemon := Outcome{Component: ComponentDaemon, Build: fresh.daemon, Kind: UpToDate}
	if fresh.daemon != d.deps.DaemonBuild {
		// THE FRESH BINARY IS ASKED WHICH LAYOUT IT WRITES before anything is
		// decided. A handover's successor opens the state READ-ONLY and cannot
		// migrate it, so a layout change handed over is a successor that dies
		// at boot (2026-09-27); it is rolled out stop-then-start instead. A
		// binary that cannot answer is not handed over on a guess.
		layout, err := d.deps.StateLayout(ctx, staged.DaemonBin())
		fields := dlog.Context{"running": d.deps.DaemonBuild, "fresh": fresh.daemon, "forced": force}
		if err != nil {
			d.log.Error(opDecide, "the fresh daemon's state layout could not be read; the daemon is neither handed over nor restarted", withCause(fields, err))
			return []Outcome{daemon}, fmt.Errorf("deploy: read the fresh daemon's state layout: %w", err)
		}
		fields["running_layout"], fields["fresh_layout"] = d.deps.RunningLayout, layout
		if layout != d.deps.RunningLayout {
			d.log.Info(opDecide, "this daemon runs an older build whose state layout differs from the fresh one; restarting it rather than handing over", fields)
			d.progress(&deployprogress.Progress{Phase: deployprogress.RestartingServices,
				Components: []deployprogress.Component{deployprogress.Daemon}, Draining: !force})
			accepted, err := d.deps.Rollout.Restart(ctx, force)
			if err != nil {
				d.log.Error(opDecide, "the restart was not accepted", withCause(fields, err))
				return []Outcome{daemon}, err
			}
			daemon.Kind, daemon.Handover = RestartingAcrossLayout, accepted
			daemon.Layouts = LayoutChange{Running: d.deps.RunningLayout, Fresh: layout}
			return []Outcome{
				daemon,
				{Component: ComponentShim, Build: fresh.shim, Kind: DeferredToSuccessor},
				{Component: ComponentWebapp, Build: fresh.webapp, Kind: DeferredToSuccessor},
			}, nil
		}
		d.log.Info(opDecide, "this daemon runs an older build; handing over", fields)
		d.progress(&deployprogress.Progress{Phase: deployprogress.HandingOver, Draining: !force})
		accepted, err := d.deps.Rollout.HandOver(ctx, force)
		if err != nil {
			d.log.Error(opDecide, "the handover was not accepted", withCause(dlog.Context{"forced": force}, err))
			return []Outcome{daemon}, err
		}
		daemon.Kind, daemon.Handover = HandingOver, accepted
		return []Outcome{
			daemon,
			{Component: ComponentShim, Build: fresh.shim, Kind: DeferredToSuccessor},
			{Component: ComponentWebapp, Build: fresh.webapp, Kind: DeferredToSuccessor},
		}, nil
	}
	d.log.Debug(opDecide, "this daemon runs the fresh build", dlog.Context{"fresh": fresh.daemon})
	shims, err := d.shims(ctx, fresh, force)
	if err != nil {
		return []Outcome{daemon}, err
	}
	return []Outcome{daemon, shims, d.webapp(fresh)}, nil
}

// shims asks the rollout to judge every live shim against the installed
// bundle; each stale one goes to the bounce registry.
func (d *Deployer) shims(ctx context.Context, fresh builds, force bool) (Outcome, error) {
	out := Outcome{Component: ComponentShim, Build: fresh.shim, Kind: UpToDate}
	workspaces, err := d.deps.Workspaces(ctx)
	if err != nil {
		d.log.Error(opDecide, "could not list the workspaces whose shims are judged", withCause(nil, err))
		return out, fmt.Errorf("deploy: list the workspaces: %w", err)
	}
	for _, ws := range workspaces {
		check, err := d.deps.Rollout.CheckStaleness(ctx, ws, force)
		if err != nil {
			// ONE WORKSPACE THAT CANNOT BE JUDGED IS ITS OWN LOUD FAILURE; the
			// rest are still judged.
			d.log.Error(opDecide, "a workspace's shim could not be judged", withCause(dlog.Context{"workspace": string(ws)}, err))
			continue
		}
		if !check.Stale || check.Skipped != "" {
			continue
		}
		out.Shims = append(out.Shims, ShimBounce{Workspace: ws, Decision: check.Bounce})
		d.log.Info(opDecide, "a stale shim went to the bounce registry", dlog.Context{
			"workspace": string(ws), "running": check.Reported, "fresh": check.Installed,
			"now": check.Bounce.Now, "forced": check.Bounce.Forced,
			"turn_in_flight": check.Bounce.TurnInFlight, "detached_work": check.Bounce.DetachedWork,
		})
	}
	if len(out.Shims) > 0 {
		out.Kind = ShimsBouncing
	}
	return out, nil
}

// webapp tells every workspace with a webview on an older build to reload it.
func (d *Deployer) webapp(fresh builds) Outcome {
	out := Outcome{Component: ComponentWebapp, Build: fresh.webapp, Kind: UpToDate}
	for ws, running := range d.deps.Clients.WebviewBuilds() {
		stale := false
		for _, build := range running {
			if build != fresh.webapp {
				stale = true
			}
		}
		if !stale {
			continue
		}
		d.deps.Clients.PushReloadWebapp(ws)
		out.Recipients++
		d.log.Info(opDecide, "a webview runs an older webapp; pushed the reload", dlog.Context{
			"workspace": string(ws), "running": running, "fresh": fresh.webapp,
		})
	}
	if out.Recipients > 0 {
		out.Kind = ReloadPushed
	}
	return out
}

// summarize renders a result for a record.
func summarize(r Result) map[string]string {
	out := make(map[string]string, len(r.Outcomes))
	for _, o := range r.Outcomes {
		out[string(o.Component)] = string(o.Kind)
	}
	return out
}

func wsNames(workspaces []ids.WorkspaceID) []string {
	out := make([]string, 0, len(workspaces))
	for _, ws := range workspaces {
		out = append(out, string(ws))
	}
	return out
}

// processAlive is the kernel's answer: signal 0 reaches a live process.
func processAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	return syscall.Kill(pid, 0) == nil
}

func withCause(fields dlog.Context, err error) dlog.Context {
	out := merge(fields, nil)
	out["cause"] = err.Error()
	return out
}

func merge(base, extra dlog.Context) dlog.Context {
	out := make(dlog.Context, len(base)+len(extra)+1)
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}
