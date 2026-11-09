package deploy

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/logging/buildreport"

	"agent-repld/internal/buildid"
	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/restarttiming"
)

// The labels of the services a deploy restarts: each is the launchd label,
// and the systemd unit's name before its `.service`.
const (
	StoreLabel   = "com.agentrepl.shim-store"
	SidecarLabel = "com.agentrepl.shim-claude-sidecar"
	ZettelLabel  = "com.agentrepl.zettel"
	TestsLabel   = "com.agentrepl.tests"
	// NotifierLabel is the notifier app's launchd agent.
	NotifierLabel = "com.agentrepl.notifier"
)

// Standalone is a service no other service's restart moves, and that moves
// none: zettel and the test service. Every one is built, installed, judged,
// restarted, rolled back and kept current at boot by the same code, from its
// entry in Standalones.
type Standalone struct {
	// Component names it in a deploy's outcomes.
	Component Component
	// Label is its service manager label.
	Label string
	// Service is its binary's and its build report's name.
	Service string
	// Progress names it on the update line.
	Progress deployprogress.Component
	// Short and Display name it in records: "the <Short> restart failed",
	// "restarted <Display>".
	Short, Display string
}

// The standalone services.
var (
	Zettel = Standalone{Component: ComponentZettel, Label: ZettelLabel, Service: buildreport.ServiceZettel,
		Progress: deployprogress.Zettel, Short: "zettel", Display: "zettel"}
	Tests = Standalone{Component: ComponentTests, Label: TestsLabel, Service: buildreport.ServiceTests,
		Progress: deployprogress.Tests, Short: "test service", Display: "the test service"}
)

// Standalones is every standalone service, in the order a deploy reports them.
var Standalones = []Standalone{Zettel, Tests}

// restartWith is S's restart through services, in the one shape a deploy's
// restartOnto and its rollbackRestart take.
func (s Standalone) restartWith(services Services) func(context.Context) error {
	return func(ctx context.Context) error { return services.RestartStandalone(ctx, s) }
}

const opServices = "daemon.deploy.services"

// ServiceManager is the slice of the host's service manager a service restart
// drives: launchd on macOS (Launchctl), systemd's per-user manager on Linux
// (SystemdUser). Every service is kept alive (launchd's KeepAlive, systemd's
// Restart=always), so a service the manager HOLDS comes back on its own when
// its process dies, and one it does not hold stays down until something starts
// it. A test drives a scripted fake, and NO TEST EVER REACHES A LIVE SERVICE
// MANAGER.
type ServiceManager interface {
	// Status reports whether the manager holds the label (keeps it running)
	// and, when it does, the running pid (0 when held but not running).
	Status(ctx context.Context, label string) (held bool, pid int, err error)
	// Restart restarts a held service in place, on the definition the
	// manager loaded.
	Restart(ctx context.Context, label string) error
	// Stop takes the service out of the manager's keeping, so nothing
	// relaunches it. A service the manager no longer holds may answer an
	// error wrapping ErrServiceNotLoaded: it is already where Stop leads.
	Stop(ctx context.Context, label string) error
	// Start brings the service back from its installed definition AS IT IS
	// NOW and runs it: a definition changed since the manager last loaded it
	// is the one started.
	Start(ctx context.Context, label string) error
	// Definition is the installed definition Start brings label back from:
	// a launchd plist, or a systemd unit file.
	Definition(label string) string
}

// ServiceWindows bound a service restart's waits. Each is a FAILURE BOUND,
// never a cadence: every wait ends on the kernel's own report (a process's
// exit, the socket's directory or the store's log changing), and a window
// only says how long that report may take before the step is a failure.
type ServiceWindows struct {
	// Stall is how long the store may go without writing a byte to its log
	// while its socket is still absent before it is called wedged. A boot that
	// is visibly working resets it.
	Stall time.Duration
	// Max is the upper bound on any one wait, however busy the service looks.
	Max time.Duration
}

// DefaultServiceWindows are the ported deploy chain's bounds: a 15s stall
// budget and a 180s ceiling.
var DefaultServiceWindows = ServiceWindows{Stall: 15 * time.Second, Max: 180 * time.Second}

// Restarter restarts the store and the sidecar in the RECORDED SAFE ORDER.
//
// WHY THE SIDECAR IS STOPPED BEFORE THE STORE IS RESTARTED. A store
// restart unlinks store.sock and does not rebind it until the new process is
// serving. The sidecar writes into that socket continuously, so a store
// restart taken underneath a RUNNING sidecar is an error storm in the
// sidecar's log for as long as the gap lasts (2026-09-13: `storeclient-write-
// batch` "no such file or directory", `store-write` "cursor not advanced",
// `production-suspended`, every one of them the deploy's own doing). The order
// with none of that — sidecar down, store down, store up, sidecar up — is the
// one bin/store-reset.sh records too.
//
// AND THE STOP IS THE SERVICE MANAGER'S, NOT `kill`: every service is kept
// alive, so the manager answers a signalled process with a new pid within a
// second. Stop takes the service out of the manager's keeping (launchd's
// bootout, systemd's stop); Start puts it back.
type Restarter struct {
	Services ServiceManager
	// StoreSocket is the store's socket, which the new process binds once it
	// is serving.
	StoreSocket string
	// StoreLog is the store's standard error log: the only view of a
	// boot in progress.
	StoreLog string
	Windows  ServiceWindows
	Clock    Clock
	Log      dlog.Logger
	// CacheBin is where the service manager runs the services from: the installed build a
	// running service is judged against (EnsureCurrent).
	CacheBin string
	// ReportDir is where the services write their build reports.
	ReportDir string
	// Alive reports whether a process is running; nil is the kernel's answer.
	Alive func(pid int) bool
	// ProcessExit waits for a process's exit, until ctx ends; nil is the
	// kernel's own exit event (kernelevent.WaitExit). A process already gone
	// answers at once.
	ProcessExit func(ctx context.Context, pid int) error
	// Restarts records how long each store and sidecar restart took
	// (restarttiming.ServiceRestart). REQUIRED; runtimemetrics.Surface is
	// the production one.
	Restarts restarttiming.Recorder
	// ResourceWindows measures the restarted services while each restart
	// runs: their CPU and memory as they come back
	// (restarttiming.Windows). REQUIRED; runtimemetrics.Surface is the
	// production one.
	ResourceWindows restarttiming.Windows
	// Define writes label's definition from its requirements entry, where
	// Services.Definition finds it: a service added since this machine's
	// install has none, and a deploy that ships it must bring it up.
	// REQUIRED; ScriptRequirements.Define is the production one
	// (bin/requirements.sh define-service).
	Define func(ctx context.Context, label string) error
	// EnsureHolder proves the socket holder ID serves, installing it when
	// none does (holderensure.Ensurer.EnsureID). The store takes its sockets
	// from StoreHolder, so the holder is ensured before the store starts or
	// restarts. REQUIRED.
	EnsureHolder func(ctx context.Context, id string) error
	// HolderServing reports whether the socket holder ID serves, by its cheap
	// probe (holderensure.Ensurer.ServingID), installing nothing. REQUIRED
	// when StoreHolder is set.
	HolderServing func(ctx context.Context, id string) (bool, error)
	// StoreHolder is the id of the socket holder that holds StoreSocket, the
	// one the store takes its sockets from (holderensure.HolderOfSocket);
	// empty when no declared holder holds that socket, and then the store
	// binds its own and no holder is ensured for it.
	StoreHolder string
	// NotifierApp is the installed notifier app (Live.NotifierApp), and
	// RegisterApp registers a bundle with LaunchServices (lsregister -f);
	// both are set on macOS only, where RestartNotifier runs.
	NotifierApp string
	RegisterApp func(ctx context.Context, bundle string) error
}

// serviceWindowBounds bound each service restart's resource window: about
// three times the p99 of its restart over the 7 days to 2026-11-20 (the
// store's 12s, the sidecar's 6.4s).
var serviceWindowBounds = map[restarttiming.Service]time.Duration{
	restarttiming.Store:   30 * time.Second,
	restarttiming.Sidecar: 20 * time.Second,
}

// serviceWatches are the processes each service restart brings back: the
// store's restart takes the sidecar down and back with it.
var serviceWatches = map[restarttiming.Service][]restarttiming.Watched{
	restarttiming.Store:   {{Process: restarttiming.ProcessStore}, {Process: restarttiming.ProcessSidecar}},
	restarttiming.Sidecar: {{Process: restarttiming.ProcessSidecar}},
}

// timed runs restart and records how long it took as service's restart,
// measuring the services it brings back while it runs.
func (r *Restarter) timed(service restarttiming.Service, restart func() error) error {
	began := r.Clock.Now()
	closeWindow := r.ResourceWindows.OpenWindow(restarttiming.ServiceRestart, string(service), serviceWindowBounds[service], serviceWatches[service]...)
	err := restart()
	closeWindow()
	r.Restarts.ObserveRestart(restarttiming.OfService(service, restarttiming.OutcomeOf(err), r.Clock.Now().Sub(began)))
	return err
}

// RestartStore restarts the store — and, because its socket is out while it
// restarts, the sidecar with it — in the recorded safe order. It is timed as
// the store's restart, from the sidecar's stop to its bootstrap back.
func (r *Restarter) RestartStore(ctx context.Context) error {
	return r.timed(restarttiming.Store, func() error { return r.restartStore(ctx, false) })
}

// restartStore restarts the store in the safe order. RELOAD reloads it on
// its definition even when the rewrite left the file as it was: a store
// moved onto its holder runs the definition on disk, never the one launchd
// loaded before the holder existed.
func (r *Restarter) restartStore(ctx context.Context, reload bool) error {
	definition := r.Services.Definition(SidecarLabel)
	fields := dlog.Context{"store": StoreLabel, "sidecar": SidecarLabel, "sidecar_definition": definition}

	// THE DEFINITION IS PROVED BEFORE ANYTHING IS STOPPED. A stop with no definition
	// to bootstrap back from would leave the host with no sidecar and no way
	// to return one.
	if _, err := os.Stat(definition); err != nil {
		r.Log.Error(opServices, "the sidecar's service definition is missing; nothing was stopped", dlog.WithCause(fields, err))
		return fmt.Errorf("deploy: the store restart boots the sidecar out and needs %s to bring it back: %w "+
			"(re-run .claude/install.sh)", definition, err)
	}
	// THE STORE'S HOLDER AND ITS DEFINITION ARE SETTLED BEFORE ANYTHING IS
	// STOPPED: a store whose definition takes its sockets from a holder that
	// is not up could not boot, and the sidecar would be left down with it.
	if err := r.ensureStoreHolder(ctx, fields); err != nil {
		return err
	}
	changed, err := r.redefine(ctx, StoreLabel, fields)
	if err != nil {
		return err
	}
	if err := r.stopService(ctx, SidecarLabel, "the sidecar", "the store was NOT restarted", fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "restarting the store", dlog.Merge(fields, dlog.Context{"definition_changed": changed, "reload": reload}))
	if err := r.restartOn(ctx, StoreLabel, changed || reload, fields); err != nil {
		r.failed("the store restart failed; the sidecar is down and was NOT started again", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", StoreLabel, err)
	}
	if err := r.awaitStoreSocket(ctx, fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "the store is serving; starting the sidecar", fields)
	if err := r.Services.Start(ctx, SidecarLabel); err != nil {
		r.failed("the sidecar could not be started back", fields, err)
		return fmt.Errorf("deploy: start %s: %w", SidecarLabel, err)
	}
	r.Log.Info(opServices, "restarted the store and the sidecar in the safe order", fields)
	return nil
}

// EnsureLoaded makes sure the service manager holds both services, STARTING
// ONE IT DOES NOT HOLD, so a daemon never brings shims up against a host
// whose store is gone for good. Both plists set KeepAlive, so a loaded service
// that dies comes back on its own; an UNLOADED one (a `launchctl bootout`, a
// deploy that ended between its bootout and its bootstrap) stays down until
// something bootstraps it, and every shim then retries a socket that never
// appears. MEASURED, 2026-09-30 22:41:12: a boot with the store booted out
// brought five shims up into ~80s of `connect ENOENT .../store.sock` and
// blank feeds, until a hand-run bootstrap brought it back.
//
// The store comes first and its socket is awaited before the sidecar is
// bootstrapped, the same order RestartStore keeps: a sidecar started against
// an absent socket is an error storm in its log. A service already loaded is
// left exactly as it is.
func (r *Restarter) EnsureLoaded(ctx context.Context) error {
	_, _, err := r.ensureBothLoaded(ctx)
	return err
}

// ensureBothLoaded is EnsureLoaded, answering which of the two it bootstrapped.
func (r *Restarter) ensureBothLoaded(ctx context.Context) (storeBootstrapped, sidecarBootstrapped bool, err error) {
	fields := dlog.Context{"store": StoreLabel, "sidecar": SidecarLabel}
	storeBootstrapped, err = r.ensureLoaded(ctx, StoreLabel, fields)
	if err != nil {
		return false, false, err
	}
	if storeBootstrapped {
		if err := r.awaitStoreSocket(ctx, fields); err != nil {
			return true, false, err
		}
	}
	sidecarBootstrapped, err = r.ensureLoaded(ctx, SidecarLabel, fields)
	if err != nil {
		return storeBootstrapped, false, err
	}
	r.Log.Debug(opServices, "the service manager holds both services", fields)
	return storeBootstrapped, sidecarBootstrapped, nil
}

// RestartNotifier registers the installed notifier app with LaunchServices,
// so macOS attributes its banners to the bundle now standing, and restarts
// its service onto it. A banner wanted while the app is out is recorded
// unposted by the daemon's notifier; nothing else waits on it.
func (r *Restarter) RestartNotifier(ctx context.Context) error {
	fields := dlog.Context{"notifier": NotifierLabel, "app": r.NotifierApp}
	if r.RegisterApp == nil || r.NotifierApp == "" {
		panic("deploy: RestartNotifier on a restarter with no notifier app to register")
	}
	if err := r.RegisterApp(ctx, r.NotifierApp); err != nil {
		r.failed("registering the notifier app with LaunchServices failed", fields, err)
		return fmt.Errorf("deploy: register %s: %w", r.NotifierApp, err)
	}
	if err := r.Services.Restart(ctx, NotifierLabel); err != nil {
		r.failed("the notifier app restart failed", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", NotifierLabel, err)
	}
	r.Log.Info(opServices, "registered and restarted the notifier app", fields)
	return nil
}

// RestartStandalone restarts standalone service S alone. Nothing else
// depends on its socket but the daemon's own calls, which fail loudly while
// it is out.
func (r *Restarter) RestartStandalone(ctx context.Context, s Standalone) error {
	fields := dlog.Context{s.Short: s.Label}
	// A SERVICE THE MANAGER DOES NOT HOLD CANNOT BE KICKSTARTED (2026-11-16:
	// the test service's first deploy failed with launchctl exit 113 and
	// rolled the whole deploy back). It is loaded from its definition, written
	// when missing (ensureLoaded), and a service just started from the
	// installed build needs no restart.
	bootstrapped, err := r.ensureLoaded(ctx, s.Label, fields)
	if err != nil {
		return err
	}
	if bootstrapped {
		r.Log.Info(opServices, s.Display+" was not loaded; it was started from the installed build, so it is not restarted", fields)
		return nil
	}
	changed, err := r.redefine(ctx, s.Label, fields)
	if err != nil {
		return err
	}
	if err := r.restartOn(ctx, s.Label, changed, fields); err != nil {
		r.failed("the "+s.Short+" restart failed", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", s.Label, err)
	}
	r.Log.Info(opServices, "restarted "+s.Display, fields)
	return nil
}

// EnsureStandaloneCurrent is the boot's step for standalone service S: the
// service manager holds it, and it runs the installed build (restarted alone
// when it does not). It is independent of the store and the sidecar -- no
// shim writes into it -- so the boot runs it beside the bring-ups, never
// before a shim starts.
func (r *Restarter) EnsureStandaloneCurrent(ctx context.Context, s Standalone) error {
	bootstrapped, err := r.ensureLoaded(ctx, s.Label, dlog.Context{s.Short: s.Label})
	if err != nil {
		return err
	}
	stale, err := r.runningStale(ctx, bootstrapped, s.Component, s.Label, s.Service)
	if err != nil {
		return err
	}
	if stale {
		r.Log.Info(opServices, "the running "+s.Short+" is not the installed build; restarting it", nil)
		return r.RestartStandalone(ctx, s)
	}
	r.Log.Debug(opServices, s.Short+" runs the installed build", nil)
	return nil
}

// EnsureStandaloneLoaded makes sure the service manager holds standalone
// service S, and touches nothing that runs: a joining successor's step,
// which must not restart a service the incumbent is still serving through.
func (r *Restarter) EnsureStandaloneLoaded(ctx context.Context, s Standalone) error {
	_, err := r.ensureLoaded(ctx, s.Label, dlog.Context{s.Short: s.Label})
	return err
}

// EnsureStandalonesCurrent is EnsureStandaloneCurrent for every standalone
// service, each tried whatever the others answered; the first failure is the
// answer.
func (r *Restarter) EnsureStandalonesCurrent(ctx context.Context) error {
	return eachStandalone(func(s Standalone) error { return r.EnsureStandaloneCurrent(ctx, s) })
}

// EnsureStandalonesLoaded is EnsureStandaloneLoaded for every standalone
// service, each tried whatever the others answered.
func (r *Restarter) EnsureStandalonesLoaded(ctx context.Context) error {
	return eachStandalone(func(s Standalone) error { return r.EnsureStandaloneLoaded(ctx, s) })
}

// eachStandalone runs STEP for every standalone service and joins what
// failed.
func eachStandalone(step func(Standalone) error) error {
	var errs []error
	for _, s := range Standalones {
		if err := step(s); err != nil {
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}

// EnsureCurrent is the BOOT's service step: the service manager holds both services
// (EnsureLoaded), and neither RUNS an older build than the one installed in
// CacheBin. A running service whose build report is not the installed build
// is restarted in the recorded safe order (RestartStore, which takes the
// sidecar with it, or RestartSidecar alone), judged by the ONE staleness
// check a deploy uses (serviceStaleness).
//
// The boot runs it as its two halves (EnsureStoreCurrent before any shim
// starts, then the sidecar half it answers, beside the bring-ups);
// EnsureCurrent is both, in order.
//
// THE STORE IS CURRENT BEFORE ANY SHIM STARTS, and that is the whole point of
// it. A bounce rebuilds and installs the services' binaries, stands the daemon
// down, and leaves the services to the daemon that boots next: restarted here,
// the store is down only while no shim exists to lose it. Stopped from outside instead,
// they went down under whatever the freshly relaunched daemon had already
// started (2026-10-03).
//
// Only a RUNNING service is judged. One this call just bootstrapped runs the
// installed build by construction, and one launchd holds with no process
// starts from the installed build when it next runs; restarting either would
// buy nothing. An installed build that cannot be read is an error: nothing
// can then prove the running service current, and nothing could restart it
// onto a build that is not there.
func (r *Restarter) EnsureCurrent(ctx context.Context) error {
	sidecar, err := r.EnsureStoreCurrent(ctx)
	if err != nil {
		return err
	}
	if err := sidecar(ctx); err != nil {
		return err
	}
	return r.EnsureStandalonesCurrent(ctx)
}

// EnsureStoreCurrent is EnsureCurrent's FIRST HALF, the one shims must wait
// for: the service manager holds both services, and the store runs the installed build
// (restarted, with the sidecar, when it did not). It answers the SECOND
// HALF, the sidecar's own judgement and restart, for the caller to run once
// shims may start.
//
// THE SIDECAR NEED NOT BE CURRENT BEFORE A SHIM STARTS. Only the store is: a
// shim writes into it from its first record, and a store restart under a
// shim loses what the shim wrote meanwhile. The sidecar only reads
// transcripts the shims write and resumes from the cursors it keeps in the
// store, so a sidecar-only restart beside running shims loses nothing —
// exactly what a deploy's RestartSidecar already does under live sessions.
// Holding every bring-up on it instead cost a boot the whole kickstart: 29s
// on 2026-10-12, with every workspace's tab waiting on it.
func (r *Restarter) EnsureStoreCurrent(ctx context.Context) (func(context.Context) error, error) {
	storeBootstrapped, sidecarBootstrapped, err := r.ensureBothLoaded(ctx)
	if err != nil {
		return nil, err
	}
	storeStale, err := r.runningStale(ctx, storeBootstrapped, ComponentStore, StoreLabel, buildreport.ServiceStore)
	if err != nil {
		return nil, err
	}
	offHolder, err := r.storeOffItsHolder(ctx)
	if err != nil {
		return nil, err
	}
	if storeStale || offHolder {
		// A STORE RESTART RESTARTS THE SIDECAR TOO: its socket is out while the
		// store restarts, and a fresh pair is the known-good state.
		if storeStale {
			r.Log.Info(opServices, "the running store is not the installed build; restarting it, and the sidecar with it, before any shim starts", nil)
		} else {
			r.Log.Info(opServices, "the store's holder is not up, so the store binds its own sockets; moving it onto its holder, and restarting the sidecar with it, before any shim starts", dlog.Context{"holder": r.StoreHolder})
		}
		if err := r.timed(restarttiming.Store, func() error { return r.restartStore(ctx, offHolder) }); err != nil {
			return nil, err
		}
		return func(context.Context) error {
			r.Log.Debug(opServices, "the store's restart already restarted the sidecar onto the installed build", nil)
			return nil
		}, nil
	}
	return func(ctx context.Context) error { return r.ensureSidecarCurrent(ctx, sidecarBootstrapped) }, nil
}

// storeOffItsHolder reports whether the store's declared holder is not
// serving, so the store still binds its own sockets (a machine onto which the
// holder came after its install): the cheap probe is the whole cost when it
// serves (HolderServing). A store at a socket no holder declares is never off
// one.
func (r *Restarter) storeOffItsHolder(ctx context.Context) (bool, error) {
	if r.StoreHolder == "" {
		return false, nil
	}
	serving, err := r.HolderServing(ctx, r.StoreHolder)
	if err != nil {
		// The probe recorded its own failure (holderensure).
		return false, fmt.Errorf("deploy: probe the store's socket holder %s: %w", r.StoreHolder, err)
	}
	return !serving, nil
}

// ensureSidecarCurrent is EnsureCurrent's second half: the sidecar's build
// judged and, when stale, the sidecar restarted alone. BOOTSTRAPPED says the
// first half just started it from the installed build.
func (r *Restarter) ensureSidecarCurrent(ctx context.Context, bootstrapped bool) error {
	sidecarStale, err := r.runningStale(ctx, bootstrapped, ComponentSidecar, SidecarLabel, buildreport.ServiceSidecar)
	if err != nil {
		return err
	}
	if sidecarStale {
		r.Log.Info(opServices, "the running sidecar is not the installed build; restarting it", nil)
		return r.RestartSidecar(ctx)
	}
	r.Log.Debug(opServices, "both services run the installed build", nil)
	return nil
}

// runningStale answers whether the service the manager runs under label is
// running a build other than the one installed for it. BOOTSTRAPPED says this
// boot just started it, from the installed build.
func (r *Restarter) runningStale(ctx context.Context, bootstrapped bool, component Component, label, service string) (bool, error) {
	fields := dlog.Context{"label": label, "service": service}
	if bootstrapped {
		r.Log.Debug(opServices, "the service was just started from the installed build", fields)
		return false, nil
	}
	_, pid, err := r.Services.Status(ctx, label)
	if err != nil {
		r.failed("could not read a service's service-manager state to judge its build", fields, err)
		return false, fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if pid == 0 {
		r.Log.Debug(opServices, "the service manager runs no process for the service; it starts from the installed build", fields)
		return false, nil
	}
	installed := filepath.Join(r.CacheBin, service)
	fresh, err := buildid.File(installed)
	if err != nil {
		r.Log.Error(opServices, "the service's installed build could not be read; its running build cannot be judged", dlog.WithCause(dlog.Merge(fields, dlog.Context{"installed": installed}), err))
		return false, fmt.Errorf("deploy: read the installed %s %s: %w", service, installed, err)
	}
	alive := r.Alive
	if alive == nil {
		alive = processAlive
	}
	return serviceStaleness{reportDir: r.ReportDir, alive: alive, log: r.Log, op: opServices}.stale(component, service, fresh), nil
}

// ensureLoaded starts label from its installed definition when the service
// manager does not hold it,
// and reports whether it did.
func (r *Restarter) ensureLoaded(ctx context.Context, label string, fields dlog.Context) (bool, error) {
	definition := r.Services.Definition(label)
	fields = dlog.Merge(fields, dlog.Context{"label": label, "definition": definition})
	loaded, _, err := r.Services.Status(ctx, label)
	if err != nil {
		r.failed("could not read a service's service-manager state", fields, err)
		return false, fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if loaded {
		return false, nil
	}
	if _, err := os.Stat(definition); errors.Is(err, os.ErrNotExist) {
		// A SERVICE ADDED SINCE THIS MACHINE'S INSTALL (2026-11-16: the test
		// service shipped, and no deploy could start it, so no merge gate
		// ran): its definition is written from its requirements entry, by
		// the same renderer install uses, before it is started.
		r.Log.Info(opServices, "a service is not loaded and has no definition; writing it from its requirements entry", fields)
		if err := r.Define(ctx, label); err != nil {
			r.failed("a service's definition could not be written from its requirements entry", fields, err)
			return false, fmt.Errorf("deploy: write the definition of %s: %w", label, err)
		}
		if _, err := os.Stat(definition); err != nil {
			r.Log.Error(opServices, "a service's definition is still missing after it was written", dlog.WithCause(fields, err))
			return false, fmt.Errorf("deploy: %s was written but %s is still unreadable: %w", label, definition, err)
		}
	} else if err != nil {
		r.Log.Error(opServices, "a service is not loaded and its definition cannot be read", dlog.WithCause(fields, err))
		return false, fmt.Errorf("deploy: %s is not loaded and %s cannot be read: %w", label, definition, err)
	}
	if label == StoreLabel {
		if err := r.ensureStoreHolder(ctx, fields); err != nil {
			return false, err
		}
	}
	r.Log.Info(opServices, "a service was not loaded; starting it", fields)
	if err := r.Services.Start(ctx, label); err != nil {
		r.failed("a service could not be started", fields, err)
		return false, fmt.Errorf("deploy: start %s: %w", label, err)
	}
	return true, nil
}

// ensureStoreHolder proves the store's socket holder serves, installing it
// when none does: the store's definition takes its sockets from it.
func (r *Restarter) ensureStoreHolder(ctx context.Context, fields dlog.Context) error {
	if r.StoreHolder == "" {
		r.Log.Debug(opServices, "no declared holder holds the store's socket; the store binds its own", fields)
		return nil
	}
	if err := r.EnsureHolder(ctx, r.StoreHolder); err != nil {
		// The ensure recorded its own failure (holderensure).
		return fmt.Errorf("deploy: the store's socket holder %s: %w", r.StoreHolder, err)
	}
	return nil
}

// redefine writes label's definition from its requirements entry as it
// renders now (Define), and reports whether that changed the installed one.
// EVERY RESTART RUNS THE DEFINITION ITS REQUIREMENTS RENDER: a requirement
// changed since this machine's install (2026-11-18: the store's `--holder`)
// reaches the service at its next restart, never only at the next install.
// A held service whose definition is missing is a changed one.
func (r *Restarter) redefine(ctx context.Context, label string, fields dlog.Context) (bool, error) {
	definition := r.Services.Definition(label)
	fields = dlog.Merge(fields, dlog.Context{"label": label, "definition": definition})
	before, err := os.ReadFile(definition)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		r.Log.Error(opServices, "a service's installed definition could not be read before it was rewritten; nothing was restarted", dlog.WithCause(fields, err))
		return false, fmt.Errorf("deploy: read %s: %w", definition, err)
	}
	if err := r.Define(ctx, label); err != nil {
		r.failed("a service's definition could not be written from its requirements entry; nothing was restarted", fields, err)
		return false, fmt.Errorf("deploy: write the definition of %s: %w", label, err)
	}
	after, err := os.ReadFile(definition)
	if err != nil {
		r.Log.Error(opServices, "a service's definition is unreadable after it was written; nothing was restarted", dlog.WithCause(fields, err))
		return false, fmt.Errorf("deploy: %s was written but %s is unreadable: %w", label, definition, err)
	}
	changed := !bytes.Equal(before, after)
	if changed {
		r.Log.Info(opServices, "a service's definition changed since it was loaded; it is reloaded on it", fields)
	}
	return changed, nil
}

// restartOn restarts service label: on its definition as it is now when that
// CHANGED, in place otherwise.
//
// A SERVICE THE MANAGER DOES NOT HOLD IS STARTED, never restarted in place
// (2026-11-18: a rollback's kickstart of a store booted out by the failed
// deploy found nothing to restart, exit 113).
//
// A CHANGED DEFINITION IS LOADED ONLY ONCE THE MANAGER HAS LET GO OF THE OLD
// ONE (2026-11-18: a bootstrap run straight after its bootout found launchd
// still holding the store, "Bootstrap failed: 5: Input/output error"): the
// service is stopped through stopService, which waits on the kernel's report
// of its process's exit and confirms the manager no longer holds it, and only
// then started.
func (r *Restarter) restartOn(ctx context.Context, label string, changed bool, fields dlog.Context) error {
	held, _, err := r.Services.Status(ctx, label)
	if err != nil {
		return fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if !held {
		r.Log.Info(opServices, "the service manager does not hold the service; it is started from its definition", dlog.Merge(fields, dlog.Context{"label": label}))
		return r.Services.Start(ctx, label)
	}
	if !changed {
		return r.Services.Restart(ctx, label)
	}
	if err := r.stopService(ctx, label, label, "it was NOT started on its changed definition", fields); err != nil {
		return err
	}
	return r.Services.Start(ctx, label)
}

// RestartSidecar restarts the sidecar alone. The store never moved, so its
// socket is up throughout and a plain kickstart loses nothing.
func (r *Restarter) RestartSidecar(ctx context.Context) error {
	return r.timed(restarttiming.Sidecar, func() error { return r.restartSidecar(ctx) })
}

func (r *Restarter) restartSidecar(ctx context.Context) error {
	fields := dlog.Context{"sidecar": SidecarLabel}
	changed, err := r.redefine(ctx, SidecarLabel, fields)
	if err != nil {
		return err
	}
	if err := r.restartOn(ctx, SidecarLabel, changed, fields); err != nil {
		r.failed("the sidecar restart failed", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", SidecarLabel, err)
	}
	r.Log.Info(opServices, "restarted the sidecar", fields)
	return nil
}

// stopService stops service LABEL (called WHAT in its records) and waits for
// its process's exit, the kernel's report that it left; the service manager,
// which let go of the label in the stop, must then no longer hold it. A stop
// that cannot be confirmed is a failure whose record says CONSEQUENCE.
func (r *Restarter) stopService(ctx context.Context, label, what, consequence string, fields dlog.Context) error {
	fields = dlog.Merge(fields, dlog.Context{"label": label})
	loaded, pid, err := r.Services.Status(ctx, label)
	if err != nil {
		r.failed("could not read "+what+"'s service-manager state; nothing was stopped", fields, err)
		return fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if !loaded {
		r.Log.Info(opServices, what+" is already stopped", fields)
		return nil
	}
	fields = dlog.Merge(fields, dlog.Context{"pid": pid})
	r.Log.Info(opServices, "stopping "+what, fields)
	if err := r.Services.Stop(ctx, label); errors.Is(err, ErrServiceNotLoaded) {
		// THE SERVICE LEFT BETWEEN THE LOOK AND THE STOP: the domain no longer
		// holds it, which is the state the stop was asked to reach.
		r.Log.Info(opServices, what+" had already stopped when it was stopped", dlog.WithCause(fields, err))
	} else if err != nil {
		// A stop that errs may still have taken effect; the exit and the
		// service manager's view below are the authority on whether it did.
		r.Log.Warn(opServices, what+"'s stop answered an error; waiting on its process's exit", dlog.WithCause(fields, err))
	}
	if pid != 0 {
		if err := r.awaitExit(ctx, pid); err != nil {
			if ctx.Err() == nil {
				r.Log.Error(opServices, what+"'s process did not exit within the upper bound; "+consequence,
					dlog.Merge(fields, dlog.Context{"max": r.Windows.Max.String(), "cause": err.Error()}))
			} else {
				r.failed(what+"'s stop ended with its context", fields, ctx.Err())
			}
			return fmt.Errorf("deploy: %s (pid %d) did not exit: %w", label, pid, err)
		}
	}
	loaded, _, err = r.Services.Status(ctx, label)
	if err != nil {
		r.failed("could not read "+what+"'s service-manager state once it stopped", fields, err)
		return fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if loaded {
		r.Log.Error(opServices, "the service manager still holds "+what+" after its stop and its process's exit; "+consequence, fields)
		return fmt.Errorf("deploy: the service manager still holds %s after its stop", label)
	}
	r.Log.Info(opServices, what+" is stopped", fields)
	return nil
}

// awaitExit waits for pid's exit, bounded by the upper bound.
func (r *Restarter) awaitExit(ctx context.Context, pid int) error {
	waitCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	exited := make(chan error, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() { exited <- r.processExit()(waitCtx, pid) }()
	select {
	case err := <-exited:
		return err
	case <-r.Clock.After(r.Windows.Max):
		cancel()
		<-exited
		return fmt.Errorf("no exit within %s", r.Windows.Max)
	}
}

// processExit is ProcessExit, defaulting to the kernel's exit event.
func (r *Restarter) processExit() func(ctx context.Context, pid int) error {
	if r.ProcessExit != nil {
		return r.ProcessExit
	}
	return kernelevent.WaitExit
}

// awaitStoreSocket waits ON THE SERVICE, NOT ON A STOPWATCH: it wakes on the
// kernel's reports -- the socket's directory changing, the store's log
// growing, the store's process exiting -- and a boot slower than any constant
// (a nuked database being replaced) is still working while its log grows. It
// ends on the socket, or on one of three loud terminals: the store died, it
// is wedged (its log has not grown for the stall budget), or it is past the
// upper bound. The two budgets are failure bounds; nothing here looks again
// on a timer.
func (r *Restarter) awaitStoreSocket(ctx context.Context, fields dlog.Context) error {
	fields = dlog.Merge(fields, dlog.Context{"socket": r.StoreSocket, "store_log": r.StoreLog})
	start := r.Clock.Now()
	served := func() error {
		r.Log.Info(opServices, "the store's socket is up", dlog.Merge(fields, dlog.Context{
			"after": r.Clock.Now().Sub(start).String(),
		}))
		return nil
	}
	// THE WATCH IS TAKEN BEFORE THE FIRST LOOK, so a socket bound or a byte
	// written in between is already reported: the socket's directory (the
	// socket bound), the log's directory (the log made) and the log itself.
	changed, stopWatching := kernelevent.Follow(ctx, filepath.Dir(r.StoreSocket), filepath.Dir(r.StoreLog), r.StoreLog)
	defer stopWatching()
	if socketExists(r.StoreSocket) {
		return served()
	}
	_, pid, err := r.Services.Status(ctx, StoreLabel)
	if err != nil {
		r.failed("could not read the store's service-manager state while it boots", fields, err)
		return fmt.Errorf("deploy: read %s: %w", StoreLabel, err)
	}
	died := func() error {
		// LOOK AGAIN BEFORE CALLING IT A DEATH: the socket can appear in the
		// same instant the process is seen gone.
		if socketExists(r.StoreSocket) {
			return served()
		}
		r.Log.Error(opServices, "the store died before its socket appeared; the sidecar was NOT started again",
			dlog.Merge(fields, dlog.Context{"last_words": r.storeLogTail()}))
		return fmt.Errorf("deploy: %s died before %s appeared", StoreLabel, r.StoreSocket)
	}
	if pid == 0 {
		return died()
	}
	waitCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	exited := make(chan error, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() { exited <- r.processExit()(waitCtx, pid) }()
	lastSize := r.storeLogSize()
	stall, max := r.Clock.After(r.Windows.Stall), r.Clock.After(r.Windows.Max)
	for {
		if socketExists(r.StoreSocket) {
			return served()
		}
		select {
		case <-ctx.Done():
			r.failed("a service restart ended with its context", fields, ctx.Err())
			return fmt.Errorf("deploy: service restart: %w", ctx.Err())
		case err := <-exited:
			if err != nil && ctx.Err() == nil {
				r.Log.Error(opServices, "could not wait on the store's exit while it boots", dlog.WithCause(fields, err))
				return fmt.Errorf("deploy: wait on %s (pid %d): %w", StoreLabel, pid, err)
			}
			return died()
		case err := <-changed:
			if err != nil {
				r.Log.Error(opServices, "the watch on the store's boot failed; the sidecar was NOT started again", dlog.WithCause(fields, err))
				return fmt.Errorf("deploy: watch %s: %w", StoreLabel, err)
			}
			if size := r.storeLogSize(); size != lastSize {
				lastSize = size
				stall = r.Clock.After(r.Windows.Stall)
			}
		case <-stall:
			r.Log.Error(opServices, "the store is wedged: no socket and nothing written for the stall budget; the sidecar was NOT started again",
				dlog.Merge(fields, dlog.Context{"pid": pid, "stall": r.Windows.Stall.String(), "last_words": r.storeLogTail()}))
			return fmt.Errorf("deploy: %s (pid %d) wrote nothing for %s and its socket never appeared", StoreLabel, pid, r.Windows.Stall)
		case <-max:
			r.Log.Error(opServices, "the store's socket did not appear within the upper bound; the sidecar was NOT started again",
				dlog.Merge(fields, dlog.Context{"pid": pid, "max": r.Windows.Max.String(), "last_words": r.storeLogTail()}))
			return fmt.Errorf("deploy: %s did not appear within %s", r.StoreSocket, r.Windows.Max)
		}
	}
}

// failed records a service step's failure at ERROR, unless its caller
// CANCELLED it. A daemon standing down cuts its own service step short on
// purpose; that is the caller's decision, not a failure of the step, so it is
// INFO (as scriptrunner records the cancelled script under it). A deadline
// that passed stays an ERROR: the step ran out of the time it was given.
func (r *Restarter) failed(message string, fields dlog.Context, err error) {
	if errors.Is(err, context.Canceled) {
		r.Log.Info(opServices, "a service step was cut short by its caller: "+message, dlog.WithCause(fields, err))
		return
	}
	r.Log.Error(opServices, message, dlog.WithCause(fields, err))
}

// storeLogSize is how much the store has written: a boot still emitting
// records is WORKING, not wedged. An absent log is zero.
func (r *Restarter) storeLogSize() int64 {
	info, err := os.Stat(r.StoreLog)
	if err != nil {
		return 0
	}
	return info.Size()
}

// storeLogTail is the store's last words, for the record a failed boot writes.
func (r *Restarter) storeLogTail() string {
	raw, err := os.ReadFile(r.StoreLog)
	if errors.Is(err, os.ErrNotExist) {
		return "(no " + r.StoreLog + ")"
	}
	if err != nil {
		return "(unreadable " + r.StoreLog + ": " + err.Error() + ")"
	}
	lines := strings.Split(strings.TrimRight(string(raw), "\n"), "\n")
	if len(lines) > 5 {
		lines = lines[len(lines)-5:]
	}
	return strings.Join(lines, "\n")
}

// socketExists reports whether a unix socket stands at path.
func socketExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.Mode()&os.ModeSocket != 0
}
