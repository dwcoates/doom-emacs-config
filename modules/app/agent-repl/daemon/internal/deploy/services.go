package deploy

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/logging/buildreport"

	"agent-repld/internal/buildid"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/restarttiming"
)

// The labels of the three services a deploy restarts: each is the launchd
// label, and the systemd unit's name before its `.service`.
const (
	StoreLabel   = "com.agentrepl.shim-store"
	SidecarLabel = "com.agentrepl.shim-claude-sidecar"
	ZettelLabel  = "com.agentrepl.zettel"
)

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
	// Restart restarts a held service in place.
	Restart(ctx context.Context, label string) error
	// Stop takes the service out of the manager's keeping, so nothing
	// relaunches it. A service the manager no longer holds may answer an
	// error wrapping ErrServiceNotLoaded: it is already where Stop leads.
	Stop(ctx context.Context, label string) error
	// Start brings the service back from its installed definition and runs
	// it.
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
}

// timed runs restart and records how long it took as service's restart.
func (r *Restarter) timed(service restarttiming.Service, restart func() error) error {
	began := r.Clock.Now()
	err := restart()
	r.Restarts.ObserveRestart(restarttiming.OfService(service, restarttiming.OutcomeOf(err), r.Clock.Now().Sub(began)))
	return err
}

// RestartStore restarts the store — and, because its socket is out while it
// restarts, the sidecar with it — in the recorded safe order. It is timed as
// the store's restart, from the sidecar's stop to its bootstrap back.
func (r *Restarter) RestartStore(ctx context.Context) error {
	return r.timed(restarttiming.Store, func() error { return r.restartStore(ctx) })
}

func (r *Restarter) restartStore(ctx context.Context) error {
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
	if err := r.stopSidecar(ctx, fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "restarting the store", fields)
	if err := r.Services.Restart(ctx, StoreLabel); err != nil {
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

// RestartZettel restarts the zettel service alone. Nothing else depends on
// its socket but the daemon's chat calls, which fail loudly while it is out.
func (r *Restarter) RestartZettel(ctx context.Context) error {
	fields := dlog.Context{"zettel": ZettelLabel}
	if err := r.Services.Restart(ctx, ZettelLabel); err != nil {
		r.failed("the zettel restart failed", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", ZettelLabel, err)
	}
	r.Log.Info(opServices, "restarted zettel", fields)
	return nil
}

// EnsureZettelCurrent is the boot's zettel step: the service manager holds the zettel
// service, and it runs the installed build (restarted alone when it does not).
// It is independent of the store and the sidecar -- no shim writes into it --
// so the boot runs it beside the bring-ups, never before a shim starts.
func (r *Restarter) EnsureZettelCurrent(ctx context.Context) error {
	bootstrapped, err := r.ensureLoaded(ctx, ZettelLabel, dlog.Context{"zettel": ZettelLabel})
	if err != nil {
		return err
	}
	return r.zettelCurrent(ctx, bootstrapped)
}

// EnsureZettelLoaded makes sure the service manager holds the zettel service, and touches
// nothing that runs: a joining successor's zettel step, which must not
// restart a service the incumbent is still serving through.
func (r *Restarter) EnsureZettelLoaded(ctx context.Context) error {
	_, err := r.ensureLoaded(ctx, ZettelLabel, dlog.Context{"zettel": ZettelLabel})
	return err
}

// zettelCurrent restarts a running zettel that is not the installed build.
func (r *Restarter) zettelCurrent(ctx context.Context, bootstrapped bool) error {
	stale, err := r.runningStale(ctx, bootstrapped, ComponentZettel, ZettelLabel, buildreport.ServiceZettel)
	if err != nil {
		return err
	}
	if stale {
		r.Log.Info(opServices, "the running zettel is not the installed build; restarting it", nil)
		return r.RestartZettel(ctx)
	}
	r.Log.Debug(opServices, "zettel runs the installed build", nil)
	return nil
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
	return r.EnsureZettelCurrent(ctx)
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
	if storeStale {
		// A STORE RESTART RESTARTS THE SIDECAR TOO: its socket is out while the
		// store restarts, and a fresh pair is the known-good state.
		r.Log.Info(opServices, "the running store is not the installed build; restarting it, and the sidecar with it, before any shim starts", nil)
		if err := r.RestartStore(ctx); err != nil {
			return nil, err
		}
		return func(context.Context) error {
			r.Log.Debug(opServices, "the store's restart already restarted the sidecar onto the installed build", nil)
			return nil
		}, nil
	}
	return func(ctx context.Context) error { return r.ensureSidecarCurrent(ctx, sidecarBootstrapped) }, nil
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
	if _, err := os.Stat(definition); err != nil {
		r.Log.Error(opServices, "a service is not loaded and its definition is missing", dlog.WithCause(fields, err))
		return false, fmt.Errorf("deploy: %s is not loaded and %s cannot bring it back: %w "+
			"(re-run .claude/install.sh)", label, definition, err)
	}
	r.Log.Info(opServices, "a service was not loaded; starting it", fields)
	if err := r.Services.Start(ctx, label); err != nil {
		r.failed("a service could not be started", fields, err)
		return false, fmt.Errorf("deploy: start %s: %w", label, err)
	}
	return true, nil
}

// RestartSidecar restarts the sidecar alone. The store never moved, so its
// socket is up throughout and a plain kickstart loses nothing.
func (r *Restarter) RestartSidecar(ctx context.Context) error {
	return r.timed(restarttiming.Sidecar, func() error { return r.restartSidecar(ctx) })
}

func (r *Restarter) restartSidecar(ctx context.Context) error {
	fields := dlog.Context{"sidecar": SidecarLabel}
	if err := r.Services.Restart(ctx, SidecarLabel); err != nil {
		r.failed("the sidecar restart failed", fields, err)
		return fmt.Errorf("deploy: restart %s: %w", SidecarLabel, err)
	}
	r.Log.Info(opServices, "restarted the sidecar", fields)
	return nil
}

// stopSidecar stops the sidecar and waits for its process's exit, the
// kernel's report that it left; the service manager, which let go of the
// label in the stop, must then no longer hold it.
func (r *Restarter) stopSidecar(ctx context.Context, fields dlog.Context) error {
	loaded, pid, err := r.Services.Status(ctx, SidecarLabel)
	if err != nil {
		r.failed("could not read the sidecar's service-manager state; nothing was stopped", fields, err)
		return fmt.Errorf("deploy: read %s: %w", SidecarLabel, err)
	}
	if !loaded {
		r.Log.Info(opServices, "the sidecar is already stopped", fields)
		return nil
	}
	fields = dlog.Merge(fields, dlog.Context{"pid": pid})
	r.Log.Info(opServices, "stopping the sidecar before the store restarts", fields)
	if err := r.Services.Stop(ctx, SidecarLabel); errors.Is(err, ErrServiceNotLoaded) {
		// THE SIDECAR LEFT BETWEEN THE LOOK AND THE BOOTOUT: the domain no
		// longer holds it, which is the state the stop was asked to reach.
		r.Log.Info(opServices, "the sidecar had already stopped when it was stopped", dlog.WithCause(fields, err))
	} else if err != nil {
		// A stop that errs may still have taken effect; the exit and the
		// service manager's view below are the authority on whether it did.
		r.Log.Warn(opServices, "the sidecar stop answered an error; waiting on its process's exit", dlog.WithCause(fields, err))
	}
	if pid != 0 {
		if err := r.awaitExit(ctx, pid); err != nil {
			if ctx.Err() == nil {
				r.Log.Error(opServices, "the sidecar's process did not exit within the upper bound; the store was NOT restarted",
					dlog.Merge(fields, dlog.Context{"max": r.Windows.Max.String(), "cause": err.Error()}))
			} else {
				r.failed("the sidecar's stop ended with its context", fields, ctx.Err())
			}
			return fmt.Errorf("deploy: %s (pid %d) did not exit: %w", SidecarLabel, pid, err)
		}
	}
	loaded, _, err = r.Services.Status(ctx, SidecarLabel)
	if err != nil {
		r.failed("could not read the sidecar's service-manager state once it stopped", fields, err)
		return fmt.Errorf("deploy: read %s: %w", SidecarLabel, err)
	}
	if loaded {
		r.Log.Error(opServices, "the service manager still holds the sidecar after its stop and its process's exit; the store was NOT restarted", fields)
		return fmt.Errorf("deploy: the service manager still holds %s after its stop", SidecarLabel)
	}
	r.Log.Info(opServices, "the sidecar is stopped", fields)
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
