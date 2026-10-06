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

	"claude-repld/internal/buildid"
	"claude-repld/internal/dlog"
)

// The launchd labels of the two services a deploy restarts.
const (
	StoreLabel   = "com.agentrepl.shim-store"
	SidecarLabel = "com.agentrepl.shim-claude-sidecar"
)

const opServices = "daemon.deploy.services"

// Launchd is the slice of launchctl a service restart drives. The production
// implementation shells out to launchctl in the user's GUI domain; a test
// drives a scripted fake, and NO TEST EVER REACHES THE LIVE LAUNCHD.
type Launchd interface {
	// Print reports whether launchd holds the label and, when it does, the
	// running pid (0 when loaded but not running).
	Print(ctx context.Context, label string) (loaded bool, pid int, err error)
	// Kickstart restarts a loaded service (kickstart -k).
	Kickstart(ctx context.Context, label string) error
	// Bootout removes a service from the user domain, which takes its
	// KeepAlive relaunch with it. A service the domain no longer holds
	// answers an error wrapping ErrServiceNotLoaded.
	Bootout(ctx context.Context, label string) error
	// Bootstrap loads a service from its plist; RunAtLoad starts it.
	Bootstrap(ctx context.Context, plist string) error
}

// ServiceWindows bound a service restart's waits. They are polls because the
// facts they wait on — launchd's view of a label, a socket file appearing, a
// log growing — are another process's and announce nothing.
type ServiceWindows struct {
	// Poll is how often a wait looks again.
	Poll time.Duration
	// Stall is how long the store may go without writing a byte to its log
	// while its socket is still absent before it is called wedged. A boot that
	// is visibly working resets it.
	Stall time.Duration
	// Max is the upper bound on any one wait, however busy the service looks.
	Max time.Duration
}

// DefaultServiceWindows are the ported deploy chain's bounds: a 1s look, a 15s
// stall budget, a 180s ceiling.
var DefaultServiceWindows = ServiceWindows{Poll: time.Second, Stall: 15 * time.Second, Max: 180 * time.Second}

// Restarter restarts the store and the sidecar in the RECORDED SAFE ORDER.
//
// WHY THE SIDECAR IS BOOTED OUT BEFORE THE STORE IS KICKSTARTED. A store
// kickstart unlinks store.sock and does not rebind it until the new process is
// serving. The sidecar writes into that socket continuously, so a store
// restart taken underneath a RUNNING sidecar is an error storm in the
// sidecar's log for as long as the gap lasts (2026-09-13: `storeclient-write-
// batch` "no such file or directory", `store-write` "cursor not advanced",
// `production-suspended`, every one of them the deploy's own doing). The order
// with none of that — sidecar down, store down, store up, sidecar up — is the
// one bin/store-reset.sh records too.
//
// AND THE STOP IS `bootout`, NOT `kill`: both plists set KeepAlive, so launchd
// answers a signalled process with a new pid within a second. Bootout removes
// the service from the domain; bootstrap puts it back and RunAtLoad starts it.
type Restarter struct {
	Launchd Launchd
	// PlistDir is where the installed plists live (~/Library/LaunchAgents),
	// named `<label>.plist`.
	PlistDir string
	// StoreSocket is the store's socket, which the new process binds once it
	// is serving.
	StoreSocket string
	// StoreLog is the store's launchd StandardErrorPath: the only view of a
	// boot in progress.
	StoreLog string
	Windows  ServiceWindows
	Clock    Clock
	Log      dlog.Logger
	// CacheBin is where launchd runs the services from: the installed build a
	// running service is judged against (EnsureCurrent).
	CacheBin string
	// ReportDir is where the services write their build reports.
	ReportDir string
	// Alive reports whether a process is running; nil is the kernel's answer.
	Alive func(pid int) bool
}

// RestartStore restarts the store — and, because its socket is out while it
// restarts, the sidecar with it — in the recorded safe order.
func (r *Restarter) RestartStore(ctx context.Context) error {
	plist := filepath.Join(r.PlistDir, SidecarLabel+".plist")
	fields := dlog.Context{"store": StoreLabel, "sidecar": SidecarLabel, "sidecar_plist": plist}

	// THE PLIST IS PROVED BEFORE ANYTHING IS STOPPED. A bootout with no plist
	// to bootstrap back from would leave the host with no sidecar and no way
	// to return one.
	if _, err := os.Stat(plist); err != nil {
		r.Log.Error(opServices, "the sidecar's plist is missing; nothing was stopped", withCause(fields, err))
		return fmt.Errorf("deploy: the store restart boots the sidecar out and needs %s to bring it back: %w "+
			"(re-run .claude/install.sh --with-agent-shim-services)", plist, err)
	}
	if err := r.stopSidecar(ctx, fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "kickstarting the store", fields)
	if err := r.Launchd.Kickstart(ctx, StoreLabel); err != nil {
		r.failed("the store kickstart failed; the sidecar is down and was NOT started again", fields, err)
		return fmt.Errorf("deploy: kickstart %s: %w", StoreLabel, err)
	}
	if err := r.awaitStoreSocket(ctx, fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "the store is serving; bootstrapping the sidecar", fields)
	if err := r.Launchd.Bootstrap(ctx, plist); err != nil {
		r.failed("the sidecar could not be bootstrapped back", fields, err)
		return fmt.Errorf("deploy: bootstrap %s: %w", plist, err)
	}
	r.Log.Info(opServices, "restarted the store and the sidecar in the safe order", fields)
	return nil
}

// EnsureLoaded makes sure launchd holds both services, BOOTSTRAPPING ONE THE
// USER DOMAIN DOES NOT HOLD, so a daemon never brings shims up against a host
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
	r.Log.Debug(opServices, "launchd holds both services", fields)
	return storeBootstrapped, sidecarBootstrapped, nil
}

// EnsureCurrent is the BOOT's service step: launchd holds both services
// (EnsureLoaded), and neither RUNS an older build than the one installed in
// CacheBin. A running service whose build report is not the installed build
// is restarted in the recorded safe order (RestartStore, which takes the
// sidecar with it, or RestartSidecar alone), judged by the ONE staleness
// check a deploy uses (serviceStaleness).
//
// IT RUNS BEFORE ANY SHIM STARTS, and that is the whole point of it. A bounce
// rebuilds and installs the services' binaries, stands the daemon down, and
// leaves the services to the daemon that boots next: restarted here, they are
// down only while no shim exists to lose them. Stopped from outside instead,
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
	storeBootstrapped, sidecarBootstrapped, err := r.ensureBothLoaded(ctx)
	if err != nil {
		return err
	}
	storeStale, err := r.runningStale(ctx, storeBootstrapped, ComponentStore, StoreLabel, buildreport.ServiceStore)
	if err != nil {
		return err
	}
	if storeStale {
		// A STORE RESTART RESTARTS THE SIDECAR TOO: its socket is out while the
		// store restarts, and a fresh pair is the known-good state.
		r.Log.Info(opServices, "the running store is not the installed build; restarting it, and the sidecar with it, before any shim starts", nil)
		return r.RestartStore(ctx)
	}
	sidecarStale, err := r.runningStale(ctx, sidecarBootstrapped, ComponentSidecar, SidecarLabel, buildreport.ServiceSidecar)
	if err != nil {
		return err
	}
	if sidecarStale {
		r.Log.Info(opServices, "the running sidecar is not the installed build; restarting it before any shim starts", nil)
		return r.RestartSidecar(ctx)
	}
	r.Log.Debug(opServices, "both services run the installed build", nil)
	return nil
}

// runningStale answers whether the service launchd runs under label is
// running a build other than the one installed for it. BOOTSTRAPPED says this
// boot just started it, from the installed build.
func (r *Restarter) runningStale(ctx context.Context, bootstrapped bool, component Component, label, service string) (bool, error) {
	fields := dlog.Context{"label": label, "service": service}
	if bootstrapped {
		r.Log.Debug(opServices, "the service was just bootstrapped from the installed build", fields)
		return false, nil
	}
	_, pid, err := r.Launchd.Print(ctx, label)
	if err != nil {
		r.failed("could not read a service's launchd state to judge its build", fields, err)
		return false, fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if pid == 0 {
		r.Log.Debug(opServices, "launchd runs no process for the service; it starts from the installed build", fields)
		return false, nil
	}
	installed := filepath.Join(r.CacheBin, service)
	fresh, err := buildid.File(installed)
	if err != nil {
		r.Log.Error(opServices, "the service's installed build could not be read; its running build cannot be judged", withCause(merge(fields, dlog.Context{"installed": installed}), err))
		return false, fmt.Errorf("deploy: read the installed %s %s: %w", service, installed, err)
	}
	alive := r.Alive
	if alive == nil {
		alive = processAlive
	}
	return serviceStaleness{reportDir: r.ReportDir, alive: alive, log: r.Log, op: opServices}.stale(component, service, fresh), nil
}

// ensureLoaded bootstraps label from its plist when launchd does not hold it,
// and reports whether it did.
func (r *Restarter) ensureLoaded(ctx context.Context, label string, fields dlog.Context) (bool, error) {
	plist := filepath.Join(r.PlistDir, label+".plist")
	fields = merge(fields, dlog.Context{"label": label, "plist": plist})
	loaded, _, err := r.Launchd.Print(ctx, label)
	if err != nil {
		r.failed("could not read a service's launchd state", fields, err)
		return false, fmt.Errorf("deploy: read %s: %w", label, err)
	}
	if loaded {
		return false, nil
	}
	if _, err := os.Stat(plist); err != nil {
		r.Log.Error(opServices, "a service is not loaded and its plist is missing", withCause(fields, err))
		return false, fmt.Errorf("deploy: %s is not loaded and %s cannot bring it back: %w "+
			"(re-run .claude/install.sh --with-agent-shim-services)", label, plist, err)
	}
	r.Log.Info(opServices, "a service was not loaded; bootstrapping it", fields)
	if err := r.Launchd.Bootstrap(ctx, plist); err != nil {
		r.failed("a service could not be bootstrapped", fields, err)
		return false, fmt.Errorf("deploy: bootstrap %s: %w", plist, err)
	}
	return true, nil
}

// RestartSidecar restarts the sidecar alone. The store never moved, so its
// socket is up throughout and a plain kickstart loses nothing.
func (r *Restarter) RestartSidecar(ctx context.Context) error {
	fields := dlog.Context{"sidecar": SidecarLabel}
	if err := r.Launchd.Kickstart(ctx, SidecarLabel); err != nil {
		r.failed("the sidecar kickstart failed", fields, err)
		return fmt.Errorf("deploy: kickstart %s: %w", SidecarLabel, err)
	}
	r.Log.Info(opServices, "kickstarted the sidecar", fields)
	return nil
}

// stopSidecar boots the sidecar out and waits until launchd no longer holds it.
func (r *Restarter) stopSidecar(ctx context.Context, fields dlog.Context) error {
	loaded, _, err := r.Launchd.Print(ctx, SidecarLabel)
	if err != nil {
		r.failed("could not read the sidecar's launchd state; nothing was stopped", fields, err)
		return fmt.Errorf("deploy: read %s: %w", SidecarLabel, err)
	}
	if !loaded {
		r.Log.Info(opServices, "the sidecar is already stopped", fields)
		return nil
	}
	r.Log.Info(opServices, "booting the sidecar out before the store restarts", fields)
	if err := r.Launchd.Bootout(ctx, SidecarLabel); errors.Is(err, ErrServiceNotLoaded) {
		// THE SIDECAR LEFT BETWEEN THE LOOK AND THE BOOTOUT: the domain no
		// longer holds it, which is the state the bootout was asked to reach.
		r.Log.Info(opServices, "the sidecar had already left the user domain when it was booted out", withCause(fields, err))
	} else if err != nil {
		// A bootout that errs may still have taken effect; the wait below is
		// the authority on whether it did.
		r.Log.Warn(opServices, "the sidecar bootout answered an error; waiting on launchd's own view", withCause(fields, err))
	}
	deadline := r.Clock.Now().Add(r.Windows.Max)
	for {
		loaded, _, err := r.Launchd.Print(ctx, SidecarLabel)
		if err != nil {
			r.failed("could not read the sidecar's launchd state while stopping it", fields, err)
			return fmt.Errorf("deploy: read %s: %w", SidecarLabel, err)
		}
		if !loaded {
			r.Log.Info(opServices, "the sidecar is stopped", fields)
			return nil
		}
		if !r.Clock.Now().Before(deadline) {
			r.Log.Error(opServices, "the sidecar did not leave the user domain within the upper bound; the store was NOT kickstarted",
				merge(fields, dlog.Context{"max": r.Windows.Max.String()}))
			return fmt.Errorf("deploy: %s did not leave the user domain within %s", SidecarLabel, r.Windows.Max)
		}
		if err := r.wait(ctx); err != nil {
			return err
		}
	}
}

// awaitStoreSocket waits ON THE SERVICE, NOT ON A STOPWATCH: while the store
// is alive and its log is advancing, a boot that is slower than any constant
// (a nuked database being replaced) is still working. It ends on the socket,
// or on one of three loud terminals — the store died (launchd reports no
// pid), it is wedged (its log has not grown for the stall budget), or it is
// past the upper bound.
func (r *Restarter) awaitStoreSocket(ctx context.Context, fields dlog.Context) error {
	fields = merge(fields, dlog.Context{"socket": r.StoreSocket, "store_log": r.StoreLog})
	start := r.Clock.Now()
	lastSize := r.storeLogSize()
	progress := start
	for {
		if socketExists(r.StoreSocket) {
			r.Log.Info(opServices, "the store's socket is up", merge(fields, dlog.Context{
				"after": r.Clock.Now().Sub(start).String(),
			}))
			return nil
		}
		_, pid, err := r.Launchd.Print(ctx, StoreLabel)
		if err != nil {
			r.failed("could not read the store's launchd state while it boots", fields, err)
			return fmt.Errorf("deploy: read %s: %w", StoreLabel, err)
		}
		if pid == 0 {
			// LOOK AGAIN BEFORE CALLING IT A DEATH: the socket can appear in
			// the same instant the pid is read.
			if socketExists(r.StoreSocket) {
				continue
			}
			r.Log.Error(opServices, "the store died before its socket appeared; the sidecar was NOT started again",
				merge(fields, dlog.Context{"last_words": r.storeLogTail()}))
			return fmt.Errorf("deploy: %s died before %s appeared", StoreLabel, r.StoreSocket)
		}
		if size := r.storeLogSize(); size != lastSize {
			lastSize = size
			progress = r.Clock.Now()
		}
		now := r.Clock.Now()
		if now.Sub(progress) >= r.Windows.Stall {
			r.Log.Error(opServices, "the store is wedged: no socket and nothing written for the stall budget; the sidecar was NOT started again",
				merge(fields, dlog.Context{"pid": pid, "stall": r.Windows.Stall.String(), "last_words": r.storeLogTail()}))
			return fmt.Errorf("deploy: %s (pid %d) wrote nothing for %s and its socket never appeared", StoreLabel, pid, r.Windows.Stall)
		}
		if now.Sub(start) >= r.Windows.Max {
			r.Log.Error(opServices, "the store's socket did not appear within the upper bound; the sidecar was NOT started again",
				merge(fields, dlog.Context{"pid": pid, "max": r.Windows.Max.String(), "last_words": r.storeLogTail()}))
			return fmt.Errorf("deploy: %s did not appear within %s", r.StoreSocket, r.Windows.Max)
		}
		if err := r.wait(ctx); err != nil {
			return err
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
		r.Log.Info(opServices, "a service step was cut short by its caller: "+message, withCause(fields, err))
		return
	}
	r.Log.Error(opServices, message, withCause(fields, err))
}

// wait is one poll interval on the injected clock, or the context's end.
func (r *Restarter) wait(ctx context.Context) error {
	select {
	case <-ctx.Done():
		r.failed("a service restart ended with its context", nil, ctx.Err())
		return fmt.Errorf("deploy: service restart: %w", ctx.Err())
	case <-r.Clock.After(r.Windows.Poll):
		return nil
	}
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
