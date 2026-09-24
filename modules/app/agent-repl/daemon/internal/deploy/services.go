package deploy

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

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
		r.Log.Error(opServices, "the store kickstart failed; the sidecar is down and was NOT started again", withCause(fields, err))
		return fmt.Errorf("deploy: kickstart %s: %w", StoreLabel, err)
	}
	if err := r.awaitStoreSocket(ctx, fields); err != nil {
		return err
	}
	r.Log.Info(opServices, "the store is serving; bootstrapping the sidecar", fields)
	if err := r.Launchd.Bootstrap(ctx, plist); err != nil {
		r.Log.Error(opServices, "the sidecar could not be bootstrapped back", withCause(fields, err))
		return fmt.Errorf("deploy: bootstrap %s: %w", plist, err)
	}
	r.Log.Info(opServices, "restarted the store and the sidecar in the safe order", fields)
	return nil
}

// RestartSidecar restarts the sidecar alone. The store never moved, so its
// socket is up throughout and a plain kickstart loses nothing.
func (r *Restarter) RestartSidecar(ctx context.Context) error {
	fields := dlog.Context{"sidecar": SidecarLabel}
	if err := r.Launchd.Kickstart(ctx, SidecarLabel); err != nil {
		r.Log.Error(opServices, "the sidecar kickstart failed", withCause(fields, err))
		return fmt.Errorf("deploy: kickstart %s: %w", SidecarLabel, err)
	}
	r.Log.Info(opServices, "kickstarted the sidecar", fields)
	return nil
}

// stopSidecar boots the sidecar out and waits until launchd no longer holds it.
func (r *Restarter) stopSidecar(ctx context.Context, fields dlog.Context) error {
	loaded, _, err := r.Launchd.Print(ctx, SidecarLabel)
	if err != nil {
		r.Log.Error(opServices, "could not read the sidecar's launchd state; nothing was stopped", withCause(fields, err))
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
			r.Log.Error(opServices, "could not read the sidecar's launchd state while stopping it", withCause(fields, err))
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
			r.Log.Error(opServices, "could not read the store's launchd state while it boots", withCause(fields, err))
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

// wait is one poll interval on the injected clock, or the context's end.
func (r *Restarter) wait(ctx context.Context) error {
	select {
	case <-ctx.Done():
		r.Log.Error(opServices, "a service restart ended with its context", dlog.Context{"cause": ctx.Err().Error()})
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
