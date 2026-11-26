// Command agent-repl-wifi is the persistent-wifi service: a singleton the
// platform's service manager keeps running, like the store and the test
// service. It owns the machine's lid-closed operation (the lid and power
// source watchers, the power settings, the display sleep, the application
// freeze and the Wi-Fi failover) and serves persistentwifi.v1.PersistentWifi
// to the agent-repl daemon over a unix socket a socket holder owns, so the
// daemon's connects queue until the service accepts.
//
// Flags (every path is injectable, so a test can point it at a temp dir):
//
//	--socket        UDS path to serve on, bound itself only with no --holder
//	--holder        the socket holder's control socket: the listener is taken
//	                from it, so the socket outlives every restart
//	--listen-fd     an inherited, already-listening unix socket to serve on
//	                (unset: -1); a test harness binds the socket itself, so a
//	                client's connect queues until the service accepts
//	--state-dir     the state root (default AGENT_REPL_STATE_DIR, else
//	                ~/.claude-emacs): the run log and the frozen set live under it
//	--metrics-root  the always-on metrics surface's root (empty = off)
//
// The verb `thaw-guard` runs the thaw guard (freeze.go) the service starts.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/logsite"
	"agentrepl/logging/metrics"
	"agentrepl/logging/singletonsocket"
	"agentrepl/logging/sockethold"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/netwatch"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/wifimetrics"
	"agent-repld/internal/wifirpc"
)

// holderSocketName is the name of the listener the service takes from its
// socket holder (the holder's `sockets` entry in daemon/requirements.json).
const holderSocketName = "persistent-wifi"

// shutdownGrace bounds the orderly drain of in-flight requests: a standing
// watch is one and ends with the drain, and the daemon opens it again. Five
// times the measured worst, rounded up to the millisecond: 50 drains of a
// server with a watch open took at worst 0.33ms (p99 0.33ms) on 2026-11-26
// under the test runner's CPU governor, and 5 x 0.33ms is 1.65ms.
const shutdownGrace = 2 * time.Millisecond

// probeTimeout bounds the dial that decides whether an existing socket has a
// live service behind it, when the service binds its own socket. Five times
// the measured worst, rounded up to the millisecond: 300 dials to a live
// unix socket took at worst 24us (p99 20us) on 2026-11-26 under the test
// runner's CPU governor, and 5 x 24us is 120us.
const probeTimeout = time.Millisecond

// stopGrace bounds the controller's loops ending once the service is told to
// stop. Five times the measured worst, rounded up to the millisecond: 200
// stops of the controller's loop over its fakes took at worst 0.25ms (p99
// 0.12ms) on 2026-11-26 under the test runner's CPU governor, and 5 x 0.25ms
// is 1.25ms. The measurement did not run the operating system's own lid and
// network watchers, whose stop is bounded by the same constant.
const stopGrace = 2 * time.Millisecond

func main() {
	if len(os.Args) > 1 && os.Args[1] == thawGuardVerb {
		os.Exit(runThawGuard(os.Args[2:], os.Stderr))
	}
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl-wifi: resolve the home directory: %v\n", err)
		os.Exit(1)
	}
	socketPath := flag.String("socket", filepath.Join(home, ".cache", "agent-repl", "sock", "persistent-wifi.sock"), "UDS path to serve persistentwifi.v1.PersistentWifi on")
	holderPath := flag.String("holder", "", "the socket holder's control socket: the listener is taken from it instead of bound")
	listenFD := flag.Int("listen-fd", -1, "serve on this inherited, already-listening unix socket instead of binding --socket")
	stateDir := flag.String("state-dir", "", "the state root (default $AGENT_REPL_STATE_DIR, else ~/.claude-emacs)")
	metricsRoot := flag.String("metrics-root", "", "the always-on metrics surface's root; empty = off")
	flag.Parse()
	layout, err := stateroot.Root(*stateDir, os.Getenv("AGENT_REPL_STATE_DIR"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl-wifi: resolve the state root: %v\n", err)
		os.Exit(1)
	}
	if err := run(options{socket: *socketPath, holder: *holderPath, listenFD: *listenFD, layout: layout, metricsRoot: *metricsRoot}); err != nil {
		os.Exit(1)
	}
}

// options are the flags, resolved.
type options struct {
	socket      string
	holder      string
	listenFD    int
	layout      stateroot.Layout
	metricsRoot string
}

// runLog is the service's run log, beside the thaw guard's.
func runLog(layout stateroot.Layout) string {
	return filepath.Join(layout.LogsDir(), "persistent-wifi.run.log")
}

// run opens the log, builds the controller and serves until a termination
// signal. Every failure after the log opens is recorded before it returns.
func run(o options) (err error) {
	surfaces, _, err := dlog.OpenSurfaces(runLog(o.layout))
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl-wifi: open the log: %v\n", err)
		return err
	}
	defer func() { _ = surfaces.Close() }()
	log := surfaces.Global()
	const op = "daemon.persistentwifi.service"
	defer func() {
		if err != nil {
			log.Error(op, "the persistent-wifi service is exiting on a failure", dlog.Context{"cause": err.Error()})
		} else {
			log.Info(op, "the persistent-wifi service is exiting cleanly", nil)
		}
	}()
	announceBuild(log, os.Getenv)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(sigc)

	selfExe, err := os.Executable()
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: resolve the service's own path: %w", err)
	}
	cfg, err := persistentwifi.ConfigFromEnv(os.Getenv)
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: resolve the persistent-wifi config: %w", err)
	}
	runner, err := scriptrunner.New(log)
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: build the tool runner: %w", err)
	}
	telemetry := wifimetrics.New()
	surface, err := metrics.OpenService(metrics.ProgramPersistentWifi, o.metricsRoot, metricsRecord(log), telemetry.Collectors()...)
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: open the metrics surface: %w", err)
	}
	defer metrics.CloseService(surface, metricsRecord(log))

	peers := wifirpc.NewPeers()
	freezer, err := buildFreezer(cfg, runner, o.layout.Dir(), selfExe, peers.PIDs, telemetry, log)
	if err != nil {
		return err
	}
	// THE NETWORK'S OWN EVENTS are heard here: the service is the one
	// process whose Wi-Fi link matters to it. A fake host (the harness's)
	// hears none, so a suite never reacts to the real network.
	openNetworkEvents := netwatch.OpenSystem
	if cfg.FakeLid != "" {
		openNetworkEvents = netwatch.Silent
	}
	network, err := netwatch.New(openNetworkEvents, log)
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: build the network watcher: %w", err)
	}
	controller, err := persistentwifi.New(persistentwifi.Deps{
		Config:         cfg,
		Runner:         runner,
		Clock:          clock.System{},
		NetworkChanged: network.Changed,
		PowerChanges:   cfg.PowerChanges(),
		Log:            log,
		Lid:            cfg.Lid(),
		PowerSource:    cfg.PowerSource(),
		LegacyLidDims:  sqliteLidDims{path: o.layout.DB()},
		Freezer:        freezer,
		Telemetry:      telemetry,
		EnergyRecord:   energyRecord(o.layout.Dir()),
	})
	if err != nil {
		return fmt.Errorf("agent-repl-wifi: build the persistent-wifi controller: %w", err)
	}
	ln, err := listen(o, log)
	if err != nil {
		return err
	}
	srv := wifirpc.NewServer(controller, log, peers)

	ctx, stop := context.WithCancel(context.Background())
	defer stop()
	// THE STANDING IS READ BEFORE ANYTHING IS SERVED, so the first message
	// any watch sends is a standing somebody read.
	controller.Refresh(ctx)

	var loops sync.WaitGroup
	loopErrs := make(chan error, 4)
	for name, run := range map[string]func(context.Context) error{
		"persistent_wifi":              controller.Run,
		"persistent_wifi_lid":          controller.WatchLid,
		"persistent_wifi_power_source": controller.WatchPowerSource,
		"network_watch":                network.Run,
	} {
		loops.Add(1)
		go func() {
			defer loops.Done()
			if err := run(ctx); err != nil {
				log.Error(op, "a persistent-wifi loop ended on a failure", dlog.Context{"loop": name, "cause": err.Error()})
				loopErrs <- fmt.Errorf("agent-repl-wifi: loop %s: %w", name, err)
			}
		}()
	}
	serveErr := make(chan error, 1)
	go func() { serveErr <- srv.Serve(ln) }()

	var result error
	select {
	case sig := <-sigc:
		log.Info(op, "received a termination signal", dlog.Context{"signal": sig.String()})
	case result = <-loopErrs:
	case err := <-serveErr:
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			log.Error(op, "serving ended on a failure", dlog.Context{"cause": err.Error()})
			result = err
		}
	}
	stop()
	drain, cancel := context.WithTimeout(context.Background(), shutdownGrace)
	defer cancel()
	if err := srv.Shutdown(drain); err != nil {
		log.Error(op, "draining in-flight requests failed", dlog.Context{"cause": err.Error(), "bound": shutdownGrace.String()})
		result = errors.Join(result, err)
	}
	if !waitFor(&loops, stopGrace) {
		log.Error(op, "the persistent-wifi loops did not end within their bound", dlog.Context{"bound": stopGrace.String()})
		result = errors.Join(result, errors.New("agent-repl-wifi: the loops did not end in time"))
	}
	return result
}

// waitFor answers whether wg finished within bound.
func waitFor(wg *sync.WaitGroup, bound time.Duration) bool {
	done := make(chan struct{})
	go func() { wg.Wait(); close(done) }()
	select {
	case <-done:
		return true
	case <-time.After(bound):
		return false
	}
}

// listen answers the listener the service serves on: the inherited socket
// (a harness's), else the one its socket holder hands it, else SOCKET bound
// as the host's one service. A failure is recorded before it is answered.
func listen(o options, log dlog.Logger) (net.Listener, error) {
	const op = "daemon.persistentwifi.service.listen"
	switch {
	case o.listenFD >= 0:
		file := os.NewFile(uintptr(o.listenFD), "persistent-wifi-listener")
		defer func() { _ = file.Close() }()
		ln, err := net.FileListener(file)
		if err != nil {
			log.Error(op, "the inherited listener could not be opened", dlog.Context{"fd": o.listenFD, "cause": err.Error()})
			return nil, fmt.Errorf("agent-repl-wifi: --listen-fd %d: %w", o.listenFD, err)
		}
		log.Info(op, "serving on the inherited listener", dlog.Context{"fd": o.listenFD})
		return ln, nil
	case o.holder != "":
		ln, took, err := sockethold.TakeTimed(sockethold.Take, o.holder, holderSocketName)
		if err != nil {
			log.Error(op, "the service could not take its listener from the socket holder within {bound} after {elapsed}", dlog.Context{
				"holder": o.holder, "name": holderSocketName, "bound": sockethold.TakeBound.String(), "measured": sockethold.TakeMeasured.String(), "elapsed": took.String(), "cause": err.Error()})
			return nil, fmt.Errorf("agent-repl-wifi: take the %q listener from the holder %s: %w", holderSocketName, o.holder, err)
		}
		log.Info(op, "took its listener from the socket holder in {elapsed}", dlog.Context{"holder": o.holder, "name": holderSocketName, "elapsed": took.String()})
		return ln, nil
	default:
		ln, _, err := singletonsocket.Listen(o.socket, probeTimeout)
		if err != nil {
			log.Error(op, "binding the service socket failed", dlog.Context{"socket": o.socket, "cause": err.Error()})
			return nil, fmt.Errorf("agent-repl-wifi: bind %s: %w", o.socket, err)
		}
		log.Info(op, "serving on its own socket", dlog.Context{"socket": o.socket})
		return ln, nil
	}
}

// announceBuild writes the service's own build report, so the daemon's deploy
// can tell whether it runs the fresh build. A failure is recorded and serving
// goes on: a missing report only makes the deploy restart the service.
func announceBuild(log dlog.Logger, getenv func(string) string) {
	const op = "daemon.persistentwifi.service.buildreport"
	report, err := buildreport.Self()
	if err != nil {
		log.Error(op, "the service could not determine its own build", dlog.Context{"cause": err.Error()})
		return
	}
	dir, err := buildreport.ResolveDir(getenv)
	if err != nil {
		log.Error(op, "the service could not resolve its build report directory", dlog.Context{"cause": err.Error()})
		return
	}
	if err := buildreport.Write(dir, buildreport.ServicePersistentWifi, report); err != nil {
		log.Error(op, "the service failed to write its build report", dlog.Context{"cause": err.Error(), "dir": dir})
		return
	}
	log.Info(op, "the service reported its build", dlog.Context{"build": report.Build, "path": buildreport.Path(dir, buildreport.ServicePersistentWifi)})
}

// metricsRecord is the service's canonical logger as the metrics surface
// records its decisions through it.
func metricsRecord(log dlog.Logger) metrics.Record {
	return func(level string, message logsite.Message, values map[string]any) {
		log.Log(level, "daemon.persistentwifi.service.metrics", message, dlog.Context(values))
	}
}
