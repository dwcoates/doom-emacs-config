package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"sync"
	"time"

	"claude-repld/internal/boot"
	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
	"claude-repld/internal/pprofsurface"
	"claude-repld/internal/rollout"
	"claude-repld/internal/server"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// process is what the boot spine resolved before any component was built. It is
// handed to the graph builder so the components are constructed from ONE set of
// resolved facts rather than each re-reading the environment.
type process struct {
	// Opts is the parsed command line.
	Opts options
	// Contracts is the resolved value of the four environment contracts, with
	// the flag overrides already applied.
	Contracts envc.Contracts
	// Layout names every path under the state root.
	Layout stateroot.Layout
	// Surfaces are the log sinks, already open. The run log's open failure was
	// a boot fatal before this value existed.
	Surfaces dlog.Surfaces
	// DB is the state client, opened READ-ONLY for a joining daemon until it
	// has adopted its workspaces.
	DB wsm.DB
	// Claim is the bound loopback listener and the advertisement it may
	// publish.
	Claim daemonaddr.Claim
	// Instance identifies this daemon process for serving ownership. It is
	// minted once per process: an instance id that changed mid-run would let a
	// daemon fail to recognize the workspaces it claimed itself.
	Instance ids.InstanceID
	// Exit ends the serving lifetime. It is the daemon's ORDERLY exit — the
	// drain's deadline and the handover's last transfer both leave through it
	// — and it is handed to the graph rather than taken by it, because only
	// the boot spine owns the lifetime.
	Exit func()
}

// hooks are the seams the process-level tests drive. Production supplies the
// real ones; a test injects fakes so the spine — the exclusivity claim, the
// advertisement, the joining deferral — is exercised without a component graph
// behind it.
type hooks struct {
	// Graph builds the component graph from the resolved process facts. It
	// answers the two argument sets plus the late bindings and the background
	// loops, because two edges of the graph point at the server, which cannot
	// exist until its own dependencies do.
	Graph func(ctx context.Context, p process) (*graph, error)
	// Server builds the Connect surface.
	Server func(server.Deps) (server.Server, error)
	// Serve runs the http server until ctx ends.
	Serve func(ctx context.Context, l net.Listener, h http.Handler) error
}

// productionHooks are the real seams.
func productionHooks() hooks {
	return hooks{
		Graph:  buildGraph,
		Server: server.New,
		Serve:  serve,
	}
}

// run is the daemon's boot spine, in the settled order:
//
//  1. the four environment contracts, with the flag overrides applied;
//  2. the state root's layout, its directories, and the socket path budget;
//  3. the log surfaces — the run log's open failure is a BOOT FATAL, because a
//     daemon that cannot write its own narrative cannot report what it then
//     does wrong;
//  4. pprof, BEFORE any dependency, so a boot wedged on one is still
//     diagnosable through it;
//  5. the ONE loopback listener, bound FIRST as the exclusivity claim — an
//     unflagged second daemon loses it and exits without disturbing the
//     incumbent's listeners or its advertisement;
//  6. daemon.addr, written atomically by an incumbent and DEFERRED by a
//     joining successor, which instead reports its address where the
//     incumbent that spawned it is waiting;
//  7. the state client, read-only while a successor has adopted nothing;
//  8. the component graph, the boot reconciliation, and the Connect surface;
//  9. an orderly exit that withdraws the advertisement and closes the streams.
func run(ctx context.Context, opts options, h hooks) error {
	contracts := envc.Load()
	if opts.fake {
		contracts = contracts.WithFake(true)
	}
	contracts = contracts.WithStateDir(opts.stateDir)

	layout, err := stateroot.Root(opts.stateDir, contracts.StateDir())
	if err != nil {
		return fmt.Errorf("claude-repld: resolve the state root: %w", err)
	}
	for _, dir := range layout.Dirs() {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return fmt.Errorf("claude-repld: create %s: %w", dir, err)
		}
	}
	if err := layout.CheckSocketPathBudget(); err != nil {
		return fmt.Errorf("claude-repld: the state root cannot hold a shim socket: %w", err)
	}

	surfaces, err := dlog.OpenSurfaces(layout.RunLog(), false)
	if err != nil {
		return fmt.Errorf("claude-repld: open the run log: %w", err)
	}
	defer surfaces.Close()
	log := surfaces.Global()

	// PPROF BEFORE ANY DEPENDENCY. A wildcard or routable bind is refused here
	// rather than opened, and an empty setting is OFF, which is the default.
	profiling, err := pprofsurface.Open(ctx, opts.pprof, log)
	if err != nil {
		log.Error("daemon.cmd.pprof", "the profiling surface was refused", dlog.Context{
			"pprof": opts.pprof,
			"error": err.Error(),
		})
		return fmt.Errorf("claude-repld: open the profiling surface: %w", err)
	}
	if profiling != nil {
		defer profiling.Close()
	}

	// THE ADDRESS CLAIM IS THE EXCLUSIVITY CLAIM, and it is taken before any
	// socket is touched. A successor is distinguishable because it was SPAWNED
	// with -joining, never because it raced and won -- and so it does NOT take
	// the claim here: the incumbent that spawned it still holds it, and a
	// successor racing for it would lose to its own predecessor and exit. It
	// takes the claim when it advertises, which is when it has taken over.
	joining := opts.joining != ""
	bindClaim := daemonaddr.Bind
	if joining {
		bindClaim = daemonaddr.BindJoining
	}
	claim, err := bindClaim(layout.DaemonAddr(), 0)
	if err != nil {
		if errors.Is(err, daemonaddr.ErrClaimed) {
			log.Warn("daemon.cmd.claim", "another daemon holds the boot claim; exiting without disturbing it", dlog.Context{
				"addr_path": layout.DaemonAddr(),
			})
			return err
		}
		log.Error("daemon.cmd.claim", "the loopback listener could not be claimed", dlog.Context{
			"addr_path": layout.DaemonAddr(),
			"error":     err.Error(),
		})
		return fmt.Errorf("claude-repld: claim the daemon address: %w", err)
	}
	defer claim.Close()

	if joining {
		// A JOINING DAEMON DOES NOT ADVERTISE. It reports its address where the
		// incumbent that spawned it is waiting, and daemon.addr is written only
		// once it owns every workspace (the rollout's WriteDaemonAddr hook).
		if err := rollout.ReportJoiningAddr(layout.Dir(), claim.Address()); err != nil {
			log.Error("daemon.cmd.claim", "the successor could not report its address", dlog.Context{
				"address": claim.Address(),
				"error":   err.Error(),
			})
			return fmt.Errorf("claude-repld: report the joining address: %w", err)
		}
		log.Debug("daemon.cmd.claim", "a joining successor bound and reported its address", dlog.Context{
			"address":   claim.Address(),
			"incumbent": opts.joining,
		})
	} else {
		if err := claim.Publish(); err != nil {
			log.Error("daemon.cmd.claim", "daemon.addr could not be published", dlog.Context{
				"address": claim.Address(),
				"error":   err.Error(),
			})
			return fmt.Errorf("claude-repld: publish daemon.addr: %w", err)
		}
		log.Debug("daemon.cmd.claim", "the incumbent published its address", dlog.Context{
			"address": claim.Address(),
		})
	}
	// THE ORDERLY EXIT WITHDRAWS THE ADVERTISEMENT. A daemon.addr left behind
	// names a listener nobody is serving, and the next client dials it.
	defer func() {
		if err := claim.Withdraw(); err != nil {
			log.Error("daemon.cmd.exit", "daemon.addr could not be withdrawn", dlog.Context{
				"error": err.Error(),
			})
		}
	}()

	db, err := openState(ctx, layout, log, joining)
	if err != nil {
		return err
	}
	defer db.Close()

	// SERVING IS ITS OWN LIFETIME, cancelled either by the process's signal
	// context or by the daemon's own orderly exit — the drain's deadline and
	// the handover's last transfer both end the process through it.
	serving, stopServing := context.WithCancel(ctx)
	defer stopServing()

	built, err := h.Graph(serving, process{
		Opts:      opts,
		Contracts: contracts,
		Layout:    layout,
		Surfaces:  surfaces,
		DB:        db,
		Claim:     claim,
		Instance:  wsm.NewInstanceID(),
		Exit:      stopServing,
	})
	if err != nil {
		log.Error("daemon.cmd.graph", "the component graph could not be built", dlog.Context{
			"error": err.Error(),
		})
		return err
	}

	// THE WATCHERS CLOSE BEFORE THE STATE CLIENT. Deferred here, after
	// `defer db.Close()`, so it runs FIRST: a session watcher's sinks read the
	// state client off the watcher's own goroutine, and the orderly exit that
	// closed the store under one would leave a turn end being handled with a
	// refused read on a path that owes no error at all.
	//
	// THE TEARDOWNS ARE ARMED BEFORE THE BOOT RUNS, NOT AFTER IT. The boot
	// sequence ADOPTS surviving shims (boot step "adopt"), which installs a
	// live watcher whose frame pump is already delivering into the resolvers;
	// a LATER step of the same sequence can still fail the boot outright
	// (restoreHolds on a corrupt held_prompts row is the worked example), and
	// `sequence.Run`'s error returns from this function. Armed after the run,
	// these defers were never registered on that path, so `defer db.Close()`
	// closed the store under the adopted watcher's in-flight frames and the
	// feed resolver's next workspace lookup read a closed database. Arming
	// them here makes the failed boot tear down in exactly the order the
	// orderly exit does.
	if built.CloseWatchers != nil {
		defer built.CloseWatchers()
	}

	// THE MERGE DRAIN RUNS FIRST OF ALL THE TEARDOWNS. Deferred after the
	// watchers and after `defer db.Close()`, so it runs BEFORE both: a merge
	// that has reached its terminal is still writing merged_at, closed and its
	// lease release, and closing the state client under those writes lost them
	// to failed transactions and left the lease held. The wait is BOUNDED
	// (merge.TerminalDrainBound) and takes its own context, because the
	// process's signal context is already cancelled by the time it runs.
	if built.DrainMerges != nil {
		defer built.DrainMerges(context.Background())
	}

	sequence, err := boot.New(built.Boot)
	if err != nil {
		return fmt.Errorf("claude-repld: build the boot sequence: %w", err)
	}
	report, err := sequence.Run(ctx)
	if err != nil {
		return err
	}
	log.Debug("daemon.cmd.boot", "the boot reconciliation completed", dlog.Context{
		"adopted":        len(report.Adopted),
		"orphans_closed": len(report.Orphaned),
		"holds_restored": report.HoldsRestored,
	})

	srv, err := h.Server(built.Server)
	if err != nil {
		return fmt.Errorf("claude-repld: build the server: %w", err)
	}
	defer srv.Close()

	// THE LATE BINDINGS ARE COMPLETED BEFORE ANYTHING IS SERVED: the rollout's
	// and the drain's pushes, and the workspace verbs' host relay, all reach
	// the surface that has just been built.
	if built.Bind != nil {
		built.Bind(srv)
	}
	if built.Prime != nil {
		if err := built.Prime(serving); err != nil {
			log.Error("daemon.cmd.serve", "the opening views could not be published", dlog.Context{
				"error": err.Error(),
			})
			return fmt.Errorf("claude-repld: publish the opening views: %w", err)
		}
	}
	// The background loops start only now, for the same reason: each of them
	// can push, and pushing into a surface that does not exist is a drop.
	//
	// AND THE EXIT JOINS THEM, for the same reason the watchers and the merge
	// drain are joined above. A loop's iteration reads and writes the state
	// client off its OWN goroutine: the drain sweep's hibernation releases the
	// lease and then tells the prompt queue, and a SIGTERM landing inside that
	// step left `daemon.promptqueue.lease_changed: could not read the standing
	// holds — sql: database is closed` in the log of an ORDERLY exit
	// (reproduced at -count=10 -parallel 8). This defer is registered LAST, so
	// it runs FIRST of every teardown: no loop is still running when the merge
	// drain, the watchers or the state client are torn down.
	var loops sync.WaitGroup
	for _, loop := range built.Background {
		loops.Add(1)
		go func(loop backgroundLoop) {
			defer loops.Done()
			loop.run(serving, log)
		}(loop)
	}
	defer joinBackgroundLoops(&loops, loopJoinBound, log)
	// AND THE QUEUE'S OWN GOROUTINES, for the same reason and on the same
	// bound: a classification verdict and a background revival each read and
	// write the state client off their own goroutine.
	if built.DrainQueue != nil {
		defer joinQueueWork(built.DrainQueue, loopJoinBound, log)
	}

	log.Debug("daemon.cmd.serve", "serving", dlog.Context{
		"address": claim.Address(),
		"joining": joining,
	})
	return h.Serve(serving, claim.Listener(), server.H2C(srv, log))
}

// openState opens the state client. A JOINING daemon opens it READ-ONLY: until
// it has adopted a workspace the incumbent is still the sole writer, and two
// writers on one SQLite file is the lost-update class the single-handle rule
// exists to prevent.
func openState(ctx context.Context, layout stateroot.Layout, log dlog.Logger, joining bool) (wsm.DB, error) {
	open := wsm.Open
	if joining {
		open = wsm.OpenReadOnly
	}
	db, err := open(ctx, layout.DB(), wsm.WithLogger(log))
	if err != nil {
		log.Error("daemon.cmd.state", "the state client could not be opened", dlog.Context{
			"path":    layout.DB(),
			"joining": joining,
			"error":   err.Error(),
		})
		return nil, fmt.Errorf("claude-repld: open the state client: %w", err)
	}
	return db, nil
}

// serve runs the http server on the claimed listener until ctx ends, then shuts
// it down gracefully.
func serve(ctx context.Context, l net.Listener, h http.Handler) error {
	// THE GRACE HAS TO BE OURS, so the handler must carry the gate that counts
	// the calls being answered. `Server.Shutdown` cannot do it: every client
	// dials h2c, `h2c.NewHandler` serves such a connection by HIJACKING it,
	// and net/http neither closes nor waits for a hijacked connection — so
	// Shutdown returns at once and the exit runs over the calls in flight.
	// See server.Serving.
	gate, ok := h.(server.RequestGate)
	if !ok {
		return fmt.Errorf("claude-repld: the serving handler carries no in-flight request gate; an orderly exit would cut every call it is still answering")
	}
	srv := &http.Server{Handler: h}
	done := make(chan error, 1)
	// SERVED THROUGH THE GATE'S OWN LISTENER, so the calls it counts are held
	// open until their bytes are on the socket. A handler returning is not its
	// answer leaving, and neither is the request context's cancellation — see
	// server.WriteBarrier.
	go func() { done <- srv.Serve(gate.Listener(l)) }()
	select {
	case err := <-done:
		if errors.Is(err, http.ErrServerClosed) {
			return nil
		}
		return err
	case <-ctx.Done():
		// THE CALLS BEING ANSWERED GO FIRST, and this is the wait that
		// actually happens. `UpdateShutdownSchedule{now}` ends the serving
		// lifetime from inside its own handler, so the exit races its own
		// answer unless something holds the exit until that answer is written.
		// Measured in the e2e sandbox: the handler recorded "applied the
		// shutdown schedule" and every open stream had been ended 211
		// microseconds later, so the caller read `unexpected EOF` from a stop
		// the daemon had performed. AwaitQuiet says so loudly when its own
		// bound expires; nothing is swallowed.
		gate.AwaitQuiet(shutdownGrace)
		// AND THEN THE STANDING STREAMS' OWN LAST WORDS. The gate counts
		// unary calls only — a Watch* handler does not return until its
		// client goes away, so counting one would make every exit wait out
		// its whole grace — but the daemon's last push goes out on exactly
		// those streams. `drain.fire` pushes `DaemonShutdownAnnounced` onto
		// every WatchDaemon stream and calls Exit on the next line, so
		// AwaitQuiet sees nothing in flight, returns at once, and Shutdown
		// closes the streams over an announcement that never reached the
		// socket. The page then waits out its own budget for a banner the
		// daemon did draw, which is `TestWebappLayerRoster`'s "the drain
		// banner was never drawn within 5000ms".
		//
		// Bounded by writesQuietBound, and a link still speaking at the end
		// of it says so through the gate's own logger rather than being
		// treated as an error: one h2 connection multiplexes the pushes with
		// everything else, so a genuinely busy link never falls silent and is
		// not a lost announcement.
		gate.AwaitWritesQuiet(writesQuietBound)
		// THE GRACE IS BOUNDED. Graceful shutdown waits for every in-flight
		// request, and this daemon's Watch* handlers are STANDING STREAMS that
		// end only when their client goes away — so an unbounded wait is a
		// daemon that never exits whenever anything is watching it, which is
		// every handover. In-flight unary calls get the grace; whatever is
		// still open when it expires is closed.
		shutdown, cancel := context.WithTimeout(context.Background(), shutdownGrace)
		defer cancel()
		err := srv.Shutdown(shutdown)
		if errors.Is(err, context.DeadlineExceeded) {
			return srv.Close()
		}
		return err
	}
}

// shutdownGrace is how long in-flight requests have to finish before the
// standing streams are closed underneath them.
const shutdownGrace = 2 * time.Second

// writesQuietBound is how long the exit gives the connections to stop writing
// after every counted call has been answered.
//
// IT IS `server.answerWriteBound`'s SIBLING and is sized the same way: what it
// covers is one `write(2)` of a few dozen bytes of a push onto a loopback or
// unix socket by a goroutine that is already runnable, and the barrier ends on
// quiescence rather than on this clock, so the ordinary exit spends about two
// `barrierSettle` ticks here rather than this bound. 250ms is a last resort,
// deliberately well under `shutdownGrace` so a link that never falls silent
// cannot spend the exit's own budget on top of it.
const writesQuietBound = 250 * time.Millisecond

// loopJoinBound is how long the exit waits for the background loops to leave
// after their serving context ended. Every loop is a ticker whose iteration is
// itself bounded, so this is not a shutdown budget: it covers one in-flight
// iteration finishing. A loop still running after it is a FAULT to report —
// the teardown that follows will run under it — never something to keep
// waiting on, because an unbounded wait is a daemon that does not exit.
const loopJoinBound = 2 * time.Second

// joinQueueWork waits, bounded, for the prompt queue's own background
// goroutines to leave, and says so loudly when they do not.
func joinQueueWork(drain func(time.Duration) bool, bound time.Duration, log dlog.Logger) {
	if drain(bound) {
		log.Debug("daemon.cmd.serve", "the prompt queue's background work left before the teardown", nil)
		return
	}
	log.Error("daemon.cmd.serve", "the prompt queue's background work outlived its serving context; tearing down under it", dlog.Context{
		"bound_ms": bound.Milliseconds(),
	})
}

// joinBackgroundLoops waits, bounded, for every background loop to leave, and
// says so loudly when one does not.
func joinBackgroundLoops(loops *sync.WaitGroup, bound time.Duration, log dlog.Logger) {
	left := make(chan struct{})
	go func() {
		loops.Wait()
		close(left)
	}()
	select {
	case <-left:
		log.Debug("daemon.cmd.serve", "every background loop left before the teardown", nil)
	case <-time.After(bound):
		log.Error("daemon.cmd.serve", "a background loop outlived its serving context; tearing down under it", dlog.Context{
			"bound_ms": bound.Milliseconds(),
		})
	}
}
