package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"

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
	for _, loop := range built.Background {
		go loop.run(serving, log)
	}

	log.Debug("daemon.cmd.serve", "serving", dlog.Context{
		"address": claim.Address(),
		"joining": joining,
	})
	return h.Serve(serving, claim.Listener(), server.H2C(srv))
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
	srv := &http.Server{Handler: h}
	done := make(chan error, 1)
	go func() { done <- srv.Serve(l) }()
	select {
	case err := <-done:
		if errors.Is(err, http.ErrServerClosed) {
			return nil
		}
		return err
	case <-ctx.Done():
		shutdown, cancel := context.WithCancel(context.Background())
		defer cancel()
		if err := srv.Shutdown(shutdown); err != nil {
			return err
		}
		return nil
	}
}
