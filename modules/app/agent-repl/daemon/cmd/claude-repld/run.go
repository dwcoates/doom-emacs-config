package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"sort"
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
	"claude-repld/internal/tempdirs"
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
	Exit func(standDownReason)
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
	// Serve runs the http server until ctx ends. onShuttingDown is invoked
	// once, at the very start of the shutdown sequence, while the listener is
	// still accepting — it is where the advertisement is withdrawn so a client
	// forwarding during shutdown finds no address rather than a present address
	// backed by a listener that has stopped accepting. endStreams ends every
	// standing stream the surface serves (the server's Close); the shutdown
	// runs it once the answers owed have left, and waits for every stream's
	// end frame before the process can exit.
	Serve func(ctx context.Context, l net.Listener, h http.Handler, onShuttingDown, endStreams func()) error
	// BootStall bounds the whole boot reconciliation; zero means
	// bootStallBound. It is a seam because the behavior under test is a
	// reconciliation that never finishes, and a test must not wait out a
	// production last resort to observe it.
	BootStall time.Duration
	// ClaimWait is how long the boot waits for a HELD boot claim before it
	// believes a live incumbent holds it. Production supplies
	// daemonaddr.ClaimWaitBound; zero -- what a test hooks value leaves it --
	// refuses a held claim on sight, so a suite proving the exclusivity ruling
	// does not wait out an incumbent that is never going to depart.
	ClaimWait time.Duration
	// StateCheckEvery is the cadence of the serving daemon's check that its
	// state root, daemon.lock and daemon.addr are still there; zero means
	// stateRootCheckEvery.
	StateCheckEvery time.Duration
}

// productionHooks are the real seams.
func productionHooks() hooks {
	return hooks{
		Graph:     buildGraph,
		Server:    server.New,
		Serve:     serve,
		BootStall: bootStallBound,
		ClaimWait: daemonaddr.ClaimWaitBound,
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

	surfaces, err := dlog.OpenSurfaces(layout.RunLog())
	if err != nil {
		return fmt.Errorf("claude-repld: open the run log: %w", err)
	}
	defer surfaces.Close()
	log := surfaces.Global()
	joining := opts.joining != ""
	log.Info("daemon.cmd.boot", "the daemon boot started", dlog.Context{
		"joining":    joining,
		"state_root": layout.Dir(),
	})
	defer log.Info("daemon.cmd.exit", "the daemon process ended", dlog.Context{
		"joining":    joining,
		"state_root": layout.Dir(),
	})

	// SIGQUIT IS TAKEN OVER BEFORE ANYTHING CAN WEDGE. Emacs launches the
	// daemon with its stderr discarded, so the runtime's own SIGQUIT dump goes
	// nowhere: pid 31984 spent ten hours listening and accepting nothing, and
	// the one gesture that would have named its blocking call produced no
	// evidence at all. The dump now lands in the run log.
	defer armGoroutineDump(log)()

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
	//
	// A HELD CLAIM IS WAITED ON, NOT BELIEVED ON SIGHT. daemon.addr is
	// withdrawn at the start of the outgoing daemon's shutdown while its claim
	// is held until its process ends, so a replacement is spawned into a window
	// where the address is gone and the claim is not yet free; exiting on the
	// first refusal there destroyed the daemon instead of replacing it.
	claimWait := h.ClaimWait
	if opts.replacing && claimWait < rollout.ReplacementClaimWait {
		// A REPLACEMENT WAITS OUT ITS INCUMBENT'S WHOLE ORDERLY EXIT: the
		// incumbent spawned it just before that exit began, and the claim is
		// released only when the exit ends.
		claimWait = rollout.ReplacementClaimWait
	}
	bindClaim := func(addrPath string, port int) (daemonaddr.Claim, error) {
		return daemonaddr.BindWithin(addrPath, port, claimWait)
	}
	if joining {
		bindClaim = daemonaddr.BindJoining
	}
	claim, err := bindClaim(layout.DaemonAddr(), 0)
	if err != nil {
		if errors.Is(err, daemonaddr.ErrClaimed) {
			// THE MECHANISM WORKING. Emacs spawns a daemon whenever it cannot
			// tell that one is already serving, so a loser is an ORDINARY
			// outcome of the boot claim and not a condition to remediate --
			// the incumbent keeps serving, its advertisement is untouched, and
			// this process exits having written nothing.
			log.Info("daemon.cmd.claim", "another daemon holds the boot claim; exiting without disturbing it", dlog.Context{
				"addr_path":  layout.DaemonAddr(),
				"claim_wait": claimWait.String(),
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
		// A STALE ADVERTISEMENT IS RECORDED BEFORE IT IS REPLACED. This
		// daemon holds the boot claim, so any address standing here is a
		// predecessor's; one that does not answer is what the next client
		// would have dialled and timed out on, and the record is the only
		// thing that says the predecessor went without withdrawing.
		if stale, isStale := daemonaddr.StaleAdvertisement(layout.DaemonAddr()); isStale {
			log.Warn("daemon.cmd.claim", "a stale daemon.addr names an address nobody answers; overwriting it", dlog.Context{
				"stale_address": stale,
				"addr_path":     layout.DaemonAddr(),
			})
		}
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
	// EVERY OBSERVABLE EXIT WITHDRAWS THE ADVERTISEMENT. A daemon.addr left
	// behind names a listener nobody is serving, and the next client dials it
	// and times out: realtest 1 caught Emacs probing 127.0.0.1:58161 minutes
	// after the daemon that bound it had gone.
	//
	// withdrawal.finish is the CATCH-ALL withdrawal site, and it covers every
	// path this process can observe: a cancelled context, SIGINT or SIGTERM
	// (main's signal.NotifyContext cancels ctx, which ends the serve), `Serve`
	// returning on its own, and every boot error that returns before serving
	// was ever reached. SIGKILL is not observable by anything, which is what
	// the boot's own staleness check above exists for.
	//
	// A run that reached serving withdraws EARLIER, via withdrawal.begin at the
	// start of the shutdown sequence (passed to h.Serve below), so a forward
	// during shutdown finds no address. Withdraw is idempotent and
	// address/pid-guarded, so finish is a safe no-op on that path; addrWithdrawal
	// records that the early call already took the file down so the no-op is not
	// mistaken for a successor having taken over.
	withdrawal := &addrWithdrawal{claim: claim, log: log}
	defer withdrawal.finish()

	// THE REGISTRY REFUSES TEMPORARY FOLDERS (owner ruling, 2026-10-06), and
	// the guard is built from the environment ONCE, here: its test-run seam
	// (tempdirs.EnvTestRoot) is honored only under the vendor guard, so a
	// stray one on a live daemon refuses the boot rather than exempting.
	temporary, err := tempdirs.FromEnv(contracts, os.TempDir(), os.Getenv)
	if err != nil {
		log.Error("daemon.cmd.state", "the temporary-directory guard could not be built", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("claude-repld: build the temporary-directory guard: %w", err)
	}
	log.Debug("daemon.cmd.state", "the registry refuses directories inside these temporary roots", dlog.Context{
		"roots": temporary.Roots(), "test_root": os.Getenv(tempdirs.EnvTestRoot),
	})

	// A TEST RUN'S STATE DATABASE SKIPS SQLITE'S FORCED FLUSHES (owner ruling,
	// 2026-10-06): honored only under the vendor guard, so a live daemon handed
	// the flag refuses to boot rather than run without durability.
	unsynced, err := wsm.UnsyncedFromEnv(contracts, os.Getenv)
	if err != nil {
		log.Error("daemon.cmd.state", "the state database's durability could not be settled", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("claude-repld: settle the state database's durability: %w", err)
	}
	if unsynced {
		log.Info("daemon.cmd.state", "test run: the state database skips SQLite's forced flushes (synchronous=OFF)", dlog.Context{
			"path": layout.DB(), "env": wsm.EnvTestUnsyncedWrites,
		})
	}

	db, err := openState(ctx, layout, log, joining, temporary, unsynced)
	if err != nil {
		return err
	}
	// THE CLOSE RELEASES EVERY LEASE THIS PROCESS STILL OWNS (wsm Close), so
	// its failure is a lease left behind and is said at ERROR, never dropped.
	defer func() {
		if err := db.Close(); err != nil {
			log.Error("daemon.cmd.state", "the state client did not close cleanly; a lease this process held may be left behind", dlog.Context{
				"path":  layout.DB(),
				"error": err.Error(),
			})
		}
	}()

	// THE LOG SURFACES LEARN THE MINTED WORKSPACE IDS HERE, the moment the
	// roster is readable and before any workspace-owned record can be
	// written. Every record's `workspace_id` and every log target this
	// runtime mints is named by the daemon-minted ids.WorkspaceID, which is
	// the id the shim, the webapp and the store all carry; nothing derives it
	// from the directory.
	surfaces.BindWorkspaceIDs(workspaceIDLookup(ctx, db))

	// SERVING IS ITS OWN LIFETIME, cancelled either by the process's signal
	// context or by the daemon's own orderly exit — the drain's deadline and
	// the handover's last transfer both end the process through it.
	serving, stopServing := context.WithCancel(ctx)
	defer stopServing()
	// WHY IT ENDED is recorded by whoever ends it, as a typed reason: only a
	// state-root loss stops the shims on the way out (see standDownShims).
	var ended standDownRecord

	built, err := h.Graph(serving, process{
		Opts:      opts,
		Contracts: contracts,
		Layout:    layout,
		Surfaces:  surfaces,
		DB:        db,
		Claim:     claim,
		Instance:  wsm.NewInstanceID(),
		Exit: func(reason standDownReason) {
			ended.record(reason, nil)
			stopServing()
		},
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
	// THE BANNERS CLOSE AFTER THE WATCHERS and before the state client:
	// deferred here, BEFORE CloseWatchers is, so it runs after it.
	if built.CloseBanners != nil {
		defer built.CloseBanners()
	}
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
	report, err := reconcile(ctx, log, sequence, h.BootStall)
	if err != nil {
		return err
	}
	log.Info("daemon.cmd.boot", "the boot reconciliation completed", dlog.Context{
		"adopted":              len(report.Adopted),
		"orphans_closed":       len(report.Orphaned),
		"missing_dir_closed":   len(report.MissingDirClosed),
		"missing_dir_restored": len(report.MissingDirRestored),
		"repositories_retired": len(report.RetiredRepositories),
		"holds_restored":       report.HoldsRestored,
		"pending_bring_up":     len(report.PendingBringUp),
		"orphan_leases":        len(report.OrphanLeases),
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
	var loops runningLoops
	for _, loop := range built.Background {
		loops.Go(loop.Name, func() { loop.run(serving, log) })
	}
	// THE BRING-UP IS THE ONE BOOT STEP THAT DOES NOT GATE READINESS, and it
	// starts here: after the bindings and the opening views, because it starts
	// SESSIONS and a session that pushed into a surface that did not exist
	// would push into nothing — and before `h.Serve`, so the accept loop and
	// the shim spawns run at the same time instead of one after the other.
	//
	// IT IS WHY DaemonHealth ANSWERS. Emacs recognizes a replacement daemon by
	// a unary under a 3s bound, and on 2026-09-13 a bring-up INSIDE the
	// reconciliation put two shim spawns in front of that bound: the daemon
	// came up, started both sessions, and the deploy still reported
	// `replacement identity was not observed`. Nothing in this step is allowed
	// in front of the listener again.
	//
	// It joins with the background loops, so an orderly exit waits for a
	// session start in flight on the same bound they get rather than closing
	// the state client under it.
	loops.Go("bring_up", func() { sequence.BringUp(serving, report.PendingBringUp) })
	// AND THE STATE ROOT IS WATCHED FOR AS LONG AS IT IS SERVED. A loss ends
	// the serving lifetime, and the exit reports it as the cause rather than
	// as an orderly one. It joins with the loops above.
	loops.Go("state_root_watch", func() {
		_ = rootWatch{
			verify: claim.Verify,
			every:  h.StateCheckEvery,
			log:    log,
			standDown: func(cause error) {
				ended.record(standDownStateRootLost, cause)
				stopServing()
			},
		}.run(serving)
	})
	defer joinBackgroundLoops(&loops, loopJoinBound, log)
	// AND THE QUEUE'S OWN GOROUTINES, for the same reason and on the same
	// bound: a classification verdict and a background revival each read and
	// write the state client off their own goroutine.
	if built.DrainQueue != nil {
		defer joinQueueWork(built.DrainQueue, loopJoinBound, log)
	}
	// AND THE DETACHED SESSION STARTS, on the same bound and for the same
	// reason: the register's revival brings a session up off the answer's
	// goroutine, and one still in flight at the exit reads and writes the
	// state client. The drain ENDS them before it waits, so an exit landing
	// inside a start costs the shim call's cancellation rather than the whole
	// bound.
	if built.DrainStarts != nil {
		defer joinDetachedStarts(built.DrainStarts, loopJoinBound, log)
	}

	log.Info("daemon.cmd.serve", "serving", dlog.Context{
		"address": claim.Address(),
		"joining": joining,
	})
	// THE ACCEPT LOOP IS WRAPPED, and its errors are the daemon's own. A
	// listener that stops accepting while this process keeps running is a
	// daemon that is listening and dead — the state Emacs cannot tell from a
	// healthy one, because daemon.addr still names a bound port — so an accept
	// error is reported at ERROR, a transient one is retried with backoff, and
	// anything else ends the serve and the process with it.
	// withdrawal.begin takes daemon.addr down at the START of the shutdown
	// sequence, before the listener stops accepting, so a client forwarding
	// during shutdown finds no address rather than a present address backed by
	// a listener that no longer accepts. The deferred finish above is the
	// safety net for boot errors and paths that never reached serving.
	endStreams := func() {
		if err := srv.Close(); err != nil {
			log.Error("daemon.cmd.serve", "the surface could not end its standing streams", dlog.Context{
				"error": err.Error(),
			})
		}
	}
	if err := h.Serve(serving, server.RetryAccept(claim.Listener(), log), server.H2C(srv, log), withdrawal.begin, endStreams); err != nil {
		log.Error("daemon.cmd.serve", "the daemon stopped serving its listener", dlog.Context{
			"address": claim.Address(),
			"error":   err.Error(),
		})
		return err
	}
	reason, cause := ended.get()
	standDownShims(ctx, reason, built.StopShims, log)
	if cause != nil {
		return fmt.Errorf("claude-repld: stood down: %w", cause)
	}
	return nil
}

// bootStallBound is the LAST RESORT on the whole boot reconciliation.
//
// The listener is bound and daemon.addr published before the reconciliation
// runs and http.Server.Serve is not reached until after it, so a step that
// blocks forever is a daemon that listens and accepts nothing: pid 31984 held
// its accept queue at 128/128 for ten hours while Emacs and curl timed out on
// connect. Every step the sequence runs is itself bounded — boot.DefaultAdoptBound
// covers the one that produced that wedge — so this covers a step whose bound
// somebody forgot, and its expiry is a dumped stack rather than a guess.
//
// Sized above the worst LEGITIMATE boot: every surviving workspace adopted
// CONCURRENTLY under one boot.DefaultAdoptBound (10s), plus the manifest, the
// holds, the orphan closes and the merge recovery, all of which are local
// sqlite work measured in milliseconds. 90s is that with room over it, and it
// is never paid on a healthy boot.
const bootStallBound = 90 * time.Second

// reconcile runs the boot reconciliation under a watchdog, so a step that never
// returns ends the PROCESS instead of leaving it listening on a socket nothing
// accepts on. Emacs respawns a daemon that exited; it cannot tell a wedged one
// from a healthy one.
//
// The dump is what makes the next one diagnosable: the blocked goroutine's own
// stack names the call, which on this host no debugger can recover (dlv and
// lldb both hang on task_for_pid).
func reconcile(ctx context.Context, log dlog.Logger, sequence boot.Sequence, bound time.Duration) (boot.Report, error) {
	if bound <= 0 {
		bound = bootStallBound
	}
	type outcome struct {
		report boot.Report
		err    error
	}
	finished := make(chan outcome, 1)
	go func() {
		report, err := sequence.Run(ctx)
		finished <- outcome{report: report, err: err}
	}()
	watchdog := time.NewTimer(bound)
	defer watchdog.Stop()
	select {
	case done := <-finished:
		return done.report, done.err
	case <-watchdog.C:
		recordGoroutineDump(log, "daemon.cmd.boot",
			"the boot reconciliation did not finish within its bound; exiting rather than listening on a socket nothing accepts",
			dlog.Context{"bound_ms": bound.Milliseconds()})
		return boot.Report{}, fmt.Errorf("claude-repld: the boot reconciliation did not finish within %s", bound)
	}
}

// openState opens the state client. A JOINING daemon opens it READ-ONLY: until
// it has adopted a workspace the incumbent is still the sole writer, and two
// writers on one SQLite file is the lost-update class the single-handle rule
// exists to prevent. Its one write is carrying an older file forward by
// ADDITIVE steps, which leave the incumbent's statements working
// (wsm.OpenJoining).
//
// unsynced is the test run's seam (wsm.UnsyncedFromEnv), already proved by the
// boot; a live daemon always passes false.
func openState(ctx context.Context, layout stateroot.Layout, log dlog.Logger, joining bool, temporary tempdirs.Guard, unsynced bool) (wsm.DB, error) {
	open := wsm.Open
	if joining {
		open = wsm.OpenJoining
	}
	opts := []wsm.Option{wsm.WithLogger(log), wsm.WithTemporaryGuard(temporary)}
	if unsynced {
		opts = append(opts, wsm.WithUnsyncedWrites())
	}
	db, err := open(ctx, layout.DB(), opts...)
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

// workspaceIDLookup answers a workspace directory's daemon-minted id out of
// the roster. A directory the roster does not know is an ERROR, never a
// path-derived stand-in: the caller that asked for the sink reports it, and
// the record is refused rather than attributed to an id no other runtime
// would ever state.
func workspaceIDLookup(ctx context.Context, db wsm.DB) dlog.WorkspaceIDLookup {
	return func(dir string) (string, error) {
		record, err := db.WorkspaceByDir(ctx, dir)
		if err != nil {
			return "", fmt.Errorf("look up the workspace registered at %q: %w", dir, err)
		}
		return string(record.ID), nil
	}
}

// serve runs the http server on the claimed listener until ctx ends, then shuts
// it down gracefully.
func serve(ctx context.Context, l net.Listener, h http.Handler, onShuttingDown, endStreams func()) error {
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
		// THE ADVERTISEMENT COMES DOWN BEFORE THE LISTENER STOPS ACCEPTING.
		// A daemon that is shutting down is "not there" for clients, so its
		// address is withdrawn the instant shutdown begins — while srv.Serve
		// is still accepting — rather than after Shutdown has stopped the
		// listener. Withdrawing here closes the shutdown transient that
		// realtest 1 caught: a present daemon.addr backed by a listener that
		// no longer accepts, which the sidecar forwarded into and logged as a
		// real connection-refused WARN. With the address gone first, a forward
		// during shutdown finds no address and is the vanished-address
		// transient the sidecar already treats as gone.
		if onShuttingDown != nil {
			onShuttingDown()
		}
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
		// EVERY STANDING STREAM ENDS WITH ITS END FRAME, BEFORE THE PROCESS
		// CAN EXIT. `Shutdown` below cannot do it: it neither closes nor waits
		// for a hijacked h2c connection, and the surface's own Close used to
		// run only as a deferred call the process exit raced -- so every client
		// still watching (a workspace no transfer notice reached, the roster)
		// read "producer closed without an end frame". The streams are ended
		// here, after the last pushes have left, and the exit waits for each
		// one's end frame to reach the socket. This holds on EVERY exit path:
		// a handover's last transfer, a restart's stand-down, a drain, a
		// signal, a state-root loss.
		if endStreams != nil {
			gate.EndStreams(endStreams)
			gate.AwaitStreamsEnded(streamsEndBound)
		}
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

// withdrawer is the slice of daemonaddr.Claim that addrWithdrawal needs: the
// idempotent, address/pid-guarded removal and the address it names.
type withdrawer interface {
	Withdraw() (bool, error)
	Address() string
}

// addrWithdrawal takes daemon.addr down and keeps the exit's records honest.
//
// A daemon that is shutting down is "not there" for clients, so begin removes
// the advertisement at the START of shutdown — before the listener stops
// accepting — closing the shutdown transient realtest 1 caught: a present
// daemon.addr backed by a listener that no longer accepts, which the sidecar
// forwarded into and logged as a real connection-refused WARN.
//
// finish is the deferred catch-all covering boot errors and paths that never
// reached serving. Withdraw is idempotent, so finish is a safe no-op after
// begin; the early flag keeps that no-op from being recorded as a successor
// takeover. A "withdrew" record fires only when a call actually removed the
// file, and a Withdraw error is always surfaced at ERROR.
type addrWithdrawal struct {
	claim withdrawer
	log   dlog.Logger
	early bool
}

// begin withdraws the advertisement at the start of the shutdown sequence.
func (w *addrWithdrawal) begin() {
	withdrawn, err := w.claim.Withdraw()
	if err != nil {
		w.log.Error("daemon.cmd.exit", "daemon.addr could not be withdrawn", dlog.Context{
			"error": err.Error(),
		})
		return
	}
	if withdrawn {
		w.early = true
		w.log.Info("daemon.cmd.exit", "daemon.addr was withdrawn at the start of shutdown, before the listener stopped accepting", dlog.Context{
			"address": w.claim.Address(),
		})
	}
}

// finish is the deferred catch-all withdrawal, run on every exit.
func (w *addrWithdrawal) finish() {
	withdrawn, err := w.claim.Withdraw()
	if err != nil {
		w.log.Error("daemon.cmd.exit", "daemon.addr could not be withdrawn", dlog.Context{
			"error": err.Error(),
		})
		return
	}
	if withdrawn {
		w.log.Info("daemon.cmd.exit", "daemon.addr was withdrawn", dlog.Context{
			"address": w.claim.Address(),
		})
		return
	}
	if w.early {
		// begin already took the file down at the start of shutdown; this
		// catch-all had nothing left to do.
		w.log.Debug("daemon.cmd.exit", "daemon.addr was already withdrawn at the start of shutdown", dlog.Context{
			"address": w.claim.Address(),
		})
		return
	}
	// The file names somebody else -- a successor that took over and published
	// its own address -- or nothing at all.
	w.log.Info("daemon.cmd.exit", "daemon.addr was left alone; it does not name this daemon", dlog.Context{
		"address": w.claim.Address(),
	})
}

// streamsEndBound is how long the exit gives the standing streams, once ended,
// to have returned and written their end frames.
//
// IT NESTS server's per-stream answerWriteBound (250ms, one end frame's write
// after its handler returns) with the same again for the handlers to leave:
// each one selects on the lifetime Close cancels, so it returns within one
// scheduling of the goroutine. It is a last resort well under shutdownGrace;
// an overrun is ERROR naming how many streams were left open.
const streamsEndBound = 500 * time.Millisecond

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

// joinDetachedStarts ends the session starts that run off a caller's goroutine
// and waits, bounded, for them to leave, saying so loudly when one does not.
func joinDetachedStarts(drain func(time.Duration) bool, bound time.Duration, log dlog.Logger) {
	if drain(bound) {
		log.Debug("daemon.cmd.serve", "every detached session start left before the teardown", nil)
		return
	}
	log.Error("daemon.cmd.serve", "a detached session start outlived its serving context; tearing down under it", dlog.Context{
		"bound_ms": bound.Milliseconds(),
	})
}

// runningLoops is the set of background loops the serving lifetime started,
// with the names of those still running, so an exit that outlives
// loopJoinBound names the loop it is tearing down under instead of only
// counting it.
type runningLoops struct {
	wg      sync.WaitGroup
	mu      sync.Mutex
	running map[string]int
}

// Go runs one named loop on its own goroutine and joins it with the rest.
func (r *runningLoops) Go(name string, run func()) {
	r.mu.Lock()
	if r.running == nil {
		r.running = map[string]int{}
	}
	r.running[name]++
	r.mu.Unlock()
	r.wg.Add(1)
	go func() {
		defer r.wg.Done()
		defer r.left(name)
		run()
	}()
}

func (r *runningLoops) left(name string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.running[name]--
	if r.running[name] == 0 {
		delete(r.running, name)
	}
}

// outstanding names the loops still running, sorted.
func (r *runningLoops) outstanding() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	names := make([]string, 0, len(r.running))
	for name := range r.running {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

// joinBackgroundLoops waits, bounded, for every background loop to leave, and
// says so loudly when one does not: the record names the loops still running
// and carries every goroutine's stack, which is what names the call each one
// is blocked in.
func joinBackgroundLoops(loops *runningLoops, bound time.Duration, log dlog.Logger) {
	left := make(chan struct{})
	go func() {
		loops.wg.Wait()
		close(left)
	}()
	select {
	case <-left:
		log.Debug("daemon.cmd.serve", "every background loop left before the teardown", nil)
	case <-time.After(bound):
		recordGoroutineDump(log, "daemon.cmd.serve", "a background loop outlived its serving context; tearing down under it", dlog.Context{
			"bound_ms": bound.Milliseconds(),
			"loops":    loops.outstanding(),
		})
	}
}
