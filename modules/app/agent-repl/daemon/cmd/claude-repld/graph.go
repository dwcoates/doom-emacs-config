package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/agentreplsession"
	"claude-repld/internal/boot"
	"claude-repld/internal/bringup"
	"claude-repld/internal/buildid"
	"claude-repld/internal/checkout"
	"claude-repld/internal/classifier"
	"claude-repld/internal/clock"
	"claude-repld/internal/commandfile"
	"claude-repld/internal/desktopnotify"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/editorinstance"
	"claude-repld/internal/envc"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/handover"
	"claude-repld/internal/headless"
	"claude-repld/internal/health"
	"claude-repld/internal/heldingress"
	"claude-repld/internal/ids"
	"claude-repld/internal/imageorigin"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/login"
	"claude-repld/internal/merge"
	"claude-repld/internal/paint"
	"claude-repld/internal/persistentwifi"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/scriptrunner"
	"claude-repld/internal/server"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/startup"
	"claude-repld/internal/titlesynth"
	"claude-repld/internal/vendortraffic"
	"claude-repld/internal/vocab"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// THE COMPOSITION ROOT.
//
// buildGraph constructs every component once, in dependency order, and wires
// them to each other. IT HOLDS NO POLICY: everything that looks like a
// decision here is either a path the operator can override or one of the
// forwarders in forward.go, which exist solely because two edges of the graph
// point at a surface that cannot be built until its own dependencies are.
//
// The order is forced, and each step is what the next one needs:
//
//  1. the leaves — paths, the render vocabulary, the git client, the account
//     roster, the classifier, the browser, the shim supervisor;
//  2. the five view resolvers the session fleet routes frames into;
//  3. the session fleet, which owns every shim client and answers freeness,
//     occupancy, the served cold gate and the shim build a session reported;
//  4. the prompt queue, which delivers through the fleet;
//  5. the rollout and drain controllers, whose server halves are forwarders;
//  6. the merge orchestrator, the workspace verbs, the health reporter, the
//     login manager, the prompt handler, the command-file ingress and the
//     held-prompt ingress;
//  7. the boot sequence's dependencies and the server's, returned together
//     with the late bindings and the background loops.
//
// NOTHING HERE IMPROVISES A STAND-IN. A dependency with no producer would be
// declared in `unwired` and would fail the boot LOUDLY, naming it.

// graphOperation is the operation this file's own records carry.
const graphOperation = "daemon.cmd.graph"

// opHeadlessBinary is the boot record naming the vendor binary every headless
// call — the classifier's routing question and the workspace naming call —
// will exec.
const opHeadlessBinary = "daemon.headless.binary"

// unwired names every Deps field this composition root cannot supply. It is
// EMPTY: every collaborator the wave-3 graph needs has a landed producer. The
// list stays so that a future dependency with no producer is declared rather
// than filled in here — a stand-in written at the composition root would be a
// second, undocumented implementation of a seam that belongs to the component
// it serves.
var unwired []string

// graph is everything buildGraph produced. The two argument sets are what boot
// and the server are built from; the two hooks are what a surface that does
// not exist yet is completed with.
type graph struct {
	// Server is server.New's argument.
	Server server.Deps
	// Boot is boot.New's argument.
	Boot boot.Deps
	// Bind completes the late bindings once the surface exists. It is called
	// immediately after server.New and before anything is served.
	Bind func(srv server.Server)
	// Prime publishes what a client must be able to receive before it has
	// asked for anything — today the editor-global roster. The boot spine
	// calls it after the bindings and before anything is served, and its
	// failure is a BOOT FATAL: a surface that cannot state its opening truth
	// is not serving.
	Prime func(ctx context.Context) error
	// Background are the long-running loops the daemon owns. They start after
	// the bindings, because each of them can push.
	Background []backgroundLoop
	// CloseWatchers ends every session watcher and joins its in-flight sink
	// work. `run` calls it BEFORE closing the state client: a watcher's sinks
	// read that client, and a turn end still being handled when the store
	// closes under it is a refused read on a path that owes no error.
	CloseWatchers func()
	// CloseBanners cancels every desktop banner still awaiting its click and
	// joins them. `run` calls it AFTER the watchers close (no turn end can
	// raise another banner then) and BEFORE the state client closes: a banner
	// reads that client for the workspace's name.
	CloseBanners func()
	// DrainQueue is the BOUNDED wait for the prompt queue's own background
	// goroutines — the asynchronous classification verdicts and the background
	// revivals. `run` calls it BEFORE the state client closes, for the reason
	// promptqueue.Queue.Drain states: both read and write that client off their
	// own goroutine.
	DrainQueue func(bound time.Duration) bool
	// DrainStarts is the BOUNDED wait for the session starts that run off a
	// caller's goroutine -- the register's revival of an announced
	// workspace's conversation. It ENDS them first and then joins them, and
	// `run` calls it BEFORE the state client closes, for the same reason
	// DrainQueue is called there: a start reads and writes that client from
	// its own goroutine.
	DrainStarts func(bound time.Duration) bool
	// DrainMerges is the BOUNDED wait for merge runs that have reached their
	// terminal. `run` calls it BEFORE the watchers close and before the state
	// client does: a SIGTERM landing mid-terminal used to close the store
	// under the landing's own stamps, losing merged_at, closed and the lease
	// release to failed transactions. See merge.TerminalDrainBound.
	DrainMerges func(ctx context.Context)
	// StopShims force-stops every shim this daemon supervises and answers how
	// many it stopped. `run` calls it ONLY on a state-root-loss stand-down
	// (standDownShims): an ordinary exit or a handover leaves them for a
	// successor to adopt, and with the root gone nothing ever can.
	StopShims func(ctx context.Context) (int, error)
}

// backgroundLoop is one long-running loop the daemon runs for its whole
// serving lifetime.
type backgroundLoop struct {
	// Name is what a record calls it.
	Name string
	// Run drives it until ctx ends.
	Run func(ctx context.Context) error
}

// run drives one loop and records how it ended. A loop that ends with the
// serving lifetime ended normally; any other end is the daemon losing a
// faculty, which is said out loud rather than passing silently.
func (l backgroundLoop) run(ctx context.Context, log dlog.Logger) {
	err := l.Run(ctx)
	switch {
	case err == nil, ctx.Err() != nil:
		log.Debug(graphOperation, "a background loop ended with the serving lifetime", dlog.Context{
			"loop": l.Name,
		})
	default:
		log.Error(graphOperation, "a background loop ended before the daemon did", dlog.Context{
			"loop": l.Name, "cause": err.Error(),
		})
	}
}

// buildGraph builds the component graph from the resolved process facts.
func buildGraph(ctx context.Context, p process) (*graph, error) {
	log := p.Surfaces.Global()
	if len(unwired) > 0 {
		return nil, fmt.Errorf(
			"claude-repld: the component graph has %d collaborators with no landed source, and cmd substitutes none:\n  - %s",
			len(unwired), strings.Join(unwired, "\n  - "))
	}

	// THE ADOPTION BOUND IS A TEST KNOB, not a product window: the suite
	// proves that an UNREACHABLE survivor does not wedge the boot, and waiting
	// out the production last resort to observe it would cost every such test
	// ten seconds. A malformed or non-positive value is REFUSED rather than
	// ignored, so a run that set it cannot lie about what bound it measured.
	adoptBound, err := resolveAdoptBound(os.Getenv(envBootAdoptBound))
	if err != nil {
		log.Error(graphOperation, "the boot adoption bound was refused", dlog.Context{
			"cause": err.Error(),
		})
		return nil, err
	}
	// THE START BOUND, on the same contract and for the same reason: the
	// integration suite's subject includes a shim that accepts a start and
	// never answers, and waiting out the production window to observe it would
	// cost that test a minute.
	startBound, err := resolveStartBound(os.Getenv(envStartSessionBound))
	if err != nil {
		log.Error(graphOperation, "the session start bound was refused", dlog.Context{
			"cause": err.Error(),
		})
		return nil, err
	}

	paths, err := resolvePaths(p.Opts)
	if err != nil {
		log.Error(graphOperation, "the checkout's paths could not be resolved", dlog.Context{
			"cause": err.Error(),
		})
		return nil, err
	}
	log.Debug(graphOperation, "resolved the checkout the binary was deployed from", dlog.Context{
		"checkout": paths.Checkout, "shim_main": paths.ShimMain,
		"webapp_dist": paths.WebappDist, "prompts_dir": paths.PromptsDir,
	})

	colors, err := vocab.LoadRenderColors(paths.VocabDir)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: load the render colors: %w", err)
	}
	classes, err := vocab.LoadPaintClasses(paths.VocabDir)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: load the paint classes: %w", err)
	}
	painter, err := paint.New(classes)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the painter: %w", err)
	}

	git, err := gitclient.New(p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the git client: %w", err)
	}
	accounts, err := account.New(account.Roots{
		Default:       p.Opts.defaultConfigDir,
		MultiRepo:     p.Opts.multiRepoConfigDir,
		MultiRepoRoot: os.Getenv(envMultiRepoRoot),
	}, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the account resolver: %w", err)
	}

	guard := envc.NewVendorGuard(p.Contracts)
	// ONE RESOLVED VENDOR BINARY FOR EVERY HEADLESS CALL. It is recorded at
	// boot because the alternative was invisible: buildJudge used to hand the
	// classifier an empty binary, so every classification refused before it
	// reached the model and nothing said so.
	headlessClient := headless.New(guard, "")
	log.Info(opHeadlessBinary, "resolved the vendor binary for the daemon's headless calls", dlog.Context{
		"bin":    headlessClient.Bin(),
		"source": headless.BinSource(""),
	})
	judge, err := buildJudge(p.Contracts, guard, headlessClient, paths.PromptsDir)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the classifier: %w", err)
	}
	// THE BROWSER IS OPTIONAL, and its absence is a STATE THE CONTRACT SPELLS.
	// A daemon with `--no-browser`, or one on a host with neither
	// $AGENT_REPL_BROWSER_CMD nor the pinned default binary, has no launcher
	// at all: the dependency is left nil and OpenExternal answers
	// no_browser_configured rather than reporting a launch that never had
	// anything to run.
	var browser externalbrowser.Opener
	switch {
	case p.Opts.noBrowser:
		log.Warn(graphOperation, "no external browser is configured for this daemon", dlog.Context{
			"reason": "--no-browser",
		})
	case os.Getenv(externalbrowser.EnvBrowserCmd) == "" && !externalbrowser.DefaultLauncherConfigured():
		log.Warn(graphOperation, "no external browser is configured for this daemon", dlog.Context{
			"reason":  "neither $" + externalbrowser.EnvBrowserCmd + " nor the pinned default launcher is present",
			"default": externalbrowser.DefaultBinary,
		})
	default:
		browser, err = externalbrowser.New(externalbrowser.Config{Logger: log})
		if err != nil {
			return nil, fmt.Errorf("claude-repld: build the external browser: %w", err)
		}
	}
	// THE ADOPTED-DEATH WITNESS. Without it an adopted shim whose socket is
	// gone is redialed forever: no exit is ever published, so the workspace
	// stays wedged on a process that is not there. The witness reads the
	// workspace's kernel lock through the session fleet — the same probe
	// rollout is handed below — and only a lock that reads FREE is evidence of
	// death. A probe that could NOT TELL surfaces its error, and the client
	// keeps redialing, per the boot rule that a could-not-tell probe is never
	// read as free.
	//
	// The closure is late-bound over `fleet` because the fleet is built BELOW:
	// it needs this supervisor. By the time a shim's link can break, the fleet
	// exists; before then the witness refuses rather than concluding anything.
	var fleet *workspace.Fleet
	// The editor startup's bring-up reads ownership at run time; it is built
	// with the merge orchestrator, below the startup.
	var ownership workspace.Ownership
	// THE SERVICES' LATCH: every spawn waits until the boot's service step
	// has made the store and the sidecar current (see serviceGate).
	services := newServiceGate()
	supervisor, err := shimclient.NewSupervisor(p.Surfaces,
		shimclient.WithServicesReady(services.Ready()),
		shimclient.WithLockProbe(adoptedDeathWitness(func(workspaceDir string) (sessionlock.State, error) {
			if fleet == nil {
				return sessionlock.StateUnknown, errors.New("the session fleet is not built yet")
			}
			return fleet.ProbeLock(workspaceDir)
		})))
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the shim supervisor: %w", err)
	}

	// ---- the five view resolvers ----

	workspaceDir := func(ws ids.WorkspaceID) (string, error) {
		record, err := p.DB.Workspace(ctx, ws)
		if err != nil {
			return "", err
		}
		return record.Dir, nil
	}
	// A FORK'S PORTED CONVERSATION is the daemon's own record of the parent's
	// questions, carried over under the child's turn ids at the fork. The feed
	// draws it above everything the workspace has of its own, which is what
	// makes a forked feed the parent's conversation followed by the fork's.
	portedPrompts := func(ctx context.Context, ws ids.WorkspaceID) ([]feed.PortedPrompt, error) {
		rows, err := p.DB.PortedPrompts(ctx, ws)
		if err != nil {
			return nil, err
		}
		out := make([]feed.PortedPrompt, 0, len(rows))
		for _, row := range rows {
			out = append(out, feed.PortedPrompt{
				Turn:   string(row.Turn),
				Text:   row.Text,
				Origin: conversationv1.PromptOrigin(conversationv1.PromptOrigin_value[row.Origin]),
			})
		}
		return out, nil
	}
	// The metaprompt sentinels are stripped from DRAWN text only; the record
	// keeps the full text. Both the feed resolver and the queue's mirror draw
	// prompt rows, so both take the same one implementation.
	stripSentinels := sentinelStripper(log)
	// THE IMAGE ORIGIN AND THE RESOLVER ARE ONE WIRING. A path becomes
	// servable only by the feed resolver drawing a record that referenced it,
	// so the registrar the resolver holds IS the origin's own Register: there
	// is no way to serve an image that no conversation carried.
	images, err := imageorigin.New(log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the image origin: %w", err)
	}
	resolveImage, err := feed.PathImageResolver(images.Register, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the image resolver: %w", err)
	}
	// Zero leaves the resolver's own DefaultMomentaryDwell in force; the flag
	// and its environment knob are what let a caller compress a window whose
	// whole purpose is to be long enough for a person to read.
	// THE VENDOR SERVING AGAIN RELEASES THE PROMPTS a mid-session vendor block
	// held after reconnect (promptqueue/vendorblock.go). The footer is the one
	// accumulation that edge is read from, and the queue it tells is built
	// later, so the edge rides a forwarder bound once the queue exists.
	vendorServes := &vendorServesForwarder{log: log}
	footerOpts := []footer.Option{footer.WithVendorServes(vendorServes.VendorServes)}
	if p.Opts.footerMomentaryDwell > 0 {
		footerOpts = append(footerOpts, footer.WithMomentaryDwell(p.Opts.footerMomentaryDwell))
	}
	footerResolver, err := footer.New(colors, p.Surfaces, footerOpts...)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the footer resolver: %w", err)
	}
	// THE DAEMON'S WORKSPACE WARNINGS AND ERRORS REACH THE STRIP through ONE
	// tee at dlog's workspace-logger emit point, never a hook beside a call
	// site: every Warn and Error a workspace logger writes is handed to the
	// footer, which draws it as a transient line (and skips its own records).
	p.Surfaces.BindRecordTee(footerResolver)
	// THE TOPBAR IS BUILT BEFORE THE FEED, which raises onto its warning chip
	// every row it cannot place, and before the fault hook, which raises the
	// faults the topbar carries (a failed deploy) onto every strip.
	topbarResolver, err := topbar.New(colors, p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the topbar resolver: %w", err)
	}

	// EVERY FAULT REACHES THE FOOTER (the owner's ruling of 2026-09-13). The
	// footer learns about faults from the ONE place faults are written —
	// the state client every raise site shares — rather than from plumbing
	// beside each raise, which is how three fault kinds came to have a footer
	// path and sixteen did not. Every collaborator built below takes the
	// decorated client, so a fault opened anywhere lands on the strip. The
	// faults the topbar carries (health.FaultTopbarLine) reach its warning
	// strip through the same hook (owner ruling, 2026-09-28).
	// The same faults reach every Emacs as the standing loud faults on its
	// WatchDaemon stream (owner request, 2026-09-28): the set is built here,
	// before the hook and the server, so no open can precede it.
	loudFaults := health.NewLoudFaults(log)
	// THE ROSTER IS BUILT BEFORE THE HOOK because it is one of the hook's
	// surfaces: the network fault reaches it through the same door as the
	// footer. It raises no fault itself, so it needs no decorated client.
	// THE ROSTER'S LAST TURN RESULT IS DURABLE: a daemon that did not see a
	// workspace's turn end draws its row as it stood, not `ready`.
	sidebarResolver, err := sidebar.New(colors, p.Surfaces,
		sidebar.WithResultSink(rosterResults(p.DB, p.Surfaces.Global())))
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the sidebar resolver: %w", err)
	}
	p.DB = health.ObserveFaults(p.DB, newFaultSurfaces(footerResolver, sidebarResolver, topbarResolver, loudFaults), p.Surfaces)

	// THE LOCK STALL WATCHDOG is built before every component whose hot lock
	// it watches (the feed, the session watchers, the prompt queue), and it
	// runs as a background loop. A wedged lock is otherwise invisible until
	// Emacs's unary timeout, and its evidence dies with the SIGQUIT that finds
	// it (daemon/AGENTS.md "The lock stall watchdog").
	stalls, err := lockwatch.New(lockwatch.Deps{Log: log})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the lock stall watchdog: %w", err)
	}

	// THE FEED RESOLVER IS BUILT AFTER THE DECORATION, and that ordering is the
	// wiring. It raises the `final_answer_unresolved` fault when a turn concludes
	// with no green answer standing, and it must raise it into the SAME state
	// client every other site raises into — the decorated one — or the fault
	// would be recorded and reach no footer.
	history := &historyForwarder{}
	feedResolver, err := feed.New(feed.Deps{
		Log: p.Surfaces,
		// HISTORY IS READ ONLY ON A READER'S REQUEST, from the workspace's
		// shim, through the fleet built below (forward.go).
		History:        history,
		Stalls:         stalls,
		WorkspaceDir:   workspaceDir,
		Painter:        painter,
		StripSentinels: stripSentinels,
		// An image reference is turned into a source on the daemon's own
		// image origin (`path`) or answered verbatim (`url`); an unset arm
		// is refused loudly rather than drawn as an empty src.
		ResolveImage:  resolveImage,
		PortedPrompts: portedPrompts,
		// A REPLAYED TURN WHOSE PAGE CARRIES NO TERMINAL is ended from its
		// durable close, the record the prompt queue's door wrote.
		TurnCloses:    p.DB.TurnCloses,
		TurnAddresses: p.DB.TurnAddresses,
		// A FORK'S OWN TURNS are the ones its workspace recorded; every other
		// main-agent entry of its book is the conversation it inherited.
		OwnedTurns: p.DB.RecordedTurns,
		// A MERGE'S BUBBLE is drawn again by a new daemon from this record.
		DurableRows: p.DB,
		Faults:      p.DB,
		// A ROLLED-BACK TURN STAYS ROLLED BACK across a restart.
		RolledBack: p.DB,
		// A row the feed cannot place is drawn nowhere and raised on the
		// topbar's warning chip, the webapp's one error surface.
		Warnings: topbarResolver,
		// Zero leaves the resolver's own DefaultTailRetention in force; the
		// flag and its environment knob are what make token_expired reachable.
		TailRetention: p.Opts.feedTailRetention,
		// THE FOOTER'S JUMP ROWS NAME THE ENTRY THE FEED DREW, on the feed that
		// draws it: a subagent of a subagent lands on its parent's sub-feed,
		// and only the feed knows that. Called under the feed's lock; the
		// footer never calls back into the feed (the fault path above already
		// takes the same feed-then-footer order).
		EntryPlaced: footerResolver.OnEntryPlaced,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the feed resolver: %w", err)
	}

	holdsResolver, err := holds.New(p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the hold tray: %w", err)
	}

	// ---- the forwarders, and the fleet they let exist first ----

	pushes := &serverForwarder{}
	relay := &relayForwarder{}
	verbsRef := &verbsForwarder{}

	var queue promptqueue.Queue
	healthRef := &healthForwarder{}
	rolloutRef := &rolloutForwarder{}
	lifecycle := &lifecycleSink{verbs: verbsRef, relay: relay, health: healthRef, builds: rolloutRef, sessions: p.DB, log: log}

	// THE INSTALLED SHIM BUNDLE, guarded: a spawn holds it from the hash it
	// stamps the shim with until the shim has answered, and a deploy replaces
	// it only while no spawn holds it. SHIM_BUILD_SHA answers only while no
	// bundle is installed (every harness runs a fake shim with no bundle).
	shimBundle := buildid.NewShimBundle(paths.ShimMain, os.Getenv(buildid.EnvShimBuild))

	// The title synthesizer rides the fleet's OWN session-watch sinks, so it is
	// built before the fleet and its digest call reaches back through a
	// late-bound forwarder (like verbsRef/healthRef). It installs the daemon's
	// synthesized title into the topbar, and its cheap headless call bills the
	// workspace's own account via the daemon's one workspace-dir lookup.
	digestRef := &digestForwarder{}
	// ONE ACCOUNT RESOLUTION FOR THE DAEMON'S OWN HEADLESS CALLS: the title
	// and the turn summary bill the workspace's own account the same way.
	// THE LOOKUP RUNS ON THE CALLER'S CONTEXT, not the graph's: a banner or a
	// title composed while the daemon stands down sees its own call's end, and
	// can tell that stand-down apart from a workspace with no record.
	configDirs := titleConfigDirs{workspaceDir: func(ctx context.Context, ws ids.WorkspaceID) (string, error) {
		record, err := p.DB.Workspace(ctx, ws)
		if err != nil {
			return "", err
		}
		return record.Dir, nil
	}, accounts: accounts}
	titleSynth := titlesynth.New(titlesynth.Deps{
		Digester:   digestRef,
		Headless:   headlessClient,
		ConfigDirs: configDirs,
		Titles:     topbarResolver,
		PromptsDir: paths.PromptsDir,
		Log:        log,
	})

	// THE EDITOR'S STARTUP (internal/startup) is told every bring-up step the
	// fleet takes, so it is built first; it reaches the fleet only once the
	// daemon serves, by which time the fleet below exists.
	startupRuns, err := startup.New(startup.Deps{
		Order: func() []sidebar.TabEntry {
			roster, _ := sidebarResolver.Topic().Latest()
			return sidebar.TabOrder(roster)
		},
		Live: func(ws ids.WorkspaceID) bool { return fleet.Live(ws) },
		BringUp: func(pending []ids.WorkspaceID, done func(ids.WorkspaceID, error)) {
			editorBringUp(fleet, p.DB, ownership, sidebarResolver.SetBringingUp, log, pending, done)
		},
		Now: time.Now,
		Log: log,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the editor startup: %w", err)
	}

	fleet, err = workspace.NewFleet(workspace.FleetDeps{
		PublishHost: relay.PublishHostWorkspace,
		DB:          p.DB,
		Instance:    p.Instance,
		Accounts:    accounts,
		Supervisor:  supervisor,
		Sinks: sessionwatcher.Sinks{
			Feed:      feedResolver,
			Footer:    footerResolver,
			Topbar:    topbarResolver,
			Sidebar:   sidebarResolver,
			Holds:     holdsResolver,
			Lifecycle: lifecycle,
			Title:     titleSynth,
			Stalls:    stalls,
		},
		Feed:         feedResolver,
		Footer:       footerResolver,
		Topbar:       topbarResolver,
		BringUps:     sidebarResolver.SetBringingUp,
		VendorStarts: sidebarResolver.SetVendorStart,
		Steps:        startupRuns.Step,
		SessionsUp:   lifecycle.SessionUp,
		SocketPath:   func(ws ids.WorkspaceID) string { return p.Layout.ShimSocket(string(ws)) },
		StoreSocket:  p.Opts.storeSocket,
		NodeBin:      p.Opts.node,
		MainJS:       paths.ShimMain,
		ShimBundle:   shimBundle,
		LockDir:      paths.RunDir,
		Fake:         p.Contracts.Fake() || fakeShims(),
		ForbidVendor: p.Contracts.ForbidVendorCalls(),
		StartBound:   startBound,
		Log:          p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the session fleet: %w", err)
	}
	history.bind(fleet)
	// The synthesizer's digest call reaches the shim through the fleet, now that
	// it exists.
	digestRef.bind(fleet)

	// ---- the desktop notifier ----

	// THE DAEMON POSTS EVERY DESKTOP BANNER. Emacs's focus rides its
	// WatchDaemon stream (server.Deps.Focus) and decides each one; a click is
	// relayed to the host stream through the server, bound late.
	focus := desktopnotify.NewFocus(log)
	backend, backendErr := resolveBannerBackend(log)
	clicks := &clickForwarder{}
	notifier := desktopnotify.New(desktopnotify.Deps{
		Focus: focus, Backend: backend, BackendErr: backendErr,
		Clicks: clicks, Names: workspaceNames{db: p.DB}, Log: log,
	})
	turnBanners := desktopnotify.NewTurnBanners(desktopnotify.TurnDeps{
		Endings: feedResolver,
		Poster:  notifier,
		Summaries: desktopnotify.Summarizer{
			Headless: headlessClient, ConfigDirs: configDirs, PromptsDir: paths.PromptsDir, Log: log,
		},
		Now: time.Now,
		Log: log,
	})

	// ---- the prompt queue ----

	var drainController drain.Controller
	queue, err = promptqueue.New(promptqueue.Deps{
		TurnBanners:    turnBanners,
		StripSentinels: stripSentinels,
		ResolveImage:   resolveImage,
		DB:             p.DB,
		Judge:          judge,
		Feed:           feedResolver,
		Footer:         footerResolver,
		Sidebar:        sidebarResolver,
		Holds:          holdsResolver,
		Client:         fleet.Sender,
		Revive:         fleet.Start,
		Watcher:        fleet.Watcher,
		SessionStarted: fleet.Serving,
		SessionAbsent:  fleet.SessionAbsent,
		ColdGate:       fleet.ColdGateDetail,
		DrainRefusals:  refusalNoter{ref: &drainController},
		// A held-prompt edit is state on the host view; the server exists only
		// later, so the publish reads it out of the relay forwarder.
		PublishHost: relay.PublishHostWorkspace,
		Log:         p.Surfaces,
		Stalls:      stalls,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the prompt queue: %w", err)
	}
	lifecycle.queue = queue
	vendorServes.bind(queue)

	// ---- the handover halves ----

	intake, err := handover.NewIntake(p.DB, queue, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the handover intake: %w", err)
	}
	views, err := handover.NewViews(topbarResolver, footerResolver, holdsResolver, sidebarResolver, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the view republisher: %w", err)
	}

	// ---- the rollout and drain controllers ----

	scripts, err := scriptrunner.New(log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the script runner: %w", err)
	}

	// ---- the persistent-wifi controller ----
	//
	// Every host tool it runs goes through the one script runner, and every
	// standing it reads reaches the topbar's chip on every strip.
	wifiConfig, err := persistentwifi.ConfigFromEnv(os.Getenv)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: resolve the persistent-wifi config: %w", err)
	}
	wifi, err := persistentwifi.New(persistentwifi.Deps{
		Config:   wifiConfig,
		Runner:   scripts,
		Clock:    clock.System{},
		OnChange: topbarResolver.SetPersistentWifi,
		Log:      log,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the persistent-wifi controller: %w", err)
	}
	// ---- agent-repl's session ----
	//
	// The span the connectivity indicator's dropdown reports (owner ruling,
	// 2026-10-06): begun by a login made through the daemon's own login flow
	// or by a new Emacs, whichever came later, and durable across daemon
	// restarts. Its traffic is the vendor traffic sampler's (below).
	agentReplSession, err := agentreplsession.New(ctx, p.DB, topbarResolver, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build agent-repl's session: %w", err)
	}
	loginWatch, err := agentreplsession.NewLoginWatch(account.ReadLoginRecord, agentReplSession, time.Now, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the login watch: %w", err)
	}
	selfExe, err := os.Executable()
	if err != nil {
		return nil, fmt.Errorf("claude-repld: resolve this daemon's own binary: %w", err)
	}
	holdoutWarnEvery, err := resolveHoldoutWarnEvery()
	if err != nil {
		return nil, err
	}
	factsBound, err := resolveFactsBound(os.Getenv(envHandoverFactsBound))
	if err != nil {
		return nil, err
	}
	rolloutController, err := rollout.New(rollout.Deps{
		HoldoutWarnEvery: holdoutWarnEvery,
		FactsBound:       factsBound,
		PublishHost:      relay.PublishHostWorkspace,
		SelfExe:          selfExe,
		SelfAddress:      p.Claim.Address(),
		Instance:         p.Instance,
		StateDir:         p.Layout.Dir(),
		IntentManifest:   p.Layout.IntentManifest(),
		DB:               p.DB,
		Spawner:          rollout.NewProcessSpawner(selfExe, p.Layout.Dir(), p.Opts.inherited),
		Announcer:        pushes,
		Pusher:           pushes,
		Participants:     pushes,
		Quiesce:          intake.Quiesce,
		DrainIntake:      intake.DrainIntake,
		LeaseChanged:     queue.OnLeaseChanged,
		Bounces:          queue,
		Freeness:         fleet.Freeness(),
		Shims:            fleet,
		LockProbe:        fleet.ProbeLock,
		StartSession:     fleet.Start,
		BringingUp:       sidebarResolver.SetBringingUp,
		PublishViews:     views.PublishViews,
		StateUnreported: func(ws ids.WorkspaceID, unreported bool) {
			footerResolver.SetStateUnreported(ws, unreported)
			sidebarResolver.SetStateUnreported(ws, unreported)
		},
		WriteDaemonAddr: func(context.Context) error { return p.Claim.Publish() },
		AwaitBootClaim:  p.Claim.AwaitBootClaim,
		ShimBuild:       shimBundle.Build,
		ColdGate:        fleet.RaiseColdGate,
		Progress:        footerResolver,
		// THE ROLLOUT'S ONLY EXIT IS THE HANDOVER, whose shims a successor
		// adopts.
		Exit:     orderlyExit(func() { p.Exit(standDownHandover) }),
		Lifetime: ctx,
		Log:      p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the rollout controller: %w", err)
	}
	rolloutRef.bind(rolloutController)

	// ---- the deploy ----

	clients := &deployClientsForwarder{}
	deployer, restarter, err := buildDeployer(ctx, deployerParams{
		Surfaces:   p.Surfaces,
		Checkout:   paths.Checkout,
		ShimMain:   paths.ShimMain,
		WebappDist: paths.WebappDist,
		StateDir:   p.Layout.Dir(),
		SelfExe:    selfExe,
		Bundle:     shimBundle,
		Rollout:    rolloutController,
		Clients:    clients,
		Runner:     scripts,
		Store:      p.Opts.storeSocket,
		Workspace:  fleet.Workspaces,
		Progress:   footerResolver,
		Faults:     p.DB,
		Joining:    p.Opts.joining != "",
		Getenv:     os.Getenv,
	})
	if err != nil {
		return nil, err
	}

	drainController, err = drain.New(drain.Deps{
		DB:         p.DB,
		IdleCutoff: p.Opts.idleCutoff,
		Stand:      fleet,
		// THE SUPERVISOR ITSELF, not the fleet: an immediate shutdown must
		// also reach the shims that were spawned and have not yet reached the
		// fleet's session map, and the supervisor is the only thing that knows
		// one exists.
		Spawns:       supervisor,
		Freeness:     fleet.Freeness(),
		Reviving:     queue.Reviving,
		Announcer:    pushes,
		LeaseChanged: queue.OnLeaseChanged,
		PublishHost:  relay.PublishHostWorkspace,
		// The verbs own the roster's durable half and are built AFTER this
		// controller, so the republish reads them out of the forwarder --
		// exactly as the merge orchestrator's own roster republish does.
		PublishRegistry: func(ctx context.Context) error {
			verbs, ok := verbsRef.verbs()
			if !ok {
				return fmt.Errorf("claude-repld: a session was hibernated before the workspace verbs existed")
			}
			return verbs.PublishRegistry(ctx)
		},
		// ONE PARK, ONE CALL, BOTH SESSION-SCOPED SURFACES. The footer's strip
		// and the topbar's indicator each draw the shim link, and the roster
		// promises they agree with it about the same link
		// (resolve/sidebar/status.go's linkArm); telling them from one closure
		// is what makes a park they could disagree about unrepresentable.
		SetParked: func(ws ids.WorkspaceID, parked bool) {
			footerResolver.SetParked(ws, parked)
			topbarResolver.SetParked(ws, parked)
		},
		Exit: orderlyExit(func() { p.Exit(standDownOrderly) }),
		Log:  p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the drain controller: %w", err)
	}

	// ---- the merge orchestrator ----

	ownership = workspace.NewOwnership(rolloutController)
	mergeOrchestrator, err := merge.New(merge.Deps{
		PublishHost: relay.PublishHostWorkspace,
		// The verbs own the roster's durable half and are built AFTER the
		// orchestrator, so the republish reads them out of the forwarder.
		PublishRegistry: func(ctx context.Context) error {
			verbs, ok := verbsRef.verbs()
			if !ok {
				return fmt.Errorf("claude-repld: a merge landed before the workspace verbs existed")
			}
			return verbs.PublishRegistry(ctx)
		},
		DB:           p.DB,
		Git:          git,
		Queue:        queue,
		Feed:         feedResolver,
		Footer:       footerResolver,
		Sidebar:      sidebarResolver,
		Holds:        holdsResolver,
		PromptsDir:   paths.PromptsDir,
		CheckoutRoot: paths.Checkout,
		Briefs:       merge.BriefsFrom(paths.PromptsDir),
		SelfRepoDir:  paths.SelfRepo,
		StateDir:     p.Layout.Dir(),
		TestCommand:  merge.TestCommandFor,
		TestRunner:   scripts,
		Painter:      painter,
		StartSession: fleet.Start,
		StopSession:  fleet.KillSession,
		Occupy:       fleet.Occupy,
		AwaitTurnEnd: fleet.AwaitTurnEnd,
		CaptureDisplaced: func(ctx context.Context, ws ids.WorkspaceID) (merge.Displaced, bool, error) {
			d, ok, err := fleet.CaptureDisplaced(ctx, ws)
			return merge.Displaced{Turn: d.Turn, Text: d.Text}, ok, err
		},
		Freeness:          fleet,
		PauseAfterCapture: capturePause(p.Surfaces.Global()),
		PauseInTerminal:   terminalPause(ctx, p.Surfaces.Global()),
		TurnInFlight:      turnInFlight(fleet),
		Rollout:           deployer,
		Log:               p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the merge orchestrator: %w", err)
	}

	// ---- health, login, the verbs ----

	healthReporter, err := health.New(health.Deps{
		DB:       p.DB,
		Live:     fleet.Health,
		Log:      p.Surfaces,
		Instance: p.Instance,
		PID:      os.Getpid(),
		BuildSHA: deployStamp(paths.BuiltSHA),
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the health reporter: %w", err)
	}
	healthRef.bind(healthReporter)
	loginManager, err := login.New(guard, "", func(ws ids.WorkspaceID) (string, error) {
		dir, err := workspaceDir(ws)
		if err != nil {
			return "", err
		}
		return accounts.ConfigDirFor(dir), nil
	}, log, loginWatch)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the login manager: %w", err)
	}

	verbs, err := workspace.New(workspace.Deps{
		Instance:     p.Instance,
		DB:           p.DB,
		Git:          git,
		Accounts:     accounts,
		Queue:        queue,
		Merge:        mergeOrchestrator,
		Rollout:      rolloutController,
		Drain:        drainController,
		Health:       healthReporter,
		Feed:         feedResolver,
		Footer:       footerResolver,
		Topbar:       topbarResolver,
		Sidebar:      sidebarResolver,
		Holds:        holdsResolver,
		Host:         relay,
		Banners:      notifier,
		Sessions:     fleet,
		Headless:     headlessClient,
		Browser:      browser,
		PromptsDir:   paths.PromptsDir,
		CheckoutRoot: paths.Checkout,
		Log:          p.Surfaces,
		Shim:         fleet.Shim,
		Freeness:     fleet.Running,
		Ownership:    ownership,
		Cards:        workspace.NewCards(feedResolver, topbarResolver, fleet),
		LoadPrompt:   prompts.Load,
		SplicePrompt: func(prompt prompts.Prompt, values map[string]string) (string, error) { return prompt.Splice(values) },
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the workspace verbs: %w", err)
	}
	verbsRef.bind(verbs)

	// ---- the three ingresses ----

	handler, err := prompthandler.New(prompthandler.Deps{
		Queue:    queue,
		Feed:     feedResolver,
		DB:       p.DB,
		Panels:   server.Panels(topbarResolver, deployStamp(paths.BuiltSHA), log),
		MintTurn: wsm.NewTurnID,
		MovedOn:  mergeOrchestrator.RetireConcluded,
		Log:      p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the prompt handler: %w", err)
	}
	ingress, err := commandfile.New(commandfile.Deps{
		Dir: p.Layout.OutputDir(),
		// The ingress joins the pattern onto its own directory, so it takes
		// the NAME pattern: the layout's CommandFileGlob is the whole path,
		// and joining that onto the directory again yields a pattern that
		// matches nothing at all.
		Glob:    filepath.Base(p.Layout.CommandFileGlob()),
		Verbs:   verbs,
		DB:      p.DB,
		Merge:   mergeOrchestrator,
		Prompts: handler,
		Home:    paths.Home,
		// ONLY THE DAEMON THAT SERVES TAKES INTAKE (internal/intakegate): not
		// a successor still joining, not an incumbent handing over.
		Serves: rolloutController.ServesIntake,
		Log:    p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the command-file ingress: %w", err)
	}

	// THE HELD-PROMPT INGRESS is where a client leaves a prompt it could not
	// hand to a live daemon. It submits through the same handler the rpc
	// does, under the prompt's own idempotency key, so a prompt this daemon
	// (or its predecessor) already accepted is never delivered twice.
	held, err := heldingress.New(heldingress.Deps{
		Dir:            p.Layout.HeldPromptDir(),
		WorkspaceByDir: p.DB.WorkspaceByDir,
		Prompts:        handler,
		PublishHost:    relay.PublishHostWorkspace,
		Serves:         rolloutController.ServesIntake,
		Log:            p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the held-prompt ingress: %w", err)
	}

	// THE LANDED-WORKTREE REAPER reads the registry and the fleet's live set
	// and takes no lock any interactive path takes: it is background work.
	reaper, err := buildWorktreeReaper(git, p.DB, fleet.Workspaces, paths.RunDir, log)
	if err != nil {
		return nil, err
	}

	// THE DAILY NEWS DIGEST runs only on the daemon that serves, one run at a
	// time across processes, and its condensing call bills the default account.
	digest, err := buildNewsDigest(newsDigestInputs{
		Guard:      guard,
		Headless:   headlessClient,
		PromptsDir: paths.PromptsDir,
		ConfigDir:  p.Opts.defaultConfigDir,
		Store:      p.DB,
		RunDir:     paths.RunDir,
		Serves:     rolloutController.ServesIntake,
		Getenv:     os.Getenv,
		Log:        log,
	})
	if err != nil {
		return nil, err
	}

	// A FULL EMACS RESTART is told apart from a reconnect by the Emacs
	// process identity every Emacs WatchDaemon carries, judged only by the
	// daemon that serves (a joining successor's state client is read-only).
	editors, err := editorinstance.New(p.DB, rolloutController.ServesIntake, time.Now, log, agentReplSession)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the editor instance tracker: %w", err)
	}

	// THE VENDOR TRAFFIC SAMPLER measures every live shim's processes through
	// the kernel, only while this daemon serves, and states what it counted to
	// agent-repl's session once per round.
	background := []backgroundLoop{
		{Name: "drain", Run: drainController.Run},
		{Name: "command_file_ingress", Run: ingress.Run},
		{Name: "held_prompt_ingress", Run: held.Run},
		{Name: "shim_log_roll", Run: func(ctx context.Context) error {
			return runShimLogRolls(ctx, p.Surfaces.ShimRollRequests(), p.DB, rolloutController)
		}},
		{Name: "worktree_reaper", Run: reaper.Run},
		{Name: "lock_watchdog", Run: stalls.Run},
		{Name: "persistent_wifi", Run: wifi.Run},
		{Name: "news_digest", Run: digest.Run},
	}
	// A PROCESS WHOSE VENDOR IS FORBIDDEN MEASURES NOTHING: every test process
	// sets AGENT_REPL_FORBID_VENDOR_CALLS, its vendor is the fake SDK, and no
	// test reads the real kernel's network statistics.
	if !p.Contracts.ForbidVendorCalls() {
		sampler, err := vendortraffic.New(vendortraffic.Config{
			ShimPIDs:  fleet.ShimPIDs,
			Serves:    rolloutController.ServesIntake,
			Processes: vendortraffic.KernelProcesses{},
			Dial:      vendortraffic.DialStatistics,
			Sink:      agentReplSession,
			Log:       log,
		})
		if err != nil {
			return nil, fmt.Errorf("claude-repld: build the vendor traffic sampler: %w", err)
		}
		background = append(background, backgroundLoop{Name: "vendor_traffic", Run: func(ctx context.Context) error {
			sampler.Run(ctx)
			return nil
		}})
	} else {
		log.Info(graphOperation, "vendor traffic is not measured: vendor calls are forbidden in this process", dlog.Context{envc.EnvForbidVendorCalls: "1"})
	}

	log.Debug(graphOperation, "the component graph is built", dlog.Context{
		"joining": p.Opts.joining != "",
	})

	return &graph{
		Server: server.Deps{
			Instance:         p.Instance,
			SessionFacts:     hostSessionFacts{fleet: fleet},
			DB:               p.DB,
			Prompts:          handler,
			Queue:            queue,
			Verbs:            verbs,
			Merge:            mergeOrchestrator,
			Drain:            drainController,
			Rollout:          rolloutController,
			Deploy:           deployer,
			Health:           healthReporter,
			Login:            loginManager,
			Commands:         ingress,
			Ownership:        ownership,
			SuccessorAddress: rolloutController.SuccessorAddress,
			Feed:             feedResolver,
			Footer:           footerResolver,
			Topbar:           topbarResolver,
			Sidebar:          sidebarResolver,
			Holds:            holdsResolver,
			LoudFaults:       loudFaults.Topic(),
			Focus:            focus,
			PersistentWifi:   wifi,
			NewsDigest:       digest,
			EditorInstances:  editors,
			Startup:          startupRuns,
			WebappDist:       paths.WebappDist,
			ImageOrigin:      images.Handler(),
			Log:              p.Surfaces,
		},
		Boot: boot.Deps{
			BindViews:              verbs.BindViews,
			RestoreMissingWorktree: verbs.RestoreMissingWorktree,
			Layout:                 p.Layout,
			DB:                     p.DB,
			Supervisor:             supervisor,
			Queue:                  queue,
			Merge:                  mergeOrchestrator,
			Rollout:                rolloutController,
			RunDir:                 paths.RunDir,
			JoiningAddress:         p.Opts.joining,
			Adopted:                fleet.Install,
			SessionAdopted:         fleet.NoteAdoptedSession,
			StartSession:           fleet.Start,
			EnsureServices:         services.step(bootServiceStep(p.Opts.joining != "", restarter.EnsureLoaded, restarter.EnsureCurrent)),
			Unserved:               fleet.MarkUnserved,
			BringingUp:             sidebarResolver.SetBringingUp,
			AdoptBound:             adoptBound,
			Log:                    p.Surfaces,
		},
		// PRIME IS WHERE A STANDING DRAIN COMES BACK. The daemon topic replays
		// only this process's own latest value, so a schedule that outlived a
		// bounce has to be announced again by the process that inherited it —
		// after the push surface is bound and before anything is served.
		Prime: func(ctx context.Context) error {
			if err := verbs.PublishRegistry(ctx); err != nil {
				return err
			}
			if err := drainController.Republish(ctx); err != nil {
				return err
			}
			// THE FEED TEXT ZOOM COMES BACK the same way a standing drain does:
			// the feed watch topic replays only this process's own latest
			// value, so a zoom persisted across a bounce has to be published
			// again by the process that inherited it — after the surface is
			// bound and before anything is served. The push rides the bound
			// server forwarder, exactly as the drain's republish does.
			scale, err := p.DB.FeedTextScale(ctx)
			if err != nil {
				return err
			}
			pushes.SeedFeedTextScale(scale)
			// THE PERSISTENT-WIFI STANDING IS READ BEFORE ANYTHING IS SERVED,
			// so no topbar chip or Emacs stream is ever drawn from a standing
			// nobody read.
			wifi.Refresh(ctx)
			// THE STANDING NEWS DIGEST COMES BACK the same way: a digest
			// nobody dismissed before a bounce stands again in every webview,
			// read before anything is served.
			return digest.Republish(ctx)
		},
		Bind: func(srv server.Server) {
			pushes.bind(srv)
			relay.bind(srv.Relay())
			clicks.bind(srv)
			clients.bind(srv)
		},
		Background:    background,
		CloseWatchers: fleet.CloseWatchers,
		CloseBanners:  notifier.Close,
		DrainQueue:    queue.Drain,
		DrainStarts:   fleet.DrainStarts,
		DrainMerges:   mergeOrchestrator.Drain,
		StopShims:     stopEveryShim(fleet, supervisor, rootLossShimStopBound),
	}, nil
}

// envMultiRepoRoot selects the multi-repo account root for a workspace under
// it. It is a process contract, never a flag.
const envMultiRepoRoot = "MULTI_REPO_ROOT"

// paths are every filesystem location the graph is built against, with the
// flag overrides already applied over the checkout's defaults.
type paths struct {
	// Checkout is the agent-repl module root the binary was deployed from.
	Checkout string
	// ShimMain, WebappDist, PromptsDir and VocabDir are the four locations
	// beneath it.
	ShimMain, WebappDist, PromptsDir, VocabDir string
	// SelfRepo is the daemon's OWN checkout identity, which the merge
	// orchestrator's two methods key on: the REPOSITORY root containing
	// Checkout, never Checkout itself (resolveSelfRepo).
	SelfRepo string
	// BuiltSHA is daemon/bin/.built-sha, the source-revision stamp the build
	// writes beside the daemon binary: the version the health and status
	// surfaces show.
	BuiltSHA string
	// Home is the user's home directory, which a producer's leading `~`
	// expands to (dirpath.Absolute). A daemon that cannot name it does not
	// boot.
	Home string
	// RunDir is the absolute directory the shim-held kernel locks live in
	// (sessionlock.ResolveRunDir), resolved once for the fleet, the boot
	// sequence and the reaper alike.
	RunDir string
}

// envSelfRepo overrides the daemon's own-checkout identity for tests. The flag
// beats it, and it beats the resolved checkout.
const envSelfRepo = "AGENT_REPL_SELF_REPO_DIR"

// envPromptsDir is the prompts directory's operator contract; the flag beats
// it, and it beats the checkout's own prompts/.
const envPromptsDir = "AGENT_REPL_PROMPTS_DIR"

// resolvePaths resolves the checkout and applies the flag and environment
// overrides over it. The checkout is resolved even when every path is
// overridden, because the render vocabulary has no flag: it is shared with
// every other system in the repository and is read from the tree.
func resolvePaths(opts options) (paths, error) {
	root, err := checkout.Root(mustExecutable())
	if err != nil {
		return paths{}, fmt.Errorf("claude-repld: %w", err)
	}
	selfRepo, err := resolveSelfRepo(opts, root)
	if err != nil {
		return paths{}, err
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return paths{}, fmt.Errorf("claude-repld: resolve the home directory a producer's `~` expands to: %w", err)
	}
	runDir, err := sessionlock.ResolveRunDir()
	if err != nil {
		return paths{}, fmt.Errorf("claude-repld: resolve the kernel-lock directory: %w", err)
	}
	out := paths{
		Checkout:   root,
		ShimMain:   firstNonEmpty(opts.shim, checkout.ShimMain(root)),
		WebappDist: firstNonEmpty(opts.webapp, checkout.WebappDist(root)),
		PromptsDir: firstNonEmpty(opts.promptsDir, os.Getenv(envPromptsDir), checkout.PromptsDir(root)),
		VocabDir:   checkout.VocabDir(root),
		SelfRepo:   selfRepo,
		BuiltSHA:   filepath.Join(root, "daemon", "bin", ".built-sha"),
		Home:       home,
		RunDir:     runDir,
	}
	return out, nil
}

// resolveSelfRepo answers the daemon's own checkout identity: the flag, then
// the environment, then the REPOSITORY root containing the module root. The
// merge orchestrator compares it against a workspace's target worktree root
// and runs the gate at <it>/modules/app/agent-repl/bin/test-all.sh, so it must
// be the repository root, never the module root beneath it. The repository
// root is derived only when nothing overrides it, so an unmarked
// $AGENT_REPL_CHECKOUT paired with an override still boots.
func resolveSelfRepo(opts options, root string) (string, error) {
	if override := firstNonEmpty(opts.selfRepo, os.Getenv(envSelfRepo)); override != "" {
		return override, nil
	}
	repo, err := checkout.RepoRoot(root)
	if err != nil {
		return "", fmt.Errorf("claude-repld: resolving the self repository: %w", err)
	}
	return repo, nil
}

// mustExecutable answers this process's binary, falling back to argv[0] when
// the platform will not say: the checkout resolution has its own fallbacks
// after this one, and refusing here would refuse a boot the next step can
// still complete.
func mustExecutable() string {
	if exe, err := os.Executable(); err == nil {
		return exe
	}
	return os.Args[0]
}

// firstNonEmpty answers the first override that was actually set.
func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if v != "" {
			return v
		}
	}
	return ""
}

// hostSessionFacts adapts the session fleet to server.SessionFacts. The
// conversion lives HERE because internal/workspace sits below internal/server
// and cannot name its types: the composition root is the one place that knows
// both sides.
type hostSessionFacts struct{ fleet *workspace.Fleet }

func (h hostSessionFacts) HostSessionFacts(ws ids.WorkspaceID) (server.HostFacts, bool) {
	facts, live := h.fleet.HostSessionFacts(ws)
	if !live {
		return server.HostFacts{}, false
	}
	// BACKFILL HAS NO PRODUCER HERE. It is the file plane's delivery into the
	// store; the daemon never imports store.v1 and holds no store client, so
	// the only honest arm is `none` -- "no transcript reached the store
	// through anything this daemon can see". The fleet's BackfillKnown says
	// the same, and it is always false.
	return server.HostFacts{
		SessionID:    facts.SessionID,
		Generation:   facts.Generation,
		ShimAttached: facts.ShimAttached,
		Backfill:     server.BackfillNone,
	}, true
}

// StandingGate answers the gate the user must answer on the workspace: the
// cold gate while it stands unanswered.
func (h hostSessionFacts) StandingGate(ws ids.WorkspaceID) (server.HostGateKind, bool) {
	if h.fleet.ColdGateShown(ws) {
		return server.HostGateColdGate, true
	}
	return 0, false
}

// sentinelStripper adapts prompts.StripSentinels to the resolvers' drawing
// seam. An UNBALANCED marker is a producer bug: it is recorded at WARNING and
// the text is drawn as it stands, because losing the prompt row is worse than
// drawing a marker the reader can see and report.
func sentinelStripper(log dlog.Logger) func(string) string {
	return func(text string) string {
		drawn, err := prompts.StripSentinels(text)
		if err != nil {
			log.Warn("daemon.cmd.strip_sentinels",
				"a prompt's injected spans are unbalanced; it is drawn unstripped",
				dlog.Context{"error": err.Error()})
			return text
		}
		return drawn
	}
}

// DeployStampEnv overrides the daemon's source-revision stamp, which the
// harness needs because the stamp file lives beside a binary it builds itself.
const DeployStampEnv = "AGENT_REPL_DEPLOY_STAMP"

// deployStamp reads the daemon's source-revision stamp: the version the health
// and status surfaces show. It is NOT a staleness authority — a deploy judges
// every component by content hash (internal/buildid).
func deployStamp(path string) func() (string, error) {
	return func() (string, error) {
		if fromEnv := os.Getenv(DeployStampEnv); fromEnv != "" {
			return fromEnv, nil
		}
		raw, err := os.ReadFile(path)
		if errors.Is(err, os.ErrNotExist) {
			// NO STAMP AT ALL is an ordinary state, not a failure: this
			// checkout was never put through the deploy chain (every test
			// harness builds the binary with `go build -o <tmp>`). The
			// staleness check reads an empty stamp as "leave the shim alone",
			// which is exactly right, and nothing is warned about. A stamp
			// that EXISTS and cannot be read, or is blank, is still an error.
			return "", nil
		}
		if err != nil {
			return "", fmt.Errorf("read the deploy stamp %s: %w", path, err)
		}
		sha := strings.TrimSpace(string(raw))
		if sha == "" {
			return "", fmt.Errorf("the deploy stamp %s is empty", path)
		}
		return sha, nil
	}
}

// orderlyExit ends the serving lifetime, which is what unwinds the boot
// spine's deferred withdrawal, stream closes and handle closes in order.
func orderlyExit(stop func()) func(context.Context) error {
	return func(context.Context) error {
		stop()
		return nil
	}
}

// turnInFlight answers a workspace's turn in flight off the session fleet: the
// turn an agent's merge request waits on before it is put in line. A workspace
// with no live session, or one parked behind a cold gate, has none.
func turnInFlight(fleet *workspace.Fleet) func(ids.WorkspaceID) (ids.TurnID, bool) {
	return func(ws ids.WorkspaceID) (ids.TurnID, bool) {
		running, live := fleet.Running(ws)
		if !live || running.Turn == nil {
			return "", false
		}
		return *running.Turn, true
	}
}

// FakeShimsEnv forces every shim spawn into the shim's offline scripted SDK
// WITHOUT putting the whole stack in fake mode. It is a TEST HOOK, and the one
// seam that lets a suite exercise a REAL vendor call site — the classifier's
// headless run — against a live session: whole-stack fake mode makes the
// classifier scripted too, and turning it off makes the shim spawn a vendor
// call the guard refuses before any session exists.
//
// It can only turn fake ON. Nothing about it can make a production spawn less
// fake than the contract already says it is.
const FakeShimsEnv = "AGENT_REPL_FAKE_SHIMS"

// fakeShims reports whether the shim-only fake hook is set.
func fakeShims() bool {
	value := os.Getenv(FakeShimsEnv)
	return value != "" && value != "0" && !strings.EqualFold(value, "false")
}

// buildJudge builds the interjection classifier: the scripted one under the
// whole stack's fake mode, the vendor-backed one otherwise. The vendor guard
// refuses the real one when vendor calls are forbidden, which is why it is
// handed in rather than checked here.
func buildJudge(contracts envc.Contracts, guard envc.VendorGuard, runner headless.Runner, promptsDir string) (classifier.Judge, error) {
	if contracts.Fake() {
		return classifier.NewFake(), nil
	}
	return classifier.New(guard, runner, promptsDir)
}

// refusalNoter carries the queue's drain refusals to the drain controller,
// which is built after the queue. It reads the controller out of a pointer the
// graph fills in, rather than being a second record of the same refusals.
type refusalNoter struct{ ref *drain.Controller }

// NoteRefusal records one refused submission, dropping it only if the queue
// somehow refused something before the drain controller existed — which is a
// boot-order defect rather than a state.
func (n refusalNoter) NoteRefusal(ws ids.WorkspaceID) {
	if n.ref == nil || *n.ref == nil {
		return
	}
	(*n.ref).NoteRefusal(ws)
}

// MergeCapturePauseEnv is a TEST-ONLY knob naming a rendezvous file. When it
// is set, a merge run — right after it captured and ended the user's displaced
// turn — creates that file and then blocks forever, which is the only way a
// suite can hold a daemon inside the window a crash has to land in for the
// boot recovery of displaced turns to have anything to do. UNSET IN
// PRODUCTION: with no rendezvous named, the merge orchestrator gets a nil
// pause and the code below is never reached.
const MergeCapturePauseEnv = "AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE"

// capturePause builds the test-only admission pause, or nil when the knob
// names no rendezvous. A rendezvous file that cannot be created is an ERROR
// and the run is NOT held: a pause whose signal never reached the test would
// hang the suite with nothing said about why.
func capturePause(log dlog.Logger) merge.AdmissionPause {
	const op = "daemon.merge.capture_pause"
	path := os.Getenv(MergeCapturePauseEnv)
	if path == "" {
		return nil
	}
	return func(ctx context.Context, ws ids.WorkspaceID) {
		fields := dlog.Context{"workspace": string(ws), "rendezvous": path}
		if err := os.WriteFile(path, []byte(string(ws)), 0o644); err != nil {
			log.Error(op, "the merge capture rendezvous file could not be written; the run is not held",
				dlog.Context{"workspace": string(ws), "rendezvous": path, "error": err.Error()})
			return
		}
		log.Info(op, "holding the merge run after the displaced capture", fields)
		<-ctx.Done()
	}
}

// MergeTerminalPauseEnv names the rendezvous file the TEST-ONLY terminal pause
// writes and then holds a merge run on, inside its terminal and before its
// first durable stamp. It is what lets a suite stop the daemon exactly in the
// window the shutdown drain covers. UNSET IN PRODUCTION: with no rendezvous
// named, the orchestrator gets a nil pause and the code below is never
// reached.
const MergeTerminalPauseEnv = "AGENT_REPL_MERGE_PAUSE_IN_TERMINAL"

// terminalPause builds the test-only terminal pause, or nil when the knob
// names no rendezvous. It holds on the SERVING lifetime rather than on the
// run's own context — the run's context is the pump's, which nothing cancels —
// so the held terminal resumes the instant the daemon starts its orderly exit,
// which is the whole point: the stamps then race the store's close, and the
// drain is what decides the outcome. A rendezvous file that cannot be written
// is an ERROR and the run is NOT held, because a pause whose signal never
// reached the test would hang the suite with nothing said about why.
func terminalPause(serving context.Context, log dlog.Logger) merge.AdmissionPause {
	const op = "daemon.merge.terminal_pause"
	path := os.Getenv(MergeTerminalPauseEnv)
	if path == "" {
		return nil
	}
	return func(_ context.Context, ws ids.WorkspaceID) {
		fields := dlog.Context{"workspace": string(ws), "rendezvous": path}
		if err := os.WriteFile(path, []byte(string(ws)), 0o644); err != nil {
			log.Error(op, "the merge terminal rendezvous file could not be written; the run is not held",
				dlog.Context{"workspace": string(ws), "rendezvous": path, "error": err.Error()})
			return
		}
		log.Info(op, "holding the merge run inside its terminal", fields)
		<-serving.Done()
	}
}

// HoldoutWarnEnv compresses the rollout's never-free holdout warning cadence
// for tests. A suite that must observe the warning cannot wait the production
// ten minutes for it, and nothing else in the daemon can shorten it.
const HoldoutWarnEnv = "AGENT_REPL_HOLDOUT_WARN_EVERY"

// resolveHoldoutWarnEvery answers the holdout warning cadence: the environment
// knob when it is set, else zero, which the rollout controller fills with its
// own DefaultHoldoutWarnEvery. A MALFORMED value is an ERROR rather than a
// fall-through to the default: a test knob that silently did nothing would make
// the suite it was set for lie.
func resolveHoldoutWarnEvery() (time.Duration, error) {
	raw := os.Getenv(HoldoutWarnEnv)
	if raw == "" {
		return 0, nil
	}
	return parsePositiveDuration(HoldoutWarnEnv, raw)
}

// adoptedDeathWitness renders a kernel-lock probe as the supervisor's
// adopted-death witness. ONLY a lock that reads FREE is evidence that an
// adopted shim is gone: StateUnknown and a probe error both answer "not free",
// and the error is surfaced so the client records it and keeps redialing,
// per the boot rule that a could-not-tell probe is never read as free.
func adoptedDeathWitness(probe func(string) (sessionlock.State, error)) func(string) (bool, error) {
	return func(workspaceDir string) (bool, error) {
		state, err := probe(workspaceDir)
		if err != nil {
			return false, err
		}
		return state == sessionlock.StateFree, nil
	}
}

// envStartSessionBound overrides workspace.DefaultStartSessionBound. It exists
// for the integration suite, whose subject includes a shim that accepts the
// start and never answers it.
const envStartSessionBound = "AGENT_REPL_START_SESSION_BOUND"

// resolveStartBound reads the start bound's override. Empty is
// workspace.DefaultStartSessionBound; a malformed or non-positive value is a
// REFUSAL, on the same reasoning resolveAdoptBound states.
func resolveStartBound(value string) (time.Duration, error) {
	return resolveDurationKnob(envStartSessionBound, value, workspace.DefaultStartSessionBound)
}

// envBootAdoptBound overrides boot.DefaultAdoptBound. It exists for the
// integration suite, whose subject includes a survivor that never answers.
const envBootAdoptBound = "AGENT_REPL_BOOT_ADOPT_BOUND"

// resolveAdoptBound reads the adoption bound's override. Empty is
// boot.DefaultAdoptBound; a malformed or non-positive value is a REFUSAL,
// because a knob that silently did nothing would make the run it was set for
// report a bound it never used.
func resolveAdoptBound(value string) (time.Duration, error) {
	return resolveDurationKnob(envBootAdoptBound, value, boot.DefaultAdoptBound)
}

// envHandoverFactsBound overrides rollout.DefaultFactsBound. It exists for
// the integration suite, whose subject includes an adopted shim that never
// re-announces its session facts.
const envHandoverFactsBound = "AGENT_REPL_HANDOVER_FACTS_BOUND"

// resolveFactsBound reads the mid-work adoption's facts bound override. Empty
// is rollout.DefaultFactsBound; a malformed or non-positive value is a
// REFUSAL, as every duration knob's is.
func resolveFactsBound(value string) (time.Duration, error) {
	return resolveDurationKnob(envHandoverFactsBound, value, rollout.DefaultFactsBound)
}

// faultSurfaces is the health package's fault sink: every fault is drawn on
// the footer, and the ones the topbar carries on its warning strip too and in
// the standing loud faults every Emacs is told. It
// translates the health verdict into each resolver's own vocabulary and adds
// nothing: the partition and the lines are health's, the drawing theirs.
type faultSurfaces struct {
	footer  footer.Resolver
	sidebar sidebar.Resolver
	topbar  topbar.Resolver
	loud    *health.LoudFaults

	mu sync.Mutex
	// onTopbar are the open faults raised on the topbar (and so told to
	// Emacs), so a close retracts exactly those.
	onTopbar map[ids.FaultID]bool
}

func newFaultSurfaces(f footer.Resolver, s sidebar.Resolver, t topbar.Resolver, loud *health.LoudFaults) *faultSurfaces {
	return &faultSurfaces{footer: f, sidebar: s, topbar: t, loud: loud, onTopbar: map[ids.FaultID]bool{}}
}

// FaultOpened puts a standing fault on the workspace's strip, or on every
// strip when the fault is daemon-scoped, and on the topbar when it carries
// the fault.
func (f *faultSurfaces) FaultOpened(ws ids.WorkspaceID, line health.FaultLine) {
	f.footer.OpenFault(ws, footer.Fault{
		ID:        string(line.ID),
		Kind:      line.Kind,
		Status:    string(line.Cell.Status),
		SubStatus: line.Cell.SubStatus,
		Detail:    line.Detail,
		At:        line.At,
	})
	// THE ROSTER TAKES THE NETWORK FAULT FROM THE SAME DOOR, so the footer's
	// network_fault and the roster's never stand on different facts. Every
	// other domain reaches the roster by its own edges (the link, the
	// vendor-start run).
	if line.Cell.Status == health.FaultStatusNetworkFault && ws != "" {
		f.sidebar.NetworkFaultOpened(ws, string(line.ID))
	}
	// ONLY A DAEMON-SCOPED FAULT reaches the topbar: health.FaultTopbarLine
	// states no line for any other.
	if line.Topbar == "" || ws != "" {
		return
	}
	f.mu.Lock()
	f.onTopbar[line.ID] = true
	f.mu.Unlock()
	f.topbar.RaiseDaemonWarning(string(line.ID), topbarWarning(line))
	f.loud.Opened(line)
}

// topbarWarning is a daemon-scoped fault's topbar row: its line, and for a
// failed deploy the overlay saying what failed.
func topbarWarning(line health.FaultLine) topbar.DaemonWarning {
	warning := topbar.DaemonWarning{Line: line.Topbar}
	if line.Kind != health.KindDeployFailed {
		return warning
	}
	if o, ok := health.DeployFailedOverlay(line.Record); ok {
		warning.DeployFailed = &topbar.DeployFailedOverlay{
			Step: o.Step, Component: o.Component, Rollback: o.Rollback, Detail: o.Detail, Log: o.Log,
		}
	}
	return warning
}

// FaultClosed retracts it again, from the topbar too when it was raised
// there.
func (f *faultSurfaces) FaultClosed(ws ids.WorkspaceID, id ids.FaultID) {
	f.footer.CloseFault(ws, string(id))
	if ws != "" {
		f.sidebar.FaultClosed(ws, string(id))
	}
	f.mu.Lock()
	raised := f.onTopbar[id]
	delete(f.onTopbar, id)
	f.mu.Unlock()
	if raised {
		f.topbar.RetractDaemonWarning(string(id))
		f.loud.Closed(id)
	}
}

// opEditorBringUp names the editor startup's own bring-up records.
const opEditorBringUp = "daemon.startup.bring_up"

// editorBringUp starts the named workspaces' sessions for the editor's startup
// through THE ONE BRING-UP (bringup.Run, and through it Fleet.Start), on the
// fleet's own detached lifetime so the exit drains and joins it. Each
// workspace's bring-up marker is raised before any start, as the boot and the
// takeover raise theirs; a workspace whose record cannot be read is told done
// with that error at once and never started.
// editorFleet is the slice of the fleet the editor's bring-up drives: the one
// start path, and the fleet's detached lifetime it runs on.
type editorFleet interface {
	Start(ctx context.Context, ws ids.WorkspaceID) error
	Detach(run func(context.Context))
}

// editorRecords is what the editor's bring-up reads: each workspace's record,
// and (through bringup.Run) its session record.
type editorRecords interface {
	bringup.SessionReader
	Workspace(ctx context.Context, ws ids.WorkspaceID) (wsm.Workspace, error)
}

func editorBringUp(fleet editorFleet, db editorRecords, ownership workspace.Ownership, marker func(ids.WorkspaceID, bool), log dlog.Logger,
	pending []ids.WorkspaceID, done func(ids.WorkspaceID, error)) {
	records := make([]wsm.Workspace, 0, len(pending))
	for _, ws := range pending {
		// ONLY A WORKSPACE THIS DAEMON SERVES IS STARTED HERE. One handed to a
		// successor, or not yet adopted by this joining daemon, is the other
		// daemon's to start: starting it here races that daemon's shim (a
		// StartSession it answers `already_started`). It is told done, so its
		// tab is not held behind a start that is not this daemon's.
		standing, err := ownership.Standing(context.Background(), ws)
		if err != nil {
			log.Error(opEditorBringUp, "a workspace's serving standing could not be read; it is not brought up", dlog.Context{
				dlog.KeyWorkspaceID: string(ws), "error": err.Error(),
			})
			done(ws, fmt.Errorf("read the serving standing of %q: %w", ws, err))
			continue
		}
		if standing != workspace.StandingOwned {
			log.Info(opEditorBringUp, "a workspace another daemon serves is not brought up here", dlog.Context{
				dlog.KeyWorkspaceID: string(ws), "standing": int(standing),
			})
			done(ws, nil)
			continue
		}
		record, err := db.Workspace(context.Background(), ws)
		if err != nil {
			log.Error(opEditorBringUp, "a workspace the editor's startup names could not be read; it is not brought up", dlog.Context{
				dlog.KeyWorkspaceID: string(ws), "error": err.Error(),
			})
			done(ws, fmt.Errorf("read the workspace record of %q: %w", ws, err))
			continue
		}
		marker(ws, true)
		records = append(records, record)
	}
	fleet.Detach(func(ctx context.Context) {
		bringup.Run(ctx, bringup.Deps{
			DB: db,
			StartSession: func(ctx context.Context, ws ids.WorkspaceID) error {
				err := fleet.Start(ctx, ws)
				if err == nil {
					return nil
				}
				// A HANDOVER CAN TAKE THE WORKSPACE WHILE ITS START RUNS: the
				// successor's shim then refuses this daemon's start. Re-read
				// the standing; one no longer this daemon's is stood down.
				standing, standingErr := ownership.Standing(ctx, ws)
				if standingErr != nil {
					return errors.Join(err, fmt.Errorf("read the serving standing of %q: %w", ws, standingErr))
				}
				if standing != workspace.StandingOwned {
					return fmt.Errorf("%w: %w", bringup.ErrNotServed, err)
				}
				return err
			},
			BringingUp: marker,
			Log:        log,
			Operation:  opEditorBringUp,
			Done:       done,
		}, records)
	})
}
