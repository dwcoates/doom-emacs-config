package main

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/boot"
	"claude-repld/internal/checkout"
	"claude-repld/internal/classifier"
	"claude-repld/internal/commandfile"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/envc"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/handover"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
	"claude-repld/internal/merge"
	"claude-repld/internal/paint"
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
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
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
//     login manager, the prompt handler and the command-file ingress;
//  7. the boot sequence's dependencies and the server's, returned together
//     with the late bindings and the background loops.
//
// NOTHING HERE IMPROVISES A STAND-IN. A dependency with no producer would be
// declared in `unwired` and would fail the boot LOUDLY, naming it.

// graphOperation is the operation this file's own records carry.
const graphOperation = "daemon.cmd.graph"

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
	judge, err := buildJudge(p.Contracts, guard, paths.PromptsDir)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the classifier: %w", err)
	}
	browser, err := externalbrowser.New(externalbrowser.Config{Logger: log})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the external browser: %w", err)
	}
	supervisor, err := shimclient.NewSupervisor(p.Surfaces)
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
	feedResolver, err := feed.New(feed.Deps{
		Log:          p.Surfaces,
		WorkspaceDir: workspaceDir,
		Painter:      painter,
		// THE IMAGE ORIGIN HAS NO PRODUCER. The daemon serves the webapp's
		// dist directory and nothing else, so an image reference has no
		// servable source; the resolver refuses loudly and names what is
		// missing rather than drawing a broken image.
		ResolveImage: feed.UnproducedImageResolver(log),
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the feed resolver: %w", err)
	}
	footerResolver, err := footer.New(p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the footer resolver: %w", err)
	}
	topbarResolver, err := topbar.New(colors, p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the topbar resolver: %w", err)
	}
	sidebarResolver, err := sidebar.New(colors, p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the sidebar resolver: %w", err)
	}
	holdsResolver, err := holds.New(p.Surfaces)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the hold tray: %w", err)
	}

	// ---- the forwarders, and the fleet they let exist first ----

	pushes := &serverForwarder{}
	relay := &relayForwarder{}
	verbsRef := &verbsForwarder{}
	mergeRef := &mergeForwarder{}

	var queue promptqueue.Queue
	lifecycle := &lifecycleSink{verbs: verbsRef, log: log}

	fleet, err := workspace.NewFleet(workspace.FleetDeps{
		DB:         p.DB,
		Accounts:   accounts,
		Supervisor: supervisor,
		Sinks: sessionwatcher.Sinks{
			Feed:      feedResolver,
			Footer:    footerResolver,
			Topbar:    topbarResolver,
			Sidebar:   sidebarResolver,
			Holds:     holdsResolver,
			Lifecycle: lifecycle,
		},
		Feed:         feedResolver,
		Footer:       footerResolver,
		SocketPath:   func(ws ids.WorkspaceID) string { return p.Layout.ShimSocket(string(ws)) },
		StoreSocket:  p.Opts.storeSocket,
		NodeBin:      p.Opts.node,
		MainJS:       paths.ShimMain,
		ShimBuildSHA: paths.ShimBuildSHA,
		DefaultModel: os.Getenv(workspace.DefaultModelEnv),
		Fake:         p.Contracts.Fake(),
		ForbidVendor: p.Contracts.ForbidVendorCalls(),
		Log:          p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the session fleet: %w", err)
	}

	// ---- the prompt queue ----

	var drainController drain.Controller
	queue, err = promptqueue.New(promptqueue.Deps{
		DB:      p.DB,
		Judge:   judge,
		Feed:    feedResolver,
		Footer:  footerResolver,
		Holds:   holdsResolver,
		Client:  fleet.Sender,
		Watcher: fleet.Watcher,
		// A submission that arrives under a PARKED merge lease goes to the
		// orchestrator as guidance. The queue never imports merge, so the
		// route is a function; the orchestrator does not exist yet, so the
		// function reads it out of the forwarder when it is called.
		ParkedRoute: func(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) (ids.TurnID, error) {
			orchestrator, ok := mergeRef.orchestrator()
			if !ok {
				return "", fmt.Errorf("claude-repld: a parked submission arrived before the merge orchestrator existed")
			}
			// The orchestrator answers only whether the guidance was taken;
			// the turn it runs as is the orchestrator's own and does not come
			// back through this seam.
			return "", orchestrator.RouteParked(ctx, ws, said)
		},
		DrainRefusals: refusalNoter{ref: &drainController},
		OneShotFinish: func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
			verbs, ok := verbsRef.verbs()
			if !ok {
				return fmt.Errorf("claude-repld: a one-shot turn concluded before the workspace verbs existed")
			}
			return verbs.OnOneShotTurnConcluded(ctx, ws, turn)
		},
		Log: p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the prompt queue: %w", err)
	}
	lifecycle.queue = queue

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
	selfExe, err := os.Executable()
	if err != nil {
		return nil, fmt.Errorf("claude-repld: resolve this daemon's own binary: %w", err)
	}
	rolloutController, err := rollout.New(rollout.Deps{
		Deploy:          scripts,
		SelfExe:         selfExe,
		SelfRepoDir:     paths.SelfRepo,
		SelfAddress:     p.Claim.Address(),
		Instance:        p.Instance,
		StateDir:        p.Layout.Dir(),
		IntentManifest:  p.Layout.IntentManifest(),
		Git:             git,
		DB:              p.DB,
		Spawner:         rollout.NewProcessSpawner(selfExe, p.Layout.Dir()),
		Announcer:       pushes,
		Pusher:          pushes,
		Participants:    pushes,
		Quiesce:         intake.Quiesce,
		DrainIntake:     intake.DrainIntake,
		LeaseChanged:    queue.OnLeaseChanged,
		Freeness:        fleet.Freeness(),
		Shims:           fleet,
		LockProbe:       fleet.ProbeLock,
		PublishViews:    views.PublishViews,
		WriteDaemonAddr: func(context.Context) error { return p.Claim.Publish() },
		DeployStamp:     deployStamp(paths.BuiltSHA),
		SessionBuildSHA: fleet.SessionBuildSHA,
		ColdGate:        fleet.RaiseColdGate,
		Exit:            orderlyExit(p.Exit),
		Log:             p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the rollout controller: %w", err)
	}

	drainController, err = drain.New(drain.Deps{
		DB:         p.DB,
		IdleCutoff: p.Opts.idleCutoff,
		Stand:      fleet,
		Freeness:   fleet.Freeness(),
		Announcer:  pushes,
		Exit:       orderlyExit(p.Exit),
		Log:        p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the drain controller: %w", err)
	}

	// ---- the merge orchestrator ----

	ownership := workspace.NewOwnership(rolloutController)
	mergeOrchestrator, err := merge.New(merge.Deps{
		DB:               p.DB,
		Git:              git,
		Queue:            queue,
		Feed:             feedResolver,
		Footer:           footerResolver,
		Sidebar:          sidebarResolver,
		Holds:            holdsResolver,
		PromptsDir:       paths.PromptsDir,
		Briefs:           merge.BriefsFrom(paths.PromptsDir),
		SelfRepoDir:      paths.SelfRepo,
		StateDir:         p.Layout.Dir(),
		TestCommand:      merge.TestCommandFor(paths.SelfRepo),
		TestRunner:       scripts,
		Painter:          painter,
		StartSession:     fleet.Start,
		Occupy:           fleet.Occupy,
		AwaitTurnEnd:     fleet.AwaitTurnEnd,
		CaptureDisplaced: fleet.CaptureDisplaced,
		ParkedRoute:      guidanceRoute(fleet, mergeRef),
		Rollout:          rolloutController,
		Log:              p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the merge orchestrator: %w", err)
	}
	mergeRef.bind(mergeOrchestrator)

	// ---- health, login, the verbs ----

	healthReporter, err := health.New(health.Deps{
		DB:   p.DB,
		Live: fleet.Health,
		Log:  p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the health reporter: %w", err)
	}
	loginManager, err := login.New(guard, "", func(ws ids.WorkspaceID) (string, error) {
		dir, err := workspaceDir(ws)
		if err != nil {
			return "", err
		}
		return accounts.ConfigDirFor(dir), nil
	}, log)
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the login manager: %w", err)
	}

	verbs, err := workspace.New(workspace.Deps{
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
		Sessions:     fleet,
		Browser:      browser,
		PromptsDir:   paths.PromptsDir,
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

	// ---- the two ingresses ----

	handler, err := prompthandler.New(prompthandler.Deps{
		Queue:    queue,
		Feed:     feedResolver,
		DB:       p.DB,
		Panels:   server.Panels(topbarResolver, deployStamp(paths.BuiltSHA), log),
		MintTurn: wsm.NewTurnID,
		Log:      p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the prompt handler: %w", err)
	}
	ingress, err := commandfile.New(commandfile.Deps{
		Dir:     p.Layout.OutputDir(),
		Glob:    p.Layout.CommandFileGlob(),
		Verbs:   verbs,
		DB:      p.DB,
		Merge:   mergeOrchestrator,
		Prompts: handler,
		Log:     p.Surfaces,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the command-file ingress: %w", err)
	}

	log.Debug(graphOperation, "the component graph is built", dlog.Context{
		"joining": p.Opts.joining != "",
	})

	return &graph{
		Server: server.Deps{
			Instance:         p.Instance,
			DB:               p.DB,
			Prompts:          handler,
			Queue:            queue,
			Verbs:            verbs,
			Merge:            mergeOrchestrator,
			Drain:            drainController,
			Rollout:          rolloutController,
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
			WebappDist:       paths.WebappDist,
			Log:              p.Surfaces,
		},
		Boot: boot.Deps{
			Layout:         p.Layout,
			DB:             p.DB,
			Supervisor:     supervisor,
			Queue:          queue,
			Merge:          mergeOrchestrator,
			Rollout:        rolloutController,
			RunDir:         lockDir(),
			JoiningAddress: p.Opts.joining,
			Adopted:        fleet.Install,
			Log:            p.Surfaces,
		},
		Prime: verbs.PublishRegistry,
		Bind: func(srv server.Server) {
			pushes.bind(srv)
			relay.bind(srv.Relay())
		},
		Background: []backgroundLoop{
			{Name: "drain", Run: drainController.Run},
			{Name: "command_file_ingress", Run: ingress.Run},
		},
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
	// orchestrator's two methods key on.
	SelfRepo string
	// BuiltSHA is daemon/bin/.built-sha, the stamp the deploy chain writes.
	BuiltSHA string
	// ShimBuildSHA is the bundle sha every shim spawn is stamped with,
	// resolved from the shim's own build stamp or the environment override.
	ShimBuildSHA string
}

// envShimBuildSHA overrides the shim bundle's build sha when the shim's build
// stamp is absent — a checkout that has not built the shim, and every test
// harness, which runs a fake shim that has no bundle at all.
const envShimBuildSHA = "SHIM_BUILD_SHA"

// resolveShimBuildSHA answers the sha every shim spawn is stamped with. The
// shim's own build stamp answers first, because that is the bundle the daemon
// actually launches; the environment answers when the stamp is absent. With
// neither, the boot REFUSES: an unstamped spawn cannot be checked for
// staleness, and a blank stamp would silently call every shim current.
func resolveShimBuildSHA(stampPath, fromEnv string) (string, error) {
	raw, err := os.ReadFile(stampPath)
	switch {
	case err == nil:
		sha := strings.TrimSpace(string(raw))
		if sha == "" {
			return "", fmt.Errorf("claude-repld: the shim build stamp %s is empty", stampPath)
		}
		return sha, nil
	case errors.Is(err, fs.ErrNotExist):
		if sha := strings.TrimSpace(fromEnv); sha != "" {
			return sha, nil
		}
		return "", fmt.Errorf(
			"claude-repld: the shim build sha is unresolvable: the shim build stamp %s does not exist and %s is unset",
			stampPath, envShimBuildSHA)
	default:
		return "", fmt.Errorf("claude-repld: read the shim build stamp %s: %w", stampPath, err)
	}
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
	out := paths{
		Checkout:   root,
		ShimMain:   firstNonEmpty(opts.shim, checkout.ShimMain(root)),
		WebappDist: firstNonEmpty(opts.webapp, checkout.WebappDist(root)),
		PromptsDir: firstNonEmpty(opts.promptsDir, os.Getenv(envPromptsDir), checkout.PromptsDir(root)),
		VocabDir:   checkout.VocabDir(root),
		SelfRepo:   firstNonEmpty(opts.selfRepo, os.Getenv(envSelfRepo), root),
		BuiltSHA:   filepath.Join(root, "daemon", "bin", ".built-sha"),
	}
	sha, err := resolveShimBuildSHA(checkout.ShimBuildStamp(root), os.Getenv(envShimBuildSHA))
	if err != nil {
		return paths{}, err
	}
	out.ShimBuildSHA = sha
	return out, nil
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

// lockDir answers where the shim-held kernel locks live: the test override,
// then the default under the home directory. The daemon only ever PROBES them.
func lockDir() string {
	if fromEnv := os.Getenv(workspace.LockDirEnv); fromEnv != "" {
		return fromEnv
	}
	if home, err := os.UserHomeDir(); err == nil {
		return filepath.Join(home, ".cache", "agent-repl", "run")
	}
	return workspace.DefaultLockDir
}

// deployStamp reads the deployed build's sha from the stamp the deploy chain
// writes. A missing stamp is an ERROR rather than an empty sha: a staleness
// check against nothing would call every shim current.
func deployStamp(path string) func() (string, error) {
	return func() (string, error) {
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

// guidanceRoute delivers the merge's guidance into the workspace's session as
// a turn of its own. The ORIGIN is taken from the orchestrator's own active
// tab, because the two parked tabs are two different repairs and a prompt's
// origin is what makes it traceable to the situation that caused it.
func guidanceRoute(fleet *workspace.Fleet, mergeRef *mergeForwarder) merge.ParkedRouter {
	return func(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) (ids.TurnID, error) {
		orchestrator, ok := mergeRef.orchestrator()
		if !ok {
			return "", fmt.Errorf("claude-repld: guidance was routed before the merge orchestrator existed")
		}
		facts, known := orchestrator.Facts(ws)
		if !known {
			return "", fmt.Errorf("claude-repld: guidance was routed for %q, which has no merge", ws)
		}
		origin, err := guidanceOrigin(facts.ActiveTab)
		if err != nil {
			return "", err
		}
		return fleet.RouteGuidance(ctx, ws, said, origin)
	}
}

// guidanceOrigin names the repair a parked merge's guidance belongs to. A tab
// the origin vocabulary does not cover is a REFUSAL: mislabeling a prompt's
// origin is exactly what the closed attribution exists to prevent.
func guidanceOrigin(tab string) (conversationv1.PromptOrigin, error) {
	switch tab {
	case merge.TabConflicts:
		return conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, nil
	case merge.TabTests, merge.TabFixes:
		return conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, nil
	default:
		return conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED,
			fmt.Errorf("claude-repld: a parked merge on the %q tab has no prompt origin to route guidance under", tab)
	}
}

// buildJudge builds the interjection classifier: the scripted one under the
// whole stack's fake mode, the vendor-backed one otherwise. The vendor guard
// refuses the real one when vendor calls are forbidden, which is why it is
// handed in rather than checked here.
func buildJudge(contracts envc.Contracts, guard envc.VendorGuard, promptsDir string) (classifier.Judge, error) {
	if contracts.Fake() {
		return classifier.NewFake(), nil
	}
	return classifier.New(guard, "", promptsDir)
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
