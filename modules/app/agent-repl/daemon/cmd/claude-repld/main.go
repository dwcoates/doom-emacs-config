// Command claude-repld is the agent-repl daemon.
//
// Boot only: flags, the environment contracts, pprof (before any dependency is
// built), the log surfaces, the state root, the address claim and daemon.addr,
// the WSM open, joining mode, wiring the graph, serving, and an orderly exit.
// No policy lives here.
//
// NO ARGV IS REQUIRED: Emacs launches the binary with none and the state
// travels in the environment. Every flag below is an override, and -joining is
// what a successor is spawned with.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"

	"claude-repld/internal/commandfile"
	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/dirpath"
	"claude-repld/internal/envc"
	"claude-repld/internal/rollout"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// exitSuccess is the status of a run that ended with nothing wrong -- and of a
// -probe-boot-claim run that found the claim FREE.
const exitSuccess = 0

// exitFailure is the status a daemon that could not start returns.
const exitFailure = 2

// exitClaimLost is the status an UNFLAGGED second daemon returns: it lost the
// boot-exclusivity claim to the incumbent and exited without disturbing it. It
// is distinct from a failure because nothing went wrong — a daemon was already
// serving, which is what the claim exists to establish.
const exitClaimLost = 3

// probeBootClaim answers whether this state root's boot claim is held.
//
// It resolves the state root exactly as the boot does -- the same flag, the
// same environment contract -- so the claim it asks about is the claim a
// daemon booting here would race for, and never a different directory's.
func probeBootClaim(opts options) error {
	contracts := envc.Load().WithStateDir(opts.stateDir)
	layout, err := stateroot.Root(opts.stateDir, contracts.StateDir())
	if err != nil {
		return fmt.Errorf("claude-repld: resolve the state root: %w", err)
	}
	return daemonaddr.ProbeBootClaim(layout.DaemonAddr())
}

// options is the parsed command line. Every field is an override of an
// environment contract or of a path the daemon would otherwise derive.
type options struct {
	// stateDir overrides $AGENT_REPL_STATE_DIR.
	stateDir string
	// fake overrides $AGENT_REPL_FAKE: spawn shims with --fake and use the
	// classifier's scripted heuristic.
	fake bool
	// joining is the incumbent daemon's address. Set, this process is a
	// joining successor: it takes ownership workspace by workspace and
	// publishes daemon.addr only once it owns every one.
	joining string
	// pprof is the OPT-IN profiling surface: a unix socket path or an
	// explicitly loopback host:port. Empty is OFF and is the default.
	pprof string
	// webapp is the webapp dist directory served on the daemon's one origin.
	webapp string
	// shim is agent-shim/claude/shim/dist/main.js.
	shim string
	// node is the node binary that runs the shim.
	node string
	// storeSocket is the store's unix socket. This flag BEATS
	// $AGENT_REPL_STORE_SOCKET, which in turn beats the default.
	storeSocket string
	// promptsDir is the prompts/ directory the briefs are read from at use
	// time.
	promptsDir string
	// multiRepoConfigDir is the account config root for workspaces under
	// $MULTI_REPO_ROOT.
	multiRepoConfigDir string
	// defaultConfigDir is the account config root for every other workspace.
	defaultConfigDir string
	// idleCutoff is how long a session may go unengaged before the idle sweep
	// hibernates it.
	idleCutoff time.Duration
	// noBrowser states that this daemon has NO external browser configured.
	// It is the operator's explicit spelling of the condition
	// OpenExternalError.no_browser_configured describes; without it the graph
	// only reaches that state on a host where neither
	// $AGENT_REPL_BROWSER_CMD nor the pinned default binary exists.
	noBrowser bool
	// feedTailRetention is how many published rows one feed retains for a
	// tail's replay. It is what makes WatchFeed's `token_expired` refusal
	// reachable at all: below it a re-opened tail's pinned start is still in
	// the log, above it the token is expired and the client must re-open.
	feedTailRetention int
	// footerMomentaryDwell is how long a MOMENTARY footer status
	// (`interrupted`, `loading`) stands before the daemon's own successor push
	// retires it. It is a real product window — the reader has to be able to
	// see the status — and therefore a real wait for anything that observes
	// its retirement, which is why it is overridable at all. Zero leaves
	// footer.DefaultMomentaryDwell in force.
	footerMomentaryDwell time.Duration
	// selfRepo overrides the daemon's own checkout identity. It is a TEST
	// HOOK: the merge orchestrator keys its two methods on whether a target is
	// the same repository as this, and a test needs to say so explicitly.
	selfRepo string
	// probeBootClaim asks ONE question and starts no daemon: is the boot claim
	// of this state root held right now? It is how Emacs watches a daemon
	// depart -- the claim, not daemon.addr, is what "the previous daemon is
	// gone" means, because the address is withdrawn at the start of a shutdown
	// and the claim is released only when the process ends.
	probeBootClaim bool
	// layoutVersion asks ONE question and starts no daemon: which state
	// layout does this binary write? A deploy asks it of the STAGED binary
	// before it decides between a handover and a restart, because a joining
	// successor carries an older layout forward by additive steps alone
	// (wsm.OpenJoining; see rollout.Controller.Restart).
	layoutVersion bool
	// migrationKindFrom asks ONE question and starts no daemon: what do the
	// migration steps from this layout up to the binary's own mean for the
	// build that wrote it -- additive or breaking? A deploy asks it of the
	// STAGED binary when the layouts differ: an additive chain is handed over
	// (the joining successor applies it, wsm.OpenJoining), a breaking one is
	// restarted across. Zero is unset.
	migrationKindFrom int
	// replacing marks a daemon spawned by an incumbent RESTARTING across a
	// state layout change: it waits for the incumbent's boot claim for
	// rollout.ReplacementClaimWait rather than the ordinary bound, because the
	// incumbent spawns it just before its own orderly exit and that exit --
	// streams closed, loops joined, the state handle released -- is what
	// frees the claim.
	replacing bool
	// inherited is the CONFIGURATION argv every daemon this one spawns
	// inherits (newFlagSets): each configuration flag that was set, as parsed,
	// and no boot flag.
	inherited []string
}

// envStoreSocket is the store socket's environment contract, which the
// -store-socket flag overrides.
const envStoreSocket = "AGENT_REPL_STORE_SOCKET"

// defaultStoreSocket is the store's socket when neither the flag nor the
// environment names one.
const defaultStoreSocket = "~/.cache/agent-repl/sock/store.sock"

func main() {
	if len(os.Args) > 1 && os.Args[1] == deployVerb {
		// THE VERB STARTS NOTHING: it asks the serving daemon to deploy and
		// prints the daemon's decisions.
		ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
		code := runDeployVerb(ctx, os.Args[2:], dialDaemon, os.Stdout, os.Stderr)
		stop()
		os.Exit(code)
	}
	if len(os.Args) > 1 && os.Args[1] == mergeQueueVerb {
		// THE VERB STARTS NOTHING: it drops a command file for the serving
		// daemon and reads the outcome off its roster.
		ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
		code := runMergeQueueVerb(ctx, os.Args[2:], dialMergeQueueDaemon,
			mergeQueueEnv{getwd: os.Getwd, poll: commandfile.DefaultInterval}, os.Stdout, os.Stderr)
		stop()
		os.Exit(code)
	}
	opts, err := parseFlags(os.Args[0], os.Args[1:])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(exitFailure)
	}
	if opts.layoutVersion {
		// THE ANSWER IS THE BINARY'S OWN, and it starts nothing: no state is
		// opened and nothing is logged.
		fmt.Fprintln(os.Stdout, wsm.LayoutVersion)
		os.Exit(exitSuccess)
	}
	if opts.migrationKindFrom != 0 {
		// THE ANSWER IS THE BINARY'S OWN migration list, and it starts
		// nothing: no state is opened and nothing is logged.
		os.Exit(answerMigrationKind(opts.migrationKindFrom, os.Stdout, os.Stderr))
	}
	if opts.probeBootClaim {
		// THE PROBE STARTS NOTHING. No log surfaces, no state root creation, no
		// signal handling: it opens the lock file, asks the kernel, and answers
		// in its exit status.
		switch err := probeBootClaim(opts); {
		case err == nil:
			os.Exit(exitSuccess)
		case errors.Is(err, daemonaddr.ErrClaimed):
			fmt.Fprintln(os.Stderr, err)
			os.Exit(exitClaimLost)
		default:
			// AN UNDECIDED CLAIM IS NOT A FREE ONE, and it must not be read as
			// one. exitFailure is the answer, and the reason is on stderr.
			fmt.Fprintln(os.Stderr, err)
			os.Exit(exitFailure)
		}
	}
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()
	switch err := run(ctx, opts, productionHooks()); {
	case err == nil:
	case errors.Is(err, daemonaddr.ErrClaimed):
		fmt.Fprintln(os.Stderr, err)
		os.Exit(exitClaimLost)
	default:
		fmt.Fprintln(os.Stderr, err)
		os.Exit(exitFailure)
	}
}

// newFlagSets declares every flag the daemon takes, bound to opts, in exactly
// one of two sets.
//
// CONFIGURATION is what this daemon IS: every daemon it spawns (a handover's
// successor, a layout restart's replacement) inherits each one that was set,
// because a spawn assembled from a curated list would differ from its
// incumbent in exactly the ways nobody thought to list.
//
// BOOT flags say how THIS process starts -- the role it boots in, or the one
// question it answers before exiting -- and no spawned daemon inherits one:
// the spawn appends its own role. A new flag is sorted by where it is
// declared, so a boot flag cannot leak into a spawn by being left off a strip
// list (the 2026-09-30 handover failure: `--replacing` inherited beside
// `--joining`).
func newFlagSets(program string, opts *options) (config, boot *flag.FlagSet) {
	config = flag.NewFlagSet(program, flag.ContinueOnError)
	config.StringVar(&opts.stateDir, "state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	config.BoolVar(&opts.fake, "fake", false, "run without a real vendor: shims spawn with --fake and the classifier is scripted")
	config.StringVar(&opts.pprof, "pprof", "", "opt-in profiling surface: a unix socket path or a loopback host:port (empty is off)")
	config.StringVar(&opts.webapp, "webapp-dist", "", "webapp dist directory to serve")
	config.StringVar(&opts.shim, "shim-main", "", "path to the shim's dist/main.js")
	config.StringVar(&opts.node, "node", "node", "node binary that runs the shim")
	config.StringVar(&opts.storeSocket, "store-socket", "", "store unix socket, overriding $"+envStoreSocket)
	config.StringVar(&opts.promptsDir, "prompts-dir", "", "prompts directory the briefs are read from")
	config.StringVar(&opts.multiRepoConfigDir, "multi-repo-config-dir", "", "account config root for workspaces under the multi-repo root")
	config.StringVar(&opts.defaultConfigDir, "default-config-dir", "", "account config root for every other workspace")
	config.DurationVar(&opts.idleCutoff, "idle-cutoff", 0, "how long a session may go unengaged before the idle sweep hibernates it")
	config.IntVar(&opts.feedTailRetention, "feed-tail-retention", 0, "how many published rows one feed retains for a tail's replay (0 uses the built-in default)")
	config.DurationVar(&opts.footerMomentaryDwell, "footer-momentary-dwell", 0, "how long a momentary footer status stands before its successor push retires it (0 uses the built-in default)")
	config.BoolVar(&opts.noBrowser, "no-browser", false, "this daemon has no external browser: OpenExternal answers no_browser_configured")
	config.StringVar(&opts.selfRepo, "self-repo", "", "override the daemon's own checkout identity (test hook)")

	boot = flag.NewFlagSet(program, flag.ContinueOnError)
	boot.StringVar(&opts.joining, "joining", "", "address of the incumbent daemon to take over from")
	boot.BoolVar(&opts.probeBootClaim, "probe-boot-claim", false, "report whether this state root's boot claim is held and exit: 0 free, 3 held, 2 undecided")
	boot.BoolVar(&opts.layoutVersion, rollout.LayoutVersionFlagName, false, "print the state layout version this binary writes and exit")
	boot.IntVar(&opts.migrationKindFrom, rollout.MigrationKindFromFlagName, 0, "print whether the migrations from this state layout to the binary's own are additive or breaking and exit")
	boot.BoolVar(&opts.replacing, rollout.ReplacingFlagName, false, "this daemon replaces an incumbent restarting across a state layout change: wait longer for its boot claim")
	return config, boot
}

// parseFlags parses the command line. It is separate from main so the flag set
// is testable without running the daemon.
//
// Both sets of newFlagSets parse as ONE command line, and opts.inherited is
// what a spawned daemon inherits: every CONFIGURATION flag that was set, as
// `--name=value` from its parsed value, and never a boot flag.
func parseFlags(program string, args []string) (options, error) {
	var opts options
	config, boot := newFlagSets(program, &opts)
	fs := flag.NewFlagSet(program, flag.ContinueOnError)
	for _, set := range []*flag.FlagSet{config, boot} {
		set.VisitAll(func(f *flag.Flag) { fs.Var(f.Value, f.Name, f.Usage) })
	}
	if err := fs.Parse(args); err != nil {
		return options{}, err
	}
	opts.inherited = []string{}
	fs.Visit(func(f *flag.Flag) {
		if config.Lookup(f.Name) != nil {
			opts.inherited = append(opts.inherited, "--"+f.Name+"="+f.Value.String())
		}
	})
	if opts.replacing && opts.joining != "" {
		return options{}, fmt.Errorf("%s: -%s and -joining are exclusive: a replacement boots as the incumbent, a successor joins one", program, rollout.ReplacingFlagName)
	}
	storeSocket, err := resolveStoreSocket(opts.storeSocket, os.Getenv(envStoreSocket), os.UserHomeDir)
	if err != nil {
		return options{}, err
	}
	opts.storeSocket = storeSocket
	retention, err := resolveFeedTailRetention(opts.feedTailRetention, os.Getenv(envFeedTailRetention))
	if err != nil {
		return options{}, err
	}
	opts.feedTailRetention = retention
	dwell, err := resolveFooterMomentaryDwell(opts.footerMomentaryDwell, os.Getenv(envFooterMomentaryDwell))
	if err != nil {
		return options{}, err
	}
	opts.footerMomentaryDwell = dwell
	return opts, nil
}

// envFooterMomentaryDwell is the footer dwell's test knob. It BEATS the flag,
// exactly as the feed tail retention's does: a test that sets it must not also
// have to know how the daemon was launched.
const envFooterMomentaryDwell = "AGENT_REPL_FOOTER_MOMENTARY_DWELL"

// resolveFooterMomentaryDwell applies the dwell's precedence: the environment
// beats the flag, and zero means the footer resolver's own default.
//
// A MALFORMED OR NON-POSITIVE VALUE IS A REFUSAL, never a fall-through: a knob
// that silently did nothing would make the suite it was set for lie about how
// long the daemon actually held the status.
func resolveFooterMomentaryDwell(flagValue time.Duration, envValue string) (time.Duration, error) {
	if envValue != "" {
		return parsePositiveDuration(envFooterMomentaryDwell, envValue)
	}
	if flagValue < 0 {
		return 0, fmt.Errorf("claude-repld: -footer-momentary-dwell=%s is not a positive duration", flagValue)
	}
	return flagValue, nil
}

// envFeedTailRetention is the feed tail retention's test knob. It BEATS the
// flag: a test that sets it must not also have to know how the daemon was
// launched.
const envFeedTailRetention = "AGENT_REPL_FEED_TAIL_RETENTION"

// resolveFeedTailRetention applies the retention's precedence: the environment
// beats the flag, and zero means the resolver's own default.
//
// A MALFORMED OR NON-POSITIVE VALUE IS A REFUSAL, never a fall-through: a test
// knob that silently did nothing would make the suite it was set for lie.
func resolveFeedTailRetention(flagValue int, envValue string) (int, error) {
	if envValue != "" {
		rows, err := strconv.Atoi(envValue)
		if err != nil {
			return 0, fmt.Errorf("claude-repld: %s=%q is not a whole number of rows: %w", envFeedTailRetention, envValue, err)
		}
		if rows <= 0 {
			return 0, fmt.Errorf("claude-repld: %s=%q is not a positive number of rows", envFeedTailRetention, envValue)
		}
		return rows, nil
	}
	if flagValue < 0 {
		return 0, fmt.Errorf("claude-repld: -feed-tail-retention=%d is not a positive number of rows", flagValue)
	}
	return flagValue, nil
}

// resolveStoreSocket applies the store socket's precedence: the flag beats the
// environment, which beats the default under the home directory. Whichever
// wins goes through dirpath.Absolute, so a `~` is expanded and a relative
// socket is refused. The home directory is asked for only when the winner
// needs it, and a daemon that cannot name it then does not boot: it used to
// fall back to the bare relative default, a socket nobody listens on.
func resolveStoreSocket(flagValue, envValue string, userHome func() (string, error)) (string, error) {
	chosen := firstNonEmpty(flagValue, envValue, defaultStoreSocket)
	home := ""
	if strings.HasPrefix(chosen, "~") {
		h, err := userHome()
		if err != nil {
			return "", fmt.Errorf("claude-repld: resolve the home directory the store socket %q is under: %w", chosen, err)
		}
		home = h
	}
	socket, err := dirpath.Absolute(chosen, home)
	if err != nil {
		return "", fmt.Errorf("claude-repld: resolve the store socket: %w", err)
	}
	return socket, nil
}

// answerMigrationKind prints what the migration steps from a running layout up
// to this binary's own mean for the build that wrote it, and answers the exit
// status: success with `additive` or `breaking` on stdout, failure with the
// reason on stderr when no chain reaches this binary's layout.
func answerMigrationKind(from int, stdout, stderr io.Writer) int {
	kind, err := wsm.ChainKind(from)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return exitFailure
	}
	fmt.Fprintln(stdout, kind)
	return exitSuccess
}
