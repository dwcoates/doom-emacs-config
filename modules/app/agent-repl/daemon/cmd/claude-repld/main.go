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
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"claude-repld/internal/daemonaddr"
)

// exitFailure is the status a daemon that could not start returns.
const exitFailure = 2

// exitClaimLost is the status an UNFLAGGED second daemon returns: it lost the
// boot-exclusivity claim to the incumbent and exited without disturbing it. It
// is distinct from a failure because nothing went wrong — a daemon was already
// serving, which is what the claim exists to establish.
const exitClaimLost = 3

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
	// selfRepo overrides the daemon's own checkout identity. It is a TEST
	// HOOK: the merge orchestrator keys its two methods on whether a target is
	// the same repository as this, and a test needs to say so explicitly.
	selfRepo string
}

// envStoreSocket is the store socket's environment contract, which the
// -store-socket flag overrides.
const envStoreSocket = "AGENT_REPL_STORE_SOCKET"

// defaultStoreSocket is the store's socket when neither the flag nor the
// environment names one.
const defaultStoreSocket = ".cache/agent-repl/sock/store.sock"

func main() {
	opts, err := parseFlags(os.Args[0], os.Args[1:])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(exitFailure)
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

// parseFlags parses the command line. It is separate from main so the flag set
// is testable without running the daemon.
func parseFlags(program string, args []string) (options, error) {
	var opts options
	fs := flag.NewFlagSet(program, flag.ContinueOnError)
	fs.StringVar(&opts.stateDir, "state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	fs.BoolVar(&opts.fake, "fake", false, "run without a real vendor: shims spawn with --fake and the classifier is scripted")
	fs.StringVar(&opts.joining, "joining", "", "address of the incumbent daemon to take over from")
	fs.StringVar(&opts.pprof, "pprof", "", "opt-in profiling surface: a unix socket path or a loopback host:port (empty is off)")
	fs.StringVar(&opts.webapp, "webapp-dist", "", "webapp dist directory to serve")
	fs.StringVar(&opts.shim, "shim-main", "", "path to the shim's dist/main.js")
	fs.StringVar(&opts.node, "node", "node", "node binary that runs the shim")
	fs.StringVar(&opts.storeSocket, "store-socket", "", "store unix socket, overriding $"+envStoreSocket)
	fs.StringVar(&opts.promptsDir, "prompts-dir", "", "prompts directory the briefs are read from")
	fs.StringVar(&opts.multiRepoConfigDir, "multi-repo-config-dir", "", "account config root for workspaces under the multi-repo root")
	fs.StringVar(&opts.defaultConfigDir, "default-config-dir", "", "account config root for every other workspace")
	fs.DurationVar(&opts.idleCutoff, "idle-cutoff", 0, "how long a session may go unengaged before the idle sweep hibernates it")
	fs.BoolVar(&opts.noBrowser, "no-browser", false, "this daemon has no external browser: OpenExternal answers no_browser_configured")
	fs.StringVar(&opts.selfRepo, "self-repo", "", "override the daemon's own checkout identity (test hook)")
	if err := fs.Parse(args); err != nil {
		return options{}, err
	}
	opts.storeSocket = resolveStoreSocket(opts.storeSocket, os.Getenv(envStoreSocket))
	return opts, nil
}

// resolveStoreSocket applies the store socket's precedence: the flag beats the
// environment, which beats the default under the home directory.
func resolveStoreSocket(flagValue, envValue string) string {
	if flagValue != "" {
		return flagValue
	}
	if envValue != "" {
		return envValue
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return defaultStoreSocket
	}
	return filepath.Join(home, defaultStoreSocket)
}
