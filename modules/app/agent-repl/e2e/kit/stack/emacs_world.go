package stack

import (
	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
	"agentrepl/e2e/kit"
	"agentrepl/logging/buildreport"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// EmacsWorld is one world whose CLIENT IS EMACS.
//
// It differs from NewWorld in exactly one structural way, and that
// difference is the whole point of this layer: the DAEMON is not started
// here. Emacs starts it, through the module's own launcher, when a scenario
// calls EnsureDaemon. Everything else — the store, the sidecar, the built
// binaries, the scripted fake git, the build identity, the vendor-call
// prohibition — is the same machinery NewWorld assembles, reused verbatim.
type EmacsWorld struct {
	Emacs   *Emacs
	Store   *Store
	Sidecar *Sidecar

	// Git is the SCRIPTED fake git every process below inherits. No real
	// git runs anywhere in this suite, per SPEC.md's mocking ruling.
	Git *harness.GitWorld

	// Deploy is the daemon's deploy build (AGENT_REPL_DEPLOY_BUILDER). It
	// stages exactly what this world runs, so a deploy changes nothing unless
	// a scenario stages another build first (Deploy.Stage). No deploy in this
	// layer ever builds for real.
	Deploy *harness.DeployBuilder
	// Services is the service manager's tool a service restart would drive
	// (harness.NewFakeServiceManager: launchctl on macOS, systemctl on
	// Linux), so no deploy here reaches a live service manager.
	Services *harness.Recorder
	// Zettel is the world's real zettel (WithZettel), or nil.
	Zettel *Zettel
}

// EmacsWorldOption tunes one EmacsWorld. The DAEMON in this layer is spawned
// by Emacs, so anything the Go layer states through `harness.Opts` can only
// be stated here as environment the EMACS process carries and its daemon
// child inherits (WithEmacsEnv); the one other option runs a real zettel
// (WithZettel).
type EmacsWorldOption func(*emacsWorldConfig)

// emacsWorldConfig is the accumulated options.
type emacsWorldConfig struct {
	extraEnv []string
	// zettel runs a real zettel beside the daemon (WithZettel).
	zettel bool
}

// WithZettel runs a REAL zettel beside the daemon Emacs spawns, on a socket
// and a graph of the world's own, for a scenario about the chat threads.
func WithZettel() EmacsWorldOption {
	return func(c *emacsWorldConfig) { c.zettel = true }
}

// WithEmacsEnv threads one `KEY=VALUE` into the Emacs process's environment,
// and therefore into the daemon Emacs spawns. It is how a scenario states a
// daemon fact that has no other route in this layer — its merge gate
// (`AGENT_REPL_TEST_ALL_SCRIPT`) being the one scenario 40 needs.
func WithEmacsEnv(key, value string) EmacsWorldOption {
	return func(c *emacsWorldConfig) { c.extraEnv = append(c.extraEnv, key+"="+value) }
}

// NewEmacsWorld assembles the world and brings Emacs up, but not the daemon.
func NewEmacsWorld(t *testing.T, box Sandbox, options ...EmacsWorldOption) *EmacsWorld {
	t.Helper()

	var cfg emacsWorldConfig
	for _, option := range options {
		option(&cfg)
	}

	// THE SLOT COMES FIRST, BEFORE THE PER-RUN BUILDS AND BEFORE THE STORE.
	//
	// Every scenario is parallel, a world per step, and the machine is not
	// unbounded; see emacsParallelSlots. Taking the slot HERE rather than inside StartEmacs
	// matters for two measured reasons:
	//
	//   * The first scenarios of a run also do this suite's one-time builds
	//     -- the esbuild shim bundle and four Go binaries. Under the old
	//     placement those ran BESIDE two booting Emacsen instead of counting
	//     against the budget, and the boots they starved missed Doom's own
	//     3500ms bound in scenarios that had nothing to do with them.
	//   * A scenario blocked on a slot would otherwise already be holding a
	//     running store and sidecar, paying for a world it cannot yet use.
	TakeEmacsSlot(t)

	node := requireNode(t)
	shimMain := requireShimBundle(t)
	sidecarBin := requireSidecarBinary(t)
	lockBin := requireLockBinary(t)
	daemonBin := harness.DaemonBinary(t)
	webappDist := requireWebappDist(t)

	// START ORDER: store, then sidecar, then Emacs (which spawns the daemon,
	// which spawns the shim per session). Cleanups register in that order,
	// so t.Cleanup's LIFO unwind tears down Emacs and its daemon first and
	// leaves nothing writing to a store that has already gone away.
	logsDir := filepath.Join(box.Scratch(), "logs")
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", logsDir, err)
	}

	// The kernel-lock run directory, redirected. `sessionlock.Probe` creates
	// it when it is missing -- nobody can hold a lock in a directory that
	// does not exist -- but it is created here too, so this world's tree is
	// complete before anything runs rather than as a side effect of the first
	// probe. It is settled before the store because the store and the
	// sidecar write their build reports into it, where the daemon's deploy
	// reads them.
	lockDir := filepath.Join(box.Scratch(), "locks")
	if err := os.MkdirAll(lockDir, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", lockDir, err)
	}

	store := startStore(t,
		shortSocketPath(t, "store"),
		filepath.Join(box.Scratch(), "store.db"),
		filepath.Join(logsDir, "store.log"),
		lockDir)

	// ONE spool root, for the same reason NewWorld gives: the fake SDK's own
	// default is the REAL vendor location /tmp/claude-<uid>, so leaving it
	// unset would write into the developer's live vendor spool.
	spoolRoot := filepath.Join(box.Scratch(), "spool")
	if err := os.MkdirAll(spoolRoot, 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", spoolRoot, err)
	}

	// The scripted fake git, installed into a directory that LEADS the Emacs
	// process's PATH so the daemon and every one of its children inherit it.
	// harness.StartDaemon normally does this; Emacs is starting the daemon
	// here, so this layer does it at the Emacs process instead.
	git := harness.World(t)
	fakeBin := filepath.Join(box.Scratch(), "fakebin")
	harness.InstallFakeGit(t, fakeBin)

	// THE DEPLOY'S SEAMS. Emacs starts the daemon here, so nothing states
	// them unless this layer does, and a daemon without them would run the
	// REAL build and drive the REAL service manager on any deploy — a landing on
	// its own checkout or `agent-repl-deploy'. The fake build stages what
	// this world runs: the bundle, the served dist, and copies of the very
	// store and sidecar binaries whose own reports sit in lockDir.
	deployBin := filepath.Join(box.Scratch(), "deploybin")
	deploy := harness.NewFakeDeployBuilder(t, deployBin, harness.DeploySources{
		ShimMain: shimMain, WebappDist: webappDist,
		Store: store.bin, Sidecar: sidecarBin,
	})
	deploy.Stage(harness.DeployCurrent)
	// No world runs a real zettel: its staged placeholder's report is stated
	// here, from this test's own (live) pid, so a deploy restarts no zettel.
	deploy.StatePlaceholderBuilds(t, lockDir, os.Getpid())
	services := harness.NewFakeServiceManager(t, deployBin)

	extraEnv := []string{
		"PATH=" + fakeBin + ":" + os.Getenv("PATH"),
		"AGENT_REPL_LOCK_DIR=" + lockDir,
		// THE LOCK HOLDER. Node cannot take a flock, so the shim spawns
		// shim-lock for each kernel claim and refuses every session without
		// it. Emacs starts the daemon here, so the statement travels as
		// environment from this process, the same way the lock directory does.
		"AGENT_REPL_SHIM_LOCK_BIN=" + lockBin,
		"FAKEGIT_STATE=" + git.StateFile,
		// FAKE MODE, the counterpart of the vendor prohibition StartEmacs
		// sets. `AGENT_REPL_FORBID_VENDOR_CALLS=1` alone makes the daemon
		// REFUSE to spawn a shim ("vendor calls are forbidden: shim-spawn"),
		// because a shim that is not declared fake is a vendor call. The Go
		// worlds say the same thing with `--fake` on the daemon's argv
		// (daemon/integration/harness/daemon.go); here EMACS composes that
		// argv through its own launcher, so the statement has to travel as
		// environment -- which `envc.Load` reads for exactly this purpose.
		// It reaches the real TypeScript shim as `--fake` all the same, so
		// the shim is real and only the SDK behind it is not.
		"AGENT_REPL_FAKE=1",
		"AGENT_REPL_FAKE_SPOOL_ROOT=" + spoolRoot,
		// THE MODULE ROOT IS THE CHECKOUT: the real Emacs loads the elisp
		// beneath it, so a deploy's fresh elisp build is the one Emacs runs.
		// The sandbox's working copy is a throwaway, so what a deploy
		// installs there touches nothing of the host's (see kit.CheckoutEnv).
		kit.CheckoutEnv + "=" + Repo.repoDir,
		"AGENT_REPL_DEPLOY_BUILDER=" + deploy.Path,
		"AGENT_REPL_LAUNCHCTL=" + services.Path,
		"AGENT_REPL_SYSTEMCTL=" + services.Path,
		"AGENT_REPL_LAUNCH_AGENTS_DIR=" + filepath.Join(box.Scratch(), "LaunchAgents"),
		"AGENT_REPL_SYSTEMD_USER_DIR=" + filepath.Join(box.Scratch(), "systemd-user"),
		harness.UnservedZettelEnv(harness.ShortTempDir(t)),
	}
	var zettel *Zettel
	if cfg.zettel {
		zettel = startZettel(t, shortSocketPath(t, "zettel"), filepath.Join(box.Scratch(), "zettel.db"),
			filepath.Join(logsDir, "zettel.log"), lockDir)
		// After the unserved socket above, so this one is the daemon's.
		extraEnv = append(extraEnv, harness.ZettelSocketEnv+"="+zettel.Socket)
	}
	// LAST, so a scenario's own statement is the one the process carries.
	extraEnv = append(extraEnv, cfg.extraEnv...)

	e := StartEmacs(t, box, EmacsOpts{
		DaemonBinary: daemonBin,
		// THE THREE ARTIFACTS THE DAEMON TAKES ON ITS ARGV. Each was
		// previously stated as an environment variable that NOTHING READ
		// (`AGENT_REPL_SHIM_NODE`, `AGENT_REPL_SHIM_MAIN`), so the daemon
		// fell back to its checkout defaults and spawned a shim bundle that
		// does not exist -- "Cannot find module .../shim/dist/main.js".
		// `daemon/integration/harness/daemon.go` states the same three as
		// flags; this layer states them the same way, on the command the
		// launcher then appends its own account-root flags to.
		DaemonArgs: []string{
			"--node", node,
			"--shim-main", shimMain,
			"--webapp-dist", webappDist,
		},
		StoreSocket: store.Socket,
		ExtraEnv:    extraEnv,
	})

	// THE WORLD-LEVEL PATHS A STRAY CAN NAME. The reaper StartEmacs armed
	// matches an argv against this scenario's own paths, and the Emacs root
	// alone does not cover every one of them: a leaked `shim-lock` names the
	// kernel-lock directory, and a leaked fake SDK names the spool root.
	// Both are unique per test, so neither can match another scenario's
	// process. Without them the strays this reaper exists for -- measured at
	// 95 MiB for one leaked shim -- stay resident for the rest of the run.
	e.AddReapPath(lockDir)
	e.AddReapPath(spoolRoot)

	// The sidecar watches the SAME two account roots the launcher was given.
	// SPEC.md §B's "One string per config root" invariant applies here
	// unchanged: the sidecar records cursors under the path it WALKED, which
	// is symlink-resolved, so the roots are resolved before they are handed
	// over or a cursor is never seen to advance.
	sidecar := startSidecar(t, sidecarBin, sidecarOpts{
		StoreSocket: store.Socket,
		ConfigRoots: []string{resolveOrFail(t, e.DefaultConfigDir), resolveOrFail(t, e.MultiRepoConfigDir)},
		SpoolRoot:   spoolRoot,
		LogPath:     filepath.Join(logsDir, "sidecar.log"),
		// THE SAME STATE ROOT EMACS HANDS THE DAEMON. `StartEmacs' exports
		// `AGENT_REPL_STATE_DIR=e.StateDir', so every shim the daemon spawns
		// writes its identity records under it; a sidecar told any other root
		// would book a `/clear'-rotated transcript under a second id, which
		// the store refuses. Empty is refused by `startSidecar' rather than
		// defaulted, because the default is the developer's own
		// ~/.claude-emacs.
		StateDir: e.StateDir,
		LockDir:  lockDir,
	})
	// A deploy reads the sidecar as current only once it has reported.
	reportCtx, cancelReport := grantedclock.WithBound(context.Background(), DefaultTimeout)
	awaitBuildReport(t, reportCtx, lockDir, buildreport.ServiceSidecar, sidecar.cmd.Process.Pid)
	cancelReport()

	// Registered LAST, so LIFO runs it FIRST: it observes what died on its
	// OWN, before this world's own teardown killed anything on purpose.
	t.Cleanup(func() { requireServicesOutlivedTheTest(t, store, sidecar, zettel) })

	// The module's workspace-agnostic elisp log is aimed at THIS scenario's
	// own state root by `writeSettings' (e.LogFile), so it is already inside
	// the state tree dumpArtifacts copies and needs no entry here. It used to
	// be the uid-keyed default under `temporary-file-directory', one file for
	// every Emacs in this container, which interleaved four scenarios'
	// records and rotated them away mid-scenario.
	//
	// Workspace-owned records go to each repo's .claude/emacs, which a test
	// adds once it has a repo.
	e.ArtifactPaths = append(e.ArtifactPaths, git.StateFile, logsDir)
	return &EmacsWorld{Emacs: e, Store: store, Sidecar: sidecar, Git: git, Deploy: deploy, Services: services, Zettel: zettel}
}

// webappDistOnce checks the staged webapp dist at most once per test binary.
var (
	webappDistOnce sync.Once
	webappDistPath string
	webappDistErr  error
)

// webappDistCheckBound bounds the staleness check.
//
// MEASURED: the check is a `stat` walk of webapp/src plus the generated proto
// TypeScript -- about 1,100 files -- and finished in 60ms at its slowest
// inside the container. 30s is three orders above that on purpose: what this
// bound exists to catch is a check that cannot finish AT ALL, and it must
// fail in the test rather than at the suite's own timeout.
const webappDistCheckBound = 30 * time.Second

// requireWebappDist answers the REAL webapp dist the daemon serves, and
// refuses a stale one.
//
// The Go layers hand the daemon a STUB dist (harness.NewFakeWebappDist) and
// that is right for them: no Connect client ever loads the page. This layer
// does. The panel is an `xwidget-webkit` webview pointed at the daemon's own
// origin, so the bytes it renders are the bytes of this dist -- a stub would
// mean every present and future webview assertion inspected a placeholder
// while reporting on the product.
//
// THE DIST IS HOST-STAGED, NOT BUILT HERE. It used to be built by this
// function, inside the container, on the first test of every run: `npm run
// build` is `tsc --noEmit && vite build`, which cost ~6s and about a
// gigabyte of resident memory for tsc alone -- inside a 5.8 GiB Docker VM,
// on a tmpfs, with a real Emacs about to start. `e2e-sandbox.sh run` builds
// it on the host now (bin/webapp-dist.sh `ensure`) and the entrypoint stages
// it in with the working copy, exactly as the shim bundle's build identity
// is staged rather than recomputed.
//
// STALENESS STAYS HONEST, and by the SAME RULE the host used: this runs
// `bin/webapp-dist.sh check`, the one implementation of that rule, against
// the staged sources. A dist older than any source it is built from is a
// FAILURE naming the command to run, never a silently served older bundle.
func requireWebappDist(t *testing.T) string {
	t.Helper()
	webappDistOnce.Do(func() {
		script := filepath.Join(Repo.repoDir, "e2e", "sandbox", "bin", "webapp-dist.sh")
		if _, err := os.Stat(script); err != nil {
			webappDistErr = fmt.Errorf("the webapp dist staleness rule is missing at %s: %w", script, err)
			return
		}
		started := time.Now()
		ctx, cancel := grantedclock.WithBound(context.Background(), webappDistCheckBound)
		defer cancel()
		cmd := exec.CommandContext(ctx, script, "check")
		out, err := cmd.CombinedOutput()
		if err != nil {
			webappDistErr = fmt.Errorf(
				"the staged webapp dist is STALE or absent, and this layer serves it to a REAL webview, "+
					"so it will not run against one:\n%s\n"+
					"Build it on the HOST (the container has no network and must not run tsc/vite):\n"+
					"    modules/app/agent-repl/e2e/sandbox/bin/webapp-dist.sh ensure\n"+
					"`e2e-sandbox.sh run` does this for you unless AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1",
				strings.TrimRight(string(out), "\n"))
			return
		}
		dist := filepath.Join(Repo.repoDir, "webapp", "dist")
		entry := filepath.Join(dist, "index.html")
		if _, err := os.Stat(entry); err != nil {
			webappDistErr = fmt.Errorf("the staleness rule passed but there is no entry point at %s: %w", entry, err)
			return
		}
		webappDistPath = dist
		t.Logf("e2e: the staged webapp dist is fresh (checked in %s): %s",
			time.Since(started).Round(time.Millisecond), dist)
	})
	if webappDistErr != nil {
		t.Fatalf("e2e: %v", webappDistErr)
	}
	return webappDistPath
}

// resolveOrFail resolves a directory's symlinks, per the config-root
// invariant above.
func resolveOrFail(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := kit.ResolvedPath(dir)
	if err != nil {
		t.Fatalf("e2e: resolve the config root %s: %v", dir, err)
	}
	return resolved
}

// elispString renders a Go string as an elisp string literal.
func ElispString(s string) string {
	quoted := make([]rune, 0, len(s)+2)
	quoted = append(quoted, '"')
	for _, r := range s {
		if r == '"' || r == '\\' {
			quoted = append(quoted, '\\')
		}
		quoted = append(quoted, r)
	}
	return string(append(quoted, '"'))
}

// elispStringList renders a Go slice as space-separated elisp string
// literals, for splicing into a `(list ...)` form.
func elispStringList(xs []string) string {
	quoted := make([]string, 0, len(xs))
	for _, x := range xs {
		quoted = append(quoted, ElispString(x))
	}
	return strings.Join(quoted, " ")
}

func DecodeStrings(raw json.RawMessage) []string {
	if IsJSONNull(raw) {
		return nil
	}
	var xs []string
	if err := json.Unmarshal(raw, &xs); err != nil {
		return nil
	}
	return xs
}

// emacsVerbBound bounds ONE workspace verb's full round trip: the unary call
// out of Emacs, the daemon's answer, the roster push back, and Emacs's own
// reconciliation of it into tabs. It is the suite's default because a verb is
// the same shape of work as every other awaited daemon answer here.
const EmacsVerbBound = DefaultTimeout
