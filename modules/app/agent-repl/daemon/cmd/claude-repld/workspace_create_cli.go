package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"os"

	"claude-repld/internal/dlog"
	"claude-repld/internal/registry"
	"claude-repld/internal/statedb"
	workspacecreate "claude-repld/internal/workspace/create"
	"claude-repld/internal/workspace/geometry"
)

// createWorkspaceSubcommand is the one-off, runtime-free workspace creation
// entry point: `claude-repld create-workspace --git-root R --name N`.
//
// It exists because the worktree half of workspace creation is useful WITHOUT
// the agent-repl runtime. Everything after that half — the agent-repl session,
// the shim health gate, the Emacs materialization handshake, initial-prompt
// delivery — needs a live daemon, a live frontend, and a live editor, and this
// command deliberately runs none of them. What it does run is the SAME code the
// daemon runs (create.WorktreeStage over the same DaemonWorktree planner and
// the same geometry recorder), so a workspace made here is indistinguishable
// from a daemon-made one at the git and merge-geometry level.
const createWorkspaceSubcommand = "create-workspace"

// createWorkspaceOptions is the parsed command line.
type createWorkspaceOptions struct {
	GitRoot    string
	Name       string
	BaseCommit string
	StateDB    string
	Verbose    bool
}

// parseCreateWorkspaceArgs parses the subcommand's flags. Every required value
// is checked here so a missing one fails before any git or database work.
func parseCreateWorkspaceArgs(args []string, stderr io.Writer) (createWorkspaceOptions, error) {
	fs := flag.NewFlagSet("claude-repld "+createWorkspaceSubcommand, flag.ContinueOnError)
	fs.SetOutput(stderr)
	var opts createWorkspaceOptions
	fs.StringVar(&opts.GitRoot, "git-root", "", "absolute path (or ~/ path) of the repository's main worktree the new workspace is cut from (required)")
	fs.StringVar(&opts.Name, "name", "", "requested workspace name; the branch and worktree directory take it, with a collision suffix when it is taken (required)")
	fs.StringVar(&opts.BaseCommit, "base-commit", "HEAD", "commit-ish the workspace is cut from, resolved in --git-root. \"master\" additionally fetches origin/master and fast-forwards the local master first, exactly as the daemon does")
	fs.StringVar(&opts.StateDB, "state-db", "", "path to the daemon state database that holds the merge geometry table (empty = the daemon's default)")
	fs.BoolVar(&opts.Verbose, "verbose", false, "also emit verbose records to stderr")
	if err := fs.Parse(args); err != nil {
		return createWorkspaceOptions{}, err
	}
	if fs.NArg() != 0 {
		return createWorkspaceOptions{}, fmt.Errorf("unexpected positional argument %q; every input is a flag", fs.Arg(0))
	}
	if opts.GitRoot == "" {
		return createWorkspaceOptions{}, fmt.Errorf("--git-root is required")
	}
	if opts.Name == "" {
		return createWorkspaceOptions{}, fmt.Errorf("--name is required")
	}
	if opts.BaseCommit == "" {
		return createWorkspaceOptions{}, fmt.Errorf("--base-commit cannot be empty")
	}
	return opts, nil
}

// runCreateWorkspace performs the one-off creation and writes the created
// worktree's absolute path to stdout as the last line.
//
// JOB PERSISTENCE IS BYPASSED ENTIRELY, and that is the whole safety argument
// against corrupting a running daemon. The durable create store
// (workspace-create-jobs.json) is the daemon's own state machine, whose every
// record is a promise that a session, a health check, an Emacs handshake and
// possibly a prompt are still owed. A one-off run owes none of them, so writing
// a record there would leave a job the daemon's boot resume would pick up and
// try to drive to `ready` — creating a session for a workspace nobody asked for
// a session for. This command therefore never opens that file: the job value
// below lives only in this process, and `checkpoint` just returns it.
//
// The merge-geometry table IS written, because it is not a promise — it is the
// three coordinates that make the workspace mergeable at all, and a workspace
// created without them is one the daemon must later refuse to merge. It lives
// in the shared SQLite state store, which is opened WAL with a busy timeout, so
// a concurrent daemon and this process are two ordinary writers rather than a
// corruption risk.
func runCreateWorkspace(ctx context.Context, args []string, stdout, stderr io.Writer) error {
	opts, err := parseCreateWorkspaceArgs(args, stderr)
	if err != nil {
		return err
	}
	// Records go to STDERR only: stdout is the machine-readable channel whose
	// last line the wrapper script cds into, so a log line on it would be read
	// as a path. io.Discard is the durable sink because this process is not the
	// daemon and must not append to (or rotate) the daemon's own run log.
	logger := dlog.New(io.Discard, stderr, opts.Verbose)
	logf := dlog.Legacy(logger.With("operation", createWorkspaceSubcommand))

	statePath := opts.StateDB
	if statePath == "" {
		statePath, err = registry.DefaultDBPath()
		if err != nil {
			return fmt.Errorf("resolve state database path: %w", err)
		}
	}
	stateStore, err := statedb.Open(statePath)
	if err != nil {
		return fmt.Errorf("open state store %s: %w", statePath, err)
	}
	defer func() {
		if closeErr := stateStore.Close(); closeErr != nil {
			logf("workspace-create: close state store %s: %v", statePath, closeErr)
		}
	}()
	geometryStore, err := geometry.Open(stateStore, dlog.Logf(logf))
	if err != nil {
		return fmt.Errorf("open merge geometry store: %w", err)
	}

	jobID, err := createWorkspaceJobID(opts.Name)
	if err != nil {
		return err
	}
	job := workspacecreate.Job{
		ID:          jobID,
		SourceFile:  createWorkspaceSubcommand,
		SourceIndex: 0,
		State:       workspacecreate.StateWorktreeCreating,
		Request: workspacecreate.Request{
			Name:       opts.Name,
			GitRoot:    opts.GitRoot,
			BaseCommit: opts.BaseCommit,
		},
	}
	logf("workspace-create: one-off create STARTED job=%s name=%q git_root=%s base=%q state_db=%s", job.ID, opts.Name, opts.GitRoot, opts.BaseCommit, statePath)

	// The production adapters, unmodified. Registry is nil because a one-off
	// create has no --fork-from: the planner consults the registry only to
	// resolve a fork source, and refuses loudly rather than silently if one is
	// ever asked for without it.
	worktrees := DaemonWorktree{Git: ExecGitRunner{}, Marker: osProjectileMarker{}, Logf: logf}
	stage := workspacecreate.WorktreeStage{
		Planner:   worktrees,
		Worktrees: worktrees,
		Geometry:  daemonGeometryRecorder{Store: geometryStore, Logf: logf},
		Logf:      logf,
	}
	created, err := stage.Materialize(ctx, job, func(_ context.Context, result workspacecreate.WorktreeResult) (workspacecreate.Job, error) {
		job.WorktreePath = result.Path
		job.FinalName = result.FinalName
		job.Branch = result.Branch
		job.ResolvedBaseCommit = result.BaseCommit
		job.Request.ForkSessionID = result.ForkSessionID
		return job, nil
	})
	if err != nil {
		return err
	}
	logf("workspace-create: one-off create COMPLETE job=%s name=%q branch=%q path=%s base=%s session=none emacs=none", created.ID, created.FinalName, created.Branch, created.WorktreePath, created.ResolvedBaseCommit)
	if _, err := fmt.Fprintln(stdout, created.WorktreePath); err != nil {
		return fmt.Errorf("write created worktree path: %w", err)
	}
	return nil
}

// createWorkspaceJobID mints this run's job identity. It is never persisted;
// it names the run in the log records and seeds the planner's collision suffix,
// so it must be unique per run rather than derived from the requested name.
func createWorkspaceJobID(name string) (string, error) {
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		return "", fmt.Errorf("mint one-off create job id: %w", err)
	}
	return createWorkspaceSubcommand + ":" + name + ":" + hex.EncodeToString(raw[:]), nil
}

// oneOffSubcommands is THE roster of runtime-free subcommands, so adding one is
// a table entry rather than another branch in dispatchSubcommand. Every entry
// obeys the same two rules: stdout carries only machine-readable output, and no
// entry starts a listener, a session controller, or a shim.
var oneOffSubcommands = map[string]func(context.Context, []string, io.Writer, io.Writer) error{
	createWorkspaceSubcommand: runCreateWorkspace,
	listWorkspacesSubcommand:  runListWorkspaces,
}

// dispatchSubcommand runs a one-off subcommand when argv names one, reporting
// whether it handled the invocation and the exit code it wants. The daemon
// runtime starts only when it reports false, so a one-off subcommand never
// boots a listener, a session controller, or a shim.
func dispatchSubcommand(ctx context.Context, argv []string, stdout, stderr io.Writer) (handled bool, code int) {
	if len(argv) < 2 {
		return false, 0
	}
	run, ok := oneOffSubcommands[argv[1]]
	if !ok {
		return false, 0
	}
	if err := run(ctx, argv[2:], stdout, stderr); err != nil {
		// This is the documented pre-logger boundary: the failure must reach
		// the operator's terminal even when it happened before (or while
		// building) the command's own logger.
		fmt.Fprintf(stderr, "claude-repld %s: %v\n", argv[1], err)
		if err == flag.ErrHelp {
			return true, 2
		}
		return true, 1
	}
	return true, 0
}

// runSubcommandOrExit is main's first act. It is a separate function so main's
// daemon bootstrap can assume it is running the daemon.
func runSubcommandOrExit(ctx context.Context) {
	if handled, code := dispatchSubcommand(ctx, os.Args, os.Stdout, os.Stderr); handled {
		os.Exit(code)
	}
}
