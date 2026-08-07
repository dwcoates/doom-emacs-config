package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"io"
	"path/filepath"
	"sort"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/registry"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
)

// listWorkspacesSubcommand is the one-off, runtime-free workspace listing entry
// point: `claude-repld list-workspaces --git-root R`.
//
// It is the read counterpart of `create-workspace`, and it exists for the same
// reason: knowing which workspaces a repository has is useful WITHOUT the
// agent-repl runtime, and re-deriving that list in a shell script would make a
// second authority on a question the daemon already answers.
//
// TWO SOURCES OF TRUTH, each answering the question it owns:
//
//   - git answers "does this worktree exist, and on what branch?". `git -C
//     <root> worktree list --porcelain` is the complete, current set of the
//     repository's worktrees; nothing in the daemon's state can contradict it,
//     because a worktree removed behind the daemon's back is simply gone.
//   - geometry.Store answers "is this worktree an agent-repl WORKSPACE?". A
//     worktree with recorded merge geometry was created by (or backfilled for)
//     agent-repl and can be merged back; one without it is a worktree somebody
//     made by hand, which this command reports only under --include-unmanaged
//     and always marked as such.
//
// A geometry record whose worktree is NOT in the git enumeration is omitted
// from stdout and named on stderr. The listing's consumer cds into the paths it
// prints, so printing a path that no longer exists would hand it a directory it
// cannot enter.
const listWorkspacesSubcommand = "list-workspaces"

// originUnmanaged is the origin column's value for a worktree git knows about
// and the geometry store does not. It is deliberately not a geometry.Origin:
// the store never holds it, and it must not become a third recordable origin.
const originUnmanaged = "unmanaged"

// listWorkspacesOptions is the parsed command line.
type listWorkspacesOptions struct {
	GitRoot          string
	StateDB          string
	IncludeUnmanaged bool
	Verbose          bool
}

// parseListWorkspacesArgs parses the subcommand's flags, checking every
// required value before any git or database work happens.
func parseListWorkspacesArgs(args []string, stderr io.Writer) (listWorkspacesOptions, error) {
	fs := flag.NewFlagSet("claude-repld "+listWorkspacesSubcommand, flag.ContinueOnError)
	fs.SetOutput(stderr)
	var opts listWorkspacesOptions
	fs.StringVar(&opts.GitRoot, "git-root", "", "absolute path (or ~/ path) of the repository's main worktree whose workspaces are listed (required)")
	fs.StringVar(&opts.StateDB, "state-db", "", "path to the daemon state database that holds the merge geometry table (empty = the daemon's default)")
	fs.BoolVar(&opts.IncludeUnmanaged, "include-unmanaged", false, "also list linked worktrees that have NO recorded agent-repl merge geometry, marked with origin \"unmanaged\"")
	fs.BoolVar(&opts.Verbose, "verbose", false, "also emit verbose records to stderr")
	if err := fs.Parse(args); err != nil {
		return listWorkspacesOptions{}, err
	}
	if fs.NArg() != 0 {
		return listWorkspacesOptions{}, fmt.Errorf("unexpected positional argument %q; every input is a flag", fs.Arg(0))
	}
	if opts.GitRoot == "" {
		return listWorkspacesOptions{}, fmt.Errorf("--git-root is required")
	}
	return opts, nil
}

// gitWorktree is one entry of `git worktree list --porcelain`.
type gitWorktree struct {
	// Path is the worktree directory, cleaned into the geometry store's key
	// spelling so it compares equal to a recorded workspace.
	Path string
	// Branch is the short branch name, or "" when the worktree is detached.
	Branch string
	// Bare marks the repository's bare "worktree", which is a git-dir rather
	// than a directory anyone can work (or cd) into.
	Bare bool
}

// parseGitWorktreeList parses `git worktree list --porcelain` output.
//
// The porcelain format is a blank-line-separated stanza per worktree, whose
// first line is always `worktree <path>`; the MAIN worktree is always the first
// stanza, which is what lets the caller drop it without asking git a second
// question.
func parseGitWorktreeList(out string) ([]gitWorktree, error) {
	var (
		list    []gitWorktree
		current *gitWorktree
	)
	scanner := bufio.NewScanner(strings.NewReader(out))
	scanner.Buffer(make([]byte, 0, 64*1024), 4*1024*1024)
	for scanner.Scan() {
		line := scanner.Text()
		switch {
		case strings.HasPrefix(line, "worktree "):
			list = append(list, gitWorktree{Path: filepath.Clean(strings.TrimPrefix(line, "worktree "))})
			current = &list[len(list)-1]
		case current == nil:
			// Any content before the first `worktree` line means this is not
			// the porcelain format at all, and guessing at it would produce a
			// listing with the wrong paths in it.
			return nil, fmt.Errorf("git worktree list --porcelain: unexpected line %q before any worktree stanza", line)
		case strings.HasPrefix(line, "branch refs/heads/"):
			current.Branch = strings.TrimPrefix(line, "branch refs/heads/")
		case line == "bare":
			current.Bare = true
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("git worktree list --porcelain: read output: %w", err)
	}
	if len(list) == 0 {
		return nil, fmt.Errorf("git worktree list --porcelain: no worktree stanza in output")
	}
	return list, nil
}

// workspaceMatchKey is THE spelling both sides of the join are compared on.
//
// git and the geometry store can spell the same worktree differently: git
// reports the symlink-resolved path (on macOS every /var/... path is really
// /private/var/...), while the store holds whatever path the create request
// named. Comparing the raw strings makes the same workspace look like two, and
// the workspace then vanishes from a listing a human treats as exhaustive.
//
// A path that cannot be resolved keeps its cleaned spelling. That is not a
// fallback around a failure: an unresolvable path is a worktree that is not on
// disk, which is exactly the recorded-but-missing case this command reports.
func workspaceMatchKey(path string) string {
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		return geometry.Key(resolved)
	}
	return geometry.Key(path)
}

// workspaceListing is one stdout row.
type workspaceListing struct {
	Name   string
	Path   string
	Branch string
	Origin string
}

// runListWorkspaces enumerates the repository's workspaces and writes them to
// stdout, one per line.
//
// FORMAT: tab-separated, four fields, no header and no quoting:
//
//	<name>\t<absolute-path>\t<branch>\t<origin>
//
// `name` is the worktree directory's base name (the workspace name the create
// planner gave it). `branch` is the short branch name, or "-" when the worktree
// is detached. `origin` is "created", "backfilled", or "unmanaged".
//
// Tab-separated rather than JSON because the consumer is a shell menu, and a
// value can never contain a tab: paths and branch names both come from git,
// which forbids the ASCII control characters outright in ref names and would
// have written a quoted path (which this command refuses, below) otherwise.
func runListWorkspaces(ctx context.Context, args []string, stdout, stderr io.Writer) error {
	opts, err := parseListWorkspacesArgs(args, stderr)
	if err != nil {
		return err
	}
	// Records go to STDERR only: stdout is the machine-readable channel a shell
	// menu reads line by line, so a log line on it would be read as a workspace.
	logger := dlog.New(io.Discard, stderr, opts.Verbose)
	logf := dlog.Legacy(logger.With("operation", listWorkspacesSubcommand))

	root, err := normalizeWorkspacePath(opts.GitRoot)
	if err != nil {
		return fmt.Errorf("--git-root %q: %w", opts.GitRoot, err)
	}

	worktrees, err := repositoryWorktrees(ctx, ExecGitRunner{}, root)
	if err != nil {
		return err
	}

	recorded, statePath, err := recordedGeometry(ctx, opts.StateDB, logf)
	if err != nil {
		return err
	}
	logf("list-workspaces: git_root=%s worktrees=%d geometry_records=%d state_db=%s", root, len(worktrees), len(recorded), statePath)

	rows, err := workspaceRows(worktrees, recorded, opts.IncludeUnmanaged)
	if err != nil {
		return err
	}
	// A recorded workspace git no longer has is reported, never printed: the
	// consumer cds into every path on stdout.
	present := map[string]bool{}
	for _, wt := range worktrees {
		present[workspaceMatchKey(wt.Path)] = true
	}
	missing := make([]string, 0, len(recorded))
	for key := range recorded {
		if !present[key] {
			missing = append(missing, key)
		}
	}
	sort.Strings(missing)
	for _, key := range missing {
		logf("list-workspaces: recorded workspace %s has no worktree under %s — OMITTED from the listing", key, root)
	}

	for _, row := range rows {
		if _, err := fmt.Fprintf(stdout, "%s\t%s\t%s\t%s\n", row.Name, row.Path, row.Branch, row.Origin); err != nil {
			return fmt.Errorf("write workspace listing: %w", err)
		}
	}
	logf("list-workspaces: listed=%d include_unmanaged=%v", len(rows), opts.IncludeUnmanaged)
	return nil
}

// repositoryWorktrees asks git for the repository's worktrees, dropping the
// main worktree (the first stanza) and any bare entry.
func repositoryWorktrees(ctx context.Context, git GitRunner, root string) ([]gitWorktree, error) {
	out, exit, err := git.RunGit(ctx, root, "worktree", "list", "--porcelain")
	if err != nil {
		return nil, fmt.Errorf("git worktree list in %s: %w", root, err)
	}
	if exit != 0 {
		return nil, fmt.Errorf("git worktree list in %s exited %d: %s", root, exit, strings.TrimSpace(out))
	}
	all, err := parseGitWorktreeList(out)
	if err != nil {
		return nil, err
	}
	linked := make([]gitWorktree, 0, len(all))
	for i, wt := range all {
		if i == 0 || wt.Bare {
			// The main worktree is the repository itself, not a workspace, and
			// a bare entry is a git-dir nobody can cd into.
			continue
		}
		linked = append(linked, wt)
	}
	return linked, nil
}

// recordedGeometry opens the shared state store and returns every recorded
// workspace keyed by geometry.Key, plus the database path for the log record.
func recordedGeometry(ctx context.Context, stateDB string, logf func(string, ...any)) (map[string]geometry.Record, string, error) {
	statePath := stateDB
	if statePath == "" {
		var err error
		statePath, err = registry.DefaultDBPath()
		if err != nil {
			return nil, "", fmt.Errorf("resolve state database path: %w", err)
		}
	}
	stateStore, err := statedb.Open(statePath)
	if err != nil {
		return nil, "", fmt.Errorf("open state store %s: %w", statePath, err)
	}
	defer func() {
		if closeErr := stateStore.Close(); closeErr != nil {
			logf("list-workspaces: close state store %s: %v", statePath, closeErr)
		}
	}()
	geometryStore, err := geometry.Open(stateStore, dlog.Logf(logf))
	if err != nil {
		return nil, "", fmt.Errorf("open merge geometry store: %w", err)
	}
	records, err := geometryStore.List(ctx)
	if err != nil {
		return nil, "", err
	}
	byKey := make(map[string]geometry.Record, len(records))
	for _, rec := range records {
		byKey[workspaceMatchKey(rec.Workspace)] = rec
	}
	return byKey, statePath, nil
}

// workspaceRows joins the git enumeration with the recorded geometry, in git's
// own order (which is creation order), and refuses any path that cannot be
// printed as one tab-separated field.
func workspaceRows(worktrees []gitWorktree, recorded map[string]geometry.Record, includeUnmanaged bool) ([]workspaceListing, error) {
	rows := make([]workspaceListing, 0, len(worktrees))
	for _, wt := range worktrees {
		rec, managed := recorded[workspaceMatchKey(wt.Path)]
		if !managed && !includeUnmanaged {
			continue
		}
		origin := originUnmanaged
		if managed {
			origin = string(rec.Origin)
		}
		branch := wt.Branch
		if branch == "" {
			branch = "-"
		}
		row := workspaceListing{Name: filepath.Base(wt.Path), Path: wt.Path, Branch: branch, Origin: origin}
		if strings.ContainsAny(row.Name+row.Path+row.Branch, "\t\n") {
			// The row format has no escape, so an unprintable row is a hard
			// failure rather than a line the consumer would silently mis-split.
			return nil, fmt.Errorf("workspace %s has a tab or newline in its path or branch, which the tab-separated listing cannot express", wt.Path)
		}
		rows = append(rows, row)
	}
	return rows, nil
}
