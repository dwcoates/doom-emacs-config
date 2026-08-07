package main

import (
	"bytes"
	"context"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
)

// listStateDB returns a throwaway state database path carrying the geometry
// records the caller names, recorded through the production store.
func listStateDB(t *testing.T, records ...geometry.Record) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	defer func() { _ = db.Close() }()
	store, err := geometry.Open(db, dlog.Logf(func(string, ...any) {}))
	if err != nil {
		t.Fatalf("geometry.Open: %v", err)
	}
	for _, rec := range records {
		if err := store.Record(context.Background(), rec); err != nil {
			t.Fatalf("record %s: %v", rec.Workspace, err)
		}
	}
	return path
}

// listWorktree adds a linked worktree on its own branch and returns its path.
func listWorktree(t *testing.T, repo, name string) string {
	t.Helper()
	path := filepath.Join(cliWorktreeParent(repo), name)
	cliGit(t, repo, "worktree", "add", "--quiet", "-b", name, path, "HEAD")
	return path
}

func TestParseListWorkspacesArgsAcceptsTheCompleteCommandLine(t *testing.T) {
	// Arrange.
	args := []string{"--git-root", "/repo", "--state-db", "/tmp/state.db", "--include-unmanaged", "--verbose"}

	// Act.
	opts, err := parseListWorkspacesArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseListWorkspacesArgs: %v", err)
	}
	want := listWorkspacesOptions{GitRoot: "/repo", StateDB: "/tmp/state.db", IncludeUnmanaged: true, Verbose: true}
	if opts != want {
		t.Fatalf("options = %+v, want %+v", opts, want)
	}
}

func TestParseListWorkspacesArgsDefaultsToManagedWorkspacesOnly(t *testing.T) {
	// Arrange.
	args := []string{"--git-root", "/repo"}

	// Act.
	opts, err := parseListWorkspacesArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseListWorkspacesArgs: %v", err)
	}
	if opts.IncludeUnmanaged {
		t.Fatal("include-unmanaged defaulted to true, want false")
	}
}

func TestParseListWorkspacesArgsRejectsIncompleteCommandLines(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "no git root", args: []string{"--state-db", "/tmp/x.db"}, want: "--git-root is required"},
		{name: "positional argument", args: []string{"--git-root", "/repo", "extra"}, want: `unexpected positional argument "extra"`},
		{name: "unknown flag", args: []string{"--git-root", "/repo", "--name", "x"}, want: "flag provided but not defined"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			stderr := new(bytes.Buffer)

			// Act.
			_, err := parseListWorkspacesArgs(tc.args, stderr)

			// Assert.
			if err == nil {
				t.Fatalf("parseListWorkspacesArgs(%v) succeeded, want an error", tc.args)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want it to contain %q", err, tc.want)
			}
		})
	}
}

func TestParseGitWorktreeListReadsEveryStanzaShape(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want []gitWorktree
	}{
		{
			name: "main worktree on a branch",
			out:  "worktree /repo\nHEAD abc\nbranch refs/heads/master\n\n",
			want: []gitWorktree{{Path: "/repo", Branch: "master"}},
		},
		{
			name: "detached linked worktree",
			out:  "worktree /repo\nbranch refs/heads/master\n\nworktree /repo-worktrees/w\nHEAD abc\ndetached\n\n",
			want: []gitWorktree{{Path: "/repo", Branch: "master"}, {Path: "/repo-worktrees/w"}},
		},
		{
			name: "bare repository",
			out:  "worktree /repo.git\nbare\n\n",
			want: []gitWorktree{{Path: "/repo.git", Bare: true}},
		},
		{
			name: "path is cleaned into the geometry key spelling",
			out:  "worktree /repo//sub/.\nbranch refs/heads/master\n\n",
			want: []gitWorktree{{Path: "/repo/sub", Branch: "master"}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := parseGitWorktreeList(tc.out)

			// Assert.
			if err != nil {
				t.Fatalf("parseGitWorktreeList: %v", err)
			}
			if len(got) != len(tc.want) {
				t.Fatalf("worktrees = %+v, want %+v", got, tc.want)
			}
			for i := range tc.want {
				if got[i] != tc.want[i] {
					t.Fatalf("worktrees[%d] = %+v, want %+v", i, got[i], tc.want[i])
				}
			}
		})
	}
}

func TestParseGitWorktreeListRejectsOutputWithNoWorktreeStanza(t *testing.T) {
	// Arrange.
	out := ""

	// Act.
	_, err := parseGitWorktreeList(out)

	// Assert.
	if err == nil {
		t.Fatal("parseGitWorktreeList(\"\") succeeded, want an error")
	}
	if !strings.Contains(err.Error(), "no worktree stanza") {
		t.Fatalf("error = %v, want it to name the missing stanza", err)
	}
}

func TestParseGitWorktreeListRejectsContentBeforeTheFirstStanza(t *testing.T) {
	// Arrange.
	out := "branch refs/heads/master\nworktree /repo\n"

	// Act.
	_, err := parseGitWorktreeList(out)

	// Assert.
	if err == nil {
		t.Fatal("parseGitWorktreeList succeeded on malformed output, want an error")
	}
	if !strings.Contains(err.Error(), "before any worktree stanza") {
		t.Fatalf("error = %v, want it to name the misplaced line", err)
	}
}

func TestWorkspaceRowsJoinsGitAndGeometry(t *testing.T) {
	managed := geometry.Record{Workspace: "/wt/alpha", SourceBranch: "alpha", SourceDir: "/wt/alpha", TargetDir: "/repo", Origin: geometry.OriginCreated}
	backfilled := geometry.Record{Workspace: "/wt/beta", SourceBranch: "beta", SourceDir: "/wt/beta", TargetDir: "/repo", Origin: geometry.OriginBackfilled}
	recorded := map[string]geometry.Record{"/wt/alpha": managed, "/wt/beta": backfilled}
	worktrees := []gitWorktree{
		{Path: "/wt/alpha", Branch: "alpha"},
		{Path: "/wt/beta", Branch: "beta"},
		{Path: "/wt/handmade", Branch: "handmade"},
		{Path: "/wt/detached"},
	}
	tests := []struct {
		name             string
		includeUnmanaged bool
		want             []workspaceListing
	}{
		{
			name: "managed only by default",
			want: []workspaceListing{
				{Name: "alpha", Path: "/wt/alpha", Branch: "alpha", Origin: "created"},
				{Name: "beta", Path: "/wt/beta", Branch: "beta", Origin: "backfilled"},
			},
		},
		{
			name:             "unmanaged worktrees marked when asked for",
			includeUnmanaged: true,
			want: []workspaceListing{
				{Name: "alpha", Path: "/wt/alpha", Branch: "alpha", Origin: "created"},
				{Name: "beta", Path: "/wt/beta", Branch: "beta", Origin: "backfilled"},
				{Name: "handmade", Path: "/wt/handmade", Branch: "handmade", Origin: "unmanaged"},
				{Name: "detached", Path: "/wt/detached", Branch: "-", Origin: "unmanaged"},
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := workspaceRows(worktrees, recorded, tc.includeUnmanaged)

			// Assert.
			if err != nil {
				t.Fatalf("workspaceRows: %v", err)
			}
			if len(got) != len(tc.want) {
				t.Fatalf("rows = %+v, want %+v", got, tc.want)
			}
			for i := range tc.want {
				if got[i] != tc.want[i] {
					t.Fatalf("rows[%d] = %+v, want %+v", i, got[i], tc.want[i])
				}
			}
		})
	}
}

func TestWorkspaceRowsRefusesAPathTheFormatCannotExpress(t *testing.T) {
	// Arrange.
	worktrees := []gitWorktree{{Path: "/wt/tab\there", Branch: "b"}}
	recorded := map[string]geometry.Record{}

	// Act.
	_, err := workspaceRows(worktrees, recorded, true)

	// Assert.
	if err == nil {
		t.Fatal("workspaceRows accepted a tab in a path, want an error")
	}
	if !strings.Contains(err.Error(), "tab or newline") {
		t.Fatalf("error = %v, want it to name the unprintable field", err)
	}
}

func TestRunListWorkspacesPrintsTheRepositorysRecordedWorkspaces(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	alpha := listWorktree(t, repo, "alpha")
	stateDB := listStateDB(t, geometry.Record{
		Workspace: alpha, SourceBranch: "alpha", SourceDir: alpha, TargetDir: repo, Origin: geometry.OriginCreated,
	})
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", repo, "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListWorkspaces: %v (stderr %s)", err, stderr)
	}
	// git prints the symlink-resolved worktree path, which on macOS differs
	// from t.TempDir()'s /var spelling.
	resolved, err := filepath.EvalSymlinks(alpha)
	if err != nil {
		t.Fatalf("resolve %s: %v", alpha, err)
	}
	want := "alpha\t" + resolved + "\talpha\tcreated\n"
	if stdout.String() != want {
		t.Fatalf("stdout = %q, want %q", stdout.String(), want)
	}
}

func TestRunListWorkspacesOmitsTheMainWorktree(t *testing.T) {
	// Arrange. The main worktree carries a geometry record it must never get,
	// so only the "first stanza" rule can keep it off stdout.
	repo := cliRepo(t)
	stateDB := listStateDB(t, geometry.Record{
		Workspace: repo, SourceBranch: "master", SourceDir: repo, TargetDir: filepath.Dir(repo), Origin: geometry.OriginCreated,
	})
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", repo, "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListWorkspaces: %v (stderr %s)", err, stderr)
	}
	if stdout.String() != "" {
		t.Fatalf("stdout = %q, want the main worktree to be omitted", stdout.String())
	}
}

func TestRunListWorkspacesPrintsNothingWhenNoWorkspaceIsRecorded(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	listWorktree(t, repo, "handmade")
	stateDB := listStateDB(t)
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", repo, "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListWorkspaces: %v (stderr %s)", err, stderr)
	}
	if stdout.String() != "" {
		t.Fatalf("stdout = %q, want an empty listing", stdout.String())
	}
}

func TestRunListWorkspacesReportsARecordedWorkspaceWithNoWorktreeOnStderr(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	gone := filepath.Join(cliWorktreeParent(repo), "gone")
	stateDB := listStateDB(t, geometry.Record{
		Workspace: gone, SourceBranch: "gone", SourceDir: gone, TargetDir: repo, Origin: geometry.OriginCreated,
	})
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", repo, "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListWorkspaces: %v (stderr %s)", err, stderr)
	}
	if stdout.String() != "" {
		t.Fatalf("stdout = %q, want a workspace with no worktree omitted", stdout.String())
	}
	if !strings.Contains(stderr.String(), "OMITTED") || !strings.Contains(stderr.String(), gone) {
		t.Fatalf("stderr = %q, want it to name the omitted workspace %s", stderr.String(), gone)
	}
}

func TestRunListWorkspacesRejectsARelativeGitRoot(t *testing.T) {
	// Arrange.
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", "relative/repo"}, stdout, stderr)

	// Assert.
	if err == nil {
		t.Fatal("runListWorkspaces accepted a relative --git-root, want an error")
	}
	if !strings.Contains(err.Error(), "absolute path") {
		t.Fatalf("error = %v, want it to name the absolute-path requirement", err)
	}
}

func TestRunListWorkspacesFailsWhenTheGitRootIsNotARepository(t *testing.T) {
	// Arrange.
	notARepo := t.TempDir()
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListWorkspaces(context.Background(), []string{"--git-root", notARepo, "--state-db", listStateDB(t)}, stdout, stderr)

	// Assert.
	if err == nil {
		t.Fatal("runListWorkspaces succeeded outside a repository, want an error")
	}
	if !strings.Contains(err.Error(), "git worktree list") {
		t.Fatalf("error = %v, want it to name the failed git command", err)
	}
}

func TestDispatchSubcommandRoutesListWorkspaces(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	argv := []string{"claude-repld", listWorkspacesSubcommand, "--git-root", repo, "--state-db", listStateDB(t)}
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, stdout, stderr)

	// Assert.
	if !handled {
		t.Fatal("dispatchSubcommand did not handle list-workspaces")
	}
	if code != 0 {
		t.Fatalf("exit code = %d, want 0 (stderr %s)", code, stderr)
	}
}
