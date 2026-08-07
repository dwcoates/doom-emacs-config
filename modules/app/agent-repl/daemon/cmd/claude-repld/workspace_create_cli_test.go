package main

import (
	"bytes"
	"context"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
)

// cliGit runs one fixture git command through the SAME env-stripped runner the
// planner uses, so an inherited GIT_DIR (a test run launched from a git hook)
// cannot point these writes at a real repository.
func cliGit(t *testing.T, dir string, args ...string) string {
	t.Helper()
	identity := []string{"-c", "user.name=cli test", "-c", "user.email=cli@test.invalid", "-c", "commit.gpgsign=false"}
	out, exit, err := ExecGitRunner{}.RunGit(context.Background(), dir, append(identity, args...)...)
	if err != nil || exit != 0 {
		t.Fatalf("git %v in %s exit=%d: %v (%s)", args, dir, exit, err, out)
	}
	return out
}

// cliRepo creates a repository with one commit on master and returns its main
// worktree directory.
func cliRepo(t *testing.T) string {
	t.Helper()
	main := filepath.Join(t.TempDir(), "repo")
	if err := os.MkdirAll(main, 0o755); err != nil {
		t.Fatal(err)
	}
	cliGit(t, main, "init", "--quiet")
	cliGit(t, main, "symbolic-ref", "HEAD", "refs/heads/master")
	if err := os.WriteFile(filepath.Join(main, "seed.txt"), []byte("seed\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	cliGit(t, main, "add", "seed.txt")
	cliGit(t, main, "commit", "--quiet", "-m", "seed")
	return main
}

// cliWorktreeParent is where a main worktree's linked worktrees land: a
// sibling "<repo>-worktrees" directory, which is the planner's own rule.
func cliWorktreeParent(repo string) string {
	return filepath.Join(filepath.Dir(repo), filepath.Base(repo)+"-worktrees")
}

func TestParseCreateWorkspaceArgsAcceptsTheCompleteCommandLine(t *testing.T) {
	// Arrange.
	args := []string{"--git-root", "/repo", "--name", "feature", "--base-commit", "master", "--state-db", "/tmp/state.db", "--verbose"}

	// Act.
	opts, err := parseCreateWorkspaceArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseCreateWorkspaceArgs: %v", err)
	}
	want := createWorkspaceOptions{GitRoot: "/repo", Name: "feature", BaseCommit: "master", StateDB: "/tmp/state.db", Verbose: true}
	if opts != want {
		t.Fatalf("options = %+v, want %+v", opts, want)
	}
}

func TestParseCreateWorkspaceArgsDefaultsTheBaseCommitToHEAD(t *testing.T) {
	// Arrange.
	args := []string{"--git-root", "/repo", "--name", "feature"}

	// Act.
	opts, err := parseCreateWorkspaceArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseCreateWorkspaceArgs: %v", err)
	}
	if opts.BaseCommit != "HEAD" {
		t.Fatalf("base commit = %q, want HEAD", opts.BaseCommit)
	}
}

func TestParseCreateWorkspaceArgsRejectsIncompleteCommandLines(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "no git root", args: []string{"--name", "feature"}, want: "--git-root is required"},
		{name: "no name", args: []string{"--git-root", "/repo"}, want: "--name is required"},
		{name: "empty base commit", args: []string{"--git-root", "/repo", "--name", "feature", "--base-commit", ""}, want: "--base-commit cannot be empty"},
		{name: "positional argument", args: []string{"--git-root", "/repo", "--name", "feature", "extra"}, want: `unexpected positional argument "extra"`},
		{name: "unknown flag", args: []string{"--git-root", "/repo", "--name", "feature", "--prompt", "hi"}, want: "flag provided but not defined"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			stderr := new(bytes.Buffer)

			// Act.
			_, err := parseCreateWorkspaceArgs(tc.args, stderr)

			// Assert.
			if err == nil {
				t.Fatalf("parseCreateWorkspaceArgs(%v) succeeded, want an error", tc.args)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want it to contain %q", err, tc.want)
			}
		})
	}
}

func TestRunCreateWorkspaceCreatesTheWorktreeAndPrintsItsPath(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	stateDB := filepath.Join(t.TempDir(), "state.db")
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "feature-one", "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runCreateWorkspace: %v (stderr=%s)", err, stderr)
	}
	want := filepath.Join(cliWorktreeParent(repo), "feature-one")
	got := strings.TrimSpace(stdout.String())
	if got != want {
		t.Fatalf("stdout path = %q, want %q", got, want)
	}
	if branch := strings.TrimSpace(cliGit(t, want, "rev-parse", "--abbrev-ref", "HEAD")); branch != "feature-one" {
		t.Fatalf("worktree branch = %q, want feature-one", branch)
	}
}

func TestRunCreateWorkspacePrintsExactlyOneStdoutLine(t *testing.T) {
	// Arrange — the wrapper script cds into the LAST stdout line, so a second
	// line of any kind on that channel is a path it would try to enter.
	repo := cliRepo(t)
	stateDB := filepath.Join(t.TempDir(), "state.db")
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "feature-solo", "--state-db", stateDB}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runCreateWorkspace: %v (stderr=%s)", err, stderr)
	}
	if lines := strings.Split(strings.TrimSuffix(stdout.String(), "\n"), "\n"); len(lines) != 1 {
		t.Fatalf("stdout lines = %d (%q), want exactly 1", len(lines), stdout.String())
	}
}

func TestRunCreateWorkspaceRecordsMergeGeometry(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	stateDB := filepath.Join(t.TempDir(), "state.db")
	stdout := new(bytes.Buffer)

	// Act.
	if err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "feature-geo", "--state-db", stateDB}, stdout, new(bytes.Buffer)); err != nil {
		t.Fatalf("runCreateWorkspace: %v", err)
	}

	// Assert — the record is what makes the workspace mergeable later, so it is
	// the one durable effect a one-off create is allowed to have.
	db, err := statedb.Open(stateDB)
	if err != nil {
		t.Fatal(err)
	}
	defer db.Close()
	store, err := geometry.Open(db, dlog.Logf(func(string, ...any) {}))
	if err != nil {
		t.Fatal(err)
	}
	rec, found, err := store.Lookup(context.Background(), strings.TrimSpace(stdout.String()))
	if err != nil {
		t.Fatal(err)
	}
	if !found {
		t.Fatal("no merge geometry was recorded for the created workspace")
	}
	want := geometry.Record{
		Workspace:    geometry.Key(strings.TrimSpace(stdout.String())),
		SourceBranch: "feature-geo",
		SourceDir:    geometry.Key(strings.TrimSpace(stdout.String())),
		TargetDir:    geometry.Key(repo),
		Origin:       geometry.OriginCreated,
	}
	if rec != want {
		t.Fatalf("geometry = %+v, want %+v", rec, want)
	}
}

func TestRunCreateWorkspaceWritesNoCreateJobStore(t *testing.T) {
	// Arrange — a one-off create owes no session, no Emacs handshake and no
	// prompt, so a durable job a running daemon's boot resume would pick up
	// must never be written.
	repo := cliRepo(t)
	stateRoot := t.TempDir()
	t.Setenv("AGENT_REPL_STATE_DIR", stateRoot)
	stateDB := filepath.Join(t.TempDir(), "state.db")

	// Act.
	if err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "feature-nojob", "--state-db", stateDB}, new(bytes.Buffer), new(bytes.Buffer)); err != nil {
		t.Fatalf("runCreateWorkspace: %v", err)
	}

	// Assert.
	storePath, _, err := workspaceCreatePaths(stateRoot)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(storePath); !os.IsNotExist(err) {
		t.Fatalf("stat %s = %v, want the create job store to be absent", storePath, err)
	}
}

func TestRunCreateWorkspaceSuffixesAColidingName(t *testing.T) {
	// Arrange — the second run's requested name is already a branch, which is
	// the planner's collision path rather than a failure.
	repo := cliRepo(t)
	stateDB := filepath.Join(t.TempDir(), "state.db")
	first := new(bytes.Buffer)
	if err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "twice", "--state-db", stateDB}, first, new(bytes.Buffer)); err != nil {
		t.Fatalf("first runCreateWorkspace: %v", err)
	}
	second := new(bytes.Buffer)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "twice", "--state-db", stateDB}, second, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("second runCreateWorkspace: %v", err)
	}
	got := strings.TrimSpace(second.String())
	if got == strings.TrimSpace(first.String()) {
		t.Fatalf("second worktree path = %q, want a path distinct from the first", got)
	}
	if !strings.HasPrefix(filepath.Base(got), "twice-") {
		t.Fatalf("second worktree name = %q, want a suffixed \"twice-\" name", filepath.Base(got))
	}
}

func TestRunCreateWorkspaceRejectsAMissingGitRoot(t *testing.T) {
	// Arrange.
	missing := filepath.Join(t.TempDir(), "absent")
	stdout := new(bytes.Buffer)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", missing, "--name", "feature", "--state-db", filepath.Join(t.TempDir(), "state.db")}, stdout, new(bytes.Buffer))

	// Assert.
	if err == nil {
		t.Fatal("runCreateWorkspace succeeded, want a failure for a nonexistent git root")
	}
	if !strings.Contains(err.Error(), "stat git_root") {
		t.Fatalf("error = %v, want it to name the unusable git root", err)
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout = %q, want nothing printed on a failure", stdout)
	}
}

func TestRunCreateWorkspaceRejectsARelativeGitRoot(t *testing.T) {
	// Arrange.
	stdout := new(bytes.Buffer)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", "relative/repo", "--name", "feature", "--state-db", filepath.Join(t.TempDir(), "state.db")}, stdout, new(bytes.Buffer))

	// Assert.
	if err == nil {
		t.Fatal("runCreateWorkspace succeeded, want a failure for a relative git root")
	}
	if !strings.Contains(err.Error(), "must be an absolute path") {
		t.Fatalf("error = %v, want it to reject the relative path", err)
	}
}

func TestRunCreateWorkspaceRejectsADirectoryThatIsNotARepository(t *testing.T) {
	// Arrange.
	plain := t.TempDir()

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", plain, "--name", "feature", "--state-db", filepath.Join(t.TempDir(), "state.db")}, new(bytes.Buffer), new(bytes.Buffer))

	// Assert.
	if err == nil {
		t.Fatal("runCreateWorkspace succeeded, want a failure for a directory with no .git")
	}
	if !strings.Contains(err.Error(), "has no .git") {
		t.Fatalf("error = %v, want it to name the missing .git", err)
	}
}

func TestRunCreateWorkspaceRejectsAnUnresolvableBaseCommit(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)

	// Act.
	err := runCreateWorkspace(context.Background(), []string{"--git-root", repo, "--name", "feature", "--base-commit", "no-such-ref", "--state-db", filepath.Join(t.TempDir(), "state.db")}, new(bytes.Buffer), new(bytes.Buffer))

	// Assert.
	if err == nil {
		t.Fatal("runCreateWorkspace succeeded, want a failure for an unresolvable base commit")
	}
	if !strings.Contains(err.Error(), "resolve base") {
		t.Fatalf("error = %v, want it to name the unresolvable base", err)
	}
}

func TestDispatchSubcommandDeclinesADaemonInvocation(t *testing.T) {
	// Arrange.
	argv := []string{"claude-repld", "-addr", "127.0.0.1:8787"}

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, new(bytes.Buffer), new(bytes.Buffer))

	// Assert.
	if handled || code != 0 {
		t.Fatalf("dispatchSubcommand = (%t, %d), want (false, 0) so the daemon runtime starts", handled, code)
	}
}

func TestDispatchSubcommandRunsTheCreateWorkspaceSubcommand(t *testing.T) {
	// Arrange.
	repo := cliRepo(t)
	argv := []string{"claude-repld", createWorkspaceSubcommand, "--git-root", repo, "--name", "dispatched", "--state-db", filepath.Join(t.TempDir(), "state.db")}
	stdout := new(bytes.Buffer)

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, stdout, new(bytes.Buffer))

	// Assert.
	if !handled || code != 0 {
		t.Fatalf("dispatchSubcommand = (%t, %d), want (true, 0)", handled, code)
	}
	if got := strings.TrimSpace(stdout.String()); got != filepath.Join(cliWorktreeParent(repo), "dispatched") {
		t.Fatalf("stdout path = %q, want the created worktree", got)
	}
}

func TestDispatchSubcommandReportsAFailureExitCode(t *testing.T) {
	// Arrange.
	argv := []string{"claude-repld", createWorkspaceSubcommand, "--name", "feature"}
	stderr := new(bytes.Buffer)

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, new(bytes.Buffer), stderr)

	// Assert.
	if !handled || code != 1 {
		t.Fatalf("dispatchSubcommand = (%t, %d), want (true, 1)", handled, code)
	}
	if !strings.Contains(stderr.String(), "--git-root is required") {
		t.Fatalf("stderr = %q, want the failure explained", stderr)
	}
}

func TestDispatchSubcommandReportsUsageExitCodeForHelp(t *testing.T) {
	// Arrange.
	argv := []string{"claude-repld", createWorkspaceSubcommand, "-h"}

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, new(bytes.Buffer), new(bytes.Buffer))

	// Assert.
	if !handled || code != 2 {
		t.Fatalf("dispatchSubcommand = (%t, %d), want (true, 2) for %v", handled, code, flag.ErrHelp)
	}
}
