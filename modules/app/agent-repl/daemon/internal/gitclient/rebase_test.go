package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// rebaseDirFixture answers `rev-parse --git-path rebase-merge` with a path
// that exists on disk when inProgress is true, and one that does not when it
// is false.
func rebaseDirFixture(t *testing.T, inProgress bool) gitFixture {
	t.Helper()
	path := filepath.Join(t.TempDir(), "rebase-merge")
	if inProgress {
		if err := os.Mkdir(path, 0o755); err != nil {
			t.Fatalf("mkdir %s: %v", path, err)
		}
	}
	return ok(path+"\n", "rev-parse", "--git-path", "rebase-merge")
}

// sequenceEditorPath reads the todo path the sequence editor binding copies.
func sequenceEditorPath(t *testing.T, env []string) string {
	t.Helper()
	values := envValues(env, "GIT_SEQUENCE_EDITOR")
	if len(values) != 1 {
		t.Fatalf("GIT_SEQUENCE_EDITOR bindings = %v, want exactly one", values)
	}
	command := strings.TrimPrefix(values[0], "GIT_SEQUENCE_EDITOR=")
	quoted, found := strings.CutPrefix(command, "cp ")
	if !found {
		t.Fatalf("GIT_SEQUENCE_EDITOR = %q, want a cp of the daemon's todo", command)
	}
	return strings.Trim(quoted, "'")
}

func TestCommitsBetweenWalksTheRangeOldestFirstWithoutMerges(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(commitLine("aaa", "one", "A", "2026-09-30T10:00:00Z")+"\n", "rev-list"))

	// Act.
	if _, err := git.CommitsBetween(context.Background(), "/wt", "base", "tip"); err != nil {
		t.Fatalf("CommitsBetween: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rev-list", "--reverse", "--no-merges", "--format="+commitFormat, "--no-commit-header", "base..tip")
}

func TestCommitsBetweenAnswersEveryCommitInOrder(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(commitLine("aaa", "one", "A", "2026-09-30T10:00:00Z")+"\n"+
		commitLine("bbb", "two", "A", "2026-09-30T10:01:00Z")+"\n", "rev-list"))

	// Act.
	commits, err := git.CommitsBetween(context.Background(), "/wt", "base", "tip")

	// Assert.
	if err != nil {
		t.Fatalf("CommitsBetween: %v", err)
	}
	var got []string
	for _, c := range commits {
		got = append(got, c.SHA+":"+c.Subject)
	}
	if want := []string{"aaa:one", "bbb:two"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("CommitsBetween = %v, want %v", got, want)
	}
}

func TestCommitsBetweenFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: bad revision\n", "rev-list"))

	// Act.
	_, err := git.CommitsBetween(context.Background(), "/wt", "base", "tip")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "bad revision") {
		t.Fatalf("CommitsBetween error = %v, want git's own evidence", err)
	}
}

func TestStartRebaseRunsAnInteractiveRebaseOntoTheTip(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rebase"), rebaseDirFixture(t, true))

	// Act.
	if _, err := git.StartRebase(context.Background(), "/wt", "tip", []string{"aaa", "bbb"}); err != nil {
		t.Fatalf("StartRebase: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rebase", "-i", "--empty=drop", "tip")
}

func TestStartRebaseOpensNoEditor(t *testing.T) {
	// Arrange: the daemon has no terminal.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rebase"), rebaseDirFixture(t, true))

	// Act.
	if _, err := git.StartRebase(context.Background(), "/wt", "tip", []string{"aaa"}); err != nil {
		t.Fatalf("StartRebase: %v", err)
	}

	// Assert.
	if got := envValues(fake.call(0).Env, "GIT_EDITOR"); !reflect.DeepEqual(got, []string{"GIT_EDITOR=true"}) {
		t.Fatalf("GIT_EDITOR bindings = %v, want exactly [GIT_EDITOR=true]", got)
	}
}

func TestStartRebaseRemovesItsTodoListOnceGitHasRead(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rebase"), rebaseDirFixture(t, true))

	// Act.
	if _, err := git.StartRebase(context.Background(), "/wt", "tip", []string{"aaa", "bbb"}); err != nil {
		t.Fatalf("StartRebase: %v", err)
	}

	// Assert.
	todo := sequenceEditorPath(t, fake.call(0).Env)
	if _, err := os.Stat(todo); !os.IsNotExist(err) {
		t.Fatalf("the todo list %s still stands (stat error %v); it must be removed", todo, err)
	}
}

func TestStartRebaseRefusesAnEmptyReplay(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	fake := newFakeGit(t)

	// Act.
	_, err := git.StartRebase(context.Background(), "/wt", "tip", nil)

	// Assert.
	if err == nil {
		t.Fatalf("StartRebase(no commits) = nil, want a refusal")
	}
	if len(fake.calls()) != 0 {
		t.Fatalf("StartRebase(no commits) ran git %v, want nothing run", subjects(fake.calls()))
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.start_rebase"); !found {
		t.Fatalf("the refusal was not logged at ERROR: %+v", surfaces.records())
	}
}

func TestRebaseCommandsAnswerWhereTheRebaseStands(t *testing.T) {
	tests := []struct {
		name       string
		exit       int
		inProgress bool
		conflicted string
		want       RebaseStep
	}{
		{name: "stopped at a break", exit: 0, inProgress: true, want: RebaseStep{}},
		{name: "finished", exit: 0, inProgress: false, want: RebaseStep{Done: true}},
		{name: "conflicted", exit: 1, conflicted: "a.go\x00b.go\x00", want: RebaseStep{Conflicted: []string{"a.go", "b.go"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			git, _ := newTestClient(t)
			fixtures := []gitFixture{{Match: []string{"rebase"}, Exit: tt.exit}}
			if tt.exit == 0 {
				fixtures = append(fixtures, rebaseDirFixture(t, tt.inProgress))
			} else {
				fixtures = append(fixtures, ok(tt.conflicted, "diff"))
			}
			newFakeGit(t, fixtures...)

			// Act.
			got, err := git.ContinueRebase(context.Background(), "/wt")

			// Assert.
			if err != nil {
				t.Fatalf("ContinueRebase: %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ContinueRebase = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestContinueRebaseContinuesWithNoEditor(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rebase"), rebaseDirFixture(t, true))

	// Act.
	if _, err := git.ContinueRebase(context.Background(), "/wt"); err != nil {
		t.Fatalf("ContinueRebase: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rebase", "--continue")
	if got := envValues(fake.call(0).Env, "GIT_EDITOR"); !reflect.DeepEqual(got, []string{"GIT_EDITOR=true"}) {
		t.Fatalf("GIT_EDITOR bindings = %v, want exactly [GIT_EDITOR=true]", got)
	}
}

func TestAbortRebaseAbortsTheStandingRebaseWithNoEditor(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rebase"))

	// Act.
	if err := git.AbortRebase(context.Background(), "/wt"); err != nil {
		t.Fatalf("AbortRebase: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rebase", "--abort")
	if got := envValues(fake.call(0).Env, "GIT_EDITOR"); !reflect.DeepEqual(got, []string{"GIT_EDITOR=true"}) {
		t.Fatalf("GIT_EDITOR bindings = %v, want exactly [GIT_EDITOR=true]", got)
	}
}

func TestAbortRebaseFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: No rebase in progress?\n", "rebase"))

	// Act.
	err := git.AbortRebase(context.Background(), "/wt")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "No rebase in progress") {
		t.Fatalf("AbortRebase error = %v, want git's own evidence", err)
	}
}

func TestARebaseFailureWithoutConflictsIsAFailure(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "error: cannot rebase: You have unstaged changes.\n", "rebase"), ok("", "diff"), rebaseDirFixture(t, false))

	// Act.
	_, err := git.ContinueRebase(context.Background(), "/wt")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "unstaged changes") {
		t.Fatalf("ContinueRebase error = %v, want git's own evidence", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.continue_rebase"); !found {
		t.Fatalf("the failure was not logged at ERROR: %+v", surfaces.records())
	}
}

func TestARebaseStopThatRerereTracksNothingIsAFailure(t *testing.T) {
	// Arrange: the rebase stands stopped with nothing unmerged, and rerere
	// resolved nothing -- an untracked file the pick would overwrite.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "error: The following untracked working tree files would be overwritten\n", "rebase"),
		ok("", "diff"), rebaseDirFixture(t, true), ok("", "rerere", "status"))

	// Act.
	_, err := git.ContinueRebase(context.Background(), "/wt")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "untracked working tree files") {
		t.Fatalf("ContinueRebase error = %v, want git's own evidence", err)
	}
}

func TestARebaseStopReadAsAFailureRecordsTheWorktreeAtTheStop(t *testing.T) {
	// Arrange: the stop reads as no conflict, and the evidence reads answer.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "Recorded preimage for 'a.go'\n", "rebase"),
		ok("", "diff"), rebaseDirFixture(t, true), ok("", "rerere", "status"),
		ok("100644 abc 2\ta.go\n", "ls-files", "-u"), ok("M  a.go\n", "status"))

	// Act.
	_, err := git.ContinueRebase(context.Background(), "/wt")

	// Assert.
	if err == nil {
		t.Fatal("ContinueRebase succeeded, want the stop read as a failure")
	}
	record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.continue_rebase")
	if !found {
		t.Fatalf("the failure was not logged at ERROR: %+v", surfaces.records())
	}
	got := []any{record.Context["unmerged"], record.Context["status"], record.Context["rerere_status"]}
	want := []any{"100644 abc 2\ta.go", "M  a.go", ""}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("evidence = %q, want %q", got, want)
	}
}

func TestARebaseStopEvidenceNamesAReadThatFailed(t *testing.T) {
	// Arrange: the status read fails.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "error: could not apply\n", "rebase"),
		ok("", "diff"), rebaseDirFixture(t, true), ok("", "rerere", "status"),
		ok("", "ls-files", "-u"), fails(128, "fatal: index file corrupt\n", "status"))

	// Act.
	_, _ = git.ContinueRebase(context.Background(), "/wt")

	// Assert.
	record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.continue_rebase")
	if !found {
		t.Fatalf("the failure was not logged at ERROR: %+v", surfaces.records())
	}
	if cause, _ := record.Context["status_error"].(string); !strings.Contains(cause, "index file corrupt") {
		t.Fatalf("status_error = %v, want the failed read's cause", record.Context["status_error"])
	}
}

func TestARebaseStopRerereResolvedAnswersTheResolvedPaths(t *testing.T) {
	// Arrange: the commit conflicted, and rerere staged its recorded
	// resolution of both paths.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "Staged 'a.go' using previous resolution.\n", "rebase"),
		ok("", "diff"), rebaseDirFixture(t, true), ok("a.go\nb.go\n", "rerere", "status"))

	// Act.
	got, err := git.ContinueRebase(context.Background(), "/wt")

	// Assert.
	if err != nil {
		t.Fatalf("ContinueRebase: %v", err)
	}
	if want := (RebaseStep{Resolved: []string{"a.go", "b.go"}}); !reflect.DeepEqual(got, want) {
		t.Fatalf("ContinueRebase = %+v, want %+v", got, want)
	}
}

func TestEveryRebaseCommandReusesRecordedResolutions(t *testing.T) {
	tests := []struct {
		name string
		act  func(Git) error
	}{
		{name: "start", act: func(g Git) error {
			_, err := g.StartRebase(context.Background(), "/wt", "tip", []string{"aaa"})
			return err
		}},
		{name: "continue", act: func(g Git) error {
			_, err := g.ContinueRebase(context.Background(), "/wt")
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			git, _ := newTestClient(t)
			fake := newFakeGit(t, ok("", "rebase"), rebaseDirFixture(t, true))

			// Act.
			if err := tt.act(git); err != nil {
				t.Fatalf("rebase command: %v", err)
			}

			// Assert.
			env := fake.call(0).Env
			want := []string{"GIT_CONFIG_COUNT=2", "GIT_CONFIG_KEY_0=rerere.enabled", "GIT_CONFIG_VALUE_0=true",
				"GIT_CONFIG_KEY_1=rerere.autoUpdate", "GIT_CONFIG_VALUE_1=true"}
			for _, binding := range want {
				name, _, _ := strings.Cut(binding, "=")
				if got := envValues(env, name); !reflect.DeepEqual(got, []string{binding}) {
					t.Fatalf("%s bindings = %v, want exactly [%s]", name, got, binding)
				}
			}
		})
	}
}

func TestARebaseConflictIsAnOutcomeLoggedAtInfo(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "CONFLICT\n", "rebase"), ok("a.go\x00", "diff"))

	// Act.
	if _, err := git.ContinueRebase(context.Background(), "/wt"); err != nil {
		t.Fatalf("ContinueRebase: %v", err)
	}

	// Assert: a conflict is the rebase's ordinary stop, which its caller
	// resolves, so it is never a WARN.
	if _, found := recordFor(surfaces.records(), "info", "daemon.gitclient.continue_rebase"); !found {
		t.Fatalf("the conflict was not logged at INFO: %+v", surfaces.records())
	}
	if _, found := recordFor(surfaces.records(), "warn", "daemon.gitclient.continue_rebase"); found {
		t.Fatalf("the conflict was logged at WARN: %+v", surfaces.records())
	}
}

func TestARebaseConflictIsNeverAborted(t *testing.T) {
	// Arrange: a conflicted rebase is left in progress for its resolution.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, fails(1, "CONFLICT\n", "rebase"), ok("a.go\x00", "diff"))

	// Act.
	if _, err := git.ContinueRebase(context.Background(), "/wt"); err != nil {
		t.Fatalf("ContinueRebase: %v", err)
	}

	// Assert.
	fake.assertNever("rebase", "--abort")
}

func TestRebaseInProgressResolvesARelativeGitPathAgainstTheWorktree(t *testing.T) {
	// Arrange: git answers a path relative to the directory it ran in.
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git", "rebase-merge"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	git, _ := newTestClient(t)
	newFakeGit(t, ok(".git/rebase-merge\n", "rev-parse", "--git-path", "rebase-merge"))

	// Act.
	got, err := git.RebaseInProgress(context.Background(), dir)

	// Assert.
	if err != nil || !got {
		t.Fatalf("RebaseInProgress = (%v, %v), want (true, nil)", got, err)
	}
}

func TestRebaseInProgressIsFalseWithNoRebaseDirectory(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, rebaseDirFixture(t, false))

	// Act.
	got, err := git.RebaseInProgress(context.Background(), "/wt")

	// Assert.
	if err != nil || got {
		t.Fatalf("RebaseInProgress = (%v, %v), want (false, nil)", got, err)
	}
}

func TestWriteRebaseTodoBreaksAfterEveryPickButTheLast(t *testing.T) {
	// Arrange / Act.
	path, err := writeRebaseTodo([]string{"aaa", "bbb", "ccc"})
	if err != nil {
		t.Fatalf("writeRebaseTodo: %v", err)
	}
	defer os.Remove(path)
	body, err := os.ReadFile(path)

	// Assert.
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	want := "pick aaa\nbreak\npick bbb\nbreak\npick ccc\n"
	if string(body) != want {
		t.Fatalf("todo = %q, want %q", body, want)
	}
}

func TestAddWorktreeChecksTheExistingBranchOut(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.AddWorktree(context.Background(), "/repo", "/state/wt", "feature"); err != nil {
		t.Fatalf("AddWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "add", "/state/wt", "feature")
}

func TestFetchFetchesTheRemote(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.Fetch(context.Background(), "/repo", "origin"); err != nil {
		t.Fatalf("Fetch: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "fetch", "origin")
}

func TestFetchFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: could not read from remote\n", "fetch"))

	// Act.
	err := git.Fetch(context.Background(), "/repo", "origin")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "could not read") {
		t.Fatalf("Fetch error = %v, want git's own evidence", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.fetch"); !found {
		t.Fatalf("the failure was not logged at ERROR: %+v", surfaces.records())
	}
}
