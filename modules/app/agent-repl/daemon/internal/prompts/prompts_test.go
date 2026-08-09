package prompts

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// writePrompt stages one prompt file in a temp directory and points DirEnv at
// it, so a test exercises the very same override a user would set.
func writePrompt(t *testing.T, name, content string) {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(content), 0o600); err != nil {
		t.Fatalf("write prompt fixture: %v", err)
	}
	t.Setenv(DirEnv, dir)
}

func TestRenderSubstitutesPlaceholders(t *testing.T) {
	// Arrange.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{who}} -->\nhello {{who}}\n")

	// Act.
	got, err := Render("p.md", map[string]string{"who": "world"})

	// Assert.
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if got != "hello world" {
		t.Fatalf("Render = %q, want %q", got, "hello world")
	}
}

func TestRenderStripsTheHeaderComment(t *testing.T) {
	// Arrange.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: none -->\nbody\n")

	// Act.
	got, err := Render("p.md", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if strings.Contains(got, "used by") {
		t.Fatalf("Render = %q, want the header comment stripped", got)
	}
}

func TestRenderKeepsAHtmlCommentInsideTheBody(t *testing.T) {
	// Arrange: only the LEADING comment is a header; prose may contain others.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: none -->\nsay <!-- this --> aloud\n")

	// Act.
	got, err := Render("p.md", nil)

	// Assert.
	if got != "say <!-- this --> aloud" || err != nil {
		t.Fatalf("Render = %q, %v; want the in-body comment preserved", got, err)
	}
}

func TestRenderDropsExactlyOneTrailingNewline(t *testing.T) {
	// Arrange: two terminators means the prompt itself ends with one.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: none -->\nbody\n\n")

	// Act.
	got, err := Render("p.md", nil)

	// Assert.
	if got != "body\n" || err != nil {
		t.Fatalf("Render = %q, %v; want %q", got, err, "body\n")
	}
}

func TestRenderSubstitutesEveryOccurrenceOfAPlaceholder(t *testing.T) {
	// Arrange.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{x}} -->\n{{x}} and {{x}}\n")

	// Act.
	got, err := Render("p.md", map[string]string{"x": "a"})

	// Assert.
	if got != "a and a" || err != nil {
		t.Fatalf("Render = %q, %v; want %q", got, err, "a and a")
	}
}

func TestRenderErrorsOnAMissingFile(t *testing.T) {
	// Arrange.
	t.Setenv(DirEnv, t.TempDir())

	// Act.
	_, err := Render("absent.md", nil)

	// Assert.
	if err == nil {
		t.Fatal("Render of a missing prompt returned no error; a missing file must never fall back to a baked-in copy")
	}
	if !strings.Contains(err.Error(), "absent.md") {
		t.Fatalf("error %q does not name the missing file", err)
	}
}

func TestRenderErrorsOnAnUnsubstitutedPlaceholder(t *testing.T) {
	// Arrange: the file's spelling drifted from the call site's.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{who}} -->\nhello {{whom}}\n")

	// Act.
	_, err := Render("p.md", map[string]string{"who": "world"})

	// Assert.
	if err == nil {
		t.Fatal("Render left {{whom}} unsubstituted without erroring")
	}
	if !strings.Contains(err.Error(), "{{whom}}") {
		t.Fatalf("error %q does not name the leftover placeholder", err)
	}
}

func TestUnsubstitutedPlaceholderErrorListsTheExpectedPlaceholders(t *testing.T) {
	// Arrange.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{a}}, {{b}} -->\n{{a}} {{typo}}\n")

	// Act.
	_, err := Render("p.md", map[string]string{"a": "1", "b": "2"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "{{b}}") {
		t.Fatalf("error = %v; want it to list the call site's supplied placeholders so a typo is diagnosable", err)
	}
}

func TestRenderErrorsWhenASuppliedPlaceholderIsNeverUsed(t *testing.T) {
	// Arrange: an edit deleted {{b}}, which would silently drop its value.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{a}} -->\nonly {{a}}\n")

	// Act.
	_, err := Render("p.md", map[string]string{"a": "1", "b": "2"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "{{b}}") {
		t.Fatalf("error = %v; want a loud error naming the dropped placeholder", err)
	}
}

func TestRenderErrorsOnAnEmptyBody(t *testing.T) {
	// Arrange: a file that is nothing but its header.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: none -->\n")

	// Act.
	_, err := Render("p.md", nil)

	// Assert.
	if err == nil {
		t.Fatal("Render of a header-only prompt returned no error; a blank prompt would send an empty turn")
	}
}

func TestRenderLeavesBracedProseAlone(t *testing.T) {
	// Arrange: shell expansions and code samples are not placeholders.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: none -->\nrun ${HOME} and {{Not A Placeholder}}\n")

	// Act.
	got, err := Render("p.md", nil)

	// Assert.
	if err != nil || !strings.Contains(got, "{{Not A Placeholder}}") {
		t.Fatalf("Render = %q, %v; want braced prose preserved verbatim", got, err)
	}
}

func TestDirPrefersTheEnvironmentOverride(t *testing.T) {
	// Arrange.
	want := t.TempDir()
	t.Setenv(DirEnv, want)

	// Act.
	got, err := Dir()

	// Assert.
	if got != want || err != nil {
		t.Fatalf("Dir = %q, %v; want %q", got, err, want)
	}
}

func TestDirIgnoresABlankEnvironmentOverride(t *testing.T) {
	// Arrange: an exported-but-empty variable must not resolve to "".
	t.Setenv(DirEnv, "   ")

	// Act: it falls through to the executable walk-up, which in a test binary
	// either finds a checkout or fails loudly.
	got, err := Dir()

	// Assert.
	if got == "" && err == nil {
		t.Fatal("Dir accepted a blank override as the answer, resolving to the empty directory with no error")
	}
}

func TestSourceDirNamesTheCheckoutsPromptsDirectory(t *testing.T) {
	// Act.
	got, err := SourceDir()

	// Assert.
	if err != nil {
		t.Fatalf("SourceDir: %v", err)
	}
	if !strings.HasSuffix(filepath.ToSlash(got), RelDir) {
		t.Fatalf("SourceDir = %q, want it to end in %q", got, RelDir)
	}
	if _, err := os.Stat(got); err != nil {
		t.Fatalf("SourceDir %q does not exist: %v", got, err)
	}
}

func TestRenderCarriesBracedTextInsideASubstitutedValue(t *testing.T) {
	// Arrange — a user's own prompt may contain something shaped like a
	// placeholder, and failing the send over it would be a false alarm.
	writePrompt(t, "p.md", "<!-- used by: test; placeholders: {{msg}} -->\nsay: {{msg}}\n")

	// Act.
	got, err := Render("p.md", map[string]string{"msg": "what does {{foo}} mean?"})

	// Assert.
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if got != "say: what does {{foo}} mean?" {
		t.Fatalf("Render = %q, want the braced user text carried through verbatim", got)
	}
}

// git runs one git command in dir with every inherited git environment variable
// stripped. A parent process's GIT_DIR — a hook, an outer test harness — would
// otherwise point these commands at some other repository entirely.
func git(t *testing.T, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	// An exported-but-EMPTY GIT_DIR is still a set GIT_DIR to git ("the empty
	// string is not a valid path"), so the variables are dropped from the
	// environment rather than blanked.
	env := make([]string, 0, len(os.Environ()))
	for _, kv := range os.Environ() {
		if strings.HasPrefix(kv, "GIT_") {
			continue
		}
		env = append(env, kv)
	}
	cmd.Env = append(env,
		"GIT_AUTHOR_NAME=t", "GIT_AUTHOR_EMAIL=t@example.com",
		"GIT_COMMITTER_NAME=t", "GIT_COMMITTER_EMAIL=t@example.com",
	)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %s in %s: %v\n%s", strings.Join(args, " "), dir, err, out)
	}
}

// rebaseWorktree builds the shape the merge gate runs its suite in: an
// agent-repl checkout, plus a LINKED GIT WORKTREE of it under a temp directory
// at the leaf name `rebase`. A linked worktree carries a `.git` FILE rather than
// a directory, which is precisely the shape a resolution that assumes the
// primary checkout gets wrong. It returns the primary checkout and the worktree.
func rebaseWorktree(t *testing.T) (primary, worktree string) {
	t.Helper()
	primary = filepath.Join(t.TempDir(), "checkout")
	if err := os.MkdirAll(filepath.Join(primary, filepath.FromSlash("modules/app/agent-repl/bin")), 0o700); err != nil {
		t.Fatalf("stage primary checkout: %v", err)
	}
	if err := os.WriteFile(filepath.Join(primary, filepath.FromSlash(markerRelPath)), []byte("#!/bin/sh\n"), 0o700); err != nil {
		t.Fatalf("stage checkout marker: %v", err)
	}
	git(t, primary, "init", "--initial-branch=master")
	git(t, primary, "add", ".")
	git(t, primary, "commit", "-m", "checkout")
	worktree = filepath.Join(t.TempDir(), "rebase")
	git(t, primary, "worktree", "add", "--detach", worktree, "master")
	return primary, worktree
}

func TestCheckoutContainingResolvesTheRunningWorktreeNotThePrimaryCheckout(t *testing.T) {
	// Arrange: the gate compiles and runs the suite inside the linked worktree,
	// so resolution must answer with THAT tree even though the primary checkout
	// carries the same marker.
	primary, worktree := rebaseWorktree(t)
	probe := filepath.Join(worktree, filepath.FromSlash("daemon/internal/prompts"))

	// Act.
	got, ok := checkoutContaining(probe)

	// Assert.
	if !ok {
		t.Fatalf("checkoutContaining(%q) found no checkout, want the worktree %q", probe, worktree)
	}
	if got != worktree {
		t.Fatalf("checkoutContaining(%q) = %q, want the running worktree %q (primary checkout is %q)", probe, got, worktree, primary)
	}
}

func TestCheckoutContainingKeepsThePathSpellingItWasGiven(t *testing.T) {
	// Arrange: on macOS the gate's temp path is /var/folders/... while its
	// canonical form is /private/var/folders/... . Resolution must not silently
	// hand back one spelling when it was asked about the other, or every path
	// comparison downstream compares two names for the same directory.
	_, worktree := rebaseWorktree(t)
	canonical, err := filepath.EvalSymlinks(worktree)
	if err != nil {
		t.Fatalf("canonicalize worktree: %v", err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(canonical, link); err != nil {
		t.Fatalf("stage symlinked spelling: %v", err)
	}

	tests := []struct {
		name string
		root string
	}{
		{name: "canonical spelling", root: canonical},
		{name: "symlinked spelling", root: link},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, ok := checkoutContaining(filepath.Join(tc.root, filepath.FromSlash("daemon/internal/prompts")))

			// Assert.
			if !ok {
				t.Fatalf("checkoutContaining under %q found no checkout", tc.root)
			}
			if got != tc.root {
				t.Fatalf("checkoutContaining under %q = %q, want %q", tc.root, got, tc.root)
			}
		})
	}
}

func TestCheckoutContainingReportsNoCheckoutWhenTheWorktreeIsGone(t *testing.T) {
	// Arrange: the gate's worktree can be removed out from under a running
	// suite. That must stay a LOUD not-found rather than resolving to whatever
	// unrelated checkout happens to sit above the temp directory.
	_, worktree := rebaseWorktree(t)
	if err := os.RemoveAll(worktree); err != nil {
		t.Fatalf("remove worktree: %v", err)
	}

	// Act.
	got, ok := checkoutContaining(filepath.Join(worktree, filepath.FromSlash("daemon/internal/prompts")))

	// Assert.
	if ok {
		t.Fatalf("checkoutContaining of a removed worktree = %q, true; want not found", got)
	}
}
