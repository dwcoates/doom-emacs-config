// testrepo_test.go holds what both test files build on: REAL repositories made
// with `git init` under t.TempDir. Nothing here mocks git — the whole point of
// this leaf is what git actually does, and a mock would only assert that the
// argument vectors are the ones the test author expected.
package gitclient

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// testSurfaces is the dlog.Surfaces double. gitclient logs everything
// globally — it is a leaf that is handed arbitrary repository and worktree
// directories and knows nothing of workspaces, so it has no workspace sink to
// resolve — and every other method here is unreachable from this package.
type testSurfaces struct {
	global *dlog.TestLogger
}

func newTestSurfaces() *testSurfaces {
	return &testSurfaces{global: dlog.NewTestLogger()}
}

func (s *testSurfaces) Global() dlog.Logger { return s.global }

func (s *testSurfaces) Workspace(string) (dlog.Logger, error) {
	panic("gitclient must never resolve a workspace sink: it is a leaf with no workspace identity")
}

func (s *testSurfaces) ShimSink(string) (dlog.Borrowed, error) {
	panic("gitclient must never borrow a shim sink")
}

func (s *testSurfaces) ClientLog(string, dlog.ClientRecord) error {
	panic("gitclient must never persist a client record")
}

func (s *testSurfaces) Close() error { return nil }

// records returns every log record captured so far.
func (s *testSurfaces) records() []dlog.Record { return s.global.Records() }

// newTestClient builds the client under test with a hermetic git environment.
//
// GIT_CONFIG_GLOBAL/GIT_CONFIG_SYSTEM are pointed at /dev/null so the
// developer's own git configuration — an `init.defaultBranch`, a commit
// signing requirement, a merge driver — cannot decide a test's outcome. They
// are deliberately NOT in the scrub list: they configure git, they do not
// select a repository.
func newTestClient(t *testing.T) (*client, *testSurfaces) {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv("GIT_CONFIG_GLOBAL", os.DevNull)
	t.Setenv("GIT_CONFIG_SYSTEM", os.DevNull)

	surfaces := newTestSurfaces()
	git, err := New(surfaces)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return git.(*client), surfaces
}

// gitAt runs a raw git in dir for the test's own arranging and asserting. It
// uses the same scrubbed environment as the client so an inherited GIT_DIR
// cannot make the FIXTURE wrong and disguise a passing test as a real one.
func gitAt(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = scrubEnv(os.Environ())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %s in %s: %v\n%s", strings.Join(args, " "), dir, err, out)
	}
	return strings.TrimRight(string(out), "\n")
}

// gitExitAt runs a raw git and returns its exit code, for assertions whose
// subject is a nonzero status.
func gitExitAt(t *testing.T, dir string, args ...string) int {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = scrubEnv(os.Environ())
	if err := cmd.Run(); err != nil {
		return cmd.ProcessState.ExitCode()
	}
	return 0
}

// initRepo creates an empty repository whose initial branch is `branch`, with
// a committable identity and signing off.
func initRepo(t *testing.T, branch string) string {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "repo")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	gitAt(t, dir, "init", "--quiet", "--initial-branch="+branch)
	gitAt(t, dir, "config", "user.name", "Test Author")
	gitAt(t, dir, "config", "user.email", "test@example.invalid")
	gitAt(t, dir, "config", "commit.gpgsign", "false")
	return dir
}

// writeCommit writes a file and commits it, answering the new commit's sha.
func writeCommit(t *testing.T, dir, path, content, subject string) string {
	t.Helper()
	full := filepath.Join(dir, path)
	if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
		t.Fatalf("mkdir for %s: %v", full, err)
	}
	if err := os.WriteFile(full, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", full, err)
	}
	gitAt(t, dir, "add", path)
	gitAt(t, dir, "commit", "--quiet", "-m", subject)
	return gitAt(t, dir, "rev-parse", "HEAD")
}

// seedRepo creates a repository on `branch` with one commit in it.
func seedRepo(t *testing.T, branch string) string {
	t.Helper()
	dir := initRepo(t, branch)
	writeCommit(t, dir, "README.md", "base\n", "base commit")
	return dir
}

// conflictRepo builds a repository whose `feature` branch and default branch
// changed the SAME line of the same file, so merging one into the other must
// conflict.
func conflictRepo(t *testing.T, branch string) (dir string, conflictedPath string) {
	t.Helper()
	dir = initRepo(t, branch)
	writeCommit(t, dir, "shared.txt", "original\n", "base commit")

	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "shared.txt", "feature side\n", "feature edit")

	gitAt(t, dir, "checkout", "--quiet", branch)
	writeCommit(t, dir, "shared.txt", "target side\n", "target edit")

	return dir, "shared.txt"
}

// recordFor finds the first captured record with that operation and level.
func recordFor(records []dlog.Record, level, operation string) (dlog.Record, bool) {
	for _, record := range records {
		if record.Level == level && record.Operation == operation {
			return record, true
		}
	}
	return dlog.Record{}, false
}
