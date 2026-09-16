package account_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
)

// newResolver builds a resolver over the two roots for a test.
func newResolver(t *testing.T, roots account.Roots) account.Resolver {
	t.Helper()
	r, err := account.New(roots, dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("account.New() = %v, want nil", err)
	}
	return r
}

func TestConfigDirForRouting(t *testing.T) {
	tests := []struct {
		name          string
		multiRepoRoot string
		workspaceDir  string
		want          string
	}{
		{
			name:          "directly under the root",
			multiRepoRoot: "/home/user/multi",
			workspaceDir:  "/home/user/multi/repoA/proj",
			want:          "/roots/multi",
		},
		{
			name:          "the root itself",
			multiRepoRoot: "/home/user/multi",
			workspaceDir:  "/home/user/multi",
			want:          "/roots/multi",
		},
		{
			name:          "outside the root",
			multiRepoRoot: "/home/user/multi",
			workspaceDir:  "/home/user/personal/proj",
			want:          "/roots/default",
		},
		{
			name:          "a sibling whose name merely starts with the root's",
			multiRepoRoot: "/home/user/multi",
			workspaceDir:  "/home/user/multi-other/proj",
			want:          "/roots/default",
		},
		{
			name:          "a repo that CONTAINS the root is not under it",
			multiRepoRoot: "/home/user/multi",
			workspaceDir:  "/home/user",
			want:          "/roots/default",
		},
		{
			name:          "an unnamed root routes everything to the default",
			multiRepoRoot: "",
			workspaceDir:  "/home/user/multi/repoA",
			want:          "/roots/default",
		},
		{
			name:          "an uncleaned path still routes by segment",
			multiRepoRoot: "/home/user/multi/",
			workspaceDir:  "/home/user/multi/./repoA/../repoB",
			want:          "/roots/multi",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := newResolver(t, account.Roots{
				Default:       "/roots/default",
				MultiRepo:     "/roots/multi",
				MultiRepoRoot: tc.multiRepoRoot,
			})

			// Act.
			got := r.ConfigDirFor(tc.workspaceDir)

			// Assert.
			if got != tc.want {
				t.Fatalf("ConfigDirFor(%q) = %q, want %q", tc.workspaceDir, got, tc.want)
			}
		})
	}
}

func TestConfigDirForResolvesSymlinkedWorkspace(t *testing.T) {
	// Arrange: a real multi-repo root with a real workspace inside it, reached
	// through a symlink that lives OUTSIDE the root.
	base := t.TempDir()
	root := filepath.Join(base, "multi")
	real := filepath.Join(root, "repoA")
	if err := os.MkdirAll(real, 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	link := filepath.Join(base, "elsewhere")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("Symlink() = %v", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: root})

	// Act.
	got := r.ConfigDirFor(link)

	// Assert.
	if got != "/roots/multi" {
		t.Fatalf("ConfigDirFor(symlink) = %q, want %q", got, "/roots/multi")
	}
}

func TestConfigDirForResolvesSymlinkedRoot(t *testing.T) {
	// Arrange: the CONFIGURED root is a symlink; a workspace under the real
	// directory must still route to the multi-repo account.
	base := t.TempDir()
	real := filepath.Join(base, "real-multi")
	ws := filepath.Join(real, "repoA")
	if err := os.MkdirAll(ws, 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	link := filepath.Join(base, "linked-multi")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("Symlink() = %v", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: link})

	// Act.
	got := r.ConfigDirFor(ws)

	// Assert.
	if got != "/roots/multi" {
		t.Fatalf("ConfigDirFor() = %q, want %q", got, "/roots/multi")
	}
}

// volumeIsCaseInsensitive probes the volume backing base by creating a
// mixed-case directory and asking whether its lower-cased spelling stats to the
// same inode. It is a real probe, not an assumption, so a case-skew test can
// skip-with-reason on a case-sensitive volume rather than silently pass.
func volumeIsCaseInsensitive(t *testing.T, base string) bool {
	t.Helper()
	upper := filepath.Join(base, "CaseProbe")
	if err := os.MkdirAll(upper, 0o700); err != nil {
		t.Fatalf("MkdirAll(probe) = %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(upper) })
	upperInfo, err := os.Stat(upper)
	if err != nil {
		t.Fatalf("Stat(probe) = %v", err)
	}
	lowerInfo, err := os.Stat(filepath.Join(base, "caseprobe"))
	if err != nil {
		return false // lower-cased spelling does not resolve: case-sensitive.
	}
	return os.SameFile(upperInfo, lowerInfo)
}

func TestConfigDirForRoutesDifferentlyCasedRealPathToMultiRepo(t *testing.T) {
	// Arrange: the exact observed defect. The multi-repo root is recorded with
	// one casing and a real workspace under it is opened through a
	// differently-cased path to the SAME on-disk directory. Byte-wise prefix
	// routing sent this to the personal account; the SameFile inode test must
	// route it to the work account.
	base := t.TempDir()
	if !volumeIsCaseInsensitive(t, base) {
		t.Skip("volume is case-sensitive; the case-skew defect cannot manifest here")
	}
	root := filepath.Join(base, "ChessCom")
	sub := filepath.Join(root, "explanation-engine-worktrees", "iterm-1")
	if err := os.MkdirAll(sub, 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	lowerCasedDir := filepath.Join(base, "chesscom", "explanation-engine-worktrees", "iterm-1")
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: root})

	// Act.
	got := r.ConfigDirFor(lowerCasedDir)

	// Assert.
	if got != "/roots/multi" {
		t.Fatalf("ConfigDirFor(%q) = %q, want %q (differently-cased real path must route to the work account)", lowerCasedDir, got, "/roots/multi")
	}
}

func TestConfigDirForRoutesUnrelatedRealSiblingToDefault(t *testing.T) {
	// Arrange: a real root and a genuinely-unrelated real sibling directory
	// that is NOT under it.
	base := t.TempDir()
	root := filepath.Join(base, "multi")
	sibling := filepath.Join(base, "personal", "proj")
	if err := os.MkdirAll(root, 0o700); err != nil {
		t.Fatalf("MkdirAll(root) = %v", err)
	}
	if err := os.MkdirAll(sibling, 0o700); err != nil {
		t.Fatalf("MkdirAll(sibling) = %v", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: root})

	// Act.
	got := r.ConfigDirFor(sibling)

	// Assert.
	if got != "/roots/default" {
		t.Fatalf("ConfigDirFor(%q) = %q, want %q", sibling, got, "/roots/default")
	}
}

func TestConfigDirForRoutesAboutToBeCreatedDirUnderExistingRootToMultiRepo(t *testing.T) {
	// Arrange: the root exists on disk but the workspace directory does not yet
	// (the daemon is about to create it). The existing-ancestor inode walk must
	// still recognize it as under the root.
	base := t.TempDir()
	root := filepath.Join(base, "multi")
	if err := os.MkdirAll(root, 0o700); err != nil {
		t.Fatalf("MkdirAll(root) = %v", err)
	}
	notYetCreated := filepath.Join(root, "brand-new-repo", "proj")
	if _, err := os.Stat(notYetCreated); !os.IsNotExist(err) {
		t.Fatalf("Stat(notYetCreated) = %v, want a not-exist error so the test covers the missing-path path", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: root})

	// Act.
	got := r.ConfigDirFor(notYetCreated)

	// Assert.
	if got != "/roots/multi" {
		t.Fatalf("ConfigDirFor(%q) = %q, want %q (an about-to-be-created dir under an existing root routes to work)", notYetCreated, got, "/roots/multi")
	}
}

func TestConfigDirForEmptyMultiRepoRootRoutesRealDirToDefault(t *testing.T) {
	// Arrange: an unnamed multi-repo root routes everything to the default,
	// even a real directory that would otherwise look routable.
	base := t.TempDir()
	ws := filepath.Join(base, "any", "workspace")
	if err := os.MkdirAll(ws, 0o700); err != nil {
		t.Fatalf("MkdirAll() = %v", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: ""})

	// Act.
	got := r.ConfigDirFor(ws)

	// Assert.
	if got != "/roots/default" {
		t.Fatalf("ConfigDirFor(%q) = %q, want %q", ws, got, "/roots/default")
	}
}

func TestConfigDirForRealSegmentBoundarySiblingRoutesToDefault(t *testing.T) {
	// Arrange: the segment-boundary guard must survive the inode-based test.
	// `.../multi-other` shares a name PREFIX with `.../multi` but is a distinct
	// directory, so it is not under the root even with both on disk.
	base := t.TempDir()
	root := filepath.Join(base, "multi")
	sibling := filepath.Join(base, "multi-other", "proj")
	if err := os.MkdirAll(root, 0o700); err != nil {
		t.Fatalf("MkdirAll(root) = %v", err)
	}
	if err := os.MkdirAll(sibling, 0o700); err != nil {
		t.Fatalf("MkdirAll(sibling) = %v", err)
	}
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi", MultiRepoRoot: root})

	// Act.
	got := r.ConfigDirFor(sibling)

	// Assert.
	if got != "/roots/default" {
		t.Fatalf("ConfigDirFor(%q) = %q, want %q (a name-prefix sibling is not under the root)", sibling, got, "/roots/default")
	}
}

func TestReadPresent(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	writeIdentity(t, dir, `{"oauthAccount":{"emailAddress":"who@example.com"},"other":[1,2,3]}`)
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: "/roots/multi"})

	// Act.
	got, err := r.Read(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("Read() = %v, want nil", err)
	}
	if got.Email != "who@example.com" || !got.LoggedIn || got.ConfigDir != dir {
		t.Fatalf("Read() = %+v, want who@example.com logged in under %s", got, dir)
	}
}

func TestReadAbsentIsLoggedOut(t *testing.T) {
	// Arrange: a config root that has never been logged into.
	dir := t.TempDir()
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: "/roots/multi"})

	// Act.
	got, err := r.Read(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("Read() = %v, want nil (logged out is a state, not a failure)", err)
	}
	if got.LoggedIn || got.Email != "" {
		t.Fatalf("Read() = %+v, want logged out", got)
	}
}

func TestReadNoEmailIsLoggedOut(t *testing.T) {
	// Arrange: the file exists but names no account.
	dir := t.TempDir()
	writeIdentity(t, dir, `{"oauthAccount":{}}`)
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: "/roots/multi"})

	// Act.
	got, err := r.Read(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("Read() = %v, want nil", err)
	}
	if got.LoggedIn {
		t.Fatalf("Read() = %+v, want logged out", got)
	}
}

func TestReadMalformedIsAnError(t *testing.T) {
	// Arrange: a corrupt install, which must never read as logged out.
	dir := t.TempDir()
	writeIdentity(t, dir, `{"oauthAccount":`)
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: "/roots/multi"})

	// Act.
	_, err := r.Read(context.Background(), dir)

	// Assert.
	if err == nil {
		t.Fatal("Read() = nil error, want a parse failure")
	}
}

func TestReadRejectsEmptyConfigDir(t *testing.T) {
	// Arrange.
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi"})

	// Act.
	_, err := r.Read(context.Background(), "")

	// Assert.
	if err == nil {
		t.Fatal("Read(\"\") = nil error, want a refusal")
	}
}

func TestReadHonorsCancelledContext(t *testing.T) {
	// Arrange.
	r := newResolver(t, account.Roots{Default: "/roots/default", MultiRepo: "/roots/multi"})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := r.Read(ctx, "/roots/default")

	// Assert.
	if err == nil {
		t.Fatal("Read() = nil error, want the context's error")
	}
}

func TestRosterIsBothRoots(t *testing.T) {
	// Arrange.
	def := t.TempDir()
	multi := t.TempDir()
	writeIdentity(t, def, `{"oauthAccount":{"emailAddress":"a@example.com"}}`)
	writeIdentity(t, multi, `{"oauthAccount":{"emailAddress":"b@example.com"}}`)
	r := newResolver(t, account.Roots{Default: def, MultiRepo: multi})

	// Act.
	got, err := r.Roster(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Roster() = %v, want nil", err)
	}
	if len(got) != 2 || got[0].Email != "a@example.com" || got[1].Email != "b@example.com" {
		t.Fatalf("Roster() = %+v, want the default root first then the multi-repo root", got)
	}
}

func TestRosterDeduplicatesOneRootConfiguredTwice(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	writeIdentity(t, dir, `{"oauthAccount":{"emailAddress":"a@example.com"}}`)
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: dir})

	// Act.
	got, err := r.Roster(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Roster() = %v, want nil", err)
	}
	if len(got) != 1 {
		t.Fatalf("Roster() = %d rows, want 1", len(got))
	}
}

func TestRosterFailsWholeWhenOneRootIsMalformed(t *testing.T) {
	// Arrange: a partial roster would draw an unreadable account as absent.
	def := t.TempDir()
	multi := t.TempDir()
	writeIdentity(t, def, `{"oauthAccount":{"emailAddress":"a@example.com"}}`)
	writeIdentity(t, multi, `not json at all`)
	r := newResolver(t, account.Roots{Default: def, MultiRepo: multi})

	// Act.
	got, err := r.Roster(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("Roster() = nil error, want the malformed root's failure")
	}
	if got != nil {
		t.Fatalf("Roster() = %+v, want nothing loaded", got)
	}
}

// writeIdentity plants a .claude.json in a config root.
func writeIdentity(t *testing.T, configDir, body string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(configDir, ".claude.json"), []byte(body), 0o600); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
}

func TestReadNeverWritesTheIdentityFileOrTheRoot(t *testing.T) {
	// Arrange: a config root the process cannot write to at all, so any write
	// the daemon attempted would fail loudly instead of passing unnoticed.
	// The daemon READS oauthAccount.emailAddress and nothing else; it never
	// writes .claude.json and never touches the CLI's projects.<cwd> entry.
	dir := t.TempDir()
	writeIdentity(t, dir, `{"oauthAccount":{"emailAddress":"who@example.com"},"projects":{"/home/user/proj":{"allowedTools":[]}}}`)
	before, err := os.ReadFile(filepath.Join(dir, ".claude.json"))
	if err != nil {
		t.Fatalf("reading the identity file back = %v, want nil", err)
	}
	if err := os.Chmod(dir, 0o500); err != nil {
		t.Fatalf("making the config root read-only = %v, want nil", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o700) })
	r := newResolver(t, account.Roots{Default: dir, MultiRepo: "/roots/multi"})

	// Act.
	got, err := r.Read(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("Read() = %v, want nil", err)
	}
	if got.Email != "who@example.com" {
		t.Fatalf("Read() = %+v, want who@example.com", got)
	}
	after, err := os.ReadFile(filepath.Join(dir, ".claude.json"))
	if err != nil {
		t.Fatalf("reading the identity file after Read = %v, want nil", err)
	}
	if string(after) != string(before) {
		t.Fatalf("identity file changed across Read:\n before = %s\n after  = %s", before, after)
	}
}

func TestIsMultiRepo(t *testing.T) {
	tests := []struct {
		name      string
		default_  string
		multiRepo string
		configDir string
		want      bool
	}{
		{
			name:      "the multi-repo root is the work account",
			default_:  "/roots/default",
			multiRepo: "/roots/multi",
			configDir: "/roots/multi",
			want:      true,
		},
		{
			name:      "the default root is personal",
			default_:  "/roots/default",
			multiRepo: "/roots/multi",
			configDir: "/roots/default",
			want:      false,
		},
		{
			name:      "an unrelated dir is not the work account",
			default_:  "/roots/default",
			multiRepo: "/roots/multi",
			configDir: "/roots/other",
			want:      false,
		},
		{
			name:      "an empty config dir is not the work account",
			default_:  "/roots/default",
			multiRepo: "/roots/multi",
			configDir: "",
			want:      false,
		},
		{
			name:      "one root for both accounts has no distinct work account",
			default_:  "/roots/only",
			multiRepo: "/roots/only",
			configDir: "/roots/only",
			want:      false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := newResolver(t, account.Roots{
				Default:   tc.default_,
				MultiRepo: tc.multiRepo,
			})

			// Act.
			got := r.IsMultiRepo(tc.configDir)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsMultiRepo(%q) = %v, want %v", tc.configDir, got, tc.want)
			}
		})
	}
}
