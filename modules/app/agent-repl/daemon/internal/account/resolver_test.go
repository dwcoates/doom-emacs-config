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
