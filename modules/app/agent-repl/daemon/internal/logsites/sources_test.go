package logsites

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

func TestStateLogsOwnsEverySinkAndItsGenerations(t *testing.T) {
	tests := []struct {
		base string
		want bool
	}{
		{"daemon.run.log", true},
		{"daemon.run.log.3", true},
		{"thaw-guard.run.log", true},
		{"emacs.central.log", true},
		{"emacs.central.log.5", true},
		{"agent-repl-0123456789abcdef-daemon-12345.log", true},
		{"agent-repl-0123456789abcdef-shim-x9.log.1", true},
		{"agent-repl-99808d49-webapp-42.log", true},
		{"agent-repl-0123456789abcdef-sidecar-7.log", true},
		{"agent-repl-0123456789abcdef-emacs-abc.log", true},
		{"daemon.run.warn.log", false},
		{"agent-repl-0123456789abcdef-webapp-42.warn.log", false},
		{"daemon.stdio.log", false},
		{"agent-repl-0123456789abcdef-notifier-1.log", false},
		{"daemon.run.log.0", false},
		{"daemon.run.log.bak", false},
	}
	for _, tt := range tests {
		t.Run(tt.base, func(t *testing.T) {
			// Act
			got := StateLogs("/s").Owned(tt.base)

			// Assert
			if got != tt.want {
				t.Fatalf("StateLogs owns %q = %v, want %v", tt.base, got, tt.want)
			}
		})
	}
}

func TestCacheLogsOwnsEveryServicesOwnLog(t *testing.T) {
	tests := []struct {
		base string
		want bool
	}{
		{"shim-store.log", true},
		{"shim-store.log.2", true},
		{"shim-claude-sidecar.log", true},
		{"zettel.log", true},
		{"tests.log", true},
		{"sockethold-daemon.log", true},
		{"shim-store.warn.log", false},
		{"shim-store.err.log", false},
		{"shim-store.out.log", false},
		{"victoriametrics.log", false},
		{"notifier.log", false},
	}
	for _, tt := range tests {
		t.Run(tt.base, func(t *testing.T) {
			// Act
			got := CacheLogs("/c").Owned(tt.base)

			// Assert
			if got != tt.want {
				t.Fatalf("CacheLogs owns %q = %v, want %v", tt.base, got, tt.want)
			}
		})
	}
}

func TestListSourcesAnswersEveryOwnedRegularFileSorted(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	for _, name := range []string{"emacs.central.log", "daemon.run.log.1", "daemon.run.log", "daemon.run.warn.log"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte("x\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.Mkdir(filepath.Join(dir, "agent-repl-0a-daemon-1.log"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(dir, "daemon.run.log"), filepath.Join(dir, "agent-repl-0a-shim-1.log")); err != nil {
		t.Fatal(err)
	}

	// Act
	sources, err := listSources([]Dir{StateLogs(dir), StateLogs(filepath.Join(dir, "absent"))})

	// Assert
	var got []string
	for _, s := range sources {
		got = append(got, filepath.Base(s.path))
	}
	want := []string{"daemon.run.log", "daemon.run.log.1", "emacs.central.log"}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("listSources = (%v, %v), want %v: every owned regular file, no warnings twin, directory or link", got, err, want)
	}
}

func TestListSourcesFailsOnADirectoryItCannotList(t *testing.T) {
	// Arrange
	file := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := listSources([]Dir{StateLogs(file)})

	// Assert
	if err == nil {
		t.Fatal("listing a file as a directory succeeded, want an error")
	}
}

func TestAFileKeepsItsIdentityAcrossARenameAndANewFileHasItsOwn(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "a.log")
	if err := os.WriteFile(path, []byte("x\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	identity := func(p string) string {
		t.Helper()
		info, err := os.Lstat(p)
		if err != nil {
			t.Fatal(err)
		}
		id, err := identityOf(p, info)
		if err != nil {
			t.Fatal(err)
		}
		return id
	}
	before := identity(path)

	// Act
	if err := os.Rename(path, path+".1"); err != nil {
		t.Fatal(err)
	}
	renamed := identity(path + ".1")
	if err := os.WriteFile(path, []byte("y\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	fresh := identity(path)

	// Assert
	if renamed != before {
		t.Errorf("identity across a rename = %q, want %q", renamed, before)
	}
	if fresh == before {
		t.Errorf("a new file at the old path has the renamed file's identity %q", fresh)
	}
}
