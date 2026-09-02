package dlog

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// newWorkspace makes a temp workspace directory and its log id.
func newWorkspace(t *testing.T) (dir, id string) {
	t.Helper()
	dir = t.TempDir()
	id, err := LogWorkspaceID(dir)
	if err != nil {
		t.Fatalf("LogWorkspaceID: %v", err)
	}
	return dir, id
}

func TestOpenSinkLinksToAnExternalTarget(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)

	// Act.
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })

	// Assert.
	link := filepath.Join(dir, ".claude", "emacs", "daemon.log")
	info, err := os.Lstat(link)
	if err != nil {
		t.Fatalf("lstat %s: %v", link, err)
	}
	if info.Mode()&os.ModeSymlink == 0 {
		t.Fatalf("%s is not a symlink; the canonical path must name an external target", link)
	}
	dest, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}
	if dest != s.target {
		t.Fatalf("link names %q, want the owned target %q", dest, s.target)
	}
	if strings.HasPrefix(dest, dir) {
		t.Fatalf("target %q lives inside the workspace; it must live under the OS temp dir", dest)
	}
}

func TestOpenSinkDisplacesAWorkspaceProvidedRegularFile(t *testing.T) {
	// Arrange: the workspace already has a regular file at the canonical path.
	dir, id := newWorkspace(t)
	linkDir := filepath.Join(dir, ".claude", "emacs")
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	link := filepath.Join(linkDir, "daemon.log")
	if err := os.WriteFile(link, []byte("someone else's file\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := s.write([]byte("{\"daemon\":1}\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert: the record went to the owned target, never into the foreign file.
	info, err := os.Lstat(link)
	if err != nil {
		t.Fatalf("lstat: %v", err)
	}
	if info.Mode()&os.ModeSymlink == 0 {
		t.Fatalf("the workspace-provided regular file was followed instead of displaced")
	}
	target, err := os.ReadFile(s.target)
	if err != nil {
		t.Fatalf("read target: %v", err)
	}
	if !strings.Contains(string(target), "\"daemon\":1") {
		t.Fatalf("target = %q, want the record", target)
	}
	if strings.Contains(string(target), "someone else's") {
		t.Fatalf("the foreign file's content leaked into the owned target")
	}
}

func TestOpenSinkReplacesAForeignSymlink(t *testing.T) {
	// Arrange: the canonical path already points somewhere of the workspace's
	// choosing.
	dir, id := newWorkspace(t)
	linkDir := filepath.Join(dir, ".claude", "emacs")
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	foreign := filepath.Join(t.TempDir(), "foreign.log")
	if err := os.WriteFile(foreign, nil, 0o644); err != nil {
		t.Fatalf("write foreign: %v", err)
	}
	link := filepath.Join(linkDir, "daemon.log")
	if err := os.Symlink(foreign, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })

	// Assert.
	dest, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}
	if dest == foreign {
		t.Fatalf("the foreign symlink was followed; the daemon must own its target")
	}
	if dest != s.target {
		t.Fatalf("link names %q, want %q", dest, s.target)
	}
}

func TestSinkWriteAppends(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })

	// Act.
	for _, line := range []string{"{\"n\":1}\n", "{\"n\":2}\n"} {
		if err := s.write([]byte(line)); err != nil {
			t.Fatalf("write: %v", err)
		}
	}

	// Assert.
	got, err := os.ReadFile(s.target)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(got) != "{\"n\":1}\n{\"n\":2}\n" {
		t.Fatalf("target = %q, want both records appended in order", got)
	}
}

func TestSinkTruncatesInPlaceAtTheCap(t *testing.T) {
	// Arrange: put the sink just under the cap without writing 64 MiB.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := s.f.Truncate(CapBytes); err != nil {
		t.Fatalf("grow the target: %v", err)
	}
	s.size = CapBytes
	before, err := os.Stat(s.target)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}

	// Act.
	if err := s.write([]byte("{\"after\":\"cap\"}\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert: the same inode, cleared, now holding only the new record.
	after, err := os.Stat(s.target)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if !os.SameFile(before, after) {
		t.Fatalf("the target was replaced; readers holding it open must keep the same inode")
	}
	got, err := os.ReadFile(s.target)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(got) != "{\"after\":\"cap\"}\n" {
		t.Fatalf("target = %q, want only the post-truncation record", got)
	}
}

func TestSinkRefusesToTruncateAnInodeItNoLongerOwns(t *testing.T) {
	// Arrange: the workspace redirects the canonical link elsewhere.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	if err := os.Symlink(filepath.Join(t.TempDir(), "elsewhere.log"), s.link); err != nil {
		t.Fatalf("symlink: %v", err)
	}
	s.size = CapBytes

	// Act.
	err = s.write([]byte("{\"n\":1}\n"))

	// Assert.
	if err == nil {
		t.Fatalf("write succeeded; cap maintenance must refuse an inode the link no longer names")
	}
	if !errors.Is(err, ErrPoisoned) {
		t.Fatalf("error = %v, want a poisoned sink", err)
	}
}

func TestSinkPoisonIsWorkspaceAttributed(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	s.size = CapBytes

	// Act.
	err = s.write([]byte("{\"n\":1}\n"))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), id) || !strings.Contains(err.Error(), dir) {
		t.Fatalf("error = %v, want the workspace id %q and dir %q named", err, id, dir)
	}
}

func TestSinkRefusesEveryRecordOncePoisoned(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "daemon", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	s.size = CapBytes
	first := s.write([]byte("{\"n\":1}\n"))
	if first == nil {
		t.Fatalf("the sink was not poisoned")
	}

	// Act: a healthy-looking, small record after the poison.
	second := s.write([]byte("{\"n\":2}\n"))

	// Assert.
	if second == nil {
		t.Fatalf("a poisoned sink accepted a record; it must refuse every further one")
	}
	if !errors.Is(second, ErrPoisoned) {
		t.Fatalf("error = %v, want ErrPoisoned", second)
	}
}

func TestSinkScanSeesWritesTheDaemonNeverMade(t *testing.T) {
	// Arrange: the shim writes straight to the same inode through fd 3, so the
	// daemon's own byte count is only a lower bound.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "shim", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	shim, err := os.OpenFile(s.target, os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatalf("open as the shim would: %v", err)
	}
	defer shim.Close()
	if err := shim.Truncate(CapBytes + 1); err != nil {
		t.Fatalf("grow past the cap: %v", err)
	}

	// Act.
	if err := s.scan(); err != nil {
		t.Fatalf("scan: %v", err)
	}

	// Assert.
	info, err := os.Stat(s.target)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if info.Size() != 0 {
		t.Fatalf("size after the scan = %d, want 0 — the scan is the only thing that sees fd-3 writes", info.Size())
	}
}

func TestSinkScanLeavesAnUnderCapTargetAlone(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(dir, id, "shim", "")
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := s.write([]byte("{\"n\":1}\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	if err := s.scan(); err != nil {
		t.Fatalf("scan: %v", err)
	}

	// Assert.
	got, err := os.ReadFile(s.target)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(got) != "{\"n\":1}\n" {
		t.Fatalf("target = %q, want the record untouched", got)
	}
}

func TestReplaceLinkIsAtomicOverAnExistingLink(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	link := filepath.Join(dir, "daemon.log")
	first := filepath.Join(dir, "first")
	second := filepath.Join(dir, "second")
	if err := replaceLink(link, first); err != nil {
		t.Fatalf("first replaceLink: %v", err)
	}

	// Act.
	if err := replaceLink(link, second); err != nil {
		t.Fatalf("second replaceLink: %v", err)
	}

	// Assert.
	dest, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}
	if dest != second {
		t.Fatalf("link names %q, want %q", dest, second)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("readdir: %v", err)
	}
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), ".daemon.log.") {
			t.Fatalf("temporary link %q was left behind", e.Name())
		}
	}
}
