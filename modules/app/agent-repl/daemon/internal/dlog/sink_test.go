package dlog

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// newWorkspace makes a temp workspace directory and its daemon-minted id.
func newWorkspace(t *testing.T) (dir, id string) {
	t.Helper()
	dir = t.TempDir()
	return dir, mintedTestID(dir)
}

func TestOpenSinkLinksToAnExternalTarget(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)

	// Act.
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
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
		t.Fatalf("target %q lives inside the workspace; it must live under the supplied logs directory", dest)
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
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
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
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
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
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
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

func TestDaemonOwnedSinksRotateWithGenerationsAtTheCap(t *testing.T) {
	for _, name := range []string{"daemon", "webapp", "sidecar"} {
		t.Run(name, func(t *testing.T) {
			// Arrange: one generation at its eight-byte cap and a reader holding
			// the inode that is about to retire.
			dir, id := newWorkspace(t)
			s, err := openSinkSized(t.TempDir(), dir, id, name, "", true, 8, 2)
			if err != nil {
				t.Fatalf("openSinkSized: %v", err)
			}
			t.Cleanup(func() { s.close(); os.Remove(s.target) })
			if err := s.write([]byte("old-one\n")); err != nil {
				t.Fatalf("write old generation: %v", err)
			}
			held, err := os.Open(s.target)
			if err != nil {
				t.Fatalf("open a reader on the old target: %v", err)
			}
			defer held.Close()
			oldInfo, err := held.Stat()
			if err != nil {
				t.Fatalf("stat old target: %v", err)
			}

			// Act.
			if err := s.write([]byte("new-one\n")); err != nil {
				t.Fatalf("write new generation: %v", err)
			}
			if err := s.write([]byte("new-two\n")); err != nil {
				t.Fatalf("write second new generation: %v", err)
			}

			// Assert: the canonical symlink resolves to a fresh inode, the two
			// configured generations are retained, and the held reader stays on
			// the oldest retained inode.
			currentInfo, err := os.Stat(s.link)
			if err != nil {
				t.Fatalf("stat canonical link: %v", err)
			}
			if os.SameFile(oldInfo, currentInfo) {
				t.Fatal("the canonical link still resolves to the retired inode")
			}
			heldBytes, err := io.ReadAll(held)
			if err != nil {
				t.Fatalf("read the held old target: %v", err)
			}
			if got := string(heldBytes); got != "old-one\n" {
				t.Fatalf("held target = %q, want the old generation", got)
			}
			if got, err := os.ReadFile(s.target + ".1"); err != nil || string(got) != "new-one\n" {
				t.Fatalf("generation .1 = %q, %v; want the immediately retired generation", got, err)
			}
			if got, err := os.ReadFile(s.target + ".2"); err != nil || string(got) != "old-one\n" {
				t.Fatalf("generation .2 = %q, %v; want the oldest retained generation", got, err)
			}
			if got, err := os.ReadFile(s.target); err != nil || string(got) != "new-two\n" {
				t.Fatalf("current target = %q, %v; want the new generation", got, err)
			}
		})
	}
}

func TestSinkRefusesToRepointALinkItNoLongerOwns(t *testing.T) {
	// Arrange: the workspace redirects the canonical link elsewhere.
	dir, id := newWorkspace(t)
	s, err := openSinkSized(t.TempDir(), dir, id, "daemon", "", true, 8, 2)
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
	if err := s.write([]byte("12345678")); err != nil {
		t.Fatalf("fill target: %v", err)
	}
	if s.size != 8 || s.cap != 8 {
		t.Fatalf("filled sink = size %d cap %d, want 8 and 8", s.size, s.cap)
	}

	// Act.
	err = s.write([]byte("{\"n\":1}\n"))

	// Assert.
	if err == nil {
		t.Fatalf("write succeeded; rotation must refuse a link the daemon no longer owns")
	}
	if !errors.Is(err, ErrPoisoned) {
		t.Fatalf("error = %v, want a poisoned sink", err)
	}
}

func TestSinkPoisonIsWorkspaceAttributed(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSinkSized(t.TempDir(), dir, id, "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	if err := s.write([]byte("12345678")); err != nil {
		t.Fatalf("fill target: %v", err)
	}

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
	s, err := openSinkSized(t.TempDir(), dir, id, "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	if err := s.write([]byte("12345678")); err != nil {
		t.Fatalf("fill target: %v", err)
	}
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

func TestSinkScanMarksWritesTheDaemonNeverMadeForTheNextShimRoll(t *testing.T) {
	// Arrange: the shim writes straight to the same inode through fd 3, so the
	// daemon's own byte count is only a lower bound.
	dir, id := newWorkspace(t)
	s, err := openSink(t.TempDir(), dir, id, "shim", "", true)
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
	result, err := s.scan()
	if err != nil {
		t.Fatalf("scan: %v", err)
	}

	// Assert: the target remains intact for fd 3 and is marked for the next
	// process roll instead of being truncated in place.
	info, err := os.Stat(s.target)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if info.Size() != CapBytes+1 {
		t.Fatalf("size after the scan = %d, want the fd-3 bytes left intact", info.Size())
	}
	if !result.marked || !s.rotatePending {
		t.Fatalf("scan result = %+v and rotatePending = %v, want the next shim roll marked", result, s.rotatePending)
	}
}

func TestSinkScanLeavesAnUnderCapTargetAlone(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(t.TempDir(), dir, id, "shim", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := s.write([]byte("{\"n\":1}\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	result, err := s.scan()
	if err != nil {
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
	if result.marked || s.rotatePending {
		t.Fatalf("scan result = %+v and rotatePending = %v, want no roll below the cap", result, s.rotatePending)
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

// TestCreateTargetMintsUnderTheGivenLogsDirectory pins WHERE a durable sink's
// target lives: the state root's logs directory, never the OS temp dir. A
// target under TMPDIR is swept by the operating system, differs per launcher,
// and leaves one orphan per run in a directory nothing owns.
func TestCreateTargetMintsUnderTheGivenLogsDirectory(t *testing.T) {
	// Arrange.
	logsDir := filepath.Join(t.TempDir(), "logs")

	// Act.
	target, err := createTarget(logsDir, "ws-abc", "daemon")

	// Assert.
	if err != nil {
		t.Fatalf("createTarget: %v", err)
	}
	if filepath.Dir(target) != logsDir {
		t.Fatalf("target = %q, want it minted under %q", target, logsDir)
	}
}

// TestCreateTargetRefusesWithNoLogsDirectory covers the invariant's other side:
// an unresolved logs directory is a loud failure, never a silent fall back to
// the OS temp dir.
func TestCreateTargetRefusesWithNoLogsDirectory(t *testing.T) {
	// Arrange, Act.
	_, err := createTarget("", "ws-abc", "daemon")

	// Assert.
	if err == nil {
		t.Fatal("createTarget with no logs directory = nil, want a loud refusal")
	}
}

// TestANewRuntimeAppendsToTheStandingTarget pins the realtest-1 finding: a new
// daemon instance used to mint a fresh generation and retarget the link at it,
// so the workspace's canonical daemon.log named only the current instance and
// the previous one's records were on an inode nothing named any more.
func TestANewRuntimeAppendsToTheStandingTarget(t *testing.T) {
	// Arrange: one runtime writes a record and closes its sink.
	dir, id := newWorkspace(t)
	logs := t.TempDir()
	first, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (first runtime): %v", err)
	}
	if err := first.write([]byte("{\"instance\":1}\n")); err != nil {
		t.Fatalf("write (first runtime): %v", err)
	}
	first.close()

	// Act: the NEXT runtime opens the same workspace sink knowing no target.
	second, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (second runtime): %v", err)
	}
	t.Cleanup(func() { second.close() })
	if err := second.write([]byte("{\"instance\":2}\n")); err != nil {
		t.Fatalf("write (second runtime): %v", err)
	}

	// Assert.
	if second.target != first.target {
		t.Fatalf("the second runtime opened %q, want the standing target %q", second.target, first.target)
	}
	body, err := os.ReadFile(second.target)
	if err != nil {
		t.Fatalf("read target: %v", err)
	}
	if !strings.Contains(string(body), "\"instance\":1") || !strings.Contains(string(body), "\"instance\":2") {
		t.Fatalf("target = %q, want both instances' records in one file", body)
	}
}

// TestANewRuntimeLeavesTheStandingLinkInPlace is the reader's half: the
// canonical path keeps naming the file that spans both instances, so
// `bin/logs.sh --workspace` sees the whole narrative.
func TestANewRuntimeLeavesTheStandingLinkInPlace(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	logs := t.TempDir()
	first, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (first runtime): %v", err)
	}
	first.close()
	link := filepath.Join(dir, ".claude", "emacs", "daemon.log")
	before, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}

	// Act.
	second, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (second runtime): %v", err)
	}
	t.Cleanup(func() { second.close() })

	// Assert.
	after, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}
	if after != before {
		t.Fatalf("the canonical link moved from %q to %q across instances", before, after)
	}
}

// TestANewRuntimeStartsAGenerationAtTheCap pins the one condition that DOES
// start a fresh file: a standing target already at the cap is a generation to
// roll, never one to join.
func TestANewRuntimeStartsAGenerationAtTheCap(t *testing.T) {
	// Arrange: an 8-byte cap and a standing target that has reached it.
	dir, id := newWorkspace(t)
	logs := t.TempDir()
	first, err := openSinkSized(logs, dir, id, "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSinkSized (first runtime): %v", err)
	}
	if err := first.write([]byte("12345678\n")); err != nil {
		t.Fatalf("write: %v", err)
	}
	first.close()

	// Act.
	second, err := openSinkSized(logs, dir, id, "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSinkSized (second runtime): %v", err)
	}
	t.Cleanup(func() { second.close() })

	// Assert.
	if second.target == first.target {
		t.Fatalf("the second runtime joined a target already at the cap (%q)", second.target)
	}
}

// TestANewRuntimeNeverAppendsToAForeignTarget pins that the appending rule
// does not weaken the ownership rule: a canonical link the workspace pointed
// somewhere of its own choosing is displaced, never joined.
func TestANewRuntimeNeverAppendsToAForeignTarget(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	linkDir := filepath.Join(dir, ".claude", "emacs")
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	foreign := filepath.Join(t.TempDir(), "foreign.log")
	if err := os.WriteFile(foreign, []byte("someone else's records\n"), 0o644); err != nil {
		t.Fatalf("write foreign: %v", err)
	}
	if err := os.Symlink(foreign, filepath.Join(linkDir, "daemon.log")); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close() })

	// Assert.
	if s.target == foreign {
		t.Fatalf("the foreign target %q was joined; the daemon must own its target", foreign)
	}
}

// TestANewRuntimeNeverAppendsToAWorkspaceProvidedRegularFile is the other
// ownership arm: a regular file at the canonical path is not a symlink to
// anything, so there is nothing to join.
func TestANewRuntimeNeverAppendsToAWorkspaceProvidedRegularFile(t *testing.T) {
	// Arrange.
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
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close() })

	// Assert.
	if s.target == link {
		t.Fatalf("the workspace-provided regular file %q was joined", link)
	}
	body, err := os.ReadFile(s.target)
	if err != nil {
		t.Fatalf("read target: %v", err)
	}
	if strings.Contains(string(body), "someone else's") {
		t.Fatalf("the foreign file's content leaked into the owned target")
	}
}

// TestAStandingTargetThatIsGoneIsNotJoined pins the dangling link: the
// generation the last instance named has been swept, so there is nothing to
// append to and a fresh target is minted.
func TestAStandingTargetThatIsGoneIsNotJoined(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	logs := t.TempDir()
	first, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (first runtime): %v", err)
	}
	first.close()
	if err := os.Remove(first.target); err != nil {
		t.Fatalf("remove the standing target: %v", err)
	}

	// Act.
	second, err := openSink(logs, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink (second runtime): %v", err)
	}
	t.Cleanup(func() { second.close() })

	// Assert.
	if second.target == first.target {
		t.Fatalf("the second runtime joined the swept target %q", second.target)
	}
	if _, err := os.Stat(second.target); err != nil {
		t.Fatalf("stat the minted target: %v", err)
	}
}

// A newly minted target is named by the DAEMON-MINTED workspace id, so a sink
// file name and a log record name the workspace with the same characters.
func TestMintedTargetNameCarriesTheMintedWorkspaceID(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	logsDir := t.TempDir()

	// Act.
	s, err := openSink(logsDir, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close() })

	// Assert.
	base := filepath.Base(s.target)
	if !strings.HasPrefix(base, "agent-repl-"+id+"-daemon-") {
		t.Fatalf("target %q is not named agent-repl-<minted id>-daemon-*", base)
	}
	if !s.mintedTarget {
		t.Fatalf("the sink does not report having minted its target")
	}
}

// MIGRATION: a target an older daemon minted under the 8-character directory
// hash is APPENDED TO, not replaced. Renaming it or minting beside it would
// orphan every record written before the id scheme changed.
func TestOpenSinkKeepsAppendingToADirectoryHashNamedTarget(t *testing.T) {
	// Arrange: the canonical link already names an under-cap target whose
	// name carries the directory hash, exactly as an older daemon left it.
	dir, id := newWorkspace(t)
	logsDir := t.TempDir()
	hash, err := WorkspaceDirHash(dir)
	if err != nil {
		t.Fatalf("WorkspaceDirHash: %v", err)
	}
	legacy := filepath.Join(logsDir, "agent-repl-"+hash+"-daemon-123456.log")
	if err := os.WriteFile(legacy, []byte("{\"old\":true}\n"), 0o600); err != nil {
		t.Fatalf("write the legacy target: %v", err)
	}
	linkDir := filepath.Join(dir, ".claude", "emacs")
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.Symlink(legacy, filepath.Join(linkDir, "daemon.log")); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	s, err := openSink(logsDir, dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close() })
	if err := s.write([]byte("{\"new\":true}\n")); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert: the same file, still the only one, with both records in it.
	if s.target != legacy {
		t.Fatalf("target = %q, want the standing %q — the history must not be orphaned", s.target, legacy)
	}
	if s.mintedTarget {
		t.Fatalf("the sink minted a target although a standing one was joinable")
	}
	entries, err := os.ReadDir(logsDir)
	if err != nil {
		t.Fatalf("read the logs directory: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("the logs directory holds %d files, want only the standing target", len(entries))
	}
	raw, err := os.ReadFile(legacy)
	if err != nil {
		t.Fatalf("read the target: %v", err)
	}
	if want := "{\"old\":true}\n{\"new\":true}\n"; string(raw) != want {
		t.Fatalf("target contents = %q, want %q", raw, want)
	}
}

// TestCloseDoesNotPoisonTheSink pins that releasing the descriptor is not a
// failure: poison outlives the runtime and is reserved for what ErrPoisoned
// documents, so an evicted sink must be re-openable.
func TestCloseDoesNotPoisonTheSink(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { os.Remove(s.target) })

	// Act.
	if err := s.close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Assert.
	if poison := s.poisoned(); poison != nil {
		t.Fatalf("poison = %v, want a closed sink to carry none", poison)
	}
}

// TestAClosedSinkRefusesARecordWithoutPoisoning pins the guard on a handle
// retained somewhere else: the write is refused, and the refusal says the sink
// is closed rather than poisoned.
func TestAClosedSinkRefusesARecordWithoutPoisoning(t *testing.T) {
	// Arrange.
	dir, id := newWorkspace(t)
	s, err := openSink(t.TempDir(), dir, id, "daemon", "", true)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { os.Remove(s.target) })
	if err := s.close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	err = s.write([]byte("{\"n\":1}\n"))

	// Assert.
	if !errors.Is(err, ErrSinkClosed) {
		t.Fatalf("error = %v, want ErrSinkClosed", err)
	}
	if errors.Is(err, ErrPoisoned) {
		t.Fatalf("error = %v, want a closed sink not to report as poisoned", err)
	}
}

// TestAWriteFailureStillPoisonsAfterTheCloseChange pins the other half: the
// cap-maintenance failure ErrPoisoned exists for still takes the sink out of
// service for good.
func TestAWriteFailureStillPoisonsAfterTheCloseChange(t *testing.T) {
	// Arrange: a sink at its cap whose canonical link the daemon no longer
	// owns, so the rotation the next record needs must refuse.
	dir, id := newWorkspace(t)
	s, err := openSinkSized(t.TempDir(), dir, id, "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSink: %v", err)
	}
	t.Cleanup(func() { s.close(); os.Remove(s.target) })
	if err := os.Remove(s.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	if err := s.write([]byte("12345678")); err != nil {
		t.Fatalf("fill target: %v", err)
	}

	// Act.
	err = s.write([]byte("{\"n\":1}\n"))

	// Assert.
	if !errors.Is(err, ErrPoisoned) {
		t.Fatalf("error = %v, want ErrPoisoned", err)
	}
	if poison := s.poisoned(); poison == nil {
		t.Fatal("the sink did not stay poisoned after a failed write")
	}
}
