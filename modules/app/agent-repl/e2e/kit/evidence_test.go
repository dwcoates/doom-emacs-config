package kit

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// gdbCapture builds a capture in gdb's own shape: a preamble, an `info
// threads` table, then one `Thread N (...)` section per thread. `thread apply
// 1 bt` puts the main thread first; the `all` walk then repeats it last.
func gdbCapture(mainFrames int, helpers int) string {
	var b strings.Builder
	b.WriteString("[New LWP 650]\n")
	b.WriteString("0x0000ffffaf1b0cd4 in pselect () from /lib/libc.so.6\n")
	b.WriteString("  Id   Target Id                     Frame \n")
	b.WriteString("* 1    Thread 0xffffb87f9020 \"emacs\" pselect ()\n")
	for i := 0; i < helpers; i++ {
		b.WriteString(fmt.Sprintf("  %d    Thread 0xffff9b80e44%d \"gmain\" poll ()\n", i+2, i))
	}
	main := func() {
		b.WriteString("\nThread 1 (Thread 0xffffb87f9020 (LWP 628) \"emacs\"):\n")
		for i := 0; i < mainFrames; i++ {
			b.WriteString(fmt.Sprintf("#%d  0x0000aaaae9a26554 in mainframe%d () at thread.c:624\n", i, i))
		}
	}
	main()
	for i := 0; i < helpers; i++ {
		b.WriteString(fmt.Sprintf("\nThread %d (Thread 0xffff9b80e44%d (LWP 65%d) \"gmain\"):\n", i+2, i, i))
		for f := 0; f < 8; f++ {
			b.WriteString(fmt.Sprintf("#%d  0x0000ffffb80f7958 in helper%d () from /lib/libglib-2.0.so.0\n", f, f))
		}
	}
	main()
	return strings.TrimSuffix(b.String(), "\n")
}

func TestTheExcerptLeadsWithTheMainThread(t *testing.T) {
	t.Parallel()
	// Arrange, Act.
	got := NativeBacktraceExcerpt(gdbCapture(20, 3))

	// Assert: the first thread section quoted is thread 1's.
	first := ""
	for _, line := range got {
		if strings.HasPrefix(line, "Thread ") {
			first = line
			break
		}
	}
	if !strings.HasPrefix(first, "Thread 1 ") {
		t.Fatalf("first thread section = %q, want thread 1's", first)
	}
}

func TestTheExcerptKeepsEveryMainThreadFrame(t *testing.T) {
	t.Parallel()
	// Arrange: a main stack far longer than the trim budget.
	frames := NativeBacktraceHeadFrames * 3

	// Act.
	got := strings.Join(NativeBacktraceExcerpt(gdbCapture(frames, 3)), "\n")

	// Assert.
	for i := 0; i < frames; i++ {
		if !strings.Contains(got, fmt.Sprintf("mainframe%d ", i)) {
			t.Fatalf("frame %d of the main thread is missing from the excerpt:\n%s", i, got)
		}
	}
}

func TestTheExcerptQuotesTheMainThreadOnlyOnce(t *testing.T) {
	t.Parallel()
	// Arrange, Act: gdb prints thread 1 twice, once per `bt` command.
	got := NativeBacktraceExcerpt(gdbCapture(5, 2))

	// Assert.
	headers := 0
	for _, line := range got {
		if strings.HasPrefix(line, "Thread 1 ") {
			headers++
		}
	}
	if headers != 1 {
		t.Fatalf("thread 1 sections = %d, want the repeat dropped", headers)
	}
}

func TestTheExcerptKeepsTheThreadIndex(t *testing.T) {
	t.Parallel()
	// Arrange, Act.
	got := strings.Join(NativeBacktraceExcerpt(gdbCapture(5, 3)), "\n")

	// Assert: gdb's `info threads` table is the index to what follows.
	if !strings.Contains(got, "Id   Target Id") {
		t.Fatalf("excerpt lost the info-threads table:\n%s", got)
	}
}

func TestTheExcerptTrimsHelperThreadsAndSaysSo(t *testing.T) {
	t.Parallel()
	// Arrange: more helper stacks than the budget past the main thread.
	// Act.
	got := strings.Join(NativeBacktraceExcerpt(gdbCapture(5, 12)), "\n")

	// Assert.
	if !strings.Contains(got, "more lines in "+NativeBacktraceFile) {
		t.Fatalf("excerpt trimmed silently:\n%s", got)
	}
}

func TestTheExcerptTrimsAHelperThreadWholeOrNotAtAll(t *testing.T) {
	t.Parallel()
	// Arrange, Act.
	got := NativeBacktraceExcerpt(gdbCapture(5, 12))

	// Assert: a quoted helper section carries all eight of its frames.
	frames := 0
	inHelper := false
	for _, line := range got {
		if strings.HasPrefix(line, "Thread ") {
			if inHelper && frames != 8 {
				t.Fatalf("a helper thread was quoted with %d frames, want a whole stack", frames)
			}
			inHelper = !strings.HasPrefix(line, NativeBacktraceMainThread)
			frames = 0
			continue
		}
		if inHelper && strings.HasPrefix(line, "#") {
			frames++
		}
	}
}

func TestAnUnthreadedCaptureIsQuotedWhole(t *testing.T) {
	t.Parallel()
	// Arrange: gdb said something, but named no thread — a failed attach.
	text := "ptrace: Operation not permitted.\n(gdb exited with: exit status 1)"

	// Act.
	got := strings.Join(NativeBacktraceExcerpt(text), "\n")

	// Assert.
	if got != text {
		t.Fatalf("excerpt = %q, want the capture verbatim", got)
	}
}

// TestLogTailOfNamesTheChildsLastRecords pins what a failed stop prints: the
// end of the child's own log, or why it cannot be read -- never nothing.
func TestLogTailOfNamesTheChildsLastRecords(t *testing.T) {
	dir := t.TempDir()
	present := filepath.Join(dir, "store.log")
	if err := os.WriteFile(present, []byte(strings.Repeat("x", StopLogTailBytes)+"\nreceived signal=terminated\n"), 0o644); err != nil {
		t.Fatalf("write the log: %v", err)
	}
	tests := []struct {
		name string
		path string
		want string
	}{
		{name: "a readable log answers its last record", path: present, want: "received signal=terminated"},
		{name: "a missing log answers why it cannot be read", path: filepath.Join(dir, "absent.log"), want: "cannot read"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := LogTailOf(tt.path)

			// Assert
			if !strings.Contains(got, tt.want) {
				t.Errorf("LogTailOf(%s) = %q, want it to contain %q", tt.path, got, tt.want)
			}
		})
	}
}

// tailArtifactsSaid runs TailArtifacts over root and answers what it said.
func tailArtifactsSaid(root string) []string {
	var said []string
	TailArtifacts(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) }, root)
	return said
}

// writeArtifact writes one collected file under root.
func writeArtifact(t *testing.T, root, rel, body string) {
	t.Helper()
	path := filepath.Join(root, rel)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestTailArtifactsPrintsEveryCollectedLog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "state/logs/daemon.run.log", "{\"operation\":\"daemon.marker\"}\n")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if !strings.Contains(strings.Join(said, "\n"), "daemon.marker") {
		t.Fatalf("said %q, want the daemon run log's tail", said)
	}
}

func TestTailArtifactsPrintsEmacsMessagesFirst(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "a-first-by-name.log", "other\n")
	writeArtifact(t, root, MessagesFile, "messages\n")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if len(said) == 0 || !strings.Contains(said[0], MessagesFile) {
		t.Fatalf("first tail = %q, want Emacs's *Messages*", said)
	}
}

func TestTailArtifactsSkipsWhatIsNotALog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "fakegit.json", "{\"marker\":\"fixture\"}")

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if strings.Contains(strings.Join(said, "\n"), "fixture") {
		t.Fatalf("said %q, want the fixture file left out of the tails", said)
	}
}

func TestTailArtifactsBoundsOneLog(t *testing.T) {
	// Arrange
	root := t.TempDir()
	writeArtifact(t, root, "big.log", strings.Repeat("0123456789abcde\n", 4*ArtifactTailFileBytes/16))

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if len(said) == 0 || len(said[0]) > ArtifactTailFileBytes+256 {
		t.Fatalf("one tail is %d bytes, want at most %d plus its header", len(said[0]), ArtifactTailFileBytes)
	}
}

func TestTailArtifactsNamesALogPastTheBudget(t *testing.T) {
	// Arrange
	root := t.TempDir()
	line := strings.Repeat("x", 1023) + "\n"
	for i := 0; i <= 2*ArtifactTailBudget/ArtifactTailFileBytes; i++ {
		writeArtifact(t, root, fmt.Sprintf("sink-%03d.log", i), strings.Repeat(line, ArtifactTailFileBytes/1024+1))
	}

	// Act
	said := tailArtifactsSaid(root)

	// Assert
	if !strings.Contains(strings.Join(said, "\n"), "not printed: the tail budget") {
		t.Fatalf("no log was named as past the budget; a dropped tail must still be said")
	}
}

func TestReadCgroupCPUStatReadsEveryCounter(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "cpu.stat")
	writeArtifact(t, filepath.Dir(path), "cpu.stat", "usage_usec 1500\nnr_throttled 3\nthrottled_usec 900\n")

	// Act
	got, err := ReadCgroupCPUStat(path)

	// Assert
	if err != nil || got["usage_usec"] != 1500 || got["nr_throttled"] != 3 || got["throttled_usec"] != 900 {
		t.Fatalf("ReadCgroupCPUStat = (%v, %v), want the three counters", got, err)
	}
}

func TestReadCgroupCPUStatRefusesAMalformedCounter(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "cpu.stat")
	writeArtifact(t, filepath.Dir(path), "cpu.stat", "usage_usec many\n")

	// Act
	_, err := ReadCgroupCPUStat(path)

	// Assert
	if err == nil {
		t.Fatal("a malformed counter was read as a number; it must be refused loudly")
	}
}

func TestCPUOverProbeSaysWhenTheBeforeReadFailed(t *testing.T) {
	// Act
	got := CpuOverProbe(nil, os.ErrNotExist, time.Second)

	// Assert
	if !strings.Contains(got, "could not be read before the probe") {
		t.Fatalf("CpuOverProbe = %q, want the failed read said", got)
	}
}
