package e2e

import (
	"fmt"
	"strings"
	"testing"
)

// THE INLINE EXCERPT OF A NATIVE BACKTRACE.
//
// The capture itself is gdb's, taken from outside a stalled Emacs, and the
// whole of it is filed as an artifact. What this file is about is the part
// quoted INLINE in the failure message, because that is the part a reader
// sees first and, on 2026-09-04, the part that had already run out of room by
// the time gdb's descending thread walk reached thread 1 — the only thread
// running lisp, and so the only one that says why the editor stopped.

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
	got := nativeBacktraceExcerpt(gdbCapture(20, 3))

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
	frames := nativeBacktraceHeadFrames * 3

	// Act.
	got := strings.Join(nativeBacktraceExcerpt(gdbCapture(frames, 3)), "\n")

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
	got := nativeBacktraceExcerpt(gdbCapture(5, 2))

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
	got := strings.Join(nativeBacktraceExcerpt(gdbCapture(5, 3)), "\n")

	// Assert: gdb's `info threads` table is the index to what follows.
	if !strings.Contains(got, "Id   Target Id") {
		t.Fatalf("excerpt lost the info-threads table:\n%s", got)
	}
}

func TestTheExcerptTrimsHelperThreadsAndSaysSo(t *testing.T) {
	t.Parallel()
	// Arrange: more helper stacks than the budget past the main thread.
	// Act.
	got := strings.Join(nativeBacktraceExcerpt(gdbCapture(5, 12)), "\n")

	// Assert.
	if !strings.Contains(got, "more lines in "+nativeBacktraceFile) {
		t.Fatalf("excerpt trimmed silently:\n%s", got)
	}
}

func TestTheExcerptTrimsAHelperThreadWholeOrNotAtAll(t *testing.T) {
	t.Parallel()
	// Arrange, Act.
	got := nativeBacktraceExcerpt(gdbCapture(5, 12))

	// Assert: a quoted helper section carries all eight of its frames.
	frames := 0
	inHelper := false
	for _, line := range got {
		if strings.HasPrefix(line, "Thread ") {
			if inHelper && frames != 8 {
				t.Fatalf("a helper thread was quoted with %d frames, want a whole stack", frames)
			}
			inHelper = !strings.HasPrefix(line, nativeBacktraceMainThread)
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
	got := strings.Join(nativeBacktraceExcerpt(text), "\n")

	// Assert.
	if got != text {
		t.Fatalf("excerpt = %q, want the capture verbatim", got)
	}
}
