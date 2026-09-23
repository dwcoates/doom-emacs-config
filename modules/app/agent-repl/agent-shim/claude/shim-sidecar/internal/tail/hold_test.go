package tail

// The cursor side of DEFERRED FRAMES: a handler that cannot settle the tail of
// a batch leaves it unconverted, and the tailer must not commit past it. These
// tests own the mechanics (rewind, redelivery, refusal); the decision of WHAT
// to defer belongs to the handler package.

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

// holdStub defers whatever offset hold returns, and emits one entry per frame
// it did convert — the same shape a real handler's hold has.
type holdStub struct {
	hold    func(fr []Frame) (int64, bool)
	batches [][]Frame
	lastCtx Context
}

func (s *holdStub) Handle(fr []Frame, ctx *Context) []*storev1.StoreEntry {
	s.batches = append(s.batches, fr)
	kept := len(fr)
	ctx.HeldOffset, ctx.HeldDeliveries = 0, 0
	if off, ok := s.hold(fr); ok {
		ctx.HeldOffset, ctx.HeldDeliveries = off, 1
		for i, f := range fr {
			if f.Offset >= off {
				kept = i
				break
			}
		}
	}
	var out []*storev1.StoreEntry
	for _, f := range fr[:kept] {
		if f.Obj != nil {
			out = append(out, stubEntry(ctx.SessionID))
		}
	}
	s.lastCtx = *ctx
	return out
}

// holdLast defers the batch's last frame.
func holdLast(fr []Frame) (int64, bool) {
	if len(fr) == 0 {
		return 0, false
	}
	return fr[len(fr)-1].Offset, true
}

// newHoldTailer lays down a file with content and opens a tailer whose handler
// defers per hold.
func newHoldTailer(t *testing.T, content string, hold func([]Frame) (int64, bool)) (*Tailer, *holdStub, *[]string, string) {
	t.Helper()
	p := filepath.Join(t.TempDir(), "t.jsonl")
	writeFile(t, p, content)
	h := &holdStub{hold: hold}
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "test"})
	return New(p, JSONLCodec{}, h, &Context{SessionID: "s1"}, log), h, &logs, p
}

func TestTailerRedeliversIsSetForEveryHandle(t *testing.T) {
	// Arrange: the tailer can re-read any byte it has not committed past, so it
	// always promises redelivery.
	tr, h, _, _ := newHoldTailer(t, `{"a":1}`+"\n", func([]Frame) (int64, bool) { return 0, false })
	// Act
	if _, err := tr.Poll(); err != nil {
		t.Fatalf("poll: %v", err)
	}
	// Assert
	if !h.lastCtx.Redelivers {
		t.Fatal("the tailer handed a batch over without promising to redeliver what the handler defers")
	}
}

func TestTailerCursorStopsBeforeAHeldFrame(t *testing.T) {
	// Arrange: two lines, the second of which the handler defers.
	first := `{"a":1}` + "\n"
	tr, _, _, _ := newHoldTailer(t, first+`{"b":2}`+"\n", holdLast)
	// Act
	r, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}
	tr.Commit(r)
	// Assert: the committed cursor names the held frame's FIRST byte, so the
	// frame is still on the unread side of it.
	if off := r.Next.GetOffset(); off != int64(len(first)) {
		t.Fatalf("committed offset = %d, want %d (the held frame's first byte)", off, int64(len(first)))
	}
	if len(r.Entries) != 1 {
		t.Fatalf("entries = %d, want 1 (only the converted frame)", len(r.Entries))
	}
	if r.Records != 1 {
		t.Fatalf("records = %d, want 1 (a deferred frame is not yet observed)", r.Records)
	}
}

func TestTailerRedeliversAHeldFrameOnTheNextPoll(t *testing.T) {
	// Arrange: the handler defers the batch's last frame the first time and
	// settles everything the second time.
	held := true
	tr, h, _, p := newHoldTailer(t, `{"a":1}`+"\n"+`{"b":2}`+"\n", func(fr []Frame) (int64, bool) {
		if !held {
			return 0, false
		}
		held = false
		return holdLast(fr)
	})
	r1, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r1)
	// Act: nothing new is appended; the next poll must re-read the deferred line.
	r2, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll2: %v", err)
	}
	tr.Commit(r2)
	// Assert
	if n := len(h.batches[1]); n != 1 {
		t.Fatalf("second batch = %d frames, want 1 (the deferred line, handed back)", n)
	}
	fi, err := os.Stat(p)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if off := r2.Next.GetOffset(); off != fi.Size() {
		t.Fatalf("committed offset = %d, want %d (the hold released)", off, fi.Size())
	}
}

func TestTailerRejectsABatchWhoseHoldIsOutsideIt(t *testing.T) {
	tests := []struct {
		name string
		// at is the offset the handler names, relative to the batch it was given.
		at int64
	}{
		{name: "behind the batch", at: -1},
		{name: "past the batch", at: 1 << 20},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a handler naming an offset it was never given frames for.
			tr, _, _, _ := newHoldTailer(t, `{"a":1}`+"\n", func([]Frame) (int64, bool) { return tc.at, true })

			// Act.
			_, err := tr.Poll()

			// Assert: obeying it would rewind over converted records or park the
			// cursor ahead of the frame it claims to hold, so the whole batch is
			// rejected as a producer defect.
			if !errors.Is(err, ErrHoldOutOfBatch) {
				t.Fatalf("Poll error = %v, want ErrHoldOutOfBatch", err)
			}
		})
	}
}

func TestTailerLogsARejectedOutOfBatchHold(t *testing.T) {
	// Arrange.
	tr, _, logs, _ := newHoldTailer(t, `{"a":1}`+"\n", func([]Frame) (int64, bool) { return 1 << 20, true })

	// Act.
	if _, err := tr.Poll(); err == nil {
		t.Fatal("an out-of-batch hold was accepted")
	}

	// Assert.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "hold-out-of-batch", "error")
	if got, ok := rec.Context["offset"].(float64); !ok || int64(got) != 1<<20 {
		t.Fatalf("hold-out-of-batch/error offset = %v, want %d", rec.Context["offset"], int64(1<<20))
	}
}

func TestTailerRejectedHoldLeavesTheCursorUnmoved(t *testing.T) {
	// Arrange.
	tr, _, _, _ := newHoldTailer(t, `{"a":1}`+"\n", func([]Frame) (int64, bool) { return 1 << 20, true })

	// Act.
	if _, err := tr.Poll(); err == nil {
		t.Fatal("an out-of-batch hold was accepted")
	}

	// Assert: nothing was written, so nothing may be committed.
	if got := tr.offset; got != 0 {
		t.Fatalf("committed offset = %d, want 0 (a rejected batch commits nothing)", got)
	}
}

func TestTailerForcesConversionOnTheRedelivery(t *testing.T) {
	// Arrange: a handler that holds the batch's last frame every time it is
	// asked, so only the tailer's bound can end the hold.
	tr, h, _, _ := newHoldTailer(t, `{"a":1}`+"\n"+`{"b":2}`+"\n", holdLast)
	r1, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r1)

	// Act: the redelivery.
	r2, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll2: %v", err)
	}
	tr.Commit(r2)

	// Assert: the handler was told the delivery was forced.
	if !h.lastCtx.HoldForced {
		t.Fatal("the redelivery did not tell the handler its hold was forced")
	}
}

func TestTailerAdvancesPastAHoldThatSurvivesItsRedelivery(t *testing.T) {
	// Arrange: the same never-settling handler.
	tr, _, _, p := newHoldTailer(t, `{"a":1}`+"\n"+`{"b":2}`+"\n", holdLast)
	r1, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r1)

	// Act.
	r2, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll2: %v", err)
	}
	tr.Commit(r2)

	// Assert: a record held forever is a record never stored, so the bound wins.
	fi, err := os.Stat(p)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if off := r2.Next.GetOffset(); off != fi.Size() {
		t.Fatalf("committed offset = %d, want %d (the exhausted hold must release)", off, fi.Size())
	}
}

func TestTailerLogsAnExhaustedHold(t *testing.T) {
	// Arrange.
	first := `{"a":1}` + "\n"
	tr, _, logs, _ := newHoldTailer(t, first+`{"b":2}`+"\n", holdLast)
	r1, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r1)

	// Act.
	if _, err := tr.Poll(); err != nil {
		t.Fatalf("poll2: %v", err)
	}

	// Assert: the exhausted hold names the SECOND frame's offset, the one it
	// held on both the original delivery and its forced redelivery.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "hold-exhausted", "warn")
	if got, ok := rec.Context["offset"].(float64); !ok || int64(got) != int64(len(first)) {
		t.Fatalf("hold-exhausted/warn offset = %v, want %d", rec.Context["offset"], int64(len(first)))
	}
}

func TestTailerHoldingEveryFrameIsNotAChangeToWrite(t *testing.T) {
	// Arrange: a settled first line (which is also what teaches the tailer the
	// file's identity), then an appended line the handler defers — so the poll
	// that reads it produces neither an event nor a cursor advance.
	holding := false
	tr, _, _, p := newHoldTailer(t, `{"a":1}`+"\n", func(fr []Frame) (int64, bool) {
		if !holding {
			return 0, false
		}
		return holdLast(fr)
	})
	r0, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r0)
	holding = true
	appendFile(t, p, `{"b":2}`+"\n")
	// Act
	r, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll2: %v", err)
	}
	// Assert
	if r.Changed {
		t.Fatal("a fully deferred batch reported a change; there is nothing to write and nothing to commit")
	}
}

func TestAHeldBoundedBatchReportsNoMore(t *testing.T) {
	// Arrange: a batch cut by its frame bound whose last kept frame is HELD. A
	// hold waits for the next line on purpose, so an immediate re-poll would
	// force the redelivery early.
	tr, _, _, _ := newHoldTailer(t, `{"a":1}`+"\n"+`{"b":2}`+"\n"+`{"c":3}`+"\n", holdLast)
	tr.maxFrames = 2

	// Act.
	r, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}

	// Assert.
	if r.More {
		t.Fatal("a batch holding a frame reported more; its re-poll would force the hold")
	}
}
