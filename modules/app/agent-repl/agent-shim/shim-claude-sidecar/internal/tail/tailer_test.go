package tail

import (
	"os"
	"path/filepath"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// stubHandler records the frames it saw and emits one event per decoded object.
type stubHandler struct {
	batches   [][]Frame
	lastCtx   Context
}

func (s *stubHandler) Handle(fr []Frame, ctx *Context) []*corev1.Event {
	s.batches = append(s.batches, fr)
	s.lastCtx = *ctx
	var out []*corev1.Event
	for _, f := range fr {
		if f.Obj != nil {
			out = append(out, &corev1.Event{SessionId: ctx.SessionID})
		}
	}
	return out
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
}

func appendFile(t *testing.T, path, content string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatalf("open append: %v", err)
	}
	defer f.Close()
	if _, err := f.WriteString(content); err != nil {
		t.Fatalf("append: %v", err)
	}
}

func newTailer(t *testing.T, path string) (*Tailer, *stubHandler) {
	t.Helper()
	h := &stubHandler{}
	return New(path, JSONLCodec{}, h, &Context{SessionID: "s1"}, nil), h
}

func TestTailerReadsAppendedLines(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	tr, _ := newTailer(t, p)
	// Act
	r1, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(r1)
	// Assert
	if len(r1.Events) != 2 || !r1.Changed {
		t.Fatalf("poll1 events = %d changed=%v, want 2 true", len(r1.Events), r1.Changed)
	}
	// Act: append a third line.
	appendFile(t, p, `{"c":3}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: only the new line is read.
	if len(r2.Events) != 1 {
		t.Fatalf("poll2 events = %d, want 1", len(r2.Events))
	}
	if r2.Next.GetOffset() != int64(len(`{"a":1}`+"\n"+`{"b":2}`+"\n"+`{"c":3}`+"\n")) {
		t.Fatalf("offset = %d", r2.Next.GetOffset())
	}
}

func TestTailerNoNewBytes(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n")
	tr, _ := newTailer(t, p)
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Act: poll again with no new bytes.
	r2, _ := tr.Poll()
	// Assert
	if r2.Changed || len(r2.Events) != 0 {
		t.Fatalf("poll2 changed=%v events=%d, want false 0", r2.Changed, len(r2.Events))
	}
}

func TestTailerPartialLineCarriedThenCompleted(t *testing.T) {
	// Arrange: a complete line + a partial one.
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":`)
	tr, _ := newTailer(t, p)
	// Act
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Assert: one event, carry retained.
	if len(r1.Events) != 1 {
		t.Fatalf("poll1 events = %d, want 1", len(r1.Events))
	}
	if len(r1.Next.GetCarry()) == 0 {
		t.Fatalf("expected carry for the partial line")
	}
	// Act: complete the partial line.
	appendFile(t, p, `2}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: the reassembled line yields one event.
	if len(r2.Events) != 1 {
		t.Fatalf("poll2 events = %d, want 1 (reassembled)", len(r2.Events))
	}
	if len(r2.Next.GetCarry()) != 0 {
		t.Fatalf("carry should be drained after completion")
	}
}

func TestTailerTruncationResets(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	tr, _ := newTailer(t, p)
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Act: truncate to a shorter, fresh content (size < committed offset).
	writeFile(t, p, `{"z":9}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: cursor reset to 0, the new content read from the top.
	if len(r2.Events) != 1 {
		t.Fatalf("post-truncation events = %d, want 1", len(r2.Events))
	}
	if r2.Next.GetOffset() != int64(len(`{"z":9}`+"\n")) {
		t.Fatalf("post-truncation offset = %d", r2.Next.GetOffset())
	}
}

func TestTailerRotationResets(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n")
	tr, _ := newTailer(t, p)
	r1, _ := tr.Poll()
	tr.Commit(r1)
	firstID := tr.FileID()
	// Act: replace the file with a new inode carrying MORE bytes than the old
	// offset (so only rotation — not truncation — can explain reading from 0).
	if err := os.Remove(p); err != nil {
		t.Fatalf("remove: %v", err)
	}
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: a new file_id and a full re-read from 0.
	if tr.FileID() == firstID {
		t.Fatalf("file_id unchanged after rotation")
	}
	if len(r2.Events) != 2 {
		t.Fatalf("post-rotation events = %d, want 2 (full re-read)", len(r2.Events))
	}
}

func TestTailerBoundedRead(t *testing.T) {
	// Arrange: a file larger than one bounded read.
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n"+`{"c":3}`+"\n")
	tr, _ := newTailer(t, p)
	tr.maxRead = 9 // smaller than the file; forces multiple polls
	// Act: first bounded poll.
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Assert: it did not consume the whole file in one go.
	if r1.Next.GetOffset() != 9 {
		t.Fatalf("bounded offset = %d, want 9", r1.Next.GetOffset())
	}
	// Act: drain the rest across further polls.
	total := len(r1.Events)
	for i := 0; i < 5; i++ {
		r, _ := tr.Poll()
		tr.Commit(r)
		total += len(r.Events)
	}
	// Assert: all three records eventually surfaced.
	if total != 3 {
		t.Fatalf("total events across bounded polls = %d, want 3", total)
	}
}

func TestTailerRestoreResumesFromCursor(t *testing.T) {
	// Arrange: a file whose first line was already consumed per a stored cursor.
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	first := `{"a":1}` + "\n"
	writeFile(t, p, first+`{"b":2}`+"\n")
	tr, _ := newTailer(t, p)
	// Prime file_id via a stat, then restore an offset past the first line.
	fi, _ := os.Stat(p)
	tr.Restore(&corev1.CursorState{FileId: statID(fi), Path: p, Offset: int64(len(first))})
	// Act
	r, _ := tr.Poll()
	tr.Commit(r)
	// Assert: only the second line is read.
	if len(r.Events) != 1 {
		t.Fatalf("events after restore = %d, want 1", len(r.Events))
	}
}

func TestTailerCountersReportedToHandler(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	tr, h := newTailer(t, p)
	// Act
	r, _ := tr.Poll()
	tr.Commit(r)
	// Assert: the handler saw cumulative counters through the batch.
	if h.lastCtx.RecordsObserved != 2 {
		t.Fatalf("records = %d, want 2", h.lastCtx.RecordsObserved)
	}
	if h.lastCtx.BytesObserved != r.Next.GetOffset() {
		t.Fatalf("bytes = %d, want %d", h.lastCtx.BytesObserved, r.Next.GetOffset())
	}
}
