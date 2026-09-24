package tail

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/testclose"
)

func testLog() *logging.Bound {
	return logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
}

// stubEntry is the minimal stored record a stub handler emits per decoded
// object: enough to carry the attribution the tailer handed it, and nothing
// else the tailer's own mechanics depend on.
//
// The session rides on write_id because store.v1 StoreEntry has no session
// field at all — protocol.v1 ExternalEntry, which carried one, was deleted.
func stubEntry(sessionID string) *storev1.StoreEntry {
	return &storev1.StoreEntry{WriteId: sessionID}
}

// stubHandler records the frames it saw and emits one entry per decoded object.
type stubHandler struct {
	batches [][]Frame
	lastCtx Context
}

func (s *stubHandler) Handle(fr []Frame, ctx *Context) []*storev1.StoreEntry {
	s.batches = append(s.batches, fr)
	s.lastCtx = *ctx
	var out []*storev1.StoreEntry
	for _, f := range fr {
		if f.Obj != nil {
			out = append(out, stubEntry(ctx.SessionID))
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
	defer testclose.OrFail(t, f)
	if _, err := f.WriteString(content); err != nil {
		t.Fatalf("append: %v", err)
	}
}

func newTailer(t *testing.T, path string) (*Tailer, *stubHandler) {
	t.Helper()
	h := &stubHandler{}
	return New(path, JSONLCodec{}, h, &Context{SessionID: "s1"}, testLog()), h
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
	if len(r1.Entries) != 2 || !r1.Changed {
		t.Fatalf("poll1 entries = %d changed=%v, want 2 true", len(r1.Entries), r1.Changed)
	}
	// Act: append a third line.
	appendFile(t, p, `{"c":3}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: only the new line is read.
	if len(r2.Entries) != 1 {
		t.Fatalf("poll2 entries = %d, want 1", len(r2.Entries))
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
	if r2.Changed || len(r2.Entries) != 0 {
		t.Fatalf("poll2 changed=%v entries=%d, want false 0", r2.Changed, len(r2.Entries))
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
	if len(r1.Entries) != 1 {
		t.Fatalf("poll1 entries = %d, want 1", len(r1.Entries))
	}
	if len(r1.Next.GetCarry()) == 0 {
		t.Fatalf("expected carry for the partial line")
	}
	// Act: complete the partial line.
	appendFile(t, p, `2}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert: the reassembled line yields one event.
	if len(r2.Entries) != 1 {
		t.Fatalf("poll2 entries = %d, want 1 (reassembled)", len(r2.Entries))
	}
	if len(r2.Next.GetCarry()) != 0 {
		t.Fatalf("carry should be drained after completion")
	}
}

// levelsFor returns the persisted level of every record whose message contains
// substring. The persistent sink is the canonical JSONL log, one object a line.
func levelsFor(t *testing.T, sink *bytes.Buffer, substring string) []string {
	t.Helper()
	var out []string
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var record struct {
			Level   string `json:"level"`
			Message string `json:"message"`
		}
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("persisted record is not JSON: %v", err)
		}
		if strings.Contains(record.Message, substring) {
			out = append(out, record.Level)
		}
	}
	return out
}

func TestTailerTruncationWarnsAboutUnrecoverableBytes(t *testing.T) {
	// Arrange — a truncation destroys bytes in place, unlike a rotation.
	var sink bytes.Buffer
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	tr := New(p, JSONLCodec{}, &stubHandler{}, &Context{SessionID: "s1"},
		logging.New(io.Discard, &sink).With(logging.Context{Component: "test"}))
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Act
	writeFile(t, p, `{"z":9}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert
	if got := levelsFor(t, &sink, "resetting cursor to 0; bytes past the committed offset"); len(got) != 1 || got[0] != "warn" {
		t.Fatalf("truncation record levels = %v, want exactly one warn", got)
	}
}

func TestTailerRotationStaysInfo(t *testing.T) {
	// Arrange — a new inode loses nothing: the whole file is re-read.
	var sink bytes.Buffer
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n")
	tr := New(p, JSONLCodec{}, &stubHandler{}, &Context{SessionID: "s1"},
		logging.New(io.Discard, &sink).With(logging.Context{Component: "test"}))
	r1, _ := tr.Poll()
	tr.Commit(r1)
	// Act — replace the file so the inode changes but the size does not shrink.
	if err := os.Remove(p); err != nil {
		t.Fatalf("remove: %v", err)
	}
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	r2, _ := tr.Poll()
	tr.Commit(r2)
	// Assert
	if got := levelsFor(t, &sink, "resetting cursor to 0"); len(got) != 1 || got[0] != "info" {
		t.Fatalf("rotation record levels = %v, want exactly one info", got)
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
	if len(r2.Entries) != 1 {
		t.Fatalf("post-truncation entries = %d, want 1", len(r2.Entries))
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
	if len(r2.Entries) != 2 {
		t.Fatalf("post-rotation entries = %d, want 2 (full re-read)", len(r2.Entries))
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
	total := len(r1.Entries)
	for i := 0; i < 5; i++ {
		r, _ := tr.Poll()
		tr.Commit(r)
		total += len(r.Entries)
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
	tr.Restore(&storev1.CursorState{FileId: statID(fi), Path: p, Offset: int64(len(first))})
	// Act
	r, _ := tr.Poll()
	tr.Commit(r)
	// Assert: only the second line is read.
	if len(r.Entries) != 1 {
		t.Fatalf("events after restore = %d, want 1", len(r.Entries))
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

// droppingHandler converts every frame and stores none of them — the shape a
// batch the reader withheld as residue takes (convert/neverpersist.go).
type droppingHandler struct{ frames int }

func (d *droppingHandler) Handle(fr []Frame, _ *Context) []*storev1.StoreEntry {
	d.frames += len(fr)
	return nil
}

func TestABatchOfOnlyDroppedLinesStillAdvancesTheCursor(t *testing.T) {
	// Arrange. THE OFFSET IS ABSORBED BY THE CURSOR, NOT BY THE WRITE LEDGER. A
	// line that produced no entry mints no write_id, so the store's ledger — one
	// row per APPLIED write — holds nothing for it. What stops it being re-read
	// and re-decided is that the batch's cursor advance is the BYTES READ, and
	// the batch carries that advance whether or not it carries entries.
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	body := `{"a":1}` + "\n" + `{"b":2}` + "\n"
	writeFile(t, p, body)
	h := &droppingHandler{}
	tr := New(p, JSONLCodec{}, h, &Context{SessionID: "s1"}, testLog())

	// Act.
	r, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}
	tr.Commit(r)

	// Assert.
	if h.frames != 2 {
		t.Fatalf("frames handled = %d, want both lines still READ", h.frames)
	}
	if len(r.Entries) != 0 || !r.Changed {
		t.Fatalf("entries = %d changed = %v, want 0 and true", len(r.Entries), r.Changed)
	}
	if got := r.Next.GetOffset(); got != int64(len(body)) {
		t.Fatalf("cursor advance = %d, want %d — the dropped lines' bytes", got, len(body))
	}
}

func TestTheSecondPollAfterAnAllDroppedBatchRereadsNothing(t *testing.T) {
	// Arrange. The other half of the same claim: with the advance committed, the
	// dropped lines are never handed to the converter a second time, so nothing
	// re-decides them.
	dir := t.TempDir()
	p := filepath.Join(dir, "t.jsonl")
	writeFile(t, p, `{"a":1}`+"\n"+`{"b":2}`+"\n")
	h := &droppingHandler{}
	tr := New(p, JSONLCodec{}, h, &Context{SessionID: "s1"}, testLog())
	first, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll1: %v", err)
	}
	tr.Commit(first)

	// Act.
	second, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll2: %v", err)
	}

	// Assert.
	if h.frames != 2 {
		t.Fatalf("frames handled across both polls = %d, want 2 — the same lines must not be re-read", h.frames)
	}
	if second.Changed {
		t.Fatalf("the second poll reported a change; the dropped lines' offset was already committed")
	}
}

// threeLines is a fixture of three one-record lines, and lineTwoAt the offset
// its second line starts at.
const threeLines = `{"a":1}` + "\n" + `{"b":2}` + "\n" + `{"c":3}` + "\n"

const lineTwoAt = int64(len(`{"a":1}` + "\n"))

func TestTailerBatchBounds(t *testing.T) {
	tests := []struct {
		name       string
		maxRead    int
		maxFrames  int
		wantFrames int
		wantOffset int64
		wantMore   bool
	}{
		{name: "the frame bound stops the batch at the first frame past it", maxRead: MaxBatchBytes, maxFrames: 1, wantFrames: 1, wantOffset: lineTwoAt, wantMore: true},
		{name: "the byte bound stops the read short of the file's end", maxRead: 9, maxFrames: MaxBatchFrames, wantFrames: 1, wantOffset: 9, wantMore: true},
		{name: "a batch inside both bounds reads to the end and reports no more", maxRead: MaxBatchBytes, maxFrames: MaxBatchFrames, wantFrames: 3, wantOffset: int64(len(threeLines)), wantMore: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			p := filepath.Join(t.TempDir(), "t.jsonl")
			writeFile(t, p, threeLines)
			tr, h := newTailer(t, p)
			tr.maxRead, tr.maxFrames = tc.maxRead, tc.maxFrames

			// Act.
			r, err := tr.Poll()
			if err != nil {
				t.Fatalf("poll: %v", err)
			}

			// Assert.
			if got := len(h.batches[0]); got != tc.wantFrames {
				t.Fatalf("handler saw %d frame(s), want %d", got, tc.wantFrames)
			}
			if got := r.Next.GetOffset(); got != tc.wantOffset {
				t.Fatalf("next offset = %d, want %d", got, tc.wantOffset)
			}
			if r.More != tc.wantMore {
				t.Fatalf("more = %t, want %t", r.More, tc.wantMore)
			}
		})
	}
}

func TestTailerFrameBoundedBatchesLoseNothing(t *testing.T) {
	// Arrange: a frame bound of one forces a batch per line.
	p := filepath.Join(t.TempDir(), "t.jsonl")
	writeFile(t, p, threeLines)
	tr, _ := newTailer(t, p)
	tr.maxFrames = 1

	// Act: drain while the tailer says there is more.
	total, polls := 0, 0
	for {
		r, err := tr.Poll()
		if err != nil {
			t.Fatalf("poll: %v", err)
		}
		tr.Commit(r)
		total += len(r.Entries)
		polls++
		if !r.More {
			break
		}
	}

	// Assert: every record surfaced exactly once, one bounded batch each.
	if total != 3 || polls != 3 {
		t.Fatalf("records = %d over %d poll(s), want 3 over 3", total, polls)
	}
}

func TestTailerFrameBoundCountsOnlyTheFramesItKept(t *testing.T) {
	// Arrange.
	p := filepath.Join(t.TempDir(), "t.jsonl")
	writeFile(t, p, threeLines)
	tr, h := newTailer(t, p)
	tr.maxFrames = 2

	// Act.
	r, err := tr.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}

	// Assert: the counters describe the bounded batch, not the whole read.
	if r.Records != 2 || h.lastCtx.RecordsObserved != 2 {
		t.Fatalf("records = %d observed = %d, want 2 each", r.Records, h.lastCtx.RecordsObserved)
	}
	if h.lastCtx.BytesObserved != 2*lineTwoAt {
		t.Fatalf("bytes observed = %d, want the bounded batch's end %d", h.lastCtx.BytesObserved, 2*lineTwoAt)
	}
}
