package tail

import (
	"fmt"
	"io"
	"os"
	"syscall"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// defaultMaxRead bounds one Poll's physical read so a huge appended chunk (or a
// full re-read after truncation) is drained across several bounded batches
// rather than one unbounded allocation (§7.2 "bounded reads").
const defaultMaxRead = 4 << 20

// Tailer is the Layer-1 cursored reader for ONE file: on each Poll it stats the
// file, detects truncation/rotation, reads appended bytes (bounded), frames them
// via its codec, and hands them to its handler. It holds the committed cursor in
// memory; the caller commits a batch's advance only after the store acks it, so a
// crash re-reads and dedup absorbs the overlap (§7.3 exactly-once).
type Tailer struct {
	path    string
	codec   Codec
	handler Handler
	ctx     *Context
	log     *logging.Bound
	maxRead int

	// committed cursor state
	fileID  string
	offset  int64
	carry   []byte
	records int64
}

// New builds a Tailer over path with the given codec, handler, and attribution
// context. A recovered cursor (from the store) may be applied via Restore.
func New(path string, codec Codec, h Handler, ctx *Context, log *logging.Bound) *Tailer {
	if ctx == nil {
		ctx = &Context{Path: path}
	}
	ctx.Path = path
	log.With(logging.Context{Operation: "tailer-new", Path: path}).LogVerbose("constructing tailer codec=%T handler=%T", codec, h)
	return &Tailer{path: path, codec: codec, handler: h, ctx: ctx, log: log, maxRead: defaultMaxRead}
}

// Restore seeds the committed cursor from a recovered CursorState (§7.3 startup
// recovery). Only offset/carry/file_id are restored; the record counter resumes
// from 0 (progress counts are advisory, not durable).
func (t *Tailer) Restore(c *corev1.CursorState) {
	if c == nil {
		t.log.With(logging.Context{Operation: "tailer-restore", Path: t.path}).LogVerbose("no recovered cursor supplied")
		return
	}
	t.fileID = c.GetFileId()
	t.offset = c.GetOffset()
	t.carry = append([]byte(nil), c.GetCarry()...)
	t.log.With(logging.Context{Operation: "tailer-restore", Path: t.path}).LogVerbose("restored cursor file_id=%q offset=%d carry_bytes=%d", t.fileID, t.offset, len(t.carry))
}

// PollResult is one batch: the events to write plus the cursor advance to commit
// atomically with them. Changed is false when the batch is nothing to write:
// the file had no new bytes, or every frame it did have was deferred by the
// handler and so neither produced an event nor moved the cursor.
type PollResult struct {
	Events  []*corev1.Event
	Next    *corev1.CursorState
	Records int64
	Changed bool
}

// FileID returns the tailer's last-known "dev:inode" identity (empty until first Poll).
func (t *Tailer) FileID() string { return t.fileID }

// Poll reads any appended bytes and returns the resulting batch WITHOUT mutating
// the committed cursor. The caller writes Events+Next to the store, then calls
// Commit(result) on a successful ack.
func (t *Tailer) Poll() (PollResult, error) {
	t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path}).LogVerbose("poll start file_id=%q offset=%d carry_bytes=%d records=%d", t.fileID, t.offset, len(t.carry), t.records)
	fi, err := os.Stat(t.path)
	if err != nil {
		return PollResult{}, err
	}
	fileID := statID(fi)
	size := fi.Size()

	offset, carry, records := t.offset, t.carry, t.records
	switch {
	case t.fileID != "" && fileID != t.fileID:
		t.log.With(logging.Context{Operation: "rotation", Path: t.path}).Log("file_id %s -> %s, resetting cursor to 0", t.fileID, fileID)
		offset, carry, records = 0, nil, 0
	case size < offset:
		// Unlike a rotation, a truncation destroys bytes IN PLACE: anything
		// appended past the committed offset before it is unrecoverable, and
		// everything re-read from 0 leans on store dedup not to duplicate.
		t.log.With(logging.Context{Operation: "truncation", Path: t.path, Level: "warn"}).Log("size %d < offset %d, resetting cursor to 0; bytes past the committed offset are unrecoverable", size, offset)
		offset, carry, records = 0, nil, 0
	}

	if size <= offset {
		// No new bytes; still surface the (possibly reset) cursor so file_id and
		// a truncation reset commit.
		result := PollResult{
			Next:    &corev1.CursorState{FileId: fileID, Path: t.path, Offset: offset, Carry: carry},
			Records: records,
			Changed: offset != t.offset || fileID != t.fileID,
		}
		t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path}).LogVerbose("poll no-new-bytes size=%d offset=%d cursor_changed=%t", size, offset, result.Changed)
		return result, nil
	}

	toRead := size - offset
	if toRead > int64(t.maxRead) {
		toRead = int64(t.maxRead)
	}
	buf := make([]byte, toRead)
	if err := readAt(t.path, buf, offset); err != nil {
		return PollResult{}, err
	}

	full := append(append([]byte(nil), carry...), buf...)
	frames, newCarry := t.codec.Decode(full, offset-int64(len(carry)))
	for _, f := range frames {
		if f.Obj != nil {
			records++
		}
	}
	newOffset := offset + toRead

	// Fill the counters the handler reports (totals through this batch).
	t.ctx.RecordsObserved = records
	t.ctx.BytesObserved = newOffset
	// The tailer can re-read any byte it has not committed past, so it can hand
	// deferred frames back (Context "deferred frames").
	t.ctx.Redelivers = true
	events := t.handler.Handle(frames, t.ctx)
	newOffset, newCarry, records = t.applyHold(frames, offset, newOffset, newCarry, records)

	result := PollResult{
		Events:  events,
		Next:    &corev1.CursorState{FileId: fileID, Path: t.path, Offset: newOffset, Carry: newCarry},
		Records: records,
		// A batch whose every frame was deferred moves neither the cursor nor
		// the store, so it is not a change to write: the next poll re-reads
		// those same bytes.
		Changed: newOffset != t.offset || fileID != t.fileID || len(events) > 0,
	}
	t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path}).LogVerbose("poll decoded frames=%d events=%d read_bytes=%d next_offset=%d carry_bytes=%d held=%d changed=%t", len(frames), len(events), toRead, result.Next.GetOffset(), len(result.Next.GetCarry()), t.ctx.HeldDeliveries, result.Changed)
	return result, nil
}

// applyHold rolls the batch's cursor advance back to the offset the handler
// deferred, so the committed cursor NEVER moves past a frame that was not
// converted. The deferred bytes are re-read (and re-delivered) on the next
// poll, and a restart from the committed cursor re-reads them too — which is
// the whole point: an unsettled record survives a crash mid-hold.
//
// The carry is dropped with the rewind: every carried byte precedes the held
// frame's first byte, so it has already been consumed by a frame in this batch.
func (t *Tailer) applyHold(frames []Frame, offset, newOffset int64, newCarry []byte, records int64) (int64, []byte, int64) {
	if t.ctx.HeldDeliveries <= 0 {
		return newOffset, newCarry, records
	}
	held := t.ctx.HeldOffset
	if held < offset || held >= newOffset {
		// The handler named an offset outside the batch it was just given.
		// Honoring it would rewind the cursor over already-converted records or
		// leave it ahead of the frame it claims to hold, so it is refused
		// loudly rather than obeyed or silently ignored.
		t.log.With(logging.Context{Operation: "hold-offset", Path: t.path, Level: "error"}).Log(
			"handler held offset %d outside this batch's [%d,%d); ignoring the hold and advancing the cursor",
			held, offset, newOffset)
		t.ctx.HeldOffset, t.ctx.HeldDeliveries = 0, 0
		return newOffset, newCarry, records
	}
	for _, f := range frames {
		if f.Obj != nil && f.Offset >= held {
			records--
		}
	}
	// Re-report the totals as of the rewind, so the next Handle sees the counts
	// for what has actually been converted.
	t.ctx.RecordsObserved, t.ctx.BytesObserved = records, held
	t.log.With(logging.Context{Operation: "tailer-hold", Path: t.path}).Log("deferred deliveries=%d rewinding cursor from=%d to=%d", t.ctx.HeldDeliveries, newOffset, held)
	return held, nil, records
}

// Commit advances the committed cursor to a polled result (call only after the
// store has durably accepted the batch).
func (t *Tailer) Commit(r PollResult) {
	t.log.With(logging.Context{Operation: "tailer-commit", Path: t.path}).LogVerbose("commit requested next_present=%t records=%d", r.Next != nil, r.Records)
	if r.Next != nil {
		t.fileID = r.Next.GetFileId()
		t.offset = r.Next.GetOffset()
		t.carry = append([]byte(nil), r.Next.GetCarry()...)
	}
	t.records = r.Records
	t.log.With(logging.Context{Operation: "tailer-commit", Path: t.path}).LogVerbose("cursor committed file_id=%q offset=%d carry_bytes=%d", t.fileID, t.offset, len(t.carry))
}

// readAt reads len(buf) bytes at off from path.
func readAt(path string, buf []byte, off int64) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	if _, err := f.ReadAt(buf, off); err != nil && err != io.EOF {
		return fmt.Errorf("tail: reading %d bytes at %d from %s: %w", len(buf), off, path, err)
	}
	return nil
}

// statID returns the "dev:inode" identity used to detect rotation.
func statID(fi os.FileInfo) string {
	if st, ok := fi.Sys().(*syscall.Stat_t); ok {
		return fmt.Sprintf("%d:%d", uint64(st.Dev), uint64(st.Ino))
	}
	// Fallback (non-unix): size+mtime is a coarse identity. The sidecar only
	// targets unix, so this path is effectively unreachable.
	return fmt.Sprintf("nosys:%d:%d", fi.Size(), fi.ModTime().UnixNano())
}
