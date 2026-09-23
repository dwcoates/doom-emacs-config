package tail

import (
	"errors"
	"fmt"
	"io"
	"os"
	"syscall"

	storev1 "agentrepl/proto/store/v1"
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

	// lastSize is the file's size as of the last SUCCESSFUL poll, and sized
	// says a poll ever observed one. Together with the committed offset they
	// answer the only question a vanished file leaves open: was there anything
	// past the offset that went with it? A tailer that never polled cannot
	// answer, which is why the boolean exists rather than a zero sentinel.
	lastSize int64
	sized    bool
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
func (t *Tailer) Restore(c *storev1.CursorState) {
	if c == nil {
		t.log.With(logging.Context{Operation: "tailer-restore", Path: t.path}).LogVerbose("no recovered cursor supplied")
		return
	}
	t.fileID = c.GetFileId()
	t.offset = c.GetOffset()
	t.carry = append([]byte(nil), c.GetCarry()...)
	t.ctx.FileID = t.fileID
	t.log.With(logging.Context{Operation: "tailer-restore", Path: t.path, FileID: t.fileID, Offset: logging.Off(t.offset)}).
		LogVerbose("restored cursor carry_bytes=%d", len(t.carry))
}

// PollResult is one batch: the records to write plus the cursor advance to
// commit atomically with them. Changed is false when the batch is nothing to
// write: the file had no new bytes, or every frame it did have was deferred by
// the handler and so neither produced a record nor moved the cursor.
type PollResult struct {
	Entries []*storev1.StoreEntry
	Next    *storev1.CursorState
	Records int64
	Changed bool
}

// LastSize returns the file size seen by the last successful poll, and whether
// any poll ever saw one.
func (t *Tailer) LastSize() (int64, bool) { return t.lastSize, t.sized }

// FileID returns the tailer's last-known "dev:inode" identity (empty until first Poll).
func (t *Tailer) FileID() string { return t.fileID }

// Poll reads any appended bytes and returns the resulting batch WITHOUT mutating
// the committed cursor. The caller writes Entries+Next to the store, then calls
// Commit(result) on a successful ack.
func (t *Tailer) Poll() (PollResult, error) {
	t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path, FileID: t.fileID, Offset: logging.Off(t.offset)}).
		LogVerbose("poll start carry_bytes=%d records=%d", len(t.carry), t.records)
	fi, err := os.Stat(t.path)
	if err != nil {
		return PollResult{}, err
	}
	fileID := statID(fi)
	size := fi.Size()
	t.lastSize, t.sized = size, true

	offset, carry, records := t.offset, t.carry, t.records
	switch {
	case t.fileID != "" && fileID != t.fileID:
		t.log.With(logging.Context{Operation: "rotation", Path: t.path, FileID: fileID, Offset: logging.Off(0)}).
			Log("file_id %s -> %s, resetting cursor to 0", t.fileID, fileID)
		offset, carry, records = 0, nil, 0
	case size < offset:
		// Unlike a rotation, a truncation destroys bytes IN PLACE: anything
		// appended past the committed offset before it is unrecoverable, and
		// everything re-read from 0 leans on store dedup not to duplicate.
		t.log.With(logging.Context{Operation: "truncation", Path: t.path, FileID: fileID, Level: "warn"}).Log("size %d < offset %d, resetting cursor to 0; bytes past the committed offset are unrecoverable", size, offset)
		offset, carry, records = 0, nil, 0
	}

	if size <= offset {
		// No new bytes; still surface the (possibly reset) cursor so file_id and
		// a truncation reset commit.
		result := PollResult{
			Next:    &storev1.CursorState{FileId: fileID, Path: t.path, Offset: offset, Carry: carry},
			Records: records,
			Changed: offset != t.offset || fileID != t.fileID,
		}
		t.ctx.FileID = fileID
		t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path, FileID: fileID, Offset: logging.Off(offset)}).
			LogVerbose("poll no-new-bytes size=%d cursor_changed=%t", size, result.Changed)
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
	t.ctx.FileID = fileID
	// The tailer can re-read any byte it has not committed past, so it can hand
	// held frames back (Context, "the hold").
	t.ctx.Redelivers = true
	// A frame already held once is being REDELIVERED: this delivery is forced,
	// and the handler must convert it whatever the evidence.
	t.ctx.HoldForced = t.ctx.HeldDeliveries > 0
	forced := t.ctx.HoldForced
	entries := t.handler.Handle(frames, t.ctx)
	newOffset, newCarry, records, err = t.applyHold(frames, forced, offset, newOffset, newCarry, records)
	if err != nil {
		return PollResult{}, err
	}

	result := PollResult{
		Entries: entries,
		Next:    &storev1.CursorState{FileId: fileID, Path: t.path, Offset: newOffset, Carry: newCarry},
		Records: records,
		// A batch whose every frame was held moves neither the cursor nor the
		// store, so it is not a change to write: the next poll re-reads those
		// same bytes.
		Changed: newOffset != t.offset || fileID != t.fileID || len(entries) > 0,
	}
	t.log.With(logging.Context{Operation: "tailer-poll", Path: t.path, FileID: fileID, Offset: logging.Off(result.Next.GetOffset())}).
		LogVerbose("poll decoded frames=%d entries=%d read_bytes=%d carry_bytes=%d held_deliveries=%d changed=%t",
			len(frames), len(entries), toRead, len(result.Next.GetCarry()), t.ctx.HeldDeliveries, result.Changed)
	return result, nil
}

// ErrHoldOutOfBatch is returned when a handler holds an offset outside the
// batch it was just given. Honoring it would rewind the cursor over records
// already converted, or leave it ahead of the frame it claims to hold; both
// silently lose or duplicate records, so the batch is REJECTED as a producer
// defect rather than obeyed or quietly ignored.
var ErrHoldOutOfBatch = errors.New("tail: handler held an offset outside the delivered batch")

// applyHold rolls the batch's cursor advance back to the offset the handler
// held, so the committed cursor NEVER moves past a frame that was not
// converted. The held bytes are re-read (and redelivered) on the next poll, and
// a restart from the committed cursor re-reads them too — which is the whole
// point: an unsettled record survives a crash mid-hold.
//
// THE HOLD IS BOUNDED TO ONE REDELIVERY. `forced` is the state the handler was
// given: on a forced delivery the record's meaning is as settled as it will
// ever get, so a hold that survives it is refused and the cursor advances past
// the frame. A record held forever is a record never stored.
//
// The carry is dropped with the rewind: every carried byte precedes the held
// frame's first byte, so it has already been consumed by a frame in this batch.
func (t *Tailer) applyHold(frames []Frame, forced bool, offset, newOffset int64, newCarry []byte, records int64) (int64, []byte, int64, error) {
	if t.ctx.HeldDeliveries <= 0 {
		return newOffset, newCarry, records, nil
	}
	held := t.ctx.HeldOffset
	if held < offset || held >= newOffset {
		t.log.With(logging.Context{Operation: "hold-out-of-batch", Path: t.path, Level: "error", Offset: logging.Off(held)}).Log(
			"handler held offset %d outside this batch's [%d,%d); the batch is rejected as a producer defect", held, offset, newOffset)
		t.ctx.HeldOffset, t.ctx.HeldDeliveries, t.ctx.HoldForced = 0, 0, false
		return 0, nil, 0, fmt.Errorf("%w: held=%d batch=[%d,%d)", ErrHoldOutOfBatch, held, offset, newOffset)
	}
	if forced {
		// The redelivery already happened and the handler held again. The bound
		// is the whole reason the hold is safe, so it is enforced here rather
		// than trusted to the handler.
		t.log.With(logging.Context{Operation: "hold-exhausted", Path: t.path, Level: "warn", Offset: logging.Off(held)}).Log(
			"handler held offset %d again on its forced redelivery; the hold is refused and the cursor advances to %d", held, newOffset)
		t.ctx.HeldOffset, t.ctx.HeldDeliveries, t.ctx.HoldForced = 0, 0, false
		return newOffset, newCarry, records, nil
	}
	for _, f := range frames {
		if f.Obj != nil && f.Offset >= held {
			records--
		}
	}
	// Re-report the totals as of the rewind, so the next Handle sees the counts
	// for what has actually been converted.
	t.ctx.RecordsObserved, t.ctx.BytesObserved = records, held
	t.log.With(logging.Context{Operation: "tailer-hold", Path: t.path, Offset: logging.Off(held)}).Log(
		"held deliveries=%d rewinding cursor from=%d to=%d; the next delivery is forced", t.ctx.HeldDeliveries, newOffset, held)
	return held, nil, records, nil
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
	t.log.With(logging.Context{Operation: "tailer-commit", Path: t.path, FileID: t.fileID, Offset: logging.Off(t.offset)}).
		LogVerbose("cursor committed carry_bytes=%d", len(t.carry))
}

// readAt reads len(buf) bytes at off from path.
func readAt(path string, buf []byte, off int64) (err error) {
	f, openErr := os.Open(path)
	if openErr != nil {
		return openErr
	}
	defer func() {
		if closeErr := f.Close(); closeErr != nil {
			err = errors.Join(err, fmt.Errorf("tail: close %s after reading it: %w", path, closeErr))
		}
	}()
	if _, readErr := f.ReadAt(buf, off); readErr != nil && readErr != io.EOF {
		return fmt.Errorf("tail: reading %d bytes at %d from %s: %w", len(buf), off, path, readErr)
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

// Handler returns the handler this tailer drives, so the reader can ask it for
// something only a converter can spell (a LOST run's terminal, say).
func (t *Tailer) Handler() Handler { return t.handler }

// Offset returns the committed read position.
func (t *Tailer) Offset() int64 { return t.offset }

// Context returns the attribution the tailer hands its handler on every batch.
// It is the reader's own statement of whose work this file is, exposed so the
// seam's callers can assert what was resolved rather than re-deriving it.
func (t *Tailer) Context() *Context { return t.ctx }

// Identity answers a path's stable file identity — the same "dev:inode"
// spelling a tailer stamps onto CursorState.file_id.
//
// IT IS EXPORTED SO THERE IS ONE SPELLING. The cursor's identity is what
// survives the vendor's renames, so the reader has to be able to ask a path for
// it before it has a tailer — and a second, privately-computed spelling of the
// same thing is exactly how a cursor stops matching the file it belongs to.
func Identity(path string) (string, error) {
	fi, err := os.Stat(path)
	if err != nil {
		return "", fmt.Errorf("tail: reading the identity of %s: %w", path, err)
	}
	return statID(fi), nil
}
