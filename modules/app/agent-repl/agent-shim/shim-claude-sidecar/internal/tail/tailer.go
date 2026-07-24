package tail

import (
	"fmt"
	"io"
	"os"
	"syscall"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// Logf is the loud-logging sink (§12).
type Logf = func(format string, args ...any)

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
	log     Logf
	maxRead int

	// committed cursor state
	fileID  string
	offset  int64
	carry   []byte
	records int64
}

// New builds a Tailer over path with the given codec, handler, and attribution
// context. A recovered cursor (from the store) may be applied via Restore.
func New(path string, codec Codec, h Handler, ctx *Context, log Logf) *Tailer {
	if log == nil {
		log = func(string, ...any) {}
	}
	if ctx == nil {
		ctx = &Context{Path: path}
	}
	ctx.Path = path
	return &Tailer{path: path, codec: codec, handler: h, ctx: ctx, log: log, maxRead: defaultMaxRead}
}

// Restore seeds the committed cursor from a recovered CursorState (§7.3 startup
// recovery). Only offset/carry/file_id are restored; the record counter resumes
// from 0 (progress counts are advisory, not durable).
func (t *Tailer) Restore(c *corev1.CursorState) {
	if c == nil {
		return
	}
	t.fileID = c.GetFileId()
	t.offset = c.GetOffset()
	t.carry = append([]byte(nil), c.GetCarry()...)
}

// PollResult is one batch: the events to write plus the cursor advance to commit
// atomically with them. Changed is false when the file had no new bytes.
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
	fi, err := os.Stat(t.path)
	if err != nil {
		return PollResult{}, err
	}
	fileID := statID(fi)
	size := fi.Size()

	offset, carry, records := t.offset, t.carry, t.records
	switch {
	case t.fileID != "" && fileID != t.fileID:
		t.log("tail: ROTATION on %s (file_id %s → %s); resetting cursor to 0", t.path, t.fileID, fileID)
		offset, carry, records = 0, nil, 0
	case size < offset:
		t.log("tail: TRUNCATION on %s (size %d < offset %d); resetting cursor to 0", t.path, size, offset)
		offset, carry, records = 0, nil, 0
	}

	if size <= offset {
		// No new bytes; still surface the (possibly reset) cursor so file_id and
		// a truncation reset commit.
		return PollResult{
			Next:    &corev1.CursorState{FileId: fileID, Path: t.path, Offset: offset, Carry: carry},
			Records: records,
			Changed: offset != t.offset || fileID != t.fileID,
		}, nil
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
	events := t.handler.Handle(frames, t.ctx)

	return PollResult{
		Events:  events,
		Next:    &corev1.CursorState{FileId: fileID, Path: t.path, Offset: newOffset, Carry: newCarry},
		Records: records,
		Changed: true,
	}, nil
}

// Commit advances the committed cursor to a polled result (call only after the
// store has durably accepted the batch).
func (t *Tailer) Commit(r PollResult) {
	if r.Next != nil {
		t.fileID = r.Next.GetFileId()
		t.offset = r.Next.GetOffset()
		t.carry = append([]byte(nil), r.Next.GetCarry()...)
	}
	t.records = r.Records
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
