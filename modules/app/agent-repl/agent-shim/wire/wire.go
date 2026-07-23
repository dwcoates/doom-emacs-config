// Package wire implements the length-prefixed framing every agent-shim UDS
// hop uses: a 4-byte big-endian length followed by exactly that many bytes
// of one serialized protobuf message.
//
// The framing is deliberately trivial and owned in ONE place so the
// shim-store, the shim-claude-sidecar, and the daemon cannot drift. A frame
// larger than MaxFrame, a zero-length read, or a truncated payload is a
// protocol violation and surfaces as a loud error — never absorbed, never
// resynced past (a corrupted stream cannot be trusted after a bad length).
package wire

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
)

// MaxFrame caps a single frame's payload. Nothing legitimate approaches
// this (the largest observed transcript lines are <1MiB); a bigger length
// prefix means the stream is corrupt or hostile.
const MaxFrame = 32 << 20

// ErrFrameTooLarge reports a length prefix exceeding MaxFrame.
var ErrFrameTooLarge = errors.New("wire: frame exceeds MaxFrame")

// WriteFrame writes one length-prefixed frame carrying payload.
func WriteFrame(w io.Writer, payload []byte) error {
	if len(payload) > MaxFrame {
		return fmt.Errorf("%w: %d bytes", ErrFrameTooLarge, len(payload))
	}
	var hdr [4]byte
	binary.BigEndian.PutUint32(hdr[:], uint32(len(payload)))
	if _, err := w.Write(hdr[:]); err != nil {
		return fmt.Errorf("wire: writing frame header: %w", err)
	}
	if _, err := w.Write(payload); err != nil {
		return fmt.Errorf("wire: writing frame payload (%d bytes): %w", len(payload), err)
	}
	return nil
}

// ReadFrame reads one length-prefixed frame and returns its payload.
// io.EOF is returned ONLY at a clean frame boundary (connection closed
// between frames); a stream ending mid-frame is io.ErrUnexpectedEOF.
func ReadFrame(r io.Reader) ([]byte, error) {
	var hdr [4]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		if errors.Is(err, io.EOF) {
			return nil, io.EOF // clean close between frames
		}
		return nil, fmt.Errorf("wire: reading frame header: %w", err)
	}
	n := binary.BigEndian.Uint32(hdr[:])
	if n > MaxFrame {
		return nil, fmt.Errorf("%w: header claims %d bytes", ErrFrameTooLarge, n)
	}
	payload := make([]byte, n)
	if _, err := io.ReadFull(r, payload); err != nil {
		return nil, fmt.Errorf("wire: reading %d-byte frame payload: %w", n, err)
	}
	return payload, nil
}
