// Package wire implements the length-prefixed framing every agent-shim UDS
// hop uses: a 4-byte big-endian length followed by exactly that many bytes
// of one serialized protobuf message.
//
// The framing is deliberately trivial and owned in ONE place so the
// shim-store, the shim-claude-sidecar, and the daemon cannot drift. A frame
// larger than MaxFrame, a zero-length read, or a truncated payload is a
// protocol violation and surfaces as a loud error — never absorbed, never
// resynced past (a corrupted stream cannot be trusted after a bad length).
//
// # Two layers: raw frames, and the Any envelope on top of them
//
// WriteFrame/ReadFrame move opaque bytes. On top of them sits the SECOND half
// of the convention every hop speaks: the payload of a frame is a serialized
// google.protobuf.Any whose type_url is THE message discriminator, resolved
// against the proto global registry. core.proto carries no top-level frame
// oneof, so the Any IS the type tag.
//
// That envelope layer used to be copy-pasted into four packages, each with its
// own hand-maintained copy of one wire contract — exactly the drift this
// package exists to make impossible. So the encode and decode halves live here:
// MarshalAny/UnmarshalAny for callers that need the two steps apart, and
// WriteAny/ReadAny for the ordinary case. Today's callers are shim-store's
// server, the sidecar's store client, the daemon's shimclient, and the daemon's
// shim listener.
//
// The protobuf dependency this adds is not new weight on any consumer: every
// package that frames an Any already depends on protobuf, and only packages
// that speak the envelope layer reach for these.
package wire

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
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

// --- the Any envelope layer -------------------------------------------------

// MarshalAny wraps m in a google.protobuf.Any and returns the bytes that go in
// one frame's payload. Split out from WriteAny for the one caller that must
// encode OUTSIDE a write lock it holds only for the socket write.
//
// Both failure points are reported separately and name the Go type, because
// they mean different things: a wrap failure is a message whose descriptor the
// binary cannot resolve, and a marshal failure is a malformed message.
func MarshalAny(m proto.Message) ([]byte, error) {
	env, err := anypb.New(m)
	if err != nil {
		return nil, fmt.Errorf("wire: wrapping %T in Any: %w", m, err)
	}
	payload, err := proto.Marshal(env)
	if err != nil {
		return nil, fmt.Errorf("wire: marshaling Any(%T): %w", m, err)
	}
	return payload, nil
}

// UnmarshalAny decodes one frame payload's Any and resolves it to the concrete
// message its type_url names, via the proto global registry.
//
// The two failure points stay distinct: a decode failure is a corrupt frame,
// while a resolve failure is a type_url this binary's compiled schema set does
// not know — a version-skew signal, and the reason the type URL is quoted into
// the error rather than dropped.
func UnmarshalAny(payload []byte) (proto.Message, error) {
	env := &anypb.Any{}
	if err := proto.Unmarshal(payload, env); err != nil {
		return nil, fmt.Errorf("wire: decoding Any frame: %w", err)
	}
	m, err := env.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("wire: resolving Any type %q: %w", env.GetTypeUrl(), err)
	}
	return m, nil
}

// WriteAny writes m as one Any-wrapped frame: MarshalAny then WriteFrame.
func WriteAny(w io.Writer, m proto.Message) error {
	payload, err := MarshalAny(m)
	if err != nil {
		return err
	}
	return WriteFrame(w, payload)
}

// ReadAny reads one Any-wrapped frame and returns the concrete message it
// carries.
//
// ReadFrame's error is returned VERBATIM — io.EOF at a clean frame boundary,
// io.ErrUnexpectedEOF mid-frame, and a net.Conn's own deadline/closed errors
// all reach the caller unwrapped. Every call site classifies on those (a clean
// close is a normal disconnect, everything else is a fault), so wrapping them
// here would be a behavior change dressed as tidier prose.
func ReadAny(r io.Reader) (proto.Message, error) {
	payload, err := ReadFrame(r)
	if err != nil {
		return nil, err
	}
	return UnmarshalAny(payload)
}
