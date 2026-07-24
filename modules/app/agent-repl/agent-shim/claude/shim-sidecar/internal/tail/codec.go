// Package tail is the sidecar's Layer-1 generic streaming core (design §7.2):
// per-file cursored, truncation-aware tailing plus the file-type framing codecs.
// It carries no schema knowledge — it frames bytes into records and hands them
// to a handler; the handler (Layer 2) owns all proto conversion.
package tail

import (
	"bytes"
	"encoding/json"
)

// defaultMaxCarry bounds a single partial-line carry. A line longer than this
// without a newline is treated as corrupt: the codec emits one ParseErr frame
// and resyncs (§7.2 "bounded partial-tail carry"). Nothing legitimate on disk
// approaches it (the largest observed transcript lines are <1MiB).
const defaultMaxCarry = 16 << 20

// Frame is one unit a codec extracts from appended bytes.
type Frame struct {
	// Obj is the decoded JSON object for a JSONL frame, or nil for a raw-text
	// frame or a frame that failed to parse.
	Obj map[string]any
	// Raw is the record's bytes (the line without its trailing newline, or the
	// raw chunk). Retained for UnparsedEvent evidence.
	Raw []byte
	// Offset is the byte offset of this frame's first byte within the file.
	Offset int64
	// ParseErr is non-nil when a JSONL line could not be decoded; the tailer
	// turns it into a core.UnparsedEvent (never a silent drop).
	ParseErr error
}

// Codec frames appended bytes into records. Decode consumes as many complete
// frames as buf holds and returns the incomplete tail as carry (to be prepended
// to the next read). startOffset is the file offset of buf[0].
type Codec interface {
	Decode(buf []byte, startOffset int64) (frames []Frame, carry []byte)
}

// JSONLCodec frames newline-delimited JSON: it consumes through the last '\n',
// decodes each line, and carries the trailing partial line. A line that fails to
// decode becomes a ParseErr frame and the codec resyncs at the next newline.
type JSONLCodec struct {
	// MaxCarry bounds the partial-line carry; 0 uses defaultMaxCarry.
	MaxCarry int
}

// Decode implements Codec for JSONL.
func (c JSONLCodec) Decode(buf []byte, startOffset int64) ([]Frame, []byte) {
	max := c.MaxCarry
	if max <= 0 {
		max = defaultMaxCarry
	}
	var frames []Frame
	off := startOffset
	i := 0
	for {
		nl := bytes.IndexByte(buf[i:], '\n')
		if nl < 0 {
			break
		}
		line := buf[i : i+nl]
		lineOff := off
		consumed := nl + 1
		off += int64(consumed)
		i += consumed
		trimmed := bytes.TrimSpace(line)
		if len(trimmed) == 0 {
			continue
		}
		var obj map[string]any
		if err := json.Unmarshal(trimmed, &obj); err != nil {
			frames = append(frames, Frame{Raw: cloneBytes(line), Offset: lineOff, ParseErr: err})
			continue
		}
		frames = append(frames, Frame{Obj: obj, Raw: cloneBytes(line), Offset: lineOff})
	}
	carry := cloneBytes(buf[i:])
	if len(carry) > max {
		// An unterminated line larger than the bound: surface it as a parse
		// failure and resync (drop the oversized carry).
		frames = append(frames, Frame{
			Raw:      carry,
			Offset:   off,
			ParseErr: errOversizeCarry(len(carry), max),
		})
		return frames, nil
	}
	return frames, carry
}

// RawTextCodec frames shell spools: every appended byte is consumed as one raw
// chunk with no parsing and no carry (byte-count progress only, §7.2).
type RawTextCodec struct{}

// Decode implements Codec for raw text.
func (RawTextCodec) Decode(buf []byte, startOffset int64) ([]Frame, []byte) {
	if len(buf) == 0 {
		return nil, nil
	}
	return []Frame{{Raw: cloneBytes(buf), Offset: startOffset}}, nil
}

func cloneBytes(b []byte) []byte {
	if len(b) == 0 {
		return nil
	}
	out := make([]byte, len(b))
	copy(out, b)
	return out
}

type oversizeCarryError struct {
	got, max int
}

func (e oversizeCarryError) Error() string {
	return "tail: partial line exceeds bounded carry"
}

func errOversizeCarry(got, max int) error { return oversizeCarryError{got: got, max: max} }
