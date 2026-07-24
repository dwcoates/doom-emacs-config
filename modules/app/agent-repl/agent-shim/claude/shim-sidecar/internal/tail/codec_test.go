package tail

import (
	"strings"
	"testing"
)

func TestJSONLCodecCompleteLines(t *testing.T) {
	// Arrange
	buf := []byte(`{"a":1}` + "\n" + `{"b":2}` + "\n")
	// Act
	frames, carry := JSONLCodec{}.Decode(buf, 0)
	// Assert
	if len(frames) != 2 {
		t.Fatalf("frames = %d, want 2", len(frames))
	}
	if len(carry) != 0 {
		t.Fatalf("carry = %q, want empty", carry)
	}
	if frames[0].Offset != 0 || frames[1].Offset != 8 {
		t.Fatalf("offsets = [%d %d], want [0 8]", frames[0].Offset, frames[1].Offset)
	}
	if frames[0].Obj["a"].(float64) != 1 {
		t.Fatalf("frame0 obj = %v", frames[0].Obj)
	}
}

func TestJSONLCodecPartialTailCarried(t *testing.T) {
	// Arrange: a complete line then a partial one (no trailing newline).
	buf := []byte(`{"a":1}` + "\n" + `{"b":`)
	// Act
	frames, carry := JSONLCodec{}.Decode(buf, 0)
	// Assert
	if len(frames) != 1 {
		t.Fatalf("frames = %d, want 1 (partial carried)", len(frames))
	}
	if string(carry) != `{"b":` {
		t.Fatalf("carry = %q, want the partial line", carry)
	}
}

func TestJSONLCodecCarryResumes(t *testing.T) {
	// Arrange: first read ends mid-line; the carry + next read completes it.
	frames1, carry := JSONLCodec{}.Decode([]byte(`{"a":`), 0)
	if len(frames1) != 0 {
		t.Fatalf("frames1 = %d, want 0", len(frames1))
	}
	// Act: prepend carry to the next chunk (as the tailer does), continuing the
	// file offset from where carry began.
	next := append(carry, []byte(`1}`+"\n")...)
	frames2, carry2 := JSONLCodec{}.Decode(next, 0)
	// Assert
	if len(frames2) != 1 || carry2 != nil {
		t.Fatalf("frames2 = %d carry2 = %q, want 1 and empty", len(frames2), carry2)
	}
	if frames2[0].Obj["a"].(float64) != 1 {
		t.Fatalf("reassembled obj = %v", frames2[0].Obj)
	}
}

func TestJSONLCodecParseErrorResyncs(t *testing.T) {
	// Arrange: a bad line between two good ones.
	buf := []byte(`{"a":1}` + "\n" + `not json` + "\n" + `{"b":2}` + "\n")
	// Act
	frames, _ := JSONLCodec{}.Decode(buf, 0)
	// Assert: three frames, the middle one a ParseErr; the good lines still decode.
	if len(frames) != 3 {
		t.Fatalf("frames = %d, want 3", len(frames))
	}
	if frames[1].ParseErr == nil {
		t.Fatalf("frame1 should carry a ParseErr")
	}
	if frames[1].Obj != nil {
		t.Fatalf("frame1 should have no decoded object")
	}
	if frames[2].Obj["b"].(float64) != 2 {
		t.Fatalf("frame2 (post-resync) obj = %v", frames[2].Obj)
	}
	if string(frames[1].Raw) != "not json" {
		t.Fatalf("frame1 raw = %q, want the offending line", frames[1].Raw)
	}
}

func TestJSONLCodecBlankLinesSkipped(t *testing.T) {
	// Arrange
	buf := []byte("\n\n" + `{"a":1}` + "\n\n")
	// Act
	frames, _ := JSONLCodec{}.Decode(buf, 0)
	// Assert
	if len(frames) != 1 {
		t.Fatalf("frames = %d, want 1 (blanks skipped)", len(frames))
	}
}

func TestJSONLCodecOversizeCarryResyncs(t *testing.T) {
	// Arrange: a partial line longer than the (tiny) bound, no newline.
	buf := []byte(strings.Repeat("x", 100))
	// Act
	frames, carry := JSONLCodec{MaxCarry: 10}.Decode(buf, 0)
	// Assert: one ParseErr frame, carry dropped (resync).
	if len(frames) != 1 || frames[0].ParseErr == nil {
		t.Fatalf("frames = %+v, want one ParseErr", frames)
	}
	if carry != nil {
		t.Fatalf("carry = %q, want dropped after oversize", carry)
	}
}

func TestRawTextCodecSingleChunk(t *testing.T) {
	// Arrange
	buf := []byte("some raw\nbytes without json meaning")
	// Act
	frames, carry := RawTextCodec{}.Decode(buf, 42)
	// Assert
	if len(frames) != 1 || carry != nil {
		t.Fatalf("frames = %d carry = %q, want 1 and nil", len(frames), carry)
	}
	if frames[0].Offset != 42 || string(frames[0].Raw) != string(buf) {
		t.Fatalf("frame = %+v", frames[0])
	}
}

func TestRawTextCodecEmpty(t *testing.T) {
	// Arrange / Act
	frames, carry := RawTextCodec{}.Decode(nil, 0)
	// Assert
	if len(frames) != 0 || carry != nil {
		t.Fatalf("empty decode = %d frames / %q carry", len(frames), carry)
	}
}
