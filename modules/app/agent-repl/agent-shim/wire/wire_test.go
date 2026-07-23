package wire

import (
	"bytes"
	"errors"
	"io"
	"testing"
)

func TestWriteReadRoundTrip(t *testing.T) {
	tests := []struct {
		name    string
		payload []byte
	}{
		{name: "ordinary payload", payload: []byte("hello agent-shim")},
		{name: "empty payload", payload: []byte{}},
		{name: "binary payload", payload: []byte{0x00, 0xff, 0x10, 0x00}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var buf bytes.Buffer

			// Act
			if err := WriteFrame(&buf, tt.payload); err != nil {
				t.Fatalf("WriteFrame: %v", err)
			}
			got, err := ReadFrame(&buf)

			// Assert
			if err != nil {
				t.Fatalf("ReadFrame: %v", err)
			}
			if !bytes.Equal(got, tt.payload) {
				t.Fatalf("round-trip mismatch: got %q want %q", got, tt.payload)
			}
		})
	}
}

func TestReadFrameSequential(t *testing.T) {
	// Arrange
	var buf bytes.Buffer
	first, second := []byte("first"), []byte("second frame")
	if err := WriteFrame(&buf, first); err != nil {
		t.Fatalf("WriteFrame first: %v", err)
	}
	if err := WriteFrame(&buf, second); err != nil {
		t.Fatalf("WriteFrame second: %v", err)
	}

	// Act / Assert
	got1, err := ReadFrame(&buf)
	if err != nil || !bytes.Equal(got1, first) {
		t.Fatalf("first frame: got %q err %v", got1, err)
	}
	got2, err := ReadFrame(&buf)
	if err != nil || !bytes.Equal(got2, second) {
		t.Fatalf("second frame: got %q err %v", got2, err)
	}
}

func TestReadFrameCleanEOF(t *testing.T) {
	// Arrange: an empty stream is a clean close between frames.
	// Act
	_, err := ReadFrame(bytes.NewReader(nil))
	// Assert
	if !errors.Is(err, io.EOF) {
		t.Fatalf("want io.EOF on empty stream, got %v", err)
	}
}

func TestReadFrameTruncatedHeader(t *testing.T) {
	// Arrange: two header bytes then the stream dies.
	r := bytes.NewReader([]byte{0x00, 0x00})
	// Act
	_, err := ReadFrame(r)
	// Assert
	if err == nil || errors.Is(err, io.EOF) {
		t.Fatalf("want loud truncation error, got %v", err)
	}
}

func TestReadFrameTruncatedPayload(t *testing.T) {
	// Arrange: header claims 10 bytes, only 3 arrive.
	var buf bytes.Buffer
	buf.Write([]byte{0x00, 0x00, 0x00, 0x0a})
	buf.Write([]byte("abc"))
	// Act
	_, err := ReadFrame(&buf)
	// Assert
	if err == nil {
		t.Fatal("want error on truncated payload, got nil")
	}
}

func TestReadFrameOversizeHeader(t *testing.T) {
	// Arrange: header claims MaxFrame+1 bytes.
	var hdr [4]byte
	hdr[0], hdr[1], hdr[2], hdr[3] = 0x02, 0x00, 0x00, 0x01 // 32MiB + 1
	// Act
	_, err := ReadFrame(bytes.NewReader(hdr[:]))
	// Assert
	if !errors.Is(err, ErrFrameTooLarge) {
		t.Fatalf("want ErrFrameTooLarge, got %v", err)
	}
}

func TestWriteFrameOversizePayload(t *testing.T) {
	// Arrange
	huge := make([]byte, MaxFrame+1)
	// Act
	err := WriteFrame(io.Discard, huge)
	// Assert
	if !errors.Is(err, ErrFrameTooLarge) {
		t.Fatalf("want ErrFrameTooLarge, got %v", err)
	}
}
