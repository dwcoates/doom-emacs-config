package wire

import (
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/durationpb"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

// The Any envelope layer is a WIRE CONTRACT shared by four packages that were
// each carrying their own copy of it (shim-store's server, the sidecar's store
// client, the daemon's shimclient, and the daemon's storesub). These tests pin
// the byte-level behavior those copies had, so the unification is verifiable
// rather than asserted: TestWriteAnyIsByteIdenticalToTheHandRolledSequence is
// the one that actually holds the contract, and the rest pin the error
// classification each call site keys on.
//
// The messages under test are well-known types on purpose. This module owns
// framing, not the agent-shim schema, so it must not grow a dependency on
// agentrepl/proto to test itself.

// handRolled encodes msg exactly the way all four copies did before the
// extraction: anypb.New, proto.Marshal, WriteFrame.
func handRolled(t *testing.T, w io.Writer, msg proto.Message) {
	t.Helper()
	env, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	b, err := proto.Marshal(env)
	if err != nil {
		t.Fatalf("proto.Marshal: %v", err)
	}
	if err := WriteFrame(w, b); err != nil {
		t.Fatalf("WriteFrame: %v", err)
	}
}

func TestWriteAnyIsByteIdenticalToTheHandRolledSequence(t *testing.T) {
	// Arrange
	msg := wrapperspb.String("hello agent-shim")
	var want, got bytes.Buffer
	handRolled(t, &want, msg)

	// Act
	if err := WriteAny(&got, msg); err != nil {
		t.Fatalf("WriteAny: %v", err)
	}

	// Assert — the same bytes on the socket, header included.
	if !bytes.Equal(got.Bytes(), want.Bytes()) {
		t.Fatalf("WriteAny emitted %x, want %x", got.Bytes(), want.Bytes())
	}
}

func TestReadAnyDecodesTheHandRolledEncoding(t *testing.T) {
	// Arrange — the other direction of the same contract: a peer still running
	// the old code must be readable.
	var buf bytes.Buffer
	handRolled(t, &buf, wrapperspb.String("from an older peer"))

	// Act
	got, err := ReadAny(&buf)

	// Assert
	if err != nil {
		t.Fatalf("ReadAny: %v", err)
	}
	sv, ok := got.(*wrapperspb.StringValue)
	if !ok || sv.GetValue() != "from an older peer" {
		t.Fatalf("ReadAny = %#v, want a StringValue carrying the payload", got)
	}
}

func TestWriteAnyReadAnyRoundTrip(t *testing.T) {
	// Arrange
	msg := durationpb.New(90)
	var buf bytes.Buffer

	// Act
	if err := WriteAny(&buf, msg); err != nil {
		t.Fatalf("WriteAny: %v", err)
	}
	got, err := ReadAny(&buf)

	// Assert
	if err != nil {
		t.Fatalf("ReadAny: %v", err)
	}
	if !proto.Equal(got, msg) {
		t.Fatalf("round-trip mismatch: got %v want %v", got, msg)
	}
}

func TestReadAnyResolvesTheConcreteTypeNotTheEnvelope(t *testing.T) {
	// Arrange — the type_url IS the discriminator; a caller type-switches on
	// what comes back.
	var buf bytes.Buffer
	if err := WriteAny(&buf, durationpb.New(1)); err != nil {
		t.Fatalf("WriteAny: %v", err)
	}

	// Act
	got, err := ReadAny(&buf)

	// Assert
	if err != nil {
		t.Fatalf("ReadAny: %v", err)
	}
	if _, ok := got.(*durationpb.Duration); !ok {
		t.Fatalf("ReadAny returned %T, want *durationpb.Duration", got)
	}
}

func TestReadAnySequentialFramesOnOneStream(t *testing.T) {
	// Arrange — every hop reads in a loop off a long-lived connection.
	var buf bytes.Buffer
	if err := WriteAny(&buf, wrapperspb.String("first")); err != nil {
		t.Fatalf("WriteAny first: %v", err)
	}
	if err := WriteAny(&buf, wrapperspb.String("second")); err != nil {
		t.Fatalf("WriteAny second: %v", err)
	}

	// Act
	first, err1 := ReadAny(&buf)
	second, err2 := ReadAny(&buf)

	// Assert
	if err1 != nil || err2 != nil {
		t.Fatalf("reads failed: %v / %v", err1, err2)
	}
	if first.(*wrapperspb.StringValue).GetValue() != "first" ||
		second.(*wrapperspb.StringValue).GetValue() != "second" {
		t.Fatalf("got %v then %v, want first then second", first, second)
	}
}

func TestReadAnyReturnsCleanEOFVerbatim(t *testing.T) {
	// Arrange — every call site distinguishes a clean disconnect from a fault
	// with errors.Is(err, io.EOF); wrapping it here would be a behavior change.
	// Act
	_, err := ReadAny(bytes.NewReader(nil))
	// Assert
	if !errors.Is(err, io.EOF) {
		t.Fatalf("want io.EOF on a clean boundary, got %v", err)
	}
}

func TestReadAnyDoesNotWrapTheFrameError(t *testing.T) {
	// Arrange — verbatim means IDENTICAL, not merely errors.Is-compatible: the
	// three migrated copies all returned ReadFrame's error untouched.
	// Act
	_, err := ReadAny(bytes.NewReader(nil))
	// Assert
	if err != io.EOF { //nolint:errorlint // identity is the property under test
		t.Fatalf("ReadAny wrapped the frame error (%v); it must pass it through", err)
	}
}

func TestReadAnyKeepsATransportErrorMatchable(t *testing.T) {
	// Arrange — storesub classifies a read deadline with
	// errors.Is(err, os.ErrDeadlineExceeded) and shimclient classifies a closed
	// socket with net.ErrClosed. Both arrive from the transport mid-header, so
	// the sentinel has to survive ReadFrame's wrap and ReadAny's passthrough.
	sentinel := errors.New("transport is gone")
	// Act
	_, err := ReadAny(errReader{err: sentinel})
	// Assert
	if !errors.Is(err, sentinel) {
		t.Fatalf("transport sentinel did not survive ReadAny: %v", err)
	}
}

// errReader fails every read with a fixed error, standing in for a socket
// whose deadline elapsed or which was closed underneath the reader.
type errReader struct{ err error }

func (e errReader) Read([]byte) (int, error) { return 0, e.err }

func TestReadAnyReportsATruncatedFrame(t *testing.T) {
	// Arrange — a header claiming 10 bytes with only 3 behind it.
	var buf bytes.Buffer
	buf.Write([]byte{0x00, 0x00, 0x00, 0x0a})
	buf.Write([]byte("abc"))
	// Act
	_, err := ReadAny(&buf)
	// Assert
	if !errors.Is(err, io.ErrUnexpectedEOF) {
		t.Fatalf("want io.ErrUnexpectedEOF on a truncated frame, got %v", err)
	}
}

func TestReadAnySurfacesAnOversizeHeader(t *testing.T) {
	// Arrange — a corrupt or hostile length prefix.
	hdr := []byte{0x02, 0x00, 0x00, 0x01} // MaxFrame + 1
	// Act
	_, err := ReadAny(bytes.NewReader(hdr))
	// Assert
	if !errors.Is(err, ErrFrameTooLarge) {
		t.Fatalf("want ErrFrameTooLarge, got %v", err)
	}
}

func TestReadAnyRejectsAPayloadThatIsNotAnAny(t *testing.T) {
	// Arrange — a well-framed payload of garbage.
	var buf bytes.Buffer
	if err := WriteFrame(&buf, []byte{0xff, 0xff, 0xff, 0xff}); err != nil {
		t.Fatalf("WriteFrame: %v", err)
	}
	// Act
	_, err := ReadAny(&buf)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "decoding Any frame") {
		t.Fatalf("want a decode failure, got %v", err)
	}
}

func TestReadAnyRejectsAnUnknownTypeURL(t *testing.T) {
	// Arrange — a type_url this binary's compiled schema set does not know,
	// which is the version-skew signal.
	var buf bytes.Buffer
	raw, err := proto.Marshal(&anypb.Any{TypeUrl: "type.googleapis.com/not.A.Real.Message"})
	if err != nil {
		t.Fatalf("proto.Marshal: %v", err)
	}
	if werr := WriteFrame(&buf, raw); werr != nil {
		t.Fatalf("WriteFrame: %v", werr)
	}
	// Act
	_, err = ReadAny(&buf)
	// Assert
	if err == nil {
		t.Fatal("an unresolvable type_url must error, never yield a nil message")
	}
}

func TestReadAnyNamesTheUnresolvableTypeURL(t *testing.T) {
	// Arrange — the type URL is the only thing that identifies the skew, so it
	// must reach the log rather than being dropped.
	var buf bytes.Buffer
	raw, err := proto.Marshal(&anypb.Any{TypeUrl: "type.googleapis.com/not.A.Real.Message"})
	if err != nil {
		t.Fatalf("proto.Marshal: %v", err)
	}
	if werr := WriteFrame(&buf, raw); werr != nil {
		t.Fatalf("WriteFrame: %v", werr)
	}
	// Act
	_, err = ReadAny(&buf)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "not.A.Real.Message") {
		t.Fatalf("error must name the unresolvable type URL, got %v", err)
	}
}

func TestMarshalAnyMatchesWriteAnyPayload(t *testing.T) {
	// Arrange — shimclient encodes with MarshalAny OUTSIDE its write lock and
	// then calls WriteFrame itself, so the two paths must agree byte for byte.
	msg := wrapperspb.String("split encode")
	var whole bytes.Buffer
	if err := WriteAny(&whole, msg); err != nil {
		t.Fatalf("WriteAny: %v", err)
	}

	// Act
	payload, err := MarshalAny(msg)
	if err != nil {
		t.Fatalf("MarshalAny: %v", err)
	}
	var split bytes.Buffer
	if werr := WriteFrame(&split, payload); werr != nil {
		t.Fatalf("WriteFrame: %v", werr)
	}

	// Assert
	if !bytes.Equal(split.Bytes(), whole.Bytes()) {
		t.Fatalf("split encode emitted %x, want %x", split.Bytes(), whole.Bytes())
	}
}

func TestUnmarshalAnyMatchesReadAny(t *testing.T) {
	// Arrange — the decode halves must agree too.
	payload, err := MarshalAny(wrapperspb.String("split decode"))
	if err != nil {
		t.Fatalf("MarshalAny: %v", err)
	}

	// Act
	got, err := UnmarshalAny(payload)

	// Assert
	if err != nil {
		t.Fatalf("UnmarshalAny: %v", err)
	}
	if got.(*wrapperspb.StringValue).GetValue() != "split decode" {
		t.Fatalf("UnmarshalAny = %v, want the round-tripped value", got)
	}
}

func TestWriteAnySurfacesAnOversizeMessage(t *testing.T) {
	// Arrange — a message whose Any envelope exceeds MaxFrame.
	msg := wrapperspb.Bytes(make([]byte, MaxFrame+1))
	// Act
	err := WriteAny(io.Discard, msg)
	// Assert
	if !errors.Is(err, ErrFrameTooLarge) {
		t.Fatalf("want ErrFrameTooLarge, got %v", err)
	}
}

func TestWriteAnyReportsAWriteFailure(t *testing.T) {
	// Arrange — a socket that refuses the bytes must not be a silent no-op.
	// Act
	err := WriteAny(failingWriter{}, wrapperspb.String("x"))
	// Assert
	if err == nil {
		t.Fatal("a failed write must surface, not be swallowed")
	}
}

// failingWriter fails every write, standing in for a dead socket.
type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("socket is gone") }
