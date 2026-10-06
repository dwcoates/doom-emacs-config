package vendortraffic

import (
	"bytes"
	"encoding/binary"
	"reflect"
	"strings"
	"testing"
)

// message builds one kernel message of the given type and total length, with
// its header filled in and the rest zero.
func message(ctx uint64, typ uint32, length int, flags uint16) []byte {
	b := make([]byte, length)
	binary.LittleEndian.PutUint64(b[0:], ctx)
	binary.LittleEndian.PutUint32(b[8:], typ)
	binary.LittleEndian.PutUint16(b[12:], uint16(length))
	binary.LittleEndian.PutUint16(b[14:], flags)
	return b
}

// updateMessage builds a source update the way the kernel lays it out: 496
// bytes on the measured Darwin 25, of which only the first 64 are read.
func updateMessage(ref uint64, received, sent uint64, closing bool) []byte {
	var flags uint16
	if closing {
		flags = headerFlagClosing
	}
	b := message(0, msgSrcUpdate, 496, flags)
	binary.LittleEndian.PutUint64(b[16:], ref)
	binary.LittleEndian.PutUint64(b[32:], 7) // rxpackets: never read
	binary.LittleEndian.PutUint64(b[40:], received)
	binary.LittleEndian.PutUint64(b[48:], 9) // txpackets: never read
	binary.LittleEndian.PutUint64(b[56:], sent)
	return b
}

// removedMessage builds a source removal.
func removedMessage(ref uint64) []byte {
	b := message(0, msgSrcRemoved, srcRemovedSize, 0)
	binary.LittleEndian.PutUint64(b[16:], ref)
	return b
}

// errorMessage builds the kernel's refusal of the request carrying ctx.
func errorMessage(ctx uint64, errno uint32) []byte {
	b := message(ctx, msgError, 24, 0)
	binary.LittleEndian.PutUint32(b[16:], errno)
	return b
}

// datagram concatenates messages the way the kernel batches them.
func datagram(messages ...[]byte) []byte { return bytes.Join(messages, nil) }

func TestEncodeAddAllSrcsLaysOutTheSubscription(t *testing.T) {
	// Arrange.
	want := make([]byte, 56)
	binary.LittleEndian.PutUint64(want[0:], 3)
	binary.LittleEndian.PutUint32(want[8:], 1002)
	binary.LittleEndian.PutUint16(want[12:], 56)
	binary.LittleEndian.PutUint64(want[16:], 0x1_0010_1000)
	binary.LittleEndian.PutUint32(want[32:], 2)
	binary.LittleEndian.PutUint32(want[36:], 4242)

	// Act.
	got := encodeAddAllSrcs(3, providerTCPKernel, 4242)

	// Assert.
	if !bytes.Equal(got, want) {
		t.Fatalf("encodeAddAllSrcs =\n%x\nwant\n%x", got, want)
	}
}

func TestEncodeGetUpdateAsksForEverySource(t *testing.T) {
	// Arrange.
	want := make([]byte, 24)
	binary.LittleEndian.PutUint64(want[0:], 11)
	binary.LittleEndian.PutUint32(want[8:], 1007)
	binary.LittleEndian.PutUint16(want[12:], 24)
	binary.LittleEndian.PutUint64(want[16:], ^uint64(0))

	// Act.
	got := encodeGetUpdate(11)

	// Assert.
	if !bytes.Equal(got, want) {
		t.Fatalf("encodeGetUpdate =\n%x\nwant\n%x", got, want)
	}
}

func TestDecodeDatagramReadsEachMessage(t *testing.T) {
	cases := []struct {
		name string
		in   []byte
		want []event
	}{
		{
			name: "a live source's update carries its cumulative counts",
			in:   updateMessage(263, 1_000_000, 424, false),
			want: []event{{kind: eventUpdate, srcRef: 263, counts: Counts{Received: 1_000_000, Sent: 424}}},
		},
		{
			name: "a closing source's update is marked closing",
			in:   updateMessage(264, 5, 6, true),
			want: []event{{kind: eventUpdate, srcRef: 264, closing: true, counts: Counts{Received: 5, Sent: 6}}},
		},
		{
			name: "a removal names its source",
			in:   removedMessage(265),
			want: []event{{kind: eventRemoved, srcRef: 265}},
		},
		{
			name: "a success names the request it answers",
			in:   message(9, msgSuccess, headerSize, 0),
			want: []event{{kind: eventSuccess, ctx: 9}},
		},
		{
			name: "an error names the request and the errno",
			in:   errorMessage(2, 22),
			want: []event{{kind: eventFailure, ctx: 2, errno: 22}},
		},
		{
			name: "an added notice is ignored",
			in:   message(0, msgSrcAdded, 32, 0),
			want: []event{{kind: eventIgnored}},
		},
		{
			name: "a batched datagram yields every message in order",
			in:   datagram(updateMessage(1, 10, 20, false), removedMessage(2), message(4, msgSuccess, headerSize, 0)),
			want: []event{
				{kind: eventUpdate, srcRef: 1, counts: Counts{Received: 10, Sent: 20}},
				{kind: eventRemoved, srcRef: 2},
				{kind: eventSuccess, ctx: 4},
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := decodeDatagram(tc.in)

			// Assert.
			if err != nil {
				t.Fatalf("decodeDatagram: %v", err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("decodeDatagram = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestDecodeDatagramRefusesAMalformedDatagram(t *testing.T) {
	zeroLength := message(0, msgSrcUpdate, headerSize, 0)
	binary.LittleEndian.PutUint16(zeroLength[12:], 0)
	overrun := message(0, msgSrcUpdate, headerSize, 0)
	binary.LittleEndian.PutUint16(overrun[12:], 496)
	cases := []struct {
		name string
		in   []byte
		want string
	}{
		{name: "trailing bytes shorter than a header", in: datagram(removedMessage(1), []byte{1, 2, 3}), want: "shorter than a message header"},
		{name: "a declared length below the header", in: zeroLength, want: "declares length 0"},
		{name: "a declared length past the datagram", in: overrun, want: "declares length 496"},
		{name: "a source update too short for its counts", in: message(0, msgSrcUpdate, 48, 0), want: "a source update of 48 bytes"},
		{name: "a removal too short for its source", in: message(0, msgSrcRemoved, 20, 0), want: "a source removal of 20 bytes"},
		{name: "an error too short for its errno", in: message(0, msgError, 16, 0), want: "an error message of 16 bytes"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := decodeDatagram(tc.in)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("decodeDatagram error = %v, want one containing %q", err, tc.want)
			}
		})
	}
}
