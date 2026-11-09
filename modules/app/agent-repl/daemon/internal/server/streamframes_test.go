package server

import (
	"encoding/binary"
	"testing"
)

// envelope is one Connect envelope with FLAGS around PAYLOAD.
func envelope(flags byte, payload string) []byte {
	out := make([]byte, 5, 5+len(payload))
	out[0] = flags
	binary.BigEndian.PutUint32(out[1:], uint32(len(payload)))
	return append(out, payload...)
}

func TestFrameCounterCountsEveryDataEnvelope(t *testing.T) {
	two := append(envelope(0, "one"), envelope(0, "two")...)
	cases := []struct {
		name   string
		writes [][]byte
		want   uint64
	}{
		{name: "one envelope in one write", writes: [][]byte{envelope(0, "frame")}, want: 1},
		{name: "two envelopes in one write", writes: [][]byte{two}, want: 2},
		{name: "an envelope split across writes, header included",
			writes: [][]byte{two[:3], two[3:6], two[6:]}, want: 2},
		{name: "an empty payload is still a frame", writes: [][]byte{envelope(0, "")}, want: 1},
		{name: "the end-of-stream envelope is not a frame",
			writes: [][]byte{envelope(0, "frame"), envelope(0x02, "{}")}, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := &frameCounter{}

			// Act
			for _, w := range tc.writes {
				c.observe(w)
			}

			// Assert
			if got := c.frames.Load(); got != tc.want {
				t.Fatalf("frames = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestStreamFramesAnswersEveryLiveTaggedStream(t *testing.T) {
	// Arrange
	var f streamFrames
	a := f.register("a")
	b := f.register("b")
	a.observe(envelope(0, "x"))
	b.observe(append(envelope(0, "x"), envelope(0, "y")...))
	f.register("gone")
	f.forget("gone")

	// Act
	got := f.written()

	// Assert
	if len(got) != 2 || got[0].GetStreamTag() != "a" || got[0].GetFrames() != 1 ||
		got[1].GetStreamTag() != "b" || got[1].GetFrames() != 2 {
		t.Fatalf("written = %v, want a:1 and b:2 only", got)
	}
}
