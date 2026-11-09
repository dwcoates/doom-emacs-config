package server

import (
	"encoding/binary"
	"sort"
	"sync"
	"sync/atomic"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// StreamTagHeader is the request header a client (the test harness) opens a
// standing stream with to have its written frames counted: the tag AwaitIdle
// names the stream's frames under. An untagged stream is not counted.
const StreamTagHeader = "Agent-Repl-Stream-Tag"

// streamFrames is every accepted standing stream's count of frames written,
// by the tag its client opened it with. It is what orders the test barrier
// (AwaitIdle) with the stream a test reads: a frame written is not yet a
// frame read, and the count says how many to wait for.
type streamFrames struct {
	mu      sync.Mutex
	streams map[string]*frameCounter
}

// frameCounter counts the Connect envelopes written on one stream, parsing
// them out of the bytes as they are written: a 1-byte flags field, a 4-byte
// big-endian length, then the payload. An end-of-stream envelope (flag 0x02)
// is not a frame the client reads as a push, so it is not counted.
type frameCounter struct {
	frames atomic.Uint64
	// header holds a partial envelope prefix; payloadLeft is how much of the
	// current envelope's payload is still to come.
	header      []byte
	payloadLeft uint64
}

// observe counts the envelopes that complete in P.
func (c *frameCounter) observe(p []byte) {
	for len(p) > 0 {
		if c.payloadLeft > 0 {
			n := min(uint64(len(p)), c.payloadLeft)
			c.payloadLeft -= n
			p = p[n:]
			continue
		}
		need := 5 - len(c.header)
		take := min(need, len(p))
		c.header = append(c.header, p[:take]...)
		p = p[take:]
		if len(c.header) < 5 {
			return
		}
		flags := c.header[0]
		c.payloadLeft = uint64(binary.BigEndian.Uint32(c.header[1:5]))
		c.header = c.header[:0]
		if flags&0x02 == 0 {
			c.frames.Add(1)
		}
	}
}

// register counts an accepted stream under the tag its client gave it.
func (f *streamFrames) register(tag string) *frameCounter {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.streams == nil {
		f.streams = map[string]*frameCounter{}
	}
	c := &frameCounter{}
	f.streams[tag] = c
	return c
}

// forget drops a stream whose response ended.
func (f *streamFrames) forget(tag string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.streams, tag)
}

// written answers every live stream's frames written, ordered by id.
func (f *streamFrames) written() []*agentreplv1.StreamFramesWritten {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]*agentreplv1.StreamFramesWritten, 0, len(f.streams))
	for tag, c := range f.streams {
		out = append(out, &agentreplv1.StreamFramesWritten{StreamTag: tag, Frames: c.frames.Load()})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].GetStreamTag() < out[j].GetStreamTag() })
	return out
}
