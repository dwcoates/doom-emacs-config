package dlog

import (
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"strconv"
	"sync"
	"time"

	"agent-repld/internal/envc"
)

// EnvTestLogPipe asks a TEST RUN's daemon to hand every record it would write
// to the run log or to a daemon-written workspace sink (daemon, webapp,
// sidecar) to the unix socket it names, instead of appending it to the file.
// The terminal mirror goes over the same socket.
//
// WHY. An integration suite starts ~1,000 daemons, and their logs were the
// daemon side's largest share of a full run's tens of gigabytes of writes:
// every record once into its sink and once more into the harness's capture of
// the terminal mirror. The harness reads those records back in the same
// process that started the daemon, so it keeps them in memory, keyed by the
// file each record names, and writes them to disk only for a test that fails.
//
// WHAT STAYS ON DISK. Every file is still created exactly as it is without the
// seam -- the run log, each sink's minted target and its canonical link --
// so nothing that lists, stats or links them changes; they stay empty. The
// shim sink is not routed: the shim writes it itself through the descriptor
// the daemon lends it, and one file must have one writer's order.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (envc.EnvForbidVendorCalls), the
// marker every test harness sets and a live daemon never carries: a live
// daemon that inherited it REFUSES TO BOOT rather than send its narrative to
// a socket nobody keeps.
const EnvTestLogPipe = "AGENT_REPL_TEST_LOG_PIPE"

// The test log pipe's frame kinds. A frame is the kind byte, a uvarint-sized
// key and a uvarint-sized body (AppendPipeFrame).
const (
	// PipeHello opens a connection: key is the daemon's logs directory, body
	// its pid in decimal.
	PipeHello byte = 'H'
	// PipeOpen says the file the key names is routed from here on, before any
	// record of it arrives. Its body is empty.
	PipeOpen byte = 'O'
	// PipeRecord is one record's exact JSONL bytes, keyed by the file it would
	// have been appended to.
	PipeRecord byte = 'R'
	// PipeTerminal is a batch of the terminal mirror's bytes. Its key is empty.
	PipeTerminal byte = 'T'
)

// testLogPipeFromEnv reads EnvTestLogPipe at boot: empty is off, and a socket
// named without the vendor guard is a boot refusal.
func testLogPipeFromEnv(contracts envc.Contracts, getenv func(string) string) (string, error) {
	sock := getenv(EnvTestLogPipe)
	if sock == "" {
		return "", nil
	}
	if !contracts.ForbidVendorCalls() {
		return "", fmt.Errorf("dlog: %s is a test-run seam and is honored only with %s set; a live daemon writes its logs to its own files", EnvTestLogPipe, envc.EnvForbidVendorCalls)
	}
	return sock, nil
}

// logPipe is the daemon's one connection to the harness's collector. Every
// sink writes through it, so a frame is built and written whole under mu: the
// collector never sees two records' bytes interleaved.
type logPipe struct {
	mu   sync.Mutex
	conn net.Conn
	buf  []byte
}

// dialLogPipe connects to the collector and says which logs directory and pid
// the frames that follow belong to.
func dialLogPipe(sock, logsDir string, pid int) (*logPipe, error) {
	conn, err := net.Dial("unix", sock)
	if err != nil {
		return nil, fmt.Errorf("dlog: dial the test log pipe %s: %w", sock, err)
	}
	// A unix socket buffers 8KB by default on macOS, so a collector that fell
	// one boot's worth of records behind would block every log call here.
	if err := conn.(*net.UnixConn).SetWriteBuffer(PipeBufferBytes); err != nil {
		_ = conn.Close()
		return nil, fmt.Errorf("dlog: size the test log pipe's buffer: %w", err)
	}
	p, err := newLogPipe(conn, logsDir, pid)
	if err != nil {
		return nil, err
	}
	// THE ACK IS WHAT MAKES THE FIRST RECORD READABLE. The collector reads a
	// logs directory by draining the connections it has registered for it,
	// so nothing is written until it has registered this one.
	if err := awaitPipeAck(conn, pipeAckBound); err != nil {
		_ = conn.Close()
		return nil, err
	}
	return p, nil
}

// PipeBufferBytes sizes each end of the test log pipe's socket.
const PipeBufferBytes = 4 << 20

// pipeAck is the collector's one-byte answer to a hello, and pipeAckBound how
// long a booting daemon waits for it.
const (
	pipeAck      = 'A'
	pipeAckBound = 30 * time.Second
)

// awaitPipeAck reads the collector's ack of the hello.
func awaitPipeAck(conn net.Conn, bound time.Duration) error {
	if err := conn.SetReadDeadline(time.Now().Add(bound)); err != nil {
		return fmt.Errorf("dlog: bound the test log pipe's ack: %w", err)
	}
	ack := make([]byte, 1)
	if _, err := io.ReadFull(conn, ack); err != nil {
		return fmt.Errorf("dlog: the test log pipe never acked the hello: %w", err)
	}
	if ack[0] != pipeAck {
		return fmt.Errorf("dlog: the test log pipe answered the hello with %q, want its ack", ack[0])
	}
	if err := conn.SetReadDeadline(time.Time{}); err != nil {
		return fmt.Errorf("dlog: clear the test log pipe's ack bound: %w", err)
	}
	return nil
}

// newLogPipe sends the hello over an open connection.
func newLogPipe(conn net.Conn, logsDir string, pid int) (*logPipe, error) {
	p := &logPipe{conn: conn}
	if err := p.send(PipeHello, logsDir, []byte(strconv.Itoa(pid))); err != nil {
		_ = conn.Close()
		return nil, err
	}
	return p, nil
}

// send writes one frame. A failure is the sink's failure: it is returned, and
// the caller reports it exactly as a failed file append.
func (p *logPipe) send(kind byte, key string, body []byte) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.buf = AppendPipeFrame(p.buf[:0], kind, key, body)
	if _, err := p.conn.Write(p.buf); err != nil {
		return fmt.Errorf("dlog: send a %q frame for %q over the test log pipe: %w", kind, key, err)
	}
	return nil
}

// close ends the connection; the collector reads it to its end.
func (p *logPipe) close() error {
	if err := p.conn.Close(); err != nil {
		return fmt.Errorf("dlog: close the test log pipe: %w", err)
	}
	return nil
}

// pipeTerminal is the terminal mirror's writer when the pipe is on.
type pipeTerminal struct{ p *logPipe }

func (t pipeTerminal) Write(b []byte) (int, error) {
	if err := t.p.send(PipeTerminal, "", b); err != nil {
		return 0, err
	}
	return len(b), nil
}

// AppendPipeFrame appends one frame to b.
func AppendPipeFrame(b []byte, kind byte, key string, body []byte) []byte {
	b = append(b, kind)
	b = binary.AppendUvarint(b, uint64(len(key)))
	b = append(b, key...)
	b = binary.AppendUvarint(b, uint64(len(body)))
	return append(b, body...)
}

// ParsePipeFrame decodes the frame at the head of b. ok is false when b holds
// only part of one; n is the frame's whole length. A size that cannot be
// decoded is an error: the stream is corrupt from there on.
func ParsePipeFrame(b []byte) (kind byte, key string, body []byte, n int, ok bool, err error) {
	if len(b) < 1 {
		return 0, "", nil, 0, false, nil
	}
	kind, pos := b[0], 1
	klen, w := binary.Uvarint(b[pos:])
	if w < 0 {
		return 0, "", nil, 0, false, fmt.Errorf("dlog: a test log pipe frame's key size overflows")
	}
	if w == 0 || uint64(len(b)-pos-w) < klen {
		return 0, "", nil, 0, false, nil
	}
	pos += w
	key = string(b[pos : pos+int(klen)])
	pos += int(klen)
	blen, w := binary.Uvarint(b[pos:])
	if w < 0 {
		return 0, "", nil, 0, false, fmt.Errorf("dlog: a test log pipe frame's body size overflows")
	}
	if w == 0 || uint64(len(b)-pos-w) < blen {
		return 0, "", nil, 0, false, nil
	}
	pos += w
	return kind, key, b[pos : pos+int(blen)], pos + int(blen), true, nil
}
