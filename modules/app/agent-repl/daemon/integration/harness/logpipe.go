package harness

import (
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"sync"
	"syscall"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// THE TEST LOG PIPE (dlog.EnvTestLogPipe). Every daemon this process starts
// sends its run-log and daemon-written workspace records over one unix socket
// to the collector below instead of appending them to its files, and the
// harness keeps them in memory keyed by the file each would have gone to. The
// files themselves are still created and stay empty, so nothing that globs,
// stats or links them changes; every read goes through readLog, which reads a
// piped file from memory. A FAILED test gets its records written into those
// files before they are preserved (materializePipedLogs).
//
// WHY. The daemon's logs were the daemon side's largest share of a run's
// writes, twice over: once into the sinks and once into the stderr capture of
// the terminal mirror, which now goes over the pipe too.
//
// A RECORD IS READABLE ONCE ITS WRITE HAS RETURNED, exactly as with a file. A
// unix socket's write has put the bytes in the collector's receive buffer by
// the time it returns, so a read first drains every connection of the logs
// directory it is about to read (barrier), and anything the daemon wrote
// before the read began is then in memory. A daemon's first record cannot
// race its connection's registration either: the daemon waits for the
// collector's ack of its hello before it writes anything else.
//
// Memory: a test daemon's records are on the order of a few hundred KB, held
// until the last daemon of that state root's test ends.

// pipeAck is the collector's one-byte answer to a hello.
const pipeAck = 'A'

// logPipe is the test process's one collector.
type logPipe struct {
	path string
	ln   *net.UnixListener

	mu sync.Mutex
	// files is every routed file's bytes, by its resolved path.
	files map[string][]byte
	// terminals is every daemon's terminal mirror, by pid.
	terminals map[int][]byte
	// conns is every registered connection, by its resolved logs directory.
	conns map[string][]*pipeConn
	// routed counts the live daemons of each resolved logs directory this
	// harness started with the pipe on.
	routed map[string]int
}

var (
	thePipe     *logPipe
	thePipeErr  error
	thePipeOnce sync.Once
)

// testLogPipe answers the process's collector, starting it on first use under
// the run root (a short path, as a unix socket's must be).
func testLogPipe(t *testing.T) *logPipe {
	t.Helper()
	thePipeOnce.Do(func() {
		root := RunRoot()
		if root == "" {
			thePipeErr = errors.New("harness: the test log pipe needs the run root; the suite's TestMain must call harness.Main or WithRunRoot")
			return
		}
		path := filepath.Join(root, "logpipe.sock")
		ln, err := net.ListenUnix("unix", &net.UnixAddr{Name: path, Net: "unix"})
		if err != nil {
			thePipeErr = fmt.Errorf("harness: listen for the test log pipe at %s: %w", path, err)
			return
		}
		thePipe = &logPipe{
			path:      path,
			ln:        ln,
			files:     map[string][]byte{},
			terminals: map[int][]byte{},
			conns:     map[string][]*pipeConn{},
			routed:    map[string]int{},
		}
		go thePipe.accept()
	})
	if thePipeErr != nil {
		t.Fatal(thePipeErr)
	}
	return thePipe
}

// accept serves every daemon that connects, for the life of the process.
func (p *logPipe) accept() {
	for {
		c, err := p.ln.AcceptUnix()
		if err != nil {
			// The listener lives as long as the process; an accept failure
			// is a broken collector, and every daemon that dials next fails
			// its boot loudly on its own.
			fmt.Fprintf(os.Stderr, "harness: the test log pipe stopped accepting: %v\n", err)
			return
		}
		go p.serve(c)
	}
}

// route marks a logs directory as piped for one daemon's life and answers its
// release.
func (p *logPipe) route(logsDir string) func() {
	dir := resolvedPath(logsDir)
	p.mu.Lock()
	p.routed[dir]++
	p.mu.Unlock()
	return func() {
		p.mu.Lock()
		defer p.mu.Unlock()
		if p.routed[dir]--; p.routed[dir] > 0 {
			return
		}
		delete(p.routed, dir)
		for key := range p.files {
			if filepath.Dir(key) == dir {
				delete(p.files, key)
			}
		}
		for _, c := range p.conns[dir] {
			delete(p.terminals, c.pid)
		}
		delete(p.conns, dir)
	}
}

// isRouted reports whether a resolved logs directory is piped.
func (p *logPipe) isRouted(dir string) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.routed[dir] > 0
}

// barrier drains every connection of a resolved logs directory, so every
// record written to it before the call is in memory when it returns.
func (p *logPipe) barrier(dir string) error {
	p.mu.Lock()
	conns := append([]*pipeConn(nil), p.conns[dir]...)
	p.mu.Unlock()
	var errs []error
	for _, c := range conns {
		errs = append(errs, c.drain())
	}
	return errors.Join(errs...)
}

// read answers a routed file's bytes from offset on, and whether the file is
// routed at all (a file never opened over the pipe is read from disk).
func (p *logPipe) read(key string, offset int64) ([]byte, bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	body, ok := p.files[key]
	if !ok {
		return nil, false
	}
	if offset >= int64(len(body)) {
		return nil, true
	}
	return append([]byte(nil), body[offset:]...), true
}

// terminal answers one daemon's terminal mirror so far.
func (p *logPipe) terminal(pid int) []byte {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]byte(nil), p.terminals[pid]...)
}

// materialize appends every routed file of a resolved logs directory to its
// file on disk, for a failed test's preserved logs.
func (p *logPipe) materialize(dir string) error {
	p.mu.Lock()
	bodies := map[string][]byte{}
	for key, body := range p.files {
		if filepath.Dir(key) == dir {
			bodies[key] = append([]byte(nil), body...)
		}
	}
	p.mu.Unlock()
	var errs []error
	for key, body := range bodies {
		f, err := os.OpenFile(key, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0o644)
		if err != nil {
			errs = append(errs, fmt.Errorf("harness: write the piped %s: %w", key, err))
			continue
		}
		if _, err := f.Write(body); err != nil {
			errs = append(errs, fmt.Errorf("harness: write the piped %s: %w", key, err))
		}
		if err := f.Close(); err != nil {
			errs = append(errs, fmt.Errorf("harness: close the piped %s: %w", key, err))
		}
	}
	return errors.Join(errs...)
}

// pipeConn is one daemon process's connection.
type pipeConn struct {
	p    *logPipe
	c    *net.UnixConn
	raw  syscall.RawConn
	pid  int
	dirs map[string]string // a key's directory as the daemon spells it -> resolved

	mu        sync.Mutex
	cond      *sync.Cond
	requested uint64
	served    uint64
	closed    bool
	err       error
	pending   []byte
}

// serve reads the hello, registers and acks the connection, then reads frames
// until the daemon closes it.
func (p *logPipe) serve(c *net.UnixConn) {
	if err := c.SetReadBuffer(dlog.PipeBufferBytes); err != nil {
		fmt.Fprintf(os.Stderr, "harness: the test log pipe cannot size a connection's buffer: %v\n", err)
		_ = c.Close()
		return
	}
	raw, err := c.SyscallConn()
	if err != nil {
		fmt.Fprintf(os.Stderr, "harness: the test log pipe cannot read a connection: %v\n", err)
		_ = c.Close()
		return
	}
	pc := &pipeConn{p: p, c: c, raw: raw, dirs: map[string]string{}}
	pc.cond = sync.NewCond(&pc.mu)
	if err := pc.hello(); err != nil {
		fmt.Fprintf(os.Stderr, "harness: the test log pipe refused a connection: %v\n", err)
		_ = c.Close()
		return
	}
	buf := make([]byte, 64<<10)
	for {
		n, err := c.Read(buf)
		if n > 0 {
			pc.consume(buf[:n])
		}
		switch {
		case err == nil:
		case errors.Is(err, os.ErrDeadlineExceeded):
			if done := pc.serveBarriers(buf); done {
				return
			}
		default:
			pc.finish(err)
			return
		}
	}
}

// hello reads the connection's first frame, registers it under its logs
// directory and acks it.
func (pc *pipeConn) hello() error {
	var head []byte
	buf := make([]byte, 4096)
	for {
		n, err := pc.c.Read(buf)
		head = append(head, buf[:n]...)
		kind, key, body, used, ok, perr := dlog.ParsePipeFrame(head)
		if perr != nil {
			return perr
		}
		if ok {
			if kind != dlog.PipeHello {
				return fmt.Errorf("first frame is %q, want the hello", kind)
			}
			pid, err := strconv.Atoi(string(body))
			if err != nil {
				return fmt.Errorf("hello names pid %q: %w", body, err)
			}
			pc.pid = pid
			dir := resolvedPath(key)
			pc.dirs[key] = dir
			pc.pending = append(pc.pending, head[used:]...)
			pc.p.mu.Lock()
			pc.p.conns[dir] = append(pc.p.conns[dir], pc)
			pc.p.mu.Unlock()
			if _, err := pc.c.Write([]byte{pipeAck}); err != nil {
				return fmt.Errorf("ack the hello: %w", err)
			}
			return nil
		}
		if err != nil {
			return fmt.Errorf("read the hello: %w", err)
		}
	}
}

// consume decodes every whole frame in b, keeping a partial one for later.
func (pc *pipeConn) consume(b []byte) {
	pc.pending = append(pc.pending, b...)
	p := pc.p
	p.mu.Lock()
	defer p.mu.Unlock()
	for len(pc.pending) > 0 {
		kind, key, body, n, ok, err := dlog.ParsePipeFrame(pc.pending)
		if err != nil {
			pc.err = err
			pc.pending = nil
			return
		}
		if !ok {
			break
		}
		switch kind {
		case dlog.PipeOpen:
			k := pc.resolveKey(key)
			if _, ok := p.files[k]; !ok {
				p.files[k] = []byte{}
			}
		case dlog.PipeRecord:
			k := pc.resolveKey(key)
			p.files[k] = append(p.files[k], body...)
		case dlog.PipeTerminal:
			p.terminals[pc.pid] = append(p.terminals[pc.pid], body...)
		default:
			pc.err = fmt.Errorf("harness: the test log pipe got an unknown frame kind %q", kind)
		}
		pc.pending = pc.pending[n:]
	}
	if len(pc.pending) == 0 {
		pc.pending = nil
	}
}

// resolveKey spells a daemon's path for a file as the harness reads it: its
// directory resolved through symlinks (macOS's /tmp is /private/tmp).
func (pc *pipeConn) resolveKey(key string) string {
	dir := filepath.Dir(key)
	resolved, ok := pc.dirs[dir]
	if !ok {
		resolved = resolvedPath(dir)
		pc.dirs[dir] = resolved
	}
	return filepath.Join(resolved, filepath.Base(key))
}

// drain asks the serving goroutine to read everything the kernel holds for
// this connection and waits until it has.
func (pc *pipeConn) drain() error {
	pc.mu.Lock()
	defer pc.mu.Unlock()
	if pc.closed {
		return pc.err
	}
	pc.requested++
	ticket := pc.requested
	if err := pc.c.SetReadDeadline(time.Now()); err != nil {
		return fmt.Errorf("harness: wake the test log pipe's reader: %w", err)
	}
	for pc.served < ticket && !pc.closed {
		pc.cond.Wait()
	}
	return pc.err
}

// serveBarriers answers every drain asked for so far: it reads what the kernel
// holds without waiting, then marks the drains served. It reports whether the
// connection ended while it read.
func (pc *pipeConn) serveBarriers(buf []byte) bool {
	for {
		pc.mu.Lock()
		ticket := pc.requested
		err := pc.c.SetReadDeadline(time.Time{})
		pc.mu.Unlock()
		if err != nil {
			pc.finish(fmt.Errorf("harness: rearm the test log pipe's reader: %w", err))
			return true
		}
		interrupted, err := pc.readAvailable(buf)
		if err != nil {
			pc.finish(err)
			return true
		}
		if interrupted {
			continue
		}
		pc.mu.Lock()
		pc.served = ticket
		pc.cond.Broadcast()
		again := pc.requested > pc.served
		pc.mu.Unlock()
		if !again {
			return false
		}
	}
}

// readAvailable reads, without blocking, until the kernel holds nothing more
// for this connection. io.EOF says the daemon closed it.
//
// A DRAIN ASKED FOR WHILE THIS ONE RUNS IS NOT A BROKEN CONNECTION. It sets
// the read deadline to now, and RawConn.Read checks that deadline before it
// calls the read, so the read is refused: readAvailable answers interrupted,
// and serveBarriers rearms and drains again from the start, serving no drain
// until one pass has read the kernel dry.
func (pc *pipeConn) readAvailable(buf []byte) (interrupted bool, err error) {
	for {
		var n int
		var rerr error
		if err := pc.raw.Read(func(fd uintptr) bool {
			n, rerr = syscall.Read(int(fd), buf)
			return true
		}); err != nil {
			if errors.Is(err, os.ErrDeadlineExceeded) {
				return true, nil
			}
			return false, err
		}
		switch {
		case errors.Is(rerr, syscall.EAGAIN), errors.Is(rerr, syscall.EWOULDBLOCK):
			return false, nil
		case errors.Is(rerr, syscall.EINTR):
			continue
		case rerr != nil:
			return false, rerr
		case n == 0:
			return false, io.EOF
		}
		pc.consume(buf[:n])
	}
}

// finish ends the connection: an EOF is the daemon's ordinary close, anything
// else is kept as the connection's error for every later drain to report.
func (pc *pipeConn) finish(err error) {
	_ = pc.c.Close()
	pc.mu.Lock()
	defer pc.mu.Unlock()
	if !errors.Is(err, io.EOF) && pc.err == nil {
		pc.err = fmt.Errorf("harness: the test log pipe's connection from pid %d broke: %w", pc.pid, err)
	}
	if len(pc.pending) != 0 && pc.err == nil {
		pc.err = fmt.Errorf("harness: the test log pipe's connection from pid %d ended inside a frame", pc.pid)
	}
	pc.closed = true
	pc.cond.Broadcast()
}

// resolvedPath resolves the longest existing prefix of path through symlinks
// and keeps the rest as spelled: a logs directory a daemon has not created
// yet resolves to where it will be.
func resolvedPath(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		return filepath.Clean(path)
	}
	rest := ""
	for dir := abs; ; dir = filepath.Dir(dir) {
		if resolved, err := filepath.EvalSymlinks(dir); err == nil {
			return filepath.Join(resolved, rest)
		}
		if dir == filepath.Dir(dir) {
			return abs
		}
		rest = filepath.Join(filepath.Base(dir), rest)
	}
}

// pipedRead reads a log file over the test log pipe when it is routed, and
// answers false when it is a file on disk.
func pipedRead(t *testing.T, path string, offset int64) ([]byte, bool) {
	t.Helper()
	p := thePipe
	if p == nil {
		return nil, false
	}
	real, err := filepath.EvalSymlinks(path)
	if err != nil {
		return nil, false
	}
	dir := filepath.Dir(real)
	if !p.isRouted(dir) {
		return nil, false
	}
	if err := p.barrier(dir); err != nil {
		t.Fatalf("harness: drain the test log pipe for %s: %v", path, err)
	}
	return p.read(real, offset)
}

// pipedTerminal answers a daemon's piped terminal mirror, empty when the pipe
// is off.
func pipedTerminal(pid int, logsDir string) ([]byte, error) {
	p := thePipe
	if p == nil {
		return nil, nil
	}
	if err := p.barrier(resolvedPath(logsDir)); err != nil {
		return nil, err
	}
	return p.terminal(pid), nil
}

// materializePipedLogs writes a failed test's piped records into their files.
func materializePipedLogs(logsDir string) error {
	p := thePipe
	if p == nil {
		return nil
	}
	dir := resolvedPath(logsDir)
	if !p.isRouted(dir) {
		return nil
	}
	if err := p.barrier(dir); err != nil {
		return err
	}
	return p.materialize(dir)
}

// pipeEnv is the daemon's environment entry for the pipe.
func pipeEnv(p *logPipe) string { return dlog.EnvTestLogPipe + "=" + p.path }
