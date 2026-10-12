package dlog

import (
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"

	"agentrepl/logging"

	"agent-repld/internal/envc"
)

// pipeFrame is one decoded test log pipe frame.
type pipeFrame struct {
	kind byte
	key  string
	body string
}

// pipeCollector is the test's end of the pipe: it decodes every frame until
// the daemon side closes, and done closes once it has.
type pipeCollector struct {
	mu     sync.Mutex
	frames []pipeFrame
	err    error
	done   chan struct{}
}

func newPipeCollector(t *testing.T, conn net.Conn) *pipeCollector {
	t.Helper()
	c := &pipeCollector{done: make(chan struct{})}
	go func() {
		defer close(c.done)
		raw, err := io.ReadAll(conn)
		c.mu.Lock()
		defer c.mu.Unlock()
		c.err = err
		for len(raw) > 0 {
			kind, key, body, n, ok, perr := ParsePipeFrame(raw)
			if perr != nil || !ok {
				c.err = errors.Join(c.err, perr, errors.New("a partial frame at the stream's end"))
				return
			}
			c.frames = append(c.frames, pipeFrame{kind: kind, key: key, body: string(body)})
			raw = raw[n:]
		}
	}()
	return c
}

// all waits for the stream's end and answers every frame.
func (c *pipeCollector) all(t *testing.T) []pipeFrame {
	t.Helper()
	<-c.done
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.err != nil {
		t.Fatalf("collector: %v", c.err)
	}
	return c.frames
}

// pipedSurfaces opens real surfaces over temp paths with the pipe on.
func pipedSurfaces(t *testing.T) (*surfaces, string, *pipeCollector) {
	t.Helper()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	daemonEnd, testEnd := net.Pipe()
	collector := newPipeCollector(t, testEnd)
	pipe, err := newLogPipe(daemonEnd, filepath.Dir(runLogPath), 4242)
	if err != nil {
		t.Fatalf("newLogPipe: %v", err)
	}
	level, err := parseLevel(LevelDebug)
	if err != nil {
		t.Fatalf("parseLevel: %v", err)
	}
	s, err := openSurfacesPiped(runLogPath, logging.FixedWindow(level), io.Discard, pipe)
	if err != nil {
		t.Fatalf("openSurfacesPiped: %v", err)
	}
	bindTestWorkspaceIDs(s)
	return s, runLogPath, collector
}

// records answers the bodies of the record frames keyed by key.
func records(frames []pipeFrame, key string) []string {
	var out []string
	for _, f := range frames {
		if f.kind == PipeRecord && f.key == key {
			out = append(out, f.body)
		}
	}
	return out
}

// opened reports whether an open frame names key.
func opened(frames []pipeFrame, key string) bool {
	for _, f := range frames {
		if f.kind == PipeOpen && f.key == key {
			return true
		}
	}
	return false
}

func fileSize(t *testing.T, path string) int64 {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	return info.Size()
}

func TestTestLogPipeFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		pipe    string
		forbid  string
		want    string
		wantErr bool
	}{
		{name: "unset is off", pipe: "", forbid: "1", want: ""},
		{name: "a socket under the vendor guard is honored", pipe: "/tmp/x.sock", forbid: "1", want: "/tmp/x.sock"},
		{name: "a socket without the vendor guard is refused", pipe: "/tmp/x.sock", forbid: "", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tt.forbid)
			getenv := func(k string) string {
				if k == EnvTestLogPipe {
					return tt.pipe
				}
				return ""
			}

			// Act
			got, err := testLogPipeFromEnv(envc.Load(), getenv)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("testLogPipeFromEnv error = %v, want error %v", err, tt.wantErr)
			}
			if got != tt.want {
				t.Fatalf("testLogPipeFromEnv = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestPipedSurfacesSayHelloWithTheirLogsDirAndPid(t *testing.T) {
	// Arrange
	s, runLogPath, collector := pipedSurfaces(t)

	// Act
	s.Close()

	// Assert
	frames := collector.all(t)
	if len(frames) == 0 || frames[0].kind != PipeHello || frames[0].key != filepath.Dir(runLogPath) || frames[0].body != strconv.Itoa(4242) {
		t.Fatalf("first frame = %+v, want the hello naming %s and pid 4242", frames, filepath.Dir(runLogPath))
	}
}

func TestPipedRunLogRecordGoesOverThePipeNotIntoTheFile(t *testing.T) {
	// Arrange
	s, runLogPath, collector := pipedSurfaces(t)

	// Act
	s.Global().Info("daemon.test.piped", "a run-log record", nil)
	s.Close()

	// Assert
	frames := collector.all(t)
	if !opened(frames, runLogPath) {
		t.Fatalf("frames = %+v, want the run log opened before its records", frames)
	}
	got := records(frames, runLogPath)
	if len(got) != 1 || !strings.Contains(got[0], `"operation":"daemon.test.piped"`) || !strings.HasSuffix(got[0], "\n") {
		t.Fatalf("run-log records = %q, want the one record's whole JSONL line", got)
	}
	if size := fileSize(t, runLogPath); size != 0 {
		t.Fatalf("run log file holds %d bytes, want it empty under the pipe", size)
	}
}

func TestPipedWorkspaceRecordGoesOverThePipeKeyedByItsTarget(t *testing.T) {
	// Arrange
	s, _, collector := pipedSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	target, err := filepath.EvalSymlinks(filepath.Join(dir, ".claude", "emacs", "daemon.log"))
	if err != nil {
		t.Fatalf("resolve the daemon sink's link: %v", err)
	}

	// Act
	log.Info("daemon.test.piped", "a workspace record", nil)
	s.Close()

	// Assert
	frames := collector.all(t)
	key := ""
	for _, f := range frames {
		if f.kind == PipeOpen && filepath.Base(f.key) == filepath.Base(target) {
			key = f.key
		}
	}
	if key == "" {
		t.Fatalf("frames = %+v, want an open frame for the target %s", frames, target)
	}
	got := records(frames, key)
	if len(got) != 1 || !strings.Contains(got[0], `"operation":"daemon.test.piped"`) {
		t.Fatalf("workspace records = %q, want the one record", got)
	}
	if size := fileSize(t, target); size != 0 {
		t.Fatalf("target holds %d bytes, want it empty under the pipe", size)
	}
}

func TestPipedClientLogRecordGoesOverThePipe(t *testing.T) {
	// Arrange
	s, _, collector := pipedSurfaces(t)
	dir := t.TempDir()

	// Act
	err := s.ClientLog(dir, ClientRecord{ClientKind: RuntimeWebapp, Level: LevelInfo, Operation: "webapp.test.piped", Message: "forwarded"})
	s.Close()

	// Assert
	if err != nil {
		t.Fatalf("ClientLog: %v", err)
	}
	found := false
	for _, f := range collector.all(t) {
		if f.kind == PipeRecord && strings.Contains(f.key, "-webapp-") && strings.Contains(f.body, "webapp.test.piped") {
			found = true
		}
	}
	if !found {
		t.Fatal("no record frame carries the forwarded webapp record")
	}
}

func TestPipedShimSinkStaysOnItsFile(t *testing.T) {
	// Arrange
	s, _, collector := pipedSurfaces(t)
	dir := t.TempDir()

	// Act
	if _, err := s.ShimSink(dir); err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	s.Close()

	// Assert
	for _, f := range collector.all(t) {
		if strings.Contains(f.key, "-shim-") {
			t.Fatalf("frame %+v routes the shim sink, which the shim writes itself", f)
		}
	}
}

func TestPipedTerminalMirrorGoesOverThePipe(t *testing.T) {
	// Arrange
	s, _, collector := pipedSurfaces(t)

	// Act
	s.Global().Info("daemon.test.mirrored", "a mirrored record", nil)
	s.Close()

	// Assert
	var terminal strings.Builder
	for _, f := range collector.all(t) {
		if f.kind == PipeTerminal {
			terminal.WriteString(f.body)
		}
	}
	if !strings.Contains(terminal.String(), `"operation":"daemon.test.mirrored"`) {
		t.Fatalf("terminal frames = %q, want the mirrored record", terminal.String())
	}
}

func TestAPipedRunLogWhosePipeFailedReportsThePoison(t *testing.T) {
	// Arrange
	daemonEnd, testEnd := net.Pipe()
	collector := newPipeCollector(t, testEnd)
	pipe, err := newLogPipe(daemonEnd, t.TempDir(), 1)
	if err != nil {
		t.Fatalf("newLogPipe: %v", err)
	}
	rl, err := openRunLog(filepath.Join(t.TempDir(), "daemon.run.log"), RunLogBackups)
	if err != nil {
		t.Fatalf("openRunLog: %v", err)
	}
	t.Cleanup(func() { _ = rl.close() })
	rl.pipe = pipe
	_ = pipe.close()
	<-collector.done

	// Act
	err = rl.write([]byte("{}\n"))

	// Assert
	if !errors.Is(err, ErrPoisoned) {
		t.Fatalf("write over a closed pipe = %v, want ErrPoisoned", err)
	}
}

func TestParsePipeFrame(t *testing.T) {
	whole := AppendPipeFrame(nil, PipeRecord, "k", []byte("line\n"))
	tests := []struct {
		name   string
		in     []byte
		wantOK bool
	}{
		{name: "a whole frame", in: whole, wantOK: true},
		{name: "a frame cut inside its key", in: whole[:2], wantOK: false},
		{name: "a frame cut inside its body", in: whole[:len(whole)-1], wantOK: false},
		{name: "nothing", in: nil, wantOK: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			kind, key, body, n, ok, err := ParsePipeFrame(tt.in)

			// Assert
			if err != nil || ok != tt.wantOK {
				t.Fatalf("ParsePipeFrame = ok %v, err %v; want ok %v", ok, err, tt.wantOK)
			}
			if ok && (kind != PipeRecord || key != "k" || string(body) != "line\n" || n != len(whole)) {
				t.Fatalf("ParsePipeFrame = %q %q %q %d, want the frame back", kind, key, body, n)
			}
		})
	}
}
