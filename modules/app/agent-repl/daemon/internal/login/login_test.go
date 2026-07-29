package login

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"reflect"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// fakeProc is a scripted terminal: tests make it "print" with say and read
// back what the session wrote into it.
type fakeProc struct {
	emit      chan []byte
	leftover  []byte
	closeOnce sync.Once

	mu        sync.Mutex
	written   []byte
	rows      uint16
	cols      uint16
	waitCalls int
}

type errorProc struct {
	readErr   error
	writeErr  error
	resizeErr error
	closeErr  error
	waitErr   error
}

func (p errorProc) Read([]byte) (int, error) {
	if p.readErr != nil {
		return 0, p.readErr
	}
	return 0, io.EOF
}
func (p errorProc) Write([]byte) (int, error)   { return 0, p.writeErr }
func (p errorProc) Resize(uint16, uint16) error { return p.resizeErr }
func (p errorProc) Close() error                { return p.closeErr }
func (p errorProc) Wait() error                 { return p.waitErr }

type capturedLog struct {
	durable  bytes.Buffer
	terminal bytes.Buffer
	logger   *dlog.Logger
}

func newCapturedLog() *capturedLog {
	logs := &capturedLog{}
	logs.logger = dlog.New(&logs.durable, &logs.terminal, false)
	return logs
}

func discardLogger() *dlog.Logger {
	return dlog.New(io.Discard, io.Discard, false)
}

func (l *capturedLog) records(t *testing.T) []dlog.Record {
	t.Helper()
	decoder := json.NewDecoder(bytes.NewReader(l.durable.Bytes()))
	var records []dlog.Record
	for {
		var record dlog.Record
		if err := decoder.Decode(&record); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			t.Fatalf("decode canonical log record: %v", err)
		}
		records = append(records, record)
	}
	return records
}

func (l *capturedLog) require(t *testing.T, operation, message string, level dlog.Level, verbosity dlog.Verbosity, context map[string]any) {
	t.Helper()
	var matches []dlog.Record
	for _, record := range l.records(t) {
		if record.Operation == operation && record.Message == message {
			matches = append(matches, record)
		}
	}
	if len(matches) != 1 {
		t.Fatalf("records = %#v, operation=%q message=%q count=%d, want exactly 1", l.records(t), operation, message, len(matches))
	}
	record := matches[0]
	if record.Runtime != dlog.RuntimeDaemon || record.Level != level || record.Verbosity != verbosity {
		t.Fatalf("record = %#v, want runtime=%q level=%q verbosity=%q", record, dlog.RuntimeDaemon, level, verbosity)
	}
	if record.WorkspaceDirectory != "" || record.WorkspaceID != "" {
		t.Fatalf("record = %#v, account-scoped login record must use global scope", record)
	}
	if !reflect.DeepEqual(record.Context, context) {
		t.Fatalf("record context = %#v, want %#v", record.Context, context)
	}
}

func newFakeProc() *fakeProc { return &fakeProc{emit: make(chan []byte, 64)} }

func (f *fakeProc) Read(p []byte) (int, error) {
	if len(f.leftover) == 0 {
		chunk, ok := <-f.emit
		if !ok {
			return 0, io.EOF
		}
		f.leftover = chunk
	}
	n := copy(p, f.leftover)
	f.leftover = f.leftover[n:]
	return n, nil
}

func (f *fakeProc) Write(p []byte) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.written = append(f.written, p...)
	return len(p), nil
}

func (f *fakeProc) Resize(rows, cols uint16) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rows, f.cols = rows, cols
	return nil
}

func (f *fakeProc) Wait() error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.waitCalls++
	return nil
}

func (f *fakeProc) Close() error {
	f.closeOnce.Do(func() { close(f.emit) })
	return nil
}

// say makes the terminal print s.
func (f *fakeProc) say(s string) { f.emit <- []byte(s) }

func (f *fakeProc) keystrokes() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return string(f.written)
}

func (f *fakeProc) size() (uint16, uint16) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.rows, f.cols
}

// recvWithin reads one chunk from a viewer, failing if none arrives.
func recvWithin(t *testing.T, c *Client, d time.Duration) string {
	t.Helper()
	select {
	case chunk, ok := <-c.Out:
		if !ok {
			t.Fatal("viewer channel closed while expecting terminal output")
		}
		return string(chunk)
	case <-time.After(d):
		t.Fatalf("no terminal output within %s", d)
		return ""
	}
}

// closedWithin waits for a viewer's channel to close, draining as it goes.
func closedWithin(t *testing.T, c *Client, d time.Duration) {
	t.Helper()
	deadline := time.After(d)
	for {
		select {
		case _, ok := <-c.Out:
			if !ok {
				return
			}
		case <-deadline:
			t.Fatalf("viewer channel still open after %s", d)
		}
	}
}

// managerWith returns a Manager whose spawns are recorded.
func managerWith(t *testing.T) (*Manager, *[]string, map[string]*fakeProc) {
	t.Helper()
	var mu sync.Mutex
	spawned := []string{}
	procs := map[string]*fakeProc{}
	m := NewManager(Config{
		Logger: discardLogger(),
		Start: func(account string) (Proc, error) {
			mu.Lock()
			defer mu.Unlock()
			spawned = append(spawned, account)
			p := newFakeProc()
			procs[account] = p
			return p, nil
		},
	})
	t.Cleanup(m.CloseAll)
	return m, &spawned, procs
}

func TestNewManager_RequiresCanonicalLogger(t *testing.T) {
	defer func() {
		if recovered := recover(); recovered != "login: Logger is required" {
			t.Fatalf("panic = %v, want required Logger invariant", recovered)
		}
	}()
	_ = NewManager(Config{})
}

func TestOpen_LogsStartFailureWithAccountAndOutcome(t *testing.T) {
	logs := newCapturedLog()
	want := errors.New("vendor executable unavailable")
	m := NewManager(Config{
		Logger: logs.logger,
		Start:  func(string) (Proc, error) { return nil, want },
	})

	if _, err := m.Open("/root/.claude-work"); !errors.Is(err, want) {
		t.Fatalf("Open error = %v, want %v", err, want)
	}

	logs.require(t, "login.open", "starting login terminal", dlog.LevelInfo, dlog.Verbose, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "starting",
	})
	logs.require(t, "login.open", "login terminal start failed", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "start-error",
		"error":   "vendor executable unavailable",
	})
}

func TestSessionOperationFailuresLogInputsAndOutcomes(t *testing.T) {
	writeFailure := errors.New("pty write failed")
	resizeFailure := errors.New("pty resize failed")
	closeFailure := errors.New("pty close failed")
	logs := newCapturedLog()
	sess := &Session{
		account: "/root/.claude-work",
		proc: errorProc{
			writeErr:  writeFailure,
			resizeErr: resizeFailure,
			closeErr:  closeFailure,
		},
		logger: logs.logger,
	}

	if err := sess.Write([]byte("secret input")); !errors.Is(err, writeFailure) {
		t.Fatalf("Write error = %v, want %v", err, writeFailure)
	}
	if err := sess.Resize(40, 180); !errors.Is(err, resizeFailure) {
		t.Fatalf("Resize error = %v, want %v", err, resizeFailure)
	}
	if err := sess.Close(); !errors.Is(err, closeFailure) {
		t.Fatalf("Close error = %v, want %v", err, closeFailure)
	}

	logs.require(t, "login.write", "login terminal write failed", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "proc-write",
		"bytes":   float64(12),
		"error":   "login: write to /root/.claude-work: pty write failed",
	})
	logs.require(t, "login.resize", "login terminal resize failed", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "proc-resize",
		"rows":    float64(40),
		"cols":    float64(180),
		"error":   "login: resize /root/.claude-work: pty resize failed",
	})
	logs.require(t, "login.close", "login terminal close failed", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "proc-close",
		"error":   "login: close /root/.claude-work: pty close failed",
	})
}

func TestSessionRejectedOperationsLogExitedOutcome(t *testing.T) {
	logs := newCapturedLog()
	sess := &Session{account: "/root/.claude-work", proc: errorProc{}, logger: logs.logger, exited: true}

	if err := sess.Write([]byte("x")); err == nil {
		t.Fatal("Write error = nil, want exited failure")
	}
	if err := sess.Resize(24, 80); err == nil {
		t.Fatal("Resize error = nil, want exited failure")
	}

	logs.require(t, "login.write", "login terminal write rejected", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "exited",
		"bytes":   float64(1),
		"error":   "login: /root/.claude-work has exited",
	})
	logs.require(t, "login.resize", "login terminal resize rejected", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "exited",
		"rows":    float64(24),
		"cols":    float64(80),
		"error":   "login: /root/.claude-work has exited",
	})
}

func TestFinish_LogsChildExitError(t *testing.T) {
	logs := newCapturedLog()
	sess := &Session{
		account: "/root/.claude-work",
		proc:    errorProc{waitErr: errors.New("child exited 1")},
		logger:  logs.logger,
		onExit:  func(string) {},
		clients: map[*Client]struct{}{},
	}

	sess.finish()

	logs.require(t, "login.child.exit", "login child exited with error", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "error",
		"error":   "child exited 1",
	})
}

func TestPump_LogsTerminalReadFailure(t *testing.T) {
	logs := newCapturedLog()
	sess := &Session{
		account: "/root/.claude-work",
		proc:    errorProc{readErr: errors.New("pty read failed")},
		logger:  logs.logger,
		onExit:  func(string) {},
		clients: map[*Client]struct{}{},
	}

	sess.pump()

	logs.require(t, "login.read", "login terminal read failed", dlog.LevelError, dlog.Normal, map[string]any{
		"account": "/root/.claude-work",
		"outcome": "read-error",
		"error":   "pty read failed",
	})
}

func TestBroadcast_LogsAndDetachesLaggingViewer(t *testing.T) {
	logs := newCapturedLog()
	viewer := &Client{Out: make(chan []byte)}
	sess := &Session{
		account: "/root/.claude-work",
		logger:  logs.logger,
		clients: map[*Client]struct{}{viewer: {}},
	}

	sess.broadcast([]byte("terminal output"))

	if _, attached := sess.clients[viewer]; attached {
		t.Fatal("lagging viewer remained attached")
	}
	logs.require(t, "login.viewer.drop", "dropping lagging login viewer", dlog.LevelWarn, dlog.Normal, map[string]any{
		"account":      "/root/.claude-work",
		"outcome":      "viewer-lagged",
		"viewer_count": float64(1),
	})
	logs.require(t, "login.viewer.detach", "login viewer detached", dlog.LevelInfo, dlog.Verbose, map[string]any{
		"account":      "/root/.claude-work",
		"viewer_count": float64(0),
	})
}

func TestOpen_ViewerSeesTheTerminal(t *testing.T) {
	// Arrange
	m, _, procs := managerWith(t)
	sess, err := m.Open("/root/.claude-chesscom")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	c := NewClient()
	sess.Attach(c)

	// Act
	procs["/root/.claude-chesscom"].say("Paste code here >")

	// Assert
	if got := recvWithin(t, c, 2*time.Second); got != "Paste code here >" {
		t.Errorf("viewer: got %q", got)
	}
}

func TestOpen_IsIdempotentPerAccount(t *testing.T) {
	// Arrange: a second click, or a second workspace on the same account,
	// must JOIN the open terminal. Racing a second OAuth flow against the
	// first is the bug this guards.
	m, spawned, _ := managerWith(t)

	// Act
	first, err := m.Open("/root/.claude")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	second, err := m.Open("/root/.claude")
	if err != nil {
		t.Fatalf("Open again: %v", err)
	}

	// Assert
	if first != second {
		t.Error("second Open on the same account returned a different session")
	}
	if len(*spawned) != 1 {
		t.Errorf("spawns: got %d (%v), want 1", len(*spawned), *spawned)
	}
}

func TestOpen_TwoAccountsLoginConcurrently(t *testing.T) {
	// Arrange: the whole point of the config-dir split. A ~/.claude login
	// and a ~/.claude-chesscom login must be able to be open at once.
	m, spawned, procs := managerWith(t)

	// Act
	personal, err := m.Open("/root/.claude")
	if err != nil {
		t.Fatalf("Open personal: %v", err)
	}
	work, err := m.Open("/root/.claude-chesscom")
	if err != nil {
		t.Fatalf("Open work: %v", err)
	}
	pc, wc := NewClient(), NewClient()
	personal.Attach(pc)
	work.Attach(wc)
	procs["/root/.claude"].say("personal")
	procs["/root/.claude-chesscom"].say("work")

	// Assert — two live sessions, each on its own terminal.
	if personal == work {
		t.Fatal("two accounts shared one login session")
	}
	if len(*spawned) != 2 {
		t.Errorf("spawns: got %v, want one per account", *spawned)
	}
	if got := recvWithin(t, pc, 2*time.Second); got != "personal" {
		t.Errorf("personal viewer: got %q", got)
	}
	if got := recvWithin(t, wc, 2*time.Second); got != "work" {
		t.Errorf("work viewer: got %q", got)
	}
}

func TestAttach_ReplaysTheTerminalToALateViewer(t *testing.T) {
	// Arrange: the child draws its screen the moment it starts, well before
	// the webapp has opened its socket. A viewer that missed that must still
	// see it.
	m, _, procs := managerWith(t)
	sess, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	procs[""].say("https://claude.com/cai/oauth/authorize?code=true")

	// Give the pump time to retain it before anyone is watching.
	deadline := time.Now().Add(2 * time.Second)
	for {
		sess.mu.Lock()
		retained := len(sess.scroll) > 0
		sess.mu.Unlock()
		if retained || time.Now().After(deadline) {
			break
		}
		time.Sleep(5 * time.Millisecond)
	}

	// Act
	c := NewClient()
	sess.Attach(c)

	// Assert
	if got := recvWithin(t, c, 2*time.Second); !strings.Contains(got, "oauth/authorize") {
		t.Errorf("replay: got %q, want the auth URL the viewer missed", got)
	}
}

func TestWrite_ForwardsKeystrokesToTheChild(t *testing.T) {
	// Arrange
	m, _, procs := managerWith(t)
	sess, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act
	if err := sess.Write([]byte("code-123\r")); err != nil {
		t.Fatalf("Write: %v", err)
	}

	// Assert
	if got := procs[""].keystrokes(); got != "code-123\r" {
		t.Errorf("child stdin: got %q, want %q", got, "code-123\r")
	}
}

func TestResize_ForwardsGeometryToTheChild(t *testing.T) {
	// Arrange
	m, _, procs := managerWith(t)
	sess, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act
	if err := sess.Resize(40, 180); err != nil {
		t.Fatalf("Resize: %v", err)
	}

	// Assert
	rows, cols := procs[""].size()
	if rows != 40 || cols != 180 {
		t.Errorf("child geometry: got %dx%d, want 40x180", rows, cols)
	}
}

func TestExit_ReleasesEveryViewer(t *testing.T) {
	// Arrange
	m, _, procs := managerWith(t)
	sess, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	c := NewClient()
	sess.Attach(c)

	// Act — the child goes away.
	_ = procs[""].Close()

	// Assert
	closedWithin(t, c, 2*time.Second)
	if !sess.Exited() {
		t.Error("Exited: got false after the child went away")
	}
}

func TestExit_LetsTheNextOpenStartAFreshLogin(t *testing.T) {
	// Arrange: a login that finished (or was killed) must not be handed
	// back to the next caller as if it were live.
	m, spawned, procs := managerWith(t)
	first, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	_ = procs[""].Close()
	deadline := time.Now().Add(2 * time.Second)
	for !first.Exited() && time.Now().Before(deadline) {
		time.Sleep(5 * time.Millisecond)
	}

	// Act
	second, err := m.Open("")
	if err != nil {
		t.Fatalf("Open after exit: %v", err)
	}

	// Assert
	if second == first {
		t.Error("Open handed back the exited login")
	}
	if len(*spawned) != 2 {
		t.Errorf("spawns: got %d, want a fresh one after the exit", len(*spawned))
	}
}

func TestWrite_AfterExitIsAnError(t *testing.T) {
	// Arrange: keystrokes aimed at a dead terminal must fail loudly rather
	// than vanish, or the webapp would show a typing user no reason nothing
	// is happening.
	m, _, procs := managerWith(t)
	sess, err := m.Open("")
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	_ = procs[""].Close()
	deadline := time.Now().Add(2 * time.Second)
	for !sess.Exited() && time.Now().Before(deadline) {
		time.Sleep(5 * time.Millisecond)
	}

	// Act
	err = sess.Write([]byte("x"))

	// Assert
	if err == nil {
		t.Error("Write to an exited login: got nil error, want a failure")
	}
}

func TestGet_ReportsNoLoginWhenNoneIsRunning(t *testing.T) {
	// Arrange
	m, _, _ := managerWith(t)

	// Act / Assert
	if sess := m.Get("/root/.claude"); sess != nil {
		t.Errorf("Get: got %v, want nil for an account with no login", sess)
	}
}

func TestChildEnv_SelectsTheAccountOnlyWhenThereIsOne(t *testing.T) {
	// Arrange: an empty account must leave the daemon's own value inherited.
	// Exporting an empty override names a config root literally called "".
	t.Setenv("CLAUDE_CONFIG_DIR", "/inherited")

	// Act
	withAccount := childEnv("/root/.claude-chesscom")
	withoutAccount := childEnv("")

	// Assert
	if !slices.Contains(withAccount, "CLAUDE_CONFIG_DIR=/root/.claude-chesscom") {
		t.Error("named account: CLAUDE_CONFIG_DIR override missing")
	}
	if slices.Contains(withoutAccount, "CLAUDE_CONFIG_DIR=") {
		t.Error("default account: exported an empty CLAUDE_CONFIG_DIR")
	}
	// TERM is what makes the TUI render at all.
	for _, env := range [][]string{withAccount, withoutAccount} {
		if !slices.Contains(env, "TERM=xterm-256color") {
			t.Error("TERM missing: the login TUI renders nothing legible without it")
		}
	}
}

func TestSpawn_RejectsAnEmptyCommand(t *testing.T) {
	// Arrange / Act
	_, err := Spawn(nil)("")

	// Assert
	if err == nil {
		t.Error("Spawn with no command: got nil error, want a failure")
	}
}

func TestSpawn_RunsARealTerminal(t *testing.T) {
	// Arrange: the one test that exercises the real pty path end to end.
	// `tput cols` reports the child's true terminal width, which proves both
	// that it HAS a terminal and that the wide default landed on it.
	proc, err := Spawn([]string{"sh", "-c", "tput cols"})("")
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	defer func() { _ = proc.Close() }()

	// Act
	out := make([]byte, 64)
	n, err := proc.Read(out)
	if err != nil && err != io.EOF {
		t.Fatalf("read: %v", err)
	}

	// Assert
	if got := strings.TrimSpace(string(out[:n])); got != "400" {
		t.Errorf("child columns: got %q, want %q (the OAuth URL must not wrap)", got, "400")
	}
}
