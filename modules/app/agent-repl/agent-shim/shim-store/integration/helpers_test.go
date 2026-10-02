// Package integration is the shim-store's black-box suite: it starts the REAL
// store binary on a private unix socket and drives it ONLY through store.v1
// Connect clients, acting as the two fake producers the store really has (a
// shim-shaped stream-plane producer and a sidecar-shaped file-plane producer).
//
// Nothing here reaches into the store's internal packages: the store is a
// process, its contract is the Connect service, and its diagnostics are the
// JSONL log file it was told to write. Readiness and delivery are observed
// through real signals — a socket that accepts, a WriteBatch that returns its
// durable ack, a watch stream that delivers a frame — never through sleeps.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"bufio"
	"context"
	"crypto/rand"
	"crypto/tls"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	connect "connectrpc.com/connect"
	"golang.org/x/net/http2"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/structpb"
)

const (
	// readyTimeout bounds the wait for the store's socket to accept. It is a
	// deadline on a polling loop, never a sleep that stands in for a signal.
	// 2s is ~3x the observed healthy max for this package (0.62s, -race,
	// go test -v ./... baseline) — the store binary is built once in TestMain
	// and every boot in this suite lands in well under 100ms.
	readyTimeout = 2 * time.Second
	// shutdownTimeout bounds an orderly SIGTERM exit before the harness kills.
	// Same 2s basis as readyTimeout: this suite's real store process has never
	// taken close to that long to exit.
	shutdownTimeout = 2 * time.Second
	// callTimeout bounds any single rpc so a hung store fails the test loudly
	// instead of hanging the suite. Same 2s basis as readyTimeout.
	callTimeout = 2 * time.Second
	// streamTimeout bounds a watch stream's first delivery. Same 2s basis as
	// readyTimeout.
	streamTimeout = 2 * time.Second

	// burstCallTimeout and burstStreamTimeout are the EXPLICIT per-site bounds
	// for the one place in this package that moves thousands of items in a
	// single call: the default-buffer burst test. The package-wide 2s bounds
	// above are sized off single-item calls, so applying them to a
	// 4096-entry WriteBatch left this site with no stated basis at all.
	//
	// Measured on the burst test at -count=10 under representative contention
	// (16 cpu burners on 16 cores, matching the 8-parallel full-package run):
	// worst WriteBatch 0.40s, worst 4096-frame delivery 0.28s. Coverage
	// instrumentation raises the observed WriteBatch cost to 1.27s. These
	// bounds retain roughly three times the slowest observed cost while still
	// failing a stalled call promptly.
	burstCallTimeout   = 4 * time.Second
	burstStreamTimeout = 3 * time.Second

	// baseURL is a syntactic placeholder: every transport below dials the
	// unix socket, so the authority is never resolved.
	baseURL = "http://store.localhost"

	// streamProducerName and fileProducerName are the two producer strings the
	// store attributes writes to in its own logs.
	streamProducerName = "claude-shim:itest"
	fileProducerName   = "shim-claude-sidecar"
)

// storeBinary is the freshly built store, shared by every test in the binary.
var storeBinary string

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		fmt.Fprintf(os.Stderr, "integration: setting AGENT_REPL_FORBID_VENDOR_CALLS: %v\n", err)
		os.Exit(1)
	}
	if err := os.Unsetenv("AGENT_REPL_LOG_LEVEL"); err != nil {
		fmt.Fprintf(os.Stderr, "integration: unsetting AGENT_REPL_LOG_LEVEL: %v\n", err)
		os.Exit(1)
	}

	binDir, err := os.MkdirTemp("", "shim-store-itest-bin")
	if err != nil {
		fmt.Fprintf(os.Stderr, "integration: temp dir for the store binary: %v\n", err)
		os.Exit(1)
	}
	storeBinary = filepath.Join(binDir, "shim-store")

	build := exec.Command("go", "build", "-o", storeBinary, "./")
	build.Dir = ".."
	build.Stdout = os.Stderr
	build.Stderr = os.Stderr
	if err := build.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "integration: building the store binary: %v\n", err)
		if rmErr := os.RemoveAll(binDir); rmErr != nil {
			fmt.Fprintf(os.Stderr, "integration: removing the store binary dir: %v\n", rmErr)
		}
		os.Exit(1)
	}

	code := m.Run()
	if err := os.RemoveAll(binDir); err != nil {
		fmt.Fprintf(os.Stderr, "integration: removing the store binary dir: %v\n", err)
	}
	os.Exit(code)
}

// ---- the store process harness ----

// storeOptions are the knobs a test needs over one store process. Zero values
// mean "the store's own default": the flag is not passed at all.
type storeOptions struct {
	// socketPath overrides the generated short socket path.
	socketPath string
	// dbPath overrides the generated database path. A test that wants a
	// bootstrap failure points this somewhere unwritable.
	dbPath string
	// logPath overrides the generated log path.
	logPath string
	// watchBuffer, when > 0, is passed as --watch-buffer.
	watchBuffer int
	// pprofAddr, when non-empty, is passed as --pprof.
	pprofAddr string
	// noWait starts the process without waiting for the socket, for the
	// bootstrap-failure subjects.
	noWait bool
	// envSocketPath, when non-empty, is the value of AGENT_REPL_STORE_SOCKET in
	// the child's environment. Empty means "the socket the harness serves on",
	// which is what every ordinary subject wants.
	envSocketPath string
	// noSocketFlag starts the store WITHOUT --socket, so the environment
	// variable is the only thing that can name its socket.
	noSocketFlag bool
	// verbose runs the store with AGENT_REPL_LOG_LEVEL=debug, which is what makes
	// its per-statement traces durable — the only way a test can assert that a
	// refused request never reached storage.
	verbose bool
}

// storeProcess is one running (or crashed) store, with everything a test needs
// to talk to it, restart it, and read what it logged.
type storeProcess struct {
	t          *testing.T
	opts       storeOptions
	socket     string
	dbPath     string
	logPath    string
	stderrPath string
	// lockDir is AGENT_REPL_LOCK_DIR for the child: a per-test directory, so
	// the boot's build-report write (agentrepl/logging/buildreport) never
	// lands in the owner's real ~/.cache/agent-repl/run.
	lockDir string

	cmd  *exec.Cmd
	done chan struct{}
	// startedAt is when cmd started, for the readiness report.
	startedAt time.Time

	mu      sync.Mutex
	exitErr error
}

// startStore builds one store process on a private socket and, unless the test
// asked otherwise, waits until it accepts connections.
func startStore(t *testing.T, opts storeOptions) *storeProcess {
	t.Helper()

	work := t.TempDir()
	if opts.socketPath == "" {
		opts.socketPath = shortSocketPath(t)
	}
	if opts.dbPath == "" {
		opts.dbPath = filepath.Join(work, "events.db")
	}
	if opts.logPath == "" {
		opts.logPath = filepath.Join(work, "shim-store.log")
	}

	s := &storeProcess{
		t:          t,
		opts:       opts,
		socket:     opts.socketPath,
		dbPath:     opts.dbPath,
		logPath:    opts.logPath,
		stderrPath: filepath.Join(work, "shim-store.stderr"),
		lockDir:    filepath.Join(work, "run"),
	}
	s.launch()
	t.Cleanup(s.stop)
	if !opts.noWait {
		s.awaitReady()
	}
	return s
}

// launch starts one instance of the store against this harness's fixed paths.
// restart() calls it again, which is what makes durability observable.
func (s *storeProcess) launch() {
	s.t.Helper()

	args := []string{
		"--db", s.dbPath,
		"--log", s.logPath,
	}
	if !s.opts.noSocketFlag {
		args = append(args, "--socket", s.socket)
	}
	if s.opts.watchBuffer > 0 {
		args = append(args, "--watch-buffer", strconv.Itoa(s.opts.watchBuffer))
	}
	if s.opts.pprofAddr != "" {
		args = append(args, "--pprof", s.opts.pprofAddr)
	}

	stderr, err := os.OpenFile(s.stderrPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		s.t.Fatalf("opening the store's stderr capture %q: %v", s.stderrPath, err)
	}

	envSocket := s.opts.envSocketPath
	if envSocket == "" {
		envSocket = s.socket
	}
	cmd := exec.Command(storeBinary, args...)
	cmd.Env = storeEnv(envSocket, s.lockDir, s.opts.verbose)
	cmd.Stdout = stderr
	cmd.Stderr = stderr

	if err := cmd.Start(); err != nil {
		testclose.OrFail(s.t, stderr)
		s.t.Fatalf("starting the store: %v", err)
	}

	s.cmd = cmd
	s.startedAt = time.Now()
	s.done = make(chan struct{})
	s.mu.Lock()
	s.exitErr = nil
	s.mu.Unlock()

	go func(c *exec.Cmd, f *os.File, done chan struct{}) {
		err := c.Wait()
		// The test is still running here: its Cleanup (stop) waits on done,
		// which closes only after this, so failing it from this goroutine is
		// sound.
		testclose.OrFail(s.t, f)
		s.mu.Lock()
		s.exitErr = err
		s.mu.Unlock()
		close(done)
	}(cmd, stderr, s.done)
}

// storeEnv is the child's environment: the private socket as the documented
// default, the vendor-call guard on, info logging unless the subject asked
// for it, and a private AGENT_REPL_LOCK_DIR.
//
// THE LOCK DIR IS NOT OPTIONAL. It is also where the boot's build report
// (agentrepl/logging/buildreport) lands, and a real store spawned without it
// would overwrite the owner's own ~/.cache/agent-repl/run/shim-store.build.json.
func storeEnv(socket, lockDir string, verbose bool) []string {
	env := make([]string, 0, len(os.Environ())+4)
	for _, kv := range os.Environ() {
		if strings.HasPrefix(kv, "AGENT_REPL_LOG_LEVEL=") ||
			strings.HasPrefix(kv, "AGENT_REPL_STORE_SOCKET=") ||
			strings.HasPrefix(kv, "AGENT_REPL_FORBID_VENDOR_CALLS=") ||
			strings.HasPrefix(kv, "AGENT_REPL_LOCK_DIR=") {
			continue
		}
		env = append(env, kv)
	}
	env = append(env,
		"AGENT_REPL_STORE_SOCKET="+socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_LOCK_DIR="+lockDir,
		"AGENT_REPL_LOG_LEVEL=info",
	)
	if verbose {
		env[len(env)-1] = "AGENT_REPL_LOG_LEVEL=debug"
	}
	return env
}

// TestStoreEnvSetsAPrivateLockDir is the hermeticity regression: a real store
// spawned by this harness must never resolve its boot build-report
// (agentrepl/logging/buildreport) into the owner's real
// ~/.cache/agent-repl/run, which AGENT_REPL_LOCK_DIR redirects.
func TestStoreEnvSetsAPrivateLockDir(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOCK_DIR", "/should-never-survive")

	// Act.
	env := storeEnv("/tmp/store.sock", "/private/tmp/this-test-lock-dir", false)

	// Assert.
	var found int
	for _, kv := range env {
		if kv == "AGENT_REPL_LOCK_DIR=/private/tmp/this-test-lock-dir" {
			found++
		}
		if kv == "AGENT_REPL_LOCK_DIR=/should-never-survive" {
			t.Fatalf("env carried the ambient AGENT_REPL_LOCK_DIR instead of the harness's private one: %v", env)
		}
	}
	if found != 1 {
		t.Fatalf("env = %v, want exactly one AGENT_REPL_LOCK_DIR entry naming the harness's private dir", env)
	}
}

// awaitReady polls the socket under a deadline, failing at once if the process
// died instead of coming up.
func (s *storeProcess) awaitReady() {
	s.t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), readyTimeout)
	defer cancel()

	ticker := time.NewTicker(2 * time.Millisecond)
	defer ticker.Stop()

	firstAccept := time.Duration(-1)
	for {
		conn, err := net.Dial("unix", s.socket)
		if err == nil {
			if firstAccept < 0 {
				firstAccept = time.Since(s.startedAt)
			}
			if err := conn.Close(); err != nil {
				s.t.Fatalf("closing the readiness probe: %v", err)
			}
			// THE SOCKET ACCEPTS BEFORE STARTUP HAS FINISHED LOGGING: it is
			// bound, then `store.server.new` and `store.serve` are written. A
			// test that takes its log mark on the dial alone can count those
			// startup records as its own rpc's under load, so ready means this
			// process has also logged `store.serve`.
			if s.loggedServe() {
				return
			}
		}
		select {
		case <-s.done:
			s.t.Fatalf("the store exited before it was ready: %v\nstderr:\n%s", s.exit(), s.stderrText())
		case <-ctx.Done():
			// WHERE THE TIME WENT is the whole diagnosis of a missed boot: an
			// exec that never ran, a database open that stalled, or a serve
			// record that never landed are different faults.
			s.t.Fatalf("the store did not accept on %q within %s\n%s\nstderr:\n%s",
				s.socket, readyTimeout, describeStartup(s.startedAt, firstAccept, s.cmd.Process.Pid, s.logRecords()), s.stderrText())
		case <-ticker.C:
		}
	}
}

// loggedServe reports whether the running process has written its
// `store.serve` record, the last one startup writes.
func (s *storeProcess) loggedServe() bool {
	s.t.Helper()
	pid := s.cmd.Process.Pid
	for _, rec := range s.logRecords() {
		if rec.Operation == "store.serve" && rec.PID == pid {
			return true
		}
	}
	return false
}

// describeStartup says how far one store process got: when its socket first
// accepted (a negative firstAccept is never) and each record it logged,
// timed from its start.
func describeStartup(startedAt time.Time, firstAccept time.Duration, pid int, records []logRecord) string {
	var b strings.Builder
	if firstAccept < 0 {
		b.WriteString("startup: the socket never accepted")
	} else {
		fmt.Fprintf(&b, "startup: the socket first accepted %s after the process started", firstAccept.Round(time.Millisecond))
	}
	logged := 0
	for _, rec := range records {
		if rec.PID != pid {
			continue
		}
		logged++
		at, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if err != nil {
			fmt.Fprintf(&b, "\n  %s at an unreadable timestamp %q", rec.Operation, rec.Timestamp)
			continue
		}
		fmt.Fprintf(&b, "\n  %s at +%s", rec.Operation, at.Sub(startedAt).Round(time.Millisecond))
	}
	if logged == 0 {
		fmt.Fprintf(&b, "\n  pid %d logged nothing", pid)
	}
	return b.String()
}

// signal delivers one signal to the running store.
func (s *storeProcess) signal(sig syscall.Signal) {
	s.t.Helper()
	if err := s.cmd.Process.Signal(sig); err != nil {
		s.t.Fatalf("signalling the store with %v: %v", sig, err)
	}
}

// stop asks the store to exit and waits for it, killing only if it will not.
func (s *storeProcess) stop() {
	select {
	case <-s.done:
		return
	default:
	}
	_ = s.cmd.Process.Signal(syscall.SIGTERM)
	select {
	case <-s.done:
	case <-time.After(shutdownTimeout):
		_ = s.cmd.Process.Kill()
		<-s.done
	}
}

// awaitExit blocks until the store has exited and returns its exit error.
func (s *storeProcess) awaitExit() error {
	s.t.Helper()
	select {
	case <-s.done:
		return s.exit()
	case <-time.After(shutdownTimeout):
		s.t.Fatalf("the store did not exit within %s\nstderr:\n%s", shutdownTimeout, s.stderrText())
		return nil
	}
}

func (s *storeProcess) exit() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.exitErr
}

// restart stops the store and starts a new one on the SAME database and the
// SAME socket path. Every durability subject is expressed through this.
//
// IT DOES NOT REMOVE THE SOCKET. Doing so masked the reclaim path entirely:
// every restart handed the successor a clean path, so the store's own decision
// about a socket already sitting there — dial it, refuse a live one, reclaim a
// dead one — was never exercised by a single subject.
func (s *storeProcess) restart() {
	s.t.Helper()
	s.stop()
	s.launch()
	s.awaitReady()
}

// kill ends the store with SIGKILL and waits for it. There is no orderly
// shutdown and therefore no listener close, so the socket file is LEFT ON DISK
// exactly as a crashed store leaves it.
func (s *storeProcess) kill() {
	s.t.Helper()
	select {
	case <-s.done:
		return
	default:
	}
	if err := s.cmd.Process.Kill(); err != nil {
		s.t.Fatalf("killing the store: %v", err)
	}
	<-s.done
}

// restartAfterKill kills the store and starts a successor over the socket its
// predecessor abandoned. This is the ONLY path that reaches the reclaim branch,
// because an orderly exit unlinks the socket on its way out.
func (s *storeProcess) restartAfterKill() {
	s.t.Helper()
	s.kill()
	if !s.socketExists() {
		s.t.Fatalf("SIGKILL left no socket at %q; the reclaim path is not being exercised", s.socket)
	}
	s.launch()
	s.awaitReady()
}

func (s *storeProcess) stderrText() string {
	data, err := os.ReadFile(s.stderrPath)
	if err != nil {
		return fmt.Sprintf("<stderr unreadable: %v>", err)
	}
	return string(data)
}

// socketExists reports whether the store's socket file is still on disk, which
// is how an orderly exit's cleanup is observed.
func (s *storeProcess) socketExists() bool {
	_, err := os.Stat(s.socket)
	return err == nil
}

// shortSocketPath keeps the unix path inside the platform's ~104-byte limit;
// a path under t.TempDir() is routinely too long on macOS.
func shortSocketPath(t *testing.T) string {
	t.Helper()

	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		t.Fatalf("generating a socket name: %v", err)
	}
	name := "ar-" + hex.EncodeToString(raw[:]) + ".sock"

	path := filepath.Join(os.TempDir(), name)
	if len(path) > 100 {
		path = filepath.Join("/tmp", name)
	}
	if len(path) > 100 {
		t.Fatalf("no short enough socket path available (got %q, %d bytes)", path, len(path))
	}
	t.Cleanup(func() {
		if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
			t.Errorf("removing socket %q: %v", path, err)
		}
	})
	return path
}

// ---- transports and clients ----

func unixDialContext(socket string) func(context.Context, string, string) (net.Conn, error) {
	var d net.Dialer
	return func(ctx context.Context, _, _ string) (net.Conn, error) {
		return d.DialContext(ctx, "unix", socket)
	}
}

// h2cHTTPClient speaks prior-knowledge HTTP/2 over the unix socket.
func h2cHTTPClient(socket string) *http.Client {
	dial := unixDialContext(socket)
	return &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				return dial(ctx, network, addr)
			},
		},
	}
}

// http1HTTPClient speaks HTTP/1.1 over the unix socket.
func http1HTTPClient(socket string) *http.Client {
	return &http.Client{
		Transport: &http.Transport{
			DialContext:       unixDialContext(socket),
			ForceAttemptHTTP2: false,
		},
	}
}

// client is the suite's default store client: h2c, binary protobuf codec.
func (s *storeProcess) client() storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(h2cHTTPClient(s.socket), baseURL)
}

// http1Client is the same service over HTTP/1.1.
func (s *storeProcess) http1Client() storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(http1HTTPClient(s.socket), baseURL)
}

// jsonClient is the same service with the JSON codec instead of the binary one.
// IT IS NOT A SECOND ENDPOINT: the same handler answers a different content
// type, which is what makes the surface curl-able — and what makes every rpc's
// JSON encoding a real part of the contract rather than an accident.
func (s *storeProcess) jsonClient() storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(h2cHTTPClient(s.socket), baseURL, connect.WithProtoJSON())
}

// ---- the store's log file ----

// logRecord mirrors the store's JSONL record. The suite reads the log as the
// store's own account of what it did; the correlation keys live in Context.
type logRecord struct {
	Timestamp string         `json:"timestamp"`
	Runtime   string         `json:"runtime"`
	PID       int            `json:"pid"`
	Level     string         `json:"level"`
	Verbosity string         `json:"verbosity"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	RequestID string         `json:"request_id"`
	Context   map[string]any `json:"context"`
}

// logRecords parses the whole log file. A line that will not parse is a defect
// in the store's log sink and fails the test rather than being skipped.
func (s *storeProcess) logRecords() []logRecord {
	s.t.Helper()

	f, err := os.Open(s.logPath)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		s.t.Fatalf("opening the store log %q: %v", s.logPath, err)
	}
	defer testclose.OrFail(s.t, f)

	var records []logRecord
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 8*1024*1024)
	for line := 1; scanner.Scan(); line++ {
		text := strings.TrimSpace(scanner.Text())
		if text == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			s.t.Fatalf("store log %q line %d is not JSON: %v\nline: %s", s.logPath, line, err, text)
		}
		records = append(records, rec)
	}
	if err := scanner.Err(); err != nil {
		s.t.Fatalf("reading the store log %q: %v", s.logPath, err)
	}
	return records
}

// logRecordsAfter returns the records written after mark, so a test can scope
// its log assertions to one rpc.
func (s *storeProcess) logRecordsAfter(mark int) []logRecord {
	s.t.Helper()
	all := s.logRecords()
	if mark > len(all) {
		s.t.Fatalf("log mark %d is beyond the %d records written", mark, len(all))
	}
	return all[mark:]
}

// logMark is the current record count, taken before an rpc a test wants to
// read the log for.
func (s *storeProcess) logMark() int {
	s.t.Helper()
	return len(s.logRecords())
}

// assertNoErrorRecords is the green-path assertion: a path the store served
// without refusing anything writes no error record at all.
func (s *storeProcess) assertNoErrorRecords() {
	s.t.Helper()
	for _, rec := range s.logRecords() {
		if rec.Level == "error" {
			s.t.Errorf("green path logged an error record: operation=%q message=%q context=%v", rec.Operation, rec.Message, rec.Context)
		}
	}
}

// assertExactlyOneNormalRecord asserts that a refusal produced exactly ONE
// normal-verbosity record, and returns it.
//
// EVERY ERROR IS LOGGED EXACTLY ONCE BY ITS OWNING LAYER. Two layers each
// writing a normal-level record for one refusal is not redundancy — it is a
// count that lies to anyone who alerts on it, and a reader who cannot tell one
// refusal from two.
func assertExactlyOneNormalRecord(t *testing.T, records []logRecord, what string) logRecord {
	t.Helper()
	return assertExactlyOneNormalRecordAtLevel(t, records, what, "warn", "error")
}

// assertExactlyOneNormalRecordAtLevel is the same count for a refusal whose
// class is NOT recorded loudly.
//
// A LEVEL IS NOT A COUNT. The exactly-once rule is about how many layers wrote
// a record for one refusal, and it holds whatever severity the class carries —
// `unknown_agent` is an `info` because "no such book" is OpenAgentSession's
// ordinary answer, and it must still be written once and only once. Scoping the
// count to the levels the subject expects is what keeps the assertion from
// passing on a record of the wrong severity entirely.
func assertExactlyOneNormalRecordAtLevel(t *testing.T, records []logRecord, what string, levels ...string) logRecord {
	t.Helper()
	wanted := make(map[string]bool, len(levels))
	for _, level := range levels {
		wanted[level] = true
	}
	var normal []logRecord
	for _, rec := range records {
		if rec.Verbosity == "normal" && wanted[rec.Level] {
			normal = append(normal, rec)
		}
	}
	if len(normal) != 1 {
		t.Fatalf("%s produced %d normal-level records at %v, want exactly 1: %v", what, len(normal), levels, normal)
	}
	return normal[0]
}

// assertNoErrorRecordIn fails if any record in the window is an error.
func assertNoErrorRecordIn(t *testing.T, records []logRecord, what string) {
	t.Helper()
	for _, rec := range records {
		if rec.Level == "error" {
			t.Errorf("%s logged an error record: operation=%q message=%q context=%v", what, rec.Operation, rec.Message, rec.Context)
		}
	}
}

// recordsAtLevel filters records by level ("warn", "error").
func recordsAtLevel(records []logRecord, level string) []logRecord {
	var out []logRecord
	for _, rec := range records {
		if rec.Level == level {
			out = append(out, rec)
		}
	}
	return out
}

// recordsAtOperation filters records by their stable operation name.
//
// A LEVEL FILTER ALONE IS NOT AN ASSERTION. "Some warn was logged" passes for a
// reclaimed socket or a slow query as readily as for the thing under test, so
// every warning subject narrows to the operation it means.
func recordsAtOperation(records []logRecord, operation string) []logRecord {
	var out []logRecord
	for _, rec := range records {
		if rec.Operation == operation {
			out = append(out, rec)
		}
	}
	return out
}

// recordsWithContextKey filters records that carry one correlation key.
func recordsWithContextKey(records []logRecord, key string) []logRecord {
	var out []logRecord
	for _, rec := range records {
		if _, ok := rec.Context[key]; ok {
			out = append(out, rec)
		}
	}
	return out
}

// assertNoDatabaseTouch asserts that ONE refused request never reached storage.
//
// IT SCOPES BY request_id, WHICH IS WHAT MAKES IT MEAN ANYTHING. The old version
// looked for any record carrying a `statement` family at all — and the only
// record that carried one was the slow-query warning, which fires past a
// threshold, so its absence meant "nothing was slow", not "nothing ran". Every
// validation subject passed it without ever exercising the claim. The store now
// traces each statement family it runs, at verbose, with the request id the
// caller sent; a refusal that never opened a transaction leaves none carrying
// that id.
//
// The store must be started with `verbose: true` for this to be a real
// assertion, and the caller must have sent a request id — assertRefusedRequest
// below does both.
func assertNoDatabaseTouch(t *testing.T, records []logRecord, requestID string) {
	t.Helper()
	if requestID == "" {
		t.Fatal("assertNoDatabaseTouch needs the refused request's id; without it the assertion is vacuous")
	}
	// THERE IS NO PER-WINDOW POSITIVE CONTROL HERE, DELIBERATELY. A refused
	// request is often the only call in its window, so "some statement record
	// exists in this window" is not a property a refusal subject can have; the
	// control that keeps these assertions honest is the global one,
	// TestAnAcceptedRequestDoesLeaveAStatementRecordCarryingItsId in
	// validation_test.go, which fails the moment the store stops leaving the
	// mark this scan looks for.
	for _, rec := range records {
		if _, ok := rec.Context["statement"]; !ok {
			continue
		}
		if rec.RequestID == requestID {
			t.Errorf("a refused request reached the database: operation=%q statement=%v request_id=%q",
				rec.Operation, rec.Context["statement"], rec.RequestID)
		}
	}
}

// assertRefusalKeys asserts the two keys every refusal record carries: the SITE
// that said no and the wire ARM the caller received.
//
// THEY ARE NOT THE SAME FACT. Several sites map to one arm, so a record naming
// only the site leaves a reader unable to tell whether the caller could ever
// have retried, and a record naming only the arm leaves it unable to find the
// check that fired.
func assertRefusalKeys(t *testing.T, rec logRecord, wantSite, wantKind string) {
	t.Helper()
	if rec.Context["refusal_site"] != wantSite {
		t.Errorf("the refusal record's refusal_site is %v, want %q", rec.Context["refusal_site"], wantSite)
	}
	if rec.Context["refusal_kind"] != wantKind {
		t.Errorf("the refusal record's refusal_kind is %v, want %q", rec.Context["refusal_kind"], wantKind)
	}
}

// requestIDHeader is the header the store reads a caller's correlation id from.
const requestIDHeader = "X-Agent-Repl-Request-Id"

// newRequestID mints a correlation id unique to one subject, so a log scan can
// isolate exactly one call.
func newRequestID(t *testing.T) string {
	t.Helper()
	var raw [8]byte
	if _, err := rand.Read(raw[:]); err != nil {
		t.Fatalf("generating a request id: %v", err)
	}
	return "req-itest-" + hex.EncodeToString(raw[:])
}

// ---- fake producers ----

// producer is a fake store producer: the shim-shaped stream-plane writer or
// the sidecar-shaped file-plane writer. It builds well-formed store.v1
// envelopes around well-formed conversation.v1 facts and nothing else — there
// is no real shim and no real sidecar anywhere in this suite.
type producer struct {
	name string
	file bool
	cli  storev1connect.ShimStoreClient
	// requestID, when set, rides every write as the caller's correlation
	// header. A subject that must prove a refusal never reached storage needs
	// it: the proof is scoped by request id.
	requestID string
}

// correlated returns a copy of this producer that stamps every write with one
// correlation id.
func (p *producer) correlated(requestID string) *producer {
	copied := *p
	copied.requestID = requestID
	return &copied
}

// streamProducer writes as the shim does: plane stream, never a cursor.
func streamProducer(cli storev1connect.ShimStoreClient) *producer {
	return &producer{name: streamProducerName, file: false, cli: cli}
}

// fileProducer writes as the sidecar does: plane file, cursor riding the batch.
func fileProducer(cli storev1connect.ShimStoreClient) *producer {
	return &producer{name: fileProducerName, file: true, cli: cli}
}

// writeClass is the queue this producer's writes take, as the real producer
// states it: the shim writes interactive, the sidecar writes bulk.
func (p *producer) writeClass() *storev1.WriteClass {
	if p.file {
		return bulkClass()
	}
	return interactiveClass()
}

func interactiveClass() *storev1.WriteClass {
	return &storev1.WriteClass{WriteClass: &storev1.WriteClass_Interactive{Interactive: &storev1.WriteClassInteractive{}}}
}

func bulkClass() *storev1.WriteClass {
	return &storev1.WriteClass{WriteClass: &storev1.WriteClass_Bulk{Bulk: &storev1.WriteClassBulk{}}}
}

func (p *producer) plane() *storev1.Plane {
	if p.file {
		return &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	}
	return &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}}
}

// testConversionVersion is the conversion version a file-plane fixture is
// stamped with, as the sidecar stamps every entry it writes.
const testConversionVersion = 1

// conversionVersion is StoreEntry.conversion_version as this producer's plane
// states it: set on the file plane, unset on the stream plane.
func (p *producer) conversionVersion() *uint32 {
	if !p.file {
		return nil
	}
	v := uint32(testConversionVersion)
	return &v
}

// currentConversion is the conversion a cursor advance states when its file is
// read under the current version with nothing to re-derive.
func currentConversion() *storev1.CursorConversion {
	return &storev1.CursorConversion{
		Version: testConversionVersion,
		State:   &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}},
	}
}

// agentEntry wraps one StoreAgentUpdate in this producer's envelope.
func (p *producer) agentEntry(writeID, upsertKey string, update *storev1.StoreAgentUpdate) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:             p.plane(),
		WriteId:           writeID,
		UpsertKey:         upsertKey,
		ConversionVersion: p.conversionVersion(),
		Entry:             &storev1.StoreEntry_AgentUpdate{AgentUpdate: update},
	}
}

// sessionEntry wraps one raw SessionUpdate in this producer's envelope.
func (p *producer) sessionEntry(writeID, upsertKey string, update *conversationv1.SessionUpdate) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:             p.plane(),
		WriteId:           writeID,
		UpsertKey:         upsertKey,
		ConversionVersion: p.conversionVersion(),
		Entry:             &storev1.StoreEntry_SessionUpdate{SessionUpdate: update},
	}
}

// attempt sends one batch and returns whatever came back, refusals included.
func (p *producer) attempt(ctx context.Context, batch *storev1.EntryBatch) (*storev1.WriteBatchResponse, error) {
	req := connect.NewRequest(&storev1.WriteBatchRequest{
		Producer:   p.name,
		Batch:      batch,
		WriteClass: p.writeClass(),
	})
	if p.requestID != "" {
		req.Header().Set(requestIDHeader, p.requestID)
	}
	resp, err := p.cli.WriteBatch(ctx, req)
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

// write sends one batch and asserts the durable-ack success arm.
func (p *producer) write(ctx context.Context, t *testing.T, entries ...*storev1.StoreEntry) {
	t.Helper()
	p.writeWithCursor(ctx, t, nil, entries...)
}

// writeWithCursor sends one batch with a cursor advance riding it and asserts
// the durable-ack success arm.
func (p *producer) writeWithCursor(ctx context.Context, t *testing.T, cursor *storev1.CursorState, entries ...*storev1.StoreEntry) {
	t.Helper()
	batch := &storev1.EntryBatch{Entries: entries, CursorAdvance: cursor}
	resp, err := p.attempt(ctx, batch)
	if err != nil {
		t.Fatalf("WriteBatch transport error: %v", err)
	}
	if failure := resp.GetFailure(); failure != nil {
		t.Fatalf("WriteBatch refused a well-formed batch: %s", failure.GetDetail())
	}
	if resp.GetSuccess() == nil {
		t.Fatalf("WriteBatch answered neither success nor failure: %v", resp)
	}
}

// writeExpectingFailure sends one batch and asserts the typed failure arm,
// returning the whole failure so the caller can assert its KIND and its FIELD —
// not only its human detail, which nothing may switch on.
func (p *producer) writeExpectingFailure(ctx context.Context, t *testing.T, cursor *storev1.CursorState, entries ...*storev1.StoreEntry) *storev1.WriteBatchFailure {
	t.Helper()
	resp, err := p.attempt(ctx, &storev1.EntryBatch{Entries: entries, CursorAdvance: cursor})
	if err != nil {
		t.Fatalf("WriteBatch answered a transport error where a typed failure was owed: %v", err)
	}
	failure := resp.GetFailure()
	if failure == nil {
		t.Fatalf("WriteBatch accepted a batch it owed a typed failure for: %v", resp)
	}
	return failure
}

// writeExpectingSkips sends one batch, asserts the DURABLE success arm, and
// returns the legacy book-conflict entries the store skipped. It is the
// re-ingest idempotency path: an entry whose upsert_key already names a row
// under a different book is kept-and-skipped rather than refused, so the batch
// succeeds and names the skips on its success arm.
func (p *producer) writeExpectingSkips(ctx context.Context, t *testing.T, cursor *storev1.CursorState, entries ...*storev1.StoreEntry) []*storev1.WriteBatchSkippedEntry {
	t.Helper()
	resp, err := p.attempt(ctx, &storev1.EntryBatch{Entries: entries, CursorAdvance: cursor})
	if err != nil {
		t.Fatalf("WriteBatch transport error: %v", err)
	}
	if failure := resp.GetFailure(); failure != nil {
		t.Fatalf("WriteBatch refused where a skip was owed: %s", failure.GetDetail())
	}
	success := resp.GetSuccess()
	if success == nil {
		t.Fatalf("WriteBatch answered neither success nor failure: %v", resp)
	}
	return success.GetSkipped()
}

// ---- failure-arm assertions ----
//
// A FAILURE WITH AN UNSET KIND IS ITSELF A DEFECT. `detail` is prose for a
// human and is never switched on, so a caller that received no arm would have
// to parse it to learn whether retrying the same bytes could ever help. Every
// refusal subject therefore asserts the arm, and every invalid_request asserts
// the FIELD the store blames.

func assertDetail(t *testing.T, what, detail string) {
	t.Helper()
	if detail == "" {
		t.Errorf("%s carried no detail", what)
	}
}

func assertWriteInvalidRequest(t *testing.T, failure *storev1.WriteBatchFailure, wantField string) {
	t.Helper()
	assertDetail(t, "the WriteBatch refusal", failure.GetDetail())
	invalid := failure.GetInvalidRequest()
	if invalid == nil {
		t.Fatalf("WriteBatch failure kind = %v, want invalid_request (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
	if invalid.GetField() != wantField {
		t.Errorf("WriteBatch invalid_request.field = %q, want %q (detail: %s)", invalid.GetField(), wantField, failure.GetDetail())
	}
}

func assertOpenInvalidRequest(t *testing.T, failure *storev1.OpenAgentSessionFailure, wantField string) {
	t.Helper()
	assertDetail(t, "the OpenAgentSession refusal", failure.GetDetail())
	invalid := failure.GetInvalidRequest()
	if invalid == nil {
		t.Fatalf("OpenAgentSession failure kind = %v, want invalid_request (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
	if invalid.GetField() != wantField {
		t.Errorf("OpenAgentSession invalid_request.field = %q, want %q (detail: %s)", invalid.GetField(), wantField, failure.GetDetail())
	}
}

func assertOpenStalePointer(t *testing.T, failure *storev1.OpenAgentSessionFailure) {
	t.Helper()
	assertDetail(t, "the OpenAgentSession refusal", failure.GetDetail())
	if failure.GetStalePointer() == nil {
		t.Fatalf("OpenAgentSession failure kind = %v, want stale_pointer (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
}

func assertOpenUnknownAgent(t *testing.T, failure *storev1.OpenAgentSessionFailure) {
	t.Helper()
	assertDetail(t, "the OpenAgentSession refusal", failure.GetDetail())
	if failure.GetUnknownAgent() == nil {
		t.Fatalf("OpenAgentSession failure kind = %v, want unknown_agent (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
}

// openUnknownAgent is the whole refusal in one call, for the subjects that only
// need to say "this id names no book of this store".
func openUnknownAgent(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, agent string) {
	t.Helper()
	assertOpenUnknownAgent(t, openSessionExpectingFailure(ctx, t, cli, &storev1.OpenAgentSessionRequest{
		Agent:    agentID(agent),
		PageSize: 10,
	}))
}

func assertReadInvalidRequest(t *testing.T, failure *storev1.ReadAgentPageFailure, wantField string) {
	t.Helper()
	assertDetail(t, "the ReadAgentPage refusal", failure.GetDetail())
	invalid := failure.GetInvalidRequest()
	if invalid == nil {
		t.Fatalf("ReadAgentPage failure kind = %v, want invalid_request (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
	if invalid.GetField() != wantField {
		t.Errorf("ReadAgentPage invalid_request.field = %q, want %q (detail: %s)", invalid.GetField(), wantField, failure.GetDetail())
	}
}

func assertReadStalePointer(t *testing.T, failure *storev1.ReadAgentPageFailure) {
	t.Helper()
	assertDetail(t, "the ReadAgentPage refusal", failure.GetDetail())
	if failure.GetStalePointer() == nil {
		t.Fatalf("ReadAgentPage failure kind = %v, want stale_pointer (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
}

func assertCursorsInvalidRequest(t *testing.T, failure *storev1.GetSidecarCursorsFailure, wantField string) {
	t.Helper()
	assertDetail(t, "the GetSidecarCursors refusal", failure.GetDetail())
	invalid := failure.GetInvalidRequest()
	if invalid == nil {
		t.Fatalf("GetSidecarCursors failure kind = %v, want invalid_request (detail: %s)", failure.GetKind(), failure.GetDetail())
	}
	if invalid.GetField() != wantField {
		t.Errorf("GetSidecarCursors invalid_request.field = %q, want %q (detail: %s)", invalid.GetField(), wantField, failure.GetDetail())
	}
}

// ---- conversation.v1 fact builders ----
//
// Every builder fills every non-optional field of every message it touches:
// the store's validation invariant refuses a zero-value placeholder, so a
// helper that left one unset would test the refusal path by accident.

func agentID(value string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: value}
}

func activityID(value string) *conversationv1.AgentActivityId {
	return &conversationv1.AgentActivityId{Value: value}
}

func turnID(value string) *conversationv1.TurnId {
	return &conversationv1.TurnId{Value: value}
}

func detachedWorkID(value string) *conversationv1.DetachedWorkId {
	return &conversationv1.DetachedWorkId{Value: value}
}

func startedAt(atMS int64) *conversationv1.AgentActivityStartedAt {
	return &conversationv1.AgentActivityStartedAt{AtMs: atMS}
}

func userSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: text},
				},
			}},
		},
	}
}

// promptFact is one delivered prompt: the one form of a page line that is not
// an agent's own frame.
func promptFact(turn, agent, text string) *conversationv1.AgentPrompt {
	return &conversationv1.AgentPrompt{
		Id:     turnID(turn),
		Agent:  agentID(agent),
		Said:   userSaid(text),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}
}

// responseFrame is the suite's ordinary `update` frame: one settled response
// whose markdown IS the line's label, so page assertions read as text.
func responseFrame(agent, actID, markdown string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_Activity{
					Activity: &conversationv1.AgentActivity{
						ActivityId: activityID(actID),
						Item: &conversationv1.AgentActivity_Response{
							Response: &conversationv1.AgentResponse{
								Result: &conversationv1.AgentResponse_Success{
									Success: &conversationv1.AgentResponseSuccess{
										Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
										Authorship: &conversationv1.AgentResponseSuccess_FromModel{
											FromModel: &conversationv1.AgentResponseFromModel{},
										},
									},
								},
							},
						},
					},
				},
			},
		},
	}
}

// successFrame is an agent's terminal on asked-for terms.
func successFrame(agent, answerActID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Success{
			Success: &conversationv1.AgentSuccess{
				Outcome: &conversationv1.AgentSuccess_Completed{
					Completed: &conversationv1.AgentCompleted{Answer: activityID(answerActID)},
				},
			},
		},
	}
}

// failureFrame is an agent's terminal because something broke.
func failureFrame(agent string, errs ...string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Failure{
			Failure: &conversationv1.AgentFailure{
				Errors: errs,
				Failure: &conversationv1.AgentFailure_ExecutionError{
					ExecutionError: &conversationv1.AgentExecutionError{},
				},
			},
		},
	}
}

// contextCutFrame is the `update` arm that says the conversation was cut. A
// page line of the main agent's book like any other update, and instantaneous:
// one frame, no lifecycle.
func contextCutFrame(agent string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_ContextCut{
					ContextCut: &conversationv1.ContextCut{
						Cut: &conversationv1.ContextCut_Cleared{
							Cleared: &conversationv1.ContextCleared{},
						},
					},
				},
			},
		},
	}
}

// apiErrorFrame is the `update` arm carrying a vendor request that failed
// MID-TURN and was recovered from: evidence, never a terminal. A page line.
func apiErrorFrame(agent, message string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_ApiError{
					ApiError: &conversationv1.ApiRequestFailed{
						Message: message,
						Kind: &conversationv1.ApiRequestFailed_Overloaded{
							Overloaded: &conversationv1.ApiOverloaded{},
						},
					},
				},
			},
		},
	}
}

// subagentPrompt is a fully-specified spawn description; the isolation oneof
// is set because an unset oneof is illegal.
func subagentPrompt(text, subagentType string) *conversationv1.AgentSubagentPrompt {
	return &conversationv1.AgentSubagentPrompt{
		Text:         text,
		SubagentType: &subagentType,
		Isolation: &conversationv1.AgentSubagentPrompt_None{
			None: &conversationv1.AgentSubagentIsolationNone{},
		},
	}
}

// subagentSpawnFrame is an `update` whose activity CREATES another agent: one
// page line in the spawner's book, and the created agent's own row.
func subagentSpawnFrame(spawner, actID, createdAgent, promptText string, at int64) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(spawner),
		Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_Activity{
					Activity: &conversationv1.AgentActivity{
						ActivityId: activityID(actID),
						Item: &conversationv1.AgentActivity_Subagent{
							Subagent: &conversationv1.AgentSubagent{
								Result: &conversationv1.AgentSubagent_Start{
									Start: &conversationv1.AgentSubagentStart{
										CreatedAgentId: agentID(createdAgent),
										Prompt:         subagentPrompt(promptText, "opus-medium"),
										StartedAt:      startedAt(at),
									},
								},
							},
						},
					},
				},
			},
		},
	}
}

// detachedBashFrame announces a detached shell run created on this stream.
func detachedBashFrame(owner, workID, commandLine string, at int64) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(owner),
		Result: &conversationv1.AgentFrame_DetachedWork{
			DetachedWork: &conversationv1.AgentDetachedWork{
				Work: detachedWorkID(workID),
				Origin: &conversationv1.AgentDetachedWork_Created{
					Created: &conversationv1.DetachedWorkCreated{
						WorkCreated: &conversationv1.DetachableWork{
							Work: &conversationv1.DetachableWork_Bash{Bash: bashStart(commandLine, at)},
						},
					},
				},
			},
		},
	}
}

func bashCommand(line string) *conversationv1.AgentBashCommand {
	return &conversationv1.AgentBashCommand{
		Line: line,
		Sandbox: &conversationv1.AgentBashCommand_Sandboxed{
			Sandboxed: &conversationv1.AgentBashSandboxed{},
		},
	}
}

// bashStart is a detached run's opening frame.
func bashStart(line string, at int64) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{
				Command:   bashCommand(line),
				StartedAt: startedAt(at),
			},
		},
	}
}

// bashTail is a detached run's rendered tail — what the sidecar writes as it
// copies the spool, one row the run's every write supersedes (owner ruling
// 2026-09-23: output beyond what is rendered is not stored).
func bashTail(text string) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Tail{Tail: &conversationv1.AgentBashTail{Text: text}},
	}
}

// bashSuccess is a detached run's terminal frame.
func bashSuccess(line string, exitCode int32) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{
			Success: &conversationv1.AgentBashSuccess{
				Command: bashCommand(line),
				Outcome: &conversationv1.AgentBashSuccess_Completed{
					Completed: &conversationv1.AgentBashCompleted{
						Output: &conversationv1.AgentBashOutput{
							Form: &conversationv1.AgentBashOutput_Text{
								Text: &conversationv1.AgentBashOutputText{
									Stdout: "ok\n",
									Stderr: "",
									Extent: &conversationv1.AgentBashOutputText_Whole{
										Whole: &conversationv1.AgentBashOutputWhole{},
									},
								},
							},
						},
						Termination: &conversationv1.AgentBashTermination{
							How: &conversationv1.AgentBashTermination_Exited{
								Exited: &conversationv1.AgentBashExited{Code: exitCode},
							},
						},
					},
				},
			},
		},
	}
}

// workflowStartFrame is a workflow run's opening frame — accepted durably this
// wave, served by nothing.
func workflowStartFrame(name string, at int64) *conversationv1.AgentWorkflow {
	return &conversationv1.AgentWorkflow{
		Result: &conversationv1.AgentWorkflow_Start{
			Start: &conversationv1.AgentWorkflowStart{
				Name:   name,
				Script: &conversationv1.AgentWorkflowScript{},
				Placement: &conversationv1.AgentWorkflowStart_Local{
					Local: &conversationv1.AgentWorkflowPlacementLocal{},
				},
				StartedAt: startedAt(at),
			},
		},
	}
}

// identityRotated is the session fact that must never split an agent's book.
func identityRotated(previous, next string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_IdentityRotated{
			IdentityRotated: &conversationv1.SessionIdentityRotated{
				PreviousVendorSessionId: previous,
				VendorSessionId:         next,
			},
		},
	}
}

// ---- store.v1 envelope builders ----

func frameItem(f *conversationv1.AgentFrame) *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentFrame{AgentFrame: f}}
}

func promptItem(p *conversationv1.AgentPrompt) *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: p}}
}

// frameLine makes an agent's frame a page line of that agent's own book.
func frameLine(topLevel *conversationv1.AgentId, f *conversationv1.AgentFrame) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		TopLevel: topLevel,
		AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
			ServeableFrame: &storev1.StorePageLine{
				Book:      &storev1.StorePageLine_PageAgentId{PageAgentId: f.GetAgentId()},
				AgentItem: frameItem(f),
			},
		},
	}
}

// promptLine makes a delivered prompt a page line of its addressee's book.
func promptLine(topLevel *conversationv1.AgentId, p *conversationv1.AgentPrompt) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		TopLevel: topLevel,
		AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
			ServeableFrame: &storev1.StorePageLine{
				Book:      &storev1.StorePageLine_PageAgentId{PageAgentId: p.GetAgent()},
				AgentItem: promptItem(p),
			},
		},
	}
}

// keepaliveLine is the RETIRED keep-alive arm, which the store refuses. The
// tag is reserved, so it rides as the unknown field a stale producer's bytes
// decode to.
func keepaliveLine(topLevel *conversationv1.AgentId, p *conversationv1.AgentPrompt) *storev1.StoreAgentUpdate {
	body, err := proto.Marshal(promptItem(p))
	if err != nil {
		panic(fmt.Sprintf("marshaling a keep-alive's held item: %v", err))
	}
	item := &storev1.StoreUnservedItem{}
	item.ProtoReflect().SetUnknown(protowire.AppendBytes(protowire.AppendTag(nil, 1, protowire.BytesType), body))
	return &storev1.StoreAgentUpdate{
		TopLevel:  topLevel,
		AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: item},
	}
}

// rawResidue is the VERBATIM record every residue arm exists to carry. Residue
// with no raw record is the drop it was meant to prevent, dressed up as
// durability — the store refuses it, and a fixture that omitted it was testing
// that refusal by accident.
func rawResidue(kind string) *structpb.Struct {
	raw, err := structpb.NewStruct(map[string]any{"type": kind, "verbatim": true})
	if err != nil {
		panic("shim-store integration: building a raw residue record: " + err.Error())
	}
	return raw
}

// vendorSpecificLine is understood residue: carried, never served.
func vendorSpecificLine(kind string) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
					VendorSpecific: &storev1.StoreVendorSpecific{Kind: kind, Raw: rawResidue(kind)},
				},
			},
		},
	}
}

// unknownLine is parsed-but-unmodeled residue.
func unknownLine(discriminator, field string) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_Unknown{
					Unknown: &storev1.StoreUnknown{
						Discriminator:      discriminator,
						DiscriminatorField: field,
						Raw:                rawResidue(discriminator),
					},
				},
			},
		},
	}
}

// unparsedLine is residue that could not be read at all.
func unparsedLine(source string, offset uint64, parseError, raw string) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_Unparsed{
					Unparsed: &storev1.StoreUnparsed{
						Source:     source,
						Offset:     offset,
						ParseError: parseError,
						Raw:        raw,
					},
				},
			},
		},
	}
}

// bashRun wraps a detached shell run's frame with the unit id that announced
// it. Structurally never a page line.
func bashRun(topLevel *conversationv1.AgentId, runActID string, frame *conversationv1.AgentBash) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		TopLevel: topLevel,
		AgentInfo: &storev1.StoreAgentUpdate_Bash{
			Bash: &storev1.StoreAgentBash{
				Run:   activityID(runActID),
				Frame: frame,
			},
		},
	}
}

// bashActivityFrame is a shell run's activity IN ITS SPAWNING AGENT'S BOOK — an
// ordinary page line whose activity_id is the run's unit id. When its item
// reaches a terminal arm, that is what closes the detached row joined on
// origin_unit.
func bashActivityFrame(agent, runActID string, frame *conversationv1.AgentBash) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(agent),
		Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_Activity{
					Activity: &conversationv1.AgentActivity{
						ActivityId: activityID(runActID),
						Item:       &conversationv1.AgentActivity_Bash{Bash: frame},
					},
				},
			},
		},
	}
}

// workflowRun wraps a workflow run's frame with the run's agent identity.
func workflowRun(topLevel *conversationv1.AgentId, runAgent string, frame *conversationv1.AgentWorkflow) *storev1.StoreAgentUpdate {
	return &storev1.StoreAgentUpdate{
		TopLevel: topLevel,
		AgentInfo: &storev1.StoreAgentUpdate_Workflow{
			Workflow: &storev1.StoreAgentWorkflow{
				Run:   agentID(runAgent),
				Frame: frame,
			},
		},
	}
}

func cursorState(fileID, path string, offset int64, carry []byte) *storev1.CursorState {
	return &storev1.CursorState{
		FileId:     fileID,
		Path:       path,
		Offset:     offset,
		Carry:      carry,
		Conversion: currentConversion(),
	}
}

// ---- read-side call helpers ----

// callContext bounds one rpc.
// seedBook registers an agent with the store by writing ONE ordinary line of
// its book, so a later open addresses a book that exists.
//
// A SUBJECT ABOUT THE TAIL NEEDS THIS AND IS NOT CHANGED BY IT: the seed line
// is written before the open, so it lands in the opening PAGE and never on the
// stream that follows.
func seedBook(ctx context.Context, t *testing.T, p *producer, agent, tag string) {
	t.Helper()
	p.write(ctx, t, p.agentEntry("w-seed-"+tag, "u-seed-"+tag,
		frameLine(agentID(agent), responseFrame(agent, "act-seed-"+tag, "seed:"+tag))))
}

// registerEmptyBook makes the store aware of an agent WITHOUT putting a line in
// its book, exactly the way a real spawn does: the spawn frame is a page line of
// the SPAWNER's book, and the agent row it creates is the spawned agent's own.
// That is what makes a freshly spawned subagent openable before it has spoken.
func registerEmptyBook(ctx context.Context, t *testing.T, p *producer, spawner, created, tag string) {
	t.Helper()
	p.write(ctx, t, p.agentEntry("w-spawn-"+tag, "u-spawn-"+tag,
		frameLine(agentID(spawner), subagentSpawnFrame(spawner, "act-spawn-"+tag, created, "go", 1000))))
}

func callContext(t *testing.T) (context.Context, context.CancelFunc) {
	t.Helper()
	return callContextWithin(t, callTimeout)
}

// callContextWithin is callContext with an explicit bound, for the sites whose
// call is not a single-item rpc and cannot honestly be held to callTimeout.
func callContextWithin(t *testing.T, within time.Duration) (context.Context, context.CancelFunc) {
	t.Helper()
	return context.WithTimeout(context.Background(), within)
}

func openSession(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, agent string, pageSize uint32, knownThrough *storev1.StoreItemPointer) *storev1.OpenAgentSessionSuccess {
	t.Helper()
	resp, err := cli.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent:        agentID(agent),
		PageSize:     pageSize,
		KnownThrough: knownThrough,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%q) transport error: %v", agent, err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		t.Fatalf("OpenAgentSession(%q) refused: %s", agent, failure.GetDetail())
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenAgentSession(%q) answered neither arm: %v", agent, resp.Msg)
	}
	return success
}

// openPageOnly is the one-shot read: the caller states at the open that no
// watch follows, so the store mints nothing and the success carries no token.
func openPageOnly(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, agent string, pageSize uint32) *storev1.OpenAgentSessionSuccess {
	t.Helper()
	resp, err := cli.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent:    agentID(agent),
		PageSize: pageSize,
		PageOnly: true,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%q, page_only) transport error: %v", agent, err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		t.Fatalf("OpenAgentSession(%q, page_only) refused: %s", agent, failure.GetDetail())
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenAgentSession(%q, page_only) answered neither arm: %v", agent, resp.Msg)
	}
	if success.GetWatch() != nil {
		t.Fatalf("OpenAgentSession(%q, page_only) answered a watch token %v; a page-only open mints none", agent, success.GetWatch())
	}
	return success
}

// assertOutstandingTokensAtShutdown stops the store and reads the outstanding
// token count off its shutdown record — the only place the registry's size is
// stated, and the reason the count is stated there at all.
func assertOutstandingTokensAtShutdown(t *testing.T, store *storeProcess, want int) {
	t.Helper()
	store.stop()
	marker := fmt.Sprintf("outstanding_tokens=%d", want)
	for _, rec := range store.logRecords() {
		if rec.Operation != "store.shutdown" || !strings.Contains(rec.Message, "ending standing watches") {
			continue
		}
		if !strings.Contains(rec.Message, marker) {
			t.Fatalf("the shutdown record says %q, want it to carry %q", rec.Message, marker)
		}
		return
	}
	t.Fatalf("the store wrote no store.shutdown record stating its outstanding tokens")
}

func openSessionExpectingFailure(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, req *storev1.OpenAgentSessionRequest, requestID ...string) *storev1.OpenAgentSessionFailure {
	t.Helper()
	call := connect.NewRequest(req)
	if len(requestID) == 1 && requestID[0] != "" {
		call.Header().Set(requestIDHeader, requestID[0])
	}
	resp, err := cli.OpenAgentSession(ctx, call)
	if err != nil {
		t.Fatalf("OpenAgentSession answered a transport error where a typed failure was owed: %v", err)
	}
	failure := resp.Msg.GetFailure()
	if failure == nil {
		t.Fatalf("OpenAgentSession accepted a request it owed a typed failure for: %v", resp.Msg)
	}
	return failure
}

func readPage(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, book string, pageSize uint32, after *storev1.StoreItemPointer) *storev1.ReadAgentPageSuccess {
	t.Helper()
	resp, err := cli.ReadAgentPage(ctx, connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book:     agentID(book),
		PageSize: pageSize,
		Position: &storev1.ReadAgentPageRequest_After{After: after},
	}))
	if err != nil {
		t.Fatalf("ReadAgentPage(%q) transport error: %v", book, err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		t.Fatalf("ReadAgentPage(%q) refused: %s", book, failure.GetDetail())
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("ReadAgentPage(%q) answered neither arm: %v", book, resp.Msg)
	}
	return success
}

func readPageExpectingFailure(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, req *storev1.ReadAgentPageRequest, requestID ...string) *storev1.ReadAgentPageFailure {
	t.Helper()
	call := connect.NewRequest(req)
	if len(requestID) == 1 && requestID[0] != "" {
		call.Header().Set(requestIDHeader, requestID[0])
	}
	resp, err := cli.ReadAgentPage(ctx, call)
	if err != nil {
		t.Fatalf("ReadAgentPage answered a transport error where a typed failure was owed: %v", err)
	}
	failure := resp.Msg.GetFailure()
	if failure == nil {
		t.Fatalf("ReadAgentPage accepted a request it owed a typed failure for: %v", resp.Msg)
	}
	return failure
}

// liveWork reads ONE SESSION's open obligations: the store is shared by every
// session, so every read names the main agent whose lineage it asks about.
func liveWork(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, session string) *storev1.GetLiveWorkSuccess {
	t.Helper()
	resp, err := cli.GetLiveWork(ctx, connect.NewRequest(&storev1.GetLiveWorkRequest{Session: agentID(session)}))
	if err != nil {
		t.Fatalf("GetLiveWork transport error: %v", err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		t.Fatalf("GetLiveWork refused: %s", failure.GetDetail())
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("GetLiveWork answered neither arm: %v", resp.Msg)
	}
	return success
}

func sidecarCursors(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, fileID *string) []*storev1.CursorState {
	t.Helper()
	resp, err := cli.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{FileId: fileID}))
	if err != nil {
		t.Fatalf("GetSidecarCursors transport error: %v", err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		t.Fatalf("GetSidecarCursors refused: %s", failure.GetDetail())
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("GetSidecarCursors answered neither arm: %v", resp.Msg)
	}
	return success.GetCursors()
}

// cursorsExpectingFailure asks for cursors with a request the store must refuse.
func cursorsExpectingFailure(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, fileID *string, requestID ...string) *storev1.GetSidecarCursorsFailure {
	t.Helper()
	call := connect.NewRequest(&storev1.GetSidecarCursorsRequest{FileId: fileID})
	if len(requestID) == 1 && requestID[0] != "" {
		call.Header().Set(requestIDHeader, requestID[0])
	}
	resp, err := cli.GetSidecarCursors(ctx, call)
	if err != nil {
		t.Fatalf("GetSidecarCursors answered a transport error where a typed failure was owed: %v", err)
	}
	failure := resp.Msg.GetFailure()
	if failure == nil {
		t.Fatalf("GetSidecarCursors accepted a request it owed a typed failure for: %v", resp.Msg)
	}
	return failure
}

// watch is one open tail plus the cancellation that ends it.
//
// A STANDING TAIL IS ENDED BY CANCELLING IT, NEVER BY Close ALONE. A Connect
// client's Close DRAINS the response body, and this stream never ends on its
// own — so a bare Close would block until the caller's own deadline, which is
// exactly what a real consumer must avoid too. Every watch therefore gets its
// own child context, and Close cancels it first.
type watch struct {
	*connect.ServerStreamForClient[storev1.WatchAgentSessionResponse]
	cancel context.CancelFunc
}

// Close ends the tail. The cancellation the harness itself issued is not a
// failure to report; anything else is.
func (w *watch) Close() error {
	w.cancel()
	err := w.ServerStreamForClient.Close()
	if err == nil || errors.Is(err, context.Canceled) || connect.CodeOf(err) == connect.CodeCanceled {
		return nil
	}
	return err
}

// watchStream opens the tail for a token. The caller owns Close.
func watchStream(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, token *storev1.AgentSessionToken) *watch {
	t.Helper()
	streamCtx, cancel := context.WithCancel(ctx)
	stream, err := cli.WatchAgentSession(streamCtx, connect.NewRequest(&storev1.WatchAgentSessionRequest{Watch: token}))
	if err != nil {
		cancel()
		t.Fatalf("WatchAgentSession transport error: %v", err)
	}
	return &watch{ServerStreamForClient: stream, cancel: cancel}
}

// bashWatch is one open WatchBashRun tail plus the cancellation that ends it.
// Unlike a book's tail this stream has a NATURAL END — the run's terminal — so a
// test may legitimately drain it to completion.
type bashWatch struct {
	*connect.ServerStreamForClient[storev1.WatchBashRunResponse]
	cancel context.CancelFunc
}

func (w *bashWatch) Close() error {
	w.cancel()
	err := w.ServerStreamForClient.Close()
	if err == nil || errors.Is(err, context.Canceled) || connect.CodeOf(err) == connect.CodeCanceled {
		return nil
	}
	return err
}

// watchBashRun opens the tail for one run. The caller owns Close.
func watchBashRun(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, run string) *bashWatch {
	t.Helper()
	streamCtx, cancel := context.WithCancel(ctx)
	stream, err := cli.WatchBashRun(streamCtx, connect.NewRequest(&storev1.WatchBashRunRequest{Run: activityID(run)}))
	if err != nil {
		cancel()
		t.Fatalf("WatchBashRun(%q) transport error: %v", run, err)
	}
	return &bashWatch{ServerStreamForClient: stream, cancel: cancel}
}

// bashRowLabel reduces one run row to the arm it carries, so run assertions
// read as text.
func bashRowLabel(row *storev1.StoreAgentBash) string {
	frame := row.GetFrame()
	switch {
	case frame.GetStart() != nil:
		return "start:" + frame.GetStart().GetCommand().GetLine()
	case frame.GetTail() != nil:
		return "tail:" + frame.GetTail().GetText()
	case frame.GetProgress() != nil:
		return "progress"
	case frame.GetSuccess() != nil:
		return "success"
	case frame.GetFailure() != nil:
		return "failure"
	default:
		return "<no frame arm>"
	}
}

// drainBashRun reads a run's stream to its natural end and returns every row's
// label. The stream ENDING is the synchronization primitive: no polling and no
// sleeping anywhere in this path.
func drainBashRun(t *testing.T, stream *bashWatch) []string {
	t.Helper()

	type result struct {
		labels []string
		err    error
	}
	done := make(chan result, 1)
	go func() {
		var labels []string
		for stream.Receive() {
			labels = append(labels, bashRowLabel(stream.Msg().GetRow()))
		}
		done <- result{labels: labels, err: stream.Err()}
	}()

	select {
	case res := <-done:
		if res.err != nil {
			t.Fatalf("the bash run stream ended with an error after %v: %v", res.labels, res.err)
		}
		return res.labels
	case <-time.After(streamTimeout):
		t.Fatalf("the bash run stream did not reach its natural end within %s", streamTimeout)
		return nil
	}
}

// receiveBashRows reads exactly n rows from a run's stream.
func receiveBashRows(t *testing.T, stream *bashWatch, n int) []string {
	t.Helper()

	type result struct {
		labels []string
		err    error
	}
	done := make(chan result, 1)
	go func() {
		var labels []string
		for len(labels) < n {
			if !stream.Receive() {
				done <- result{labels: labels, err: fmt.Errorf("stream ended after %d of %d rows: %w", len(labels), n, stream.Err())}
				return
			}
			labels = append(labels, bashRowLabel(stream.Msg().GetRow()))
		}
		done <- result{labels: labels}
	}()

	select {
	case res := <-done:
		if res.err != nil {
			t.Fatalf("bash run stream: %v", res.err)
		}
		return res.labels
	case <-time.After(streamTimeout):
		t.Fatalf("the bash run stream delivered fewer than %d rows within %s", n, streamTimeout)
		return nil
	}
}

// awaitBashRunEnd drains a run's stream and returns why it ended.
func awaitBashRunEnd(t *testing.T, stream *bashWatch) error {
	t.Helper()

	done := make(chan error, 1)
	go func() {
		for stream.Receive() {
			// Drain: the subject is how the stream ENDS.
		}
		done <- stream.Err()
	}()

	select {
	case err := <-done:
		return err
	case <-time.After(streamTimeout):
		t.Fatalf("the bash run stream did not end within %s", streamTimeout)
		return nil
	}
}

// assertBashRunRefused asserts the refused-open convention for a run the store
// holds no row for: there is no failure frame, so the stream closes at the
// transport with CodeNotFound.
func assertBashRunRefused(t *testing.T, stream *bashWatch) {
	t.Helper()

	done := make(chan error, 1)
	go func() {
		for stream.Receive() {
			done <- fmt.Errorf("a refused bash watch delivered a row")
			return
		}
		done <- stream.Err()
	}()

	select {
	case err := <-done:
		if err == nil {
			t.Fatalf("a refused bash watch ended cleanly; want a Connect %v error", connect.CodeNotFound)
		}
		if code := connect.CodeOf(err); code != connect.CodeNotFound {
			t.Fatalf("a refused bash watch ended with Connect code %v, want %v (error: %v)", code, connect.CodeNotFound, err)
		}
	case <-time.After(streamTimeout):
		t.Fatalf("a refused bash watch neither delivered nor closed within %s", streamTimeout)
	}
}

// receivedLine is one frame a watcher saw, reduced to what tests assert on.
type receivedLine struct {
	pointer string
	text    string
	// retired says the frame arrived on the `retired` arm: the store withdrew
	// the line rather than writing it.
	retired bool
	// place is the line's conversation place as served (placeText).
	place string
}

// placeText renders a served line's place as "<arm>:<at_ms>.<ordinal>", or
// "unset" when the store served none, so a test can compare it whole.
func placeText(at *storev1.StoreLineAt) string {
	switch place := at.GetPlace().(type) {
	case *storev1.StoreLineAt_RecordedPlace:
		return fmt.Sprintf("recorded:%d.%d", place.RecordedPlace.GetAtMs(), place.RecordedPlace.GetOrdinal())
	case *storev1.StoreLineAt_ReceivedPlace:
		return fmt.Sprintf("received:%d.%d", place.ReceivedPlace.GetAtMs(), place.ReceivedPlace.GetOrdinal())
	default:
		return "unset"
	}
}

// receiveLines reads exactly n frames from a stream, failing on a short or
// erroring stream. The stream's own delivery is the synchronization primitive:
// there is no polling and no sleeping anywhere in this path.
func receiveLines(t *testing.T, stream *watch, n int) []receivedLine {
	t.Helper()
	return receiveLinesWithin(t, stream, n, streamTimeout)
}

// receiveLinesWithin is receiveLines with an explicit bound, for the sites that
// read a burst rather than a frame or two.
func receiveLinesWithin(t *testing.T, stream *watch, n int, within time.Duration) []receivedLine {
	t.Helper()

	type result struct {
		lines []receivedLine
		err   error
	}
	done := make(chan result, 1)
	go func() {
		var lines []receivedLine
		for len(lines) < n {
			if !stream.Receive() {
				done <- result{lines: lines, err: fmt.Errorf("stream ended after %d of %d frames: %w", len(lines), n, stream.Err())}
				return
			}
			at := stream.Msg().GetLine()
			retired := stream.Msg().GetRetired()
			if retired != nil {
				at = retired
			}
			lines = append(lines, receivedLine{
				pointer: at.GetAt().GetValue(),
				text:    lineText(at.GetLine()),
				retired: retired != nil,
				place:   placeText(at),
			})
		}
		done <- result{lines: lines}
	}()

	select {
	case res := <-done:
		if res.err != nil {
			t.Fatalf("watch stream: %v", res.err)
		}
		return res.lines
	case <-time.After(within):
		t.Fatalf("watch stream delivered fewer than %d frames within %s", n, within)
		return nil
	}
}

// awaitStreamEnd waits for a stream to end and returns why. A stream that
// keeps delivering forever fails the test rather than hanging the suite.
func awaitStreamEnd(t *testing.T, stream *watch) error {
	t.Helper()

	done := make(chan error, 1)
	go func() {
		for stream.Receive() {
			// Drain: the subject is how the stream ENDS, not what it carried.
		}
		done <- stream.Err()
	}()

	select {
	case err := <-done:
		return err
	case <-time.After(streamTimeout):
		t.Fatalf("watch stream did not end within %s", streamTimeout)
		return nil
	}
}

// ---- page assertions ----

// lineText reduces one page line to the label its builder gave it, so page
// assertions compare readable slices instead of whole protos.
func lineText(line *storev1.StorePageLine) string {
	item := line.GetAgentItem()
	if prompt := item.GetAgentPrompt(); prompt != nil {
		blocks := prompt.GetSaid().GetContent().GetBlocks()
		if len(blocks) == 0 {
			return "prompt:<empty>"
		}
		return "prompt:" + blocks[0].GetText().GetText()
	}
	frame := item.GetAgentFrame()
	if frame == nil {
		return "<no item arm>"
	}
	agent := frame.GetAgentId().GetValue()
	switch {
	case frame.GetSuccess() != nil:
		return "success:" + agent
	case frame.GetFailure() != nil:
		return "failure:" + agent
	case frame.GetDetachedWork() != nil:
		return "detached:" + frame.GetDetachedWork().GetWork().GetValue()
	case frame.GetUpdate() != nil:
		if cut := frame.GetUpdate().GetContextCut(); cut != nil {
			return "cut:" + agent
		}
		if apiErr := frame.GetUpdate().GetApiError(); apiErr != nil {
			return "api_error:" + apiErr.GetMessage()
		}
		activity := frame.GetUpdate().GetActivity()
		if activity == nil {
			return "update:" + agent
		}
		if sub := activity.GetSubagent(); sub != nil {
			if start := sub.GetStart(); start != nil {
				return "spawn:" + start.GetCreatedAgentId().GetValue()
			}
			return "subagent:" + activity.GetActivityId().GetValue()
		}
		if resp := activity.GetResponse(); resp != nil {
			if success := resp.GetSuccess(); success != nil {
				return success.GetProse().GetMarkdown()
			}
		}
		return "activity:" + activity.GetActivityId().GetValue()
	}
	return "<no frame arm>"
}

// pageTexts is an opened page's labels, newest first.
func pageTexts(page *storev1.AgentSessionPage) []string {
	out := make([]string, 0, len(page.GetLines()))
	for _, at := range page.GetLines() {
		out = append(out, lineText(at.GetLine()))
	}
	return out
}

// pagePointers is an opened page's pointers, newest first.
func pagePointers(page *storev1.AgentSessionPage) []string {
	out := make([]string, 0, len(page.GetLines()))
	for _, at := range page.GetLines() {
		out = append(out, at.GetAt().GetValue())
	}
	return out
}

// readTexts is a continuation page's labels, newest first.
func readTexts(page *storev1.ReadAgentPageSuccess) []string {
	out := make([]string, 0, len(page.GetLines()))
	for _, at := range page.GetLines() {
		out = append(out, lineText(at.GetLine()))
	}
	return out
}

// readPointers is a continuation page's pointers, newest first. A continuation
// page carries REAL positions exactly as the opening page does, so a reader
// never has to mint a placeholder mark for a line it walked to.
func readPointers(page *storev1.ReadAgentPageSuccess) []string {
	out := make([]string, 0, len(page.GetLines()))
	for _, at := range page.GetLines() {
		out = append(out, at.GetAt().GetValue())
	}
	return out
}

// receivedTexts is a watcher's labels in delivery order.
func receivedTexts(lines []receivedLine) []string {
	out := make([]string, 0, len(lines))
	for _, line := range lines {
		out = append(out, line.text)
	}
	return out
}

func assertTexts(t *testing.T, what string, got, want []string) {
	t.Helper()
	if len(got) != len(want) {
		t.Fatalf("%s: got %d lines %v, want %d lines %v", what, len(got), got, len(want), want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("%s: line %d is %q, want %q (got %v, want %v)", what, i, got[i], want[i], got, want)
		}
	}
}

// assertFloor asserts a page reached the oldest retained line.
func assertPageFloor(t *testing.T, page *storev1.AgentSessionPage) {
	t.Helper()
	if page.GetFloor() == nil {
		t.Fatalf("page boundary is not floor: %v", page.GetBoundary())
	}
}

// assertPageMore asserts older lines remain and returns the continuation
// pointer.
func assertPageMore(t *testing.T, page *storev1.AgentSessionPage) *storev1.StoreItemPointer {
	t.Helper()
	more := page.GetMore()
	if more == nil {
		t.Fatalf("page boundary is not more: %v", page.GetBoundary())
	}
	return more.GetLastItem()
}

func assertReadFloor(t *testing.T, page *storev1.ReadAgentPageSuccess) {
	t.Helper()
	if page.GetFloor() == nil {
		t.Fatalf("read page boundary is not floor: %v", page.GetBoundary())
	}
}

func assertReadMore(t *testing.T, page *storev1.ReadAgentPageSuccess) *storev1.StoreItemPointer {
	t.Helper()
	more := page.GetMore()
	if more == nil {
		t.Fatalf("read page boundary is not more: %v", page.GetBoundary())
	}
	return more.GetLastItem()
}

// agentValues reduces an id list to plain strings for set assertions.
func agentValues(ids []*conversationv1.AgentId) []string {
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		out = append(out, id.GetValue())
	}
	return out
}

func workValues(ids []*conversationv1.DetachedWorkId) []string {
	out := make([]string, 0, len(ids))
	for _, id := range ids {
		out = append(out, id.GetValue())
	}
	return out
}

func contains(values []string, want string) bool {
	for _, v := range values {
		if v == want {
			return true
		}
	}
	return false
}

// assertWatchRefused asserts the store's refused-watch convention. There is NO
// failure arm on WatchAgentSession by design, so a refusal (an unknown token, a
// token already consumed, a token minted before a restart) closes at the
// transport: the stream's first Receive fails with Connect CodeNotFound, and
// the shim's recovery is to re-open.
func assertWatchRefused(t *testing.T, stream *watch) {
	t.Helper()

	type outcome struct {
		delivered bool
		err       error
	}
	done := make(chan outcome, 1)
	go func() {
		if stream.Receive() {
			done <- outcome{delivered: true}
			return
		}
		done <- outcome{err: stream.Err()}
	}()

	select {
	case got := <-done:
		if got.delivered {
			t.Fatalf("a refused watch delivered a frame instead of closing")
		}
		if got.err == nil {
			t.Fatalf("a refused watch ended cleanly; want a Connect %v error", connect.CodeNotFound)
		}
		if code := connect.CodeOf(got.err); code != connect.CodeNotFound {
			t.Fatalf("a refused watch ended with Connect code %v, want %v (error: %v)", code, connect.CodeNotFound, got.err)
		}
	case <-time.After(streamTimeout):
		t.Fatalf("a refused watch neither delivered nor closed within %s", streamTimeout)
	}
}

// assertWatchExhausted asserts a watcher the store gave up on: the stream ends
// with a Connect error rather than silently stopping, so the caller knows to
// re-open with known_through rather than assuming an idle tail.
func assertWatchExhausted(t *testing.T, err error) {
	t.Helper()
	if err == nil {
		t.Fatalf("an overrun watcher's stream ended cleanly; want a Connect error")
	}
	if code := connect.CodeOf(err); code != connect.CodeResourceExhausted {
		t.Fatalf("an overrun watcher ended with Connect code %v, want %v (error: %v)", code, connect.CodeResourceExhausted, err)
	}
}

// connectGetWorkflow builds the GetWorkflow request for one announced handle.
func connectGetWorkflow(workID string) *connect.Request[storev1.GetWorkflowRequest] {
	return connect.NewRequest(&storev1.GetWorkflowRequest{Work: detachedWorkID(workID)})
}

func TestDescribeStartup(t *testing.T) {
	start := time.Date(2026, 10, 2, 9, 0, 0, 0, time.FixedZone("x", -4*3600))
	at := func(d time.Duration) string { return start.Add(d).Format("2006-01-02T15:04:05.000000-07:00") }
	tests := []struct {
		name        string
		firstAccept time.Duration
		records     []logRecord
		want        string
	}{
		{
			name:        "a process that never accepted nor logged",
			firstAccept: -1,
			want:        "startup: the socket never accepted\n  pid 7 logged nothing",
		},
		{
			name:        "a process that accepted, with its own records timed and another's ignored",
			firstAccept: 1500 * time.Millisecond,
			records: []logRecord{
				{PID: 7, Operation: "store.db.open", Timestamp: at(40 * time.Millisecond)},
				{PID: 8, Operation: "store.serve", Timestamp: at(time.Second)},
				{PID: 7, Operation: "store.server.new", Timestamp: at(1490 * time.Millisecond)},
			},
			want: "startup: the socket first accepted 1.5s after the process started\n  store.db.open at +40ms\n  store.server.new at +1.49s",
		},
		{
			name:        "an unreadable timestamp is named, not dropped",
			firstAccept: -1,
			records:     []logRecord{{PID: 7, Operation: "store.serve", Timestamp: "soon"}},
			want:        "startup: the socket never accepted\n  store.serve at an unreadable timestamp \"soon\"",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := describeStartup(start, tt.firstAccept, 7, tt.records)

			// Assert
			if got != tt.want {
				t.Fatalf("describeStartup =\n%s\nwant\n%s", got, tt.want)
			}
		})
	}
}
