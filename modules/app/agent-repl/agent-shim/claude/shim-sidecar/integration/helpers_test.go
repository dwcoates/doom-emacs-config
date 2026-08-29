// Package integration holds the shim-claude-sidecar's BLACK-BOX suite: the real
// sidecar binary runs as a subprocess against either the real shim-store binary
// on a private socket, or an in-process fake store that records what the sidecar
// wrote and answers scripted results.
//
// NOTHING HERE REACHES INSIDE THE SIDECAR. Every assertion is made on what
// crossed the wire (store.v1 envelopes around conversation.v1 facts, read back
// from the store's own read verbs) or on the sidecar's structured log. The
// vendor's files are BUILT here — copied from testdata/corpus and the captured
// transcript under modules/app/agent-repl/projects/ into vendor-shaped trees —
// and GROWN line by line with an fsync after each append, so the sidecar sees
// exactly what a writing vendor produces.
//
// SYNCHRONIZATION IS ALWAYS A REAL SIGNAL: a WatchAgentSession frame, a channel
// the fake store feeds on every WriteBatch, or a bounded ticker-driven re-read
// of a store verb. There is no time.Sleep anywhere in this package.
package integration

import (
	"bufio"
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
)

// ---------------------------------------------------------------------------
// Contract constants — the vocabulary the briefs pin.
// ---------------------------------------------------------------------------

const (
	// producerSidecar is the WriteBatchRequest.producer string the sidecar must
	// use (endpoint_write_batch.proto: "shim-claude-sidecar").
	producerSidecar = "shim-claude-sidecar"

	// keepaliveMarker prefixes the first text block of a keep-alive user prompt.
	keepaliveMarker = "<!--agent-repl:keepalive-->"

	// vendorSpecificUserPrompt is the kind R15 pins for a file-plane user prompt.
	vendorSpecificUserPrompt = "user_prompt"

	// spoolUID is the uid segment of the vendor's spool root (claude-<uid>).
	spoolUID = "501"

	// waitBudget bounds every "wait until the store shows X" helper. Exceeding
	// it is a test failure, never a retry.
	waitBudget = 60 * time.Second

	// pollTick is the re-read cadence of the bounded store-polling helpers. It
	// is a POLL of a durable surface, never a sleep standing in for a signal.
	pollTick = 20 * time.Millisecond
)

// ---------------------------------------------------------------------------
// Repository layout, resolved from this file's own location.
//
// The paths are derived rather than written as "../../shim-store" so the suite
// is correct wherever the worktree lives and whatever the test's cwd is.
// ---------------------------------------------------------------------------

type layout struct {
	integrationDir string // .../agent-shim/claude/shim-sidecar/integration
	sidecarDir     string // .../agent-shim/claude/shim-sidecar
	agentShimDir   string // .../agent-shim
	storeDir       string // .../agent-shim/shim-store
	moduleRoot     string // .../modules/app/agent-repl
	corpusDir      string // .../modules/app/agent-repl/testdata/corpus
	projectsDir    string // .../modules/app/agent-repl/projects
}

func resolveLayout() (layout, error) {
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		return layout{}, errors.New("runtime.Caller could not locate helpers_test.go")
	}
	l := layout{integrationDir: filepath.Dir(self)}
	l.sidecarDir = filepath.Dir(l.integrationDir)
	l.agentShimDir = filepath.Dir(filepath.Dir(l.sidecarDir)) // shim-sidecar -> claude -> agent-shim
	l.storeDir = filepath.Join(l.agentShimDir, "shim-store")
	l.moduleRoot = filepath.Dir(l.agentShimDir)
	l.corpusDir = filepath.Join(l.moduleRoot, "testdata", "corpus")
	l.projectsDir = filepath.Join(l.moduleRoot, "projects")
	for name, dir := range map[string]string{
		"sidecar module":         l.sidecarDir,
		"store module":           l.storeDir,
		"golden corpus":          l.corpusDir,
		"captured projects tree": l.projectsDir,
	} {
		if _, err := os.Stat(dir); err != nil {
			return layout{}, fmt.Errorf("%s not found at %s: %w", name, dir, err)
		}
	}
	return l, nil
}

// ---------------------------------------------------------------------------
// TestMain: build BOTH binaries once. Both are this team's own systems, so
// composing them in one suite is sanctioned.
// ---------------------------------------------------------------------------

var (
	repo       layout
	sidecarBin string
	storeBin   string
)

func TestMain(m *testing.M) {
	os.Exit(runSuite(m))
}

func runSuite(m *testing.M) int {
	l, err := resolveLayout()
	if err != nil {
		fmt.Fprintf(os.Stderr, "integration: %v\n", err)
		return 1
	}
	repo = l

	binDir, err := os.MkdirTemp("", "agent-repl-itest-bin-")
	if err != nil {
		fmt.Fprintf(os.Stderr, "integration: temp bin dir: %v\n", err)
		return 1
	}
	defer os.RemoveAll(binDir)

	sidecarBin = filepath.Join(binDir, "shim-claude-sidecar")
	storeBin = filepath.Join(binDir, "shim-store")
	if err := goBuild(repo.sidecarDir, sidecarBin); err != nil {
		fmt.Fprintf(os.Stderr, "integration: building the sidecar: %v\n", err)
		return 1
	}
	if err := goBuild(repo.storeDir, storeBin); err != nil {
		fmt.Fprintf(os.Stderr, "integration: building the store: %v\n", err)
		return 1
	}

	// Nothing here ever reaches a vendor; the guard is stated so a regression
	// that tried would fail loudly rather than silently make a call.
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	return m.Run()
}

func goBuild(moduleDir, out string) error {
	cmd := exec.Command("go", "build", "-o", out, ".")
	cmd.Dir = moduleDir
	cmd.Env = append(os.Environ(), "GOFLAGS=")
	combined, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("go build in %s failed: %v\n%s", moduleDir, err, combined)
	}
	return nil
}

// ---------------------------------------------------------------------------
// Short UDS paths. macOS caps a sockaddr_un at ~104 bytes, and t.TempDir() is
// already longer than that, so every socket lives directly under os.TempDir().
// ---------------------------------------------------------------------------

func shortSocketPath(t *testing.T, tag string) string {
	t.Helper()
	buf := make([]byte, 4)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("random socket suffix: %v", err)
	}
	p := filepath.Join(os.TempDir(), fmt.Sprintf("ar-%s-%s.sock", tag, hex.EncodeToString(buf)))
	t.Cleanup(func() { os.Remove(p) })
	return p
}

// ---------------------------------------------------------------------------
// The vendor-shaped file tree.
//
// The vendor writes:
//   <config-root>/projects/<cwd-slug>/<session>.jsonl
//   <config-root>/projects/<cwd-slug>/<session>/subagents/agent-<id>.jsonl
//   <config-root>/projects/<cwd-slug>/<session>/subagents/agent-<id>.meta.json
//   <spool-root>/claude-<uid>/<cwd-slug>/<session>/tasks/<task-id>.output
// ---------------------------------------------------------------------------

// cwdSlug spells a working directory the way the vendor names its project
// directory: every '/' and '.' becomes '-'. Verified against the captured
// projects/ fixture, whose slug is
// "-Users-dodgecoates--config-doom-worktrees-bounce-continuity-probe-hhj" for
// "/Users/dodgecoates/.config/doom-worktrees/bounce-continuity-probe-hhj".
func cwdSlug(dir string) string {
	return strings.NewReplacer("/", "-", ".", "-").Replace(dir)
}

type vendorTree struct {
	t         *testing.T
	Root      string // a --config-roots entry
	SpoolRoot string // a --spool-root value
}

func newVendorTree(t *testing.T) *vendorTree {
	t.Helper()
	base := t.TempDir()
	v := &vendorTree{
		t:         t,
		Root:      filepath.Join(base, "config-root"),
		SpoolRoot: filepath.Join(base, "spool-root"),
	}
	mustMkdirAll(t, filepath.Join(v.Root, "projects"))
	mustMkdirAll(t, filepath.Join(v.SpoolRoot, "claude-"+spoolUID))
	return v
}

// newVendorTreeSharingSpool builds a second config root that writes its spools
// into an existing spool root — the multi-root shape the sidecar discovers.
func newVendorTreeSharingSpool(t *testing.T, spoolRoot string) *vendorTree {
	t.Helper()
	v := &vendorTree{
		t:         t,
		Root:      filepath.Join(t.TempDir(), "config-root-2"),
		SpoolRoot: spoolRoot,
	}
	mustMkdirAll(t, filepath.Join(v.Root, "projects"))
	return v
}

func (v *vendorTree) projectDir(slug string) string {
	return filepath.Join(v.Root, "projects", slug)
}

func (v *vendorTree) sessionPath(slug, session string) string {
	return filepath.Join(v.projectDir(slug), session+".jsonl")
}

func (v *vendorTree) subagentPath(slug, session, agentID string) string {
	return filepath.Join(v.projectDir(slug), session, "subagents", "agent-"+agentID+".jsonl")
}

func (v *vendorTree) subagentMetaPath(slug, session, agentID string) string {
	return filepath.Join(v.projectDir(slug), session, "subagents", "agent-"+agentID+".meta.json")
}

func (v *vendorTree) spoolDir(slug, session string) string {
	return filepath.Join(v.SpoolRoot, "claude-"+spoolUID, slug, session, "tasks")
}

func (v *vendorTree) spoolPath(slug, session, taskID string) string {
	return filepath.Join(v.spoolDir(slug, session), taskID+".output")
}

// unownedDir is a directory under neither config root: files here must be
// invisible to discovery.
func unownedDir(t *testing.T) string {
	t.Helper()
	d := filepath.Join(t.TempDir(), "not-a-root", "projects", "whatever")
	mustMkdirAll(t, d)
	return d
}

func mustMkdirAll(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
}

// ---------------------------------------------------------------------------
// A file the "vendor" is still writing: every append is followed by an fsync,
// so the sidecar reads a partially-written file exactly as it would in life.
// ---------------------------------------------------------------------------

type growingFile struct {
	t    *testing.T
	path string
	f    *os.File
	n    int64
}

func newGrowingFile(t *testing.T, path string) *growingFile {
	t.Helper()
	mustMkdirAll(t, filepath.Dir(path))
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		t.Fatalf("create %s: %v", path, err)
	}
	info, err := f.Stat()
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	g := &growingFile{t: t, path: path, f: f, n: info.Size()}
	t.Cleanup(func() { f.Close() })
	return g
}

// AppendLine writes one JSONL record (newline-terminated) and fsyncs. It
// returns the offset the record STARTED at, which is what a write_id digest and
// a cursor are both stated in.
func (g *growingFile) AppendLine(line string) int64 {
	g.t.Helper()
	if strings.HasSuffix(line, "\n") {
		return g.AppendRaw([]byte(line))
	}
	return g.AppendRaw([]byte(line + "\n"))
}

// AppendRaw writes arbitrary bytes — used for spools, and for cutting a line in
// half so the carry path is exercised.
func (g *growingFile) AppendRaw(b []byte) int64 {
	g.t.Helper()
	start := g.n
	if _, err := g.f.Write(b); err != nil {
		g.t.Fatalf("append to %s: %v", g.path, err)
	}
	if err := g.f.Sync(); err != nil {
		g.t.Fatalf("fsync %s: %v", g.path, err)
	}
	g.n += int64(len(b))
	return start
}

func (g *growingFile) Offset() int64 { return g.n }
func (g *growingFile) Path() string  { return g.path }

// Remove deletes the file under the reader — the file_vanished LOST evidence.
func (g *growingFile) Remove() {
	g.t.Helper()
	g.f.Close()
	if err := os.Remove(g.path); err != nil {
		g.t.Fatalf("remove %s: %v", g.path, err)
	}
}

// fileID spells a file's identity the way CursorState.file_id does: "dev:inode".
func fileID(t *testing.T, path string) string {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		t.Fatalf("stat %s: no syscall.Stat_t", path)
	}
	return fmt.Sprintf("%d:%d", st.Dev, st.Ino)
}

// ---------------------------------------------------------------------------
// Fixtures: the golden corpus and the captured session transcript.
// ---------------------------------------------------------------------------

// corpusLines returns every non-empty line of a corpus fixture, verbatim.
func corpusLines(t *testing.T, rel string) []string {
	t.Helper()
	return readLines(t, filepath.Join(repo.corpusDir, rel))
}

// corpusLine returns one line of a corpus fixture, verbatim.
func corpusLine(t *testing.T, rel string, n int) string {
	t.Helper()
	lines := corpusLines(t, rel)
	if n >= len(lines) {
		t.Fatalf("corpus fixture %s has %d lines, wanted line %d", rel, len(lines), n)
	}
	return lines[n]
}

// corpusRecord parses one line of a corpus fixture into a mutable record.
func corpusRecord(t *testing.T, rel string, n int) map[string]any {
	t.Helper()
	return decodeRecord(t, corpusLine(t, rel, n))
}

// corpusBytes returns a corpus file whole — the spool fixtures are not JSONL.
func corpusBytes(t *testing.T, rel string) []byte {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(repo.corpusDir, rel))
	if err != nil {
		t.Fatalf("read corpus fixture %s: %v", rel, err)
	}
	return b
}

// capturedSession names the one real transcript checked into the repository:
// its project slug, its session uuid, and its lines.
type capturedSession struct {
	Slug    string
	Session string
	Lines   []string
}

func loadCapturedSession(t *testing.T) capturedSession {
	t.Helper()
	entries, err := os.ReadDir(repo.projectsDir)
	if err != nil {
		t.Fatalf("read %s: %v", repo.projectsDir, err)
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		files, err := os.ReadDir(filepath.Join(repo.projectsDir, e.Name()))
		if err != nil {
			t.Fatalf("read %s: %v", filepath.Join(repo.projectsDir, e.Name()), err)
		}
		for _, f := range files {
			if !strings.HasSuffix(f.Name(), ".jsonl") {
				continue
			}
			return capturedSession{
				Slug:    e.Name(),
				Session: strings.TrimSuffix(f.Name(), ".jsonl"),
				Lines:   readLines(t, filepath.Join(repo.projectsDir, e.Name(), f.Name())),
			}
		}
	}
	t.Fatalf("no captured transcript under %s", repo.projectsDir)
	return capturedSession{}
}

func readLines(t *testing.T, path string) []string {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("open %s: %v", path, err)
	}
	defer f.Close()
	var out []string
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for sc.Scan() {
		if strings.TrimSpace(sc.Text()) == "" {
			continue
		}
		out = append(out, sc.Text())
	}
	if err := sc.Err(); err != nil {
		t.Fatalf("scan %s: %v", path, err)
	}
	return out
}

func decodeRecord(t *testing.T, line string) map[string]any {
	t.Helper()
	var obj map[string]any
	if err := json.Unmarshal([]byte(line), &obj); err != nil {
		t.Fatalf("decode vendor record: %v\n%s", err, line)
	}
	return obj
}

func encodeRecord(t *testing.T, obj map[string]any) string {
	t.Helper()
	b, err := json.Marshal(obj)
	if err != nil {
		t.Fatalf("encode vendor record: %v", err)
	}
	return string(b)
}

// withFields returns the record with the named top-level fields replaced. It is
// how a fixture is re-pointed at this test's session, agent, or spool without
// inventing a record shape.
func withFields(t *testing.T, obj map[string]any, kv map[string]any) map[string]any {
	t.Helper()
	out := make(map[string]any, len(obj)+len(kv))
	for k, v := range obj {
		out[k] = v
	}
	for k, v := range kv {
		out[k] = v
	}
	return out
}

// retargetSession re-points a fixture line at this test's session and cwd, so a
// corpus line taken from another capture belongs to the file it is written into.
func retargetSession(t *testing.T, obj map[string]any, session, cwd string) map[string]any {
	t.Helper()
	return withFields(t, obj, map[string]any{"sessionId": session, "cwd": cwd})
}

// ---------------------------------------------------------------------------
// The sidecar under test.
// ---------------------------------------------------------------------------

type sidecarOptions struct {
	StoreSocket  string
	ConfigRoots  []string
	SpoolRoot    string
	LogPath      string
	PollInterval time.Duration
	RescanEvery  time.Duration
	ExtraEnv     []string
}

func defaultSidecarOptions(t *testing.T, storeSocket string, tree *vendorTree) sidecarOptions {
	t.Helper()
	return sidecarOptions{
		StoreSocket:  storeSocket,
		ConfigRoots:  []string{tree.Root},
		SpoolRoot:    tree.SpoolRoot,
		LogPath:      filepath.Join(t.TempDir(), "sidecar.log"),
		PollInterval: 50 * time.Millisecond,
		RescanEvery:  200 * time.Millisecond,
	}
}

type sidecarProc struct {
	t       *testing.T
	cmd     *exec.Cmd
	LogPath string
	done    chan error
	stopped bool
}

func startSidecar(t *testing.T, opts sidecarOptions) *sidecarProc {
	t.Helper()
	mustMkdirAll(t, filepath.Dir(opts.LogPath))
	args := []string{
		"--store-socket", opts.StoreSocket,
		"--config-roots", strings.Join(opts.ConfigRoots, ","),
		"--spool-root", opts.SpoolRoot,
		"--log", opts.LogPath,
		"--poll-interval", opts.PollInterval.String(),
		"--rescan-interval", opts.RescanEvery.String(),
	}
	cmd := exec.Command(sidecarBin, args...)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+opts.StoreSocket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
	)
	cmd.Env = append(cmd.Env, opts.ExtraEnv...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("start sidecar: %v", err)
	}
	p := &sidecarProc{t: t, cmd: cmd, LogPath: opts.LogPath, done: make(chan error, 1)}
	go func() { p.done <- cmd.Wait() }()
	t.Cleanup(p.Stop)
	return p
}

// Stop sends SIGTERM and waits for the process to leave — the orderly stop the
// restart-recovery subject uses to cut the sidecar off mid-file.
func (p *sidecarProc) Stop() {
	p.t.Helper()
	if p.stopped {
		return
	}
	p.stopped = true
	if p.cmd.Process != nil {
		_ = p.cmd.Process.Signal(syscall.SIGTERM)
	}
	select {
	case <-p.done:
	case <-time.After(waitBudget):
		if p.cmd.Process != nil {
			_ = p.cmd.Process.Kill()
		}
		<-p.done
		p.t.Fatalf("sidecar did not exit within %s of SIGTERM", waitBudget)
	}
}

// ---------------------------------------------------------------------------
// A Connect client over a UNIX domain socket.
// ---------------------------------------------------------------------------

func udsHTTPClient(socket string) *http.Client {
	return &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				return (&net.Dialer{}).DialContext(ctx, "unix", socket)
			},
		},
	}
}

func storeClient(socket string) storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(udsHTTPClient(socket), "http://store")
}

// ---------------------------------------------------------------------------
// The REAL store, as a subprocess on a private socket.
// ---------------------------------------------------------------------------

type realStore struct {
	t       *testing.T
	Socket  string
	DBPath  string
	LogPath string
	Client  storev1connect.ShimStoreClient
	cmd     *exec.Cmd
	done    chan error
	stopped bool
}

func startRealStore(t *testing.T) *realStore {
	t.Helper()
	return startRealStoreAt(t, shortSocketPath(t, "store"), filepath.Join(t.TempDir(), "store.db"))
}

// startRealStoreAt starts the store on a named socket and database, so a test
// can stop it and start it again over the SAME durable state.
func startRealStoreAt(t *testing.T, socket, dbPath string) *realStore {
	t.Helper()
	mustMkdirAll(t, filepath.Dir(dbPath))
	logPath := filepath.Join(t.TempDir(), "store.log")
	cmd := exec.Command(storeBin,
		"--socket", socket,
		"--db", dbPath,
		"--log", logPath,
	)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
	)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("start store: %v", err)
	}
	s := &realStore{
		t:       t,
		Socket:  socket,
		DBPath:  dbPath,
		LogPath: logPath,
		Client:  storeClient(socket),
		cmd:     cmd,
		done:    make(chan error, 1),
	}
	go func() { s.done <- cmd.Wait() }()
	t.Cleanup(s.Stop)
	s.awaitReady()
	return s
}

// awaitReady blocks until a real rpc succeeds. The successful rpc IS the signal;
// nothing waits on a duration.
func (s *realStore) awaitReady() {
	s.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), waitBudget)
	defer cancel()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		_, err := s.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))
		if err == nil {
			return
		}
		select {
		case <-ctx.Done():
			s.t.Fatalf("store never answered GetSidecarCursors within %s: %v", waitBudget, err)
		case <-tick.C:
		}
	}
}

func (s *realStore) Stop() {
	s.t.Helper()
	if s.stopped {
		return
	}
	s.stopped = true
	if s.cmd.Process != nil {
		_ = s.cmd.Process.Signal(syscall.SIGTERM)
	}
	select {
	case <-s.done:
	case <-time.After(waitBudget):
		if s.cmd.Process != nil {
			_ = s.cmd.Process.Kill()
		}
		<-s.done
	}
	os.Remove(s.Socket)
}

// ---------------------------------------------------------------------------
// Reading a book back out of the real store.
// ---------------------------------------------------------------------------

func agentID(value string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: value}
}

// openBook opens one agent's reading session and returns the opening page.
func openBook(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, pageSize uint32) *storev1.OpenAgentSessionSuccess {
	t.Helper()
	res, err := c.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent:    agentID(agent),
		PageSize: pageSize,
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%s): %v", agent, err)
	}
	if f := res.Msg.GetFailure(); f != nil {
		t.Fatalf("OpenAgentSession(%s) refused: %s", agent, f.GetDetail())
	}
	ok := res.Msg.GetSuccess()
	if ok == nil {
		t.Fatalf("OpenAgentSession(%s) answered neither arm", agent)
	}
	return ok
}

// bookLines walks a whole book, newest first, across as many ReadAgentPage
// calls as the boundary arm demands.
func bookLines(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, pageSize uint32) []*storev1.StoreLineAt {
	t.Helper()
	opened := openBook(ctx, t, c, agent, pageSize)
	out := append([]*storev1.StoreLineAt(nil), opened.GetPage().GetLines()...)
	more := opened.GetPage().GetMore()
	for more != nil {
		res, err := c.ReadAgentPage(ctx, connect.NewRequest(&storev1.ReadAgentPageRequest{
			Book:     agentID(agent),
			PageSize: pageSize,
			After:    more.GetLastItem(),
		}))
		if err != nil {
			t.Fatalf("ReadAgentPage(%s): %v", agent, err)
		}
		if f := res.Msg.GetFailure(); f != nil {
			t.Fatalf("ReadAgentPage(%s) refused: %s", agent, f.GetDetail())
		}
		ok := res.Msg.GetSuccess()
		for _, line := range ok.GetLines() {
			out = append(out, &storev1.StoreLineAt{Line: line})
		}
		more = ok.GetMore()
		if len(ok.GetLines()) == 0 {
			break
		}
	}
	return out
}

// awaitBookLines re-reads one book until it holds at least want lines, bounded
// by the context. The store read is the signal; the ticker only paces it.
func awaitBookLines(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, want int) []*storev1.StoreLineAt {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	var last []*storev1.StoreLineAt
	for {
		last = bookLines(ctx, t, c, agent, 200)
		if len(last) >= want {
			return last
		}
		select {
		case <-ctx.Done():
			t.Fatalf("book %s held %d lines, wanted at least %d, within the deadline", agent, len(last), want)
		case <-tick.C:
		}
	}
}

// watchBook opens a session and follows its tail. A delivered frame is a
// legitimate synchronization primitive; the returned channel is closed when the
// stream ends.
func watchBook(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, pageSize uint32) (*storev1.OpenAgentSessionSuccess, <-chan *storev1.StoreLineAt) {
	t.Helper()
	opened := openBook(ctx, t, c, agent, pageSize)
	stream, err := c.WatchAgentSession(ctx, connect.NewRequest(&storev1.WatchAgentSessionRequest{
		Watch: opened.GetWatch(),
	}))
	if err != nil {
		t.Fatalf("WatchAgentSession(%s): %v", agent, err)
	}
	out := make(chan *storev1.StoreLineAt, 256)
	go func() {
		defer close(out)
		defer stream.Close()
		for stream.Receive() {
			select {
			case out <- stream.Msg().GetLine():
			case <-ctx.Done():
				return
			}
		}
	}()
	return opened, out
}

// cursorByPath reads the sidecar's persisted cursors and returns the one for a
// path. Looking a cursor up by PATH rather than by a locally computed file_id
// keeps the assertion independent of the store's dev:inode spelling.
func cursorByPath(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, path string) *storev1.CursorState {
	t.Helper()
	for _, cs := range allCursors(ctx, t, c) {
		if samePath(cs.GetPath(), path) {
			return cs
		}
	}
	return nil
}

func allCursors(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient) []*storev1.CursorState {
	t.Helper()
	res, err := c.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))
	if err != nil {
		t.Fatalf("GetSidecarCursors: %v", err)
	}
	if f := res.Msg.GetFailure(); f != nil {
		t.Fatalf("GetSidecarCursors refused: %s", f.GetDetail())
	}
	return res.Msg.GetSuccess().GetCursors()
}

// awaitCursorAtLeast waits until the store holds a cursor for path at or past
// offset, and returns it.
func awaitCursorAtLeast(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, path string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := cursorByPath(ctx, t, c, path); cs != nil && cs.GetOffset() >= offset {
			return cs
		}
		select {
		case <-ctx.Done():
			t.Fatalf("cursor for %s never reached offset %d within the deadline", path, offset)
		case <-tick.C:
		}
	}
}

// samePath compares two paths after resolving the macOS /tmp -> /private/tmp
// symlink, which is exactly the comparison the sidecar itself must make.
func samePath(a, b string) bool {
	return resolved(a) == resolved(b)
}

func resolved(p string) string {
	if r, err := filepath.EvalSymlinks(p); err == nil {
		return r
	}
	return filepath.Clean(p)
}

// ---------------------------------------------------------------------------
// The FAKE store: an in-process ShimStoreHandler that records every WriteBatch
// and answers scripted results.
// ---------------------------------------------------------------------------

type fakeStore struct {
	t      *testing.T
	Socket string

	mu             sync.Mutex
	calls          []string
	batches        []*storev1.WriteBatchRequest
	cursors        []*storev1.CursorState
	cursorsFailure string
	writeFailures  int
	writeDetail    string

	batchC chan *storev1.WriteBatchRequest
	callC  chan string

	srv     *http.Server
	ln      net.Listener
	done    chan struct{}
	stopped bool
}

var _ storev1connect.ShimStoreHandler = (*fakeStore)(nil)

func startFakeStore(t *testing.T) *fakeStore {
	t.Helper()
	return startFakeStoreAt(t, shortSocketPath(t, "fake"))
}

// startFakeStoreAt binds the fake to a NAMED socket, so a test can start the
// sidecar first and let the store appear afterwards.
func startFakeStoreAt(t *testing.T, socket string) *fakeStore {
	t.Helper()
	f := &fakeStore{
		t:      t,
		Socket: socket,
		batchC: make(chan *storev1.WriteBatchRequest, 4096),
		callC:  make(chan string, 4096),
		done:   make(chan struct{}),
	}
	mux := http.NewServeMux()
	path, handler := storev1connect.NewShimStoreHandler(f)
	mux.Handle(path, handler)
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen on %s: %v", socket, err)
	}
	f.ln = ln
	f.srv = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	go func() {
		defer close(f.done)
		_ = f.srv.Serve(ln)
	}()
	t.Cleanup(f.Stop)
	return f
}

func (f *fakeStore) Stop() {
	f.t.Helper()
	f.mu.Lock()
	if f.stopped {
		f.mu.Unlock()
		return
	}
	f.stopped = true
	f.mu.Unlock()
	ctx, cancel := context.WithTimeout(context.Background(), waitBudget)
	defer cancel()
	_ = f.srv.Shutdown(ctx)
	<-f.done
	os.Remove(f.Socket)
}

// FailWrites makes the next n WriteBatch calls answer the failure arm. Nothing
// is recorded as durable for them; the batches are still remembered, so a test
// can prove the sidecar re-sent the SAME records with the SAME write_ids.
func (f *fakeStore) FailWrites(n int, detail string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.writeFailures = n
	f.writeDetail = detail
}

// SeedCursors scripts the GetSidecarCursors answer.
func (f *fakeStore) SeedCursors(cursors ...*storev1.CursorState) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.cursors = cursors
}

// FailCursors makes GetSidecarCursors answer the failure arm.
func (f *fakeStore) FailCursors(detail string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.cursorsFailure = detail
}

func (f *fakeStore) GetSidecarCursors(_ context.Context, req *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	f.mu.Lock()
	f.calls = append(f.calls, "GetSidecarCursors")
	failure := f.cursorsFailure
	cursors := append([]*storev1.CursorState(nil), f.cursors...)
	f.mu.Unlock()
	f.signalCall("GetSidecarCursors")

	if failure != "" {
		return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
			Result: &storev1.GetSidecarCursorsResponse_Failure{
				Failure: &storev1.GetSidecarCursorsFailure{Detail: failure},
			},
		}), nil
	}
	if id := req.Msg.FileId; id != nil {
		filtered := cursors[:0]
		for _, cs := range cursors {
			if cs.GetFileId() == *id {
				filtered = append(filtered, cs)
			}
		}
		cursors = filtered
	}
	return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{
			Success: &storev1.GetSidecarCursorsSuccess{Cursors: cursors},
		},
	}), nil
}

func (f *fakeStore) WriteBatch(_ context.Context, req *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	recorded := proto.Clone(req.Msg).(*storev1.WriteBatchRequest)

	f.mu.Lock()
	f.calls = append(f.calls, "WriteBatch")
	f.batches = append(f.batches, recorded)
	fail := f.writeFailures > 0
	detail := f.writeDetail
	if fail {
		f.writeFailures--
	}
	f.mu.Unlock()

	f.signalCall("WriteBatch")
	select {
	case f.batchC <- recorded:
	default:
	}

	if fail {
		return connect.NewResponse(&storev1.WriteBatchResponse{
			Result: &storev1.WriteBatchResponse_Failure{
				Failure: &storev1.WriteBatchFailure{Detail: detail},
			},
		}), nil
	}
	return connect.NewResponse(&storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}), nil
}

func (f *fakeStore) OpenAgentSession(context.Context, *connect.Request[storev1.OpenAgentSessionRequest]) (*connect.Response[storev1.OpenAgentSessionResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) WatchAgentSession(context.Context, *connect.Request[storev1.WatchAgentSessionRequest], *connect.ServerStream[storev1.WatchAgentSessionResponse]) error {
	return connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) ReadAgentPage(context.Context, *connect.Request[storev1.ReadAgentPageRequest]) (*connect.Response[storev1.ReadAgentPageResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) GetWorkflow(context.Context, *connect.Request[storev1.GetWorkflowRequest]) (*connect.Response[storev1.GetWorkflowResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) GetLiveWork(context.Context, *connect.Request[storev1.GetLiveWorkRequest]) (*connect.Response[storev1.GetLiveWorkResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) signalCall(name string) {
	select {
	case f.callC <- name:
	default:
	}
}

// NextBatch blocks until the sidecar writes another batch — the fake's own
// delivery is the synchronization primitive.
func (f *fakeStore) NextBatch(ctx context.Context, t *testing.T) *storev1.WriteBatchRequest {
	t.Helper()
	select {
	case b := <-f.batchC:
		return b
	case <-ctx.Done():
		t.Fatalf("the sidecar wrote no further batch within the deadline")
		return nil
	}
}

func (f *fakeStore) Calls() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.calls...)
}

func (f *fakeStore) Batches() []*storev1.WriteBatchRequest {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*storev1.WriteBatchRequest(nil), f.batches...)
}

func (f *fakeStore) BatchCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.batches)
}

func (f *fakeStore) CallCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.calls)
}

// Entries flattens every recorded batch into producer order.
func (f *fakeStore) Entries() []*storev1.StoreEntry {
	return entriesOf(f.Batches())
}

// awaitBatches waits until the fake has recorded at least n batches.
func (f *fakeStore) awaitBatches(ctx context.Context, t *testing.T, n int) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for f.BatchCount() < n {
		select {
		case <-ctx.Done():
			t.Fatalf("the sidecar wrote %d batches, wanted at least %d, within the deadline", f.BatchCount(), n)
		case <-tick.C:
		}
	}
}

// awaitEntry waits until some recorded entry satisfies match, and returns it.
func (f *fakeStore) awaitEntry(ctx context.Context, t *testing.T, what string, match func(*storev1.StoreEntry) bool) *storev1.StoreEntry {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, e := range f.Entries() {
			if match(e) {
				return e
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("no entry matching %s was written within the deadline", what)
		case <-tick.C:
		}
	}
}

// ---------------------------------------------------------------------------
// Reading store.v1 envelopes. Every accessor goes through the generated getters
// so a field rename in the proto breaks compilation rather than an assertion.
// ---------------------------------------------------------------------------

func entriesOf(batches []*storev1.WriteBatchRequest) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, b := range batches {
		out = append(out, b.GetBatch().GetEntries()...)
	}
	return out
}

func pageLinesOf(entries []*storev1.StoreEntry) []*storev1.StorePageLine {
	var out []*storev1.StorePageLine
	for _, e := range entries {
		if line := e.GetAgentUpdate().GetServeableFrame(); line != nil {
			out = append(out, line)
		}
	}
	return out
}

func linesForBook(entries []*storev1.StoreEntry, agent string) []*storev1.StorePageLine {
	var out []*storev1.StorePageLine
	for _, line := range pageLinesOf(entries) {
		if line.GetPageAgentId().GetValue() == agent {
			out = append(out, line)
		}
	}
	return out
}

func unservedOf(entries []*storev1.StoreEntry) []*storev1.StoreUnservedItem {
	var out []*storev1.StoreUnservedItem
	for _, e := range entries {
		if u := e.GetAgentUpdate().GetUnservedItem(); u != nil {
			out = append(out, u)
		}
	}
	return out
}

func unknownsOf(entries []*storev1.StoreEntry) []*storev1.StoreUnknown {
	var out []*storev1.StoreUnknown
	for _, u := range unservedOf(entries) {
		if k := u.GetUnknown(); k != nil {
			out = append(out, k)
		}
	}
	return out
}

func unparsedOf(entries []*storev1.StoreEntry) []*storev1.StoreUnparsed {
	var out []*storev1.StoreUnparsed
	for _, u := range unservedOf(entries) {
		if p := u.GetUnparsed(); p != nil {
			out = append(out, p)
		}
	}
	return out
}

func keepalivesOf(entries []*storev1.StoreEntry) []*storev1.StoreAgentItem {
	var out []*storev1.StoreAgentItem
	for _, u := range unservedOf(entries) {
		if k := u.GetKeepalive(); k != nil {
			out = append(out, k)
		}
	}
	return out
}

func vendorSpecificOf(entries []*storev1.StoreEntry) []*storev1.StoreVendorSpecific {
	var out []*storev1.StoreVendorSpecific
	for _, u := range unservedOf(entries) {
		if v := u.GetVendorSpecific(); v != nil {
			out = append(out, v)
		}
	}
	return out
}

func vendorSpecificKinds(entries []*storev1.StoreEntry) []string {
	var out []string
	for _, v := range vendorSpecificOf(entries) {
		out = append(out, v.GetKind())
	}
	sort.Strings(out)
	return out
}

func bashFramesOf(entries []*storev1.StoreEntry) []*storev1.StoreAgentBash {
	var out []*storev1.StoreAgentBash
	for _, e := range entries {
		if b := e.GetAgentUpdate().GetBash(); b != nil {
			out = append(out, b)
		}
	}
	return out
}

// bashFramesForRun keeps only the frames of one detached run, in producer order.
func bashFramesForRun(entries []*storev1.StoreEntry, run string) []*conversationv1.AgentBash {
	var out []*conversationv1.AgentBash
	for _, b := range bashFramesOf(entries) {
		if b.GetRun().GetValue() == run {
			out = append(out, b.GetFrame())
		}
	}
	return out
}

func writeIDList(entries []*storev1.StoreEntry) []string {
	out := make([]string, 0, len(entries))
	for _, e := range entries {
		out = append(out, e.GetWriteId())
	}
	return out
}

func upsertKeySet(entries []*storev1.StoreEntry) map[string]bool {
	out := make(map[string]bool, len(entries))
	for _, e := range entries {
		out[e.GetUpsertKey()] = true
	}
	return out
}

func entryByUpsertKey(entries []*storev1.StoreEntry, key string) *storev1.StoreEntry {
	for i := len(entries) - 1; i >= 0; i-- {
		if entries[i].GetUpsertKey() == key {
			return entries[i]
		}
	}
	return nil
}

// ---------------------------------------------------------------------------
// Reading conversation.v1 facts out of a page line.
// ---------------------------------------------------------------------------

func frameOf(line *storev1.StorePageLine) *conversationv1.AgentFrame {
	return line.GetAgentItem().GetAgentFrame()
}

func activityOf(line *storev1.StorePageLine) *conversationv1.AgentActivity {
	return frameOf(line).GetUpdate().GetActivity()
}

func contextCutOf(line *storev1.StorePageLine) *conversationv1.ContextCut {
	return frameOf(line).GetUpdate().GetContextCut()
}

func apiErrorOf(line *storev1.StorePageLine) *conversationv1.ApiRequestFailed {
	return frameOf(line).GetUpdate().GetApiError()
}

// ---------------------------------------------------------------------------
// The sidecar's structured log.
// ---------------------------------------------------------------------------

// logRecord mirrors the sidecar's canonical JSON record. `context` holds the
// correlation keys (file_id, path, offset, write_id, agent_id, ...).
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

// readLog parses the sidecar log STRICTLY: every non-empty line must be a JSON
// object, because the contract is JSONL and a stray plain-text line is a defect.
func readLog(t *testing.T, path string) []logRecord {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		t.Fatalf("read sidecar log %s: %v", path, err)
	}
	var out []logRecord
	for i, line := range strings.Split(string(b), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("sidecar log line %d is not JSON: %v\n%s", i+1, err, line)
		}
		out = append(out, rec)
	}
	return out
}

func logsAtLevel(recs []logRecord, level string) []logRecord {
	var out []logRecord
	for _, r := range recs {
		if r.Level == level {
			out = append(out, r)
		}
	}
	return out
}

// logsWithContextKey keeps records carrying a correlation key.
func logsWithContextKey(recs []logRecord, key string) []logRecord {
	var out []logRecord
	for _, r := range recs {
		if _, ok := r.Context[key]; ok {
			out = append(out, r)
		}
	}
	return out
}

// awaitLog waits until the sidecar log holds a record satisfying match.
func awaitLog(ctx context.Context, t *testing.T, path string, what string, match func(logRecord) bool) logRecord {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, r := range readLog(t, path) {
			if match(r) {
				return r
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("no sidecar log record matching %s within the deadline", what)
		case <-tick.C:
		}
	}
}

// ---------------------------------------------------------------------------
// Small assertion helpers.
// ---------------------------------------------------------------------------

func testContext(t *testing.T) (context.Context, context.CancelFunc) {
	t.Helper()
	return context.WithTimeout(context.Background(), waitBudget)
}

// requireFilePlane states the envelope duty every sidecar write owes: the file
// plane arm, a non-empty write_id, and a non-empty upsert_key.
func requireFilePlane(t *testing.T, e *storev1.StoreEntry) {
	t.Helper()
	if e.GetPlane().GetFile() == nil {
		t.Errorf("entry %q is not on the file plane: %v", e.GetUpsertKey(), e.GetPlane())
	}
	if e.GetWriteId() == "" {
		t.Errorf("entry %q carries no write_id", e.GetUpsertKey())
	}
	if e.GetUpsertKey() == "" {
		t.Errorf("entry with write_id %q carries no upsert_key", e.GetWriteId())
	}
}

func requireProducer(t *testing.T, batches []*storev1.WriteBatchRequest) {
	t.Helper()
	for i, b := range batches {
		if b.GetProducer() != producerSidecar {
			t.Errorf("batch %d names producer %q, wanted %q", i, b.GetProducer(), producerSidecar)
		}
	}
}

func sortedStrings(in []string) []string {
	out := append([]string(nil), in...)
	sort.Strings(out)
	return out
}

func containsString(in []string, want string) bool {
	for _, s := range in {
		if s == want {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// Vendor-record mutators. Every one of them re-points a REAL fixture; none
// invents a record shape.
// ---------------------------------------------------------------------------

// setUserText replaces the text of a user record's first text block.
func setUserText(t *testing.T, obj map[string]any, text string) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok || len(blocks) == 0 {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	first, ok := blocks[0].(map[string]any)
	if !ok {
		t.Fatalf("record's first content block is not an object: %v", blocks[0])
	}
	newFirst := make(map[string]any, len(first))
	for k, v := range first {
		newFirst[k] = v
	}
	newFirst["text"] = text
	newBlocks := append([]any{newFirst}, blocks[1:]...)
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// setToolUseID re-points a record's single tool_use or tool_result block at a
// new call id, so a corpus call and a corpus result can be paired in one file.
func setToolUseID(t *testing.T, obj map[string]any, id string) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok || len(blocks) == 0 {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	newBlocks := make([]any, 0, len(blocks))
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if !ok {
			newBlocks = append(newBlocks, raw)
			continue
		}
		nb := make(map[string]any, len(b))
		for k, v := range b {
			nb[k] = v
		}
		switch b["type"] {
		case "tool_use":
			nb["id"] = id
		case "tool_result":
			nb["tool_use_id"] = id
		}
		newBlocks = append(newBlocks, nb)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// setToolResultText replaces a tool_result block's content string — used to
// re-point a background-launch result at this test's spool path.
func setToolResultText(t *testing.T, obj map[string]any, text string) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	blocks, ok := msg["content"].([]any)
	if !ok || len(blocks) == 0 {
		t.Fatalf("record's message carries no content blocks: %v", msg)
	}
	newBlocks := make([]any, 0, len(blocks))
	for _, raw := range blocks {
		b, ok := raw.(map[string]any)
		if !ok {
			newBlocks = append(newBlocks, raw)
			continue
		}
		nb := make(map[string]any, len(b))
		for k, v := range b {
			nb[k] = v
		}
		if b["type"] == "tool_result" {
			nb["content"] = text
		}
		newBlocks = append(newBlocks, nb)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["content"] = newBlocks
	return withFields(t, obj, map[string]any{"message": newMsg})
}

// setNested replaces one field of a nested object (e.g. toolUseResult).
func setNested(t *testing.T, obj map[string]any, outer, key string, value any) map[string]any {
	t.Helper()
	inner, ok := obj[outer].(map[string]any)
	if !ok {
		t.Fatalf("record carries no %q object: %v", outer, obj)
	}
	newInner := make(map[string]any, len(inner))
	for k, v := range inner {
		newInner[k] = v
	}
	newInner[key] = value
	return withFields(t, obj, map[string]any{outer: newInner})
}

// backgroundLaunchText spells the vendor's background-launch tool_result prose
// verbatim (the shape the captured transcript and the corpus both carry), with
// this test's task id and spool path substituted in.
func backgroundLaunchText(taskID, spoolPath string) string {
	return fmt.Sprintf(
		"Command running in background with ID: %s. Output is being written to: %s. "+
			"You will be notified when it completes. To check interim output, use Read on that file path.",
		taskID, spoolPath)
}

// compactSummaryLine spells the summary record the vendor writes immediately
// after a compact_boundary — the line the boundary must be coalesced with.
func compactSummaryLine(t *testing.T, session, cwd, uuid, parent, summary string) string {
	t.Helper()
	return encodeRecord(t, map[string]any{
		"parentUuid":       parent,
		"isSidechain":      false,
		"type":             "user",
		"isCompactSummary": true,
		"uuid":             uuid,
		"timestamp":        "2026-08-29T12:00:00.000Z",
		"message":          map[string]any{"role": "user", "content": summary},
		"userType":         "external",
		"entrypoint":       "sdk-cli",
		"cwd":              cwd,
		"sessionId":        session,
		"version":          "2.1.215",
	})
}

// awaitCursorInBatches waits until the sidecar has offered the fake store a
// cursor advance for path at or past offset — the file-plane statement that
// everything up to that byte has been read and handed over.
func awaitCursorInBatches(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := latestCursorFor(f.Batches(), path); cs != nil && cs.GetOffset() >= offset {
			return
		}
		select {
		case <-ctx.Done():
			cs := latestCursorFor(f.Batches(), path)
			t.Fatalf("the sidecar never advanced its cursor for %s to %d (last: %v) within the deadline", path, offset, cs)
		case <-tick.C:
		}
	}
}

// latestCursorFor answers the newest cursor advance a producer offered for a
// path, or nil when it offered none.
func latestCursorFor(batches []*storev1.WriteBatchRequest, path string) *storev1.CursorState {
	var out *storev1.CursorState
	for _, b := range batches {
		cs := b.GetBatch().GetCursorAdvance()
		if cs == nil || !samePath(cs.GetPath(), path) {
			continue
		}
		if out == nil || cs.GetOffset() > out.GetOffset() {
			out = cs
		}
	}
	return out
}

// batchesCarryingCursorFor answers every batch that advanced a path's cursor.
func batchesCarryingCursorFor(batches []*storev1.WriteBatchRequest, path string) []*storev1.WriteBatchRequest {
	var out []*storev1.WriteBatchRequest
	for _, b := range batches {
		if cs := b.GetBatch().GetCursorAdvance(); cs != nil && samePath(cs.GetPath(), path) {
			out = append(out, b)
		}
	}
	return out
}

// samePathAny compares a log context value (always an interface holding a
// string) against a path, resolving the /tmp -> /private/tmp symlink first.
func samePathAny(v any, path string) bool {
	s, ok := v.(string)
	if !ok {
		return false
	}
	return samePath(s, path)
}

// awaitAnyCursorFor waits until the sidecar has offered ANY cursor advance for
// a path — the signal that the file is discovered and being read, used where
// the interesting state is a PARTIAL read rather than a byte count.
func awaitAnyCursorFor(ctx context.Context, t *testing.T, f *fakeStore, path string) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if latestCursorFor(f.Batches(), path) != nil {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the sidecar never offered a cursor for %s within the deadline", path)
		case <-tick.C:
		}
	}
}

// awaitCursorPast waits until the sidecar's cursor for a path moves strictly
// past an offset — the signal that a held frame was released.
func awaitCursorPast(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := latestCursorFor(f.Batches(), path); cs != nil && cs.GetOffset() > offset {
			return cs
		}
		select {
		case <-ctx.Done():
			cs := latestCursorFor(f.Batches(), path)
			t.Fatalf("the cursor for %s never moved past %d (last: %v) within the deadline", path, offset, cs)
			return nil
		case <-tick.C:
		}
	}
}

// ---------------------------------------------------------------------------
// Helper self-tests. These exercise the harness itself — never the sidecar —
// so a fixture-shaping bug fails here rather than as a confusing subject
// failure.
// ---------------------------------------------------------------------------

// TestCwdSlugMatchesTheCapturedProjectDirectory asserts the harness spells a
// project directory exactly as the vendor did in the checked-in capture.
func TestCwdSlugMatchesTheCapturedProjectDirectory(t *testing.T) {
	// Arrange.
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/.config/doom-worktrees/bounce-continuity-probe-hhj"

	// Act.
	got := cwdSlug(cwd)

	// Assert.
	if got != captured.Slug {
		t.Fatalf("cwdSlug(%q) = %q, wanted the captured project directory %q", cwd, got, captured.Slug)
	}
}

// TestTheCapturedTranscriptStillCarriesItsExpectedUnits guards the constants the
// transcript subject asserts against: a re-capture that changed them must fail
// here, loudly, rather than as a mystifying page-order failure.
func TestTheCapturedTranscriptStillCarriesItsExpectedUnits(t *testing.T) {
	// Arrange.
	captured := loadCapturedSession(t)
	joined := strings.Join(captured.Lines, "\n")

	// Act + Assert.
	for _, want := range []string{
		capturedResponse1, capturedResponse2,
		capturedBashCall1, capturedBashCall2,
		capturedSpoolTask1,
	} {
		if !strings.Contains(joined, want) {
			t.Errorf("the captured transcript no longer carries %q; the subject constants are stale", want)
		}
	}
}

// TestGrowingFileReportsTheOffsetEachRecordStartedAt asserts the harness's own
// offset accounting, which every cursor assertion rests on.
func TestGrowingFileReportsTheOffsetEachRecordStartedAt(t *testing.T) {
	// Arrange.
	g := newGrowingFile(t, filepath.Join(t.TempDir(), "grow.jsonl"))

	// Act.
	first := g.AppendLine(`{"a":1}`)
	second := g.AppendLine(`{"b":2}`)

	// Assert.
	if first != 0 {
		t.Errorf("the first record started at %d, wanted 0", first)
	}
	if want := int64(len(`{"a":1}`) + 1); second != want {
		t.Errorf("the second record started at %d, wanted %d", second, want)
	}
	if want := second + int64(len(`{"b":2}`)+1); g.Offset() != want {
		t.Errorf("the file's offset is %d, wanted %d", g.Offset(), want)
	}
}

// setMessageID re-points an assistant record at another API message id, so
// several real fixture lines can be composed into ONE multi-line message and
// the block-ordinal rule exercised across them.
func setMessageID(t *testing.T, obj map[string]any, id string) map[string]any {
	t.Helper()
	msg, ok := obj["message"].(map[string]any)
	if !ok {
		t.Fatalf("record carries no message object: %v", obj)
	}
	newMsg := make(map[string]any, len(msg))
	for k, v := range msg {
		newMsg[k] = v
	}
	newMsg["id"] = id
	return withFields(t, obj, map[string]any{"message": newMsg})
}
