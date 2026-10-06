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
	"bytes"
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
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
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/testclose"
	"agentrepl/testrun/testenv"
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
	//
	// 10s is ~3x the observed healthy max of the slowest WHOLE subject in this
	// package (2.82s, TestMockKeepAliveTurnsNeverReachAPage, measured on a
	// green `go test ./integration/ -count=1 -json` run at the package's own
	// parallelism on a contended machine; 0.92s measured alone). A whole
	// subject's wall time is an upper bound on any single wait inside it, so
	// the budget has that margin over every individual wait several times over.
	//
	// It was 50s, on a stated basis of a ~16.6s healthy max for
	// TestMockScenarios/!subagent. That scenario measures 0.33s and no subtest
	// in the package exceeds 0.87s, so the number was ~50x its own premise and
	// a single red burned 50s of the suite's wall time proving nothing.
	waitBudget = 10 * time.Second

	// snapshotBudget bounds watchBashRun's read of an UNFINISHED run. The
	// endpoint follows until the terminal, so a snapshot of a live run has no
	// other way to end; it is a bound on a stream that is deliberately still
	// open, never a sleep standing in for a signal. 1s: this is a single RPC
	// against a store that is already up (no process boot in the wait), so it
	// does not need waitBudget's headroom for a real vendor/sidecar/store boot.
	snapshotBudget = 1 * time.Second

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
	lockDir        string // .../agent-shim/shim-lock
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
	l.lockDir = filepath.Join(l.agentShimDir, "shim-lock")
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
	repo           layout
	sidecarBin     string
	testBinaryMode testenv.BuildMode

	// The store binary is built LAZILY, on the first test that needs it.
	//
	// Most subjects run against the in-process fake store and never touch the
	// real one, so a store module that does not compile must fail only the tests
	// that actually compose the two systems — not the whole suite, and not the
	// harness's own self-tests.
	storeBinOnce sync.Once
	storeBinPath string
	storeBinErr  error

	// The shim's lock holder, built for the same reason and on the same
	// lazy terms: only the mocked-vendor subjects spawn a real shim, and a
	// real shim takes its kernel claims by spawning this binary.
	lockBinOnce sync.Once
	lockBinPath string
	lockBinErr  error
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

	mode, shared, err := testenv.BinaryMode(os.Getenv)
	if err != nil {
		fmt.Fprintf(os.Stderr, "integration: binary mode: %v\n", err)
		return 1
	}
	testBinaryMode = mode

	var binDir string
	if mode == testenv.BuildHere {
		binDir, err = os.MkdirTemp("", "agent-repl-itest-bin-")
		if err != nil {
			fmt.Fprintf(os.Stderr, "integration: temp bin dir: %v\n", err)
			return 1
		}
		defer func() {
			if err := os.RemoveAll(binDir); err != nil {
				fmt.Fprintf(os.Stderr, "integration: removing temp bin dir %s: %v\n", binDir, err)
			}
		}()
	} else if mode == testenv.BuildInto {
		binDir, err = testenv.PrebuildDir(shared, sidecarIntegrationSharedSub)
		if err != nil {
			fmt.Fprintf(os.Stderr, "integration: %v\n", err)
			return 1
		}
	} else {
		binDir = filepath.Join(shared, sidecarIntegrationSharedSub)
	}

	sidecarBin = filepath.Join(binDir, "shim-claude-sidecar")
	storeBinPath = filepath.Join(binDir, "shim-store")
	lockBinPath = filepath.Join(binDir, "shim-lock")
	if mode == testenv.UsePrebuilt {
		for _, binary := range []struct {
			name string
			dst  *string
		}{
			{name: "shim-claude-sidecar", dst: &sidecarBin},
			{name: "shim-store", dst: &storeBinPath},
			{name: "shim-lock", dst: &lockBinPath},
		} {
			path, err := testenv.SharedBinary(shared, sidecarIntegrationSharedSub, binary.name)
			if err != nil {
				fmt.Fprintf(os.Stderr, "integration: resolve prebuilt %s: %v\n", binary.name, err)
				return 1
			}
			*binary.dst = path
		}
	} else {
		if err := goBuild(repo.sidecarDir, sidecarBin); err != nil {
			fmt.Fprintf(os.Stderr, "integration: building the sidecar: %v\n", err)
			return 1
		}
	}
	if mode == testenv.BuildInto {
		for _, binary := range []struct{ name, dir string }{
			{name: "shim-store", dir: repo.storeDir},
			{name: "shim-lock", dir: repo.lockDir},
		} {
			if err := goBuild(binary.dir, filepath.Join(binDir, binary.name)); err != nil {
				fmt.Fprintf(os.Stderr, "integration: prebuilding %s: %v\n", binary.name, err)
				return 1
			}
		}
		return 0
	}

	// Nothing here ever reaches a vendor; the guard is stated so a regression
	// that tried would fail loudly rather than silently make a call.
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		fmt.Fprintf(os.Stderr, "integration: %v\n", err)
		return 1
	}
	// The mocked vendor's shared throwaway store outlives every subject, so
	// nothing else can stand it down. It is a no-op when nothing started it.
	defer stopSharedVendorStore()
	return m.Run()
}

const sidecarIntegrationSharedSub = "sidecar-integration-bin"

func goBuild(moduleDir, out string) error {
	// -buildvcs=false: a test build never asks git to stamp the binary (no
	// test runs real git, owner rule).
	cmd := exec.Command("go", "build", "-buildvcs=false", "-o", out, ".")
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
	t.Cleanup(func() { testclose.RemoveOrFail(t, p) })
	return p
}

// shimListenSocketPath answers a socket path for the REAL shim's `--listen`
// flag: `workspaceIdFromListenSocket` (agent-shim/claude/shim/src/main.ts)
// refuses to start unless the basename is `<16 hex characters>.sock`, because
// that is how the daemon spells a workspace id, and this suite's mocked-vendor
// drive spawns the real shim binary rather than a fake.
//
// The id is DETERMINISTIC per test rather than random, so a failure is
// reproducible from the test name alone: it is the first 16 hex characters of
// sha256(t.Name()), which also happens to satisfy `^[0-9a-f]{16}$` by
// construction since a hex digest is already lower-case hex.
func shimListenSocketPath(t *testing.T) string {
	t.Helper()
	sum := sha256.Sum256([]byte(t.Name()))
	id := hex.EncodeToString(sum[:])[:16]
	p := filepath.Join(os.TempDir(), id+".sock")
	t.Cleanup(func() { testclose.RemoveOrFail(t, p) })
	return p
}

// TestShimListenSocketPathIsNamedAfterAWorkspaceID pins the contract this
// helper exists to satisfy: the real shim (workspaceIdFromListenSocket in
// agent-shim/claude/shim/src/main.ts) refuses to start unless its --listen
// socket's basename is exactly <16 hex characters>[.n<generation>].sock. A
// regression here would silently fail every mocked-vendor drive with "the
// --listen socket ... is not named after a workspace id" instead of failing
// this narrow, obvious check.
func TestShimListenSocketPathIsNamedAfterAWorkspaceID(t *testing.T) {
	p := shimListenSocketPath(t)
	base := filepath.Base(p)
	stem := strings.TrimSuffix(base, ".sock")
	if !regexp.MustCompile(`^[0-9a-f]{16}$`).MatchString(stem) {
		t.Fatalf("shimListenSocketPath basename %q is not <16 hex characters>.sock", base)
	}
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
// directory: EVERY byte outside [A-Za-z0-9] becomes '-', with case preserved.
// Underscores included — so "/private/var/folders/_m/x" is
// "-private-var-folders--m-x", not "-private-var-folders-_m-x".
//
// THE MAPPING IS LOSSY AND NOTHING HERE EVER INVERTS IT: '/', '.', '-' and '_'
// all collapse onto '-', so a slug names a directory and can never be decoded
// back into one. Every fixture tree is built from a cwd the test already holds.
func cwdSlug(dir string) string {
	out := []byte(dir)
	for i, b := range out {
		switch {
		case b >= 'a' && b <= 'z', b >= 'A' && b <= 'Z', b >= '0' && b <= '9':
		default:
			out[i] = '-'
		}
	}
	return string(out)
}

type vendorTree struct {
	t         *testing.T
	Root      string // a --config-roots entry
	SpoolRoot string // a --spool-root value
	// live is the subject's state root, where a session is made LIVE the way a
	// shim makes one (helpers_live_test.go). Every tree of one subject shares it.
	live *liveRoot
}

func newVendorTree(t *testing.T) *vendorTree {
	t.Helper()
	base := t.TempDir()
	v := &vendorTree{
		t:         t,
		Root:      filepath.Join(base, "config-root"),
		SpoolRoot: filepath.Join(base, "spool-root"),
		live:      liveRootFor(t),
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
		live:      liveRootFor(t),
	}
	mustMkdirAll(t, filepath.Join(v.Root, "projects"))
	return v
}

func (v *vendorTree) projectDir(slug string) string {
	return filepath.Join(v.Root, "projects", slug)
}

// sessionPath is the transcript path of a LIVE agent-repl session: asking for
// it makes the session live (helpers_live_test.go), because only an active
// workspace's files are read.
func (v *vendorTree) sessionPath(slug, session string) string {
	v.live.activate(v.t, session)
	return v.inactiveSessionPath(slug, session)
}

// inactiveSessionPath is the transcript path of a session that is NOT live: a
// session run outside agent-repl, a closed workspace's, or a rotation whose
// link has not landed yet.
func (v *vendorTree) inactiveSessionPath(slug, session string) string {
	return filepath.Join(v.projectDir(slug), session+".jsonl")
}

// subagentPath is a subagent transcript of a LIVE session; its parent session
// is made live.
func (v *vendorTree) subagentPath(slug, session, agentID string) string {
	v.live.activate(v.t, session)
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
	// ErrClosed is not a teardown fault: Remove (below) already closed f as
	// part of deleting the file out from under the reader, and this Cleanup
	// still runs after it.
	t.Cleanup(func() {
		if err := f.Close(); err != nil && !errors.Is(err, os.ErrClosed) {
			t.Errorf("closing %s: %v", path, err)
		}
	})
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
	if err := g.f.Close(); err != nil {
		g.t.Fatalf("close %s before removing it: %v", g.path, err)
	}
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
	defer testclose.OrFail(t, f)
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

// chained re-points each record at the one before it in the vendor's
// parentUuid chain. A subject that writes a SUBSET of a capture's lines has
// broken the capture's own chain, and the chain is what the keep-alive rule
// reads (convert/keepalive.go), so every such subject re-links what it writes.
func chained(t *testing.T, records ...map[string]any) []map[string]any {
	t.Helper()
	out := make([]map[string]any, len(records))
	for i, record := range records {
		if i > 0 {
			record = withFields(t, record, map[string]any{"parentUuid": out[i-1]["uuid"]})
		}
		out[i] = record
	}
	return out
}

// asOwnPrompt gives a user record its own uuid and promptId, so two prompts
// made from ONE captured record open two turns rather than restating one.
func asOwnPrompt(t *testing.T, obj map[string]any, uuid, promptID string) map[string]any {
	t.Helper()
	return withFields(t, obj, map[string]any{"uuid": uuid, "promptId": promptID})
}

// appendRecords writes each record to g, in order.
func appendRecords(t *testing.T, g *growingFile, records ...map[string]any) {
	t.Helper()
	for _, record := range records {
		g.AppendLine(encodeRecord(t, record))
	}
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
	StoreSocket string
	// StateDir is the agent-repl state root the shim writes its identity
	// records under, passed as --state-dir. A zero value is not passed at all,
	// so the sidecar resolves its production default and no subject that is not
	// about identity is affected.
	StateDir     string
	ConfigRoots  []string
	SpoolRoot    string
	LogPath      string
	PollInterval time.Duration
	RescanEvery  time.Duration
	ExtraEnv     []string

	// The LOST policy's windows and the unclaimed-spool hold. A zero field is
	// not passed at all, so the sidecar keeps its production default and every
	// subject that is not about staleness is unaffected by these.
	StaleGrace           time.Duration
	StaleShellSilence    time.Duration
	StaleAgentSilence    time.Duration
	StaleWorkflowSilence time.Duration
	UnownedSpoolWindow   time.Duration

	// The store-recovery ladder's floor and ceiling. Zero is not passed, so the
	// production ladder (250ms doubling to 10s) stands for every subject that
	// is not about an outage.
	RecoverBackoffMin time.Duration
	RecoverBackoffMax time.Duration
}

func defaultSidecarOptions(t *testing.T, storeSocket string, tree *vendorTree) sidecarOptions {
	t.Helper()
	return sidecarOptions{
		StateDir:     tree.live.state,
		StoreSocket:  storeSocket,
		ConfigRoots:  []string{tree.Root},
		SpoolRoot:    tree.SpoolRoot,
		LogPath:      filepath.Join(t.TempDir(), "sidecar.log"),
		PollInterval: 50 * time.Millisecond,
		RescanEvery:  200 * time.Millisecond,
		// The unclaimed-spool hold defaults to 60s in production, which is the
		// whole budget of this suite: a subject that is not ABOUT the hold would
		// simply time out inside it. Subjects that ARE about the hold set their
		// own window.
		UnownedSpoolWindow: 200 * time.Millisecond,
		// THE RECOVERY LADDER IS REAL WALL TIME IN THIS PACKAGE. These subjects
		// run a REAL sidecar process, so the injected clock the unit tests
		// advance (sidecar.now / sidecar.jitter) does not reach it: an outage
		// subject waits out production's own rungs, 250ms doubling to 10s. The
		// suite runs the ladder at 5ms/20ms — the SAME ladder, with the same
		// doubling and the same ceiling-held-forever behavior, at a scale the
		// suite can observe rather than sit through. The shape is what these
		// subjects assert; the durations are production's business.
		RecoverBackoffMin: 5 * time.Millisecond,
		RecoverBackoffMax: 20 * time.Millisecond,
	}
}

// ---------------------------------------------------------------------------
// A child process's own output, kept with the subject that started it.
// ---------------------------------------------------------------------------

// childOutputCap bounds what one child's captured output may hold, so a process
// that loops on an error cannot grow the harness without bound. Everything past
// it is dropped and the drop is STATED, never silently swallowed.
const childOutputCap = 256 << 10

// childOutput collects one child process's stdout and stderr and hands them to
// the subject that started it — and only to that subject, only when it FAILED.
//
// Every one of these children (the sidecar, the real store, the mocked vendor)
// used to write straight to the test binary's own os.Stderr. Serially that read
// as a transcript; in parallel it is a dozen processes interleaving on one fd,
// which buries the evidence a failing subject actually needs and, worse,
// garbles `go test -json`: stray writes are attributed to whichever test the
// parser thinks is current, and a subject's own PASS line can be lost that way.
//
// The evidence is not discarded — each child also writes its own log file, and
// this buffer is printed through t.Logf on failure, where it belongs.
type childOutput struct {
	mu       sync.Mutex
	buf      bytes.Buffer
	dropped  int
	overflow bool
}

func (c *childOutput) Write(p []byte) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if room := childOutputCap - c.buf.Len(); room < len(p) {
		if room > 0 {
			c.buf.Write(p[:room])
		}
		c.dropped += len(p) - max(room, 0)
		c.overflow = true
		return len(p), nil
	}
	return c.buf.Write(p)
}

// captureChild answers the writer a child's stdout and stderr are pointed at,
// and registers the cleanup that prints it if the subject failed.
func captureChild(t *testing.T, what string) *childOutput {
	t.Helper()
	c := &childOutput{}
	t.Cleanup(func() {
		if !t.Failed() {
			return
		}
		c.mu.Lock()
		defer c.mu.Unlock()
		if c.buf.Len() == 0 {
			return
		}
		if c.overflow {
			t.Logf("%s wrote this to its stdout/stderr (%d further byte(s) dropped at the %d-byte cap):\n%s",
				what, c.dropped, childOutputCap, c.buf.String())
			return
		}
		t.Logf("%s wrote this to its stdout/stderr:\n%s", what, c.buf.String())
	})
	return c
}

type sidecarProc struct {
	t       *testing.T
	cmd     *exec.Cmd
	LogPath string
	Daemon  *fakeClientLog
	done    chan error
	stopped bool
}

func startSidecar(t *testing.T, opts sidecarOptions) *sidecarProc {
	t.Helper()
	if opts.StateDir == "" {
		opts.StateDir = liveRootFor(t).state
	}
	daemon := startFakeClientLog(t, opts.StateDir, opts.LogPath, opts.ConfigRoots)
	mustMkdirAll(t, filepath.Dir(opts.LogPath))
	args := []string{
		"--store-socket", opts.StoreSocket,
		"--config-roots", strings.Join(opts.ConfigRoots, ","),
		"--spool-root", opts.SpoolRoot,
		"--log", opts.LogPath,
		"--poll-interval", opts.PollInterval.String(),
		"--rescan-interval", opts.RescanEvery.String(),
	}
	if opts.StateDir != "" {
		args = append(args, "--state-dir", opts.StateDir)
	}
	for _, window := range []struct {
		flag  string
		value time.Duration
	}{
		{"--stale-grace", opts.StaleGrace},
		{"--stale-shell-silence", opts.StaleShellSilence},
		{"--stale-agent-silence", opts.StaleAgentSilence},
		{"--stale-workflow-silence", opts.StaleWorkflowSilence},
		{"--unowned-spool-window", opts.UnownedSpoolWindow},
		{"--recover-backoff-min", opts.RecoverBackoffMin},
		{"--recover-backoff-max", opts.RecoverBackoffMax},
	} {
		if window.value == 0 {
			continue
		}
		args = append(args, window.flag, window.value.String())
	}
	cmd := exec.Command(sidecarBin, args...)
	cmd.Env = make([]string, 0, len(os.Environ())+len(opts.ExtraEnv)+3)
	for _, value := range os.Environ() {
		if !strings.HasPrefix(value, "AGENT_REPL_LOG_LEVEL=") {
			cmd.Env = append(cmd.Env, value)
		}
	}
	cmd.Env = append(cmd.Env,
		"AGENT_REPL_STORE_SOCKET="+opts.StoreSocket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// A private lock dir keeps the boot's build-report write
		// (agentrepl/logging/buildreport) out of the owner's real
		// ~/.cache/agent-repl/run.
		// It is also where the sidecar probes the shims' WORKSPACE LOCKS, so a
		// session the subject made live (helpers_live_test.go) is one this
		// sidecar reads.
		"AGENT_REPL_LOCK_DIR="+liveLockDir(opts.StateDir),
	)
	hasLogLevel := false
	for _, value := range opts.ExtraEnv {
		if strings.HasPrefix(value, "AGENT_REPL_LOG_LEVEL=") {
			hasLogLevel = true
		}
	}
	if !hasLogLevel {
		cmd.Env = append(cmd.Env, "AGENT_REPL_LOG_LEVEL=info")
	}
	cmd.Env = append(cmd.Env, opts.ExtraEnv...)
	captured := captureChild(t, "the sidecar (log: "+opts.LogPath+")")
	cmd.Stdout = captured
	cmd.Stderr = captured
	if err := cmd.Start(); err != nil {
		t.Fatalf("start sidecar: %v", err)
	}
	p := &sidecarProc{t: t, cmd: cmd, LogPath: opts.LogPath, Daemon: daemon, done: make(chan error, 1)}
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

// Kill ENDS the process where it stands, with no chance to shut down.
//
// It exists for the subjects that stop a sidecar while it is FROZEN inside a
// withheld store write (see writeGate). SIGTERM now cancels that write and the
// process leaves promptly (see sigterm_wedged_write_test.go), but it leaves
// through its ORDERLY shutdown; a kill is the harsher precondition those
// subjects want: the restarted reader gets no shutdown's help, only what the
// store already made durable. Releasing the write first is not an option
// either, since that would let the process take one more poll — precisely the
// poll those subjects exist to cut it off before.
func (p *sidecarProc) Kill() {
	p.t.Helper()
	if p.stopped {
		return
	}
	p.stopped = true
	if p.cmd.Process != nil {
		_ = p.cmd.Process.Kill()
	}
	<-p.done
}

// ---------------------------------------------------------------------------
// A Connect client over a UNIX domain socket.
// ---------------------------------------------------------------------------

// udsHTTPClient answers a Connect client's http.Client over one unix socket.
//
// The transport is WRAPPED, and the wrapper is not optional: connect-go
// withdraws a declared-length request body the moment `Do` returns, which a
// peer that answers on the request HEAD turns into a request contradicting its
// own Content-Length and a connection the transport tears down under every
// other call riding it. See {@link ownedRequestBody} for the whole defect and
// the trace that proves it.
func udsHTTPClient(socket string) *http.Client {
	return &http.Client{
		Transport: &ownedRequestBody{next: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				return (&net.Dialer{}).DialContext(ctx, "unix", socket)
			},
		}},
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

// storeBinary builds the real store on first use and answers its path. A store
// module that does not compile fails exactly the tests that compose the two
// systems, and says why.
func storeBinary(t *testing.T) string {
	t.Helper()
	path, err := storeBinaryPath()
	if err != nil {
		t.Fatalf("this subject runs against the REAL store, which does not build: %v", err)
	}
	return path
}

// storeBinaryPath is storeBinary without a *testing.T, for the fixtures whose
// lifetime is the TEST BINARY's rather than one subject's.
func storeBinaryPath() (string, error) {
	storeBinOnce.Do(func() {
		switch testBinaryMode {
		case testenv.BuildHere:
			storeBinErr = goBuild(repo.storeDir, storeBinPath)
		case testenv.UsePrebuilt:
		case testenv.BuildInto:
			storeBinErr = fmt.Errorf("integration: store binary requested while TestMain is only prebuilding")
		}
	})
	if storeBinErr != nil {
		return "", storeBinErr
	}
	return storeBinPath, nil
}

// lockBinary builds the shim's lock holder on first use and answers its path.
//
// EVERY REAL SHIM NEEDS IT. Node cannot take a flock, so the shim's session and
// workspace claims are shim-lock child processes; without the binary the shim
// refuses every StartSession and the mocked vendor generates nothing.
func lockBinary(t *testing.T) string {
	t.Helper()
	lockBinOnce.Do(func() {
		switch testBinaryMode {
		case testenv.BuildHere:
			lockBinErr = goBuild(repo.lockDir, lockBinPath)
		case testenv.UsePrebuilt:
		case testenv.BuildInto:
			lockBinErr = fmt.Errorf("integration: lock binary requested while TestMain is only prebuilding")
		}
	})
	if lockBinErr != nil {
		t.Fatalf("the real shim takes its kernel claims with shim-lock, which does not build: %v", lockBinErr)
	}
	return lockBinPath
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
	cmd := exec.Command(storeBinary(t),
		"--socket", socket,
		"--db", dbPath,
		"--log", logPath,
	)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// The store's database is thrown away with the test: no forced flushes.
		"AGENT_REPL_TEST_SQLITE_UNSYNCED=1",
		// A private lock dir keeps the boot's build-report write
		// (agentrepl/logging/buildreport) out of the owner's real
		// ~/.cache/agent-repl/run.
		"AGENT_REPL_LOCK_DIR="+filepath.Join(t.TempDir(), "lock"),
	)
	captured := captureChild(t, "the real store (log: "+logPath+")")
	cmd.Stdout = captured
	cmd.Stderr = captured
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
	testclose.RemoveOrFail(s.t, s.Socket)
}

// ---------------------------------------------------------------------------
// Reading a book back out of the real store.
// ---------------------------------------------------------------------------

func agentID(value string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: value}
}

// openBook opens one agent's reading session and returns the opening page. The
// agent MUST already name a book: a `unknown_agent` refusal fails the subject
// here, because a one-shot read that names an unregistered agent is asserting
// against a book that was never written.
func openBook(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) *storev1.OpenAgentSessionSuccess {
	t.Helper()
	ok, known := openBookIfKnown(ctx, t, c, agent)
	if !known {
		t.Fatalf("OpenAgentSession(%s) refused: the agent names no book of this store", agent)
	}
	return ok
}

// openBookIfKnown opens one agent's reading session, reporting whether the
// store has HEARD OF the agent at all.
//
// The store's agent register is filled by the sidecar's own page-line writes,
// so between "the file was written" and "the sidecar's first batch committed"
// the agent legitimately names no book and the store refuses with
// `unknown_agent`. For a POLLING reader that refusal is indistinguishable from
// "no lines yet" and is the state it is waiting out; every other failure arm is
// a real defect and fails the subject.
func openBookIfKnown(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) (*storev1.OpenAgentSessionSuccess, bool) {
	t.Helper()
	res, err := c.OpenAgentSession(ctx, connect.NewRequest(&storev1.OpenAgentSessionRequest{
		Agent: agentID(agent),
	}))
	if err != nil {
		t.Fatalf("OpenAgentSession(%s): %v", agent, err)
	}
	if f := res.Msg.GetFailure(); f != nil {
		if f.GetUnknownAgent() != nil {
			return nil, false
		}
		t.Fatalf("OpenAgentSession(%s) refused: %s", agent, f.GetDetail())
	}
	ok := res.Msg.GetSuccess()
	if ok == nil {
		t.Fatalf("OpenAgentSession(%s) answered neither arm", agent)
	}
	return ok, true
}

// bookLines walks a whole book, newest first, across as many ReadAgentPage
// calls as the boundary arm demands.
func bookLines(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) []*storev1.StoreLineAt {
	t.Helper()
	lines, _ := bookLinesIfKnown(ctx, t, c, agent)
	return lines
}

// bookLinesIfKnown is bookLines for a POLLING reader: an agent the store has
// never heard of yields no lines and known=false rather than failing, because
// the register is written by the sidecar's first committed batch.
func bookLinesIfKnown(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) ([]*storev1.StoreLineAt, bool) {
	t.Helper()
	opened, known := openBookIfKnown(ctx, t, c, agent)
	if !known {
		return nil, false
	}
	out := append([]*storev1.StoreLineAt(nil), opened.GetPage().GetLines()...)
	more := opened.GetPage().GetMore()
	for more != nil {
		res, err := c.ReadAgentPage(ctx, connect.NewRequest(&storev1.ReadAgentPageRequest{
			Book:     agentID(agent),
			Position: &storev1.ReadAgentPageRequest_After{After: more.GetLastItem()},
		}))
		if err != nil {
			t.Fatalf("ReadAgentPage(%s): %v", agent, err)
		}
		if f := res.Msg.GetFailure(); f != nil {
			t.Fatalf("ReadAgentPage(%s) refused: %s", agent, f.GetDetail())
		}
		ok := res.Msg.GetSuccess()
		// Landing 3: a page's lines each carry their own store-minted pointer, so
		// they are appended as they arrive rather than re-wrapped without one —
		// the pointer is what a caller reconnects and pages from.
		out = append(out, ok.GetLines()...)
		more = ok.GetMore()
		if len(ok.GetLines()) == 0 {
			break
		}
	}
	return out, true
}

// awaitBookLines re-reads one book until it holds at least want lines, bounded
// by the context. The store read is the signal; the ticker only paces it.
func awaitBookLines(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, want int) []*storev1.StoreLineAt {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	var last []*storev1.StoreLineAt
	for {
		last, _ = bookLinesIfKnown(ctx, t, c, agent)
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

// awaitBookUnits re-reads one book until it holds EVERY named unit, and returns
// its lines.
//
// IT WAITS ON THE UNITS RATHER THAN ON A COUNT, because a count is satisfied by
// whatever happens to be in the book already: a subject that stops a sidecar
// with four lines written and then waits for "at least four" is not waiting for
// anything at all, and reads the book back before the restarted sidecar has
// written a byte.
func awaitBookUnits(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, want ...string) []*storev1.StoreLineAt {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		lines, _ := bookLinesIfKnown(ctx, t, c, agent)
		held := map[string]bool{}
		for _, at := range lines {
			if a := activityOf(at.GetLine()); a != nil {
				held[a.GetActivityId().GetValue()] = true
			}
		}
		missing := false
		for _, id := range want {
			if !held[id] {
				missing = true
				break
			}
		}
		if !missing {
			return lines
		}
		select {
		case <-ctx.Done():
			t.Fatalf("book %s never held every unit %v within the deadline; it holds %v",
				agent, want, sortedStrings(keysOf(held)))
		case <-tick.C:
		}
	}
}

// watchBook opens a session and follows its tail. A delivered frame is a
// legitimate synchronization primitive; the returned channel is closed when the
// stream ends.
func watchBook(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) (*storev1.OpenAgentSessionSuccess, <-chan *storev1.StoreLineAt) {
	t.Helper()
	opened := openBook(ctx, t, c, agent)
	stream, err := c.WatchAgentSession(ctx, connect.NewRequest(&storev1.WatchAgentSessionRequest{
		Watch: opened.GetWatch(),
	}))
	if err != nil {
		t.Fatalf("WatchAgentSession(%s): %v", agent, err)
	}
	out := make(chan *storev1.StoreLineAt, 256)
	go func() {
		defer close(out)
		// NOT testclose.OrFail: this goroutine is not joined by its caller
		// (watchBook returns before it finishes, and several subjects return
		// from their own test function without draining out to closure), so a
		// t.Errorf here could fire after the test has already completed and
		// panic ("Fail in goroutine after Test has completed") instead of
		// reporting anything. A stream Close failure on this best-effort
		// teardown path is discarded rather than risk that.
		defer func() { _ = stream.Close() }()
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

// resolved answers a path's canonical spelling.
//
// IT RESOLVES THE DIRECTORY, NOT THE FILE. filepath.EvalSymlinks fails on a path
// whose last element does not exist, and the subjects that matter most here are
// exactly the ones about a file that was DELETED — so resolving the whole path
// left "/var/…/spool" and "/private/var/…/spool" comparing unequal the moment
// the file vanished, and every vanished-file assertion silently stopped
// matching the reader's own record of it.
func resolved(p string) string {
	if r, err := filepath.EvalSymlinks(p); err == nil {
		return r
	}
	dir, base := filepath.Split(p)
	if dir == "" {
		return p
	}
	if r, err := filepath.EvalSymlinks(filepath.Clean(dir)); err == nil {
		return filepath.Join(r, base)
	}
	return p
}

// ---------------------------------------------------------------------------
// The FAKE store: an in-process ShimStoreHandler that records every WriteBatch
// and answers scripted results.
// ---------------------------------------------------------------------------

type fakeStore struct {
	t      *testing.T
	Socket string

	mu      sync.Mutex
	calls   []string
	batches []*storev1.WriteBatchRequest
	// acked holds only the batches the fake answered with the SUCCESS arm — the
	// ones that are actually durable. A refused batch is remembered in `batches`
	// (a test proves the same records are re-sent under the same write_ids) but
	// it committed NOTHING, so a helper that waits for a cursor to become
	// durable must never be satisfied by one.
	acked []*storev1.WriteBatchRequest
	// bashRows is every StoreAgentBash row the fake made durable, in WRITE
	// ORDER, which is the order store.v1 WatchBashRun replays them in. Keyed by
	// the run so a watch costs one lookup.
	bashRows map[string][]*storev1.StoreAgentBash
	// bashWaiters are the open WatchBashRun streams, per run, each fed every
	// subsequent row of that run.
	bashWaiters map[string][]chan *storev1.StoreAgentBash
	// shellRunClaims is every claim the shim would have written, answered
	// to GetShellRunClaims for the ids asked.
	shellRunClaims []*storev1.ShellRunClaimed
	cursors        []*storev1.CursorState
	cursorsFailure string
	writeFailures  int
	writeDetail    string
	// writeInvalidField, when set alongside a scripted failure, answers the
	// invalid_request arm naming this field — the refusal a retry CANNOT help
	// with. Empty scripts the storage_failure arm, the recoverable one.
	writeInvalidField string
	// writeKindless scripts a failure carrying NEITHER arm, which the contract
	// forbids. It exists so the sidecar's handling of a store that violates the
	// contract has a subject.
	writeKindless bool
	// rejections holds every batch the fake refused on its OWN validation, so a
	// subject can state what the store objected to.
	rejections []string
	// gate, when armed, WITHHOLDS the response to the first batch that matches
	// it. See writeGate.
	gate *writeGate

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
		t:           t,
		Socket:      socket,
		batchC:      make(chan *storev1.WriteBatchRequest, 4096),
		callC:       make(chan string, 4096),
		done:        make(chan struct{}),
		bashRows:    map[string][]*storev1.StoreAgentBash{},
		bashWaiters: map[string][]chan *storev1.StoreAgentBash{},
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
	testclose.RemoveOrFail(f.t, f.Socket)
}

// FailWrites makes the next n WriteBatch calls answer the failure arm. Nothing
// is recorded as durable for them; the batches are still remembered, so a test
// can prove the sidecar re-sent the SAME records with the SAME write_ids.
func (f *fakeStore) FailWrites(n int, detail string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.writeFailures = n
	f.writeDetail = detail
	f.writeInvalidField = ""
	f.writeKindless = false
}

// FailWritesInvalid makes the next n WriteBatch calls answer the
// invalid_request arm naming field — the refusal a retry CANNOT help with, and
// therefore the one the sidecar must treat as a producer defect rather than as
// an outage (ruling R-S2).
func (f *fakeStore) FailWritesInvalid(n int, field, detail string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.writeFailures = n
	f.writeDetail = detail
	f.writeInvalidField = field
	f.writeKindless = false
}

// FailWritesWithoutAKind makes the next n WriteBatch calls answer a failure
// carrying NEITHER arm — a shape this contract forbids, scripted so the
// sidecar's handling of a store that violates it has a subject.
func (f *fakeStore) FailWritesWithoutAKind(n int, detail string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.writeFailures = n
	f.writeDetail = detail
	f.writeInvalidField = ""
	f.writeKindless = true
}

// scriptedFailure builds the failure arm the test asked for. THE KIND IS NOT
// OPTIONAL on this contract: a fake that always omitted it was answering a
// shape the real store never sends, and no subject could then tell the
// recoverable refusal from the one a retry can never fix. Caller must not hold
// mu.
func (f *fakeStore) scriptedFailure(detail string) *storev1.WriteBatchFailure {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := &storev1.WriteBatchFailure{Detail: detail}
	switch {
	case f.writeKindless:
		// Deliberately illegal, and deliberately left unset.
	case f.writeInvalidField != "":
		out.Kind = &storev1.WriteBatchFailure_InvalidRequest{
			InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: f.writeInvalidField},
		}
	default:
		out.Kind = &storev1.WriteBatchFailure_StorageFailure{
			StorageFailure: &storev1.WriteBatchStorageFailure{},
		}
	}
	return out
}

// Rejections lists every batch the fake refused on its OWN validation.
func (f *fakeStore) Rejections() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.rejections...)
}

// validateWriteBatchEnvelope restates shim-store's validateWriteBatchRequest.
// It answers the offending field and the store's account, or "" when the
// request is well formed.
//
// IT IS THE ENVELOPE ONLY, exactly like the real store's: the frame inside a
// StoreEntry is opaque to this layer and is routed by arm further down.
func validateWriteBatchEnvelope(req *storev1.WriteBatchRequest) (string, string) {
	if req.GetProducer() == "" {
		return "producer", "producer: the write names no producer, so nothing can be attributed"
	}
	batch := req.GetBatch()
	if batch == nil {
		return "batch", "batch: the request carries no EntryBatch"
	}
	if len(batch.GetEntries()) == 0 && batch.GetCursorAdvance() == nil {
		return "batch", "batch: the EntryBatch carries neither entries nor a cursor advance"
	}
	for i, e := range batch.GetEntries() {
		what := fmt.Sprintf("batch.entries[%d]", i)
		if e == nil {
			return what, what + ": no entry"
		}
		if e.GetPlane() == nil || e.GetPlane().GetPlane() == nil {
			return what + ".plane", what + ": plane names neither stream nor file"
		}
		if e.GetWriteId() == "" {
			return what + ".write_id", what + ": write_id is empty, so the write cannot be deduped on replay"
		}
		if e.GetUpsertKey() == "" {
			return what + ".upsert_key", what + ": upsert_key is empty, so the write identifies no row"
		}
		if e.GetEntry() == nil {
			return what + ".entry", what + ": the entry oneof names neither agent_update nor session_update"
		}
	}
	// A CURSOR-ONLY BATCH IS LEGAL: a reader that consumed bytes yielding no
	// entries must still make its position durable.
	if cs := batch.GetCursorAdvance(); cs != nil && cs.GetFileId() == "" {
		return "batch.cursor_advance.file_id", "cursor_advance: a CursorState with no file_id keys no row"
	}
	return "", ""
}

// currentConversion is the conversion a stored cursor states when this binary
// wrote it: the current version, with nothing left to re-derive. A seeded
// cursor without it reads as one stored before conversion versions existed,
// and the sidecar re-derives the whole file instead of resuming it.
func currentConversion() *storev1.CursorConversion {
	return &storev1.CursorConversion{
		Version: convert.ConversionVersion,
		State:   &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}},
	}
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

	// THE FAKE VALIDATES WHAT THE REAL STORE VALIDATES (ruling R-S5). A fake
	// that accepts anything makes every subject running against it prove only
	// that the sidecar sent SOMETHING — a batch with an empty write_id, an
	// unset plane or a cursor naming no file would sail through here and fail
	// only in production. The rules are shim-store's own
	// validateWriteBatchRequest, restated in the vocabulary of what it refuses.
	if field, detail := validateWriteBatchEnvelope(req.Msg); field != "" {
		f.mu.Lock()
		f.rejections = append(f.rejections, detail)
		f.mu.Unlock()
		return connect.NewResponse(&storev1.WriteBatchResponse{
			Result: &storev1.WriteBatchResponse_Failure{
				Failure: &storev1.WriteBatchFailure{
					Detail: detail,
					Kind: &storev1.WriteBatchFailure_InvalidRequest{
						InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: field},
					},
				},
			},
		}), nil
	}

	if fail {
		return connect.NewResponse(&storev1.WriteBatchResponse{
			Result: &storev1.WriteBatchResponse_Failure{Failure: f.scriptedFailure(detail)},
		}), nil
	}
	f.mu.Lock()
	f.acked = append(f.acked, recorded)
	f.recordBashRows(recorded)
	gate := f.gate
	f.mu.Unlock()
	// The batch is DURABLE here and only the ANSWER is withheld, so a subject
	// waiting on the gate sees the same store state the producer just wrote.
	gate.hold(recorded)
	return connect.NewResponse(&storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}), nil
}

// ---------------------------------------------------------------------------
// The write gate: STOPPING THE PRODUCER, rather than out-running it.
// ---------------------------------------------------------------------------

// writeGate withholds the answer to ONE batch, chosen by a predicate.
//
// WHY A GATE AND NOT A LONGER TIMER. Several subjects have to act between two
// things the sidecar does back to back — append a summary after a boundary was
// held but before its forced redelivery, or stop the process while a cursor is
// still parked. The way that used to be arranged was to stretch the sidecar's
// poll interval (500ms here, 5s in the restart subject) so the second event was
// far enough away for the test to win the race. That is a race the test usually
// wins, not a race it cannot lose, and it made the hold subjects the slowest in
// the package.
//
// The store is the interlock the system already has. Every batch is written
// SYNCHRONOUSLY inside the cycle and the cursor only advances on a durable
// success, so a store that has not answered is a cycle that has not moved on.
// Holding the answer to the batch that states the hold therefore freezes the
// sidecar at exactly the instant the subject needs, for as long as it needs,
// at whatever poll interval production uses.
type writeGate struct {
	match    func(*storev1.WriteBatchRequest) bool
	reached  chan struct{} // closed when a matching batch is inside the gate
	released chan struct{} // closed by release(), letting the answer out

	reachedOnce  sync.Once
	releasedOnce sync.Once
}

// hold blocks a matching batch's answer until the gate is released. A nil gate
// (nothing armed) is the ordinary case and blocks nothing.
func (g *writeGate) hold(req *storev1.WriteBatchRequest) {
	if g == nil || !g.match(req) {
		return
	}
	g.reachedOnce.Do(func() { close(g.reached) })
	<-g.released
}

// await blocks until the producer is INSIDE the gated write, and fails the
// subject rather than hanging if it never gets there.
func (g *writeGate) await(ctx context.Context, t *testing.T, what string) {
	t.Helper()
	select {
	case <-g.reached:
	case <-ctx.Done():
		t.Fatalf("%s never reached the store within the deadline, so the producer was never stopped where the subject needs it", what)
	}
}

// release lets the withheld answer out. It is idempotent so a subject can call
// it explicitly AND register it as a cleanup, and a failed subject can never
// leave a producer wedged.
func (g *writeGate) release() {
	g.releasedOnce.Do(func() { close(g.released) })
}

// gateOnBatch arms the one-shot gate. The gate is released at cleanup whatever
// the subject does, so a t.Fatalf between arming and releasing cannot leave the
// sidecar blocked in a write for the rest of the run.
func (f *fakeStore) gateOnBatch(t *testing.T, match func(*storev1.WriteBatchRequest) bool) *writeGate {
	t.Helper()
	g := &writeGate{match: match, reached: make(chan struct{}), released: make(chan struct{})}
	f.mu.Lock()
	f.gate = g
	f.mu.Unlock()
	t.Cleanup(g.release)
	return g
}

// cursorParkedAt matches the batch that offers a cursor for a path standing at
// exactly an offset — the hold's own observable event.
func cursorParkedAt(path string, offset int64) func(*storev1.WriteBatchRequest) bool {
	return func(req *storev1.WriteBatchRequest) bool {
		cs := req.GetBatch().GetCursorAdvance()
		return cs != nil && samePath(cs.GetPath(), path) && cs.GetOffset() == offset
	}
}

// recordBashRows keeps every bash row a durable batch carried. Caller holds mu.
//
// ONE ROW PER WRITE, NOT ONE PER RUN: the sidecar's key space gives each delta
// and the terminal its own row precisely so this replay is possible, and a fake
// that collapsed them would hide exactly the defect the key space fixes.
func (f *fakeStore) recordBashRows(request *storev1.WriteBatchRequest) {
	for _, entry := range request.GetBatch().GetEntries() {
		row := entry.GetAgentUpdate().GetBash()
		if row == nil {
			continue
		}
		run := row.GetRun().GetValue()
		f.bashRows[run] = append(f.bashRows[run], row)
		for _, waiter := range f.bashWaiters[run] {
			select {
			case waiter <- row:
			default:
			}
		}
	}
}

// WatchBashRun replays one run's stored rows in write order, then follows, and
// ends after the terminal row is sent — the endpoint's stated contract. A run
// with no stored row is a refused open, which the store spells as a transport
// error rather than a failure frame.
func (f *fakeStore) WatchBashRun(ctx context.Context, req *connect.Request[storev1.WatchBashRunRequest], stream *connect.ServerStream[storev1.WatchBashRunResponse]) error {
	run := req.Msg.GetRun().GetValue()
	f.mu.Lock()
	f.calls = append(f.calls, "WatchBashRun")
	stored := append([]*storev1.StoreAgentBash(nil), f.bashRows[run]...)
	var follow chan *storev1.StoreAgentBash
	if len(stored) > 0 {
		follow = make(chan *storev1.StoreAgentBash, 256)
		f.bashWaiters[run] = append(f.bashWaiters[run], follow)
	}
	f.mu.Unlock()
	f.signalCall("WatchBashRun")

	if len(stored) == 0 {
		return connect.NewError(connect.CodeNotFound, fmt.Errorf("no stored row for run %q", run))
	}
	defer f.dropBashWaiter(run, follow)

	for _, row := range stored {
		if err := stream.Send(&storev1.WatchBashRunResponse{Row: row}); err != nil {
			return err
		}
		if isBashTerminalRow(row) {
			return nil
		}
	}
	for {
		select {
		case <-ctx.Done():
			return nil
		case row := <-follow:
			if err := stream.Send(&storev1.WatchBashRunResponse{Row: row}); err != nil {
				return err
			}
			if isBashTerminalRow(row) {
				return nil
			}
		}
	}
}

func (f *fakeStore) dropBashWaiter(run string, follow chan *storev1.StoreAgentBash) {
	f.mu.Lock()
	defer f.mu.Unlock()
	kept := f.bashWaiters[run][:0]
	for _, w := range f.bashWaiters[run] {
		if w != follow {
			kept = append(kept, w)
		}
	}
	f.bashWaiters[run] = kept
}

// isBashTerminalRow reports whether a row ENDS the run: the two settled arms of
// AgentBash.result. A start, an update and a progress beat are all mid-run.
func isBashTerminalRow(row *storev1.StoreAgentBash) bool {
	frame := row.GetFrame()
	return frame.GetSuccess() != nil || frame.GetFailure() != nil
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

// GetShellRunClaims answers the claims on record for the asked spool ids.
func (f *fakeStore) GetShellRunClaims(_ context.Context, request *connect.Request[storev1.GetShellRunClaimsRequest]) (*connect.Response[storev1.GetShellRunClaimsResponse], error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "GetShellRunClaims")
	asked := map[string]bool{}
	for _, id := range request.Msg.GetVendorTaskIds() {
		asked[id] = true
	}
	var claims []*storev1.ShellRunClaimed
	for _, claim := range f.shellRunClaims {
		if asked[claim.GetClaim().GetVendorTaskId()] {
			claims = append(claims, claim)
		}
	}
	return connect.NewResponse(&storev1.GetShellRunClaimsResponse{
		Result: &storev1.GetShellRunClaimsResponse_Success{Success: &storev1.GetShellRunClaimsSuccess{Claims: claims}},
	}), nil
}

// GetRunSettlements answers the way the store does: a run is settled once a
// terminal for it is DURABLE. The record here is every batch this fake
// acknowledged, so a fresh sidecar pointed at the same fake sees exactly what
// an earlier one made durable.
func (f *fakeStore) GetRunSettlements(_ context.Context, request *connect.Request[storev1.GetRunSettlementsRequest]) (*connect.Response[storev1.GetRunSettlementsResponse], error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "GetRunSettlements")
	ended := map[string]bool{}
	for _, batch := range f.acked {
		for _, entry := range batch.GetBatch().GetEntries() {
			if run, ok := terminalRunOf(entry); ok {
				ended[run] = true
			}
		}
	}
	var settled []*storev1.RunSettlement
	for _, run := range request.Msg.GetRunIds() {
		if ended[run] {
			settled = append(settled, &storev1.RunSettlement{RunId: run, EndedAtMs: 1})
		}
	}
	return connect.NewResponse(&storev1.GetRunSettlementsResponse{
		Result: &storev1.GetRunSettlementsResponse_Success{Success: &storev1.GetRunSettlementsSuccess{Settled: settled}},
	}), nil
}

// terminalRunOf names the run an entry ENDS, the way the store's detached_work
// row is ended: a detached shell's terminal bash frame, or a spawn activity
// that settled (a backgrounded subagent's settle, whose activity id is the run).
func terminalRunOf(entry *storev1.StoreEntry) (string, bool) {
	if bash := entry.GetAgentUpdate().GetBash(); bash != nil {
		if bash.GetFrame().GetSuccess() != nil || bash.GetFrame().GetFailure() != nil {
			return bash.GetRun().GetValue(), true
		}
		return "", false
	}
	activity := entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	subagent := activity.GetSubagent()
	if subagent.GetSuccess() != nil || subagent.GetFailure() != nil {
		return activity.GetActivityId().GetValue(), true
	}
	return "", false
}

// claimShellRun records a claim the shim wrote, with the book holding its
// run's launching call.
func (f *fakeStore) claimShellRun(taskID, run, owner string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.shellRunClaims = append(f.shellRunClaims, &storev1.ShellRunClaimed{
		Claim: &storev1.ShellRunClaim{VendorTaskId: taskID, Run: &conversationv1.AgentActivityId{Value: run}},
		Owner: &conversationv1.AgentId{Value: owner},
	})
}

func (f *fakeStore) GetAgentByVendorTask(context.Context, *connect.Request[storev1.GetAgentByVendorTaskRequest]) (*connect.Response[storev1.GetAgentByVendorTaskResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) GetDetachedWork(context.Context, *connect.Request[storev1.GetDetachedWorkRequest]) (*connect.Response[storev1.GetDetachedWorkResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errors.New("the fake store serves only the sidecar's two verbs"))
}

func (f *fakeStore) ListResidueShapes(context.Context, *connect.Request[storev1.ListResidueShapesRequest]) (*connect.Response[storev1.ListResidueShapesResponse], error) {
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

// AckedBatches returns only the batches the fake made durable.
func (f *fakeStore) AckedBatches() []*storev1.WriteBatchRequest {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*storev1.WriteBatchRequest(nil), f.acked...)
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
// Reading a detached run back through store.v1 WatchBashRun.
//
// THIS IS THE RUN'S ONE READ PATH. The sidecar writes a run's rows and the
// shim's own WatchBash serves them, so a subject that only inspected the
// envelopes the sidecar sent would never prove the rows are READABLE as a run —
// which is exactly what the per-row key space exists for. Reading them back is
// the assertion; the raw envelopes are only how a failure is explained.
// ---------------------------------------------------------------------------

// watchBashRun takes a SNAPSHOT of one run: the rows the store holds for it
// right now, in the order the store replays them.
//
// IT IS BOUNDED, AND THAT IS THE POINT. WatchBashRun replays the stored rows
// and then FOLLOWS, ending only after the terminal — so a run that has not
// finished leaves the stream open indefinitely, which is correct behavior and
// exactly what a snapshot must not wait for. The snapshot therefore ends on the
// terminal row OR on its own short deadline, whichever comes first. A subject
// that wants the terminal uses awaitBashRunTerminal, which follows ONE stream
// to the end.
//
// A run the store holds no row for is a REFUSED OPEN — the endpoint's own
// convention — reported as ok=false rather than as an empty stream that would
// read as "the run produced nothing".
//
// A GENUINE STREAM FAILURE IS FATAL, never quietly reported as a snapshot: a
// read path that dies partway through a replay is precisely what these subjects
// exist to catch, and returning the rows it managed to send first would let it
// pass every assertion that only looked at those.
//
// WHAT ENDS THE SNAPSHOT IS A COUNT THE CALLER READ OFF THE WIRE, not the
// budget. The producer's rows are already durable by the time these subjects
// read a run back — every one of them waited for the cursor covering the writes
// first — so the store replays them from one snapshot taken at stream open,
// back to back. wantRows is what the caller counted in the batches the producer
// sent, so reaching it means the replay is complete AND that the read verb
// agrees with the wire about how many rows there are. A run with FEWER rows
// than the wire carried, or an unfinished run whose caller wants whatever is
// there (wantRows 0), still ends on snapshotBudget, which is a failure bound
// again rather than an unconditional cost paid on every healthy call.
func watchBashRun(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, run string, wantRows int) ([]*conversationv1.AgentBash, bool) {
	t.Helper()
	snapshot, cancel := context.WithTimeout(ctx, snapshotBudget)
	defer cancel()
	stream, err := c.WatchBashRun(snapshot, connect.NewRequest(&storev1.WatchBashRunRequest{
		Run: &conversationv1.AgentActivityId{Value: run},
	}))
	if err != nil {
		return nil, false
	}
	defer func() {
		// THE STREAM IS ENDED BY CANCELLING ITS CONTEXT, NEVER BY CLOSING IT
		// FIRST. A server-streaming Close waits for the server to finish the
		// call, and this server finishes only when the run terminates or the
		// deadline expires — so a caller that already had every row it came for
		// still paid the WHOLE snapshotBudget on the way out, on every healthy
		// call. Defers run last-registered-first, so this one has to do both in
		// the right order rather than leave it to two separate defers.
		cancel()
		_ = stream.Close()
	}()
	var out []*conversationv1.AgentBash
	for stream.Receive() {
		row := stream.Msg().GetRow()
		if got := row.GetRun().GetValue(); got != run {
			t.Fatalf("WatchBashRun(%s) sent a row for run %q", run, got)
		}
		out = append(out, row.GetFrame())
		if isTerminalFrame(row.GetFrame()) {
			return out, true
		}
		if wantRows > 0 && len(out) >= wantRows {
			return out, true
		}
	}
	if err := stream.Err(); err != nil {
		// A REFUSED OPEN ARRIVES HERE, not from the call above: Connect reports a
		// server-side stream error lazily, on the first Receive. NotFound is the
		// endpoint's own convention for a run it holds no row for, and it is an
		// answer rather than a fault.
		if connect.CodeOf(err) == connect.CodeNotFound {
			return nil, false
		}
		// The snapshot window closing on an unfinished run is the ordinary end
		// of a snapshot, not a failure: the run has simply not said anything
		// more yet. Anything else — and any expiry of the CALLER's own deadline,
		// which means the subject itself ran out of time — is a real failure.
		if snapshot.Err() != nil && ctx.Err() == nil {
			if len(out) == 0 {
				return nil, false
			}
			return out, true
		}
		t.Fatalf("WatchBashRun(%s) delivered %d row(s) and then failed: %v", run, len(out), err)
	}
	if len(out) == 0 {
		return nil, false
	}
	return out, true
}

// awaitBashRunTerminal follows ONE stream of a run until its terminal row
// arrives, and returns every row of it in write order.
//
// IT OPENS THE STREAM ONCE. Re-opening per tick replayed the stored rows from
// the start each time and returned as soon as a snapshot happened to end on a
// terminal — so the endpoint's FOLLOW phase (rows delivered live to an already
// open stream) was never driven at all, and its ordering guarantee across the
// replay/follow boundary was never tested. The stream's own delivery is the
// synchronization primitive; the deadline is the context's.
//
// A run the store holds no row for yet is a refused open, so the ONE retry loop
// that remains is the wait for the run to EXIST — never a re-read of one that
// does.
func awaitBashRunTerminal(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, run string) []*conversationv1.AgentBash {
	t.Helper()
	stream, first := awaitBashRunStream(ctx, t, c, run)
	defer testclose.OrFail(t, stream)

	// The first row was consumed to PROVE the run exists (a refusal is reported
	// lazily, on the first Receive), so it is folded in here rather than lost.
	var out []*conversationv1.AgentBash
	if got := first.GetRun().GetValue(); got != run {
		t.Fatalf("WatchBashRun(%s) sent a row for run %q", run, got)
	}
	out = append(out, first.GetFrame())
	if isTerminalFrame(first.GetFrame()) {
		return out
	}
	for stream.Receive() {
		row := stream.Msg().GetRow()
		if got := row.GetRun().GetValue(); got != run {
			t.Fatalf("WatchBashRun(%s) sent a row for run %q", run, got)
		}
		out = append(out, row.GetFrame())
		if isTerminalFrame(row.GetFrame()) {
			return out
		}
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("run %s: the stream failed after %d row(s) without a terminal: %v; its rows were %v",
			run, len(out), err, describeBashRows(out))
	}
	t.Fatalf("run %s: the stream ended after %d row(s) without a terminal; its rows were %v",
		run, len(out), describeBashRows(out))
	return nil
}

// awaitBashRunStream opens ONE WatchBashRun stream, waiting only for the run to
// come into existence — an unknown run is a refused open, which is the
// endpoint's own convention.
func awaitBashRunStream(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, run string) (*connect.ServerStreamForClient[storev1.WatchBashRunResponse], *storev1.StoreAgentBash) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		stream, err := c.WatchBashRun(ctx, connect.NewRequest(&storev1.WatchBashRunRequest{
			Run: &conversationv1.AgentActivityId{Value: run},
		}))
		if err == nil {
			// A REFUSAL IS REPORTED LAZILY on the first Receive, so a stream
			// that opened cleanly may still be a NotFound for a run the store
			// holds nothing for. The first row is what proves the run exists,
			// and it is handed back with the stream so the follow phase is
			// driven by ONE stream from that row onward.
			if stream.Receive() {
				return stream, stream.Msg().GetRow()
			}
			receiveErr := stream.Err()
			testclose.OrFail(t, stream)
			if connect.CodeOf(receiveErr) != connect.CodeNotFound && receiveErr != nil {
				t.Fatalf("run %s: opening its stream failed: %v", run, receiveErr)
			}
			err = receiveErr
		}
		select {
		case <-ctx.Done():
			t.Fatalf("run %s was never openable within the deadline: %v", run, err)
			return nil, nil
		case <-tick.C:
		}
	}
}

// isTerminalFrame reports whether a frame ENDS the run — the two settled arms.
func isTerminalFrame(frame *conversationv1.AgentBash) bool {
	return frame.GetSuccess() != nil || frame.GetFailure() != nil
}

// describeBashRows names each row's arm, for a failure message.
func describeBashRows(rows []*conversationv1.AgentBash) []string {
	var out []string
	for _, row := range rows {
		switch {
		case row.GetStart() != nil:
			out = append(out, "start")
		case row.GetTail() != nil:
			out = append(out, fmt.Sprintf("tail@%d", row.GetTail().GetBytesOmitted()+uint64(len(row.GetTail().GetText()))))
		case row.GetProgress() != nil:
			out = append(out, "progress")
		case row.GetSuccess() != nil:
			out = append(out, "success")
		case row.GetFailure() != nil:
			out = append(out, "failure")
		default:
			out = append(out, "unset")
		}
	}
	return out
}

// requireBashReplayOrder states the endpoint's ordering contract: the start
// first if there is one, then the tail, then the terminal LAST and once.
func requireBashReplayOrder(t *testing.T, run string, rows []*conversationv1.AgentBash) {
	t.Helper()
	if len(rows) == 0 {
		t.Fatalf("run %s replayed no rows at all", run)
	}
	var terminals, updatesAfterTerminal int
	seenTerminal := false
	for _, row := range rows {
		if row.GetStart() != nil && seenTerminal {
			t.Errorf("run %s replayed a start AFTER its terminal: %v", run, describeBashRows(rows))
		}
		if row.GetTail() != nil && seenTerminal {
			updatesAfterTerminal++
		}
		if isTerminalFrame(row) {
			terminals++
			seenTerminal = true
		}
	}
	if terminals != 1 {
		t.Errorf("run %s replayed %d terminal rows, want exactly 1: %v", run, terminals, describeBashRows(rows))
	}
	if updatesAfterTerminal != 0 {
		t.Errorf("run %s replayed %d tail rows after its terminal: %v", run, updatesAfterTerminal, describeBashRows(rows))
	}
	if !isTerminalFrame(rows[len(rows)-1]) {
		t.Errorf("run %s did not end on its terminal row: %v", run, describeBashRows(rows))
	}
}

// requireLatestTail states the tail contract across a run's rows and answers
// the newest tail's text: no tail carries more than the renderer's cap, and each
// one accounts for at least as many written bytes as the one before it, so a
// consumer drawing the newest never draws a run going backwards.
func requireLatestTail(t *testing.T, run string, rows []*conversationv1.AgentBash) string {
	t.Helper()
	var written uint64
	var latest string
	for i, row := range rows {
		tail := row.GetTail()
		if tail == nil {
			continue
		}
		if n := len(tail.GetText()); n > int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES) {
			t.Fatalf("run %s row %d stores %d bytes of tail, past the renderer's cap (%v)", run, i, n, describeBashRows(rows))
		}
		total := tail.GetBytesOmitted() + uint64(len(tail.GetText()))
		if total < written {
			t.Fatalf("run %s row %d accounts for %d written bytes after a row that accounted for %d (%v)",
				run, i, total, written, describeBashRows(rows))
		}
		written = total
		latest = tail.GetText()
	}
	return latest
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
	Timestamp       string         `json:"timestamp"`
	Runtime         string         `json:"runtime"`
	PID             int            `json:"pid"`
	Level           string         `json:"level"`
	Verbosity       string         `json:"verbosity"`
	Operation       string         `json:"operation"`
	Message         string         `json:"message"`
	WorkspaceDir    string         `json:"workspace_dir"`
	WorkspaceID     string         `json:"workspace_id"`
	ClaudeSessionID string         `json:"claude_session_id"`
	RequestID       string         `json:"request_id"`
	Context         map[string]any `json:"context"`
}

// readLog parses the sidecar log STRICTLY: every non-empty line must be a JSON
// object, because the contract is JSONL and a stray plain-text line is a defect.
func readLog(t *testing.T, path string) []logRecord {
	t.Helper()
	out := readLogFile(t, path)
	if value, ok := clientLogsByGlobalPath.Load(path); ok {
		out = append(out, value.(*fakeClientLog).recordsSnapshot()...)
	}
	return out
}

func readLogFile(t *testing.T, path string) []logRecord {
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
			// THE LOG IS THE EVIDENCE: a wait that expires states what the
			// process did say, so a miss is diagnosable from the failure alone.
			for _, r := range readLog(t, path) {
				t.Logf("sidecar log: %s %s %v", r.Operation, r.Message, r.Context)
			}
			t.Fatalf("no sidecar log record matching %s within the deadline", what)
		case <-tick.C:
		}
	}
}

// awaitFirstProductionCycle blocks until the reader's first cursor recovery has
// run, which is when the startup catch-up boundary is fixed. A file created
// after this point is steady state, not backlog: the reader was already running
// when it appeared, so its stale conditions are stated per file rather than
// summarized as catch-up.
// awaitCatchupEnd blocks until the sidecar states that its startup catch-up
// window has closed. A subject that asserts a STEADY-STATE per-item record
// waits here rather than on the first production cycle: between the two, the
// boot walk is still draining the corpus and its records are leveled to DEBUG.
func awaitCatchupEnd(ctx context.Context, t *testing.T, path string) logRecord {
	t.Helper()
	return awaitLog(ctx, t, path, "the end of startup catch-up", func(r logRecord) bool {
		return r.Operation == "catchup-end"
	})
}

func awaitFirstProductionCycle(ctx context.Context, t *testing.T, path string) logRecord {
	t.Helper()
	return awaitLog(ctx, t, path, "the first production cycle", func(r logRecord) bool {
		return r.Operation == "recover-cursors"
	})
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
//
// *** SYNTHETIC. THIS SHAPE IS NOT CAPTURED ANYWHERE. ***
//
// Every other fixture in this suite is a real vendor record re-pointed at the
// test's session; this one is INVENTED, because no capture in
// testdata/corpus/ or under projects/ contains an `isCompactSummary` record at
// all (grep the trees: there is not one). The production converter's
// compactSummaryText reads `type == "user"` and `isCompactSummary == true` and
// takes the summary out of `message.content`, so that much is pinned by the
// code — but the SURROUNDING envelope here (parentUuid, userType, entrypoint,
// version, and whether `content` is a bare string or a block array in the real
// article) is a plausible reconstruction, not evidence.
//
// WHAT THAT COSTS: every subject built on this fixture proves the sidecar
// handles the shape WE BELIEVE the vendor writes. If the real record differs —
// say its content is a block array — the compaction coalescing would fail in
// production while this suite stayed green. It is carried as a CONCERN for the
// teamlead rather than dressed up as a capture; the fix is a real capture of a
// compacted session, and nothing here should be read as one.
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

// awaitCursorInBatches waits until the sidecar has DURABLY advanced its cursor
// for path to at least offset — the file-plane statement that everything up to
// that byte has been read and handed over.
//
// IT COUNTS ONLY ACKED BATCHES. A refused WriteBatch committed nothing, so a
// cursor inside one is an offer the store rejected; waiting on those made every
// "and then it wrote" assertion satisfiable by a write that FAILED, which is how
// a suite that refuses the first writes could sail past the retry it meant to
// observe.
func awaitCursorInBatches(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := latestCursorFor(f.AckedBatches(), path); cs != nil && cs.GetOffset() >= offset {
			return
		}
		select {
		case <-ctx.Done():
			cs := latestCursorFor(f.AckedBatches(), path)
			t.Fatalf("the sidecar never durably advanced its cursor for %s to %d (last: %v) within the deadline", path, offset, cs)
		case <-tick.C:
		}
	}
}

// awaitCursorSettledAt waits until the sidecar's DURABLE cursor for a path
// stands at EXACTLY an offset.
//
// It is the counterpart to awaitCursorInBatches (at-or-past) and
// awaitCursorAtMost (at-or-below): a subject whose whole claim is "the reader
// came back to the file's new length" cannot express itself as an inequality,
// because every intermediate position a legitimate re-read passes through
// satisfies one side or the other.
func awaitCursorSettledAt(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		// The LAST cursor offered, not the highest: a truncation moves the
		// position BACKWARD, so latestCursorFor's maximum would keep answering
		// the pre-truncation offset forever.
		if cs := lastCursorOfferedFor(f.AckedBatches(), path); cs != nil && cs.GetOffset() == offset {
			return cs
		}
		select {
		case <-ctx.Done():
			cs := lastCursorOfferedFor(f.AckedBatches(), path)
			t.Fatalf("the sidecar's durable cursor for %s never settled at %d (last: %v) within the deadline", path, offset, cs)
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

// awaitAnyCursorFor waits until the sidecar has DURABLY advanced a path's
// cursor at all — the signal that the file is discovered and its first bytes
// are committed, used where the interesting state is a PARTIAL read rather than
// a byte count.
//
// IT COUNTS ONLY ACKED BATCHES, like awaitCursorInBatches and for the same
// reason: a refused WriteBatch committed NOTHING, so a cursor inside one is an
// offer the store rejected. Waiting on offers made every "and then it wrote"
// assertion satisfiable by a write that FAILED.
func awaitAnyCursorFor(ctx context.Context, t *testing.T, f *fakeStore, path string) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if latestCursorFor(f.AckedBatches(), path) != nil {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the sidecar never offered a cursor for %s within the deadline", path)
		case <-tick.C:
		}
	}
}

// awaitCursorPast waits until the sidecar's DURABLE cursor for a path moves
// strictly past an offset — the signal that a held frame was released.
//
// IT COUNTS ONLY ACKED BATCHES: a released hold is a released hold only once
// the store took the batch that carried it past the held byte.
func awaitCursorPast(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := latestCursorFor(f.AckedBatches(), path); cs != nil && cs.GetOffset() > offset {
			return cs
		}
		select {
		case <-ctx.Done():
			cs := latestCursorFor(f.AckedBatches(), path)
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
	t.Parallel()
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

// TestCwdSlugReplacesEveryNonAlphanumericByte pins the project lead's rule at
// the example that distinguishes it from the narrower "/ and . only" reading:
// the underscore collapses onto '-' like every other non-alphanumeric byte.
func TestCwdSlugReplacesEveryNonAlphanumericByte(t *testing.T) {
	t.Parallel()
	// Arrange.
	cwd := "/private/var/folders/_m/x"

	// Act.
	got := cwdSlug(cwd)

	// Assert.
	if want := "-private-var-folders--m-x"; got != want {
		t.Fatalf("cwdSlug(%q) = %q, wanted %q", cwd, got, want)
	}
}

// TestCwdSlugPreservesCase asserts the mapping touches only the bytes outside
// [A-Za-z0-9]; a capital stays capital.
func TestCwdSlugPreservesCase(t *testing.T) {
	t.Parallel()
	// Arrange.
	cwd := "/Users/DodgeCoates/Repo9"

	// Act.
	got := cwdSlug(cwd)

	// Assert.
	if want := "-Users-DodgeCoates-Repo9"; got != want {
		t.Fatalf("cwdSlug(%q) = %q, wanted %q", cwd, got, want)
	}
}

// TestTheCapturedTranscriptStillCarriesItsExpectedUnits guards the constants the
// transcript subject asserts against: a re-capture that changed them must fail
// here, loudly, rather than as a mystifying page-order failure.
func TestTheCapturedTranscriptStillCarriesItsExpectedUnits(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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

// TestVendorTreeMatchesTheDiscoveredPathShapes pins the harness's fixture layout
// against the four shapes discovery globs for. Stated as literals rather than by
// importing internal/discover, so the suite stays black-box: if the production
// layout moves, this fails in the harness instead of as a mystifying "no page
// line was ever written" in every subject.
//
// The shapes (internal/discover/discover.go's header):
//
//	<config root>/projects/<project>/<session>.jsonl
//	<config root>/projects/<project>/<session>/subagents/agent-<id>.jsonl
//	<config root>/projects/<project>/<session>/subagents/agent-<id>.meta.json
//	<spool root>/claude-<uid>/<project>/<session>/tasks/<task>.output
//
// --spool-root is the PARENT of claude-<uid>, so the real tree is
// /tmp/claude-<uid>/<project>/<session>/tasks/.
func TestVendorTreeMatchesTheDiscoveredPathShapes(t *testing.T) {
	t.Parallel()
	// Arrange.
	tree := newVendorTree(t)
	slug := cwdSlug("/Users/dodgecoates/layout-probe")
	session := "0e0e0e0e-0e0e-40e0-80e0-0e0e0e0e0e0e"
	agent := "aef975b7bc3422d4b"
	task := "b17"

	// Act + Assert.
	for _, tc := range []struct {
		name string
		got  string
		want string
	}{
		{
			name: "session transcript",
			got:  tree.sessionPath(slug, session),
			want: filepath.Join(tree.Root, "projects", slug, session+".jsonl"),
		},
		{
			name: "subagent transcript",
			got:  tree.subagentPath(slug, session, agent),
			want: filepath.Join(tree.Root, "projects", slug, session, "subagents", "agent-"+agent+".jsonl"),
		},
		{
			name: "subagent meta companion",
			got:  tree.subagentMetaPath(slug, session, agent),
			want: filepath.Join(tree.Root, "projects", slug, session, "subagents", "agent-"+agent+".meta.json"),
		},
		{
			name: "task spool",
			got:  tree.spoolPath(slug, session, task),
			want: filepath.Join(tree.SpoolRoot, "claude-"+spoolUID, slug, session, "tasks", task+".output"),
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if tc.got != tc.want {
				t.Fatalf("the harness writes a %s at\n  %s\nbut discovery globs for\n  %s", tc.name, tc.got, tc.want)
			}
		})
	}
}

// TestTheSpoolRootIsTheParentOfTheUidSegment guards the one layout detail that
// is easy to get wrong: --spool-root does NOT include claude-<uid>; the sidecar
// resolves that segment itself.
func TestTheSpoolRootIsTheParentOfTheUidSegment(t *testing.T) {
	t.Parallel()
	// Arrange.
	tree := newVendorTree(t)
	slug := cwdSlug("/Users/dodgecoates/spool-root-probe")
	session := "0f0f0f0f-0f0f-40f0-80f0-0f0f0f0f0f0f"

	// Act.
	got := tree.spoolDir(slug, session)
	rel, err := filepath.Rel(tree.SpoolRoot, got)
	if err != nil {
		t.Fatalf("spool dir %s is not under the spool root %s: %v", got, tree.SpoolRoot, err)
	}

	// Assert.
	segs := strings.Split(filepath.ToSlash(rel), "/")
	want := []string{"claude-" + spoolUID, slug, session, "tasks"}
	if len(segs) != len(want) {
		t.Fatalf("spool dir is %d segments below the spool root (%v), wanted %d (%v)", len(segs), segs, len(want), want)
	}
	for i := range want {
		if segs[i] != want[i] {
			t.Fatalf("spool path segment %d is %q, wanted %q", i, segs[i], want[i])
		}
	}
}

// ---------------------------------------------------------------------------
// Residue is never persisted (owner ruling 2026-09-13)
// ---------------------------------------------------------------------------
//
// The sidecar stores only TYPED entries. Every residue outcome — `vendor_specific`
// of any kind, `unknown`, and the `unparsed` bytes an unowned spool ingests — is
// still read, framed and CLASSIFIED, and is then counted and withheld at the
// write path. So a subject that used to look for a residue ROW now looks for the
// sidecar saying it classified that residue and stored none of it.
//
// THE WITHHOLDING RECORD IS VERBOSE, because it is the steady state: use
// debugLogging on the options, or the records the assertions read are never
// emitted.

// debugLogging turns on the sidecar's verbose emission, which is what the
// per-record residue-withholding records are written at.
func debugLogging(opts sidecarOptions) sidecarOptions {
	opts.ExtraEnv = append(opts.ExtraEnv, "AGENT_REPL_LOG_LEVEL=debug")
	return opts
}

// awaitResidueWithheld waits until the sidecar's log says it classified `label`
// and did not store it. The label is the residue arm plus its own discriminator
// — `vendor_specific/<kind>`, `unknown/<field>:<discriminator>`, or `unparsed`
// — which is exactly what `convert.ResidueLabel` mints.
func awaitResidueWithheld(ctx context.Context, t *testing.T, logPath, label string) logRecord {
	t.Helper()
	return awaitLog(ctx, t, logPath, "residue "+label+" classified and withheld", func(r logRecord) bool {
		return r.Operation == "residue-drop" && r.Context["reason"] == label
	})
}

// residueWithheldLabels answers every residue label the sidecar has said it
// withheld, for a failure message that names what WAS classified.
func residueWithheldLabels(t *testing.T, logPath string) []string {
	t.Helper()
	seen := map[string]bool{}
	for _, r := range readLog(t, logPath) {
		if r.Operation != "residue-drop" {
			continue
		}
		if label, ok := r.Context["reason"].(string); ok {
			seen[label] = true
		}
	}
	out := make([]string, 0, len(seen))
	for label := range seen {
		out = append(out, label)
	}
	sort.Strings(out)
	return out
}

// requireNoResidueStored asserts the invariant itself: whatever the reader
// classified, the store holds no residue row of any arm.
func requireNoResidueStored(t *testing.T, entries []*storev1.StoreEntry) {
	t.Helper()
	for _, e := range entries {
		if label := residueLabelOf(e); label != "" {
			t.Errorf("the store holds residue %q (upsert_key %q); only typed entries are persisted", label, e.GetUpsertKey())
		}
	}
}

// residueLabelOf mirrors convert.ResidueLabel over a stored entry, so a subject
// can name what leaked without importing the reader's own package.
func residueLabelOf(e *storev1.StoreEntry) string {
	switch arm := e.GetAgentUpdate().GetUnservedItem().GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		return "vendor_specific/" + arm.VendorSpecific.GetKind()
	case *storev1.StoreUnservedItem_Unknown:
		return fmt.Sprintf("unknown/%s:%s", arm.Unknown.GetDiscriminatorField(), arm.Unknown.GetDiscriminator())
	case *storev1.StoreUnservedItem_Unparsed:
		return "unparsed"
	default:
		return ""
	}
}
