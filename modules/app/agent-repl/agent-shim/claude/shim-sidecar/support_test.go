package main

import (
	"context"
	"crypto/md5"
	"crypto/rand"
	"encoding/hex"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/livelock"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
)

// currentConversion is the conversion a stored cursor states when this binary
// wrote it: the current version, with nothing left to re-derive. A seeded
// cursor without it reads as a pre-versioning one and starts a heal.
func currentConversion() *storev1.CursorConversion {
	return &storev1.CursorConversion{
		Version: convert.ConversionVersion,
		State:   &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}},
	}
}

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// fakeStore answers the sidecar's two verbs and records what it was asked.
type fakeStore struct {
	storev1connect.UnimplementedShimStoreHandler

	cursors      []*storev1.CursorState
	cursorsFail  string // non-empty: answer the failure arm
	cursorsCalls int

	writeFail string // non-empty: answer the failure arm
	// writeInvalidField, when set alongside writeFail, answers the
	// invalid_request arm naming this field — the refusal a retry CANNOT help
	// with. Left empty the failure carries the storage_failure arm, which is
	// the recoverable one. A kind-less failure is a separate, deliberately
	// illegal shape: writeKindless.
	writeInvalidField string
	writeKindless     bool
	writes            []*storev1.EntryBatch
	writeCalls        int

	// writeWedged makes WriteBatch hang until the CALL's context is cancelled,
	// which is what a store that accepted the connection and then stopped
	// answering looks like from here. entered is closed on the first such call
	// so a subject can wait for the wedge instead of sleeping toward it.
	writeWedged bool
	entered     chan struct{}

	// cursorsWedged is the same wedge on the cursor verb: GetSidecarCursors
	// hangs until the CALL's context is cancelled, so a subject can withdraw a
	// recovery that is genuinely in flight rather than one that already failed.
	// cursorsEntered is closed on the first such call.
	cursorsWedged  bool
	cursorsEntered chan struct{}
	// shapes is the residue shape catalog observations each write carried, one
	// entry per WriteBatch call, so a test can assert what a withheld line left
	// behind.
	shapes [][]*storev1.ShapeObservation

	// claims is every shell run claim on record; claimsFail, non-empty,
	// answers the failure arm; claimsAsked is every id list asked.
	claims      []*storev1.ShellRunClaimed
	claimsFail  string
	claimsAsked [][]string
}

func (f *fakeStore) GetShellRunClaims(_ context.Context, request *connect.Request[storev1.GetShellRunClaimsRequest]) (*connect.Response[storev1.GetShellRunClaimsResponse], error) {
	f.claimsAsked = append(f.claimsAsked, request.Msg.GetVendorTaskIds())
	if f.claimsFail != "" {
		return connect.NewResponse(&storev1.GetShellRunClaimsResponse{
			Result: &storev1.GetShellRunClaimsResponse_Failure{Failure: &storev1.GetShellRunClaimsFailure{
				Detail: f.claimsFail,
				Kind:   &storev1.GetShellRunClaimsFailure_StorageFailure{StorageFailure: &storev1.GetShellRunClaimsStorageFailure{}},
			}},
		}), nil
	}
	asked := map[string]bool{}
	for _, id := range request.Msg.GetVendorTaskIds() {
		asked[id] = true
	}
	var claims []*storev1.ShellRunClaimed
	for _, claim := range f.claims {
		if asked[claim.GetClaim().GetVendorTaskId()] {
			claims = append(claims, claim)
		}
	}
	return connect.NewResponse(&storev1.GetShellRunClaimsResponse{
		Result: &storev1.GetShellRunClaimsResponse_Success{Success: &storev1.GetShellRunClaimsSuccess{Claims: claims}},
	}), nil
}

func (f *fakeStore) GetSidecarCursors(ctx context.Context, _ *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	f.cursorsCalls++
	if f.cursorsWedged {
		if f.cursorsEntered != nil {
			close(f.cursorsEntered)
			f.cursorsEntered = nil
		}
		<-ctx.Done()
		return nil, ctx.Err()
	}
	if f.cursorsFail != "" {
		return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
			Result: &storev1.GetSidecarCursorsResponse_Failure{
				Failure: &storev1.GetSidecarCursorsFailure{Detail: f.cursorsFail},
			},
		}), nil
	}
	return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{
			Success: &storev1.GetSidecarCursorsSuccess{Cursors: f.cursors},
		},
	}), nil
}

func (f *fakeStore) WriteBatch(ctx context.Context, request *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	f.writeCalls++
	f.writes = append(f.writes, request.Msg.GetBatch())
	f.shapes = append(f.shapes, request.Msg.GetShapes())
	if f.writeWedged {
		if f.entered != nil {
			close(f.entered)
			f.entered = nil
		}
		<-ctx.Done()
		return nil, ctx.Err()
	}
	if f.writeFail != "" {
		failure := &storev1.WriteBatchFailure{Detail: f.writeFail}
		switch {
		case f.writeKindless:
			// Deliberately illegal on this contract, so the sidecar's own
			// handling of a store that violates it can be exercised.
		case f.writeInvalidField != "":
			failure.Kind = &storev1.WriteBatchFailure_InvalidRequest{
				InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: f.writeInvalidField},
			}
		default:
			failure.Kind = &storev1.WriteBatchFailure_StorageFailure{
				StorageFailure: &storev1.WriteBatchStorageFailure{},
			}
		}
		return connect.NewResponse(&storev1.WriteBatchResponse{
			Result: &storev1.WriteBatchResponse_Failure{Failure: failure},
		}), nil
	}
	return connect.NewResponse(&storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}), nil
}

type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

// harness is one sidecar wired to a fake store over a real unix socket, with a
// fake clock. Nothing here sleeps: the cycle is driven by calling its steps.
type harness struct {
	sc        *sidecar
	store     *fakeStore
	logs      *[]string
	base      string
	rootA     string
	spool     string
	workspace string
	// state is the agent-repl state root the shim writes its identity records
	// under. It exists for every harness so a subject can drop a record into it
	// without rebuilding the sidecar; an empty one resolves nothing, which is
	// what every other subject sees.
	state string
	// lockDir is where the shims' workspace locks live; a session the harness
	// ACTIVATES holds a real flock there, exactly as a live shim does.
	lockDir string
	// held is every workspace lock the harness holds, by workspace key.
	held map[string]*os.File
	// recorded maps every vendor session id an identity record the harness
	// wrote names — an agent-id.json's original or a link's rotated id — to
	// that record's workspace key.
	recorded map[string]string
	clock    time.Time
	socket   string
}

func shortSocket(t testing.TB) string {
	t.Helper()
	raw := make([]byte, 4)
	if _, err := rand.Read(raw); err != nil {
		t.Fatalf("generating socket suffix: %v", err)
	}
	path := filepath.Join(os.TempDir(), "ar-"+hex.EncodeToString(raw)+".sock")
	t.Cleanup(func() {
		if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
			t.Errorf("removing socket %s: %v", path, err)
		}
	})
	return path
}

// newHarness builds the sidecar. store == nil leaves the socket unbound, which
// is what an unreachable store looks like.
func newHarness(t testing.TB, store *fakeStore) *harness {
	t.Helper()
	return newHarnessAtLevel(t, store, sharedlogging.LevelDebug)
}

// newHarnessAtLevel is newHarness with its log threshold chosen: the benchmark
// runs at production's info so it measures the cycle rather than the
// formatting of debug records nothing would keep.
func newHarnessAtLevel(t testing.TB, store *fakeStore, level sharedlogging.Level) *harness {
	t.Helper()
	base := t.TempDir()
	h := &harness{
		store:     store,
		base:      base,
		rootA:     filepath.Join(base, "config-a"),
		spool:     filepath.Join(base, "spool"),
		workspace: filepath.Join(base, "workspace"),
		state:     filepath.Join(base, "state"),
		lockDir:   filepath.Join(base, "lock"),
		held:      map[string]*os.File{},
		recorded:  map[string]string{},
		clock:     time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC),
		socket:    shortSocket(t),
	}
	for _, dir := range []string{h.rootA, h.spool, h.state, h.lockDir} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("creating %s: %v", dir, err)
		}
	}
	if store != nil {
		h.serve(t, store)
	}
	var logs []string
	h.logs = &logs
	log := logging.NewAtLevel(sliceWriter{lines: &logs}, io.Discard, level).With(logging.Context{Component: "sidecar-test"})
	h.sc = newSidecar(Options{
		StoreSocket:    h.socket,
		StateDir:       h.state,
		LockDir:        h.lockDir,
		ConfigRoots:    []string{h.rootA},
		SpoolRoot:      h.spool,
		PollInterval:   time.Second,
		RescanInterval: 30 * time.Second,
	}, log)
	h.sc.now = func() time.Time { return h.clock }
	h.sc.jitter = func(d time.Duration) time.Duration { return d }
	h.sc.bootTimeMs = func() int64 { return 0 }
	t.Cleanup(func() {
		for key := range h.held {
			h.release(t, key)
		}
	})
	// THE SESSION EVERY SPAWN FIXTURE NAMES IS A LIVE agent-repl SESSION, so a
	// spool its launch claims belongs to an active workspace.
	h.activate(t, "session-1")
	return h
}

// workspaceKeyOf is the fixture's workspace key for a session: eight hex digits,
// the shape the shim's md5(cwd)[:8] has. Nothing reads meaning into it; it only
// has to join the identity record to its lock by name.
func workspaceKeyOf(session string) string {
	sum := md5.Sum([]byte(session))
	return hex.EncodeToString(sum[:])[:8]
}

// activate makes session a LIVE agent-repl session: the shim's agent-id.json
// names it as its workspace's conversation, and the workspace's kernel lock is
// held, exactly as a shim inside StartSession holds it. It answers the key.
//
// A session an identity record already names — the original a subject minted,
// or a rotation it linked — is not given a second record: its workspace's lock
// is held, which is what makes it live.
func (h *harness) activate(t testing.TB, session string) string {
	t.Helper()
	key, recorded := h.recorded[session]
	if !recorded {
		key = workspaceKeyOf(session)
		h.writeAgentID(t, key, session)
	}
	h.hold(t, key)
	return key
}

// hold takes a workspace's kernel lock, as a live shim holds it, unless the
// harness already does.
func (h *harness) hold(t testing.TB, key string) {
	t.Helper()
	if _, ok := h.held[key]; ok {
		return
	}
	path := livelock.Path(h.lockDir, key)
	file, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		t.Fatalf("creating the workspace lock %s: %v", path, err)
	}
	if err := syscall.Flock(int(file.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("taking the workspace lock %s: %v", path, err)
	}
	h.held[key] = file
}

// release drops a workspace's lock, which is what a shim's death looks like to
// every other process.
func (h *harness) release(t testing.TB, key string) {
	t.Helper()
	file, ok := h.held[key]
	if !ok {
		t.Fatalf("workspace %s's lock is not held", key)
	}
	delete(h.held, key)
	if err := file.Close(); err != nil {
		t.Fatalf("releasing workspace %s's lock: %v", key, err)
	}
}

// writeAgentID writes a workspace's agent-id.json by tmp-and-rename, the way the
// shim's engine/identity.ts does, so the record directory's mtime moves.
func (h *harness) writeAgentID(t testing.TB, key, original string) {
	t.Helper()
	dir := filepath.Join(h.state, "shim", key)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
	record := `{"original_vendor_session_id":"` + original + `","workspace_key":"` + key + `","minted_at_ms":1735689600000}` + "\n"
	temporary := filepath.Join(dir, "agent-id.json.tmp")
	if err := os.WriteFile(temporary, []byte(record), 0o644); err != nil {
		t.Fatalf("writing %s: %v", temporary, err)
	}
	if err := os.Rename(temporary, filepath.Join(dir, "agent-id.json")); err != nil {
		t.Fatalf("renaming %s into place: %v", temporary, err)
	}
	h.recorded[original] = key
}

func (h *harness) serve(t testing.TB, store *fakeStore) {
	t.Helper()
	listener, err := net.Listen("unix", h.socket)
	if err != nil {
		t.Fatalf("listening on %s: %v", h.socket, err)
	}
	mux := http.NewServeMux()
	mux.Handle(storev1connect.NewShimStoreHandler(store))
	server := &http.Server{Handler: mux}
	served := make(chan struct{})
	go func() {
		defer close(served)
		_ = server.Serve(listener)
	}()
	t.Cleanup(func() {
		if err := server.Close(); err != nil {
			t.Errorf("closing the fake store's server: %v", err)
		}
		<-served
	})
}

// advance moves the fake clock.
func (h *harness) advance(d time.Duration) { h.clock = h.clock.Add(d) }

// transcript writes a LIVE agent-repl session's transcript and returns its
// resolved path; the session is activated first.
func (h *harness) transcript(t testing.TB, session string, lines ...string) string {
	t.Helper()
	h.activate(t, session)
	return h.inactiveTranscript(t, session, lines...)
}

// inactiveTranscript writes a transcript WITHOUT making its session live: an
// external session, a closed workspace's, or a rotation whose link has not
// landed. It returns the resolved path.
func (h *harness) inactiveTranscript(t testing.TB, session string, lines ...string) string {
	t.Helper()
	path := filepath.Join(h.rootA, "projects", "proj", session+".jsonl")
	h.write(t, path, strings.Join(lines, "\n")+"\n")
	workspaceID, err := sharedlogging.WorkspaceID(h.workspace)
	if err != nil {
		t.Fatalf("derive fixture workspace id: %v", err)
	}
	h.sc.workspaceBySession[discover.Normalize(h.rootA)+"\x00proj\x00"+session] = workspaceAttribution{dir: h.workspace, id: workspaceID}
	return normalized(path)
}

// spoolFile writes a task spool and returns its resolved path.
func (h *harness) spoolFile(t *testing.T, taskID, content string) string {
	t.Helper()
	path := filepath.Join(h.spool, "claude-501", "proj", "runtime-sess", "tasks", taskID+".output")
	h.write(t, path, content)
	return normalized(path)
}

func (h *harness) write(t testing.TB, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("creating %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("writing %s: %v", path, err)
	}
	// The LOST policy seeds a run's activity clock from the file's mtime, so a
	// fixture written "now" must be stamped on the harness clock or the fake
	// clock and the file disagree about when the file last grew.
	if err := os.Chtimes(path, h.clock, h.clock); err != nil {
		t.Fatalf("stamping %s: %v", path, err)
	}
}

func (h *harness) logText() string { return strings.Join(*h.logs, "\n") }

const (
	promptLine    = `{"type":"user","message":{"role":"user","content":"do the thing"}}`
	assistantLine = `{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"ok"}]}}`
)

// normalized is discover.Normalize, spelled here so tests read in the same
// resolved spelling the sidecar keys everything by.
func normalized(path string) string { return discover.Normalize(path) }

// danglingAgentSpool writes an a* task spool that is a LINK to its subagent's
// transcript before that transcript exists — the window in which discovery
// still sees the spool under its own spelling — and returns its path.
func (h *harness) danglingAgentSpool(t *testing.T, taskID string) string {
	t.Helper()
	dir := filepath.Join(h.spool, "claude-501", "proj", "runtime-sess", "tasks")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
	transcript := filepath.Join(h.rootA, "projects", "proj", "sess-1", "subagents", "agent-"+taskID+".jsonl")
	link := filepath.Join(dir, taskID+".output")
	if err := os.Symlink(transcript, link); err != nil {
		t.Fatalf("linking %s: %v", link, err)
	}
	return normalized(link)
}
