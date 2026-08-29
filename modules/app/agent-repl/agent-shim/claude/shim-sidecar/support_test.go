package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
)

func TestMain(m *testing.M) {
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	os.Exit(m.Run())
}

// fakeStore answers the sidecar's two verbs and records what it was asked.
type fakeStore struct {
	storev1connect.UnimplementedShimStoreHandler

	cursors      []*storev1.CursorState
	cursorsFail  string // non-empty: answer the failure arm
	cursorsCalls int

	writeFail  string // non-empty: answer the failure arm
	writes     []*storev1.EntryBatch
	writeCalls int
}

func (f *fakeStore) GetSidecarCursors(_ context.Context, _ *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	f.cursorsCalls++
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

func (f *fakeStore) WriteBatch(_ context.Context, request *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	f.writeCalls++
	f.writes = append(f.writes, request.Msg.GetBatch())
	if f.writeFail != "" {
		return connect.NewResponse(&storev1.WriteBatchResponse{
			Result: &storev1.WriteBatchResponse_Failure{
				Failure: &storev1.WriteBatchFailure{Detail: f.writeFail},
			},
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
	sc      *sidecar
	store   *fakeStore
	logs    *[]string
	base    string
	rootA   string
	spool   string
	clock   time.Time
	socket  string
	started bool
}

func shortSocket(t *testing.T) string {
	t.Helper()
	raw := make([]byte, 4)
	if _, err := rand.Read(raw); err != nil {
		t.Fatalf("generating socket suffix: %v", err)
	}
	path := filepath.Join(os.TempDir(), "ar-"+hex.EncodeToString(raw)+".sock")
	t.Cleanup(func() { os.Remove(path) })
	return path
}

// newHarness builds the sidecar. store == nil leaves the socket unbound, which
// is what an unreachable store looks like.
func newHarness(t *testing.T, store *fakeStore) *harness {
	t.Helper()
	base := t.TempDir()
	h := &harness{
		store:  store,
		base:   base,
		rootA:  filepath.Join(base, "config-a"),
		spool:  filepath.Join(base, "spool"),
		clock:  time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC),
		socket: shortSocket(t),
	}
	for _, dir := range []string{h.rootA, h.spool} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("creating %s: %v", dir, err)
		}
	}
	if store != nil {
		h.serve(t, store)
	}
	var logs []string
	h.logs = &logs
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "sidecar-test"})
	h.sc = newSidecar(Options{
		StoreSocket:    h.socket,
		ConfigRoots:    []string{h.rootA},
		SpoolRoot:      h.spool,
		PollInterval:   time.Second,
		RescanInterval: 30 * time.Second,
	}, log)
	h.sc.now = func() time.Time { return h.clock }
	h.sc.jitter = func(d time.Duration) time.Duration { return d }
	h.sc.bootTimeMs = func() int64 { return 0 }
	return h
}

func (h *harness) serve(t *testing.T, store *fakeStore) {
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
		server.Close()
		<-served
	})
}

// advance moves the fake clock.
func (h *harness) advance(d time.Duration) { h.clock = h.clock.Add(d) }

// transcript writes a session transcript and returns its resolved path.
func (h *harness) transcript(t *testing.T, session string, lines ...string) string {
	t.Helper()
	path := filepath.Join(h.rootA, "projects", "proj", session+".jsonl")
	h.write(t, path, strings.Join(lines, "\n")+"\n")
	return normalized(path)
}

// spoolFile writes a task spool and returns its resolved path.
func (h *harness) spoolFile(t *testing.T, taskID, content string) string {
	t.Helper()
	path := filepath.Join(h.spool, "claude-501", "proj", "runtime-sess", "tasks", taskID+".output")
	h.write(t, path, content)
	return normalized(path)
}

func (h *harness) write(t *testing.T, path, content string) {
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
