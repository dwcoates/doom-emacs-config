package storeclient

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// fakeStore is an in-process store.v1 handler. Only the two verbs the sidecar
// owns are answerable; every other rpc is a test defect and says so.
type fakeStore struct {
	storev1connect.UnimplementedShimStoreHandler

	cursors        *storev1.GetSidecarCursorsResponse
	cursorsErr     error
	lastCursorsReq *storev1.GetSidecarCursorsRequest
	write          *storev1.WriteBatchResponse
	writeErr       error
	lastWrite      *storev1.WriteBatchRequest
	writeCallCount int
	claims         *storev1.GetShellRunClaimsResponse
	claimsErr      error
	lastClaimsReq  *storev1.GetShellRunClaimsRequest
}

func (f *fakeStore) GetShellRunClaims(_ context.Context, request *connect.Request[storev1.GetShellRunClaimsRequest]) (*connect.Response[storev1.GetShellRunClaimsResponse], error) {
	f.lastClaimsReq = request.Msg
	if f.claimsErr != nil {
		return nil, f.claimsErr
	}
	return connect.NewResponse(f.claims), nil
}

func (f *fakeStore) GetSidecarCursors(_ context.Context, request *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	f.lastCursorsReq = request.Msg
	if f.cursorsErr != nil {
		return nil, f.cursorsErr
	}
	return connect.NewResponse(f.cursors), nil
}

func (f *fakeStore) WriteBatch(_ context.Context, request *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	f.writeCallCount++
	f.lastWrite = request.Msg
	if f.writeErr != nil {
		return nil, f.writeErr
	}
	return connect.NewResponse(f.write), nil
}

// shortSocket builds a UDS path short enough for macOS's ~104-byte sun_path
// limit; t.TempDir() paths are far too long to bind.
func shortSocket(t *testing.T) string {
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

// serve binds the fake store on a short unix socket and returns a client for it.
// The listener is closed through t.Cleanup, which is what ends the goroutine.
func serve(t *testing.T, store *fakeStore) *Client {
	t.Helper()
	socket := shortSocket(t)
	listener, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listening on %s: %v", socket, err)
	}
	mux := http.NewServeMux()
	mux.Handle(storev1connect.NewShimStoreHandler(store))
	server := &http.Server{Handler: mux}
	served := make(chan struct{})
	go func() {
		defer close(served)
		if err := server.Serve(listener); err != nil && err != http.ErrServerClosed {
			return
		}
	}()
	t.Cleanup(func() {
		if err := server.Close(); err != nil {
			t.Errorf("closing the fake store's server: %v", err)
		}
		<-served
	})
	return New(socket, testLogger(t))
}

// clientTo builds a client aimed at a socket nothing is listening on.
func clientTo(t *testing.T, socket string) *Client {
	t.Helper()
	return New(socket, testLogger(t))
}

func testLogger(t *testing.T) *logging.Bound {
	t.Helper()
	return logging.New(io.Discard, io.Discard).With(logging.Context{Component: "storeclient-test"})
}

func cursor(fileID, path string, offset int64) *storev1.CursorState {
	return &storev1.CursorState{FileId: fileID, Path: path, Offset: offset}
}

func ctx() context.Context { return context.Background() }
