// Package h2ctest serves a Connect handler over h2c on a private unix socket
// for a client's test: the shape every service the daemon dials by its
// socket is served in (h2cclient.UnixClient).
package h2ctest

import (
	"net"
	"net/http"
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// ServeUnix serves HANDLER at PATH on a fresh unix socket and answers the
// socket's path. The socket's directory is short (a unix socket's path is
// capped near 104 bytes) and removed, with the server, when the test ends.
func ServeUnix(t *testing.T, path string, handler http.Handler) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "h2c")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	sock := filepath.Join(dir, "s.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	srv := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Close() })
	return sock
}
