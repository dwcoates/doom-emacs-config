package h2cclient

import (
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

func serveHello(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "uh")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	sock := filepath.Join(dir, "s.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	srv := &http.Server{Handler: h2c.NewHandler(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, r.Proto)
	}), &http2.Server{})}
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Close() })
	return sock
}

func TestClientSpeaksH2CToTheSocketWhateverTheAuthority(t *testing.T) {
	// Arrange
	sock := serveHello(t)

	// Act
	resp, err := UnixClient(sock).Get("http://any.authority/path")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if string(body) != "HTTP/2.0" {
		t.Errorf("served over %q, want HTTP/2.0", body)
	}
}

func TestTransportHandsEachDialedConnectionToTrack(t *testing.T) {
	// Arrange
	sock := serveHello(t)
	tracked := 0
	client := &http.Client{Transport: UnixTransport(sock, func(c net.Conn) net.Conn { tracked++; return c })}

	// Act
	resp, err := client.Get("http://x/")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	_ = resp.Body.Close()
	if tracked != 1 {
		t.Errorf("tracked = %d, want the one dialed connection", tracked)
	}
}

func TestADialFailureIsAnError(t *testing.T) {
	// Arrange
	client := UnixClient(filepath.Join(t.TempDir(), "absent.sock"))

	// Act
	_, err := client.Get("http://x/")

	// Assert
	if err == nil {
		t.Fatal("a request to an absent socket succeeded")
	}
}

// TestNoOtherDaemonPackageBuildsAnH2CTransport pins the extraction: every
// daemon client of an h2c Connect peer goes through this package, so the
// dialing can never drift between them.
func TestNoOtherDaemonPackageBuildsAnH2CTransport(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	found := []string{}

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || strings.Contains(path, "internal/h2cclient/") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "http2.Transport{") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(found) != 0 {
		t.Errorf("these files build their own h2c transport; use h2cclient: %v", found)
	}
}

func TestTransportDialsTheRequestsOwnAddress(t *testing.T) {
	// Arrange
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	srv := &http.Server{Handler: h2c.NewHandler(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, r.Proto)
	}), &http2.Server{})}
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Close() })

	// Act
	resp, err := Client().Get("http://" + ln.Addr().String() + "/")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if string(body) != "HTTP/2.0" {
		t.Errorf("served over %q, want HTTP/2.0", body)
	}
}
