package h2ctest

import (
	"context"
	"io"
	"net"
	"net/http"
	"testing"
)

func TestServeUnixServesTheHandlerAtItsPath(t *testing.T) {
	// Arrange
	handler := http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { _, _ = io.WriteString(w, "served") })

	// Act
	sock := ServeUnix(t, "/x/", handler)

	// Assert
	client := &http.Client{Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
		return (&net.Dialer{}).DialContext(ctx, "unix", sock)
	}}}
	resp, err := client.Get("http://unix/x/y")
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if string(body) != "served" {
		t.Fatalf("body = %q, want the handler's answer", body)
	}
}
