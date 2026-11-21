package dlog

import (
	"os"
	"path/filepath"
	"testing"
)

// stdioWith writes BODY as the stdio log of a fresh logs directory.
func stdioWith(t *testing.T, body string) string {
	t.Helper()
	dir := t.TempDir()
	if body != "" {
		if err := os.WriteFile(filepath.Join(dir, StdioLogName), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

const deadStack = `{"timestamp":"t1","level":"info","message":"booted","pid":61885}
{"timestamp":"t2","level":"error","message":"a topbar record arrived","pid":61885}
panic: topbar: a frame for workspace "886cb100626540ab", whose directory was never bound

goroutine 9724 [running]:
agent-repld/internal/resolve/topbar.(*resolver).logOf(0x14000c183c0?, {0x1400cfd57c0, 0x10}, 0x14001198608?)
	agent-repld/internal/resolve/topbar/resolver.go:153 +0x1a8
agent-repld/internal/resolve/topbar.(*resolver).mutate(0x14000a1d000)
	agent-repld/internal/resolve/topbar/resolver.go:188 +0x280
`

func TestLastPanicReadsThePredecessorsPanic(t *testing.T) {
	tests := []struct {
		name      string
		body      string
		pid       int
		wantFound bool
		want      Panic
	}{
		{name: "the predecessor's panic and its first agent-repld frame", body: deadStack, pid: 61885, wantFound: true,
			want: Panic{Message: `panic: topbar: a frame for workspace "886cb100626540ab", whose directory was never bound`,
				Site: "agent-repld/internal/resolve/topbar.(*resolver).logOf at agent-repld/internal/resolve/topbar/resolver.go:153"}},
		{name: "an older daemon's panic is not the predecessor's", body: deadStack + `{"timestamp":"t3","message":"booted","pid":70000}` + "\n", pid: 70000},
		{name: "a panic another process's record precedes is not the predecessor's", body: deadStack, pid: 6188},
		{name: "a fatal runtime error is a panic", body: `{"message":"x","pid":7}` + "\nfatal error: concurrent map writes\n", pid: 7, wantFound: true,
			want: Panic{Message: "fatal error: concurrent map writes"}},
		{name: "a log with no panic holds none", body: `{"message":"x","pid":7}` + "\n", pid: 7},
		{name: "no log holds none", pid: 7},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := stdioWith(t, tt.body)

			// Act
			got, found, err := LastPanic(dir, tt.pid)

			// Assert
			if err != nil || found != tt.wantFound || got != tt.want {
				t.Fatalf("LastPanic = %+v, %v, %v; want %+v, %v", got, found, err, tt.want, tt.wantFound)
			}
		})
	}
}

func TestLastPanicAnswersALogItCannotRead(t *testing.T) {
	// Arrange: the stdio log's path is a directory.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, StdioLogName), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	_, found, err := LastPanic(dir, 7)

	// Assert
	if err == nil || found {
		t.Fatalf("LastPanic = found %v, %v; want the read's failure", found, err)
	}
}
