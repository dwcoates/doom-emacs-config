package harness

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// signalWriter closes wrote on its first write and keeps what was written.
type signalWriter struct {
	wrote chan struct{}
	text  chan string
}

func newSignalWriter() *signalWriter {
	return &signalWriter{wrote: make(chan struct{}), text: make(chan string, 1)}
}

func (w *signalWriter) Write(p []byte) (int, error) {
	w.text <- string(p)
	close(w.wrote)
	return len(p), nil
}

func TestHoldHostTakesAFreeLockAndRecordsItsHolder(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "suite.lock")

	// Act
	f, err := holdHost(path, io.Discard)

	// Assert
	if err != nil {
		t.Fatalf("holdHost: %v", err)
	}
	defer f.Close()
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the lock: %v", err)
	}
	if want := fmt.Sprintf("pid %d since ", os.Getpid()); !strings.HasPrefix(string(got), want) {
		t.Fatalf("lock records %q, want it to start %q", got, want)
	}
}

func TestHoldHostWaitsForTheHolderAndNamesIt(t *testing.T) {
	// Arrange: one suite holds the host.
	path := filepath.Join(t.TempDir(), "suite.lock")
	first, err := holdHost(path, io.Discard)
	if err != nil {
		t.Fatalf("holdHost (first): %v", err)
	}
	w := newSignalWriter()
	held := make(chan error, 1)

	// Act: a second suite asks, says whom it waits for, and is admitted once
	// the first ends.
	go func() {
		second, err := holdHost(path, w)
		if err == nil {
			second.Close()
		}
		held <- err
	}()
	<-w.wrote
	select {
	case err := <-held:
		t.Fatalf("the second suite was admitted while the first held the host: %v", err)
	default:
	}
	first.Close()

	// Assert
	if err := <-held; err != nil {
		t.Fatalf("holdHost (second): %v", err)
	}
	if line := <-w.text; !strings.Contains(line, fmt.Sprintf("pid %d", os.Getpid())) || !strings.Contains(line, "waiting") {
		t.Fatalf("the waiting line %q does not name the holder", line)
	}
}

func TestHoldHostRefusesALockItCannotOpen(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "no-such-dir", "suite.lock")

	// Act
	f, err := holdHost(path, io.Discard)

	// Assert
	if err == nil {
		f.Close()
		t.Fatal("holdHost succeeded on a path whose directory does not exist")
	}
	if !strings.Contains(err.Error(), "open the host suite lock") {
		t.Fatalf("holdHost error = %v, want it to name the open", err)
	}
}
