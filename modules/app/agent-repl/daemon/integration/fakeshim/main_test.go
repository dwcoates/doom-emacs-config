package main

import (
	"io"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"testing"
)

func TestReportReadyWritesOneByteAndCloses(t *testing.T) {
	// Arrange
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("Pipe: %v", err)
	}
	defer r.Close()
	fd, err := syscall.Dup(int(w.Fd()))
	if err != nil {
		t.Fatalf("Dup: %v", err)
	}
	w.Close()

	// Act
	err = reportReady(fd)

	// Assert: one byte, then EOF -- the write end is closed.
	if err != nil {
		t.Fatalf("reportReady = %v", err)
	}
	got, err := io.ReadAll(r)
	if err != nil || len(got) != 1 {
		t.Fatalf("read %v (%v), want exactly one byte then EOF", got, err)
	}
}

func TestReportReadyToAGoneDaemonSaysSo(t *testing.T) {
	// Arrange: the read end is closed, as a dead daemon's is.
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("Pipe: %v", err)
	}
	r.Close()
	fd, err := syscall.Dup(int(w.Fd()))
	if err != nil {
		t.Fatalf("Dup: %v", err)
	}
	w.Close()
	signal.Ignore(syscall.SIGPIPE)

	// Act
	err = reportReady(fd)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "report ready") {
		t.Fatalf("reportReady = %v, want the failed report named", err)
	}
}
