package pty

import (
	"bufio"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"
)

// readLineWithin reads one line from r, failing the test if none arrives
// before the deadline. A PTY read blocks forever when the child never
// writes, so every read in this file is fenced.
func readLineWithin(t *testing.T, r *bufio.Reader, d time.Duration) string {
	t.Helper()
	type result struct {
		line string
		err  error
	}
	ch := make(chan result, 1)
	go func() {
		line, err := r.ReadString('\n')
		ch <- result{line, err}
	}()
	select {
	case got := <-ch:
		if got.err != nil {
			t.Fatalf("read from pty: %v", got.err)
		}
		return strings.TrimRight(got.line, "\r\n")
	case <-time.After(d):
		t.Fatalf("no line from pty within %s", d)
		return ""
	}
}

func TestStart_ChildSeesATerminal(t *testing.T) {
	// Arrange: `test -t 0` is true only when stdin is a terminal, which is
	// the entire point of this package — a pipe would print "no".
	cmd := exec.Command("sh", "-c", "test -t 0 && echo tty || echo notty")

	// Act
	master, err := Start(cmd)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	defer func() { _ = master.Close() }()
	defer func() { _ = cmd.Wait() }()

	// Assert
	if got := readLineWithin(t, bufio.NewReader(master), 5*time.Second); got != "tty" {
		t.Errorf("child stdin: got %q, want %q", got, "tty")
	}
}

func TestStart_MasterCarriesChildOutput(t *testing.T) {
	// Arrange
	cmd := exec.Command("sh", "-c", "echo hello-from-pty")

	// Act
	master, err := Start(cmd)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	defer func() { _ = master.Close() }()
	defer func() { _ = cmd.Wait() }()

	// Assert
	if got := readLineWithin(t, bufio.NewReader(master), 5*time.Second); got != "hello-from-pty" {
		t.Errorf("child stdout: got %q, want %q", got, "hello-from-pty")
	}
}

func TestStart_MasterWriteReachesChildStdin(t *testing.T) {
	// Arrange: the child echoes back whatever line it is fed, which only
	// works if the master's write side is the child's stdin.
	cmd := exec.Command("sh", "-c", "read line; echo got:$line")

	master, err := Start(cmd)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	defer func() { _ = master.Close() }()
	defer func() { _ = cmd.Wait() }()
	r := bufio.NewReader(master)

	// Act
	if _, err := master.Write([]byte("ping\n")); err != nil {
		t.Fatalf("write to pty: %v", err)
	}

	// Assert — the tty echoes the input line back before the child's own
	// output, so skip the echo and read the child's reply.
	if echo := readLineWithin(t, r, 5*time.Second); echo != "ping" {
		t.Fatalf("tty echo: got %q, want %q", echo, "ping")
	}
	if got := readLineWithin(t, r, 5*time.Second); got != "got:ping" {
		t.Errorf("child reply: got %q, want %q", got, "got:ping")
	}
}

func TestSetsize_ChildSeesTheColumns(t *testing.T) {
	// Arrange: 400 columns is the size that keeps the ~350-char OAuth URL
	// on ONE line. `tput cols` reads the child's real terminal width, so
	// this asserts the ioctl actually landed on the child.
	cmd := exec.Command("sh", "-c", "tput cols")
	master, err := Start(cmd)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	defer func() { _ = master.Close() }()
	defer func() { _ = cmd.Wait() }()

	// Act
	if err := Setsize(master, 60, 400); err != nil {
		t.Fatalf("Setsize: %v", err)
	}

	// Assert
	if got := readLineWithin(t, bufio.NewReader(master), 5*time.Second); got != "400" {
		t.Errorf("child columns: got %q, want %q", got, "400")
	}
}

func TestStart_InheritedCOLUMNSDoesNotOverrideThePTY(t *testing.T) {
	// Arrange: a parent whose environment carries a stale COLUMNS — the
	// daemon's own case whenever it is launched from a terminal. `tput
	// cols` prefers COLUMNS OVER the winsize ioctl, so before the child
	// env was scrubbed this leaked straight through and re-wrapped the
	// OAuth URL the 400-column pty exists to keep on one line.
	t.Setenv("COLUMNS", "134")
	t.Setenv("LINES", "40")
	cmd := exec.Command("sh", "-c", "tput cols")
	master, err := Start(cmd)
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	defer func() { _ = master.Close() }()
	defer func() { _ = cmd.Wait() }()

	// Act
	if err := Setsize(master, 60, 400); err != nil {
		t.Fatalf("Setsize: %v", err)
	}

	// Assert: the pty wins, not the inherited environment.
	if got := readLineWithin(t, bufio.NewReader(master), 5*time.Second); got != "400" {
		t.Errorf("child columns: got %q, want %q (inherited COLUMNS beat the pty)", got, "400")
	}
}

func TestSetsize_ErrorsOnANonTerminal(t *testing.T) {
	// Arrange: /dev/null is a character device but NOT a terminal, so the
	// winsize ioctl must fail loudly rather than silently no-op.
	null, err := os.OpenFile(os.DevNull, os.O_RDWR, 0)
	if err != nil {
		t.Fatalf("open %s: %v", os.DevNull, err)
	}
	defer func() { _ = null.Close() }()

	// Act
	err = Setsize(null, 24, 80)

	// Assert
	if err == nil {
		t.Errorf("Setsize on %s: got nil error, want a failure", os.DevNull)
	}
}
