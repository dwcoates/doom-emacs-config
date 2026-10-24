package singletonsocket

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// shortDir is a temp dir short enough for a unix socket path.
func shortDir(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "ss")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

func TestListenBindsAFreshPathOwnerOnly(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "sub", "s.sock")

	// Act
	ln, res, err := Listen(path, time.Second)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	if res.Reclaimed || res.Path != path {
		t.Errorf("result = %+v, want the path bound without a reclaim", res)
	}
	info, err := os.Stat(path)
	if err != nil || info.Mode().Perm() != 0o600 {
		t.Errorf("mode = %v (%v), want 0600", info.Mode().Perm(), err)
	}
}

func TestListenRefusesALiveListener(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "s.sock")
	first, _, err := Listen(path, time.Second)
	if err != nil {
		t.Fatal(err)
	}
	defer first.Close()

	// Act
	_, _, err = Listen(path, time.Second)

	// Assert
	var e *Error
	if !errors.As(err, &e) || e.Step != StepOccupied || !errors.Is(err, ErrOccupied) {
		t.Errorf("err = %v, want StepOccupied", err)
	}
}

func TestListenReclaimsAStaleSocket(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "s.sock")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	ln.(*net.UnixListener).SetUnlinkOnClose(false)
	if err := ln.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	again, res, err := Listen(path, time.Second)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	defer again.Close()
	if !res.Reclaimed {
		t.Error("a stale socket was not reported as reclaimed")
	}
}

func TestListenRefusesANonSocket(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "s.sock")
	if err := os.WriteFile(path, []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := Listen(path, time.Second)

	// Assert
	var e *Error
	if !errors.As(err, &e) || e.Step != StepNotASocket || e.Mode.IsDir() || !errors.Is(err, ErrNotASocket) {
		t.Errorf("err = %v, want StepNotASocket", err)
	}
	if _, statErr := os.Stat(path); statErr != nil {
		t.Errorf("the non-socket was touched: %v", statErr)
	}
}

func TestListenReportsAnUncreatableDirectory(t *testing.T) {
	// Arrange
	blocker := filepath.Join(shortDir(t), "file")
	if err := os.WriteFile(blocker, nil, 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := Listen(filepath.Join(blocker, "s.sock"), time.Second)

	// Assert
	var e *Error
	if !errors.As(err, &e) || e.Step != StepPrepareDir {
		t.Errorf("err = %v, want StepPrepareDir", err)
	}
}
