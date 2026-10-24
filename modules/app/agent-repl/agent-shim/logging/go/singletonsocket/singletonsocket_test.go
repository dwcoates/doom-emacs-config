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

// failingCloseListener is a net.Listener whose Close fails, which is the one
// input abandon's second fault needs and the kernel will not produce on
// demand.
type failingCloseListener struct {
	net.Listener
	closeErr error
}

func (l failingCloseListener) Close() error { return l.closeErr }

func TestAbandonAnswersTheCauseWhenTheCloseSucceeds(t *testing.T) {
	// Arrange
	ln, err := net.Listen("unix", filepath.Join(shortDir(t), "a.sock"))
	if err != nil {
		t.Fatal(err)
	}
	cause := &Error{Step: StepRestrict, Path: "/a.sock", Err: errors.New("chmod failed")}

	// Act
	got := abandon(ln, cause)

	// Assert
	if got != error(cause) {
		t.Fatalf("abandon = %v, want exactly the cause", got)
	}
}

func TestAbandonJoinsAFailedCloseOntoTheCause(t *testing.T) {
	// Arrange
	cause := &Error{Step: StepRestrict, Path: "/a.sock", Err: errors.New("chmod failed")}
	closeErr := errors.New("the close failed")

	// Act
	got := abandon(failingCloseListener{closeErr: closeErr}, cause)

	// Assert
	var e *Error
	if !errors.As(got, &e) || e.Step != StepRestrict || !errors.Is(got, closeErr) {
		t.Fatalf("abandon = %v, want the restrict step and the close failure", got)
	}
}

func TestListenReclaimingTakesALiveSocketWithoutProbing(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "s.sock")
	live, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	live.(*net.UnixListener).SetUnlinkOnClose(false)
	defer live.Close()

	// Act
	ln, res, err := ListenReclaiming(path)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	if !res.Reclaimed {
		t.Error("the existing socket was not reported as reclaimed")
	}
}

func TestListenReclaimingRefusesANonSocket(t *testing.T) {
	// Arrange
	path := filepath.Join(shortDir(t), "s.sock")
	if err := os.WriteFile(path, []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := ListenReclaiming(path)

	// Assert
	var e *Error
	if !errors.As(err, &e) || e.Step != StepNotASocket {
		t.Errorf("err = %v, want StepNotASocket", err)
	}
}
