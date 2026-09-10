package harness

import (
	"bufio"
	"context"
	"encoding/json"
	"net"
	"path/filepath"
	"testing"
	"time"
)

func TestShimInfoFlagFindsAValue(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--listen", "/s.sock", "--log-fd", "3", "--fake"}}

	// Act
	got, ok := info.Flag("--listen")

	// Assert
	if !ok || got != "/s.sock" {
		t.Fatalf("Flag(--listen) = %q, %v, want the socket path", got, ok)
	}
}

func TestShimInfoFlagReportsAnAbsentFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js"}}

	// Act
	_, ok := info.Flag("--listen")

	// Assert
	if ok {
		t.Fatal("Flag(--listen) reported present, want absent")
	}
}

func TestShimInfoHasFlagFindsABareFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--fake"}}

	// Act / Assert
	if !info.HasFlag("--fake") {
		t.Fatal("HasFlag(--fake) = false, want true")
	}
	if info.HasFlag("--real") {
		t.Fatal("HasFlag(--real) = true, want false")
	}
}

func TestWorkspaceLockPathMatchesTheShimContract(t *testing.T) {
	// Arrange / Act
	got := WorkspaceLockPath("/locks", "/w/one")

	// Assert
	name := filepath.Base(got)
	if len(name) != len("workspace-")+8+len(".lock") {
		t.Fatalf("WorkspaceLockPath = %q, want workspace-<8 hex>.lock", name)
	}
}

func TestWorkspaceLockPathIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := WorkspaceLockPath("/locks", "/w/one")
	noisy := WorkspaceLockPath("/locks", "/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("WorkspaceLockPath = %q and %q, want one key per cleaned path", plain, noisy)
	}
}

func TestProfileFileNameIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := profileFileName("/w/one")
	noisy := profileFileName("/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("profileFileName = %q and %q, want one name per cleaned path", plain, noisy)
	}
}

func TestShimControlConnectCapturesProcessIdentityBeforeStandDown(t *testing.T) {
	// Arrange
	socket := filepath.Join(ShortTempDir(t), "shim.ctl")
	listener, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen on fake control socket: %v", err)
	}
	t.Cleanup(func() { _ = listener.Close() })
	served := make(chan error, 1)
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			served <- err
			return
		}
		defer conn.Close()
		scanner := bufio.NewScanner(conn)
		if !scanner.Scan() {
			served <- scanner.Err()
			return
		}
		var command controlCommand
		if err := json.Unmarshal(scanner.Bytes(), &command); err != nil {
			served <- err
			return
		}
		if command.Op != "info" {
			served <- &unexpectedControlOperation{got: command.Op}
			return
		}
		served <- json.NewEncoder(conn).Encode(controlReply{OK: true, Info: &ShimInfo{PID: 424242}})
	}()
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	t.Cleanup(cancel)
	control := &ShimControl{Socket: socket, t: t, d: &Daemon{t: t, ctx: ctx}}

	// Act
	control.connect()
	t.Cleanup(control.close)

	// Assert
	if err := <-served; err != nil {
		t.Fatalf("serve opening process identity: %v", err)
	}
	if control.pid != 424242 {
		t.Fatalf("captured process id = %d, want 424242", control.pid)
	}
}

type unexpectedControlOperation struct{ got string }

func (e *unexpectedControlOperation) Error() string {
	return "unexpected control operation " + e.got
}
