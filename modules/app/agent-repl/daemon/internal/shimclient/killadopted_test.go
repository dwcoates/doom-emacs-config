package shimclient

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// A shim this daemon ADOPTED is not its child, so none of the in-process fakes
// can stand in for one: the peer credential of an in-process listener names the
// TEST, and stopping it would stop the run. Every test here therefore drives a
// real child process that binds a real unix socket, which is also the only
// shape that exercises what the fix is about — signalling a pid the kernel
// named rather than one a handle remembered.

// helperSocketEnv puts the child in socket mode and says where to bind.
const helperSocketEnv = "SHIMCLIENT_HELPER_SOCKET"

// helperIgnoreTermEnv makes the child ignore SIGTERM, so an escalation is a
// fact of the test rather than a hope about timing.
const helperIgnoreTermEnv = "SHIMCLIENT_HELPER_IGNORE_SIGTERM"

// helperGroupChildEnv makes the child fork one idle grandchild into its own
// process group before announcing readiness — the `shim-lock` holders' shape.
const helperGroupChildEnv = "SHIMCLIENT_HELPER_GROUP_CHILD"

// helperIdleEnv is the grandchild's mode: announce the pid and block.
const helperIdleEnv = "SHIMCLIENT_HELPER_IDLE"

// TestShimclientHelperProcess is not a test. It is the child every case below
// spawns, re-executing this test binary so the peer is a real process with a
// real pid and a real socket.
func TestShimclientHelperProcess(t *testing.T) {
	switch {
	case os.Getenv(helperIdleEnv) != "":
		fmt.Println(os.Getpid())
		select {}
	case os.Getenv(helperSocketEnv) == "":
		t.Skip("not the helper child")
	}

	if os.Getenv(helperIgnoreTermEnv) != "" {
		signal.Ignore(syscall.SIGTERM)
	}
	listener, err := net.Listen("unix", os.Getenv(helperSocketEnv))
	if err != nil {
		fmt.Fprintln(os.Stderr, "helper: listen:", err)
		os.Exit(1)
	}
	// THE ACCEPTED CONNECTIONS ARE HELD, never closed on arrival. A shim
	// serves its socket: it holds the connection for the session's life, and
	// the daemon's peer-credential read happens against a peer that is still
	// there. A helper that closed on accept would instead be racing every
	// reader, and on Darwin a peer credential read after the peer's close
	// answers ENOTCONN -- so the harness, not the code, would decide whether
	// the pid could be learned.
	go func() {
		var held []net.Conn
		for {
			conn, err := listener.Accept()
			if err != nil {
				for _, c := range held {
					_ = c.Close()
				}
				return
			}
			held = append(held, conn)
		}
	}()

	grandchild := 0
	if os.Getenv(helperGroupChildEnv) != "" {
		cmd := exec.Command(os.Args[0], "-test.run=TestShimclientHelperProcess")
		cmd.Env = append(os.Environ(), helperIdleEnv+"=1", helperSocketEnv+"=")
		out, err := cmd.StdoutPipe()
		if err != nil {
			fmt.Fprintln(os.Stderr, "helper: grandchild pipe:", err)
			os.Exit(1)
		}
		if err := cmd.Start(); err != nil {
			fmt.Fprintln(os.Stderr, "helper: grandchild start:", err)
			os.Exit(1)
		}
		line, err := bufio.NewReader(out).ReadString('\n')
		if err != nil {
			fmt.Fprintln(os.Stderr, "helper: grandchild readiness:", err)
			os.Exit(1)
		}
		grandchild, err = strconv.Atoi(strings.TrimSpace(line))
		if err != nil {
			fmt.Fprintln(os.Stderr, "helper: grandchild pid:", err)
			os.Exit(1)
		}
	}

	fmt.Printf("%d %d\n", os.Getpid(), grandchild)
	select {}
}

// peer is one helper child: the pid serving the socket, and the pid of the
// idle grandchild sharing its process group when one was asked for.
type peer struct {
	pid        int
	grandchild int
	uds        string
}

// startPeer spawns a helper child that binds its own socket and blocks. joinPGID
// of zero makes it lead a process group of its own, which is what the daemon's
// spawn contract gives every shim; a non-zero one puts it INTO that group
// without leading it, which is the shape the kill must refuse.
func startPeer(t *testing.T, joinPGID int, env ...string) *peer {
	t.Helper()

	uds := filepath.Join(shortDir(t), "peer.sock")
	cmd := exec.Command(os.Args[0], "-test.run=TestShimclientHelperProcess")
	cmd.Env = append(append(os.Environ(), helperSocketEnv+"="+uds), env...)
	cmd.Stderr = os.Stderr
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true, Pgid: joinPGID}
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("helper stdout: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the helper: %v", err)
	}
	line, err := bufio.NewReader(out).ReadString('\n')
	if err != nil {
		t.Fatalf("the helper never announced readiness: %v", err)
	}
	var pid, grandchild int
	if _, err := fmt.Sscanf(strings.TrimSpace(line), "%d %d", &pid, &grandchild); err != nil {
		t.Fatalf("the helper announced %q, want two pids: %v", line, err)
	}
	// THE CHILD IS REAPED AS IT DIES, on its own goroutine. In production an
	// adopted shim is nobody's child here — the daemon that spawned it exited,
	// so init reaps it — and a pid that is never waited on stays a ZOMBIE,
	// which kill(pid, 0) reports as alive forever. Without this the harness,
	// not the code, would decide the process never went down.
	reaped := make(chan struct{})
	go func() {
		_ = cmd.Wait()
		close(reaped)
	}()
	t.Cleanup(func() {
		_ = syscall.Kill(-pid, syscall.SIGKILL)
		// THE PID TOO, not only the group: a peer placed into someone else's
		// group leads no group of its own, so the group signal above finds
		// nothing and the reap below would never come.
		_ = syscall.Kill(pid, syscall.SIGKILL)
		if grandchild != 0 {
			_ = syscall.Kill(grandchild, syscall.SIGKILL)
		}
		<-reaped
	})
	return &peer{pid: pid, grandchild: grandchild, uds: uds}
}

// adoptedClientFor is a client shaped exactly like one Adopt returns: a socket
// path and no child handle at all.
func adoptedClientFor(udsPath string, grace time.Duration) *client {
	c := newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"), udsPath, defaultBackoff, nil, nil)
	c.grace = grace
	return c
}

// gone reports whether a pid is no longer running.
func gone(pid int) bool {
	return errors.Is(syscall.Kill(pid, 0), syscall.ESRCH)
}

// TestKillStopsAnAdoptedShim asserts the case the leak was: a client with no
// child handle stops the process serving its socket instead of refusing with
// ErrNoProcess.
func TestKillStopsAnAdoptedShim(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0)
	c := adoptedClientFor(p.uds, 2*time.Second)

	// Act.
	err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"})

	// Assert.
	if err != nil {
		t.Fatalf("Kill() error = %v, want nil", err)
	}
	if !gone(p.pid) {
		t.Fatalf("the adopted peer pid %d is still running after Kill()", p.pid)
	}
}

// TestKillTakesTheAdoptedShimsWholeProcessGroup asserts the group goes, not
// only the peer: a shim's kernel-lock holders are separate processes in its
// group, and a peer-only kill leaks them.
func TestKillTakesTheAdoptedShimsWholeProcessGroup(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0, helperGroupChildEnv+"=1")
	if p.grandchild == 0 {
		t.Fatal("the helper announced no group child")
	}
	c := adoptedClientFor(p.uds, 2*time.Second)

	// Act.
	if err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"}); err != nil {
		t.Fatalf("Kill() error = %v, want nil", err)
	}

	// Assert.
	if !gone(p.grandchild) {
		t.Fatalf("the adopted shim's group child pid %d is still running after Kill()", p.grandchild)
	}
}

// TestKillEscalatesAnAdoptedShimThatIgnoresSIGTERM asserts the escalation
// happens for a shim nobody parented, exactly as it does for a spawned one.
func TestKillEscalatesAnAdoptedShimThatIgnoresSIGTERM(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0, helperIgnoreTermEnv+"=1")
	c := adoptedClientFor(p.uds, 250*time.Millisecond)

	// Act.
	err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"})

	// Assert.
	if err != nil {
		t.Fatalf("Kill() error = %v, want nil", err)
	}
	if !gone(p.pid) {
		t.Fatalf("the adopted peer pid %d survived a SIGTERM it ignored; the escalation never landed", p.pid)
	}
}

// TestKillRecordsTheAdoptedShimsDeath asserts the stop is decided rather than
// merely performed: the client reports an exit, so the fleet and the views
// learn the session is over.
func TestKillRecordsTheAdoptedShimsDeath(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0)
	c := adoptedClientFor(p.uds, 2*time.Second)

	// Act.
	if err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"}); err != nil {
		t.Fatalf("Kill() error = %v, want nil", err)
	}

	// Assert.
	info, reaped := c.Reaped()
	if !reaped {
		t.Fatal("Reaped() = false after an adopted shim was stopped")
	}
	if info.PID != p.pid {
		t.Fatalf("the recorded exit names pid %d, want the adopted peer's %d", info.PID, p.pid)
	}
}

// TestKillReadsAnAbsentAdoptedSocketAsAlreadyStopped asserts a shim whose
// socket is gone is the state the caller asked for, not a failure.
func TestKillReadsAnAbsentAdoptedSocketAsAlreadyStopped(t *testing.T) {
	// Arrange.
	c := adoptedClientFor(filepath.Join(shortDir(t), "absent.sock"), 2*time.Second)

	// Act.
	err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"})

	// Assert.
	if err != nil {
		t.Fatalf("Kill() error = %v, want nil for a shim whose socket is already gone", err)
	}
}

// TestKillRefusesAnAdoptedShimThatLeadsNoProcessGroup asserts the guard that
// keeps this from signalling a group the daemon cannot account for: every shim
// leads its own group by the spawn contract, and a peer that does not is
// refused rather than signalled.
func TestKillRefusesAnAdoptedShimThatLeadsNoProcessGroup(t *testing.T) {
	// Arrange: a leader to borrow a group from, and a peer placed INTO that
	// group so it is a member without leading it.
	leader := startPeer(t, 0)
	p := startPeer(t, leader.pid)
	c := adoptedClientFor(p.uds, 2*time.Second)

	// Act.
	err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the daemon is standing down"})

	// Assert.
	if err == nil {
		t.Fatal("Kill() error = nil, want a refusal for a peer that leads no process group")
	}
	if gone(p.pid) {
		t.Fatalf("the refused peer pid %d was signalled anyway", p.pid)
	}
}

// TestSocketPeerPIDNamesTheServingProcess asserts the credential read itself:
// the pid comes from the kernel's view of who is on the other end, which is
// what makes a recycled pid unable to be mistaken for the shim.
func TestSocketPeerPIDNamesTheServingProcess(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0)

	// Act.
	pid, err := socketPeerPID(p.uds)

	// Assert.
	if err != nil {
		t.Fatalf("socketPeerPID() error = %v", err)
	}
	if pid != p.pid {
		t.Fatalf("socketPeerPID() = %d, want the serving process's %d", pid, p.pid)
	}
}

// TestSocketGoneReadsALostPeerAsGone asserts the arm that keeps a shim which
// exits DURING the credential read from being reported as unstoppable: a dial
// can win the race with the exit and hand back a socket whose peer has already
// gone, and every question about that peer then answers ENOTCONN.
func TestSocketGoneReadsALostPeerAsGone(t *testing.T) {
	// Arrange: the error exactly as `socketPeerPID' wraps it.
	err := fmt.Errorf("shimclient: peer credential of %q: %w", "/tmp/peer.sock",
		fmt.Errorf("getsockopt LOCAL_PEERPID: %w", syscall.ENOTCONN))

	// Act.
	got := isSocketGone(err)

	// Assert.
	if !got {
		t.Fatalf("isSocketGone(%v) = false, want true: a peer that dropped the connection is not serving the socket", err)
	}
}

// TestSocketGoneReadsALostPeerAsGoneThroughAnOpError asserts the same arm on
// the shape the net package produces, so a connection-level ENOTCONN is read
// the same way as a getsockopt's.
func TestSocketGoneReadsALostPeerAsGoneThroughAnOpError(t *testing.T) {
	// Arrange.
	err := &net.OpError{Op: "read", Net: "unix", Err: syscall.ENOTCONN}

	// Act.
	got := isSocketGone(err)

	// Assert.
	if !got {
		t.Fatalf("isSocketGone(%v) = false, want true for a net.OpError carrying ENOTCONN", err)
	}
}

// ---- an adopted shim's own exit, after a stand-down ----

// standingDownAdopted is an adopted client that knows its peer's pid and has
// been asked to stand down, which is the state the relaunch's reap gate
// waits on.
func standingDownAdopted(p *peer) *client {
	c := adoptedClientFor(p.uds, 2*time.Second)
	c.pid = p.pid
	c.standDown.Store(true)
	return c
}

// awaitExitInfo waits for the client's exit, failing the test past bound.
func awaitExitInfo(t *testing.T, c *client, bound time.Duration) ExitInfo {
	t.Helper()
	select {
	case info := <-c.Exited():
		return info
	case <-time.After(bound):
		t.Fatalf("no exit was published within %s", bound)
		return ExitInfo{}
	}
}

// TestAwaitAdoptedExitPublishesTheExitOnceTheProcessGroupIsGone is the reap
// gate's missing evidence: an adopted shim that leaves after its stand-down is
// seen leaving, with its pid, promptly.
func TestAwaitAdoptedExitPublishesTheExitOnceTheProcessGroupIsGone(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0)
	c := standingDownAdopted(p)
	go c.awaitAdoptedExit(context.Background())

	// Act.
	start := time.Now()
	if err := syscall.Kill(-p.pid, syscall.SIGTERM); err != nil {
		t.Fatalf("stop the peer: %v", err)
	}
	info := awaitExitInfo(t, c, 5*time.Second)

	// Assert.
	if info.PID != p.pid || !info.Inferred {
		t.Fatalf("exit = %+v, want the adopted peer's pid %d, inferred", info, p.pid)
	}
	t.Logf("the adopted shim's exit was published %s after it was signalled", time.Since(start))
}

// TestAwaitAdoptedExitWatchesAPeerThatLeadsNoGroupByItsPid covers a peer that
// is a member of someone else's group: the group outlives it, so the pid is
// what is watched.
func TestAwaitAdoptedExitWatchesAPeerThatLeadsNoGroupByItsPid(t *testing.T) {
	// Arrange.
	leader := startPeer(t, 0)
	p := startPeer(t, leader.pid)
	c := standingDownAdopted(p)
	go c.awaitAdoptedExit(context.Background())

	// Act.
	if err := syscall.Kill(p.pid, syscall.SIGKILL); err != nil {
		t.Fatalf("stop the peer: %v", err)
	}
	info := awaitExitInfo(t, c, 5*time.Second)

	// Assert.
	if info.PID != p.pid {
		t.Fatalf("exit = %+v, want the peer's pid %d while its group's leader lives on", info, p.pid)
	}
}

// TestAwaitAdoptedExitPublishesNothingForALiveShimWhenItsLifetimeEnds covers
// the end of supervision (a detach) while the shim is still running.
func TestAwaitAdoptedExitPublishesNothingForALiveShimWhenItsLifetimeEnds(t *testing.T) {
	// Arrange.
	p := startPeer(t, 0)
	c := standingDownAdopted(p)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	c.awaitAdoptedExit(ctx)

	// Assert.
	if _, reaped := c.Reaped(); reaped {
		t.Fatal("an exit was published for a shim that is still running")
	}
}

// TestAwaitAdoptedExitWithNoPidPublishesNothing covers an adoption whose
// socket yielded no pid: nothing is guessed.
func TestAwaitAdoptedExitWithNoPidPublishesNothing(t *testing.T) {
	// Arrange.
	c := adoptedClientFor(filepath.Join(shortDir(t), "absent.sock"), 2*time.Second)
	c.standDown.Store(true)

	// Act.
	c.awaitAdoptedExit(context.Background())

	// Assert.
	if _, reaped := c.Reaped(); reaped {
		t.Fatal("an exit was published for a shim whose pid was never known")
	}
}

// TestKillOfAnAbsentAdoptedSocketKeepsTheAdoptedPid covers the exit the
// relaunch reported as `pid: 0`: the pid learned at adoption stays on it.
func TestKillOfAnAbsentAdoptedSocketKeepsTheAdoptedPid(t *testing.T) {
	// Arrange.
	c := adoptedClientFor(filepath.Join(shortDir(t), "absent.sock"), 2*time.Second)
	c.pid = 28278

	// Act.
	if err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "the stand-down window expired"}); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}

	// Assert.
	info, reaped := c.Reaped()
	if !reaped || info.PID != 28278 {
		t.Fatalf("exit = %+v (reaped %v), want the adopted pid 28278 kept", info, reaped)
	}
}
