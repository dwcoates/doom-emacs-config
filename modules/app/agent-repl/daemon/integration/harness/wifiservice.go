package harness

import (
	"context"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"syscall"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/wifirpc"
)

// EVERY DAEMON'S PERSISTENT WIFI RUNS ON A REAL PERSISTENT-WIFI SERVICE OF ITS
// TEST over the fake host, never the owner's
// (~/.cache/agent-repl/sock/persistent-wifi.sock): a test daemon pointed at
// the owner's service would change the owner's power settings.
//
// The service is keyed to the STATE ROOT, as the test service is: sharing a
// state root is how this suite spells "the same daemon, restarted", and the
// service outlives each of those daemons. A restarted daemon therefore
// adopts the fake host (its tools, lid and power source) the service was
// started over, since those are what the service reads.
//
// THE HARNESS BINDS THE SOCKET AND HANDS IT OVER (--listen-fd), so a daemon's
// first connect queues in the socket's backlog until the service accepts:
// there is no readiness to wait for.

// wifiServiceStopBound bounds the service's exit on SIGTERM: its orderly
// drain (cmd/agent-repl-wifi's shutdownGrace) and the process's own exit.
const wifiServiceStopBound = 2 * 5 * time.Second

// wifiHost is the fake host a state root's service was started over.
type wifiHost struct {
	socket       string
	hostToolsDir string
	lid          *FakeLid
	powerSource  *FakePowerSource
}

// wifiServices is the service of each state root this process started one
// for.
var wifiServices sync.Map

// WifiSocketFor is the socket of the persistent-wifi service of the daemons
// over STATEDIR: a sibling of the state root, named for that root, so two
// state roots under one parent never bind one socket.
func WifiSocketFor(stateDir string) string {
	return filepath.Join(filepath.Dir(stateDir), "wifi-"+filepath.Base(stateDir), "wifi.sock")
}

// ensureWifiService starts the persistent-wifi service of the daemons over
// d's state root the first time one starts, over d's own fake host, and makes
// every later daemon over the root adopt that host. It answers the service's
// socket. The service ends with the test that started it.
func ensureWifiService(t testing.TB, d *Daemon) string {
	t.Helper()
	if v, started := wifiServices.Load(d.StateDir); started {
		host := v.(*wifiHost)
		d.HostToolsDir, d.Lid, d.PowerSource = host.hostToolsDir, host.lid, host.powerSource
		return host.socket
	}
	socket := WifiSocketFor(d.StateDir)
	if err := os.MkdirAll(filepath.Dir(socket), 0o755); err != nil {
		t.Fatalf("harness: mkdir the persistent-wifi service's socket directory: %v", err)
	}
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("harness: bind the persistent-wifi service's socket %s: %v", socket, err)
	}
	listener := ln.(*net.UnixListener)
	file, err := listener.File()
	if err != nil {
		t.Fatalf("harness: the persistent-wifi service's listener as a file: %v", err)
	}
	// The child holds the socket from here; the harness's copy closes
	// without unlinking the path the child serves on.
	listener.SetUnlinkOnClose(false)
	_ = listener.Close()
	cmd := exec.Command(WifiServiceBinary(t), "--socket", socket, "--state-dir", d.StateDir, "--listen-fd", "3")
	cmd.ExtraFiles = []*os.File{file}
	// Its build report goes to the daemons' lock directory, never the
	// owner's ~/.cache/agent-repl/run.
	cmd.Env = append(cleanGitEnv(os.Environ()),
		"AGENT_REPL_LOCK_DIR="+d.LockDir,
		"AGENT_REPL_STATE_DIR="+d.StateDir,
		wifirpc.EnvSocket+"="+socket,
		"AGENT_REPL_LOG_LEVEL=debug",
		"AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR="+d.HostToolsDir,
		persistentwifi.EnvHotspot+"="+FakeHotspot,
		persistentwifi.EnvLid+"="+d.Lid.Path,
		persistentwifi.EnvPowerSource+"="+d.PowerSource.Path,
	)
	cmd.Stdout, cmd.Stderr = os.Stderr, os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("harness: start the persistent-wifi service: %v", err)
	}
	_ = file.Close()
	// THE SERVICE NAMES THE STATE ROOT in its argv (--state-dir) and outlives
	// every daemon over it by design, so it is the TEST's process, never a
	// stray of the daemon's tree: the test stops it at its end, below.
	SpareFromStrayReaping(t, cmd.Process.Pid)
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()
	wifiServices.Store(d.StateDir, &wifiHost{socket: socket, hostToolsDir: d.HostToolsDir, lid: d.Lid, powerSource: d.PowerSource})
	t.Cleanup(func() {
		wifiServices.Delete(d.StateDir)
		_ = cmd.Process.Signal(syscall.SIGTERM)
		// THE BOUND IS ON THE GRANTED CLOCK: the service is this binary's
		// child, so a host too busy to schedule it never reads as a hang.
		wait, cancel := grantedclock.WithBound(context.Background(), wifiServiceStopBound)
		defer cancel()
		select {
		case <-exited:
		case <-wait.Done():
			_ = cmd.Process.Kill()
			<-exited
			t.Errorf("harness: the persistent-wifi service did not exit within %s granted of SIGTERM (its drain is 5s); killed: %v", wifiServiceStopBound, grantedclock.Explain(wait))
		}
	})
	return socket
}
