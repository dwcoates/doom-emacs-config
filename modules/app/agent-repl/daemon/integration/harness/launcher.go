package harness

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"runtime"
	"sync"
	"syscall"
	"testing"

	"google.golang.org/protobuf/encoding/protodelim"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	launcherv1 "agentrepl/proto/launcher/v1"

	"agentrepl/logging/buildreport"
)

// fakeLauncherExecutable is the bytes the fake deploy build stages as the
// launcher's executable (NewFakeDeployBuilder).
const fakeLauncherExecutable = "harness launcher\n"

// FakeLauncherBuild is the build the fake launcher's build report names: the
// content hash of the executable the fake deploy build stages, so a deploy
// judges the launcher up to date and restarts nothing.
var FakeLauncherBuild = func() string {
	sum := sha256.Sum256([]byte(fakeLauncherExecutable))
	return hex.EncodeToString(sum[:])
}()

// FakeLauncher stands in for the launcher (Zettel.app), macOS's banner
// poster: it accepts on its own socket, records every banner a daemon hands
// it, and answers each one posted, so no test ever raises a real banner. It
// is the macOS counterpart of the Linux banner program's recorder (Notifier).
//
// IT OUTLIVES EVERY DAEMON OF ITS TEST, as the real launcher's held socket
// outlives every daemon: a daemon standing down, and both daemons of a
// handover, post to it like a serving one.
type FakeLauncher struct {
	// Socket is the socket it accepts on (AGENT_REPL_LAUNCHER_SOCKET).
	Socket string

	mu      sync.Mutex
	banners []*agentreplv1.DesktopBanner
}

// StartFakeLauncher accepts on a socket under sockRoot until the test ends,
// and writes the launcher's build report into lockDir naming this process,
// so a deploy judges it current.
func StartFakeLauncher(t testing.TB, sockRoot, lockDir string) *FakeLauncher {
	t.Helper()
	l := &FakeLauncher{Socket: filepath.Join(sockRoot, "launcher.sock")}
	ln, err := net.Listen("unix", l.Socket)
	if err != nil {
		t.Fatalf("harness: the fake launcher could not listen on %s: %v", l.Socket, err)
	}
	t.Cleanup(func() { ln.Close() })
	if err := buildreport.Write(lockDir, buildreport.ServiceLauncher, buildreport.Report{PID: os.Getpid(), Build: FakeLauncherBuild}); err != nil {
		t.Fatalf("harness: write the fake launcher's build report: %v", err)
	}
	go l.accept(t, ln)
	return l
}

// accept answers every connection until the listener closes with the test.
func (l *FakeLauncher) accept(t testing.TB, ln net.Listener) {
	for {
		conn, err := ln.Accept()
		if errors.Is(err, net.ErrClosed) {
			return
		}
		if err != nil {
			t.Errorf("harness: the fake launcher's accept failed: %v", err)
			return
		}
		go l.serve(t, conn)
	}
}

// serve records one banner and answers it posted.
func (l *FakeLauncher) serve(t testing.TB, conn net.Conn) {
	defer conn.Close()
	var req launcherv1.PostBannerRequest
	if err := (protodelim.UnmarshalOptions{}).UnmarshalFrom(bufio.NewReader(conn), &req); err != nil {
		if daemonHungUp(err) {
			return
		}
		t.Errorf("harness: the fake launcher could not read a banner: %v", err)
		return
	}
	l.mu.Lock()
	l.banners = append(l.banners, req.GetBanner())
	l.mu.Unlock()
	if _, err := protodelim.MarshalTo(conn, &launcherv1.PostBannerResponse{Result: &launcherv1.PostBannerResponse_Outcome{
		Outcome: &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_Posted{
			Posted: &agentreplv1.DesktopBannerPostOutcomePosted{},
		}},
	}}); err != nil {
		if daemonHungUp(err) {
			return
		}
		t.Errorf("harness: the fake launcher could not answer a banner: %v", err)
	}
}

// daemonHungUp reports whether err is the posting daemon closing its end of
// the exchange: a daemon that stops, or gives up its bound, hangs up on a
// launcher that has not answered yet, which is normal and no fault of the
// launcher.
func daemonHungUp(err error) bool {
	return errors.Is(err, io.EOF) || errors.Is(err, io.ErrUnexpectedEOF) ||
		errors.Is(err, syscall.EPIPE) || errors.Is(err, syscall.ECONNRESET)
}

// Banners answers every banner the launcher was handed, in order.
func (l *FakeLauncher) Banners() []*agentreplv1.DesktopBanner {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]*agentreplv1.DesktopBanner(nil), l.banners...)
}

// PostedBanners answers the words of every banner the daemon posted, in
// order, whichever platform posted it: on macOS each banner's title and body
// as the launcher was handed them, elsewhere each run of the banner
// program's argv (the boot's version query set aside).
func (d *Daemon) PostedBanners() [][]string {
	d.t.Helper()
	if runtime.GOOS == "darwin" {
		var words [][]string
		for _, b := range d.Launcher.Banners() {
			words = append(words, []string{b.GetTitle().GetText(), b.GetBody().GetText()})
		}
		return words
	}
	var words [][]string
	for _, inv := range d.Notifier.Invocations() {
		words = append(words, inv.Argv)
	}
	return words
}
