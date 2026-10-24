// Package integration boots the REAL zettel binary on a private unix socket and
// drives it only through zettel.v1 Connect clients, the way the daemon does.
// It proves what needs the process itself: the boot, the wire, the graph
// outliving a restart, and the orderly exit on SIGTERM. Every graph rule is
// proven at the unit tier (internal/graph, internal/server).
package integration

import (
	"bufio"
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/proto/zettel/v1/zettelv1connect"
	"agentrepl/testrun/testenv"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

var binary string

func TestMain(m *testing.M) {
	dir := ""
	built, err := testenv.ProgramOr(os.Getenv, "zettel", func() (string, error) {
		d, err := os.MkdirTemp("", "zettel-itest-bin")
		if err != nil {
			return "", err
		}
		dir = d
		out := filepath.Join(d, "zettel")
		build := exec.Command("go", "build", "-buildvcs=false", "-o", out, ".")
		build.Dir = ".."
		build.Stdout, build.Stderr = os.Stderr, os.Stderr
		return out, build.Run()
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "integration: building zettel: %v\n", err)
		os.Exit(1)
	}
	binary = built
	code := m.Run()
	if dir != "" {
		_ = os.RemoveAll(dir)
	}
	os.Exit(code)
}

type process struct {
	cmd    *exec.Cmd
	socket string
	log    string
	db     string
	done   chan error
}

// readyBound is twice the measured healthy boot (a few milliseconds) plus the
// exec of a cold binary on a loaded host.
const readyBound = 10 * time.Second

func start(t *testing.T, db string) *process {
	t.Helper()
	sockDir, err := os.MkdirTemp("/tmp", "zt")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	work := t.TempDir()
	p := &process{socket: filepath.Join(sockDir, "z.sock"), log: filepath.Join(work, "zettel.log"), db: db, done: make(chan error, 1)}
	p.cmd = exec.Command(binary, "--socket", p.socket, "--db", db, "--log", p.log)
	p.cmd.Env = append(os.Environ(), "AGENT_REPL_LOCK_DIR="+filepath.Join(work, "run"), "XDG_CACHE_HOME="+work)
	if err := p.cmd.Start(); err != nil {
		t.Fatal(err)
	}
	go func() { p.done <- p.cmd.Wait() }()
	t.Cleanup(func() {
		if p.cmd.ProcessState == nil {
			_ = p.cmd.Process.Kill()
			<-p.done
		}
	})
	// READINESS IS OBSERVED, NOT WAITED OUT: the socket accepting is the
	// service's own statement that it serves.
	deadline := time.Now().Add(readyBound)
	tick := time.NewTicker(5 * time.Millisecond)
	defer tick.Stop()
	for {
		if conn, err := net.Dial("unix", p.socket); err == nil {
			_ = conn.Close()
			return p
		}
		select {
		case err := <-p.done:
			t.Fatalf("zettel exited before serving: %v\n%s", err, p.logText())
		case <-tick.C:
		}
		if time.Now().After(deadline) {
			t.Fatalf("zettel did not accept within %s\n%s", readyBound, p.logText())
		}
	}
}

func (p *process) logText() string {
	b, _ := os.ReadFile(p.log)
	return string(b)
}

func (p *process) client() zettelv1connect.ZettelClient {
	transport := &http2.Transport{AllowHTTP: true, DialTLSContext: func(ctx context.Context, _, _ string, _ *tls.Config) (net.Conn, error) {
		var d net.Dialer
		return d.DialContext(ctx, "unix", p.socket)
	}}
	return zettelv1connect.NewZettelClient(&http.Client{Transport: transport}, "http://zettel")
}

func (p *process) stop(t *testing.T) {
	t.Helper()
	if err := p.cmd.Process.Signal(syscall.SIGTERM); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-p.done:
		if err != nil {
			t.Fatalf("zettel exited with %v after SIGTERM\n%s", err, p.logText())
		}
	case <-time.After(readyBound):
		t.Fatalf("zettel did not exit within %s of SIGTERM", readyBound)
	}
}

func TestServesTheGraphOverTheSocketAndKeepsItAcrossARestart(t *testing.T) {
	// Arrange
	db := filepath.Join(t.TempDir(), "zettel.db")
	first := start(t, db)
	ctx := context.Background()
	begun, err := first.client().BeginExchange(ctx, connect.NewRequest(&zettelv1.BeginExchangeRequest{
		Thread: &zettelv1.ThreadId{Id: "t1"}, Turn: &zettelv1.ExchangeTurn{TurnId: "turn1"},
		Question: &zettelv1.NodeQuestion{Text: "What are quarks?"},
		Parent:   &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}},
	}))
	if err != nil || begun.Msg.GetSuccess() == nil {
		t.Fatalf("BeginExchange: %v %v", err, begun)
	}
	if _, err := first.client().CompleteExchange(ctx, connect.NewRequest(&zettelv1.CompleteExchangeRequest{
		Exchange: begun.Msg.GetSuccess().GetExchange(), AnswerMarkdown: "Elementary particles.",
	})); err != nil {
		t.Fatal(err)
	}
	first.stop(t)

	// Act
	second := start(t, db)
	thread, err := second.client().GetThread(ctx, connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: "t1"}}))

	// Assert
	if err != nil || len(thread.Msg.GetSuccess().GetNodes()) != 1 ||
		thread.Msg.GetSuccess().GetNodes()[0].GetAnswer().GetMarkdown() != "Elementary particles." {
		t.Fatalf("GetThread after restart = %v, %v; want the node carried forward", thread, err)
	}
	second.stop(t)
}

func TestExitsCleanlyOnSIGTERMAndSaysSo(t *testing.T) {
	// Arrange
	p := start(t, filepath.Join(t.TempDir(), "zettel.db"))

	// Act
	p.stop(t)

	// Assert
	lines := strings.Split(strings.TrimSpace(p.logText()), "\n")
	last := lines[len(lines)-1]
	if !strings.Contains(last, `"operation":"zettel.exit"`) || !strings.Contains(last, "cleanly") {
		t.Errorf("last record = %q, want the clean exit", last)
	}
	if _, err := os.Stat(p.socket); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("the socket outlived the process: %v", err)
	}
}

func TestRefusesASecondInstanceOnALiveSocket(t *testing.T) {
	// Arrange
	first := start(t, filepath.Join(t.TempDir(), "a.db"))
	work := t.TempDir()
	logPath := filepath.Join(work, "second.log")
	cmd := exec.Command(binary, "--socket", first.socket, "--db", filepath.Join(work, "b.db"), "--log", logPath)
	cmd.Env = append(os.Environ(), "AGENT_REPL_LOCK_DIR="+filepath.Join(work, "run"))

	// Act
	err := cmd.Run()

	// Assert
	var exit *exec.ExitError
	if !errors.As(err, &exit) || exit.ExitCode() != 1 {
		t.Fatalf("second instance err = %v, want exit 1", err)
	}
	f, _ := os.Open(logPath)
	defer f.Close()
	found := false
	for s := bufio.NewScanner(f); s.Scan(); {
		if strings.Contains(s.Text(), `"step":"occupied"`) {
			found = true
		}
	}
	if !found {
		t.Error("the second instance did not log the occupied socket")
	}
	first.stop(t)
}
