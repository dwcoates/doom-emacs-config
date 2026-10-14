package e2e

// THE WARM WEBAPP-LAYER DRIVER: one long-lived vitest (webapp/test/
// webapp-layer/layer-server.mjs) runs every simple area's file, so the layer
// pays vitest's start, the app's transform and its module graph once per
// package run instead of once per area. MEASURED over twelve cold children:
// 169s of child Duration was 22s of tests and 144s of transform plus collect.
//
// The spec's ruling 4 (WEBAPP-LAYER-SPEC.md) is kept whole: every area is its
// own Go test, builds its OWN world, and asks for exactly ONE file, which the
// driver runs against that world alone (handed over by provide/inject, never
// the shared environment). Runs are one at a time: an area waits its turn
// holding its own world, and its WebappLayerTimeout starts when its run does.

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"agent-repld/integration/harness"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// wlResultPrefix marks the driver's one answer line per run (RESULT_PREFIX in
// layer-server.mjs).
const wlResultPrefix = "WEBAPP-LAYER-RESULT "

// wlReadyPrefix marks the driver's one line once its warm-up has run
// (READY_PREFIX in layer-server.mjs). No area is sent before it, so no area's
// WebappLayerTimeout carries vitest's cold transform and collect.
const wlReadyPrefix = "WEBAPP-LAYER-READY "

// wlWarmupBound is how long the driver's warm-up may take: vitest's cold
// transform and collect of the module graph every area imports. MEASURED
// 2026-10-14: 17.0s on an idle host, 26.4s inside a full run at the default I/O
// tier (the cold first area of the failing runs); just over twice the loaded
// figure.
const wlWarmupBound = 55 * time.Second

// wlReady is the driver's warm-up verdict.
type wlReady struct {
	OK       bool     `json:"ok"`
	WarmMs   int64    `json:"warmMs"`
	Failures []string `json:"failures"`
}

// wlServerStopBound is how long a driver whose stdin closed has to exit
// before it is killed; it ends on stdin's EOF after its current run.
const wlServerStopBound = 5 * time.Second

// wlWorld is one area's world as the driver hands it to the page.
type wlWorld struct {
	DaemonURL    string `json:"daemonUrl"`
	WorkspaceID  string `json:"workspaceId"`
	WorkspaceDir string `json:"workspaceDir"`
	WebappBuild  string `json:"webappBuild"`
}

// wlAnswer is the driver's verdict on one run.
type wlAnswer struct {
	File     string   `json:"file"`
	OK       bool     `json:"ok"`
	Tests    int      `json:"tests"`
	Failures []string `json:"failures"`
}

// wlServer is the one warm driver of this test binary.
type wlServer struct {
	mu    sync.Mutex
	cmd   *exec.Cmd
	stdin io.WriteCloser
	// lines carries every line the driver prints, stdout and stderr; it is
	// closed once both streams have ended, which is how a dead driver is seen.
	lines chan string
	// broken is set once a run timed out or the driver died: every later
	// request is refused with it rather than sent to a driver in an unknown
	// state.
	broken error
	// tmp is the driver's own temp dir, removed once it has stopped.
	tmp string
}

var (
	wlServerMu  sync.Mutex
	wlTheServer *wlServer
	// wlOuterTempDir is the temp directory from before MainAt pointed TMPDIR
	// at its run root (main_test.go); the driver's own temp dir is made in it.
	wlOuterTempDir string
)

// wlLayerServer answers the binary's warm driver, starting it on first use.
func wlLayerServer(t *testing.T, npm, webappDir string) (*wlServer, error) {
	t.Helper()
	wlServerMu.Lock()
	defer wlServerMu.Unlock()
	if wlTheServer != nil {
		return wlTheServer, nil
	}
	tmp, err := os.MkdirTemp(wlOuterTempDir, "webapp-layer-driver-")
	if err != nil {
		return nil, fmt.Errorf("make the layer driver's temp dir: %w", err)
	}
	bg := filepath.Join(filepath.Dir(webappDir), "bin", "background.sh")
	cmd := exec.Command(bg, "node", filepath.Join("test", "webapp-layer", "layer-server.mjs"))
	cmd.Dir = webappDir
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_WEBAPP_LAYER=1",
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"CI=1",
		"TMPDIR="+tmp,
	)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, fmt.Errorf("layer driver stdin pipe: %w", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("layer driver stdout pipe: %w", err)
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, fmt.Errorf("layer driver stderr pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("start the layer driver in %s: %w", webappDir, err)
	}
	s := &wlServer{cmd: cmd, stdin: stdin, lines: make(chan string, 4096), tmp: tmp}
	var streams sync.WaitGroup
	pump := func(r io.Reader) {
		defer streams.Done()
		scanner := bufio.NewScanner(r)
		scanner.Buffer(make([]byte, 0, 64*1024), 4*1024*1024)
		for scanner.Scan() {
			s.lines <- scanner.Text()
		}
	}
	streams.Add(2)
	go pump(stdout)
	go pump(stderr)
	go func() { streams.Wait(); close(s.lines) }()
	if err := s.awaitReady(t); err != nil {
		s.broken = err
	}
	wlTheServer = s
	return s, nil
}

// awaitReady consumes the driver's output until its warm-up verdict, within
// wlWarmupBound. A warm-up that failed, a driver that died, and one that took
// too long are each answered as the driver being unusable.
func (s *wlServer) awaitReady(t *testing.T) error {
	t.Helper()
	var tail []string
	deadline := time.NewTimer(wlWarmupBound)
	defer deadline.Stop()
	for {
		select {
		case line, ok := <-s.lines:
			if !ok {
				return fmt.Errorf("the layer driver exited during its warm-up; last output:\n%s", strings.Join(tail, "\n"))
			}
			if rest, isReady := strings.CutPrefix(line, wlReadyPrefix); isReady {
				var ready wlReady
				if err := json.Unmarshal([]byte(rest), &ready); err != nil {
					return fmt.Errorf("decode the layer driver's warm-up verdict: %w (%q)", err, rest)
				}
				if !ready.OK {
					return fmt.Errorf("the layer driver's warm-up failed: %s\nlast output:\n%s", strings.Join(ready.Failures, "\n"), strings.Join(tail, "\n"))
				}
				t.Logf("e2e/webapp-layer: the driver warmed up in %dms (bound %s)", ready.WarmMs, wlWarmupBound)
				return nil
			}
			tail = append(tail, line)
			if len(tail) > wlTailLines {
				tail = tail[len(tail)-wlTailLines:]
			}
		case <-deadline.C:
			_ = syscall.Kill(-s.cmd.Process.Pid, syscall.SIGKILL)
			return fmt.Errorf("the layer driver did not warm up within %s; last output:\n%s", wlWarmupBound, strings.Join(tail, "\n"))
		}
	}
}

// wlStopLayerServer ends the binary's driver, if one was started: its stdin
// closes, it exits after its current run, and a driver that does not is
// killed with its whole group.
func wlStopLayerServer() {
	wlServerMu.Lock()
	s := wlTheServer
	wlTheServer = nil
	wlServerMu.Unlock()
	if s == nil {
		return
	}
	_ = s.stdin.Close()
	done := make(chan error, 1)
	go func() { done <- s.cmd.Wait() }()
	select {
	case err := <-done:
		if err != nil {
			fmt.Fprintf(os.Stderr, "e2e/webapp-layer: the layer driver exited with %v\n", err)
		}
	case <-time.After(wlServerStopBound):
		fmt.Fprintf(os.Stderr, "e2e/webapp-layer: the layer driver outlived its stdin by %s; killing its group\n", wlServerStopBound)
		_ = syscall.Kill(-s.cmd.Process.Pid, syscall.SIGKILL)
		<-done
	}
	if err := os.RemoveAll(s.tmp); err != nil {
		fmt.Fprintf(os.Stderr, "e2e/webapp-layer: remove the layer driver's temp dir %s: %v\n", s.tmp, err)
	}
}

// Run drives one area's file against its world and answers its failure, if
// any. The driver's output for the run is mirrored into t's log.
func (s *wlServer) Run(t *testing.T, file string, world wlWorld) error {
	t.Helper()
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.broken != nil {
		return fmt.Errorf("the layer driver is unusable: %w", s.broken)
	}
	request, err := json.Marshal(struct {
		File  string  `json:"file"`
		World wlWorld `json:"world"`
	}{file, world})
	if err != nil {
		return fmt.Errorf("encode the request for %s: %w", file, err)
	}
	started := time.Now()
	if _, err := s.stdin.Write(append(request, '\n')); err != nil {
		s.broken = fmt.Errorf("write the request for %s: %w", file, err)
		return s.broken
	}
	var tail []string
	deadline := time.NewTimer(WebappLayerTimeout)
	defer deadline.Stop()
	for {
		select {
		case line, ok := <-s.lines:
			if !ok {
				s.broken = fmt.Errorf("the layer driver exited during %s; last output:\n%s", file, strings.Join(tail, "\n"))
				return s.broken
			}
			if rest, isAnswer := strings.CutPrefix(line, wlResultPrefix); isAnswer {
				t.Logf("e2e/webapp-layer: %s run wall %s (bound %s)", file, time.Since(started).Round(time.Millisecond), WebappLayerTimeout)
				var answer wlAnswer
				if err := json.Unmarshal([]byte(rest), &answer); err != nil {
					s.broken = fmt.Errorf("decode the answer for %s: %w (%q)", file, err, rest)
					return s.broken
				}
				if answer.File != file {
					s.broken = fmt.Errorf("asked for %s, the driver answered for %s", file, answer.File)
					return s.broken
				}
				if !answer.OK {
					return fmt.Errorf("%d test(s), failures:\n%s\nlast output:\n%s", answer.Tests, strings.Join(answer.Failures, "\n"), strings.Join(tail, "\n"))
				}
				return nil
			}
			t.Logf("webapp-layer | %s", line)
			tail = append(tail, line)
			if len(tail) > wlTailLines {
				tail = tail[len(tail)-wlTailLines:]
			}
		case <-deadline.C:
			s.broken = fmt.Errorf("%s did not finish within %s; last output:\n%s", file, WebappLayerTimeout, strings.Join(tail, "\n"))
			_ = syscall.Kill(-s.cmd.Process.Pid, syscall.SIGKILL)
			return s.broken
		}
	}
}

// wlRunLayerFile drives one area's file on the warm driver against w's
// daemon and ws, its own world.
func wlRunLayerFile(t *testing.T, npm, webappDir, file string, w *World, ws *workspacev1.WorkspaceRef) error {
	t.Helper()
	if w.Addr == "" {
		return errors.New("the daemon published no address to hand the webapp")
	}
	s, err := wlLayerServer(t, npm, webappDir)
	if err != nil {
		return err
	}
	return s.Run(t, file, wlWorld{
		DaemonURL:    "http://" + w.Addr,
		WorkspaceID:  ws.GetId(),
		WorkspaceDir: ws.GetDir(),
		WebappBuild:  harness.FakeWebappEntry,
	})
}
