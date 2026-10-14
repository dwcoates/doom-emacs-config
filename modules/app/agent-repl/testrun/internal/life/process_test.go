package life

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/reap"
)

// The helper roles this test binary takes when it is re-executed: the
// sentinel, its runner (life.EnvRole), and the runner's warden. They wire the
// package exactly as cmd testrun does, with a directory standing in for the
// RAM disk and a forking shell standing in for a unit.
const (
	envHelper = "LIFE_TEST_HELPER"
	envDisk   = "LIFE_TEST_DISK"
	envRoot   = "LIFE_TEST_ROOT"
	envMode   = "LIFE_TEST_MODE"
	envRunLog = "LIFE_TEST_RUN_LOG"
)

// forkingUnit forks without pause: a few children that live on, and a stream
// of short ones, so the reap always meets a tree that is still forking.
const forkingUnit = `for i in 1 2 3; do /bin/sleep 300 & done; while :; do /usr/bin/true & done`

func TestMain(m *testing.M) {
	switch {
	case os.Getenv(EnvRole) == RoleRunner:
		os.Exit(helperRunner())
	case len(os.Args) > 1 && os.Args[1] == "warden":
		os.Exit(helperWarden(os.Args[2:]))
	case os.Getenv(envHelper) == "sentinel":
		os.Exit(Sentinel(os.Args[0], nil, os.Stdout, os.Stderr, &run.Log{Out: os.Stdout, Err: os.Stderr}))
	}
	os.Exit(m.Run())
}

func helperRunner() int {
	log := CutLog(os.Getenv(envRunLog))
	os.Unsetenv(EnvRole)
	r, err := StartRunner(reap.OSTable{}, reap.OSReap(log), WardenCommand{Name: os.Args[0], Args: []string{"warden"}}, log)
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	gone, err := Lifeline()
	if err != nil {
		log.Errorf("%v", err)
		return 1
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	r.CancelOnLifeline(gone, cancel)
	root := os.Getenv(envRoot)
	if err := r.Root(root); err != nil {
		log.Errorf("%v", err)
		return 1
	}
	script := forkingUnit
	if os.Getenv(envMode) == "normal" {
		script = "/usr/bin/true"
	}
	unit := exec.Command("/bin/bash", "-c", script)
	unit.Env = append(os.Environ(), "TMPDIR="+filepath.Join(root, "unit"))
	unit.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := unit.Start(); err != nil {
		log.Errorf("start the unit: %v", err)
		return 1
	}
	fmt.Printf("ready %d %d %d\n", os.Getpid(), r.Own.Except[2], unit.Process.Pid)
	unitDone := make(chan struct{})
	go func() { _ = unit.Wait(); close(unitDone) }()
	if os.Getenv(envMode) == "normal" {
		<-unitDone
	} else {
		<-ctx.Done()
	}
	return r.Finish(0, gone, func() error { return os.RemoveAll(os.Getenv(envDisk)) })
}

func helperWarden(argv []string) int {
	log := CutLog(os.Getenv(envRunLog))
	var runner int
	var since int64
	var sentinel int
	for i := 0; i+1 < len(argv); i += 2 {
		switch argv[i] {
		case "--runner":
			runner, _ = strconv.Atoi(argv[i+1])
		case "--since":
			since, _ = strconv.ParseInt(argv[i+1], 10, 64)
		case "--sentinel":
			sentinel, _ = strconv.Atoi(argv[i+1])
		}
	}
	return Warden{
		Runner:   runner,
		Since:    time.Unix(since, 0),
		Sentinel: sentinel,
		Reap:     reap.OSReap(log),
		Release: func(int) (bool, error) {
			return true, os.RemoveAll(os.Getenv(envDisk))
		},
		Log: log,
	}.WatchLifeline()
}

// run is one helper run as the test sees it.
type helperRun struct {
	cmd                  *exec.Cmd
	runner, warden, unit int
	disk, root, runLog   string

	mu  sync.Mutex
	out strings.Builder
	// drained closes once the sentinel's output has ended.
	drained chan struct{}
}

func (h *helperRun) output() string {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.out.String()
}

// startHelper starts a sentinel in a process group of its own, as
// bin/test-all.sh's is, and answers once its runner is ready.
func startHelper(t *testing.T, mode string) *helperRun {
	t.Helper()
	dir := t.TempDir()
	h := &helperRun{disk: filepath.Join(dir, "disk"), root: filepath.Join(dir, "root"), runLog: filepath.Join(dir, "run.log"), drained: make(chan struct{})}
	for _, d := range []string{h.disk, h.root} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			t.Fatal(err)
		}
	}
	h.cmd = exec.Command(os.Args[0])
	h.cmd.Env = append(os.Environ(), envHelper+"=sentinel", envDisk+"="+h.disk, envRoot+"="+h.root, envMode+"="+mode, envRunLog+"="+h.runLog)
	h.cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	stdout, err := h.cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	h.cmd.Stderr = h.cmd.Stdout
	if err := h.cmd.Start(); err != nil {
		t.Fatal(err)
	}
	ready := make(chan error, 1)
	go func() {
		defer close(h.drained)
		sc := bufio.NewScanner(stdout)
		sent := false
		for sc.Scan() {
			line := sc.Text()
			h.mu.Lock()
			h.out.WriteString(line + "\n")
			h.mu.Unlock()
			if !sent && strings.HasPrefix(line, "ready ") {
				_, err := fmt.Sscanf(line, "ready %d %d %d", &h.runner, &h.warden, &h.unit)
				ready <- err
				sent = true
			}
		}
		if !sent {
			ready <- io.ErrUnexpectedEOF
		}
	}()
	if err := <-ready; err != nil {
		t.Fatalf("the runner never said it was ready: %v; output:\n%s", err, h.output())
	}
	return h
}

// awaitGone waits for pids to exit, on the kernel's word.
func awaitGone(t *testing.T, pids ...int) {
	t.Helper()
	left, err := reap.OSTable{}.AwaitExit(pids, time.Minute)
	if err != nil || len(left) > 0 {
		t.Fatalf("pids %v never exited (err %v)", left, err)
	}
}

// assertNoSurvivor fails on any live process of the run's session or running
// out of its root.
func assertNoSurvivor(t *testing.T, h *helperRun) {
	t.Helper()
	procs, err := reap.OSTable{}.List()
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range procs {
		if p.State != 'Z' && (p.Sid == h.runner || strings.Contains(p.Line, h.root)) {
			t.Errorf("pid %d survived the run (session %d): %s", p.Pid, p.Sid, p.Line)
		}
	}
	if _, err := os.Stat(h.disk); !os.IsNotExist(err) {
		t.Errorf("the run's disk %s was not released (stat err = %v)", h.disk, err)
	}
}

func TestANormalRunEndsCleanlyAndRelaysItsRunner(t *testing.T) {
	// Arrange.
	h := startHelper(t, "normal")

	// Act.
	err := h.cmd.Wait()
	<-h.drained

	// Assert.
	if err != nil {
		t.Fatalf("the sentinel answered %v, want the runner's clean exit; output:\n%s", err, h.output())
	}
	awaitGone(t, h.runner, h.warden)
	assertNoSurvivor(t, h)
	if !strings.Contains(h.output(), "the run's session is ") {
		t.Fatalf("the runner's log was not relayed:\n%s", h.output())
	}
}

func TestKillingTheSentinelsGroupReapsTheRunAndReleasesItsDisk(t *testing.T) {
	// Arrange.
	h := startHelper(t, "hold")

	// Act: what a merge gate's cut does.
	if err := syscall.Kill(-h.cmd.Process.Pid, syscall.SIGKILL); err != nil {
		t.Fatal(err)
	}
	_ = h.cmd.Wait()

	// Assert.
	awaitGone(t, h.runner, h.warden)
	assertNoSurvivor(t, h)
	raw, err := os.ReadFile(h.runLog)
	if err != nil {
		t.Fatalf("the cut run left no log lines past its relay: %v", err)
	}
	for _, want := range []string{"the sentinel is gone (the run was cut)", fmt.Sprintf("reap run %d: freeze pass 1 stopped", h.runner), "the run was cut (its sentinel is gone)"} {
		if !strings.Contains(string(raw), want) {
			t.Errorf("the run log has no %q:\n%s", want, raw)
		}
	}
}

func TestKillingTheRunnerLeavesItsWardenToReapAndRelease(t *testing.T) {
	// Arrange.
	h := startHelper(t, "hold")

	// Act.
	if err := syscall.Kill(h.runner, syscall.SIGKILL); err != nil {
		t.Fatal(err)
	}
	err := h.cmd.Wait()
	<-h.drained

	// Assert.
	var exitErr *exec.ExitError
	if !asExit(err, &exitErr) || exitErr.ExitCode() != 128+int(syscall.SIGKILL) {
		t.Fatalf("the sentinel answered %v, want 137 for its killed runner", err)
	}
	awaitGone(t, h.warden)
	assertNoSurvivor(t, h)
	for _, want := range []string{fmt.Sprintf("the runner pid %d died without releasing its run", h.runner), fmt.Sprintf("reap dead run %d: freeze pass 1 stopped", h.runner), fmt.Sprintf("the warden of run %d reaped it and released what it held", h.runner)} {
		if !strings.Contains(h.output(), want) {
			t.Errorf("the relayed output has no %q:\n%s", want, h.output())
		}
	}
}

func asExit(err error, target **exec.ExitError) bool {
	e, ok := err.(*exec.ExitError)
	if ok {
		*target = e
	}
	return ok
}
