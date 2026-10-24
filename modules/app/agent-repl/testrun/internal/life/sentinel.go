package life

import (
	"errors"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"sync"
	"syscall"
)

// Sentinel starts the runner (name with args, EnvRole set to RoleRunner) in a
// session of its own, holding its lifeline for as long as this process lives,
// relays the runner's stdout and stderr to stdout and stderr, forwards SIGINT
// and SIGTERM to it, and answers its exit status: 128+N for a runner a
// signal N ended.
//
// The relay is copied until every writer of it is gone (the runner and the
// warden it started), so the run's cleanup lines reach the invoker before
// this process answers.
//
// EXTRA files reach the runner after its lifeline: the runner holds the
// lifeline as fd 3 and extra[i] as fd 4+i (the events stream, see
// EventsFD).
func Sentinel(name string, args []string, extra []*os.File, stdout, stderr io.Writer, log Logger) int {
	lifeR, lifeW, err := os.Pipe()
	if err != nil {
		log.Errorf("make the runner's lifeline: %v", err)
		return 1
	}
	// lifeW is never written: it is held, and the kernel closes it when this
	// process dies.
	defer lifeW.Close()
	outR, outW, err := os.Pipe()
	if err != nil {
		log.Errorf("make the runner's output relay: %v", err)
		return 1
	}
	errR, errW, err := os.Pipe()
	if err != nil {
		log.Errorf("make the runner's error relay: %v", err)
		return 1
	}
	cmd := exec.Command(name, args...)
	cmd.Env = append(os.Environ(), EnvRole+"="+RoleRunner)
	cmd.Stdout, cmd.Stderr = outW, errW
	cmd.ExtraFiles = append([]*os.File{lifeR}, extra...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	// The signals are taken before the runner exists, so one sent the instant
	// it starts is forwarded rather than ending this process unrelayed.
	signals := make(chan os.Signal, 4)
	signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(signals)
	startErr := cmd.Start()
	// The runner holds its copies now; the extra files are the invoker's to
	// close, and closing them here is what lets the reader see the runner's
	// end as the end of the stream.
	for _, f := range append([]*os.File{lifeR, outW, errW}, extra...) {
		f.Close()
	}
	if startErr != nil {
		outR.Close()
		errR.Close()
		log.Errorf("start the runner %s: %v", name, startErr)
		return 1
	}
	var relays sync.WaitGroup
	relay := func(dst io.Writer, src *os.File, what string) {
		defer relays.Done()
		defer src.Close()
		if _, err := io.Copy(dst, src); err != nil {
			log.Errorf("relay the runner's %s: %v", what, err)
		}
	}
	relays.Add(2)
	go relay(stdout, outR, "output")
	go relay(stderr, errR, "errors")
	exited := make(chan struct{})
	go func() {
		for {
			select {
			case sig := <-signals:
				if err := cmd.Process.Signal(sig); err != nil && !errors.Is(err, os.ErrProcessDone) {
					log.Errorf("forward %v to the runner pid %d: %v", sig, cmd.Process.Pid, err)
				}
			case <-exited:
				return
			}
		}
	}()
	waitErr := cmd.Wait()
	close(exited)
	relays.Wait()
	return exitCode(waitErr, cmd.Process.Pid, log)
}

// exitCode is the status the sentinel answers for its runner's end.
func exitCode(err error, pid int, log Logger) int {
	if err == nil {
		return 0
	}
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) {
		log.Errorf("wait for the runner pid %d: %v", pid, err)
		return 1
	}
	if ws, ok := exitErr.Sys().(syscall.WaitStatus); ok && ws.Signaled() {
		log.Errorf("the runner pid %d was ended by %v", pid, ws.Signal())
		return 128 + int(ws.Signal())
	}
	return exitErr.ExitCode()
}
