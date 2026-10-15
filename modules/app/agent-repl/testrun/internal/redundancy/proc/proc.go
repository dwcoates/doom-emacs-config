// Package proc runs the processes every `testrun redundancy` adapter runs:
// each bounded, its whole group ended at its bound, its merged output kept
// line by line with when each line arrived, and its CPU counted.
package proc

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"
)

// Cmd is one process the measurement runs.
type Cmd struct {
	Dir  string
	Path string
	Args []string
	// Env is added to the measurement's own environment.
	Env []string
	// Timeout bounds the process; zero is refused, every wait is bounded.
	Timeout time.Duration
}

// Line is one line of a process's merged stdout and stderr, stamped with
// when it arrived relative to the process's start.
type Line struct {
	At   time.Duration
	Text string
}

// Result is how a process ended.
type Result struct {
	Lines    []Line
	Exit     int
	TimedOut bool
	Wall     time.Duration
	CPU      time.Duration
}

// Text is the process's whole output.
func (r Result) Text() string {
	var b strings.Builder
	for _, l := range r.Lines {
		b.WriteString(l.Text)
		b.WriteByte('\n')
	}
	return b.String()
}

// Exec runs one Cmd. An error is a process that could not be run at all; a
// process that ran and failed is a Result with its exit status.
type Exec func(ctx context.Context, c Cmd) (Result, error)

// OSExec is the real Exec. The process leads its own group, so a timeout
// ends everything it started.
func OSExec(ctx context.Context, c Cmd) (Result, error) {
	if c.Timeout <= 0 {
		return Result{}, fmt.Errorf("proc: %s has no timeout", c.Path)
	}
	ctx, cancel := context.WithTimeout(ctx, c.Timeout)
	defer cancel()
	cmd := exec.Command(c.Path, c.Args...)
	cmd.Dir = c.Dir
	cmd.Env = append(os.Environ(), c.Env...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	pr, pw, err := os.Pipe()
	if err != nil {
		return Result{}, fmt.Errorf("proc: pipe for %s: %w", c.Path, err)
	}
	cmd.Stdout, cmd.Stderr = pw, pw
	start := time.Now()
	if err := cmd.Start(); err != nil {
		pr.Close()
		pw.Close()
		return Result{}, fmt.Errorf("proc: start %s %s: %w", c.Path, strings.Join(c.Args, " "), err)
	}
	pw.Close()
	lines := make(chan []Line, 1)
	go func() { lines <- readLines(pr, start) }()
	waited := make(chan error, 1)
	go func() { waited <- cmd.Wait() }()
	var res Result
	select {
	case err = <-waited:
	case <-ctx.Done():
		res.TimedOut = true
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		err = <-waited
	}
	res.Wall = time.Since(start)
	// Whatever the process left behind in its group holds the pipe open; end
	// it, so the read below always ends.
	_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
	res.Lines = <-lines
	pr.Close()
	if ps := cmd.ProcessState; ps != nil {
		res.CPU = ps.UserTime() + ps.SystemTime()
		res.Exit = ps.ExitCode()
	}
	var exitErr *exec.ExitError
	if err != nil && !errors.As(err, &exitErr) {
		return res, fmt.Errorf("proc: wait for %s: %w", c.Path, err)
	}
	return res, nil
}

func readLines(r io.Reader, start time.Time) []Line {
	var out []Line
	br := bufio.NewReader(r)
	for {
		s, err := br.ReadString('\n')
		if s != "" {
			out = append(out, Line{At: time.Since(start), Text: strings.TrimRight(s, "\n")})
		}
		if err != nil {
			return out
		}
	}
}

// Counter runs processes through Exec, counting each and its CPU, and ends
// a cancelled measurement: a process an interrupt cut short would
// otherwise read as a hang, and a hang as a kill.
type Counter struct {
	Exec Exec
	// Name prefixes its errors (the adapter's package).
	Name      string
	Processes int
	CPU       time.Duration
}

// Run runs one process.
func (c *Counter) Run(ctx context.Context, cmd Cmd) (Result, error) {
	if err := ctx.Err(); err != nil {
		return Result{}, fmt.Errorf("%s: interrupted: %w", c.Name, err)
	}
	res, err := c.Exec(ctx, cmd)
	c.Processes++
	c.CPU += res.CPU
	if err == nil && ctx.Err() != nil {
		return Result{}, fmt.Errorf("%s: interrupted during %s: %w", c.Name, filepath.Base(cmd.Path), ctx.Err())
	}
	return res, err
}
