// Package locktest runs a second process for a test of a record lock. A
// record lock's guarantees are about OTHER processes (this process's own
// holders never conflict with each other, and recordlock reads its own gate
// before the kernel), so a test asks another process: its own test binary
// re-executed as the package's helper test, which does what its mode names,
// prints one line, and holds until its stdin closes.
//
// A test package names its helper test and gives Run its modes:
//
//	func TestLockHelperProcess(t *testing.T) { locktest.Run(helperEnv, modes) }
//
// and starts one with Start(t, "TestLockHelperProcess", helperEnv, mode, path).
package locktest

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"
)

// Mode is what a helper process does with PATH. A mode that answers its own
// line and exits calls os.Exit itself; one that returns nil holds what it
// took, printed "ready <pid>", until its stdin closes. An error exits the
// helper with status 3, the error on its stderr.
type Mode func(path string) error

// Run is the body of a helper test: in a process Start ran (ENV set), it
// runs the mode ENV names on the path ENV_PATH names and never returns; in
// any other run it returns at once.
func Run(env string, modes map[string]Mode) {
	name := os.Getenv(env)
	if name == "" {
		return
	}
	mode, ok := modes[name]
	if !ok {
		fail(fmt.Errorf("unknown helper mode %q", name))
	}
	if err := mode(os.Getenv(env + "_PATH")); err != nil {
		fail(err)
	}
	Hold()
	os.Exit(0)
}

// Hold prints "ready <pid>" and blocks until the helper's stdin closes: the
// end of a holding mode, which Run runs after it.
func Hold() {
	fmt.Printf("ready %d\n", os.Getpid())
	_, _ = io.Copy(io.Discard, os.Stdin)
}

func fail(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(3)
}

// Process is a running helper and the first line it printed.
type Process struct {
	Cmd   *exec.Cmd
	Stdin io.WriteCloser
	Line  string
}

// Start runs this test binary as its helper test HELPER, in MODE on PATH
// (ENV and ENV_PATH), and reads its first line. A holding helper is let go
// at the end of the test.
func Start(t testing.TB, helper, env, mode, path string) *Process {
	t.Helper()
	cmd := exec.Command(os.Args[0], "-test.run=^"+helper+"$")
	cmd.Env = append(os.Environ(), env+"="+mode, env+"_PATH="+path)
	cmd.Stderr = os.Stderr
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("helper stdin: %v", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("helper stdout: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the helper: %v", err)
	}
	p := &Process{Cmd: cmd, Stdin: stdin}
	t.Cleanup(func() {
		p.Stdin.Close()
		_ = p.Cmd.Wait()
	})
	line, err := bufio.NewReader(stdout).ReadString('\n')
	if err != nil {
		t.Fatalf("the helper (%s) printed nothing: %v", mode, err)
	}
	p.Line = strings.TrimSpace(line)
	return p
}

// Field answers the integer after WORD in the helper's line.
func (p *Process) Field(t testing.TB, word string) int {
	t.Helper()
	n, err := field(p.Line, word)
	if err != nil {
		t.Fatal(err)
	}
	return n
}

// field answers the integer after WORD in LINE.
func field(line, word string) (int, error) {
	parts := strings.Fields(line)
	for i := 0; i+1 < len(parts); i++ {
		if parts[i] == word {
			n, err := strconv.Atoi(parts[i+1])
			if err != nil {
				return 0, fmt.Errorf("helper line %q: %w", line, err)
			}
			return n, nil
		}
	}
	return 0, fmt.Errorf("helper line %q names no %q", line, word)
}
