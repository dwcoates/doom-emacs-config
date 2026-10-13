// Package reap kills what a dead test run left running: the processes still
// running out of a directory the run owned (a RAM disk's mount, a harness's run
// root). The test runner's RAM disk reclaim and the daemon integration
// harness's run-root reclaim share it.
package reap

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
)

// Under SIGKILLs every process other than this one whose command line names a
// path under dir, answering how many it killed. A process gone before its
// kill counts as killed; any other failed kill is answered, never dropped.
func Under(dir string) (int, error) {
	out, err := exec.Command("ps", "-axo", "pid=,args=").Output()
	if err != nil {
		return 0, fmt.Errorf("list processes to reclaim %s: %w", dir, err)
	}
	killed := 0
	var errs []error
	for _, pid := range Matching(string(out), dir, os.Getpid()) {
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
			errs = append(errs, fmt.Errorf("kill leftover pid %d under %s: %w", pid, dir, err))
			continue
		}
		killed++
	}
	return killed, errors.Join(errs...)
}

// Matching answers the pids of the `ps -o pid=,args=` lines in ps that name a
// path under dir, leaving out self.
func Matching(ps, dir string, self int) []int {
	prefix := strings.TrimSuffix(dir, "/") + "/"
	var pids []int
	for _, line := range strings.Split(ps, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || !strings.Contains(line, prefix) {
			continue
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil || pid == self {
			continue
		}
		pids = append(pids, pid)
	}
	return pids
}
