// holders.go — naming the process behind a held lock.
//
// The lock itself is deliberately contentless: it carries no pid, because a pid
// written into a file is bookkeeping that can go stale while the kernel's claim
// cannot. That is the right trade for the QUESTION the lock answers ("is anyone
// alive for this workspace?"), but it leaves a second question unanswered:
// WHICH process is the one squatting when a survivor holds a workspace and
// never dials the daemon in.
//
// The kernel already knows, and lsof(8) is how it is asked on macOS/BSD: the
// open file description holding the flock belongs to a process, and lsof lists
// it. That keeps the lock's no-stale-state property intact — nothing is
// recorded anywhere — while still letting a caller escalate against a named
// holder instead of an anonymous one.
package sessionlock

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

// WorkspaceLockHolders returns the pids of the live processes holding the
// workspace's lock, in the order lsof reports them.
//
// An empty result with a nil error means nobody holds it. An error means "I
// could not tell", which a caller must never read as "nobody" — the whole point
// of naming a holder is to act against it, and acting on a guess would kill the
// wrong process.
func WorkspaceLockHolders(cwd string) ([]int, error) {
	path, err := WorkspaceLockPath(cwd)
	if err != nil {
		return nil, err
	}
	return holdersAt(path)
}

// holdersAt is WorkspaceLockHolders against an explicit path.
func holdersAt(path string) ([]int, error) {
	if path == "" {
		return nil, fmt.Errorf("sessionlock: empty lock path")
	}
	if _, err := os.Stat(path); err != nil {
		if os.IsNotExist(err) {
			// No lock file: nobody has ever claimed this workspace.
			return nil, nil
		}
		return nil, fmt.Errorf("sessionlock: stat %s: %w", path, err)
	}

	// -t: terminal-free output, one pid per line. Exit status 1 with no output
	// is lsof's "no process matched", which is an empty answer, not a failure.
	out, err := exec.Command("lsof", "-t", path).Output()
	pids := parsePIDs(out)
	if err != nil && len(pids) == 0 {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) && exitErr.ExitCode() == 1 {
			return nil, nil
		}
		return nil, fmt.Errorf("sessionlock: listing holders of %s: %w", path, err)
	}
	return pids, nil
}

// parsePIDs reads lsof -t output, skipping anything that is not a pid rather
// than failing the whole probe over one unparsable line.
func parsePIDs(out []byte) []int {
	var pids []int
	scanner := bufio.NewScanner(strings.NewReader(string(out)))
	for scanner.Scan() {
		field := strings.TrimSpace(scanner.Text())
		if field == "" {
			continue
		}
		pid, err := strconv.Atoi(field)
		if err != nil || pid <= 0 {
			continue
		}
		pids = append(pids, pid)
	}
	return pids
}
