//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// WHAT THE RUN IS ANSWERABLE FOR is whatever the state database holds: realtest
// 1 asserts that every workspace in it is drawn, and lists them with times. So
// the expected set comes from the database rather than from Emacs — asking
// Emacs which workspaces it drew and then checking that it drew them would be
// asserting the frontend against itself.
//
// It is read through the `sqlite3` CLI with `-readonly`, and that is deliberate
// rather than convenient. This is the OWNER'S live state database with the
// daemon holding it open in WAL mode; a reader that opened it read-write would
// create or touch `-wal` and `-shm` beside it, and a realtest may not write a
// byte of the owner's state. `-readonly` cannot.

// StateDBPath is the workspace state database under the state root.
func StateDBPath(stateDir string) string { return stateDir + "/wsm.db" }

// ReadWorkspaces returns every workspace the state database holds.
//
// `closed` is carried through rather than filtered here: a closed workspace
// legitimately has no tab, so the caller needs the distinction to know what to
// assert, and a helper that quietly dropped them would make a missing tab
// indistinguishable from a closed workspace.
func ReadWorkspaces(ctx context.Context, dbPath string) (open []Workspace, closed []Workspace, err error) {
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()

	// The separator is a unit separator rather than the default pipe: a
	// workspace name or directory containing a pipe would otherwise split into
	// the wrong number of fields and be reported as a malformed row.
	cmd := exec.CommandContext(callCtx, "sqlite3", "-readonly", "-separator", "\x1f",
		dbPath, "SELECT id, dir, name, closed FROM workspaces ORDER BY id;")
	out, runErr := cmd.CombinedOutput()
	if runErr != nil {
		return nil, nil, fmt.Errorf("read the workspaces from %s: %w; sqlite3 said: %s",
			dbPath, runErr, strings.TrimSpace(string(out)))
	}

	for i, line := range strings.Split(strings.TrimRight(string(out), "\n"), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		fields := strings.Split(line, "\x1f")
		if len(fields) != 4 {
			return nil, nil, fmt.Errorf("row %d of %s has %d fields, not 4: %q",
				i+1, dbPath, len(fields), line)
		}
		ws := Workspace{ID: fields[0], Dir: fields[1], Name: fields[2]}
		switch fields[3] {
		case "0":
			open = append(open, ws)
		case "1":
			closed = append(closed, ws)
		default:
			return nil, nil, fmt.Errorf("workspace %s has an unreadable `closed` value %q", ws.ID, fields[3])
		}
	}
	return open, closed, nil
}
