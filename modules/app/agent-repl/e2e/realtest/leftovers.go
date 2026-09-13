//go:build realtest

package realtest

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// A REALTEST LEAVES THE OWNER'S STATE EXACTLY AS IT FOUND IT, and until
// 2026-09-13 it did not.
//
// Realtests 4 through 8 register, create and fork workspaces whose worktrees
// live under the RUN DIRECTORY. The worktree is removed when the run ends —
// the scratch repository is deleted, and eventually the whole run directory
// with it — but the daemon's registry row is a separate fact, and a row the
// product never forgot outlives every byte it names. The owner then sees, for
// hours afterwards, in a *Warnings* buffer of an editor nobody restarted:
//
//	workspace "workspace-c22fed997b234b27" cannot host a durable log sink
//	(registered-dir=.../scratch-repo-worktrees/workspace-c22fed997b234b27
//	[MISSING]); its records are written centrally
//
// which is the module correctly reporting a stale registry row that a realtest
// left behind. The defect is the row, not the warning.
//
// So this file is the ONE implementation of three questions, and every caller
// — the sweep's start check, its end check, `bin/realtest.sh
// --clean-leftovers`, and the act realtests' own final assertion — goes
// through it rather than spelling the same SQL and the same command file a
// second time:
//
//   - WHICH registry rows name a directory under a given prefix
//     (`LeftoverWorkspaces`);
//   - how to say so to a human (`DescribeLeftovers`);
//   - how to remove them THROUGH THE PRODUCT (`CleanLeftovers`) — close, then
//     forget, through the daemon's command-file ingress, never by writing to
//     the owner's database.
//
// NOTHING HERE EVER DELETES A ROW ITSELF. The registry is the daemon's, and a
// harness that edited it directly would be repairing the evidence rather than
// exercising the product's own removal path — the one the owner would use.

// leftoverModeEnv selects what the leftover driver does when
// `bin/realtest.sh` invokes it: "report" lists and answers non-zero when any
// row exists, "clean" removes what it can first and answers non-zero only for
// what survived.
const leftoverModeEnv = "AGENT_REPL_REALTEST_LEFTOVERS"

// leftoverPrefixEnv is the directory prefix a leftover row must name to count.
// The sweep's start check passes the whole realtest root; its end check passes
// just this run's directory.
const leftoverPrefixEnv = "AGENT_REPL_REALTEST_LEFTOVER_PREFIX"

// LeftoverRemedy is the ONE line every refusal ends with, so an operator never
// has to work out what to run.
const LeftoverRemedy = "bin/realtest.sh --clean-leftovers"

// leftoverCleanCeiling bounds how long one close or one forget may take to
// show up in the registry.
//
// It is wsActForgetCeiling's reasoning: the command-file ingress polls its
// directory every commandfile.DefaultInterval (250ms, daemon/internal
// /commandfile/api.go), and thirty seconds is that poll a hundred and twenty
// times over — generous past a slow sweep without turning an unreachable
// daemon into a run that hangs.
//
// A var rather than a const only so leftovers_test.go can shrink it: a unit
// test of "the daemon never answered" would otherwise wait the full ceiling
// out twice, and a test that slow is a test nobody runs.
var leftoverCleanCeiling = 30 * time.Second

// leftoverPollInterval is how often the registry is re-read while waiting. Each
// read is a snapshot copy of wsm.db (state.go says why), so it is not free.
// A var for the same reason leftoverCleanCeiling is one.
var leftoverPollInterval = 250 * time.Millisecond

// LeftoverRow is one registry row naming a directory under the prefix, with
// the one thing the removal path turns on: `Forget` refuses an OPEN workspace
// (daemon/internal/workspace/forget.go), so a row's closed state decides
// whether a close has to happen first.
type LeftoverRow struct {
	Workspace
	Closed bool
}

// UnderDir reports whether dir lies at or beneath prefix.
//
// Compared on CLEANED paths with a separator boundary, so
// `/a/realtest-2` is not read as living under `/a/realtest`, which a plain
// string prefix would say.
//
// BOTH SIDES ARE RESOLVED THROUGH THEIR DEEPEST SURVIVING ANCESTOR, and that
// is not a nicety. The case this function exists for is a row whose directory
// is GONE while the run directory above it still stands, and on macOS the run
// directory routinely stands under a symlink (`/tmp` is `/private/tmp`,
// `/var/folders/...` is `/private/var/folders/...`). Resolving only the paths
// that still exist would canonicalize the prefix and leave the missing row
// literal, and the two spellings of one directory would then compare unequal —
// the leftover would be reported as somebody else's and left in the registry,
// which is the whole defect this file is about.
func UnderDir(dir, prefix string) bool {
	d, root := resolveThroughAncestor(dir), resolveThroughAncestor(prefix)
	if d == "" || root == "" {
		return false
	}
	if d == root {
		return true
	}
	return strings.HasPrefix(d, root+string(filepath.Separator))
}

// resolveThroughAncestor canonicalizes as much of p as still exists and puts
// the missing tail back on the end.
//
// It walks up until `EvalSymlinks` answers, which for a path that is entirely
// gone stops at a directory that is not (`/` at worst), and rejoins the
// components it climbed past. A path that exists comes back exactly as
// `EvalSymlinks` gives it, so nothing about the ordinary case changes.
func resolveThroughAncestor(p string) string {
	if p == "" {
		return ""
	}
	p = filepath.Clean(p)
	var missing []string
	current := p
	for {
		if resolved, err := filepath.EvalSymlinks(current); err == nil {
			return filepath.Join(append([]string{resolved}, missing...)...)
		}
		parent := filepath.Dir(current)
		if parent == current {
			// Nothing along the path exists, so there is nothing to resolve
			// against and the cleaned spelling is the best answer available.
			return p
		}
		missing = append([]string{filepath.Base(current)}, missing...)
		current = parent
	}
}

// LeftoverWorkspaces returns every registry row, open or closed, whose
// directory lies under prefix.
//
// CLOSED ROWS COUNT. A closed workspace is still a registry row naming a
// deleted directory, it is still what the module's stale-registration warning
// fires on, and it is still something the owner has to remove by hand. "Left
// the state as it was found" admits no closed-row exception.
func LeftoverWorkspaces(ctx context.Context, dbPath, prefix string) ([]LeftoverRow, error) {
	open, closed, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		return nil, fmt.Errorf("read the registry to look for rows under %s: %w", prefix, err)
	}
	var rows []LeftoverRow
	for _, ws := range open {
		if UnderDir(ws.Dir, prefix) {
			rows = append(rows, LeftoverRow{Workspace: ws, Closed: false})
		}
	}
	for _, ws := range closed {
		if UnderDir(ws.Dir, prefix) {
			rows = append(rows, LeftoverRow{Workspace: ws, Closed: true})
		}
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].ID < rows[j].ID })
	return rows, nil
}

// DescribeLeftovers renders the rows the way every refusal reports them: one
// line per row, naming the id, the name, the directory, whether that directory
// still exists, and whether the row is open or closed.
//
// Whether the directory is still there is printed because it is the difference
// between a row the owner might still want (its worktree is on disk) and a row
// that can only ever produce the stale-registration warning.
func DescribeLeftovers(prefix string, rows []LeftoverRow) string {
	var b strings.Builder
	fmt.Fprintf(&b, "%d workspace row(s) in the registry name a directory under %s:\n", len(rows), prefix)
	for _, row := range rows {
		state := "open"
		if row.Closed {
			state = "closed"
		}
		onDisk := "MISSING"
		if info, err := os.Stat(row.Dir); err == nil && info.IsDir() {
			onDisk = "on disk"
		}
		fmt.Fprintf(&b, "  %s  %s  (%s, %s)  %s\n", row.ID, orUnknown(row.Name), state, onDisk, row.Dir)
	}
	return b.String()
}

// WriteWorkspaceCommand asks the daemon to run one workspace verb on one
// workspace, through the command-file ingress, and answers the path it wrote.
//
// THE COMMAND FILE IS A REAL PRODUCTION INGRESS, not a side channel invented
// for a test: it is the same door `agent-repl workspace-dispatch` scripts
// write through, swept by daemon/internal/commandfile/ingress.go, whose
// `apply` maps each entry onto the same internal verb the equivalent rpc
// reaches. `forget` has no rpc arm at all (that needs a proto addition nobody
// has made), so for the one verb that actually removes a registry record this
// is the only door there is.
//
// The name must match the daemon's own glob, `workspace_commands_*.json`
// (daemon/internal/stateroot/stateroot.go, CommandFileGlob), or nothing ever
// reads it.
func WriteWorkspaceCommand(stateDir, verb, id string) (string, error) {
	dir := filepath.Join(stateDir, "output")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create the command-file ingress directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, fmt.Sprintf("workspace_commands_realtest-%s-%d.json", verb, time.Now().UnixNano()))
	entry, err := json.Marshal([]map[string]string{{"type": verb, "workspace": id}})
	if err != nil {
		return "", fmt.Errorf("render a %s command for %s: %w", verb, id, err)
	}
	if err := os.WriteFile(path, entry, 0o644); err != nil {
		return "", fmt.Errorf("write the command file %s: %w", path, err)
	}
	return path, nil
}

// CleanLeftovers removes every row under prefix THROUGH THE DAEMON and answers
// what survived.
//
// CLOSE, THEN FORGET, IN THAT ORDER, per row: `Forget` refuses an open
// workspace, so a row that is still open is closed first and the close is
// waited for in the registry before the forget is even requested.
//
// It reports progress through `log` rather than through a testing.T, because
// its callers are a test, a sweep-end hook and an operator command, and only
// one of those has a T. An error answer is reserved for "the registry could
// not be read at all"; a row the daemon declined to forget is not an error but
// a SURVIVOR, returned so the caller can fail on it with the evidence in hand.
func CleanLeftovers(ctx context.Context, dbPath, stateDir, prefix string, log func(string)) ([]LeftoverRow, error) {
	if log == nil {
		log = func(string) {}
	}
	rows, err := LeftoverWorkspaces(ctx, dbPath, prefix)
	if err != nil {
		return nil, err
	}
	if len(rows) == 0 {
		return nil, nil
	}
	log(fmt.Sprintf("removing %d leftover workspace row(s) under %s through the daemon", len(rows), prefix))

	for _, row := range rows {
		if !row.Closed {
			path, err := WriteWorkspaceCommand(stateDir, "close", row.ID)
			if err != nil {
				log(fmt.Sprintf("could not ask the daemon to close %s (%s): %v", row.ID, row.Name, err))
				continue
			}
			log(fmt.Sprintf("asked the daemon to close %s (%s) through %s", row.ID, row.Name, path))
			if !pollUntil(ctx, leftoverCleanCeiling, leftoverPollInterval, func() bool {
				return leftoverRowIsClosed(ctx, dbPath, row.ID)
			}) {
				log(fmt.Sprintf("%s (%s) did not read back closed within %s, so no forget was requested for it: "+
					"forget refuses an open workspace", row.ID, row.Name, leftoverCleanCeiling))
				continue
			}
		}
		path, err := WriteWorkspaceCommand(stateDir, "forget", row.ID)
		if err != nil {
			log(fmt.Sprintf("could not ask the daemon to forget %s (%s): %v", row.ID, row.Name, err))
			continue
		}
		log(fmt.Sprintf("asked the daemon to forget %s (%s) through %s", row.ID, row.Name, path))
		if !pollUntil(ctx, leftoverCleanCeiling, leftoverPollInterval, func() bool {
			return !leftoverRowExists(ctx, dbPath, row.ID)
		}) {
			log(fmt.Sprintf("%s (%s) is still in the registry %s after the forget was requested",
				row.ID, row.Name, leftoverCleanCeiling))
		}
	}

	remaining, err := LeftoverWorkspaces(ctx, dbPath, prefix)
	if err != nil {
		return nil, err
	}
	return remaining, nil
}

// leftoverRowIsClosed answers whether the registry now holds id as closed. A
// read that fails answers false rather than propagating: the caller is polling,
// and a transient snapshot failure is a reason to look again, not a verdict.
func leftoverRowIsClosed(ctx context.Context, dbPath, id string) bool {
	_, closed, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		return false
	}
	for _, ws := range closed {
		if ws.ID == id {
			return true
		}
	}
	return false
}

// leftoverRowExists answers whether the registry still holds id at all, open or
// closed. A read that fails answers TRUE — the opposite default to
// leftoverRowIsClosed, and deliberately so: this one gates "the row is gone",
// and a failed read must never be reported as a removal.
func leftoverRowExists(ctx context.Context, dbPath, id string) bool {
	open, closed, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		return true
	}
	for _, list := range [][]Workspace{open, closed} {
		for _, ws := range list {
			if ws.ID == id {
				return true
			}
		}
	}
	return false
}

// pollUntil re-reads a predicate until it holds or the ceiling expires, and
// answers whether it held. It exists because this file's callers include a
// plain command with no testing.T to hand to `waitUntil`.
func pollUntil(ctx context.Context, ceiling, interval time.Duration, predicate func() bool) bool {
	deadline := time.Now().Add(ceiling)
	for {
		if predicate() {
			return true
		}
		if time.Now().After(deadline) {
			return false
		}
		select {
		case <-ctx.Done():
			return false
		case <-time.After(interval):
		}
	}
}
