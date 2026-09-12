//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// WHAT REALTESTS 7 AND 8 NEED THAT THE WORKSPACE-ACT SUBSTRATE DOES NOT CARRY.
//
// realtest_workspace_acts_test.go is the substrate: the scratch repository, the
// chord proof, the acts entered through `call-interactively`, the roster and
// tab-order probes, the state-database polling and the cleanups. Realtests 7
// and 8 use all of it and add nothing to it, because it has other authors.
//
// What is left over is three things it has no reason to hold, all read-only:
//
//   - Columns of `wsm.db` no realtest before these two asserted on:
//     `ported_prompts` (a fork's inherited conversation), `turns` (a
//     workspace's own prompts), `sessions` (the vendor identity, and the pid
//     and terminal a kill leaves behind), and `workspaces.parent_id` and
//     `priority`. `ReadWorkspaces` reads four columns and is right not to grow.
//   - The orphan scan a kill has to pass: whether a shim is still listening for
//     a workspace, and whether anything still answers on its sockets.
//   - The one precondition realtest 7 cannot arrange for itself, the shim's
//     offline fake.
//
// Everything is prefixed `rt78` so it cannot collide with a parallel author's
// name, and nothing here writes anything anywhere.

// ---- Reading wsm.db, read-only --------------------------------------------

// rt78Query runs one read-only query against the state database.
//
// `-readonly` for the reason state.go gives: this is the owner's live database
// with the daemon holding it open in WAL mode, and a reader that opened it
// read-write would create or touch `-wal` and `-shm` beside it. The field
// separator is the substrate's unit separator, for the reason state.go gives
// too: a prompt's own text may legitimately contain a pipe.
func rt78Query(ctx context.Context, dbPath, query string) ([][]string, error) {
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	cmd := exec.CommandContext(callCtx, "sqlite3", "-readonly", "-separator", wsActFieldSep, dbPath, query)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("query %s: %w; sqlite3 said: %s", dbPath, err, strings.TrimSpace(string(out)))
	}
	var rows [][]string
	for _, line := range strings.Split(strings.TrimRight(string(out), "\n"), "\n") {
		if line == "" {
			continue
		}
		rows = append(rows, strings.Split(line, wsActFieldSep))
	}
	return rows, nil
}

// rt78Quote renders one SQL string literal.
//
// Every value these tests interpolate is a daemon-minted 16-hex id, and the
// quoting is done anyway: a helper that is safe only because of what its
// callers happen to pass is one caller away from not being safe.
func rt78Quote(value string) string {
	return "'" + strings.ReplaceAll(value, "'", "''") + "'"
}

// rt78Facts is the part of a `workspaces` row that `Workspace` does not carry.
//
// `parent_id` is the only column on that table recording a fork's parentage,
// and it cannot carry a fork verdict on its own: there is no fork column, and
// a plain child (`SPC TAB n` with a prefix argument) sets it identically,
// because the daemon's CreateSpec makes ForkFrom imply Parent. What separates
// a fork from a plain child is `ported_prompts`.
//
// `priority` is the stored enum ordinal (0 = P0.5, 1 = P1, 2 = P2, 3 = P3),
// carried as text so UNSET is a value this reader can report rather than a
// zero it would have to disambiguate from P0.5.
type rt78Facts struct {
	ID       string
	Branch   string
	ParentID string
	Priority string
	Closed   bool
}

// rt78ReadFacts returns those columns for every workspace, keyed by id.
func rt78ReadFacts(ctx context.Context, dbPath string) (map[string]rt78Facts, error) {
	rows, err := rt78Query(ctx, dbPath,
		`SELECT id, branch, COALESCE(parent_id,''), COALESCE(CAST(priority AS TEXT),''), closed
		 FROM workspaces ORDER BY id;`)
	if err != nil {
		return nil, err
	}
	out := make(map[string]rt78Facts, len(rows))
	for i, fields := range rows {
		if len(fields) != 5 {
			return nil, fmt.Errorf("row %d of `workspaces` in %s has %d fields, not 5: %q",
				i+1, dbPath, len(fields), strings.Join(fields, "|"))
		}
		out[fields[0]] = rt78Facts{
			ID: fields[0], Branch: fields[1], ParentID: fields[2],
			Priority: fields[3], Closed: fields[4] == "1",
		}
	}
	return out, nil
}

// rt78Prompt is one row of a conversation, from either `ported_prompts` or
// `turns`. The two tables carry the same three facts these tests compare on
// (which turn, what was said, where it came from), so one shape reads both.
type rt78Prompt struct {
	Turn    string
	Ordinal string
	Text    string
	Origin  string
}

// rt78PortedPrompts is a workspace's INHERITED conversation: the rows it
// carried over from its parent when it was forked, oldest first.
//
// THIS IS A FORK'S EVIDENCE, and it is the daemon's own expression of it. A
// workspace has rows here if and only if it was forked: `PutPortedPrompts` is
// written from exactly one place, the fork path
// (daemon/internal/workspace/forkconversation.go), in one all-or-nothing
// transaction, because "half a ported conversation is a feed that shows some
// of the parent's questions and not others".
func rt78PortedPrompts(ctx context.Context, dbPath, id string) ([]rt78Prompt, error) {
	rows, err := rt78Query(ctx, dbPath, fmt.Sprintf(
		`SELECT turn_id, ordinal, text, origin FROM ported_prompts
		 WHERE workspace_id = %s ORDER BY ordinal, turn_id;`, rt78Quote(id)))
	if err != nil {
		return nil, err
	}
	return rt78ScanPrompts(rows, "ported_prompts", dbPath)
}

// rt78Turns is a workspace's OWN prompts, in the order the daemon's
// `ConversationPrompts` reads them.
func rt78Turns(ctx context.Context, dbPath, id string) ([]rt78Prompt, error) {
	rows, err := rt78Query(ctx, dbPath, fmt.Sprintf(
		`SELECT id, started_at, text, origin FROM turns
		 WHERE workspace_id = %s ORDER BY started_at, id;`, rt78Quote(id)))
	if err != nil {
		return nil, err
	}
	return rt78ScanPrompts(rows, "turns", dbPath)
}

func rt78ScanPrompts(rows [][]string, table, dbPath string) ([]rt78Prompt, error) {
	out := make([]rt78Prompt, 0, len(rows))
	for i, fields := range rows {
		if len(fields) != 4 {
			return nil, fmt.Errorf("row %d of `%s` in %s has %d fields, not 4: %q",
				i+1, table, dbPath, len(fields), strings.Join(fields, "|"))
		}
		out = append(out, rt78Prompt{Turn: fields[0], Ordinal: fields[1], Text: fields[2], Origin: fields[3]})
	}
	return out, nil
}

// rt78Conversation is what A FORK OF THIS WORKSPACE would inherit: everything
// the workspace itself inherited, followed by every prompt of its own.
//
// It is the Go reading of the daemon's own `ConversationPrompts`
// (daemon/internal/wsm/portedprompts.go), spelled out rather than approximated
// by reading `turns` alone, for the reason that function gives: a fork of a
// fork must carry the grandparent's questions too, and reading only `turns`
// truncates the conversation at each generation.
func rt78Conversation(ctx context.Context, dbPath, id string) ([]rt78Prompt, error) {
	inherited, err := rt78PortedPrompts(ctx, dbPath, id)
	if err != nil {
		return nil, err
	}
	own, err := rt78Turns(ctx, dbPath, id)
	if err != nil {
		return nil, err
	}
	return append(inherited, own...), nil
}

// rt78PromptTexts is a conversation reduced to what was said, in order: the
// comparison a fork assertion actually makes.
func rt78PromptTexts(prompts []rt78Prompt) []string {
	out := make([]string, 0, len(prompts))
	for _, p := range prompts {
		out = append(out, p.Text)
	}
	return out
}

// rt78Session is the part of `sessions` these two realtests read: the vendor
// identity a fork must not share with its parent, and the pid and terminal a
// kill has to leave behind.
type rt78Session struct {
	Exists       bool
	VendorID     string
	ShimPID      string
	TerminalKind string
}

// rt78ReadSession returns a workspace's session row, if it has one.
func rt78ReadSession(ctx context.Context, dbPath, id string) (rt78Session, error) {
	rows, err := rt78Query(ctx, dbPath, fmt.Sprintf(
		`SELECT vendor_session_id, COALESCE(CAST(shim_pid AS TEXT),''), COALESCE(terminal_kind,'')
		 FROM sessions WHERE workspace_id = %s;`, rt78Quote(id)))
	if err != nil {
		return rt78Session{}, err
	}
	if len(rows) == 0 {
		return rt78Session{}, nil
	}
	if len(rows[0]) != 3 {
		return rt78Session{}, fmt.Errorf("the `sessions` row for %s in %s has %d fields, not 3",
			id, dbPath, len(rows[0]))
	}
	return rt78Session{Exists: true, VendorID: rows[0][0], ShimPID: rows[0][1], TerminalKind: rows[0][2]}, nil
}

// ---- The orphan scan ------------------------------------------------------

// rt78SockDir is where the daemon puts per-workspace shim sockets:
// `<state dir>/sock`, per daemon/internal/stateroot/stateroot.go.
func rt78SockDir(stateDir string) string { return filepath.Join(stateDir, "sock") }

// rt78ShimsFor returns every LIVE shim process listening on a socket that
// belongs to one workspace.
//
// The socket names are `<workspace id>.sock` for a workspace's first shim and
// `<workspace id>.nN.sock` for each relaunched generation (`Layout.ShimSocket`
// and `Fleet.freshSocketPath`), so the match is on the id plus a dot rather
// than on one exact path: a kill that left the SECOND generation running would
// pass an exact-path check, and that is exactly the orphan the check exists to
// catch.
//
// The process pattern is the one bin/realtest.sh already refuses on, so the
// script's preflight and this assertion cannot disagree about what a shim is.
func rt78ShimsFor(ctx context.Context, stateDir, workspaceID string) ([]string, error) {
	callCtx, cancel := context.WithTimeout(ctx, 20*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "pgrep", "-f", `shim/dist/main\.js`).Output()
	if err != nil {
		// pgrep exits non-zero when nothing matches, which is the healthy
		// answer here and not a failure to report.
		return nil, nil
	}
	prefix := filepath.Join(rt78SockDir(stateDir), workspaceID) + "."
	var found []string
	for _, pid := range strings.Fields(string(out)) {
		psCtx, psCancel := context.WithTimeout(ctx, 10*time.Second)
		line, psErr := exec.CommandContext(psCtx, "ps", "-ww", "-o", "command=", "-p", pid).Output()
		psCancel()
		if psErr != nil {
			continue
		}
		if strings.Contains(string(line), prefix) {
			found = append(found, fmt.Sprintf("pid %s: %s", pid, strings.TrimSpace(string(line))))
		}
	}
	return found, nil
}

// rt78WorkspaceSockets lists every socket node under the state directory that
// belongs to one workspace, live or stale.
func rt78WorkspaceSockets(stateDir, workspaceID string) ([]string, error) {
	entries, err := os.ReadDir(rt78SockDir(stateDir))
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("enumerate the shim sockets under %s: %w", rt78SockDir(stateDir), err)
	}
	var out []string
	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), workspaceID+".") {
			out = append(out, filepath.Join(rt78SockDir(stateDir), entry.Name()))
		}
	}
	return out, nil
}

// rt78SocketAnswers reports whether anything is listening on a socket path.
//
// THE FILE'S ABSENCE IS NOT THE CONTRACT, and asserting on it would be wrong. A
// kill stops the process; nothing unlinks the socket node, which is cleared on
// the NEXT spawn or at boot (`shimsocket.ClearStale`). So a stale `<id>.sock`
// left on disk after a kill is correct behavior, and the question that
// separates a clean kill from an orphan is whether a connect is answered or
// refused. This asks the kernel that question directly.
func rt78SocketAnswers(path string) bool {
	conn, err := net.DialTimeout("unix", path, 3*time.Second)
	if err != nil {
		return false
	}
	_ = conn.Close()
	return true
}

// rt78ProcessAlive reports whether one pid is still running.
//
// `kill -0` rather than a process-table read: it asks the kernel the exact
// question, and a shim that exited a millisecond ago is gone from it at once.
func rt78ProcessAlive(ctx context.Context, pid string) bool {
	if pid == "" || pid == "0" {
		return false
	}
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	return exec.CommandContext(callCtx, "kill", "-0", pid).Run() == nil
}

// ---- The one precondition a run cannot arrange for itself ------------------

// rt78FakeShimsEnv is the daemon's shim-only fake hook
// (daemon/cmd/claude-repld/graph.go, `FakeShimsEnv`), which makes every shim
// spawn carry `--fake` and answer from the offline scripted SDK.
//
// REALTEST 7 CANNOT RUN WITHOUT IT. It has to give a workspace a conversation
// before it can fork one, because the daemon refuses a fork whose parent has
// none (`fork_parent_has_no_conversation`), and submitting a prompt is what
// reaches the vendor: the shim's single chokepoint, `importRealSDK`, is
// entered from `createRealQuery`, and with AGENT_REPL_FORBID_VENDOR_CALLS=1
// set it THROWS there. That is the guard working exactly as designed, and the
// fake is how a realtest gets an answered prompt without a real call.
//
// The launcher states AGENT_REPL_FORBID_VENDOR_CALLS on the Emacs process and
// nothing else (launch.go, `openBackgroundArgs`), and `open` hands the
// application to launchd, which does not pass this process's environment
// along, so the hook cannot be exported from here: it has to be stated with
// `--env` on the launch the same way the vendor guard is. That is a change to
// a shared file every realtest goes through, so this is a CHECK and not a
// workaround. The lead rules on whether the launcher states the hook always or
// only for the realtests that need an answered prompt.
const rt78FakeShimsEnv = "AGENT_REPL_FAKE_SHIMS"

// rt78RequireFakeShims fails the run when the Emacs process does not carry the
// hook.
//
// It reads the KERNEL'S copy of the environment (`ps -Eww`, through
// ProcessEnvironment) for the same reason verifyVendorGuard does: what the
// launcher intended and what the process holds are different facts, and only
// the second decides whether a prompt will be answered.
func rt78RequireFakeShims(ctx context.Context, t *testing.T, client *Client) {
	t.Helper()
	pid, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Fatalf("read the Emacs pid to check the shim fake hook: %v", err)
	}
	env, err := ProcessEnvironment(ctx, pid)
	if err != nil {
		t.Fatalf("read the Emacs process (pid %d) environment to check the shim fake hook: %v", pid, err)
	}
	value := env[rt78FakeShimsEnv]
	if value == "" || value == "0" || strings.EqualFold(value, "false") {
		t.Fatalf("the Emacs process (pid %d) does not carry %s, so no prompt can be answered in this run: "+
			"the shim reaches the vendor at `createRealQuery` and %s=1 makes its guard throw there. "+
			"This realtest must give a workspace a conversation before it can fork one, because the daemon "+
			"refuses a fork whose parent has none, so it cannot run as launched. "+
			"The remedy is on the launch, not here: `open` hands Emacs to launchd, which does not inherit "+
			"this process's environment, so %s=1 has to be stated with `--env` in openBackgroundArgs "+
			"(launch.go) beside %s=1. That file is shared by every realtest, so this test does not change "+
			"it, and the lead rules on whether the launcher states the hook always or per test",
			pid, rt78FakeShimsEnv, vendorGuardEnv, rt78FakeShimsEnv, vendorGuardEnv)
	}
	t.Logf("the Emacs process (pid %d) carries %s=%s, so a submitted prompt is answered offline",
		pid, rt78FakeShimsEnv, value)
}
