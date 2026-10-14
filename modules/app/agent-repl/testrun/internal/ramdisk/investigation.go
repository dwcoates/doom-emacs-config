package ramdisk

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/testrun/internal/command"
)

// InvestigationName is the investigation workspace's name: stable, so a run
// can tell that one is already open. The workspace CLI decorates it with a
// short suffix, and may prefix it, so a branch whose last path segment starts
// with it is the investigation's.
const InvestigationName = "ramdisk-leak-investigation"

// EnvDispatch overrides the workspace CLI the investigation is created
// through: the /create-or-update-workspace skill's run.sh.
const EnvDispatch = "AGENT_REPL_TEST_WORKSPACE_DISPATCH"

// EnvRunLog names the run's full log file; bin/test-all.sh exports it.
const EnvRunLog = "AGENT_REPL_TEST_RUN_LOG"

// GitBound is how long `git worktree list --porcelain` may run: measured on
// the owner's host (2026-10-12) at 19-23ms warm and 142ms cold, and just over
// twice the cold one.
const GitBound = 300 * time.Millisecond

// DispatchBound is how long the workspace CLI's dispatch may run: its
// --dry-run (the same transform, without the file write) measured 348-559ms on
// the owner's host (2026-10-12), and this is just over twice the max.
const DispatchBound = 1200 * time.Millisecond

// Investigation is how a run opens the one workspace that investigates a RAM
// disk leak or a wedged disk manager.
type Investigation struct {
	// Module is the agent-repl module the run tests; its repository's main
	// worktree is where the workspace is cut.
	Module string
	// Git runs git within a bound.
	Git BoundedRunner
	// Dispatch hands one workspace-command array to the workspace CLI
	// (`run.sh --emit-commands`), within DispatchBound.
	Dispatch func(commands []byte) error
	// StateDir is the agent-repl state root: its daemon.addr says a daemon
	// serves, and its output/ is the daemon's command-file ingress, where a
	// dispatched create waits until the daemon applies it.
	StateDir string
	// RunLog is the run's log file; empty when the run was not told it.
	RunLog string
}

// InvestigationOutcome is what EnsureInvestigation did.
type InvestigationOutcome struct {
	// Existing names the investigation already open (a worktree, or a create
	// still waiting in the daemon's ingress); empty when one was dispatched.
	Existing string
	// Unlocked is set when the registry lock was not taken within its bound,
	// so the check for an open investigation ran without it.
	Unlocked bool
}

// EnsureInvestigation opens the investigation workspace for t, unless one is
// already open. It holds the registry lock throughout, so two runs that found
// the same trouble dispatch one workspace between them.
//
// OPEN MEANS EITHER OF TWO THINGS. A worktree on a branch of the
// investigation's name is open (closed workspaces keep their worktree too, so
// a closed, unmerged investigation also counts: it is reopened, not
// duplicated). And a create the daemon has not applied yet is open: it waits
// in the ingress (output/, then output/claimed/) until the worktree exists.
func (m Manager) EnsureInvestigation(inv Investigation, t *Trouble) (_ InvestigationOutcome, err error) {
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		return InvestigationOutcome{}, fmt.Errorf("make the RAM disk registry %s: %w", m.Registry, err)
	}
	// A STUCK REGISTRY LOCK STILL GETS ITS INVESTIGATION: the check for an
	// open one runs without the lock, and only two runs checking in the same
	// instant could both dispatch (the first's create is in the ingress before
	// its dispatch returns).
	var out InvestigationOutcome
	unlock, err := m.lockRegistry()
	var stuck *LockTimeoutError
	switch {
	case errors.As(err, &stuck):
		out.Unlocked = true
	case err != nil:
		return out, err
	default:
		defer releaseRegistry(unlock, &err)
	}
	porcelain, err := inv.Git(GitBound, "git", "-C", inv.Module, "worktree", "list", "--porcelain")
	if err != nil {
		return out, fmt.Errorf("list the repository's worktrees: %w", err)
	}
	root, open := readWorktrees(porcelain)
	if root == "" {
		return out, fmt.Errorf("git worktree list named no main worktree for %s", inv.Module)
	}
	if open != "" {
		out.Existing = open
		return out, nil
	}
	pending, err := pendingCreate(filepath.Join(inv.StateDir, "output"))
	if err != nil {
		return out, err
	}
	if pending != "" {
		out.Existing = pending
		return out, nil
	}
	addr := filepath.Join(inv.StateDir, "daemon.addr")
	if _, err := os.Stat(addr); err != nil {
		return out, fmt.Errorf("no agent-repl daemon serves to create the investigation workspace (%w)", err)
	}
	commands, err := json.Marshal([]createCommand{{
		Type:     "create",
		Name:     InvestigationName,
		GitRoot:  root,
		SourceWS: sourceWS{Name: filepath.Base(root), Path: root},
		Prompt:   investigationPrompt(t, inv.RunLog),
	}})
	if err != nil {
		return out, fmt.Errorf("encode the investigation workspace's create: %w", err)
	}
	if err := inv.Dispatch(commands); err != nil {
		return out, fmt.Errorf("dispatch the investigation workspace's create: %w", err)
	}
	return out, nil
}

// createCommand is one create entry of the workspace CLI's dispatch array.
type createCommand struct {
	Type     string   `json:"type"`
	Name     string   `json:"name"`
	GitRoot  string   `json:"git_root"`
	SourceWS sourceWS `json:"source_ws"`
	Prompt   string   `json:"prompt"`
}

type sourceWS struct {
	Name string `json:"name"`
	Path string `json:"path"`
}

// readWorktrees reads `git worktree list --porcelain`: the main worktree (the
// first listed) and the worktree on a branch of the investigation's name, if
// any, described for the log.
func readWorktrees(porcelain string) (root, open string) {
	var current string
	for _, line := range strings.Split(porcelain, "\n") {
		if path, ok := strings.CutPrefix(line, "worktree "); ok {
			current = path
			if root == "" {
				root = path
			}
			continue
		}
		branch, ok := strings.CutPrefix(line, "branch refs/heads/")
		if !ok {
			continue
		}
		if strings.HasPrefix(branch[strings.LastIndex(branch, "/")+1:], InvestigationName) && open == "" {
			open = fmt.Sprintf("the worktree %s on branch %s", current, branch)
		}
	}
	return root, open
}

// pendingCreate answers the dispatch file in the daemon's ingress dir that
// carries the investigation's create, described for the log; empty when none
// does. A file the daemon swept away between the listing and the read is
// gone, not an error.
func pendingCreate(dir string) (string, error) {
	for _, sub := range []string{dir, filepath.Join(dir, "claimed")} {
		paths, err := filepath.Glob(filepath.Join(sub, "workspace_commands_*.json"))
		if err != nil {
			return "", fmt.Errorf("list the daemon's pending workspace commands in %s: %w", sub, err)
		}
		for _, path := range paths {
			raw, err := os.ReadFile(path)
			if errors.Is(err, os.ErrNotExist) {
				continue
			}
			if err != nil {
				return "", fmt.Errorf("read the pending workspace commands %s: %w", path, err)
			}
			if strings.Contains(string(raw), InvestigationName) {
				return "the create waiting in the daemon's ingress at " + path, nil
			}
		}
	}
	return "", nil
}

// investigationPrompt is the investigation workspace's commission: the
// evidence, and what the owner wants done with it.
func investigationPrompt(t *Trouble, runLog string) string {
	var b strings.Builder
	b.WriteString("The agent-repl test runner (modules/app/agent-repl/testrun, package internal/ramdisk) refused to attach its RAM disk and ran on the ordinary disk instead, because ")
	switch {
	case t.Lock != nil:
		fmt.Fprintf(&b, "the RAM disk registry lock %s was not taken within %s (processes with it open: %s).\n\n", t.Lock.Path, t.Lock.Bound, t.Lock.Holders)
	case t.Answered():
		b.WriteString("RAM disks it made had leaked.\n\n")
	default:
		b.WriteString("the macOS disk manager (diskarbitrationd) did not answer.\n\n")
	}
	b.WriteString("## Evidence\n\n")
	if len(t.Leaks) == 0 {
		b.WriteString("- Leaked RAM disks: none found")
		if t.Lock != nil {
			b.WriteString(" (the check runs under the registry lock, so it never ran)")
		} else if !t.Answered() {
			b.WriteString(" (the listing could not be read)")
		}
		b.WriteString(".\n")
	}
	for _, l := range t.Leaks {
		fmt.Fprintf(&b, "- Leaked RAM disk %s.\n", l.String())
	}
	fmt.Fprintf(&b, "- %s.\n- %s.\n", probeLine("`diskutil list`", t.ListErr, t.ListTook), probeLine("`hdiutil info`", t.InfoErr, t.InfoTook))
	if runLog != "" {
		fmt.Fprintf(&b, "- The run's log: %s\n", runLog)
	} else {
		fmt.Fprintf(&b, "- The run's log: not named (%s was unset); the runner's ERROR line says the same.\n", EnvRunLog)
	}
	if !t.At.IsZero() {
		fmt.Fprintf(&b, "- The check began at %s.\n", stamp(t.At))
	}
	writeAnswer(&b, "hdiutil info", t.Info)
	writeAnswer(&b, "diskutil list", t.List)
	writeBlock(&b, "The host when the disk manager did not answer:", t.Host)
	b.WriteString("\n## What the owner wants\n\n")
	b.WriteString("- Find why the runner's RAM disks leak, why the disk manager wedged and why the registry lock's holder is stuck, with every link evidenced in logs.\n")
	b.WriteString("- Fix the root cause in code, with tests.\n")
	b.WriteString("- Never hand-repair: do not detach the leaked disks or restart the disk manager by hand; land the fix and let it heal the live state.\n")
	b.WriteString("- Mock every hdiutil, diskutil and workspace call in tests; never attach or detach a real RAM disk while developing.\n")
	return b.String()
}

func writeAnswer(b *strings.Builder, name, answer string) {
	writeBlock(b, fmt.Sprintf("`%s` answered:", name), answer)
}

// writeBlock writes body under heading as a fenced block; an empty body
// writes nothing.
func writeBlock(b *strings.Builder, heading, body string) {
	if strings.TrimSpace(body) == "" {
		return
	}
	fmt.Fprintf(b, "\n%s\n\n```\n%s\n```\n", heading, strings.TrimRight(body, "\n"))
}

// ScriptDispatch is the real Investigation.Dispatch: it pipes the
// workspace-command array into `<script> --emit-commands` (the
// /create-or-update-workspace skill's run.sh), within bound. A missing script,
// a failure and a timeout are each an error naming the script.
func ScriptDispatch(script string, bound time.Duration) func(commands []byte) error {
	return func(commands []byte) error {
		if _, err := os.Stat(script); err != nil {
			return fmt.Errorf("the workspace CLI is missing: %w", err)
		}
		ctx, cancel := context.WithTimeout(context.Background(), bound)
		defer cancel()
		cmd := command.Bounded(ctx, script, "--emit-commands")
		cmd.Stdin = bytes.NewReader(commands)
		out, err := cmd.CombinedOutput()
		return command.Outcome(ctx, bound, command.Line(script, []string{"--emit-commands"}), "", out, err)
	}
}
