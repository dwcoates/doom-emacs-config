package ramdisk

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/testrun/internal/command"
)

// InvestigationName is the investigation workspace's name: stable, so the
// daemon can tell that one is already there. A create of the name is refused
// as `name_held` while a workspace that has not landed holds it, and is free
// again once that workspace lands (its branch is deleted at its landing).
const InvestigationName = "ramdisk-leak-investigation"

// EnvDispatch overrides the agent CLI the investigation is created through:
// the installed front door, ~/.claude/skills/agent-repl-workspaces/agent-repld.
const EnvDispatch = "AGENT_REPL_TEST_WORKSPACE_DISPATCH"

// EnvRunLog names the run's full log file; bin/test-all.sh exports it.
const EnvRunLog = "AGENT_REPL_TEST_RUN_LOG"

// GitBound is how long `git worktree list --porcelain` may run: measured on
// the owner's host (2026-10-12) at 19-23ms warm and 142ms cold, and just over
// twice the cold one.
const GitBound = 300 * time.Millisecond

// DispatchBound is how long the create may run: CreateWorkspace measured a
// median of 5.36s and a p95 of 12.09s from acceptance to creation over 248
// creates on the owner's host (daemon.server.create_workspace, 2026-09-27 to
// 2026-10-17), and this is just over twice the p95.
const DispatchBound = 25 * time.Second

// Investigation is how a run opens the one workspace that investigates a RAM
// disk leak or a wedged disk manager.
type Investigation struct {
	// Module is the agent-repl module the run tests; its repository's main
	// worktree is where the workspace is cut.
	Module string
	// Git runs git within a bound.
	Git BoundedRunner
	// Dispatch runs one agent CLI command (`agent-repld ARGS...`), within
	// DispatchBound, and answers what it printed on stdout (the daemon's
	// answer, as protojson) beside its failure.
	Dispatch func(args []string) ([]byte, error)
	// StateDir is the agent-repl state root: its daemon.addr says a daemon
	// serves.
	StateDir string
	// RunLog is the run's log file; empty when the run was not told it.
	RunLog string
}

// InvestigationOutcome is what EnsureInvestigation did.
type InvestigationOutcome struct {
	// Existing names the investigation already there (the workspace holding
	// its name, and whether it is open); empty when one was created.
	Existing string
}

// EnsureInvestigation opens the investigation workspace for t, unless one is
// already there.
//
// THE DAEMON DECIDES WHETHER ONE IS THERE, IN THE SAME STEP AS THE CREATE: it
// refuses the name as `name_held` while any workspace that has not landed
// holds it (open, or closed and unmerged, which is reopened rather than
// duplicated), checked under the lock that claims names. So two runs never
// open two, and a branch its landed workspace left behind can neither block
// the create nor pass for an open investigation. The runner checks nothing
// itself before it asks.
func (m Manager) EnsureInvestigation(inv Investigation, t *Trouble) (InvestigationOutcome, error) {
	root, err := mainWorktree(inv)
	if err != nil {
		return InvestigationOutcome{}, err
	}
	addr := filepath.Join(inv.StateDir, "daemon.addr")
	if _, err := os.Stat(addr); err != nil {
		return InvestigationOutcome{}, fmt.Errorf("no agent-repl daemon serves to create the investigation workspace (%w)", err)
	}
	printed, dispatchErr := inv.Dispatch(createArgs(root, investigationPrompt(t, inv.RunLog)))
	if dispatchErr == nil {
		return InvestigationOutcome{}, nil
	}
	if held, ok := nameHeld(printed); ok {
		return InvestigationOutcome{Existing: held}, nil
	}
	return InvestigationOutcome{}, fmt.Errorf("dispatch the investigation workspace's create: %w", dispatchErr)
}

// mainWorktree reads the repository's main worktree within GitBound: where
// the investigation is cut.
func mainWorktree(inv Investigation) (string, error) {
	porcelain, err := inv.Git(GitBound, "git", "-C", inv.Module, "worktree", "list", "--porcelain")
	if err != nil {
		return "", fmt.Errorf("list the repository's worktrees: %w", err)
	}
	root := readMainWorktree(porcelain)
	if root == "" {
		return "", fmt.Errorf("git worktree list named no main worktree for %s", inv.Module)
	}
	return root, nil
}

// createAnswer is the part of the daemon's CreateWorkspaceResponse, as the
// agent CLI prints it (protojson), that says the name is already held
// (agentrepl.v1.CreateWorkspaceError.name_held).
type createAnswer struct {
	Error struct {
		NameHeld *struct {
			Branch    string `json:"branch"`
			Workspace struct {
				ID  string `json:"id"`
				Dir string `json:"dir"`
			} `json:"workspace"`
			Open bool `json:"open"`
		} `json:"nameHeld"`
	} `json:"error"`
}

// nameHeld reads PRINTED, a refused create's answer, for the name_held arm,
// and describes the workspace holding the name for the log. Any other answer,
// or one that is no JSON, is no held name.
func nameHeld(printed []byte) (string, bool) {
	var answer createAnswer
	if err := json.Unmarshal(printed, &answer); err != nil || answer.Error.NameHeld == nil {
		return "", false
	}
	held := answer.Error.NameHeld
	standing := "closed, not landed"
	if held.Open {
		standing = "open"
	}
	return fmt.Sprintf("the workspace %s at %s holds the name %s (%s)", held.Workspace.ID, held.Workspace.Dir, held.Branch, standing), true
}

// createArgs is the agent CLI command that creates the investigation in the
// repository whose main worktree is ROOT: a peer landing on the main branch,
// with a fresh context, its commission PROMPT, asked by the user, since a test
// run is no workspace's agent.
func createArgs(root, prompt string) []string {
	return []string{"workspace", "create",
		"-repository", root, "-relationship", "peer", "-context", "fresh",
		"-name", InvestigationName, "-prompt", prompt, "-as-user"}
}

// readMainWorktree reads `git worktree list --porcelain` for the main
// worktree, which git lists first.
func readMainWorktree(porcelain string) string {
	for _, line := range strings.Split(porcelain, "\n") {
		if path, ok := strings.CutPrefix(line, "worktree "); ok {
			return path
		}
	}
	return ""
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

// ScriptDispatch is the real Investigation.Dispatch: it runs `<cli> ARGS...`
// (the installed agent-repld front door), within bound, and answers what it
// printed on stdout, which a refused create's arm is read from. A missing CLI,
// a failure and a timeout are each an error naming the command, with
// everything it printed.
func ScriptDispatch(cli string, bound time.Duration) func(args []string) ([]byte, error) {
	return func(args []string) ([]byte, error) {
		if _, err := os.Stat(cli); err != nil {
			return nil, fmt.Errorf("the agent CLI is missing: %w", err)
		}
		ctx, cancel := context.WithTimeout(context.Background(), bound)
		defer cancel()
		cmd := command.Bounded(ctx, cli, args...)
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		out, err := cmd.Output()
		printed := append(append([]byte(nil), out...), stderr.Bytes()...)
		return out, command.Outcome(ctx, bound, command.Line(cli, args), "", printed, err)
	}
}
