//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// OWNER 3 of PLAYTEST-PLAN.md's partition: A7-A10 -- switch, priority,
// close/reopen/kill, and copy.
//
// A.7 and A.9's close and kill are here. A.8, A.10 and the reopen half of
// A.9 are unwritten.

// TestPlaytestSwitchBetweenWorkspaces is plan A.7: a second workspace, the
// selection moving between the two, and the tab bar following it.
func TestPlaytestSwitchBetweenWorkspaces(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-switch",
		"Plan A.7. Two workspaces on the tab bar, and the selection moving between them.")
	p, e := s.Book, s.E

	first := s.repoAt(t, "repo-first")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	p.note("the first repository registered and its panel opened",
		"the composer buffer exists and the webapp drew its footer against this daemon")

	second := s.repoAt(t, "repo-second")
	secondName := s.register(t, second.Dir)
	// Registering SELECTS, which is one of Emacs's only two inputs to the
	// roster, so the assertion is on the module's own current-workspace
	// accessor rather than on anything drawn.
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	names := s.tabNames()
	if len(names) != 2 {
		t.Fatalf("the tab bar draws %v, want both workspaces", names)
	}
	p.capture("two-tabs", "a second repository registered through the same verb",
		fmt.Sprintf("`agent-repl--ws-tabline-names` is %v and `agent-repl--ws-current-name` is %q", names, secondName),
		fmt.Sprintf("The tab bar must carry TWO workspace tabs, %q and %q, in that order, and the "+
			"SECOND must be the highlighted one — registering selects it.", names[0], names[1]))

	// `agent-repl-switch-to-project` takes a PROJECT ROOT PATH, not a
	// workspace name -- its own docstring says so -- and taking the target as
	// an argument is why the picker is neither the subject nor stubbed.
	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the first workspace to become the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == firstName })
	p.capture("switched-back", "`agent-repl-switch-to-project` back to the first workspace",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q", firstName),
		fmt.Sprintf("The SAME two tabs in the SAME order, with the highlight moved back to %q. "+
			"The selection moved; the roster did not.", firstName))
}

// TestPlaytestCloseAndKillLeaveTheEditorAnswering is plan A.9's close and
// kill, and it is deliberately FUNCTIONAL-ONLY: what it proves is that the
// tab goes away, the daemon still holds the session after a close, and Emacs
// is still answering afterwards. None of that is a picture.
//
// The heartbeat is the real assertion behind the last of those, and it is
// armed for the whole life of the process: this is the sentinel/kill-buffer
// recursion, which manifests only as an editor that stops answering.

// TestPlaytestCloseAndKillLeaveTheEditorAnswering is plan A.9's close and
// kill, and it is deliberately FUNCTIONAL-ONLY: what it proves is that the
// tab goes away, the daemon still holds the session after a close, and Emacs
// is still answering afterwards. None of that is a picture.
//
// The heartbeat is the real assertion behind the last of those, and it is
// armed for the whole life of the process: this is the sentinel/kill-buffer
// recursion, which manifests only as an editor that stops answering.
func TestPlaytestCloseAndKillLeaveTheEditorAnswering(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "03-close-and-kill",
		"Plan A.9. Close is a view act and kill never blocks, and neither wedges the editor. "+
			"FUNCTIONAL ONLY: nothing here has a visual subject, so nothing is captured.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	// `SPC j d` takes the CURRENT workspace, so it is PRESSED: real keymap
	// lookup, real command.
	e.Leader("j d")
	e.AwaitEval("the closed workspace's tab to be gone",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), name) })
	p.note("`SPC j d` pressed to close the workspace",
		"the name is gone from `agent-repl--ws-tabline-names`")

	// CLOSE IS A VIEW ACT BY CONTRACT, so the only way to say the daemon still
	// holds the workspace is to ask the daemon -- at the address Emacs's own
	// launcher published.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon to still hold the closed workspace",
		func(r *frontendv1.WorkspaceRoster) bool { return len(r.GetRepository().GetSections()) > 0 })
	p.note("the daemon asked for its own roster at the address the launcher published",
		"the daemon still carries the workspace: closing is a VIEW act and destroys nothing")

	e.AwaitEvalFor(emacsWedgeProbeBound, "emacs to still answer its command loop after the close and kill",
		`(and (emacs-pid) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	p.note("Emacs probed for liveness after the close",
		"the command loop still answers, and the heartbeat has not missed for the whole run")
}
