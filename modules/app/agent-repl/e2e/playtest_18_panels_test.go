//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"testing"
)

// OWNER 18 of PLAYTEST-PLAN.md's partition: I53-I56 -- panels, fullscreen,
// webview reload/rescue, and visit-file routing.
//
// I.53 and I.54 are here. I.55 and I.56 are unwritten.

// TestPlaytestPanelAndFullscreen is plan I.53 and I.54: the panel opened into
// the main area, made fullscreen, and restored.
func TestPlaytestPanelAndFullscreen(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "18-panel-and-fullscreen",
		"Plan I.53 and I.54. The panel opened into the main area, then fullscreen and back.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	windows := e.EvalInt(`(length (window-list))`)
	p.capture("panel-open", "`agent-repl-frontend-open-panel`",
		fmt.Sprintf("the panel's webview is live, the composer buffer exists, and the frame holds %d windows", windows),
		"The frame is SPLIT: the WEBAPP is drawn inside the panel window — workspace sidebar down "+
			"one side, an empty feed, and the progress footer along the bottom with a status word in "+
			"it — and a separate Emacs window holds the composer. THE WEBAPP MUST NOT BE A BLANK "+
			"WHITE RECTANGLE.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitTrue("the fullscreen configuration to be recorded",
		`(and agent-repl--window-fullscreen-config t)`)
	p.capture("fullscreen", "`agent-repl-fullscreen-and-focus` (`SPC w f`)",
		"`agent-repl--window-fullscreen-config` is non-nil, so the layout was saved to be restored",
		"ONE window fills the whole frame. The webapp is drawn edge to edge and the composer window "+
			"is gone.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitEval("the fullscreen configuration to be released",
		`(and agent-repl--window-fullscreen-config t)`,
		func(raw json.RawMessage) bool { return isJSONNull(raw) })
	if got := e.EvalInt(`(length (window-list))`); got != windows {
		t.Fatalf("the frame holds %d windows after restoring, want the %d it started with", got, windows)
	}
	p.capture("fullscreen-restored", "the same command again, restoring the layout",
		fmt.Sprintf("`agent-repl--window-fullscreen-config` is nil and the frame holds its original %d windows", windows),
		"The split of the first capture is back, unchanged: the toggle RESTORED the layout rather "+
			"than rebuilding some other one.")
}
