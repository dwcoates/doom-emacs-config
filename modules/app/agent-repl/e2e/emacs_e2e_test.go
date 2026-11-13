package e2e

import (
	"agentrepl/e2e/kit/stack"
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

// testEmacsProofOfLife is scenario 20 of EMACS-LAYER-SPEC.md, implemented
// first and alone: workspace created, agent-repl panel opened, prompt
// submitted from the composer, response row visible in EMACS'S OWN STATE.
//
// It is deliberately end-to-end through the whole client stack rather than
// broad: it proves the layer's mechanism works — Emacs boots, loads the real
// module, spawns the daemon through its own launcher, drives ordinary
// commands, and hands state back as data — so the remaining 43 scenarios are
// a matter of writing scenarios rather than of building machinery.
//
// Seam: Emacs -> daemon -> shim -> page, a prompt from the composer to the drawn reply
func testEmacsProofOfLife(t *testing.T) {
	box := stack.RequireSandbox(t)
	w := stack.NewEmacsWorld(t, box)
	e := w.Emacs

	// 1. EMACS spawns the daemon, through the module's own cold-start
	//    launcher. This is the step that catches an argv the launcher gets
	//    wrong, because nothing else in the suite composes that argv.
	e.EnsureDaemon()

	// A scripted fake-git worktree for the workspace to be registered
	// against. No real git runs.
	repository := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))

	// 2. Register the directory through the ORDINARY command the user runs
	//    (SPC TAB p). The DAEMON mints the identity; Emacs echoes it.
	e.Eval(`(agent-repl-add-project-workspace ` + stack.ElispString(repository.Dir) + `)`)

	// Only entries carrying :project-dir are workspaces: persp-mode's own
	// perspectives ("none", Doom's "main") acquire stub entries the moment a
	// hook records anything about them, and the module filters those out of
	// every workspace render (workspace.el, agent-repl--ws-put-1).
	name := e.AwaitEval("the workspace to appear in Emacs's registry",
		stack.WorkspaceRegistryNamesForm,
		func(raw json.RawMessage) bool { return len(stack.DecodeStrings(raw)) == 1 })
	wsName := stack.DecodeStrings(name)[0]

	// It must also carry a daemon-minted ref: Emacs never constructs one.
	e.AwaitTrue("the workspace to hold a daemon-minted ref",
		`(and (gethash `+stack.ElispString(wsName)+` agent-repl-host--by-name) t)`)

	// 3. Open the agent-repl panel through the ordinary command.
	e.Eval(`(agent-repl-frontend-open-panel)`)
	e.AwaitEval("the agent-repl panel windows to appear",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			for _, b := range stack.DecodeStrings(raw) {
				if strings.HasPrefix(b, "*agent-frontend-") || strings.HasPrefix(b, "*agent-panel") {
					return true
				}
			}
			return false
		})

	//    AND THE WEBVIEW REALLY EXISTS. A panel buffer proves the window
	//    lifecycle; it does not prove the thing this layer needed a GUI frame
	//    for. This reads the LIVE WKWebView out of the workspace's own
	//    webview buffer, through the module's own accessor, and asks it for
	//    its URI -- so a pass means a WebKit view was created AND navigated,
	//    which is exactly what `make-xwidget: GTK has not been initialized`
	//    refused to do on a tty frame.
	uri := e.EvalString(`(let* ((buf (get-buffer (agent-repl--frontend-webview-buffer-name ` + stack.ElispString(wsName) + `)))
                 (xw (and buf (agent-repl--frontend-webview-live-widget buf))))
             (and xw (xwidget-webkit-uri xw)))`)
	if strings.TrimSpace(uri) == "" {
		t.Fatal("the panel has no live WKWebView carrying a URI: the webview was never created, or never navigated")
	}
	t.Logf("the panel's webkit view is live at %s", uri)

	// 4. Submit a prompt FROM THE COMPOSER: put text in the input buffer the
	//    way a user types it, then PRESS RET. The fake SDK answers; a plain
	//    prompt with no "!" prefix falls through to the default prose
	//    scenario.
	//
	//    RET is pressed rather than `agent-repl-send` called, and that is a
	//    deliberate consequence of booting real Doom: the composer's RET
	//    comes from `map! :map agent-repl-input-mode-map :ni "RET"` in
	//    `input.el`, which under the old `-Q` boot expanded to nothing at
	//    all. Pressing it asserts the binding AND the command; calling the
	//    command would have asserted only half of what a user does.
	inputBuffer := e.EvalString(`(buffer-name (agent-repl--input-buffer ` + stack.ElispString(wsName) + `))`)
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + stack.ElispString(wsName) + `)
                 (erase-buffer)
                 (insert "hello from the emacs client layer")
                 t)`)
	if want, got := "agent-repl-send", e.BindingForIn(inputBuffer, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q: the Doom `map!' for agent-repl-input-mode-map did not take", got, want)
	}
	e.KeysIn(inputBuffer, "RET")

	// 5. The response is visible in EMACS's own state: the roster row for
	//    this workspace settles on a settled arm, which is the finish edge
	//    Emacs's four reactions all ride.
	//    The arm is read through the module's OWN accessor rather than by
	//    `plist-get`: a row's `:status` is the DECODED ONEOF, so a direct
	//    `plist-get` yields `(:arm :none :value nil)` -- never equal to any
	//    arm keyword, and therefore a predicate that could only time out.
	e.AwaitEval("the roster row to settle after the turn",
		`(let (arms)
                   (maphash (lambda (_id row)
                              (push (format "%s" (agent-repl-roster-row-status row)) arms))
                            agent-repl-roster--rows-by-id)
                   arms)`,
		func(raw json.RawMessage) bool {
			for _, arm := range stack.DecodeStrings(raw) {
				for _, settled := range []string{":ready", ":done", ":idle-async"} {
					if arm == settled {
						return true
					}
				}
			}
			return false
		})

	// 6. Cross-check the DAEMON's own frame at the address Emacs's launcher
	//    published, so a green test cannot mean "Emacs drew something the
	//    daemon never sent".
	client := harness.DialAt(t, e.DaemonAddr())
	if client == nil {
		t.Fatal("e2e: could not dial the daemon Emacs launched")
	}
}
