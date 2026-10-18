// emacs_interrupt_e2e_test.go — EMACS-LAYER-SPEC.md area G, "Interrupt and
// restart" (scenarios 35-37).
//
// Every test here drives EMACS as the client: the daemon is spawned by the
// module's own launcher (`agent-repl-frontend-daemon-ensure`), the acts are
// the ordinary interactive commands a user runs, and the assertions read
// Emacs's own state back AS DATA — never scraped text, never a private
// helper reached past its command.
//
// # There is no interrupt command, and that is the contract
//
// EMACS-LAYER-SPEC.md's "Two brief items the contract does not have" is
// explicit: no `agent-repl-interrupt*` symbol exists, and the interrupting
// act is `agent-repl-restart-workspace` (`SPC o C-c`). Since 2026-10-02 the
// restart is ALWAYS immediate and forced (owner ruling): it takes no prefix
// argument and has no graceful mode, so every test here drives
// `(agent-repl-restart-workspace WS)` and nothing else.
//
// This file also holds the helpers area G, H and I share. They are
// area-local by design: EMACS-LAYER-SPEC.md's shared harness
// (`emacs_test.go`, `emacs_sandbox_test.go`, `world_test.go`,
// `main_test.go`) is not this file's to grow.
package e2e

import (
	"context"
	"encoding/json"
	"fmt"
	"path/filepath"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// AREA G/H/I SHARED HELPERS
// ---------------------------------------------------------------------------

// emGHIParkedPrompt is the fake SDK's own `!interrupt` scenario
// (agent-shim/claude/shim/src/fake/scenarios/lifecycle.ts,
// INTERRUPT_MID_TOOL), whose body parks inside `awaitInterrupt()` with a
// Bash tool call genuinely live. It is the ONE prompt in this suite that
// guarantees an observable mid-turn window without a sleep: the turn does
// not advance on its own, so "the roster row is running" is a fact rather
// than a race the test happened to win. The same reasoning is what
// interrupt_e2e_test.go gives for driving it from the Go client.
const emGHIParkedPrompt = "!interrupt"

// emGHIGatedPrompt is the prompt scenario 36 parks on the fake's TURN GATE
// (hibernation_e2e_test.go's turnGatePathEnv/turnGateTextEnv, implemented in
// agent-shim/claude/shim/src/fake/index.ts's `awaitTurnGate`). It carries no
// scenario marker, so the fake answers it ordinarily once the gate opens; a
// restart hard-stops it like any other turn.
const emGHIGatedPrompt = "hold here until the gate opens"

// emGHIWorld brings up the Emacs world and has EMACS spawn the daemon,
// which is the whole layer's premise: nothing here composes the daemon's
// argv.
func emGHIWorld(t *testing.T, options ...EmacsWorldOption) (*EmacsWorld, *Emacs) {
	t.Helper()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, options...)
	w.Emacs.EnsureDaemon()
	return w, w.Emacs
}

// emGHIRegister registers a scripted-fake-git worktree through the ORDINARY
// command (`SPC TAB C-n`) and answers the workspace name Emacs recorded.
//
// The daemon mints the identity; Emacs only echoes it, so the ref's presence
// is asserted here rather than assumed by every caller.
func emGHIRegister(t *testing.T, e *Emacs, box sandbox, dirName string) (string, string) {
	t.Helper()
	repo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), dirName))
	before := len(e.EvalStrings(emGHIWorkspaceNamesForm))
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(repo.Dir) + `)`)
	raw := e.AwaitEval(fmt.Sprintf("the workspace for %s to appear in Emacs's registry", dirName),
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == before+1 })
	name := emGHINewName(t, decodeStrings(raw), before)
	e.AwaitTrue("the workspace to hold a daemon-minted ref",
		`(and (agent-repl-host-ref `+elispString(name)+`) t)`)
	return name, repo.Dir
}

// emGHIWorkspaceNamesForm reads the workspace REGISTRY as data, which is
// EMACS-LAYER-SPEC.md's named readback for "workspace registry" — never a
// buffer name.
const emGHIWorkspaceNamesForm = `(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) (sort names #'string<))`

// emGHINewName answers the single name the registry gained.
func emGHINewName(t *testing.T, names []string, before int) string {
	t.Helper()
	if len(names) != before+1 {
		t.Fatalf("the workspace registry holds %d names (%v), want %d", len(names), names, before+1)
	}
	return names[len(names)-1]
}

// emGHISelect makes WS the current workspace through the ordinary switch
// command, so the commands that read ambient workspace context
// (`agent-repl-queue-deferred-prompt`, `agent-repl-send`) act on it. The
// command takes the project root as its documented argument, so no picker is
// stubbed.
func emGHISelect(t *testing.T, e *Emacs, dir, ws string) {
	t.Helper()
	e.Eval(`(agent-repl-switch-to-project ` + elispString(dir) + `)`)
	e.AwaitEval("the workspace to become current",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return json.Unmarshal(raw, &got) == nil && got == ws
		})
}

// emGHIOpenPanel opens the agent-repl panel through the ordinary command and
// waits for its windows.
func emGHIOpenPanel(t *testing.T, e *Emacs) {
	t.Helper()
	e.Eval(`(agent-repl-frontend-open-panel)`)
	e.AwaitEval("the agent-repl panel windows to appear",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			for _, b := range decodeStrings(raw) {
				if len(b) >= 16 && b[:16] == "*agent-frontend-" {
					return true
				}
			}
			return false
		})
}

// emGHISubmit types TEXT into WS's composer and PRESSES RET, which is how a
// user submits: the composer's RET is `map! :map agent-repl-input-mode-map
// :ni "RET"` in `input.el`, so pressing it asserts the binding as well as
// the command.
//
// The workspace's session must be up first: the daemon refuses a submit to a
// workspace with no session (`no_session`), and opening the panel does not
// wait for the session. A settled roster arm is published only once the
// session is serving, so awaiting one orders the submit after it.
//
// That wait is the workspace's FIRST open being answered, bounded by
// emacsTabOpenBound rather than DefaultTimeout: the roster draws the open's
// `inactive` arm over every other until the open's webapp page has drawn
// (daemon/internal/opentabs), so the wait spans a shim spawn AND a WebKit page
// boot. Under the merge gate's parallel load a page whose script started late
// held the arm at `inactive` past DefaultTimeout while the session was long up.
func emGHISubmit(t *testing.T, e *Emacs, ws, text string) {
	t.Helper()
	emGHIAwaitStatusFor(t, e, emacsTabOpenBound, ws, "the workspace's session to be up before the submit", emGHISettledArms...)
	buffer := e.EvalString(`(buffer-name (agent-repl--input-buffer ` + elispString(ws) + `))`)
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(ws) + `)
                 (erase-buffer)
                 (insert ` + elispString(text) + `)
                 t)`)
	if want, got := "agent-repl-send", e.BindingForIn(buffer, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q: the Doom `map!' for agent-repl-input-mode-map did not take", got, want)
	}
	e.KeysIn(buffer, "RET")
}

// emGHIStatusForm reads WS's roster status arm as data. The roster's arm
// vocabulary is the ONE source for a workspace's state, per
// EMACS-LAYER-SPEC.md's readback table.
func emGHIStatusForm(ws string) string {
	return `(format "%s" (agent-repl-roster-status-for-ws ` + elispString(ws) + `))`
}

// emGHIAwaitStatus waits until WS's roster arm is one of ARMS, within
// DefaultTimeout.
func emGHIAwaitStatus(t *testing.T, e *Emacs, ws, what string, arms ...string) string {
	t.Helper()
	return emGHIAwaitStatusFor(t, e, DefaultTimeout, ws, what, arms...)
}

// emGHIAwaitStatusFor is emGHIAwaitStatus with an explicit, named bound.
func emGHIAwaitStatusFor(t *testing.T, e *Emacs, bound time.Duration, ws, what string, arms ...string) string {
	t.Helper()
	raw := e.AwaitEvalFor(bound, what, emGHIStatusForm(ws), func(raw json.RawMessage) bool {
		var got string
		if err := json.Unmarshal(raw, &got); err != nil {
			return false
		}
		for _, arm := range arms {
			if got == arm {
				return true
			}
		}
		return false
	})
	var got string
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("decode the roster arm for %s: %v", ws, err)
	}
	return got
}

// emGHIRunningArms and emGHISettledArms mirror
// `agent-repl-roster-running-statuses` and
// `agent-repl-roster-settled-statuses` in `lisp/roster.el`. They are spelled
// here as the two halves of the finish edge the assertions name.
var (
	emGHIRunningArms = []string{":submitting", ":thinking", ":clearing", ":compacting", ":permission"}
	emGHISettledArms = []string{":ready", ":done", ":interrupted", ":idle-async"}
)

// emGHIIngressWaitingForm counts WS's prompts waiting in the durable
// on-disk held-prompt ingress (`$AGENT_REPL_STATE_DIR/held-prompts/`), which
// EMACS-LAYER-SPEC.md's readback table names as
// `agent-repl-held-ingress-waiting` — never the tray. Owner ruling
// 2026-09-28: Emacs holds no prompt in memory, so the ingress is the ONLY
// place Emacs itself still holds one (a prompt the daemon did not take).
func emGHIIngressWaitingForm(ws string) string {
	return `(agent-repl-held-ingress-waiting ` + elispString(ws) + `)`
}

// emGHIAssertIngressEmpty reads, without waiting, that none of WS's prompts
// wait in the ingress. Call it only once the edge that would have written an
// entry is already behind the test, so "empty now" is the settled answer.
func emGHIAssertIngressEmpty(t *testing.T, e *Emacs, ws, when string) {
	t.Helper()
	if n := e.EvalInt(emGHIIngressWaitingForm(ws)); n != 0 {
		t.Fatalf("%d of %s's prompts wait in the held-prompt ingress %s, want none", n, ws, when)
	}
}

// ---------------------------------------------------------------------------
// AREA G
// ---------------------------------------------------------------------------

// TestEmacsForcedRestartInterruptsTheTurn is scenario 35.
//
// A prefix-argument restart is the interrupting act, and its contract has
// two halves that must BOTH hold: the live turn stops (the roster arm
// settles `:interrupted`), and the agent is NOT resumed afterwards — a
// forced restart that quietly re-drove the turn would look identical at the
// first assertion alone.
func TestEmacsForcedRestartInterruptsTheTurn(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace with a genuinely live turn. The parked
	// scenario is what makes "mid-turn" a fact rather than a race.
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-forced-restart")
	emGHISelect(t, e, dir, ws)
	emGHIOpenPanel(t, e)
	emGHISubmit(t, e, ws, emGHIParkedPrompt)
	emGHIAwaitStatus(t, e, ws, "the turn to be running before the interrupt", emGHIRunningArms...)

	// Act: `SPC o C-c`. The workspace is passed as the command's documented
	// argument rather than by faking a key press that would then also have to
	// satisfy the picker.
	e.Eval(`(agent-repl-restart-workspace ` + elispString(ws) + `)`)

	// Assert: the arm settles on `:interrupted` specifically. Any other
	// settled arm would say the turn ENDED rather than that it was stopped.
	emGHIAwaitStatus(t, e, ws, "the roster arm to settle interrupted", ":interrupted")

	// Assert: the agent is not resumed. The roster arm stays settled — a
	// resumption would move it back into the running half — and no prompt was
	// left behind for re-driving: Emacs holds prompts nowhere but the durable
	// ingress, and it is empty.
	if got := emGHIAwaitStatus(t, e, ws, "the roster arm to stay settled", emGHISettledArms...); got != ":interrupted" {
		t.Fatalf("the roster arm moved to %s after the forced restart, want it to stay :interrupted: the agent was resumed", got)
	}
	emGHIAssertIngressEmpty(t, e, ws, "after a forced restart")
}

// TestEmacsRestartHoldsPromptsMeanwhile is scenario 36.
//
// The claim under test is that a prompt written around a restart is HELD
// rather than refused: undelivered user intent may never be silently
// discarded. Owner ruling 2026-09-28 moved the holding out of Emacs: the
// deferral (`SPC j RET`) is submitted AT ONCE with `:delivery :deferred`
// and the DAEMON holds it in its held tray, delivering it as its own turn
// when the running one ends. Emacs therefore proves three things it can see:
// the deferral left at once, deferred, with the composer cleared; nothing
// was left waiting on disk (the daemon took it rather than refusing it); and
// the turn settles once the gate lets the restart reach its finish edge. The
// daemon's `:success' answer to the deferral is read at the RPC boundary.
// The daemon's tray itself is not read here: this Emacs-layer world exposes
// no daemon client.
func TestEmacsRestartHoldsPromptsMeanwhile(t *testing.T) {
	t.Parallel()
	// Arrange. The parked turn here is the fake's TURN GATE, not
	// `emGHIParkedPrompt`: a GRACEFUL restart waits for the turn to finish,
	// and `!interrupt` parks inside `awaitInterrupt()`, which only a FORCED
	// restart resolves. A gated turn is the one park with two exits — the
	// gate file releases it into an ORDINARY terminal — so the finish edge
	// this scenario is about can actually happen.
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "graceful-restart-gate")
	w, e := emGHIWorld(t,
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, emGHIGatedPrompt))
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-graceful-restart")
	emGHISelect(t, e, dir, ws)
	emGHIOpenPanel(t, e)
	emGHISubmit(t, e, ws, emGHIGatedPrompt)
	emGHIAwaitStatus(t, e, ws, "the turn to be running before the restart", emGHIRunningArms...)
	// Armed AFTER the gated turn's own send, so the observers see only the
	// deferral.
	armSubmissionObserver(t, e)
	armSubmissionAnswerObserver(t, e)

	// Act: the user writes a second prompt mid-turn and defers it through
	// the ordinary command (`SPC j RET`), then asks for a GRACEFUL restart —
	// no prefix argument, so the turn is not forced down.
	const heldText = "the held prompt"
	buffer := e.EvalString(`(buffer-name (agent-repl--input-buffer ` + elispString(ws) + `))`)
	e.Eval(`(with-current-buffer (agent-repl--input-buffer ` + elispString(ws) + `)
                 (erase-buffer)
                 (insert ` + elispString(heldText) + `)
                 t)`)
	e.Eval(`(agent-repl-queue-deferred-prompt)`)

	// Assert: the deferral reached the RPC boundary at once, DEFERRED, and
	// the composer was cleared because the words are now the daemon's.
	sent := awaitSubmissions(t, e, 1, "the deferral to reach the RPC boundary")[0]
	if sent.Text != heldText || sent.Delivery != ":deferred" {
		t.Fatalf("the deferral was submitted as text %q delivery %q, want %q delivered :deferred",
			sent.Text, sent.Delivery, heldText)
	}
	if got := e.EvalString(`(with-current-buffer ` + elispString(buffer) + ` (buffer-string))`); got != "" {
		t.Fatalf("the composer holds %q after the deferral, want it cleared", got)
	}

	// Assert: the daemon TOOK it into its held tray rather than refusing it.
	if arm := awaitSubmissionAnswer(t, e, heldText, "the daemon to answer the deferral"); arm != ":success" {
		t.Fatalf("the daemon answered the mid-turn deferral %s, want :success: a deferral is held, never refused", arm)
	}

	e.Eval(`(agent-repl-restart-workspace ` + elispString(ws) + `)`)

	// The restart is immediate: it hard-stops the gated turn itself, so the
	// gate is never opened. The held deferral must survive that stop.

	// Assert: the turn settles on the finish edge the restart produces, and
	// nothing was left waiting on disk: had the daemon refused the deferral
	// or the transport dropped it, Emacs would have written it to the
	// ingress. An empty ingress is the daemon having taken it.
	emGHIAwaitStatus(t, e, ws, "the turn to settle after the restart", emGHISettledArms...)
	emGHIAssertIngressEmpty(t, e, ws, "once the restart has settled")

	// Assert: the held prompt SURVIVED the restart: the daemon delivered it
	// as its own turn, so the conversation's own book in the store holds it.
	vendor := e.EvalString(`(or (agent-repl-host-vendor-session-id ` + elispString(ws) + `) "")`)
	if vendor == "" {
		t.Fatalf("workspace %s names no vendor conversation after the restart, so its book cannot be read", ws)
	}
	emAwaitBookPrompt(t, w, vendor, heldText, "the held prompt to be delivered after the restart")
}

// emAwaitBookPrompt waits until the store's book for AGENT holds a prompt
// saying TEXT, failing at DefaultTimeout. The store is read directly and
// read-only: this Emacs-layer world exposes no daemon client, and a prompt
// the daemon delivered is a line of its agent's book.
func emAwaitBookPrompt(t *testing.T, w *EmacsWorld, agent, text, what string) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	var said []string
	for {
		said = said[:0]
		page, err := readAgentBook(ctx, w.Store.Client, &conversationv1.AgentId{Value: agent})
		if err == nil {
			for _, line := range page.GetLines() {
				prompt := line.GetLine().GetAgentItem().GetAgentPrompt()
				if prompt == nil {
					continue
				}
				for _, block := range prompt.GetSaid().GetContent().GetBlocks() {
					said = append(said, block.GetText().GetText())
				}
			}
			if slices.Contains(said, text) {
				return
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("await %s: the book of %s never held a prompt saying %q within %s (prompts seen: %q, last error: %v)", what, agent, text, DefaultTimeout, said, err)
		case <-ticker.C:
		}
	}
}
