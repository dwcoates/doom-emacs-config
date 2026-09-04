// emacs_handover_e2e_test.go — EMACS-LAYER-SPEC.md area H, "Reconnect,
// handover and rehydration" (scenarios 38-41).
//
// This area is where the Emacs layer earns its keep against the webapp
// layer: BY CONTRACT the webapp page does not redial a successor on restart
// — re-pointing the webview is EMACS's job — so the behaviour under test
// here has no counterpart a Connect-dialing client or a vitest suite can
// reach. The daemon is the source of WHICH workspaces exist; Emacs's whole
// contribution on reconnect is to rebuild its tabs from the roster it is
// pushed and to re-register what it already holds, idempotently.
//
// Scenario 40 (HandoverTransfersAtFreeness) is NOT in this file. See the
// comment at the bottom for exactly why, and what the layer would need
// before it can be written honestly.
package e2e

import (
	"encoding/json"
	"testing"
)

// emHOTabOrderForm reads the tab order as data — `agent-repl-roster--tab-order`
// and the tab bar's own tab names, which EMACS-LAYER-SPEC.md's readback
// table names in place of the tab-bar string.
const emHOTabOrderForm = `agent-repl-roster--tab-order`

// emHOTabBarNamesForm reads the tab bar's own names, so a test can assert
// the two agree rather than trusting the variable alone.
const emHOTabBarNamesForm = `(mapcar (lambda (tab) (format "%s" (alist-get 'name tab))) (tab-bar-tabs))`

// emHODaemonPIDForm reads the pid of the process EMACS's launcher spawned.
// A restart that reused the same process would satisfy every downstream
// assertion in this file while proving nothing, so the pid is what says a
// restart really happened.
const emHODaemonPIDForm = `(if (and agent-repl--frontend-daemon-process
                                    (process-live-p agent-repl--frontend-daemon-process))
                               (process-id agent-repl--frontend-daemon-process)
                             -1)`

// emHOAwaitLinkUp waits until Emacs holds a live daemon link again. The
// bound is `emacsBootBound`, which is the layer's named bound for "a daemon
// Emacs launched became reachable" and is the same wait `EnsureDaemon`
// makes.
func emHOAwaitLinkUp(t *testing.T, e *Emacs, what string) {
	t.Helper()
	e.AwaitEvalFor(emacsBootBound, what, `(and (agent-repl-link-up-p) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// emHOAwaitNewDaemon waits until the launcher holds a LIVE daemon process
// whose pid differs from BEFORE.
func emHOAwaitNewDaemon(t *testing.T, e *Emacs, before int) {
	t.Helper()
	e.AwaitEvalFor(emacsBootBound, "a freshly spawned daemon process", emHODaemonPIDForm,
		func(raw json.RawMessage) bool {
			var pid int
			return json.Unmarshal(raw, &pid) == nil && pid > 0 && pid != before
		})
}

// TestEmacsTabsRehydrateFromTheRosterOnConnect is scenario 38.
//
// The daemon is the source of which workspaces exist. After it is stopped
// and a new one is launched under Emacs, the tabs are not remembered
// locally and rebuilt from a cache — they are rebuilt FROM THE ROSTER the
// new daemon pushes, and the tab bar follows that order strictly.
func TestEmacsTabsRehydrateFromTheRosterOnConnect(t *testing.T) {
	// Arrange: two registered workspaces, so the assertion is about an
	// ORDER and not merely about a single row surviving.
	w, e := emGHIWorld(t)
	first, firstDir := emGHIRegister(t, e, w.Emacs.box, "repo-rehydrate-one")
	second, _ := emGHIRegister(t, e, w.Emacs.box, "repo-rehydrate-two")
	emGHISelect(t, e, firstDir, first)
	before := e.AwaitEval("the tab order to carry both workspaces", emHOTabOrderForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 2 })
	wantOrder := decodeStrings(before)

	pid := e.EvalInt(emHODaemonPIDForm)
	if pid <= 0 {
		t.Fatalf("the launcher holds no live daemon process (pid %d) before the restart", pid)
	}

	// Act: Emacs's OWN restart — stop the daemon (Emacs asks; it never
	// kills one) and ensure another. This is the ordinary command, not a
	// harness-composed relaunch.
	e.Eval(`(agent-repl-frontend-daemon-restart)`)
	emHOAwaitNewDaemon(t, e, pid)
	emHOAwaitLinkUp(t, e, "the link to come back on the new daemon")

	// Assert: the tab order is rebuilt from the roster, identically.
	got := decodeStrings(e.AwaitEval("the tab order to rehydrate", emHOTabOrderForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(wantOrder) }))
	for i := range wantOrder {
		if got[i] != wantOrder[i] {
			t.Fatalf("the rehydrated tab order is %v, want %v: the roster is the only order", got, wantOrder)
		}
	}

	// Assert: the tab BAR agrees with it. The variable is the order; a tab
	// bar that drifted from it would be a paint the roster never asked for.
	names := e.AwaitEval("the tab bar to match the rehydrated order", emHOTabBarNamesForm,
		func(raw json.RawMessage) bool {
			drawn := decodeStrings(raw)
			for _, want := range wantOrder {
				found := false
				for _, name := range drawn {
					if name == want {
						found = true
					}
				}
				if !found {
					return false
				}
			}
			return true
		})
	if len(decodeStrings(names)) == 0 {
		t.Fatalf("the tab bar lists no tabs after rehydration, want %v", wantOrder)
	}

	// Assert: BOTH registered workspaces are in the rehydrated order, named
	// explicitly, so an order of the right length built from the wrong rows
	// cannot pass.
	for _, want := range []string{first, second} {
		found := false
		for _, name := range got {
			if name == want {
				found = true
			}
		}
		if !found {
			t.Fatalf("the rehydrated tab order %v does not carry %q", got, want)
		}
	}
}

// TestEmacsReRegisterIsIdempotentByDir is scenario 39.
//
// Re-registering after a daemon restart is THE NORMAL PATH, not an error:
// Emacs re-offers every directory it holds on the link-up edge, and the
// daemon keys them by directory. The defect this pins is a second row for
// the same worktree — a registry that grew, or a duplicate tab, because a
// re-registration was treated as a new workspace.
func TestEmacsReRegisterIsIdempotentByDir(t *testing.T) {
	// Arrange
	w, e := emGHIWorld(t)
	first, firstDir := emGHIRegister(t, e, w.Emacs.box, "repo-reregister-one")
	emGHIRegister(t, e, w.Emacs.box, "repo-reregister-two")
	emGHISelect(t, e, firstDir, first)
	wantNames := e.EvalStrings(emGHIWorkspaceNamesForm)
	if len(wantNames) != 2 {
		t.Fatalf("the registry holds %v before the restart, want two workspaces", wantNames)
	}
	pid := e.EvalInt(emHODaemonPIDForm)

	// Act
	e.Eval(`(agent-repl-frontend-daemon-restart)`)
	emHOAwaitNewDaemon(t, e, pid)
	emHOAwaitLinkUp(t, e, "the link to come back on the new daemon")

	// Assert: the SAME count and the SAME names. The registry is read as
	// data and sorted at the source, so this compares sets without ordering
	// noise, and any duplicate would move the count.
	e.AwaitEval("the workspace registry to settle after the re-registration",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(wantNames) })
	got := e.EvalStrings(emGHIWorkspaceNamesForm)
	if len(got) != len(wantNames) {
		t.Fatalf("the registry holds %v after the restart, want %v: a re-registration duplicated a workspace", got, wantNames)
	}
	for i := range wantNames {
		if got[i] != wantNames[i] {
			t.Fatalf("the registry holds %v after the restart, want %v", got, wantNames)
		}
	}

	// Assert: no duplicate tab either. The tab order is one name per
	// workspace, and it is where a duplicate row would become visible.
	tabs := e.EvalStrings(emHOTabOrderForm)
	seen := map[string]bool{}
	for _, name := range tabs {
		if seen[name] {
			t.Fatalf("the tab order is %v after the restart: %q appears twice", tabs, name)
		}
		seen[name] = true
	}
}

// TestEmacsDaemonDownSurfacesAndReconnects is scenario 41.
//
// Connection death is detected AT THE TRANSPORT — there are no keepalive
// frames — so the assertion is that the link goes down on its own, that the
// reconnect loop is armed rather than the outage being swallowed, and that
// the link comes back when a daemon returns.
func TestEmacsDaemonDownSurfacesAndReconnects(t *testing.T) {
	// Arrange
	w, e := emGHIWorld(t)
	ws, dir := emGHIRegister(t, e, w.Emacs.box, "repo-daemon-down")
	emGHISelect(t, e, dir, ws)

	// Act: ask the daemon to shut down. EMACS NEVER KILLS A DAEMON, so the
	// stop is the module's own command and the daemon exits itself.
	e.Eval(`(agent-repl-frontend-daemon-stop)`)

	// Assert: the outage SURFACES. The link drops without anyone telling
	// Emacs to drop it, which is the transport-level detection the contract
	// relies on.
	e.AwaitEvalFor(emacsBootBound, "the link to go down when the daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Assert: the reconnect loop is ARMED. A dropped link with no timer is
	// an outage silently absorbed, which is the failure this pins.
	e.AwaitEvalFor(emacsBootBound, "the reconnect loop to be armed",
		`(and (timerp agent-repl-link--reconnect-timer) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Act: a daemon returns, launched the same way it was the first time.
	e.Eval(`(agent-repl-frontend-daemon-ensure)`)

	// Assert: the link comes back, and the reconnect loop stands down with
	// it — a timer still armed on a live link would keep dialing forever.
	emHOAwaitLinkUp(t, e, "the link to come back when a daemon returns")
	e.AwaitEvalFor(emacsBootBound, "the reconnect loop to stand down once the link is up",
		`(if (timerp agent-repl-link--reconnect-timer) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// ---------------------------------------------------------------------------
// SCENARIO 40 — HandoverTransfersAtFreeness — NOT WRITTEN, and why
// ---------------------------------------------------------------------------
//
// Scenario 40 asks for `agent-repl-link--successor` to be promoted to
// `agent-repl-link--primary` off a real `transferred` push. Emacs only ever
// attaches a successor from a `shutdown_announced` push that CARRIES AN
// ADDRESS, and the daemon publishes exactly one such announcement:
// `daemon/internal/rollout/handover.go`, fired by a self-merge rollout
// landing on the daemon's own checkout. `drain/controller.go`'s two
// announcements carry no address and are the plain-bounce path (area E's
// scenario 30), so they cannot stand in.
//
// Provoking that rollout needs two things this layer cannot reach today:
//
//   1. `AGENT_REPL_SELF_REPO_DIR` and `AGENT_REPL_TEST_ALL_SCRIPT` in the
//      EMACS process's environment, so the daemon Emacs spawns inherits
//      them. They are set per-daemon by `harness.Opts` for the Go layer;
//      for this layer the daemon's environment is composed by
//      `NewEmacsWorld`, which takes no options and is another area's file.
//   2. A create-child-and-merge drive against the daemon Emacs launched.
//      `adoption_e2e_test.go`'s `adTriggerSelfMergeRollout` does exactly
//      this, but it and its feed-watching helpers are methods on `*World`,
//      which this layer deliberately does not build.
//
// Both are harness changes, not test-writing, and this file's author does
// not own either file. Reported to the project lead rather than absorbed by
// weakening the scenario into "call `agent-repl-link-dial-successor` at an
// address nothing announced", which would assert Emacs's dial and not the
// handover.
