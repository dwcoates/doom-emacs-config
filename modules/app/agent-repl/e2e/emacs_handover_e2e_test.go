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
// Scenario 40 (HandoverTransfersAtFreeness) provokes a REAL self-merge
// landing, whose one deploy finds the daemon stale and hands it over, to get
// the one announcement that carries a successor's address.
// See the block above `TestEmacsHandoverTransfersAtFreeness`.
package e2e

import (
	"encoding/json"
	"path/filepath"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// daemonStopBound is how long the LINK may take to go down after the daemon
// ACCEPTED an immediate shutdown. It is a different phase from every bound
// in `emacs_test.go`: nothing about Emacs is being waited on, the wait is
// the daemon flushing its writes and exiting, the kernel closing the socket,
// Emacs's own socket seeing EOF and its sentinel running.
//
// MEASURED: 2.021s and 2.023s across the two scenarios that make this wait,
// remarkably stable. 3x that. The bound it replaced named an emacsclient
// round trip at 500ms -- a quarter of this phase's real cost, so neither
// scenario could ever pass.
const daemonStopBound = 6 * time.Second

// handoverAnnounceBound is how long the whole self-merge rollout may take to
// reach Emacs as an announcement carrying a successor's address: the merge,
// the landing's deploy (a staged fake build), the SUCCESSOR DAEMON'S OWN
// SPAWN AND BOOT, and only then the push.
//
// MEASURED: 190ms from the daemon admitting the merge to Emacs logging
// `elisp.link.handover-announced`. The multiple is 10x rather than 3x, for
// the reason the boot bound states and one more: the observed number covers
// the landing's deploy and a whole daemon process spawn, whose cost is the
// machine's rather than this module's.
const handoverAnnounceBound = 2 * time.Second

// handoverPromoteBound is how long the attached successor may take to be
// PROMOTED once attached -- the outgoing daemon transferring each workspace,
// the last one reaching freeness, and the outgoing daemon then EXITING.
//
// THE EXIT IS THE EDGE, and that is why this phase costs what it costs.
// Emacs does not promote on a transfer: `agent-repl-link--handle-close'
// (`lisp/daemon-link.el') promotes when the PRIMARY stream closes while a
// successor is held. So this phase is the rollout plus exactly the phase
// `daemonStopBound' above measures -- the daemon flushing its writes and
// exiting, the kernel closing the socket, Emacs's own socket seeing EOF and
// its sentinel running -- which is why the two numbers agree to a millisecond.
//
// MEASURED, and it replaces a bound that was deliberately left unmeasured
// because no healthy promotion had ever been seen. Six samples on a quiet
// box (load average 6): 2.021s, 2.022s, 2.022s, 2.023s, 2.030s, 2.041s.
//
// THE MULTIPLE IS 10x, NOT 3x, for exactly the reason `handoverAnnounceBound`
// above already gives: the phase contains a whole daemon PROCESS lifecycle --
// there, a spawn; here, an orderly exit -- and that cost is the machine's
// rather than this module's. Taking 3x (7s) was tried and it reds on a box
// with a sibling suite running, on a run whose every upstream phase was
// healthy and whose successor attached in 225ms. A bound that fails on load
// alone is a flake, and this suite does not ship one.
//
// THE 60s IT REPLACES WAS HIDING A REGRESSION, which is the reason to
// tighten it rather than merely to record the number. The old comment named
// a 32s fault path as the only outcome this scenario produced; 32s is
// 30s + 2.02s, and the 30s is `rollout.DefaultAdoptionWindow' -- the
// outgoing daemon waiting out the whole window for a headless workspace its
// successor failed to adopt and would never retry, delaying `windows.Wait()'
// and therefore the exit this phase waits on. That defect is fixed in
// `daemon/internal/rollout/adopt.go' (`retryHeadless'); a bound of 60s would
// have let it come back silently. 21s still fails that 32s regression by a
// wide margin, which is the whole reason to move off 60s.
const handoverPromoteBound = 21 * time.Second

// emHOTabOrderForm reads the tab order as data — `agent-repl-roster--tab-order`
// and the tab bar's own tab names, which EMACS-LAYER-SPEC.md's readback
// table names in place of the tab-bar string.
const emHOTabOrderForm = `agent-repl-roster--tab-order`

// emHOTabBarNamesForm reads the tab bar's own names, so a test can assert
// the two agree rather than trusting the variable alone.
//
// NOT `tab-bar-tabs`: `status.el` paints the bar from `tab-bar-format`, so
// Emacs's built-in tabs are window configurations named after whatever
// buffer they hold (a `*magit: ...*` status buffer, once a project switch
// has run) and carry no workspace name at all. `agent-repl--ws-tabline-names`
// is the enumeration the renderer itself walks — the drawn names, in roster
// order — so it is what "the tab bar's own names" means in this module.
const emHOTabBarNamesForm = `(agent-repl--ws-tabline-names)`

// emHODaemonPIDForm reads the pid of the process EMACS's launcher spawned.
// A restart that reused the same process would satisfy every downstream
// assertion in this file while proving nothing, so the pid is what says a
// restart really happened.
const emHODaemonPIDForm = `(if (and agent-repl--frontend-daemon-process
                                    (process-live-p agent-repl--frontend-daemon-process))
                               (process-id agent-repl--frontend-daemon-process)
                             -1)`

// emHOAwaitLinkUp waits until Emacs holds a live daemon link again. The
// bound is `daemonLinkBound`, which is the layer's named bound for "a daemon
// Emacs launched became reachable" and is the same wait `EnsureDaemon`
// makes.
func emHOAwaitLinkUp(t *testing.T, e *Emacs, what string) {
	t.Helper()
	e.AwaitEvalFor(daemonLinkBound, what, `(and (agent-repl-link-up-p) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// emHOAwaitNewDaemon waits until the launcher holds a LIVE daemon process
// whose pid differs from BEFORE.
func emHOAwaitNewDaemon(t *testing.T, e *Emacs, before int) {
	t.Helper()
	e.AwaitEvalFor(daemonLinkBound, "a freshly spawned daemon process", emHODaemonPIDForm,
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	e.AwaitEvalFor(daemonStopBound, "the link to go down when the daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Assert: the reconnect loop is ARMED. A dropped link with no timer is
	// an outage silently absorbed, which is the failure this pins.
	e.AwaitEvalFor(daemonStopBound, "the reconnect loop to be armed",
		`(and (timerp agent-repl-link--reconnect-timer) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Act: a daemon returns, launched the same way it was the first time.
	e.Eval(`(agent-repl-frontend-daemon-ensure)`)

	// Assert: the link comes back, and the reconnect loop stands down with
	// it — a timer still armed on a live link would keep dialing forever.
	emHOAwaitLinkUp(t, e, "the link to come back when a daemon returns")
	e.AwaitEvalFor(daemonLinkBound, "the reconnect loop to stand down once the link is up",
		`(if (timerp agent-repl-link--reconnect-timer) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
}

// ---------------------------------------------------------------------------
// SCENARIO 40 — HandoverTransfersAtFreeness
// ---------------------------------------------------------------------------
//
// Emacs attaches a successor from EXACTLY ONE push: a `shutdown_announced`
// that CARRIES AN ADDRESS, published only by
// `daemon/internal/rollout/handover.go` when a deploy finds the daemon stale
// — here the one deploy a merge landing on the daemon's own checkout runs,
// over a staged build whose daemon is not the running one. `drain/controller.go`'s announcements carry no
// address and are the plain-bounce path (area E's scenario 30), so they
// cannot stand in, and dialing `agent-repl-link-dial-successor` at an
// address nothing announced would assert Emacs's dial rather than the
// handover.
//
// So the handover is provoked for real, the way the Go layer's
// `adTriggerDeploy` provokes it, but every act is EMACS's: the
// daemon's own checkout is stated as environment the Emacs process carries
// (`WithEmacsEnv`) and its daemon child inherits, the trigger workspace is
// created through `agent-repl-create-workspace`, and the merge is enqueued
// through `agent-repl-merge-workspace`.

// emHO40SelfMergeTriggerPath is the path the trigger commit touches — the
// same path `adoption_e2e_test.go` commits for the Go layer's own handover
// tests. What the landing's deploy changes is the staged build's
// (`EmacsWorld.Deploy`), never the path's.
const emHO40SelfMergeTriggerPath = "modules/app/agent-repl/daemon/cmd/claude-repld/main.go"

// emHO40TriggerName is the name given to the trigger workspace at creation.
const emHO40TriggerName = "trigger"

// emHO40Prompt is the fake SDK's plain streamed-prose scenario: it concludes
// on its own, which is what the merge requires (a workspace merges from
// idle), and it never parks.
const emHO40Prompt = "!prose-streamed"

// emHO40Instrument arms the two link hooks the handover runs through, so the
// ATTACH and the PROMOTION are each recorded at the moment they happen
// rather than polled for afterwards — a promotion that completed between two
// polls would otherwise be indistinguishable from one that never occurred.
//
// `agent-repl-link-promote-functions` runs from
// `agent-repl-link--promote-successor` and NOWHERE else, so a recorded
// promotion is proof the successor path ran, not an ordinary reconnect.
const emHO40Instrument = `(progn
  (defvar em-ho40-handover nil)
  (defvar em-ho40-promoted nil)
  (setq em-ho40-handover nil em-ho40-promoted nil)
  (add-hook 'agent-repl-link-handover-functions
            (lambda (_old new)
              (setq em-ho40-handover (agent-repl-connect-connection-address new))))
  (add-hook 'agent-repl-link-promote-functions
            (lambda (_old new)
              (setq em-ho40-promoted (agent-repl-connect-connection-address new))))
  t)`

// emHO40SectionLabelForm answers the roster label of DIR's repository
// section, or the empty string while the roster does not carry it yet. The
// section is keyed by the COMMON DIR, so both spellings are checked.
func emHO40SectionLabelForm(dir string) string {
	return `(or (cl-loop for s in (agent-repl-verbs--repo-sections)
                         when (member (plist-get (agent-repl-verbs--section-ref s) :dir)
                                      (list ` + elispString(dir) + ` (concat ` + elispString(dir) + ` "/.git")))
                         return (agent-repl-verbs--section-label s))
               "")`
}

// emHO40AwaitSectionLabel waits until the roster carries DIR's repository
// section and answers its label — the string the create command's repository
// picker is answered with.
func emHO40AwaitSectionLabel(t *testing.T, e *Emacs, dir string) string {
	t.Helper()
	raw := e.AwaitEval("the roster to carry the daemon's own repository section",
		emHO40SectionLabelForm(dir),
		func(raw json.RawMessage) bool {
			var got string
			return json.Unmarshal(raw, &got) == nil && got != ""
		})
	var label string
	if err := json.Unmarshal(raw, &label); err != nil {
		t.Fatalf("decode the repository section label for %s: %v", dir, err)
	}
	return label
}

// emHO40Create runs the ORDINARY create command with only its READERS
// stubbed, the way scenario 44 stubs its confirmation reader: the command's
// own call sites are what run, and nothing reaches past the command.
func emHO40Create(t *testing.T, e *Emacs, label string) {
	t.Helper()
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(label) + `))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (cond ((string-prefix-p "Initial prompt" prompt) ` + elispString(emHO40Prompt) + `)
                                ((string-prefix-p "Name" prompt) ` + elispString(emHO40TriggerName) + `)
                                (t "")))))
                 (agent-repl-create-workspace)
                 t)`)
}

// emHO40AddedName answers the ONE name the registry gained, by set
// difference — the daemon mints the name, so its sort position is not the
// test's to assume.
func emHO40AddedName(t *testing.T, before, after []string) string {
	t.Helper()
	had := map[string]bool{}
	for _, name := range before {
		had[name] = true
	}
	var added []string
	for _, name := range after {
		if !had[name] {
			added = append(added, name)
		}
	}
	if len(added) != 1 {
		t.Fatalf("the workspace registry gained %v (before %v, after %v), want exactly one workspace", added, before, after)
	}
	return added[0]
}

// TestEmacsHandoverTransfersAtFreeness is scenario 40.
//
// The assertion is the HANDOVER: a successor announced by a real self-merge
// rollout is attached, and at freeness it is PROMOTED to
// `agent-repl-link--primary` with the old connection released — not a
// reconnect that happened to find a new address, and not Emacs's own dial.
func TestEmacsHandoverTransfersAtFreeness(t *testing.T) {
	t.Parallel()
	// Arrange: a world whose Emacs — and therefore whose daemon — is told
	// which checkout is the daemon's OWN, and given a merge gate that
	// passes, so a commit landing on it runs the daemon's one deploy.
	box := requireSandbox(t)
	selfRepo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "self-repo"))
	gate := harness.NewTestAllScript(t, selfRepo.Dir)
	gate.SetExitCode(0)
	gate.SetStdout("e2e: passed in 1s\n")
	w := NewEmacsWorld(t, box,
		WithEmacsEnv("AGENT_REPL_SELF_REPO_DIR", selfRepo.Dir),
		WithEmacsEnv("AGENT_REPL_TEST_ALL_SCRIPT", gate.Path))
	// Arrange: the landing's ONE DEPLOY finds the daemon stale. The world's
	// fake build stages what runs; staged with a daemon binary that is not
	// the running one, the deploy hands the daemon over blue-green, and that
	// handover's announcement is the one that carries a successor's address.
	w.Deploy.Stage(harness.DeployStaleDaemon)
	e := w.Emacs
	e.EnsureDaemon()

	// Arrange: a HEADLESS workspace — registered through the ordinary
	// command, never opened — so it has zero rendezvous participants and is
	// free the instant the handover looks at it.
	headless, headlessDir := emGHIRegister(t, e, box, "repo-handover-headless")
	emGHISelect(t, e, headlessDir, headless)

	// Arrange: the hooks are armed BEFORE anything can announce, so neither
	// edge can be missed.
	e.Eval(emHO40Instrument)

	// Arrange: the daemon's own checkout is registered too, so the roster
	// carries the repository section the create command picks from.
	before := e.EvalStrings(emGHIWorkspaceNamesForm)
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(selfRepo.Dir) + `)`)
	label := emHO40AwaitSectionLabel(t, e, selfRepo.Dir)
	e.AwaitEval("the daemon's own checkout to appear in Emacs's registry",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 })

	// Act: create the trigger workspace on that repository, through the
	// ordinary command, and let its turn conclude — a merge is enqueued from
	// idle, never mid-turn.
	beforeCreate := e.EvalStrings(emGHIWorkspaceNamesForm)
	emHO40Create(t, e, label)
	after := decodeStrings(e.AwaitEval("the trigger workspace to appear in Emacs's registry",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(beforeCreate)+1 }))
	trigger := emHO40AddedName(t, beforeCreate, after)
	emGHIAwaitStatus(t, e, trigger, "the trigger workspace's opening turn to conclude", emGHISettledArms...)

	triggerDir := e.EvalString(`(or (plist-get (agent-repl-host-ref ` + elispString(trigger) + `) :dir) "")`)
	if triggerDir == "" {
		t.Fatalf("the trigger workspace %q holds no worktree directory, want the daemon-minted one", trigger)
	}

	// Act: land a scripted commit touching a daemon-subsystem path in that
	// worktree, then enqueue the merge through the ordinary command. The
	// merge lands, its one deploy finds the daemon stale, and the handover
	// announces.
	sha := selfRepo.CommitIn(triggerDir, emHO40SelfMergeTriggerPath, "trigger\n")
	selfRepo.SetPaths(sha, emHO40SelfMergeTriggerPath)
	e.Eval(`(agent-repl-merge-workspace ` + elispString(trigger) + `)`)

	// Assert: a successor was ATTACHED, from the announced address. This is
	// the half no plain bounce can produce — a `shutdown_announced` without
	// an address never reaches these hooks.
	successor := ""
	e.AwaitEvalFor(handoverAnnounceBound, "a successor to be attached from the announcement",
		`(or em-ho40-handover "")`,
		func(raw json.RawMessage) bool {
			var got string
			if json.Unmarshal(raw, &got) != nil || got == "" {
				return false
			}
			successor = got
			return true
		})

	// Assert: at freeness the successor is PROMOTED, and to the SAME address
	// it was attached at. A promotion to some other address would be a
	// reconnect wearing the handover's name.
	promoted := ""
	e.AwaitEvalFor(handoverPromoteBound, "the successor to be promoted at freeness",
		`(or em-ho40-promoted "")`,
		func(raw json.RawMessage) bool {
			var got string
			if json.Unmarshal(raw, &got) != nil || got == "" {
				return false
			}
			promoted = got
			return true
		})
	if promoted != successor {
		t.Fatalf("the promoted connection is at %q, want the announced successor's %q", promoted, successor)
	}

	// Assert: the promotion RELEASED the handover state — the successor slot
	// is empty and the primary is the successor's own connection. A primary
	// left on the old daemon, or a successor still held, would be a handover
	// that only half happened.
	e.AwaitEvalFor(handoverPromoteBound, "the successor slot to be released by the promotion",
		`(if agent-repl-link--successor nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	if got := e.EvalString(`(or (agent-repl-connect-connection-address agent-repl-link--primary) "")`); got != successor {
		t.Fatalf("the primary link is at %q after the handover, want the successor's %q", got, successor)
	}

	// Assert: the link is UP on the successor, and no reconnect loop was
	// armed — a handover is not an outage, and a timer still ticking would
	// say Emacs had treated it as one.
	emHOAwaitLinkUp(t, e, "the link to be up on the promoted successor")
	e.AwaitEvalFor(handoverPromoteBound, "no reconnect loop to be armed after the handover",
		`(if (timerp agent-repl-link--reconnect-timer) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	// Assert: the headless workspace TRANSFERRED — it is still Emacs's, on
	// the promoted connection, which is what "transfers at freeness" means
	// for a workspace with zero rendezvous participants.
	if got := e.EvalString(`(or (agent-repl-connect-connection-address (agent-repl-host-conn ` + elispString(headless) + `)) "")`); got != successor {
		t.Fatalf("the headless workspace %q is held on %q after the handover, want the successor's %q", headless, got, successor)
	}
}

// TestEmacsRestartKeepsTheSelectedWorkspace pins the ruling that EMACS OWNS
// THE USER'S SELECTION ACROSS A DAEMON RESTART.
//
// A relaunched daemon has no memory of `current`: it stamps it on whichever
// workspace re-registered first, and that order is Emacs's walk over its own
// registry rather than anything the user did. Measured before the fix, with
// a panel open on the FIRST workspace: the second workspace registered first
// on the link-up edge, its ref was stamped `current`, the roster push
// carrying it reached `agent-repl-roster-react-to-current` while Emacs's
// `agent-repl-host-last-selected-id` still named the old daemon's ref — so
// it read as a switch REQUEST and the frame ended on the other workspace's
// magit buffer, with the recovered panel behind a perspective nobody asked
// for.
//
// Both halves are asserted, because the first alone cannot tell a selection
// that was kept from a frame that merely happened to land right: the current
// workspace is UNCHANGED across the restart, and the panel's webview is
// still the buffer on screen.
//
// THE ARRANGEMENT IS THE HALF THAT MAKES IT A TEST. The user is put on the
// workspace that re-registers LAST, read off the link-up walk itself: a user
// standing on the one offered first is stamped `current` by accident and
// passes this scenario against the defect.
func TestEmacsRestartKeepsTheSelectedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: two registered workspaces, and THE USER STANDS ON THE ONE THAT
	// RE-REGISTERS LAST. That is the whole adversarial shape of the defect:
	// `agent-repl-host-on-link-up` walks the registry, so the workspace it
	// offers FIRST is the one the fresh daemon stamps `current` first, and a
	// user standing on that one would be carried past the bug by luck. The
	// walk order is read off Emacs rather than assumed, and the arrangement
	// fails loudly if it is not the two-workspace shape this test needs.
	w, e := emGHIWorld(t)
	one, oneDir := emGHIRegister(t, e, w.Emacs.box, "repo-selection-one")
	two, twoDir := emGHIRegister(t, e, w.Emacs.box, "repo-selection-two")
	// The walk carries every live workspace, including the ones with no
	// project dir (`main`, `none`), which link-up skips; only the two
	// registered here can be re-registered, so only their relative order
	// decides who is stamped first.
	walk := e.EvalStrings(`(agent-repl--live-ws-names)`)
	registered := make([]string, 0, 2)
	for _, name := range walk {
		if name == one || name == two {
			registered = append(registered, name)
		}
	}
	if len(registered) != 2 {
		t.Fatalf("the link-up walk %v carries %v of the registered workspaces, want both %q and %q",
			walk, registered, one, two)
	}
	first := registered[len(registered)-1]
	firstDir := oneDir
	if first == two {
		firstDir = twoDir
	}
	t.Logf("the user stands on %q, which the link-up walk %v offers LAST", first, walk)
	emGHISelect(t, e, firstDir, first)
	emGHIOpenPanel(t, e)
	pid := e.EvalInt(emHODaemonPIDForm)
	if pid <= 0 {
		t.Fatalf("the launcher holds no live daemon process (pid %d) before the restart", pid)
	}

	// Act: the stop and the ensure as two acts, which is the shape the defect
	// was measured in — the link goes fully down and Emacs re-registers from
	// scratch on a daemon that has never heard of these workspaces.
	e.Eval(`(agent-repl-frontend-daemon-stop)`)
	e.AwaitEvalFor(daemonStopBound, "the link to go down when the daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	e.Eval(`(agent-repl-frontend-daemon-ensure)`)
	emHOAwaitNewDaemon(t, e, pid)
	emHOAwaitLinkUp(t, e, "the link to come back on the new daemon")
	e.AwaitEval("both workspaces to be re-registered on the fresh daemon",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 2 })

	// Assert: the selection is the user's, still. It is read AFTER the
	// re-registration has settled, which is the window the defect lived in.
	e.AwaitEval("the user's selection to survive the daemon restart",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return json.Unmarshal(raw, &got) == nil && got == first
		})
	if got := decodeString(e.Eval(`(format "%s" (agent-repl--ws-current-name))`)); got != first {
		t.Fatalf("the current workspace is %q after the restart, want %q kept", got, first)
	}

	// Assert: and Emacs RE-ASSERTED it, rather than the frame merely having
	// landed right. The stamp the fresh daemon makes on its own is whichever
	// workspace re-registered first, so "the selection is still the user's"
	// is only a guarantee when Emacs said so again: the re-assertion is read
	// off Emacs's own log line, and the suppression it holds the frame with
	// is released again afterwards.
	e.AwaitTrue("Emacs to re-assert the user's selection on the fresh daemon",
		`(with-current-buffer "*Messages*"
                   (and (string-match-p "elisp.host.link-up-reselect ws=" (buffer-string)) t))`)
	e.AwaitTrue("the frame suppression to be released once the re-select is acknowledged",
		`(if agent-repl-host-reselect-pending nil t)`)

	// Assert: and the panel is still the buffer on screen, which is what the
	// user actually loses when the selection moves — the frame ended on the
	// other workspace's magit buffer.
	e.AwaitEval("the panel's webview to still be the buffer on screen",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			for _, name := range decodeStrings(raw) {
				if name == "*agent-frontend-"+first+"*" {
					return true
				}
			}
			return false
		})
}
