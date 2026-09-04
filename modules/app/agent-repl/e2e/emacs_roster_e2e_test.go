package e2e

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// AREA E of EMACS-LAYER-SPEC.md: ROSTER PAINT, MODELINE AND ATTENTION.
// Scenarios 26 through 30. EMACS-ONLY: a Connect-dialing client sees the
// roster frames but never sees what Emacs DECIDED to paint from them.
//
// The tab paint itself is SVG sized to the tab-bar line height, which a tty
// frame cannot rasterize. That is not a gap and this file does not pretend
// otherwise: the paint DECISION is data — the arm vocabulary,
// `agent-repl-status-color-table', `agent-repl--color-by-name' and
// `agent-repl-status-blink-schedule' — and the decision is the claim.
// Rasterization is not this layer's.
//
// The one deliberate exception is scenario 30, whose subject IS the rendered
// string: `agent-repl-link-drain-segment' is a mode-line composition, and it
// is asserted as text on purpose.

// ---------------------------------------------------------------------------
// 26. RosterArmsPaintTheTabInOrder
// ---------------------------------------------------------------------------

// TestEmacsRosterArmsPaintTheTabInOrder is scenario 26.
//
// The arms are collected INSIDE Emacs on `agent-repl-roster-update-functions'
// — the module's own per-push hook — rather than by polling a hash table from
// Go. A poll samples; the hook sees every push, so a transient running arm
// cannot be missed and the walk cannot be observed out of order.
func TestEmacsRosterArmsPaintTheTabInOrder(t *testing.T) {
	s := newEmacsScenario(t)
	e := s.E
	installArmObserver(t, e)

	// Record each DISTINCT arm this workspace's row moves through, in the
	// order the pushes carried them.
	e.Eval(`(progn
             (defvar agent-repl-e2e--arms nil)
             (setq agent-repl-e2e--arms nil)
             (defun agent-repl-e2e--record-arm (_roster)
               (let ((arm (agent-repl-e2e--arm-of ` + elispString(s.Name) + `)))
                 (when (and arm (not (eq arm (car agent-repl-e2e--arms))))
                   (push arm agent-repl-e2e--arms))))
             (add-hook 'agent-repl-roster-update-functions #'agent-repl-e2e--record-arm)
             t)`)

	// Drive ONE ordinary turn from the composer.
	typeIntoComposer(e, s.Input, "paint the tab through one turn")
	e.KeysIn(s.Input, "RET")

	// The walk ends on a SETTLED arm — the roster's own vocabulary answers
	// which those are, so this test does not restate the list.
	e.AwaitTrue("the roster row to reach a settled arm",
		`(and (memq (agent-repl-e2e--arm-of `+elispString(s.Name)+`)
                    agent-repl-roster-settled-statuses)
               t)`)

	arms := decodeStrings(e.Eval(`(mapcar (lambda (a) (format "%s" a)) (reverse agent-repl-e2e--arms))`))
	if len(arms) < 2 {
		t.Fatalf("the row showed only %v; a turn must walk a running arm to a settled one", arms)
	}

	// It passed through the RUNNING half on the way. The submitting and
	// thinking arms are the ones the spec names, and both belong to the
	// module's own running set.
	sawRunning := decodeBool(e.Eval(`(and (seq-some (lambda (a)
                                                       (memq a agent-repl-roster-running-statuses))
                                                     agent-repl-e2e--arms)
                                          t)`))
	if !sawRunning {
		t.Errorf("the row never showed a running arm; it walked %v", arms)
	}

	// EVERY arm observed is one the frozen schema declares, and EVERY one
	// resolves through the color table to a color `agent-repl--color-by-name'
	// knows. The roster's arm vocabulary is the ONE source for tab coloring,
	// so an arm that paints nothing is a defect even when the turn ran.
	unresolved := decodeStrings(e.Eval(`(let (bad)
             (dolist (arm agent-repl-e2e--arms)
               (let ((color (cdr (assq arm agent-repl-status-color-table))))
                 (cond
                  ((not (memq arm agent-repl-wire-roster-row-status-keywords))
                   (push (format "%s: not a declared arm" arm) bad))
                  ((null color)
                   (push (format "%s: no color-table entry" arm) bad))
                  ((and (not (equal color "none"))
                        (null (cdr (assoc color agent-repl--color-by-name))))
                   (push (format "%s: color %s unknown to agent-repl--color-by-name" arm color)
                         bad)))))
             bad)`))
	if len(unresolved) != 0 {
		t.Errorf("arms that do not resolve to a paint: %v (walk was %v)", unresolved, arms)
	}
}

// ---------------------------------------------------------------------------
// 27. UnknownRosterArmIsRefusedLoudly
// ---------------------------------------------------------------------------

// TestEmacsUnknownRosterArmIsRefusedLoudly is scenario 27.
//
// A daemon this suite owns cannot be made to push an undeclared arm — and it
// must not be: the fake SDK is the sole writer of vendor files and no test
// hand-writes a frame onto the wire. So the row is offered to the module's
// own decode entry point, `agent-repl-wire-decode-roster-row', which is the
// documented ingress every push travels through. The claim is that it
// REFUSES rather than defaulting to some fallback dot.
func TestEmacsUnknownRosterArmIsRefusedLoudly(t *testing.T) {
	s := newEmacsScenario(t)
	e := s.E

	// The row carries a status arm the frozen schema does not declare. The
	// key check is what refuses it, per the decoder's own docstring: "an arm
	// this build does not know is refused as an unknown field".
	got := decodeStrings(e.Eval(`(condition-case err
             (progn
               (agent-repl-wire-decode-roster-row
                '((workspace . nil) (quantumFlux . nil)))
               (list "decoded" "no error"))
           (agent-repl-wire-error
            (list (format "%s" (car err)) (format "%S" (cdr err))))
           (error
            (list (format "%s" (car err)) (format "%S" (cdr err)))))`))

	if len(got) != 2 {
		t.Fatalf("malformed readback %v", got)
	}
	if got[0] != "agent-repl-wire-error" {
		t.Fatalf("an undeclared status arm produced %s %s, want a loud agent-repl-wire-error",
			got[0], got[1])
	}
	if !strings.Contains(got[1], "quantumFlux") || !strings.Contains(got[1], "unknown field") {
		t.Errorf("the refusal does not name the undeclared arm and its reason: %s", got[1])
	}
}

// ---------------------------------------------------------------------------
// 28. PermissionAskFiresTheAttentionMarker
// ---------------------------------------------------------------------------

// TestEmacsPermissionAskFiresTheAttentionMarker is scenario 28.
//
// EMACS ANSWERS NOTHING. There is no permission-answering command in `lisp/'
// — the notification policy is Emacs's whole reaction to an ask — so this
// asserts exactly that reaction and stops. The ask is answered daemon-side,
// where `permission_e2e_test.go' already covers it.
//
// Two arrangements are forced because the reaction Emacs picks depends on
// facts about the DESKTOP, which a tty frame in a container cannot supply
// honestly:
//
//   - `agent-repl--emacs-focused-p' is overridden to t. It is an environment
//     probe, stubbed the same way an interactive `completing-read' is; a
//     container has no window system to answer it truthfully.
//   - the workspace under the ask is NOT the selected one, which is the case
//     host.el routes to `agent-repl-status-blink-tab'. A second workspace is
//     registered and selected through the ordinary command so the first is
//     genuinely unselected.
func TestEmacsPermissionAskFiresTheAttentionMarker(t *testing.T) {
	box := requireSandbox(t)

	// THE ORDER PROBLEM, and the gate that solves it. `agent-repl-send' —
	// the ONLY send there is — submits to the CURRENT workspace, so a turn
	// can only ever be started on the SELECTED one; typing into an
	// unselected workspace's composer and pressing RET submits nothing (the
	// current workspace's composer is empty) and the ask never fires. So the
	// turn is started while this workspace IS selected and PARKED on the
	// fake's turn gate (hibernation_e2e_test.go's
	// turnGatePathEnv/turnGateTextEnv) BEFORE its ask goes out; the second
	// workspace is then selected, and only then is the gate opened. The ask
	// therefore arrives with this workspace genuinely unselected, which is
	// the case `agent-repl-host--notify' routes to
	// `agent-repl-status-blink-tab'.
	gatePath := filepath.Join(box.Scratch(), "perm-ask-gate")
	const askPrompt = "!perm-hold"
	s := newEmacsScenario(t,
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, askPrompt))
	e := s.E

	e.Eval(`(progn
             (defun agent-repl-e2e--focused (&rest _) t)
             (advice-add 'agent-repl--emacs-focused-p :override #'agent-repl-e2e--focused)
             t)`)

	// The fake SDK's parked-permission scenario: the ask goes out and stays
	// outstanding, so the notification is guaranteed to arrive while the
	// turn is still running. It is submitted through the ordinary composer
	// RET, whose binding is asserted rather than assumed — a RET that
	// resolved to anything else would submit nothing and fail this test
	// silently five seconds later.
	typeIntoComposer(e, s.Input, askPrompt)
	if want, got := "agent-repl-send", e.BindingForIn(s.Input, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "RET")
	emGHIAwaitStatus(t, e, s.Name, "the gated turn to be in flight before the switch", emGHIRunningArms...)

	// A SECOND workspace, selected, so the first is unselected.
	other := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo-other"))
	otherName := addProjectWorkspace(t, e, other.Dir)
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == otherName })

	// The gate opens only now, so the ask fires against an unselected
	// workspace.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("e2e: open the turn gate: %v", err)
	}

	e.AwaitTrue("the unselected workspace's attention marker to be drawn",
		`(and (agent-repl-status-attention-visible-p `+elispString(s.Name)+`) t)`)

	// The CADENCE is data, and it is the one place every surface reads it
	// from — the webapp sidebar implements the same spec from the same
	// message, and a divergence between the two is a defect.
	schedule := decodeStrings(e.Eval(`(mapcar (lambda (step)
                                                  (format "%s %s" (car step) (if (cdr step) "on" "off")))
                                                agent-repl-status-blink-schedule)`))
	want := []string{"0.0 on", "0.5 off", "1.0 on", "1.5 off", "2.0 on"}
	if len(schedule) != len(want) {
		t.Fatalf("the blink schedule has %d steps %v, want %v", len(schedule), schedule, want)
	}
	for i := range want {
		if schedule[i] != want[i] {
			t.Errorf("blink step %d is %q, want %q", i, schedule[i], want[i])
		}
	}
}

// ---------------------------------------------------------------------------
// 29. FinishEdgeFiresTheReadyReaction
// ---------------------------------------------------------------------------

// TestEmacsFinishEdgeFiresTheReadyReaction is scenario 29.
//
// The banner is Emacs's OWN presentation policy: the daemon publishes that
// the turn finished and never asks whether Emacs is focused. So both halves
// of the policy input are supplied deliberately —
// `agent-repl--emacs-focused-p' is overridden to nil (an unfocused
// Emacs is the case that posts), and the notification backend is the
// module's own `agent-repl-notify-make-fake-backend' test seam, which proves
// what Emacs DECIDED to post without any host notification tool existing.
func TestEmacsFinishEdgeFiresTheReadyReaction(t *testing.T) {
	s := newEmacsScenario(t)
	e := s.E

	e.Eval(`(progn
             (defvar agent-repl-e2e--banners nil)
             (setq agent-repl-e2e--banners nil)
             (defun agent-repl-e2e--unfocused (&rest _) nil)
             (advice-add 'agent-repl--emacs-focused-p :override #'agent-repl-e2e--unfocused)
             (setq agent-repl--notification-backend
                   (agent-repl-notify-make-fake-backend 'agent-repl-e2e--banners))
             ;; Record that the finish edge ran its hook at all, so a missing
             ;; banner can be told apart from a missing edge.
             (defvar agent-repl-e2e--finished nil)
             (setq agent-repl-e2e--finished nil)
             (defun agent-repl-e2e--note-finish (ws)
               (push ws agent-repl-e2e--finished))
             (add-hook 'agent-repl-roster-finish-functions #'agent-repl-e2e--note-finish)
             t)`)

	typeIntoComposer(e, s.Input, "finish this turn and tell me about it")
	e.KeysIn(s.Input, "RET")

	e.AwaitTrue("the finish edge to run agent-repl-roster-finish-functions",
		`(and (member `+elispString(s.Name)+` agent-repl-e2e--finished) t)`)

	// The banner body is frozen as "Agent ready: <name>": the workspace name
	// is the fact the user scans for in a stack of notifications. The post
	// is scheduled through `agent-repl-notify-delay', so the recording is
	// awaited rather than read once.
	bannersForm := `(mapcar (lambda (entry) (format "%s" (nth 2 entry))) agent-repl-e2e--banners)`
	raw := e.AwaitEval("the notification backend to record the ready banner", bannersForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) > 0 })

	want := "Agent ready: " + s.Name
	banners := decodeStrings(raw)
	found := false
	for _, banner := range banners {
		if banner == want {
			found = true
		}
	}
	if !found {
		t.Errorf("the recorded banners are %v, want one reading %q", banners, want)
	}
}

// ---------------------------------------------------------------------------
// 30. DrainScheduleDrawsTheStandingBanner
// ---------------------------------------------------------------------------

// TestEmacsDrainScheduleDrawsTheStandingBanner is scenario 30.
//
// `agent-repl-daemon-shutdown-schedule' prompts for the reason through
// `completing-read', so the pick is stubbed FOR THE DURATION OF THE ONE CALL
// — the standard ERT way, which leaves the command running its own
// argument-collection code path — rather than the command being bypassed.
//
// This scenario's assertion on `agent-repl-link-drain-segment' is a RENDERED
// STRING, deliberately: the segment's own composition is the subject, and it
// is the sanctioned exception to "never scrape human text where a variable
// exists". The variable behind it (`agent-repl-link-drain') is asserted too.
func TestEmacsDrainScheduleDrawsTheStandingBanner(t *testing.T) {
	s := newEmacsScenario(t)
	e := s.E

	// Five minutes out: far enough that the daemon is still serving for the
	// whole test and its teardown.
	const drainMinutes = 5
	const reason = "maintenance"

	before := e.EvalInt(`(truncate (* 1000 (float-time)))`)
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(reason) + `)))
              (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(drainMinutes) + `)
              t)`)

	// The daemon echoes the schedule back as a `drain_scheduled' push; the
	// standing schedule is what Emacs then holds.
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs",
		`(and agent-repl-link-drain t)`)

	if arm := e.EvalString(`(format "%s" (plist-get (plist-get agent-repl-link-drain :reason) :arm))`); arm != ":"+reason {
		t.Errorf("the standing drain's reason arm is %q, want %q", arm, ":"+reason)
	}

	atMS := e.EvalInt(`(plist-get agent-repl-link-drain :at-ms)`)
	const minuteMS = 60 * 1000
	if lo, hi := before+drainMinutes*minuteMS, before+drainMinutes*minuteMS+int(DefaultTimeout.Milliseconds()); atMS < lo || atMS > hi {
		t.Errorf("the standing drain fires at %d ms, want it inside [%d, %d]", atMS, lo, hi)
	}

	// The rendered segment. Its shape is "drain HH:MM · <reason>".
	segment := e.EvalString(`(or agent-repl-link-drain-segment "")`)
	if !strings.HasPrefix(segment, "drain ") {
		t.Fatalf("the drain segment is %q, want it to open with \"drain \"", segment)
	}
	if !strings.HasSuffix(segment, "· "+reason) {
		t.Errorf("the drain segment %q does not name the reason %q", segment, reason)
	}

	// And it is ON the mode line, not merely computed: a segment nothing
	// draws is a banner the user never sees.
	if !decodeBool(e.Eval(`(and (memq 'agent-repl-link-drain-segment
                                        (default-value 'global-mode-string))
                                  t)`)) {
		t.Error("agent-repl-link-drain-segment is not in global-mode-string")
	}

}

// decodeBool reads an elisp truth value back. `json-encode' writes nil as
// `null' and t as `true', so anything but a JSON null is truth.
func decodeBool(raw json.RawMessage) bool {
	if isJSONNull(raw) {
		return false
	}
	return string(raw) != "false"
}
