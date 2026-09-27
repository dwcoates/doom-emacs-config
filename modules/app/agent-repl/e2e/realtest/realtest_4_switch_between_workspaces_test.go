//go:build realtest

package realtest

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"
)

// REALTEST 4 - SWITCH BETWEEN WORKSPACES.
//
// docs/REALTEST-PLAN.md, "Workspaces", item 4: "Switch between workspaces with
// `s-{`, `s-}` and `M-<n>`. Selection, tab highlight, panel and composer all
// follow."
//
// Four real chords, pressed into the real Emacs process, each one asserted
// five ways:
//
//  1. EMACS'S OWN SELECTION moved to the tab the DRAWN order says it should,
//     read back through a read-only probe rather than inferred from the log.
//  2. THE MODULE'S OWN RECORD for the switch names the same target. Emacs
//     agreeing with itself is one fact; the record the owner would read in
//     the log agreeing with it is a second, and the two are asserted apart
//     because they fail apart (a switch with no record is an instrumentation
//     hole, and a record naming a different target is a routing defect).
//  3. `last-command` is the module's own switch command, not the binding
//     Doom or evil would otherwise have resolved the chord to. This is what
//     separates "the chord reached the right keymap" from "something
//     switched workspaces".
//  4. THE CURSOR landed in the selected workspace's agent-repl input
//     composer, in evil NORMAL (command) state
//     (lisp/panels.el, `agent-repl--maybe-autoselect-input' and
//     `agent-repl--input-enter-command-state').
//  5. THE TAB BAR carries the selection indicator on exactly the selected
//     tab. Selection is an UNDERLINE and nothing else since lisp/status.el's
//     `agent-repl--render-tab' stopped painting a background for it, so the
//     assertion is that the underline is on one tab and that tab is the
//     current one.
//
// WHY THE NUMERALS GET THEIR OWN ITEM. `M-1` .. `M-9` reach
// `agent-repl-switch-to-workspace-N`, which indexes the DRAWN tab order, and
// that command exists because Doom's `+workspace/switch-to-N` indexes
// persp-mode's perspective list instead - a list whose slot 0 is Doom's own
// `main` and which carries `none` besides, so `M-1` landed on a splash screen
// and every numeral was off by one against the picture
// (lisp/keybindings.el and lisp/commands.el both carry the history). That was
// a real defect, so this realtest asserts the landing slot against the order
// the bar actually DRAWS, and says so by name when a miss has the off-by-one
// signature.
//
// THE HARVEST BAR IS IDENTICAL TO REALTEST 1: every WARN and ERROR written
// inside the run window, across every log, with no allowlist, into this run's
// own MANIFEST.md, and the test fails when the count is non-zero.
//
// THE KEY DRIVER IS REALTEST 1'S, UNCHANGED. keys.go activates Emacs for the
// instant of each keypress and restores the previously frontmost application
// (keydriver.swift says why a no-activation post reaches no key window). That
// momentary activation is accepted and is not chased here; it is also the
// focus edge that lets the parked webview pre-creation queue drain, which is
// why the composer this test asserts on exists at all.

// PRECONDITION: THREE OPEN WORKSPACES, NOT TWO.
//
// With exactly two tabs, `s-}` and `s-{` land on the SAME tab, so a run
// against two workspaces cannot tell a correct direction from a reversed one
// and would report green on the defect this item exists to catch. Three is
// the smallest bar on which left and right are distinguishable, and it is
// also the smallest on which `M-2` landing on slot 1 or slot 3 (the two
// off-by-one spellings) is visible.
const rt4MinimumWorkspaces = 3

// OBSERVATION CEILINGS, NOT BUDGETS - the same distinction realtest 1 draws.
// They bound how long this test waits before reporting that something did not
// happen, and they have no measured basis yet, which is stated rather than
// hidden: a ceiling firing is itself a finding to report.
const (
	// rt4ComposerCeiling is how long, after the warm-up chord has brought
	// Emacs forward, the selected workspace's input composer may take to
	// exist as a live window. The parked pre-creation queue drains on that
	// focus edge, so this covers the same work realtest 1's show phase
	// covers, and it is sized alongside that ceiling rather than guessed
	// smaller.
	rt4ComposerCeiling = 120 * time.Second
	// rt4SwitchCeiling is how long ONE chord may take to produce its
	// selection change, its record and its composer landing. A switch is
	// local work in the command loop, but it drives a persp activation and
	// a panel layout behind it, so this is generous on purpose.
	rt4SwitchCeiling = 60 * time.Second
)

// rt4ProbeForm is the ONE read-only probe this realtest reads its state
// through, and it answers every question one act asks at a single instant.
//
// It is one probe rather than six deliberately: selection, the drawn order,
// the selected window, evil's state and the tab bar's underline have to be
// read as one consistent picture, and six round trips would let the editor
// move between them and produce a self-contradictory report nobody could rule
// on.
//
// The underline is read off the RENDERED tab strings, not off a variable
// saying which tab is selected, because the plan's assertion is about what the
// bar draws. `agent-repl--render-tab' spells the marker as an inline
// `:underline t' on the separator, bracket and name runs of the selected entry
// and omits the key entirely on every other entry, so a face-property scan for
// it is exact: a face SYMBOL that happens to define an underline in its own
// defface does not carry the key here and cannot be mistaken for the marker.
//
// Nothing here writes. `require` of cl-lib and json is a no-op in a running
// Doom, every other form is a read, and the lambda is local to the probe.
const rt4ProbeForm = `
(progn
  (require 'cl-lib)
  (let* ((underlinedp
          (lambda (entry)
            (let ((pos 0) (found nil))
              (while (and (not found) pos (< pos (length entry)))
                (when (string-match-p ":underline"
                                      (format "%S" (get-text-property pos 'face entry)))
                  (setq found t))
                (setq pos (next-single-property-change pos 'face entry)))
              found)))
         (drawn (and (fboundp 'agent-repl--ws-tabline-names)
                     (agent-repl--ws-tabline-names)))
         (roster (and (fboundp 'agent-repl-roster-tab-order)
                      (agent-repl-roster-tab-order)))
         (current-ws (and (fboundp 'agent-repl--ws-current-name)
                          (agent-repl--ws-current-name)))
         (current (or current-ws ""))
         (entries (and drawn
                       (fboundp 'agent-repl--tabline-rendered-entries)
                       (agent-repl--tabline-rendered-entries drawn)))
         (underlined (cl-loop for name in drawn
                              for entry in entries
                              when (funcall underlinedp entry)
                              collect name))
         (input-win (and (fboundp 'agent-repl-window--panel-window)
                         current-ws
                         (agent-repl-window--panel-window :input current-ws)))
         (sel (selected-window)))
    (list (cons 'current current)
          (cons 'drawn (vconcat drawn))
          (cons 'roster (vconcat roster))
          (cons 'selected_buffer (or (buffer-name (window-buffer sel)) ""))
          (cons 'input_buffer (if (window-live-p input-win)
                                  (or (buffer-name (window-buffer input-win)) "")
                                ""))
          (cons 'selected_is_input (if (and (window-live-p input-win)
                                            (eq input-win sel))
                                       "t" "nil"))
          (cons 'evil_state (with-current-buffer (window-buffer sel)
                              (if (boundp 'evil-state) (format "%s" evil-state) "unbound")))
          (cons 'underlined (vconcat underlined)))))`

// rt4State is one instant of the editor's selection, as the probe answers it.
//
// The two booleans travel as the strings "t" and "nil" rather than as JSON
// booleans: `json-encode` renders elisp nil as null, which arrives here
// indistinguishable from a probe that could not answer the question at all,
// and those two are different findings.
type rt4State struct {
	Current         string   `json:"current"`
	Drawn           []string `json:"drawn"`
	Roster          []string `json:"roster"`
	SelectedBuffer  string   `json:"selected_buffer"`
	InputBuffer     string   `json:"input_buffer"`
	SelectedIsInput string   `json:"selected_is_input"`
	EvilState       string   `json:"evil_state"`
	Underlined      []string `json:"underlined"`
}

// rt4ReadState performs the probe.
func rt4ReadState(ctx context.Context, client *Client) (rt4State, error) {
	var state rt4State
	raw, err := client.Read(ctx, rt4ProbeForm)
	if err != nil {
		return state, err
	}
	if err := json.Unmarshal(raw, &state); err != nil {
		return state, fmt.Errorf("decode the selection probe's answer %s: %w", raw, err)
	}
	return state, nil
}

// THE CHORDS THIS REALTEST ADDS.
//
// keys.go already carries `s-}` (SwitchRight) and `M-2` (SwitchToSecond) for
// realtest 1's key self-test, and they are reused here rather than respelled.
// The two below are this file's own, named with an rt4 prefix so a parallel
// author adding chords to another realtest cannot collide with them.
//
// On this Emacs the Command key is `super` and Option is `meta` (the NS
// defaults, which ~/.config/doom does not override), so `s-{` is
// Command+Shift+[ and `M-1` is Option+1.
var (
	// rt4SwitchLeft is `s-{`, bound to `agent-repl-switch-left`
	// (lisp/keybindings.el).
	rt4SwitchLeft = Chord{
		Emacs:     "s-{",
		Keycode:   33, // [
		Modifiers: []string{"command", "shift"},
		Why:       "selects the previous workspace tab; it changes the selection and nothing else",
	}
	// rt4SwitchToFirst is `M-1`, bound to `agent-repl-switch-to-workspace-1`
	// through the numerals minor-mode map. It is the numeral Doom's own
	// `+workspace/switch-to-N` got most visibly wrong, landing on the splash
	// screen with no tab highlighted, so it is the second numeral this
	// realtest presses.
	rt4SwitchToFirst = Chord{
		Emacs:     "M-1",
		Keycode:   18, // 1
		Modifiers: []string{"option"},
		Why:       "selects the first drawn workspace tab; it changes the selection and nothing else",
	}
)

// rt4Act is one chord and everything the run expects of it.
type rt4Act struct {
	// Chord is the key the owner would press.
	Chord Chord
	// Command is the command Emacs's keymap must resolve the chord to.
	Command string
	// RecordRe matches the module's own record for this switch and captures
	// the target workspace name in its first group.
	RecordRe *regexp.Regexp
	// Target answers, from the state read just BEFORE the press, which
	// workspace the drawn tab order says this chord must land on.
	Target func(before rt4State) (string, error)
	// WrongCommands are the bindings this chord would have resolved to had
	// the module's own keymap not won, named so a failure says which layer
	// took the chord rather than only that the wrong thing happened.
	WrongCommands []string
}

// The four acts, in the order they are pressed. Each one's expected target is
// computed from the state read immediately before its own press, so the acts
// do not depend on each other's outcome and a failure in one does not cascade
// into a misleading failure in the next.
func rt4Acts() []rt4Act {
	return []rt4Act{
		{
			Chord:    SwitchRight,
			Command:  "agent-repl-switch-right",
			RecordRe: regexp.MustCompile(`^elisp\.commands\.cycle n=1 target=(\S+)`),
			Target:   func(before rt4State) (string, error) { return rt4Neighbor(before, 1) },
			// `s-}` is unbound in a stock Doom, but evil's own
			// buffer-motion bindings and Doom's workspace chords are the
			// two layers a super chord most plausibly falls through to.
			WrongCommands: []string{"+workspace/switch-right", "evil-next-buffer", "next-buffer"},
		},
		{
			Chord:         rt4SwitchLeft,
			Command:       "agent-repl-switch-left",
			RecordRe:      regexp.MustCompile(`^elisp\.commands\.cycle n=-1 target=(\S+)`),
			Target:        func(before rt4State) (string, error) { return rt4Neighbor(before, -1) },
			WrongCommands: []string{"+workspace/switch-left", "evil-prev-buffer", "previous-buffer"},
		},
		{
			Chord:    SwitchToSecond,
			Command:  "agent-repl-switch-to-workspace-2",
			RecordRe: regexp.MustCompile(`^elisp\.commands\.switch-to-workspace n=2 target=(\S+)`),
			Target:   func(before rt4State) (string, error) { return rt4Slot(before, 2) },
			// THE OFF-BY-ONE LAYER, BY NAME. `+workspace/switch-to-1` is
			// what `M-2` resolved to before the module bound the numerals,
			// because Doom counts persp-mode's list and its slot 0 is
			// `main`.
			WrongCommands: []string{"+workspace/switch-to-1", "digit-argument", "evil-digit-argument-or-evil-beginning-of-line"},
		},
		{
			Chord:         rt4SwitchToFirst,
			Command:       "agent-repl-switch-to-workspace-1",
			RecordRe:      regexp.MustCompile(`^elisp\.commands\.switch-to-workspace n=1 target=(\S+)`),
			Target:        func(before rt4State) (string, error) { return rt4Slot(before, 1) },
			WrongCommands: []string{"+workspace/switch-to-0", "digit-argument", "evil-digit-argument-or-evil-beginning-of-line"},
		},
	}
}

// rt4Neighbor is the tab `n` places from the current one along the DRAWN
// order, wrapping, which is exactly what `agent-repl--workspace-cycle`
// computes. Computing it here from the drawn order rather than reading it out
// of the module is the point: the expectation has to come from the picture,
// not from the code under test.
func rt4Neighbor(before rt4State, n int) (string, error) {
	index := rt4IndexOf(before.Drawn, before.Current)
	if index < 0 {
		return "", fmt.Errorf("the current workspace %q is not on the drawn tab bar %v, so there is no slot to count from",
			before.Current, before.Drawn)
	}
	size := len(before.Drawn)
	return before.Drawn[((index+n)%size+size)%size], nil
}

// rt4Slot is the workspace in the Nth slot of the drawn bar, counting from 1.
func rt4Slot(before rt4State, n int) (string, error) {
	if n < 1 || n > len(before.Drawn) {
		return "", fmt.Errorf("the bar draws %d tab(s), so there is no slot %d to land on", len(before.Drawn), n)
	}
	return before.Drawn[n-1], nil
}

func rt4IndexOf(names []string, name string) int {
	for i, candidate := range names {
		if candidate == name {
			return i
		}
	}
	return -1
}

func TestRealtestSwitchBetweenWorkspaces(t *testing.T) {
	if os.Getenv(runGateEnv) != "1" {
		t.Skipf("realtest 4 drives the owner's real editor and runs only through bin/realtest.sh, which sets %s=1", runGateEnv)
	}
	ctx := context.Background()

	// THE PHASE BUDGETS ARE NEITHER ENFORCED NOR REPORTED HERE, and that is
	// not a skipped gate. Realtest 1 is a measurement of startup and is
	// judged against budgets.go; realtest 4 asserts BEHAVIOR and declares no
	// phase of its own, so there is no number for a budget to hold. It does
	// start the editor, and those phase timings are written into the
	// manifest as context, but the verdict here is the assertions and the
	// harvest.

	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("resolve the owner's home directory: %v", err)
	}

	// ITS OWN RUN DIRECTORY, ALWAYS. bin/realtest.sh exports one
	// AGENT_REPL_REALTEST_OUT for the whole invocation, so two realtests in
	// one run would write two MANIFEST.md files to the same path and the
	// second would erase the first. Each realtest takes a named subdirectory
	// of it instead, so a full run leaves one manifest per realtest.
	runDir := os.Getenv(outEnv)
	if runDir == "" {
		runDir = filepath.Join(home, ".claude-emacs", "realtest",
			fmt.Sprintf("realtest-4-%s", time.Now().Format("20060102-150405")))
	} else {
		runDir = filepath.Join(runDir, "realtest-4")
	}
	if err := os.MkdirAll(runDir, 0o755); err != nil {
		t.Fatalf("create the run directory %s: %v", runDir, err)
	}
	t.Logf("realtest 4 run directory: %s", runDir)

	socket := os.Getenv(socketEnv)
	if socket == "" {
		socket = filepath.Join(os.TempDir(), fmt.Sprintf("emacs%d", os.Getuid()), "server")
	}
	client := &Client{Socket: socket, Scratch: runDir}
	t.Logf("emacs server socket: %s", socket)
	t.Logf("emacsclient: %s", EmacsClientPath)

	stateDir := filepath.Join(home, ".claude-emacs")
	dbPath := StateDBPath(stateDir)
	openWorkspaces, closedWorkspaces, err := ReadWorkspaces(ctx, dbPath)
	if err != nil {
		t.Fatalf("read the workspaces the state database holds: %v", err)
	}
	t.Logf("the state database holds %d open workspace(s) and %d closed",
		len(openWorkspaces), len(closedWorkspaces))
	for _, ws := range openWorkspaces {
		t.Logf("  open workspace %s (%s) at %s", ws.ID, ws.Name, ws.Dir)
	}
	// A REGISTRY ONE SHORT IS NOT A REASON TO REFUSE. It used to be: the run
	// stopped here and told the owner to open workspaces until the bar drew
	// three, which on 2026-09-12 meant realtest 4 could not run at all against
	// a registry holding two. It bootstraps its own instead, below, once there
	// is an editor to register through (bootstrap.go).

	env := RealEnv(home, openWorkspaces)
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("enumerate the logs to harvest: %v", err)
	}
	t.Logf("harvesting %d log source(s); the module's own global sink is %s", len(sources), env.ModuleLog)

	// THE WINDOW OPENS HERE, before anything is launched or pressed.
	started := time.Now()
	snapshot := TakeSnapshot(sources)

	manifest := Manifest{
		Title:      "Realtest 4 - switch between workspaces",
		Started:    started,
		Workspaces: openWorkspaces,
	}
	manifest.Notes = append(manifest.Notes,
		"The phase budgets in e2e/realtest/budgets.go are NOT checked by this realtest: it declares no "+
			"phase of its own and asserts behavior, not timings. Any startup timings below are context. "+
			"The log harvest IS enforced.")

	rt4EnsureEditor(ctx, t, client, sources, snapshot, openWorkspaces, &manifest)

	// THE THIRD TAB, BROUGHT BY THE RUN ITSELF. This is where the bootstrap
	// goes and not earlier: registering a directory is a command in the
	// editor, so there has to be one answering first.
	openWorkspaces = rt4BootstrapWorkspaces(ctx, t, client, dbPath, runDir, openWorkspaces, &manifest)
	manifest.Workspaces = openWorkspaces
	sources, err = EnumerateSources(RealEnv(home, openWorkspaces))
	if err != nil {
		t.Fatalf("re-enumerate the logs now that the run has bootstrapped its own workspaces: %v", err)
	}
	rt4AssertBarMatchesState(ctx, t, client, openWorkspaces)

	driver := rt4BuildKeyDriver(ctx, t, client, runDir, &manifest)
	focusBefore, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("read which application is frontmost before the first chord: %v", err)
	}

	// WHAT THE EDITOR WAS ALREADY STANDING AT, CLEARED BEFORE ANYTHING IS
	// PRESSED. This realtest ADOPTS a standing Emacs rather than starting one,
	// so it inherits whatever the owner or a previous run left half-typed —
	// and a bare `d` is an evil operator that does not expire and eats the
	// next key as its motion. That is precisely how this realtest's first act
	// once made a completely correct switch while `last-command` came back
	// `evil-delete`. inputstate.go carries the whole account; the clear is a
	// real `<escape>`, and what it cleared is reported.
	wsActClearPendingInput(ctx, t, client, driver, "the warm-up chord", &manifest)

	// THE WARM-UP CHORD, which is not one of the four measured acts.
	//
	// It exists for the focus edge, not for the switch. Emacs is
	// visible-but-unfocused until something activates it, the settled
	// webview invariant parks each workspace's pre-creation in exactly that
	// state, and the composer window the acts below assert on is downstream
	// of that queue draining. So one harmless `s-}` is pressed first, purely
	// to produce the edge, and the wait after it is for the composer to
	// exist rather than for a clock.
	warmUpKeysBefore, err := RecentKeys(ctx, client)
	if err != nil {
		t.Fatalf("read Emacs's recent keys before the warm-up %s: %v", SwitchRight.Emacs, err)
	}
	if err := driver.Press(ctx, SwitchRight); err != nil {
		t.Fatalf("press the warm-up %s to bring Emacs forward for the first time: %v", SwitchRight.Emacs, err)
	}
	rt4ProveWarmUpLanded(ctx, t, client, warmUpKeysBefore, &manifest)
	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("warm-up chord %s pressed to produce the first focus edge, so the parked webview "+
			"pre-creation queue drains and the input composer exists before the measured acts", SwitchRight.Emacs))
	rt4WaitForComposer(ctx, t, client)

	// THE FOUR ACTS.
	for _, act := range rt4Acts() {
		rt4RunAct(ctx, t, client, driver, sources, snapshot, started, act, &manifest)
	}

	// THE EXACT INVERSE, asserted across the first two acts rather than
	// inside either of them: `s-{` undoing `s-}` is a property of the PAIR,
	// and each act already asserted its own target, so this is the one
	// statement neither could make alone.
	rt4AssertInverse(ctx, t, client, driver, &manifest)

	focusAfter, err := FrontmostApp(ctx)
	if err != nil {
		t.Fatalf("read which application is frontmost after the acts: %v", err)
	}
	focusNote, focusFinding := focusAfterPressesNote("the chords", driver.KeepFocus, focusBefore, focusAfter)
	manifest.Notes = append(manifest.Notes, focusNote)
	if focusFinding {
		t.Errorf("%s", focusNote)
	}

	// THE HARVEST. Identical to realtest 1: the window closes here and the
	// sources are re-enumerated first, so rotation siblings and workspace
	// sinks the run itself created are read.
	manifest.Ended = time.Now()
	sources, err = EnumerateSources(env)
	if err != nil {
		t.Fatalf("re-enumerate the logs after the run: %v", err)
	}
	harvest, err := HarvestSources(sources, snapshot, Window{Start: started, End: manifest.Ended}, openWorkspaces)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Findings = harvest.Findings
	manifest.InfoCounts = harvest.InfoCounts

	messages, msgErr := client.Messages(ctx)
	if msgErr != nil {
		manifest.Findings = append(manifest.Findings, Finding{
			Kind:      KindMalformed,
			Source:    "*Messages*",
			Path:      "(emacs buffer)",
			Workspace: GlobalWorkspace,
			Note:      fmt.Sprintf("Emacs's *Messages* buffer could not be read, so this source was not harvested: %v", msgErr),
		})
	} else {
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, openWorkspaces)...)
		if err := os.WriteFile(filepath.Join(runDir, "Messages.txt"), []byte(messages), 0o644); err != nil {
			t.Fatalf("preserve the *Messages* buffer: %v", err)
		}
	}
	sortFindings(manifest.Findings)

	path, err := manifest.Write(runDir)
	if err != nil {
		t.Fatalf("write the run manifest: %v", err)
	}
	t.Logf("manifest: %s", path)

	if len(manifest.Findings) > 0 {
		t.Errorf("the log harvest found %d warning(s), error(s) or non-record(s) inside the run window; "+
			"every one is in %s, verbatim. There is no allowlist: nothing here is fixed by this test, "+
			"and the owner rules on each one (docs/REALTEST-PLAN.md).",
			len(manifest.Findings), path)
		for _, finding := range manifest.Findings {
			t.Logf("  [%s] %s %s (%s) %s | %s", finding.Kind, finding.Workspace, finding.Source,
				finding.Level, finding.Note, finding.Raw)
		}
	}
}

// rt4EnsureEditor gets the run a running editor with every workspace's tab
// drawn, and says which of the two ways it got there.
//
// A COLD START WHEN NOTHING IS ANSWERING, which is realtest 1's path exactly
// (coldStart, waitForUsable, assertEveryWorkspaceDrawn), reused rather than
// respelled.
//
// AN ADOPTION WHEN SOMETHING IS. bin/realtest.sh runs every TestRealtest* in
// one `go test`, and a realtest leaves the owner's editor standing, so by the
// time realtest 4 runs in a full invocation an Emacs is already answering.
// Failing there would make a full run impossible; quitting it would be a
// takeover, and the takeover decision belongs to bin/realtest.sh and nothing
// else. So the standing editor is adopted, and the drawn-tab precondition is
// then asserted from LIVE STATE rather than from the log: an adopted Emacs
// drew its tabs before this run's window opened, so no `tab-open` record for
// them exists inside it and a log-based assertion would report a correctly
// drawn bar as an empty one.
//
// THE BAR ASSERTION ITSELF IS THE CALLER'S, not this function's: the run
// bootstraps whatever workspaces the registry lacks between the two, and a bar
// checked before that would be checked against a registry the run is about to
// change.
func rt4EnsureEditor(ctx context.Context, t *testing.T, client *Client, sources []Source, snap Snapshot, expected []Workspace, manifest *Manifest) {
	t.Helper()

	if client.Alive(ctx) {
		note := fmt.Sprintf("an Emacs was already answering %s, so this realtest ADOPTED it rather than "+
			"starting one: quitting a standing editor is a takeover and only bin/realtest.sh may decide that. "+
			"The drawn tab bar is asserted from live state below, because an adopted editor drew its tabs "+
			"before this run's harvest window opened", client.Socket)
		manifest.Notes = append(manifest.Notes, note)
		t.Logf("%s", note)
		return
	}

	const run = 1
	launch := coldStart(ctx, t, client, run)
	phases := waitForUsable(ctx, t, run, sources, snap, launch.SpawnedAt, expected)
	measurements := phases.Measure()

	manifest.Runs = append(manifest.Runs, ManifestRun{
		Index:        run,
		Method:       launch.Method,
		SpawnedAt:    launch.SpawnedAt,
		DaemonPath:   phases.DaemonPath,
		FrontBefore:  launch.FrontBefore,
		FrontAfter:   launch.FrontAfter,
		Disturbed:    launch.DisturbedOwner,
		Measurements: measurements,
	})
	t.Logf("cold start via %s: daemon %s", launch.Method, orUnknown(phases.DaemonPath))
	for _, m := range measurements {
		if m.Note != "" {
			t.Logf("  phase %-14s %-14s NOT OBSERVED: %s", m.Phase, m.Workspace, m.Note)
			continue
		}
		t.Logf("  phase %-14s %-14s %s from spawn", m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond))
	}
	assertEveryWorkspaceDrawn(t, run, expected, phases)
	verifyVendorGuard(ctx, t, client, run)
}

// rt4AssertBarMatchesState checks that the bar this run will navigate draws a
// tab for every workspace the state database holds, and that the order
// NAVIGATION counts along is the order the bar DRAWS.
//
// The two orders are separate reads on purpose. `agent-repl-roster-tab-order`
// is what `agent-repl--workspace-cycle` and `agent-repl-switch-to-workspace`
// index, and `agent-repl--ws-tabline-names` is that order filtered to the
// workspaces the perspective layer actually has, which is what the bar
// renders. When they differ, every numeral is counting slots the user cannot
// see, which is the same class of defect as the persp-list off-by-one and is
// reported in its own right rather than folded into whichever act happens to
// land wrong first.
func rt4AssertBarMatchesState(ctx context.Context, t *testing.T, client *Client, expected []Workspace) {
	t.Helper()
	state, err := rt4ReadState(ctx, client)
	if err != nil {
		t.Fatalf("read the drawn tab bar before the acts: %v", err)
	}
	t.Logf("the bar draws %d tab(s): %v", len(state.Drawn), state.Drawn)
	t.Logf("the roster's navigation order is: %v", state.Roster)
	t.Logf("the selected workspace is %q", state.Current)

	for _, ws := range expected {
		if rt4IndexOf(state.Drawn, ws.Name) < 0 {
			t.Errorf("workspace %s (%s) is open in the state database but the tab bar draws no tab for it "+
				"(the bar draws %v), so no chord in this realtest can reach it", ws.ID, ws.Name, state.Drawn)
		}
	}
	if strings.Join(state.Drawn, "\x1f") != strings.Join(state.Roster, "\x1f") {
		t.Errorf("the order navigation counts along and the order the bar draws disagree: "+
			"`agent-repl-roster-tab-order` is %v and `agent-repl--ws-tabline-names` is %v. "+
			"`M-<n>` and the cycle chords index the first and the user reads the second, so every numeral "+
			"is off against the picture for as long as they differ", state.Roster, state.Drawn)
	}
	if len(state.Drawn) < rt4MinimumWorkspaces {
		t.Fatalf("the bar draws %d tab(s) and realtest 4 needs %d: with fewer, `s-}` and `s-{` reach the "+
			"same tab and a reversed direction cannot be told from a correct one",
			len(state.Drawn), rt4MinimumWorkspaces)
	}
}

// rt4BootstrapWorkspaces registers scratch repositories until the registry
// holds the three open workspaces realtest 4 needs, and answers the whole open
// set.
//
// bootstrap.go carries the reasoning. What is worth reading here is the
// cleanup: each scratch repository is removed and each workspace it minted is
// closed through `t.Cleanup`, registered the moment the thing exists, so a run
// that fails halfway still leaves the registry without the rows it added. The
// residue registering leaves behind — one closed workspace row and its
// repository row, both naming a deleted path — is the one the substrate already
// reports for realtests 5 through 8, and it is reported here the same way, by
// the same function.
func rt4BootstrapWorkspaces(ctx context.Context, t *testing.T, client *Client, dbPath, runDir string,
	open []Workspace, manifest *Manifest) []Workspace {
	t.Helper()

	need := rt4BootstrapCount(len(open))
	if need == 0 {
		t.Logf("the registry holds %d open workspace(s), which is the %d realtest 4 needs or more, so it "+
			"bootstraps none", len(open), rt4MinimumWorkspaces)
		return open
	}

	note := fmt.Sprintf("BOOTSTRAP: the registry holds %d open workspace(s) and realtest 4 needs %d, so the run "+
		"registers %d scratch repository(ies) of its own under %s and closes and deletes them on the way out, "+
		"including on failure. None of the owner's repositories is touched",
		len(open), rt4MinimumWorkspaces, need, runDir)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)

	bootstrapped := append([]Workspace{}, open...)
	for index := 0; index < need; index++ {
		scratch := wsActScratchRepo(t, runDir, rt4BootstrapRepoName(index))
		t.Cleanup(func() { wsActRemoveScratchRepo(t, scratch) })

		if err := wsActRegisterDirectory(ctx, client, scratch); err != nil {
			t.Fatalf("%s", rt4BootstrapGuardMessage(scratch, fmt.Sprintf("the register command answered %v", err)))
		}

		all := wsActWaitForDB(ctx, t,
			fmt.Sprintf("the registry to hold the bootstrap directory %s", scratch), dbPath,
			func(all []Workspace) bool {
				_, ok := wsActWorkspaceByDir(all, scratch)
				return ok
			})
		workspace, ok := wsActWorkspaceByDir(all, scratch)
		if !ok {
			t.Fatalf("%s", rt4BootstrapGuardMessage(scratch,
				"the state database holds no workspace at that directory, so registering minted no identity"))
		}

		// The TAB name, which is what the bar draws and what the cleanup's
		// close command takes. It is the roster's spelling where the roster has
		// one, because the registry's name and the drawn name can differ.
		tabName := workspace.Name
		if rows, rowsErr := wsActRosterRows(ctx, client); rowsErr == nil {
			if row, has := wsActRowByID(rows, workspace.ID); has && row.Name != "" {
				tabName = row.Name
			}
		}
		record, name := workspace, tabName
		t.Cleanup(func() { wsActCleanupRegistered(ctx, t, client, dbPath, record, name) })

		tabs := wsActWaitForTabs(ctx, t, fmt.Sprintf("the bar to draw the bootstrap tab %q", tabName), client,
			func(tabs []string) bool { return wsActHasTab(tabs, tabName) })
		if !wsActHasTab(tabs, tabName) {
			t.Fatalf("the bootstrap workspace %s (%q) is registered at %s and the bar draws %v, with no tab "+
				"for it. Realtest 4 navigates the BAR, so a workspace without a drawn tab is not a workspace "+
				"this run can reach", workspace.ID, tabName, workspace.Dir, tabs)
		}
		t.Logf("bootstrap: registered %s (%q) at %s and the bar draws its tab",
			workspace.ID, tabName, workspace.Dir)
		bootstrapped = append(bootstrapped, workspace)
	}
	return bootstrapped
}

// rt4BuildKeyDriver compiles realtest 1's key helper and establishes
// accessibility trust.
//
// It FATALS where realtest 1 only errors, and the difference is what the two
// tests are for: realtest 1 observes a startup and proves the driver at the
// end, so a driver that will not build costs it one assertion. Realtest 4 IS
// the driver, so a run that cannot press a key has nothing left to say and
// must not continue collecting a harvest that would read as a clean run.
//
// There is no elisp fallback, for the same reason there is none in realtest 1:
// an elisp call that performs the act tests the function and says nothing
// about whether the chord reaches it, which is the entire subject here. The
// owner rules on the alternative (docs/REALTEST-PLAN.md).
func rt4BuildKeyDriver(ctx context.Context, t *testing.T, client *Client, runDir string, manifest *Manifest) *KeyDriver {
	t.Helper()
	pid, err := client.ReadInt(ctx, `(emacs-pid)`)
	if err != nil {
		t.Fatalf("read the Emacs pid for the key driver: %v", err)
	}
	driver := &KeyDriver{Pid: pid, Scratch: runDir, Client: client, KeepFocus: sweepHoldsFocus()}
	if err := driver.Build(ctx); err != nil {
		note := fmt.Sprintf("KEY DRIVER UNAVAILABLE: %v", err)
		manifest.Notes = append(manifest.Notes, note)
		if _, writeErr := manifest.Write(runDir); writeErr != nil {
			t.Logf("the run manifest could not be written alongside the key-driver failure: %v", writeErr)
		}
		t.Fatalf("%s\nrealtest 4 is entirely real key events, so there is nothing it can assert without the "+
			"driver. The second mechanism, System Events `key code`, is implemented (keys.go) but delivers to "+
			"the FRONTMOST application and was NOT attempted. No elisp fallback was taken", note)
	}
	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("key driver: %s; accessibility trust held", driver.Method))
	return driver
}

// rt4WaitForComposer waits until the selected workspace's input composer is a
// live window, which is what the parked pre-creation queue draining on the
// first focus edge produces.
//
// This one wait is a POLL rather than a log read, and that is not the
// distinction realtest 1 draws against polling. That rule is about
// MEASUREMENT: a poll cannot say when something happened, so no number is ever
// taken from one. Nothing here is measured. The poll only decides when the
// editor is ready for the acts, which is exactly what realtest 1's own
// waitUntil predicates do.
func rt4WaitForComposer(ctx context.Context, t *testing.T, client *Client) {
	t.Helper()
	var state rt4State
	var err error
	waitUntil(ctx, t, "the selected workspace's input composer to exist as a live window", rt4ComposerCeiling,
		func() bool {
			state, err = rt4ReadState(ctx, client)
			return err == nil && state.InputBuffer != ""
		})
	if err != nil {
		t.Fatalf("read the editor's state while waiting for the input composer: %v", err)
	}
	if state.InputBuffer == "" {
		t.Fatalf("waited %s after bringing Emacs forward and workspace %q still has no input composer window "+
			"(`agent-repl-window--panel-window :input` answers nothing). Every act below asserts the cursor "+
			"lands in that composer, so there is nothing left to assert",
			rt4ComposerCeiling, state.Current)
	}
	t.Logf("the input composer for %q is %s", state.Current, state.InputBuffer)
}

// rt4ProveWarmUpLanded checks Emacs's own record that the warm-up key arrived.
//
// A WARM-UP NOBODY CHECKS IS A KEY THAT CAN VANISH IN SILENCE. The wait after
// it is for the input composer, and the composer is downstream of the webview
// pre-creation queue draining off the FOCUS EDGE — which the activation the
// key driver performs produces whether or not the key event itself was ever
// dispatched. So the composer appearing proves the activation, never the key.
//
// That silence mattered. A warm-up eaten by a pending evil operator, or
// dropped outright, leaves the first measured act pressing into a state the
// run has never verified — and the first measured act is the one that came
// back with `last-command` naming `evil-delete`. `recent-keys` is Emacs's own
// account of its INPUT and is the only thing that separates the two.
//
// It is a finding rather than a stop: the acts below make their own
// assertions and are still worth performing, and a run that halted here would
// say nothing about whether switching works at all.
func rt4ProveWarmUpLanded(ctx context.Context, t *testing.T, client *Client, keysBefore string, manifest *Manifest) {
	t.Helper()

	var fresh string
	waitUntil(ctx, t, fmt.Sprintf("emacs's own (recent-keys) to record the warm-up %s", SwitchRight.Emacs),
		rt4SwitchCeiling,
		func() bool {
			keysAfter, err := RecentKeys(ctx, client)
			if err != nil {
				return false
			}
			fresh = rt4FreshKeys(keysBefore, keysAfter)
			return strings.Contains(fresh, SwitchRight.Emacs)
		})

	if !strings.Contains(fresh, SwitchRight.Emacs) {
		note := fmt.Sprintf("THE WARM-UP KEY DID NOT REACH THE KEYMAP: Emacs's own (recent-keys) gained %q "+
			"after the warm-up %s was posted, and it does not contain the chord. The focus edge the warm-up "+
			"exists for may still have happened — activating Emacs produces it on its own — but the key event "+
			"did not, so the acts below start from a keyboard state this run has not verified",
			fresh, SwitchRight.Emacs)
		manifest.Notes = append(manifest.Notes, note)
		t.Errorf("%s", note)
		return
	}
	t.Logf("the warm-up %s reached Emacs's keymap: (recent-keys) gained %q", SwitchRight.Emacs, fresh)
}

// rt4RunAct presses one chord and makes all five assertions for it.
func rt4RunAct(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, sources []Source, snap Snapshot, since time.Time, act rt4Act, manifest *Manifest) {
	t.Helper()

	before, err := rt4ReadState(ctx, client)
	if err != nil {
		t.Fatalf("%s: read the selection before the press: %v", act.Chord.Emacs, err)
	}
	target, err := act.Target(before)
	if err != nil {
		t.Fatalf("%s: work out which tab the drawn order says this chord must land on: %v", act.Chord.Emacs, err)
	}
	keysBefore, err := RecentKeys(ctx, client)
	if err != nil {
		t.Fatalf("%s: read Emacs's recent keys before the press: %v", act.Chord.Emacs, err)
	}
	t.Logf("%s: standing in %q; the drawn order %v says this lands on %q",
		act.Chord.Emacs, before.Current, before.Drawn, target)

	// NOTHING PENDING BEFORE A MEASURED CHORD. An evil operator or a standing
	// prefix consumes the very next key as part of a sequence this run never
	// sent: the chord's own command still runs — `evil-read-motion` accepts
	// whatever the key is bound to and `call-interactively`s it — so the
	// switch looks perfect while `last-command` names the operator. Assertion
	// (3) below is exactly the one that catches it, and it must be measuring
	// this chord rather than somebody else's leftovers.
	wsActClearPendingInput(ctx, t, client, driver,
		fmt.Sprintf("pressing %s", act.Chord.Emacs), manifest)

	if err := driver.Press(ctx, act.Chord); err != nil {
		t.Errorf("press %s (%s): %v", act.Chord.Emacs, act.Chord.Why, err)
		return
	}

	// (1) EMACS'S OWN SELECTION. The wait is on Emacs's account of where it
	// is standing, never on a clock.
	var after rt4State
	waitUntil(ctx, t, fmt.Sprintf("emacs to report %q selected after %s", target, act.Chord.Emacs), rt4SwitchCeiling,
		func() bool {
			read, readErr := rt4ReadState(ctx, client)
			if readErr != nil {
				return false
			}
			after = read
			return after.Current == target
		})
	if after.Current != target {
		t.Errorf("%s: the drawn tab order is %v and the editor was standing in %q, so this chord had to select "+
			"%q; Emacs reports %q. %s",
			act.Chord.Emacs, before.Drawn, before.Current, target, after.Current,
			rt4Diagnose(before, target, after.Current))
		return
	}
	t.Logf("%s: Emacs selected %q", act.Chord.Emacs, after.Current)

	// (2) THE MODULE'S OWN RECORD for the switch, naming the same target.
	rt4AssertRecord(t, sources, snap, since, act, target)

	// (3) THE CHORD RESOLVED TO THE MODULE'S OWN COMMAND. `recent-keys` says
	// the event reached the keymap and `last-command` says which binding won
	// it, and both are needed: a switch with the right selection and the
	// wrong command means some other layer took the chord and happened to do
	// the same thing today.
	keysAfter, keysErr := RecentKeys(ctx, client)
	if keysErr != nil {
		t.Errorf("%s: read Emacs's recent keys after the press: %v", act.Chord.Emacs, keysErr)
	} else if fresh := rt4FreshKeys(keysBefore, keysAfter); !strings.Contains(fresh, act.Chord.Emacs) {
		t.Errorf("%s: Emacs's own (recent-keys) gained %q after the press and it does not contain the chord, "+
			"so the event did not reach its keymap", act.Chord.Emacs, fresh)
	}
	command, cmdErr := LastCommand(ctx, client)
	if cmdErr != nil {
		t.Errorf("%s: read last-command after the press: %v", act.Chord.Emacs, cmdErr)
	} else if command != act.Command {
		note := ""
		for _, wrong := range act.WrongCommands {
			if command == wrong {
				note = fmt.Sprintf(" That is `%s`, the binding this chord resolves to when the module's own "+
					"keymap does not win it, which is the defect lisp/keybindings.el exists to prevent.", wrong)
				break
			}
		}
		t.Errorf("%s: last-command is `%s`, not `%s`.%s", act.Chord.Emacs, command, act.Command, note)
	} else {
		t.Logf("%s: last-command is `%s`", act.Chord.Emacs, command)
	}

	// (4) THE CURSOR IN THE COMPOSER, IN COMMAND STATE.
	rt4AssertComposer(t, sources, snap, since, act, target, after)

	// (5) THE SELECTION INDICATOR ON THE TAB BAR.
	rt4AssertUnderline(t, act, after)

	manifest.Notes = append(manifest.Notes,
		fmt.Sprintf("real key event %s selected %q (slot %d of %d on the drawn bar) through `%s`, "+
			"left the cursor in %s in evil `%s`, and underlined that tab alone",
			act.Chord.Emacs, after.Current, rt4IndexOf(after.Drawn, after.Current)+1, len(after.Drawn),
			act.Command, after.SelectedBuffer, after.EvilState))
}

// rt4Diagnose names the two off-by-one landings by what they mean, so a miss
// says which defect it looks like rather than only that it missed.
func rt4Diagnose(before rt4State, target, landed string) string {
	if landed == "" {
		return "Emacs reports no current workspace at all, which is what a switch to a persp-mode " +
			"pseudo perspective (`none`, or Doom's `main`) looks like from here."
	}
	if rt4IndexOf(before.Drawn, landed) < 0 {
		return fmt.Sprintf("%q is not on the drawn bar at all, so this landed on a perspective the picture "+
			"never offered: `none` and Doom's `main` are the two that own no workspace, and reaching one is "+
			"the signature of navigating persp-mode's list instead of the roster's tab order.", landed)
	}
	want, got := rt4IndexOf(before.Drawn, target), rt4IndexOf(before.Drawn, landed)
	switch got - want {
	case -1:
		return "It landed exactly one slot EARLIER than the drawn bar says, which is the off-by-one " +
			"Doom's `+workspace/switch-to-N` produces by counting persp-mode's list, whose slot 0 is `main`."
	case 1:
		return "It landed exactly one slot LATER than the drawn bar says, which is an off-by-one against " +
			"the picture in the other direction."
	}
	if got == rt4IndexOf(before.Drawn, before.Current) {
		return "It did not move at all, so the chord was swallowed before it reached a switch."
	}
	return "It landed on a slot that is neither the target nor a neighbor of it."
}

// rt4AssertComposer is item 4: the cursor lands in the selected workspace's
// agent-repl input composer, in evil NORMAL (command) state.
//
// Both halves are asserted, and the module's own record for the landing is
// asserted beside them. The record is not redundant with the state: the state
// says the cursor is in the composer NOW, and the record says the switch is
// what put it there. A composer that was already selected before the switch
// would satisfy the first and not the second.
func rt4AssertComposer(t *testing.T, sources []Source, snap Snapshot, since time.Time, act rt4Act, target string, after rt4State) {
	t.Helper()

	if after.SelectedIsInput != "t" {
		t.Errorf("%s: after selecting %q the cursor is in %q, not in that workspace's agent-repl input "+
			"composer (%s). A switch is navigation and lands the user in the composer "+
			"(lisp/panels.el, `agent-repl--maybe-autoselect-input`)",
			act.Chord.Emacs, target, after.SelectedBuffer, rt4OrNone(after.InputBuffer))
	}
	if after.EvilState != "normal" {
		t.Errorf("%s: after selecting %q the composer is in evil `%s` state, not `normal`. A switch arrives "+
			"in command state rather than mid-insert (lisp/panels.el, "+
			"`agent-repl--input-enter-command-state`)",
			act.Chord.Emacs, target, after.EvilState)
	}

	// The landing record carries the workspace as the persp name the switch
	// activated, so it is matched on the target name rather than searched for
	// blindly.
	re := regexp.MustCompile(`^maybe-autoselect-input: ws=(\S+) branch=(\S+)`)
	records, err := rt4ReadEmacsRecords(sources, snap, since)
	if err != nil {
		t.Errorf("%s: read the Emacs log sinks for the composer landing record: %v", act.Chord.Emacs, err)
		return
	}
	var last rt4Record
	var branch string
	for _, rec := range records {
		got := re.FindStringSubmatch(rec.Message)
		if got == nil || got[1] != target {
			continue
		}
		last, branch = rec, got[2]
	}
	switch {
	case branch == "":
		t.Errorf("%s: selecting %q wrote no `maybe-autoselect-input: ws=%s` record inside the run window, so "+
			"nothing says the switch is what moved the cursor into the composer",
			act.Chord.Emacs, target, target)
	case branch != "select":
		t.Errorf("%s: selecting %q took the `%s` branch of `agent-repl--maybe-autoselect-input`, not `select`, "+
			"so the cursor was never moved into the composer. The record is: %s",
			act.Chord.Emacs, target, branch, last.Message)
	default:
		t.Logf("%s: the switch moved the cursor into %q's composer (%s)", act.Chord.Emacs, target, last.Message)
	}
}

// rt4AssertUnderline is item 5: exactly the selected tab carries the selection
// indicator.
//
// It asserts the NEGATIVE half as well as the positive one. A renderer that
// underlined every tab would satisfy "the selected tab is underlined" and
// would tell the user nothing at all, and the marker changed shape recently
// (a background became an underline, lisp/status.el), which is exactly when a
// one-sided assertion stops being worth anything.
func rt4AssertUnderline(t *testing.T, act rt4Act, after rt4State) {
	t.Helper()
	if len(after.Underlined) == 1 && after.Underlined[0] == after.Current {
		t.Logf("%s: the tab bar underlines %q and nothing else", act.Chord.Emacs, after.Current)
		return
	}
	if rt4IndexOf(after.Underlined, after.Current) < 0 {
		t.Errorf("%s: the selected workspace is %q and the tab bar draws no selection underline on its tab "+
			"(underlined: %v of %v). Selection is signalled by the underline alone since it stopped being a "+
			"background (lisp/status.el, `agent-repl--render-tab`), so an un-underlined selected tab leaves "+
			"the user with no marker at all",
			act.Chord.Emacs, after.Current, after.Underlined, after.Drawn)
		return
	}
	t.Errorf("%s: the tab bar underlines %v, but only the selected tab %q may carry the selection marker; "+
		"the bar draws %v",
		act.Chord.Emacs, after.Underlined, after.Current, after.Drawn)
}

// rt4AssertInverse presses `s-}` then `s-{` and asserts the editor is standing
// exactly where it started.
//
// The two chords have each already been asserted against the drawn order on
// their own, so this is not a third reading of the same fact: it is the ONE
// property of the pair, that the second undoes the first from wherever the
// first left off, including across the wrap at either end of the bar.
func rt4AssertInverse(ctx context.Context, t *testing.T, client *Client, driver *KeyDriver, manifest *Manifest) {
	t.Helper()

	origin, err := rt4ReadState(ctx, client)
	if err != nil {
		t.Fatalf("read the selection before the inverse pair: %v", err)
	}
	forward, err := rt4Neighbor(origin, 1)
	if err != nil {
		t.Fatalf("work out the tab right of %q: %v", origin.Current, err)
	}

	if err := driver.Press(ctx, SwitchRight); err != nil {
		t.Errorf("press %s for the inverse pair: %v", SwitchRight.Emacs, err)
		return
	}
	var mid rt4State
	waitUntil(ctx, t, fmt.Sprintf("emacs to report %q selected after %s", forward, SwitchRight.Emacs), rt4SwitchCeiling,
		func() bool {
			read, readErr := rt4ReadState(ctx, client)
			if readErr != nil {
				return false
			}
			mid = read
			return mid.Current == forward
		})
	if mid.Current != forward {
		t.Errorf("the inverse pair could not start: %s from %q selected %q, not %q",
			SwitchRight.Emacs, origin.Current, mid.Current, forward)
		return
	}

	if err := driver.Press(ctx, rt4SwitchLeft); err != nil {
		t.Errorf("press %s for the inverse pair: %v", rt4SwitchLeft.Emacs, err)
		return
	}
	var back rt4State
	waitUntil(ctx, t, fmt.Sprintf("emacs to report %q selected again after %s", origin.Current, rt4SwitchLeft.Emacs), rt4SwitchCeiling,
		func() bool {
			read, readErr := rt4ReadState(ctx, client)
			if readErr != nil {
				return false
			}
			back = read
			return back.Current == origin.Current
		})
	if back.Current != origin.Current {
		t.Errorf("%s then %s is not the identity: the editor started in %q, %s took it to %q, and %s left it "+
			"in %q rather than back where it began. The bar draws %v",
			SwitchRight.Emacs, rt4SwitchLeft.Emacs, origin.Current, SwitchRight.Emacs, mid.Current,
			rt4SwitchLeft.Emacs, back.Current, origin.Drawn)
		return
	}
	note := fmt.Sprintf("%s then %s is the exact inverse: %q to %q and back to %q",
		SwitchRight.Emacs, rt4SwitchLeft.Emacs, origin.Current, mid.Current, back.Current)
	manifest.Notes = append(manifest.Notes, note)
	t.Logf("%s", note)
}

// rt4AssertRecord finds the module's own record for one switch and checks it
// names the same target Emacs landed on.
//
// The LAST matching record is the one read, because a run presses the same
// chord more than once (the warm-up, the acts, the inverse pair) and every
// press writes its own record.
func rt4AssertRecord(t *testing.T, sources []Source, snap Snapshot, since time.Time, act rt4Act, target string) {
	t.Helper()
	records, err := rt4ReadEmacsRecords(sources, snap, since)
	if err != nil {
		t.Errorf("%s: read the Emacs log sinks for the switch record: %v", act.Chord.Emacs, err)
		return
	}
	var found string
	var raw string
	for _, rec := range records {
		if got := act.RecordRe.FindStringSubmatch(rec.Message); got != nil {
			found, raw = got[1], rec.Message
		}
	}
	switch {
	case found == "":
		t.Errorf("%s: no record matching `%s` was written inside the run window, so the switch the editor "+
			"performed left nothing in the log for the owner to read",
			act.Chord.Emacs, act.RecordRe)
	case found != target:
		t.Errorf("%s: Emacs selected %q but its own record names %q as the target: %s. Emacs and its log "+
			"disagree about the same switch",
			act.Chord.Emacs, target, found, raw)
	default:
		t.Logf("%s: the module recorded the switch: %s", act.Chord.Emacs, raw)
	}
}

// rt4Record is one Emacs record inside the run window.
type rt4Record struct {
	At      time.Time
	Message string
	Path    string
}

// rt4ReadEmacsRecords reads every Emacs record written since `since`, across
// the same sinks the phase reader uses.
//
// It reads BOTH the module's global sink and each workspace's own `emacs.log`,
// for the reason ReadPhases spells out: lisp/core.el routes a record logged
// against a workspace to that workspace's canonical sink and never to the
// global log, and the two records this realtest reads land on opposite sides
// of that split. `agent-repl--workspace-cycle` logs against the workspace it
// is leaving, so the cycle record lands in a workspace sink, while a record
// logged against a persp-mode pseudo perspective lands globally. A reader that
// opened only one of the two would report a correctly recorded switch as an
// unrecorded one.
//
// Offsets come from `resolveReads`, so a sink whose target was replaced
// mid-run is still read from where the run left it rather than from past the
// end of a file this run never wrote.
func rt4ReadEmacsRecords(sources []Source, snap Snapshot, since time.Time) ([]rt4Record, error) {
	var out []rt4Record
	for _, src := range sources {
		if !isEmacsPhaseSource(src) {
			continue
		}
		reads, _, err := resolveReads(src, snap)
		if err != nil {
			return nil, err
		}
		for _, r := range reads {
			read, err := rt4ReadOneLog(r.path, r.offset, since)
			if err != nil {
				return nil, err
			}
			out = append(out, read...)
		}
	}
	return out, nil
}

// rt4ReadOneLog reads one JSONL sink from `offset` to end.
//
// A file that does not exist is not an error: a workspace whose sink holds no
// records yet has no file behind its link. A line that does not parse carries
// no record and is skipped HERE and only here: the harvester reports it as a
// malformed record in its own right, and this reader duplicating that would
// report the same defect twice.
func rt4ReadOneLog(path string, offset int64, since time.Time) ([]rt4Record, error) {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("open the Emacs log %s to read the switch records: %w", path, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return nil, fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
		}
	}

	var out []rt4Record
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			continue
		}
		at, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if err != nil || at.Before(since) {
			continue
		}
		out = append(out, rt4Record{At: at, Message: rec.Message, Path: path})
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read the Emacs log %s: %w", path, err)
	}
	return out, nil
}

// rt4FreshKeys returns the part of `after` that `before` did not already hold.
//
// `recent-keys` is a ring of the last 300 input events, so after several
// chords a plain substring search would match a press from earlier in the run
// and report a dropped event as a delivered one. Comparing against the reading
// taken immediately before the press is what makes the answer about THIS
// press.
//
// The driver activates Emacs for the keypress and deactivates it afterwards,
// and those focus transitions are input events too, so the fresh portion
// legitimately carries more than the chord. It is searched, not matched whole.
//
// When `before` is not a prefix of `after` the ring has moved under the
// comparison, and the whole of `after` is returned rather than nothing: a
// weaker answer is better than one that reports every chord as missing.
func rt4FreshKeys(before, after string) string {
	if strings.HasPrefix(after, before) {
		return strings.TrimSpace(after[len(before):])
	}
	return strings.TrimSpace(after)
}

func rt4OrNone(value string) string {
	if value == "" {
		return "there is no input composer window for it"
	}
	return value
}
