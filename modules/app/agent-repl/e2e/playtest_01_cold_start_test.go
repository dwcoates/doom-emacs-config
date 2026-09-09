//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// OWNER 1 of PLAYTEST-PLAN.md's partition: A1-A3 -- cold start, adopting an
// already-answering daemon, and a build failure.
//
// All three are here. Each is the playtest's picture of one of
// EMACS-LAYER-SPEC.md's Area A scenarios (cold start 1, adopt 4, build
// failure 6), made against the same launcher and read back through the same
// variables that area names; what is new is the tab bar and the modeline
// being photographed at each step, and the roster's arm WALK being recorded
// rather than merely its last value.

// playtestBootWalk is the arm walk the daemon publishes for a workspace
// between registration and its first running turn, in the order the
// resolver's own comment states it (daemon/internal/resolve/sidebar/
// status.go, `linkArm`): `none` while nothing is wired, `init` while the
// route is being brought up and is not yet proven, `submitting` once a
// prompt is accepted, `thinking` once the vendor has shown activity.
//
// `:ready` IS DELIBERATELY NOT IN IT. Per sidebar.proto `ready` means
// "live, PROVEN USABLE, and idle", and the resolver names it only for a
// session that is up with NO turn ever run -- a revived one. A cold
// workspace's first prompt is what brings its session up, so the tab walks
// straight from `init` into the running arms and settles on `done`. The
// plan's "init -> booting -> ready" is therefore read against the product's
// own vocabulary: `init` is the booting arm, and the settled arm at the end
// of the first turn is what "ready" means for a session that has run one.
//
// The walk once ran none -> ready -> init -> submitting -> thinking, with
// `ready` published BEFORE the `init` it is supposed to follow, and that is
// the defect the order assertion below pins from the editor's side.
var playtestBootWalk = []string{playtestUnwiredArm, ":init", ":submitting", ":thinking"}

// playtestArmRecorderSetup installs a recorder on the roster's own update
// hook, so a playbook reads the WHOLE arm walk a workspace was published
// through instead of racing to catch a transient arm.
//
// `agent-repl-roster-update-functions` runs with the decoded roster after
// every accepted push, after reconciliation, so what is recorded is what
// the tab bar was painted from at that instant. `:init` is measured at
// about 340ms on this daemon (status.go), which no wait could photograph
// reliably; the recording is what turns it from a race into a fact.
const playtestArmRecorderSetup = `(progn
             (defvar agent-repl-playtest--arm-walk nil
               "Newest-first list of (WS . ARM) pairs, one per accepted roster push.")
             (setq agent-repl-playtest--arm-walk nil)
             (defun agent-repl-playtest--record-arms (_roster)
               (dolist (ws (agent-repl--ws-list-names))
                 (push (cons ws (format "%s" (agent-repl-roster-status-for-ws ws)))
                       agent-repl-playtest--arm-walk)))
             (add-hook 'agent-repl-roster-update-functions #'agent-repl-playtest--record-arms)
             t)`

// recordedArmWalk answers the arms WS was published through, oldest first,
// with consecutive repeats collapsed: a push that re-publishes the same arm
// is not a step of the walk.
func (s *playtestScenario) recordedArmWalk(t *testing.T, ws string) []string {
	t.Helper()
	raw := s.E.EvalStrings(`(let (out)
                              (dolist (entry agent-repl-playtest--arm-walk)
                                (when (equal (car entry) ` + elispString(ws) + `)
                                  (push (cdr entry) out)))
                              out)`)
	var walk []string
	for _, arm := range raw {
		if len(walk) == 0 || walk[len(walk)-1] != arm {
			walk = append(walk, arm)
		}
	}
	return walk
}

// assertBootWalkOrder asserts that WALK visits the arms of `playtestBootWalk`
// in that order and no others: every recorded arm must be one of the boot
// walk's, and each must come no earlier than the one before it. `:init`
// must be in it: a walk that skipped straight from `none` to a running arm
// never told the user the route was being brought up.
func assertBootWalkOrder(t *testing.T, ws string, walk []string) {
	t.Helper()
	rank := map[string]int{}
	for i, arm := range playtestBootWalk {
		rank[arm] = i
	}
	last := -1
	sawInit := false
	for _, arm := range walk {
		r, known := rank[arm]
		if !known {
			t.Fatalf("the roster published %s for %s during its bring-up; the boot walk is %v, and the whole walk was %v",
				arm, ws, playtestBootWalk, walk)
		}
		if r < last {
			t.Fatalf("the roster published %s for %s AFTER %s; the boot walk is %v, and the whole walk was %v",
				arm, ws, playtestBootWalk[last], playtestBootWalk, walk)
		}
		last = r
		if arm == ":init" {
			sawInit = true
		}
	}
	if !sawInit {
		t.Fatalf("the roster never published :init for %s: the tab went %v without ever saying the route was coming up",
			ws, walk)
	}
}

// assertNoDaemon asserts the world's cold precondition: no address is
// published under the state root, the launcher holds no process, and Emacs
// holds no link. These are the Area A readbacks, and a world that fails one
// of them is not cold and cannot be the subject of a cold start.
func (s *playtestScenario) assertNoDaemon(t *testing.T) {
	t.Helper()
	if _, err := os.Stat(filepath.Join(s.E.StateDir, "daemon.addr")); err == nil {
		t.Fatal("daemon.addr already exists under the state root: this world is not cold")
	} else if !os.IsNotExist(err) {
		t.Fatalf("stat daemon.addr under the state root: %v", err)
	}
	if pid := daemonPID(s.E); pid != 0 {
		t.Fatalf("the launcher already holds a live daemon process (pid %d): this world is not cold", pid)
	}
	if s.E.EvalBool(`(and (agent-repl-link-up-p) t)`) {
		t.Fatal("Emacs already holds a daemon link: this world is not cold")
	}
}

// TestPlaytestColdStartAndFirstTab is plan A.1: an editor with nothing in
// it and no daemon anywhere, the daemon Emacs spawns itself, the first
// workspace's tab, the empty hold tray, and the arm walk that tab is
// painted through when its session first comes up.
func TestPlaytestColdStartAndFirstTab(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "a1-turn-gate")
	s := newColdPlaytestScenario(t, "01-cold-start",
		"Plan A.1. A cold Emacs with no daemon builds and spawns one through its own launcher, "+
			"registers a repository, opens its panel on an empty tray, and its first tab is painted "+
			"through the daemon's own bring-up walk when its session comes up.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, playtestGatedPrompt))
	p, e := s.Book, s.E

	// THE WORLD IS COLD, and that is asserted rather than assumed: a
	// daemon.addr left behind by anything would turn this into the adopt
	// playbook without anyone noticing.
	s.assertNoDaemon(t)
	if got := s.tabNames(); len(got) != 0 {
		t.Fatalf("the tab bar draws %v before anything is registered, want nothing", got)
	}
	p.note("nothing done yet: Emacs is up on its own display with no daemon anywhere",
		"no `daemon.addr` under the state root, `agent-repl--frontend-daemon-process` is nil, "+
			"`agent-repl-link-up-p` is nil, and `agent-repl--ws-tabline-names` is empty")

	// THE LAUNCH. The build step runs through the world's own build script
	// (a no-op that succeeds: the binaries are built by the Go side, and a
	// test never shells out to bin/build-frontend.sh), so what is proven is
	// the launcher's sequence -- build, spawn, wait for the address, link --
	// and not the compiler.
	s.ensureDaemon(t)
	if pid := daemonPID(e); pid == 0 {
		t.Fatal("the launcher reports no live daemon process after EnsureDaemon")
	}
	if addr := e.DaemonAddr(); addr == "" {
		t.Fatal("the daemon published an empty address into daemon.addr under the state root")
	}
	if e.EvalBool(`(and agent-repl-daemon-build-failure t)`) {
		t.Fatalf("the launcher recorded a build failure on the cold start: %s",
			e.EvalString(`(format "%s" agent-repl-daemon-build-failure)`))
	}
	if got := s.tabNames(); len(got) != 0 {
		t.Fatalf("the tab bar draws %v with nothing registered, want nothing", got)
	}
	p.capture("cold-editor", "the daemon spawned by Emacs's own launcher, nothing registered yet",
		"`agent-repl--frontend-daemon-process` is live, the daemon published `daemon.addr` under the "+
			"state root, no build failure is recorded, and `agent-repl--ws-tabline-names` is empty",
		"An Emacs frame filling the whole screen, booted through the image's real Doom. "+
			"The tab bar carries NO workspace tab: nothing is registered yet. The mode line carries "+
			"no `daemon: build failed` and no `daemon: launch failed` segment.")

	// THE RECORDER GOES IN BEFORE THE FIRST PUSH THAT COULD CARRY THE
	// WORKSPACE, so the walk it reads starts at the workspace's first arm.
	e.Eval(playtestArmRecorderSetup)

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	if got := s.tabNames(); len(got) != 1 || got[0] != name {
		t.Fatalf("the tab bar draws %v, want exactly [%s]", got, name)
	}
	// A WORKSPACE NOTHING HAS BEEN WIRED TO IS `:none`, AND THAT IS THE
	// POINT OF THIS PICTURE. Registering mints an identity; it does not bring
	// a session up, which the first submit does. Per the module's color rule
	// `:none` is TEAL -- "nothing is wired, and nothing is wrong" -- and it
	// is emphatically not the blue of a broken link.
	s.awaitArm(t, name, "the new workspace's tab arm to be published", playtestUnwiredArm)
	s.captureArm(t, "first-tab", name,
		"`agent-repl-add-project-workspace` (`SPC TAB C-n`) on a scripted fake-git worktree",
		playtestUnwiredArm,
		fmt.Sprintf("The tab bar must carry EXACTLY ONE workspace tab and it must be named %q: "+
			"`agent-repl--ws-tabline-names` says the module knows about exactly that one.", name))

	// THE TRAY IS EMPTY. The readback is the queue, never the tray's own
	// text (EMACS-LAYER-SPEC.md's readback table); the tray having DRAWN is
	// what makes the picture one of an empty tray rather than of no tray.
	s.openPanel(t)
	emGHIAwaitHeldPrompts(t, e, name, "the workspace to hold no prompt", 0)
	s.awaitInPage(t, "the hold tray to draw",
		`document.querySelector('[data-component="hold-tray"]').textContent.trim() !== ""`)
	s.awaitArm(t, name, "the tab's arm with the panel open and nothing submitted", playtestUnwiredArm)
	s.captureArm(t, "panel-empty-tray", name,
		"`agent-repl-frontend-open-panel` on the first workspace, nothing submitted",
		playtestUnwiredArm,
		"The panel is open in the main area with the webapp drawn in it: the feed is EMPTY, the hold "+
			"tray is drawn and holds NO prompt, and the footer says the session is idle.")

	// THE BRING-UP WALK. The first prompt is what brings the session up,
	// and the fake's gate holds the turn open at `:thinking` so the walk can
	// be read whole once the tab is there. Nothing here photographs `:init`:
	// it is transient by design, and the recorder is what proves it was
	// published, in order, between `:none` and the running arms.
	s.submit(t, playtestGatedPrompt)
	s.awaitArm(t, name, "the tab's arm to reach thinking once the turn is in flight", ":thinking")
	walk := s.recordedArmWalk(t, name)
	assertBootWalkOrder(t, name, walk)
	p.note("the first prompt submitted with composer RET, held in flight by the fake's turn gate",
		fmt.Sprintf("the roster's recorded arm walk for the tab is %v: it visits `:init` between `:none` and the "+
			"running arms, in the daemon's own order, and publishes no settled arm before it", walk))
	s.captureArm(t, "first-turn-in-flight", name,
		"the session brought up by its first prompt, the turn still gated open",
		":thinking",
		"The feed carries the user's own prompt bubble and the footer says a turn is RUNNING.")

	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}
	settled := s.awaitArm(t, name, "the tab's arm to settle when the first turn concludes", emGHISettledArms...)
	emGHIAwaitHeldPrompts(t, e, name, "the tray to still hold no prompt after the turn", 0)
	s.captureArm(t, "first-turn-settled", name,
		"the gate opened, and the fake's prose answer concluded the first turn",
		settled,
		"The session is up and idle: the feed carries the prompt bubble and the answer beneath it, the "+
			"footer is idle again, and the hold tray still holds nothing.")
}

// elispLogRecords counts the records in the module's own log that carry
// MARKER. The log is under this world's own state root (`e.LogFile`), and
// the layer runs inside the container, so it is read directly.
func elispLogRecords(t *testing.T, e *Emacs, marker string) int {
	t.Helper()
	body, err := os.ReadFile(e.LogFile)
	if err != nil {
		t.Fatalf("read the module's own log at %s: %v", e.LogFile, err)
	}
	return strings.Count(string(body), marker)
}

// TestPlaytestAdoptsAnAnsweringDaemon is plan A.2: a daemon that is already
// up and answering when the ensure runs is ADOPTED -- the same process, the
// same address, no build -- and the tab that was on it comes back straight
// at its settled arm.
//
// The arrangement is Area A scenario 4's: the daemon Emacs's own launcher
// spawned is left running while Emacs forgets its link
// (`agent-repl-link-teardown`, which closes the client side and never
// touches the daemon), so the next ensure finds `daemon.addr` present and a
// daemon answering `DaemonHealth` behind it. That is exactly the state a
// second editor -- or this one after a restart of Emacs -- starts in.
func TestPlaytestAdoptsAnAnsweringDaemon(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "01-adopt-daemon",
		"Plan A.2. A daemon already up and answering is adopted by the ensure: the same process, "+
			"the same address, no build, and the workspace's tab comes back straight at its settled arm.")
	p, e := s.Book, s.E

	first := daemonPID(e)
	if first == 0 {
		t.Fatal("the first ensure spawned no daemon")
	}
	addr := e.DaemonAddr()

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	s.submit(t, "draw one plain prose answer for the adopt playbook")
	before := s.awaitArm(t, name, "the first turn to settle before the link is dropped", emGHISettledArms...)
	s.captureArm(t, "before-adopt", name,
		"one repository registered, its panel open, one plain-prose turn settled",
		before,
		"The feed carries the prompt bubble and the answer beneath it; this is the state the "+
			"adoption must hand back unchanged.")

	// THE LINK IS FORGOTTEN, THE DAEMON IS NOT. Teardown is the client's own
	// graceful close, and the daemon it was talking to keeps running: that
	// is what makes the next ensure an adoption rather than a launch.
	e.Eval(playtestArmRecorderSetup)
	builds := elispLogRecords(t, e, "elisp.daemon.build ")
	adoptions := elispLogRecords(t, e, "elisp.daemon.adopted")
	e.Eval(`(agent-repl-link-teardown)`)
	e.AwaitEvalFor(daemonLinkBound, "the link to be down after the teardown",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	if pid := daemonPID(e); pid != first {
		t.Fatalf("the daemon process changed across a link teardown: pid %d -> %d; teardown must not touch the daemon", first, pid)
	}
	if got := e.DaemonAddr(); got != addr {
		t.Fatalf("daemon.addr changed across a link teardown: %q -> %q", addr, got)
	}
	p.note("`agent-repl-link-teardown`: Emacs forgets its link and leaves the daemon running",
		fmt.Sprintf("`agent-repl-link-up-p` is nil, the daemon process (pid %d) is still live, and `daemon.addr` still names %s", first, addr))

	// THE ADOPTION, through the ordinary command.
	e.Eval(`(agent-repl-frontend-daemon-ensure)`)
	emHOAwaitLinkUp(t, e, "the link to come back on the adopted daemon")
	if pid := daemonPID(e); pid != first {
		t.Fatalf("the adopting ensure changed the daemon process: pid %d -> %d; a daemon that answers is adopted, never replaced", first, pid)
	}
	if got := e.DaemonAddr(); got != addr {
		t.Fatalf("the published address changed across an adopting ensure: %q -> %q", addr, got)
	}
	// NO BUILD RAN. The build state is read as data (nothing in flight,
	// nothing recorded as started, no failure), and the module's own log is
	// what says the ensure took the adopt branch: one more `adopted` record
	// and not one more `build` record.
	if e.EvalBool(`(and (or agent-repl-daemon--build-in-flight agent-repl-daemon--build-state agent-repl-daemon-build-failure) t)`) {
		t.Fatalf("the adopting ensure touched the build: in-flight=%s state=%s failure=%s",
			e.EvalString(`(format "%s" agent-repl-daemon--build-in-flight)`),
			e.EvalString(`(format "%s" agent-repl-daemon--build-state)`),
			e.EvalString(`(format "%s" agent-repl-daemon-build-failure)`))
	}
	if got := elispLogRecords(t, e, "elisp.daemon.adopted"); got != adoptions+1 {
		t.Fatalf("the log carries %d `elisp.daemon.adopted` records after the ensure, want %d: the ensure did not take the adopt branch", got, adoptions+1)
	}
	if got := elispLogRecords(t, e, "elisp.daemon.build "); got != builds {
		t.Fatalf("the log carries %d `elisp.daemon.build` records after the ensure, want %d: the adopting ensure ran a build", got, builds)
	}
	p.note("`agent-repl-frontend-daemon-ensure` with a daemon already answering at the published address",
		fmt.Sprintf("the link is up on the SAME daemon (pid %d, address %s), the build state is untouched, and the log "+
			"carries one more `elisp.daemon.adopted` record and no new `elisp.daemon.build` record", first, addr))

	// STRAIGHT TO ITS SETTLED ARM. The roster re-subscribes on the link-up
	// edge, and the first push it applies for this tab is already the
	// settled arm the turn left it on -- no `:init`, because nothing is
	// being brought up.
	after := s.awaitArm(t, name, "the tab's arm to be published again on the adopted link", emGHISettledArms...)
	walk := s.recordedArmWalk(t, name)
	if len(walk) != 1 || walk[0] != after {
		t.Fatalf("the tab's arm walk since the teardown is %v, want exactly [%s]: an adoption re-publishes the settled arm and nothing before it", walk, after)
	}
	if got := s.tabNames(); len(got) != 1 || got[0] != name {
		t.Fatalf("the tab bar draws %v after the adoption, want exactly [%s]", got, name)
	}
	s.captureArm(t, "adopted", name,
		"the roster re-subscribed on the adopted link",
		after,
		"The tab bar carries the one tab it had before, and the panel still shows the prompt bubble "+
			"and the answer: nothing was rebuilt and nothing was lost.")
}

// TestPlaytestBuildFailureSurfaces is plan A.3: a build that fails on the
// cold start surfaces in the modeline and in the build buffer, no daemon is
// spawned, and NOTHING WEDGES -- the ensure's guards are released, the
// command loop answers, and the interactive ensure is the retry that brings
// the stack up once the build is mended.
//
// THE MODELINE SEGMENT IS READ AS A STRING, DELIBERATELY: its composition is
// the subject, and Area A scenario 6 sanctions it for that reason.
//
// THE TAB BAR HAS NO ARM FOR THIS. A build failure happens before any
// daemon exists, so there is no roster, no row and no tab to paint: the
// product's whole surface for it is the modeline segment, the echo and the
// build buffer, and that is what the picture is of. The plan's "tab paints
// failed" has no product arm behind it on a cold start, and the recovered
// first tab at the end is what the bar paints once a daemon exists.
func TestPlaytestBuildFailureSurfaces(t *testing.T) {
	t.Parallel()
	s := newColdPlaytestScenario(t, "01-build-failure",
		"Plan A.3. The cold start's build fails: the modeline and the build buffer say so, no daemon "+
			"is spawned, nothing wedges, and the interactive ensure is the retry that brings the stack up.")
	p, e, box := s.Book, s.E, s.Box

	s.assertNoDaemon(t)
	noop := e.EvalString(`agent-repl-daemon-build-script`)
	failing := filepath.Join(box.Scratch(), "build-fails.sh")
	if err := os.WriteFile(failing, []byte("#!/usr/bin/env bash\necho 'frontend build blew up' >&2\nexit 3\n"), 0o755); err != nil {
		t.Fatalf("write the failing build script: %v", err)
	}
	// Ordinary configuration, set the way a user's own `setq' sets it.
	e.Eval(`(setq agent-repl-daemon-build-script ` + elispString(failing) + `)`)
	p.note("the build script pointed at one that prints `frontend build blew up` and exits 3; no daemon anywhere",
		"no `daemon.addr` under the state root, no daemon process, no link, and `agent-repl-daemon-build-script` names the failing script")

	e.Eval(`(agent-repl-frontend-daemon-ensure)`)
	e.AwaitEvalFor(daemonLaunchBound, "the launcher to record a build failure",
		`agent-repl-daemon-build-failure`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	segment := e.EvalString(`(format "%s" (or agent-repl-daemon-mode-line-segment ""))`)
	if segment == "" {
		t.Fatal("the daemon modeline segment is empty after a failed build")
	}
	if !strings.Contains(strings.ToLower(segment), "build") {
		t.Fatalf("the modeline segment %q does not name the build failure", segment)
	}
	if pid := daemonPID(e); pid != 0 {
		t.Fatalf("the launcher spawned a daemon (pid %d) after a failed build", pid)
	}
	if _, err := os.Stat(filepath.Join(e.StateDir, "daemon.addr")); err == nil {
		t.Fatal("daemon.addr exists under the state root after a failed build: something published an address")
	}
	if e.EvalBool(`(and (agent-repl-link-up-p) t)`) {
		t.Fatal("Emacs holds a daemon link after a failed build")
	}
	// NOTHING WEDGES. The two guards that refuse a second ensure and a
	// second build are released, the build buffer is SHOWN (the failure's
	// own surface, per `agent-repl-daemon--report-build-failure`), and the
	// command loop answers.
	if e.EvalBool(`(and (or agent-repl-daemon--ensure-in-flight agent-repl-daemon--build-in-flight) t)`) {
		t.Fatalf("a guard is still raised after the failed build: ensure-in-flight=%s build-in-flight=%s; the next ensure would be refused",
			e.EvalString(`(format "%s" agent-repl-daemon--ensure-in-flight)`),
			e.EvalString(`(format "%s" agent-repl-daemon--build-in-flight)`))
	}
	if !e.EvalBool(`(and (get-buffer-window agent-repl-daemon-build-buffer) t)`) {
		t.Fatalf("the build buffer %s is not shown in any window after the failed build",
			e.EvalString(`agent-repl-daemon-build-buffer`))
	}
	output := e.EvalString(`(with-current-buffer agent-repl-daemon-build-buffer (buffer-string))`)
	if !strings.Contains(output, "frontend build blew up") {
		t.Fatalf("the build buffer does not carry the script's own stderr; it holds %q", output)
	}
	emGHIAssertResponsive(t, e, "the failed build")
	if got := s.tabNames(); len(got) != 0 {
		t.Fatalf("the tab bar draws %v with no daemon, want nothing", got)
	}
	p.capture("build-failed", "`agent-repl-frontend-daemon-ensure` with a build script that exits 3",
		fmt.Sprintf("`agent-repl-daemon-build-failure` is set, the modeline segment reads %q, no daemon process and no "+
			"`daemon.addr` exist, no link stands, both in-flight guards are nil, the build buffer is shown and "+
			"carries the script's stderr, and the command loop answers", segment),
		fmt.Sprintf("The mode line carries the segment %q. A window shows the `*agent-repl-build-frontend*` buffer "+
			"with the line `frontend build blew up` in it. The tab bar carries NO workspace tab: there is no "+
			"daemon and therefore no roster.", segment))

	// THE RETRY. Per daemon.el there is no automatic retry; the interactive
	// ensure is it. With the build mended it goes the whole way: build,
	// spawn, address, link, and the failure segment comes down.
	e.Eval(`(setq agent-repl-daemon-build-script ` + elispString(noop) + `)`)
	s.ensureDaemon(t)
	if pid := daemonPID(e); pid == 0 {
		t.Fatal("the retrying ensure spawned no daemon")
	}
	if e.EvalBool(`(and agent-repl-daemon-build-failure t)`) {
		t.Fatalf("the build failure is still recorded after a successful build: %s",
			e.EvalString(`(format "%s" agent-repl-daemon-build-failure)`))
	}
	if got := e.EvalString(`(format "%s" (or agent-repl-daemon-mode-line-segment ""))`); strings.Contains(strings.ToLower(got), "fail") {
		t.Fatalf("the modeline segment still names a failure after the retry succeeded: %q", got)
	}
	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.awaitArm(t, name, "the first workspace's tab arm after the recovered start", playtestUnwiredArm)
	s.captureArm(t, "recovered-first-tab", name,
		"the build script mended, `agent-repl-frontend-daemon-ensure` again, and one repository registered",
		playtestUnwiredArm,
		fmt.Sprintf("The mode line carries NO `daemon: build failed` segment any more, and the tab bar carries "+
			"exactly one tab named %q.", name))
}
