//go:build playtest

package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

// OWNER 6 of PLAYTEST-PLAN.md's partition: B17-B18 -- the detached-work
// indicator while a backgrounded shell outlives its turn, and the link
// severed / degraded / recovered walk when the shim is killed out from under
// the daemon.
//
// WHAT EACH PLAYBOOK IS.
//
// B.17 (`TestPlaytestTabArmDetachedWork`) photographs the one arm that says
// "nothing is running for you, and yet work is happening". The fake's
// `bash-detach` scenario CONCLUDES ITS TURN and only then appends its spool,
// so the foreground is free while the detached shell still runs -- and the
// daemon's own precedence (daemon/internal/resolve/sidebar/status.go) puts
// `idle_async` ABOVE `done` and `ready` for exactly that reason: work
// happening NOW outranks how the last turn ended. So the picture's subject is
// YELLOW where a reviewer might expect green.
//
// WHY THE GATE EXISTS FOR B.17. The detached shell settles on the fake's own
// schedule, a tick or two after the turn's terminal, and a playbook that
// merely waited for `:idle-async` and then took a picture would be racing
// that settlement. A picture taken on the wrong side of it shows a GREEN tab
// and reads as a product that never paints the detached indicator at all --
// which is worse than no picture. So the fake takes a DETACHED-WORK GATE
// (`detachGatePathEnv`): the scenario concludes its turn, writes the first
// spool line, and then PARKS until the gate path exists, so the running half
// of this playbook is synchronized on the work rather than on a hope. The
// gate is opened by this playbook, and only then does the shell finish with
// `EXIT=0`, end the task and announce an empty live set -- which is the
// second picture.
//
// B.18 (`TestPlaytestTabArmLinkSeveredAndRecovered`) cuts the route to a
// PROVEN-GOOD session and photographs the three states the owner named.
//
// WHY THE KILL IS A SIGKILL OF THE DAEMON'S OWN CHILD. The subject is a
// route that broke while the daemon still wants it -- not a session the
// daemon retired, and not a daemon that went away. Nothing in the product
// asks for that, so it is provoked the only honest way: the shim process the
// daemon spawned is found in the process table through the Emacs's own
// reaper walk (`findStrays`, which reads /proc -- the test binary runs INSIDE
// the sandbox container, which is why /proc is this world's own) and sent
// SIGKILL. A SIGTERM would let the shim shut its stream down politely, which
// is a CLEAN close and a different arm; SIGKILL is the abrupt loss the blue
// family exists to report.
//
// WHY BOTH `severed` AND `dead` ARE ACCEPTED FOR THE FIRST PUSH. Those two
// arms are the same event seen at two instants. `linkArm` publishes `severed`
// while `shimclient` is REDIALING a process it believes is still there, and
// `dead` once that child has been reaped and the redial has given up. After a
// SIGKILL of the daemon's own child both orderings are real -- the kernel
// reaps a child on its own schedule relative to the stream's end -- so
// demanding one of them would be asserting a race rather than the product.
// The playbook therefore accepts either for the FIRST push and then waits for
// `:dead` by name, which is the state the route settles in. Both are BLUE in
// the module's own color table, so the picture is the same picture either way.
//
// WHY THE RECOVERY IS READ AS AN OUTCOME AND NOT AS A MOMENT. The revived
// turn's running half is a ~107ms transient behind a ~350ms shim spawn, and a
// poll that samples through an emacsclient round trip cannot be promised to
// land inside it -- so the recovery is asserted from the RECORDED arm walk,
// the feed's own concluded-turn count and the shim's pid, none of which can
// be missed by being looked at late. The reason is written out at the site.

// detachGatePathEnv names the fake SDK's detached-work gate.
//
// It is `agent-shim/claude/shim/src/fake/index.ts`'s DETACH_GATE_PATH_ENV,
// documented and unit-tested there: with it set, the `bash-detach` scenario
// concludes its turn, writes the FIRST spool line, and then parks until the
// named path exists on disk -- only then finishing the spool with `EXIT=0`,
// ending the task and announcing the empty live set.
const detachGatePathEnv = "AGENT_REPL_FAKE_DETACH_GATE"

// playtestDetachedArm is the arm a workspace carries while detached work runs
// under a free foreground, and playtestSeveredArm / playtestDeadArm are the
// two arms a cut route walks through.
//
// They are named rather than written at the call sites for the reason
// `playtestUnwiredArm` is: `emGHISettledArms` carries `:idle-async` among
// three others, so awaiting that set would be satisfied by `:done` and
// photograph the very state this playbook exists to distinguish it from.
const (
	playtestDetachedArm = ":idle-async"
	playtestSeveredArm  = ":severed"
	playtestDeadArm     = ":dead"
	playtestDoneArm     = ":done"
)

// playtestDetachedShellRow selects the feed row the detached shell is drawn
// in.
//
// `data-row-kind` is set from the FeedRow oneof's own arm name
// (`webapp/src/feed/feed-view.ts`), and `detachedShell` is that arm. The
// bubble inside it carries the shell's state on `data-state`
// (`webapp/src/feed/cards/shell.ts`): `live` while it runs, and the settled
// OUTCOME's own name -- `completed`, `cancelled` or `lost` -- once it is
// over, with a `data-exit-code` chip beside it. So both halves of this
// playbook read a `data-*` hook the webapp suite already asserts rather than
// any rendered word.
const playtestDetachedShellRow = `[data-feed-row][data-row-kind="detachedShell"]`

// TestPlaytestTabArmDetachedWork is plan B.17: the detached-work indicator
// while a `!bash-detach` shell runs, and its clearing on settle.
func TestPlaytestTabArmDetachedWork(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "b17-detach-gate")
	s := newPlaytestScenario(t, "06-tab-arms-link-detached-work",
		"Plan B.17. A backgrounded shell that outlives its own turn, the YELLOW detached-work "+
			"indicator the tab paints while the foreground is free, and the indicator clearing when "+
			"the shell settles with exit 0.",
		WithEmacsEnv(detachGatePathEnv, gatePath))
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	s.awaitArm(t, name, "the tab's arm before anything is submitted", playtestUnwiredArm)
	p.note("one repository registered with its panel open, nothing submitted",
		"the workspace's arm is the unwired "+playtestUnwiredArm+", so no session has ever existed behind it")

	s.submit(t, "!bash-detach")

	// `:idle-async` BY NAME, and not `emGHISettledArms`. The turn CONCLUDED
	// while the detached task is still live, so `:done` is a real arm for
	// this workspace a moment later -- and the daemon's precedence putting
	// `idle_async` above `done` is the exact claim this step makes. A wait
	// satisfied by any settled arm would pass on the picture that disproves
	// it.
	s.awaitArm(t, name, "the tab's arm to report detached work while the foreground is free",
		playtestDetachedArm)
	s.awaitInPage(t, "the detached shell's own row to be drawn, still running",
		`document.querySelector('`+playtestDetachedShellRow+` .shell-bubble[data-state="live"]')`)
	s.captureArm(t, "arm-detached-running", name,
		"`!bash-detach` submitted, its turn concluded, and its shell parked on the fake's detach gate",
		playtestDetachedArm,
		"YELLOW is the whole subject. There is NO foreground turn -- the one that was submitted has "+
			"already concluded -- and yet work is live: the feed carries the detached shell's own row "+
			"with its command and a RUNNING dot. So the tab must NOT paint the RED of a running turn, "+
			"because none is running, and must NOT paint the GREEN of a settled workspace, because "+
			"the work is not settled.")

	// The gate opens only now, so the shell finishes on this playbook's own
	// schedule rather than whenever the fake got there.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's detached-work gate at %s: %v", gatePath, err)
	}

	// `:done` BY NAME. A turn ran and its last close was not a kill, so
	// `sessionArm` answers `done` once `asyncLive` goes false -- and
	// accepting `:idle-async` here would accept the indicator NEVER clearing,
	// which is the defect this step is looking for.
	s.awaitArm(t, name, "the detached-work indicator to clear once the shell settles", playtestDoneArm)
	// `completed` is the shell card's own settled-outcome hook, and the exit
	// chip beside it carries the code -- so the assertion is that the row
	// says settled AND says zero, both off `data-*` attributes rather than
	// off any rendered word.
	s.awaitInPage(t, "the detached shell's row to read settled with exit 0",
		`document.querySelector('`+playtestDetachedShellRow+` .shell-bubble[data-state="completed"] [data-exit-code="0"]')`)
	s.captureArm(t, "arm-detached-settled", name,
		"the detach gate opened, so the shell finished its spool with EXIT=0 and the live set emptied",
		playtestDoneArm,
		"The indicator has CLEARED: the tab is GREEN, and the yellow of the previous picture is gone. "+
			"In the feed the same detached shell row now reads settled -- a done dot, an elapsed time, "+
			"and an `exit 0` chip -- so nothing is running anywhere in this workspace.")
}

// playtestShimPID finds THIS world's one shim process, failing loudly rather
// than guessing.
//
// It walks the Emacs's own reaper enumeration (`findStrays`, which matches
// every live process whose argv or environment names one of this world's
// paths), so the search is already scoped to this world and can never reach
// a peer playbook's shim on a shared container.
//
// The narrowing is two filters, and the second is not redundant:
//
//   - the shim is the node process running the shim bundle, so its argv names
//     `main.js`;
//   - THE DAEMON'S ARGV NAMES IT TOO. It is spawned with
//     `--shim-main <.../main.js>`, so a match on `main.js` alone finds two
//     processes and killing the wrong one would tear down the daemon this
//     playbook is testing. The daemon is excluded by its own binary name.
//
// Zero matches and more than one are both fatal: zero means the kill would
// have been a no-op and every arm below would then be asserting nothing,
// and more than one means this narrowing no longer identifies the process it
// claims to.
func playtestShimPID(t *testing.T, e *Emacs) int {
	t.Helper()
	var found []stray
	for _, candidate := range e.findStrays() {
		if !strings.Contains(candidate.argv, "main.js") {
			continue
		}
		if strings.Contains(candidate.argv, "claude-repld") {
			continue
		}
		found = append(found, candidate)
	}
	if len(found) != 1 {
		all := e.findStrays()
		lines := make([]string, 0, len(all))
		for _, candidate := range all {
			lines = append(lines, candidate.argv)
		}
		t.Fatalf("looking for this world's one shim process found %d, want exactly 1; the whole process set was:\n%s",
			len(found), strings.Join(lines, "\n"))
	}
	return found[0].pid
}

// assertRevivedArmWalk asserts that AFTER the route died the roster published
// a running arm and then `:done` for WS.
//
// It reads the walk rather than a moment, and the two halves are the two ways
// the recovery can be wrong. A walk whose tail never carries a running arm is
// a turn the tab never said was running -- the user watched a dead route sit
// there and an answer appear out of nothing. A tail with a running arm and no
// `:done` after it is a turn that never settled. `:done` ALONE is the one
// reading a moment could not rule out: it is also what the FIRST turn left
// behind, so a submit that never ran would have shown exactly that.
//
// Everything between is deliberately unconstrained. `:init` sits in the tail
// while the fresh shim spawns, and which running arm is published depends on
// how much of the turn the daemon saw before the fake answered.
func assertRevivedArmWalk(t *testing.T, ws string, walk []string) {
	t.Helper()
	died := -1
	for i, arm := range walk {
		if arm == playtestDeadArm {
			died = i
		}
	}
	if died < 0 {
		t.Fatalf("the roster never published %s for %s, so there is no recovery to read; the whole walk was %v",
			playtestDeadArm, ws, walk)
	}
	tail := walk[died+1:]
	running := -1
	for i, arm := range tail {
		if containsString(emGHIRunningArms, arm) {
			running = i
			break
		}
	}
	if running < 0 {
		t.Fatalf("after the route died the roster never published a running arm for %s: the tab went %v with no turn ever shown as in flight, and the whole walk was %v",
			ws, tail, walk)
	}
	if !containsString(tail[running+1:], playtestDoneArm) {
		t.Fatalf("after the route died %s was published as running and never settled on %s: the tail was %v, and the whole walk was %v",
			ws, playtestDoneArm, tail, walk)
	}
}

// TestPlaytestTabArmLinkSeveredAndRecovered is plan B.18: the shim killed out
// from under the daemon, the BLUE paint of a compromised route, and the
// recovery when the next prompt brings a fresh shim up.
func TestPlaytestTabArmLinkSeveredAndRecovered(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "06-tab-arms-link-severed-recovered",
		"Plan B.18. A proven-good session whose shim is SIGKILLed under the daemon: the BLUE arms of "+
			"a compromised route, the webapp's own disconnected footer, and the GREEN recovery when "+
			"the next prompt brings a fresh shim up.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)

	// THE ARM WALK IS RECORDED, because the recovery below is a walk and not
	// a state. See `assertRevivedArmWalk`: the running half of a revived turn
	// is a ~100ms transient this playbook once tried to photograph with a
	// poll, and the recorder is what turns it from a race into a fact.
	e.Eval(playtestArmRecorderSetup)

	// A REAL SHIM IS PROVEN UP BEFORE ANYTHING IS CUT. The subject is a route
	// that WAS serving, so this playbook does not kill a shim it merely
	// assumes exists: an ordinary prose turn is run to its settled terminal
	// first, which is the product's own statement that the route worked.
	s.submit(t, "draw one plain prose answer before the link is cut")
	s.awaitArm(t, name, "the first turn to settle, which proves the route served", playtestDoneArm)
	s.captureArm(t, "arm-before-cut", name,
		"one plain-prose turn submitted with composer RET and run to its terminal",
		playtestDoneArm,
		"THE REVIEWER'S BASELINE for the three pictures below. The tab is GREEN: the route served, a "+
			"turn ran on it and finished, and the feed carries the prompt bubble and the answer "+
			"beneath it. Nothing is broken in this picture.")

	// THE CUT. See this file's header for why it is a SIGKILL and why it is
	// aimed at the daemon's own child.
	shimPID := playtestShimPID(t, e)
	if err := syscall.Kill(shimPID, syscall.SIGKILL); err != nil {
		t.Fatalf("SIGKILL this world's shim (pid %d): %v", shimPID, err)
	}
	// `coldGateReapBound` is reused rather than restated: it is the same
	// kernel work on the same kind of process -- one node shim leaving the
	// process table after a SIGKILL -- and the reason recorded there for it
	// deserving seconds is this site's reason too. It is never spent on a
	// healthy run; the wait ends the moment the pid is gone.
	awaitPIDsGone(t, []int{shimPID}, coldGateReapBound)
	p.note("this world's shim SIGKILLed out from under the daemon",
		"the shim's pid has left the process table, so the route is genuinely gone rather than merely signalled")

	// EITHER OF THE TWO, for the FIRST push. See the header: `severed` and
	// `dead` are the same loss seen before and after the child is reaped, and
	// after a SIGKILL both orderings are real.
	cut := s.awaitArm(t, name, "the tab's arm to report the route it lost",
		playtestSeveredArm, playtestDeadArm)
	s.captureArm(t, "arm-link-severed", name,
		"the shim killed, and the daemon's own link state published for the route it lost",
		cut,
		"BLUE is the whole subject: a COMPROMISED ROUTE to work that is otherwise fine. So the tab "+
			"must NOT paint the PURPLE that means the vendor's own work failed -- nothing failed at "+
			"the vendor, the local process is simply gone -- and must not paint the RED of a running "+
			"turn, because no turn is running.")

	// AND THEN `:dead` BY NAME, which is where a reaped child's route
	// settles: `linkArm` answers `dead` once the link is dead and the route
	// had ever connected, which this one had (the baseline turn above proves
	// it).
	s.awaitArm(t, name, "the route's arm to settle on the reaped child's own arm", playtestDeadArm)
	// THE WEBAPP'S OWN STATEMENT ABOUT THE SAME LINK. The footer's status
	// word carries `footer-status arm-<case>` (`webapp/src/footer/strip.ts`),
	// and `disconnected` is the case the footer resolver's own first step
	// answers for a route that is not serving
	// (`daemon/internal/resolve/footer/status.go`). So this asserts the strip
	// and the dot agree about one link, which is what those two files claim
	// of each other.
	s.awaitInPage(t, "the webapp's footer to paint its disconnected status",
		`document.querySelector(".footer-status.arm-disconnected")`)
	s.captureArm(t, "page-degraded", name,
		"the route settled on the reaped child's arm, with the webapp's own footer read alongside it",
		playtestDeadArm,
		"TWO SURFACES, ONE LINK. Inside the panel the webapp's footer status word reads DISCONNECTED, "+
			"and the tab above it is still BLUE. The two must agree: a footer that says disconnected "+
			"beside a green tab, or a blue tab beside a serving footer, is the defect this picture "+
			"exists to catch.")

	// RECOVERY, provoked the way a user provokes it: by asking for work. No
	// revive command is invoked, because the product's own answer to a dead
	// route is to bring a fresh shim up on the next prompt -- and if that is
	// not what happens, the submit is refused or never runs and this step
	// fails RIGHT HERE, which is the design.
	s.submit(t, "wake the link back up")
	// THE OUTCOME IS AWAITED, NEVER THE MOMENT.
	//
	// This step once awaited a RUNNING arm here and then `:done`. Both are
	// satisfied by a turn that ran, and the first is a race: the revived
	// walk is `:dead` -> `:init` while the fresh shim spawns (measured at
	// ~350ms) -> a running arm -> `:done`, and the running half is ~107ms
	// wide on this fake vendor. `AwaitEval` samples every 20ms through an
	// emacsclient round trip whose own healthy maximum is ~95ms, so under
	// load one sample can straddle the whole running window -- and the wait
	// then fails with `:done` as its last value, which is the CORRECT
	// outcome reported as a failure. Widening the bound cannot help: the
	// window is not late, it is narrow, and the poll is not there.
	//
	// Worse, the two awaits together asserted nothing about the turn: had
	// the submit never run, the arm would have stayed on the FIRST turn's
	// `:done` and the second await would have passed on it. So the walk is
	// read off the recorder -- which sees every roster push in publication
	// order, skipping none (`daemon/internal/publish/topic.go`) -- and the
	// turn's own terminal row is counted in the feed.
	s.awaitArm(t, name, "the second turn to settle on the route the daemon brought back",
		playtestDoneArm)
	// TWO CONCLUDED TURNS: the baseline turn's terminal row and the revived
	// one's. The TERMINAL row is counted rather than the answer bubbles
	// because a turn draws as many answer rows as the vendor sent units --
	// this fake sends two per prose turn -- while `turnEnded` is exactly one
	// row per turn and carries that turn's own settled outcome on
	// `data-state`. So this counts turns that ENDED, which is the claim.
	s.awaitInPage(t, "the revived turn to have concluded in the feed",
		`document.querySelectorAll('[data-feed-row][data-row-kind="turnEnded"][data-state="concluded"]').length === 2`)
	assertRevivedArmWalk(t, name, s.recordedArmWalk(t, name))
	// AND THE SHIM IS A DIFFERENT PROCESS. `playtestShimPID` fails unless
	// this world holds EXACTLY ONE shim, so this asserts both halves of "a
	// fresh shim": the killed one did not answer the prompt (it is gone, and
	// a second live shim would fail the lookup) and the one that did is not
	// it.
	if revived := playtestShimPID(t, e); revived == shimPID {
		t.Fatalf("the revived turn ran on pid %d, which is the shim this playbook SIGKILLed; the daemon answered the prompt on a dead route rather than bringing a fresh shim up", revived)
	}
	s.captureArm(t, "arm-recovered", name,
		"a second prompt submitted against the dead route, which the daemon answered by bringing a fresh shim up",
		playtestDoneArm,
		"GREEN AGAIN, and the blue is gone: the daemon brought a fresh shim up on the prompt and the "+
			"turn ran on it to its terminal. The feed carries the NEW answer beneath the new prompt "+
			"bubble, with the first turn's two bubbles still above them, and the footer no longer "+
			"reads disconnected.")
}
