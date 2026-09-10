//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// OWNER 19 of PLAYTEST-PLAN.md's partition: J57-J60 -- the daemon's own
// lifecycle as a user sees it.
//
//	J.57  a scheduled shutdown raises a standing drain banner; cancelling
//	      takes it down.
//	J.58  `UpdateShutdownSchedule{now}` -- every tab goes down, and Emacs
//	      does not wedge.
//	J.59  a GRACEFUL workspace restart holds the prompt written meanwhile;
//	      a FORCED one interrupts the running turn.
//	J.60  a real blue-green handover: the tabs survive it, the adopted
//	      session keeps answering, and a daemon that then goes away
//	      surfaces and reconnects.
//
// WHERE THE DRAIN BANNER BELONGS, AND WHY IT IS ASSERTED RATHER THAN LOOKED
// AT. `endpoint_watch_daemon.proto` calls it "the page-wide restart banner",
// which says the banner is the PAGE's rather than any feed's and stops
// there -- it names no geometry. The lead's ruling on that silence is what
// this file pins: the notice is drawn in the MAIN COLUMN, above the feed,
// spanning that column, and it stands until the drain resolves. Every half
// of that is a programmatic assertion in
// `playtestDrainBannerPlacement` below, read off the LIVE BOXES, so a layout
// that moves it into the sidebar or down into the footer reds HERE rather
// than in a reviewer's judgment of a picture.

// ---------------------------------------------------------------------------
// SELECTORS AND ARMS
// ---------------------------------------------------------------------------

// The page hooks this owner reads. Every one of them is a `data-*` attribute
// the webapp's own suite already asserts, never a rendered word.
//
//   - `[data-component="drain-banner"]` is the banner HOST (`webapp/index.html`)
//     and `[data-drain-scheduled]` is the notice `drawDrainNotice` puts in it
//     (`webapp/src/lifecycle/lifecycle.ts`). The host is always in the
//     document; the notice exists only while a schedule stands, which is what
//     makes "the banner came down" a statement about the page rather than
//     about a stylesheet.
//   - `.failure-card[data-arm="daemonUnreachable"]` is the client-local card
//     the failure overlay draws when the page's streams lose the daemon
//     (`webapp/src/failure/overlay.ts`, `webapp/src/rpc/streams.ts`).
//   - `.turn-ended[data-arm="interrupted"]` is `FeedTurnEnded.interrupted`'s
//     own row (`webapp/src/feed/rows/turn-ended.ts`).
const (
	playtestDrainBannerHost = `[data-component="drain-banner"]`
	playtestDrainNotice     = playtestDrainBannerHost + ` [data-drain-scheduled]`
	playtestUnreachableCard = `#failure-overlay .failure-card[data-arm="daemonUnreachable"]`
	playtestInterruptedRow  = `.turn-ended[data-arm="interrupted"]`
)

// playtestInterruptedArm is the arm a workspace settles on when its turn was
// STOPPED rather than allowed to end. It is named rather than folded into
// `emGHISettledArms` for the reason owner 6 names its own arms: a wait on
// the settled set would be satisfied by `:done`, which is the very state
// this section exists to distinguish an interrupt from.
const playtestInterruptedArm = ":interrupted"

// playtestDrainReason is the reason arm every schedule in this file states.
// `DrainReason` is REQUIRED by the proto and the banner names it verbatim,
// so it is the one word a reviewer can check the picture's sentence against.
const playtestDrainReason = "maintenance"

// playtestDrainReasonMark selects the notice's reason element carrying the
// arm this file's schedules state.
//
// THE ATTRIBUTE VALUE IS DOUBLE-QUOTED IN THE SELECTOR RATHER THAN BUILT BY
// `jsString`, and it is a measured mistake rather than a style choice:
// `jsString` renders a JavaScript STRING LITERAL, in single quotes, and the
// selectors here are themselves single-quoted -- so splicing one into a
// selector closes the literal early, WebKit never parses the script, the
// probe's callback never fires, and the wait's last value is the probe's
// untouched nil. That is a page assertion that fails saying nothing about
// the page.
const playtestDrainReasonMark = playtestDrainNotice + ` .lifecycle-banner-reason[data-arm="` + playtestDrainReason + `"]`

// playtestDrainMinutes puts the drain's instant far enough out that the
// daemon is still serving for the rest of the playbook AND for its teardown:
// the standing SCHEDULE is what is under test, never its firing.
const playtestDrainMinutes = 5

// ---------------------------------------------------------------------------
// THE RULED PLACEMENT, AS AN ASSERTION
// ---------------------------------------------------------------------------

// playtestDrainBannerPlacement is the JavaScript predicate that holds when
// the standing notice is where the ruling puts it.
//
// FOUR CLAIMS, ALL READ OFF LIVE BOXES rather than off the markup:
//
//  1. the notice's host is a child of `#main-col` -- so it is in the MAIN
//     COLUMN, not in the sidebar rail and not in the footer;
//  2. its top edge is at or below the topbar's bottom edge;
//  3. its bottom edge is at or above the scroll zone's top edge -- so it is
//     ABOVE THE FEED rather than floating over it;
//  4. it SPANS that column: symmetrically inset, and at least 95% of the
//     column's width.
//
// WHY THE FOURTH CLAIM IS "SPANS" RATHER THAN "EDGE TO EDGE", AND IT IS A
// MEASUREMENT. The ruling says full width of the main column. The product
// draws the notice as a CARD -- `.lifecycle-banner` takes
// `margin: 0.25rem 0.75rem` and a rounded amber outline, the same register
// `#revival-gate` is drawn in, and `styles.css` states that choice as a
// deliberate one ("outlined rather than filled", so a routine restart does
// not spend the alarm's red). Measured in this playbook's own frame: the
// main column is 1008px wide, the banner's box 984px, inset 12px on each
// side -- 97.6% of the column, centred in it, with the topbar above and the
// scroll zone below both running the column's full 1008px.
//
// So the claim is written to catch everything the ruling is ABOUT -- a
// notice in the sidebar rail, one in the footer, one floating over the feed,
// a corner chip, a half-width strip, an off-centre one -- while accepting
// the card's own 12px gutter. A banner narrower than 95% of the column, or
// one whose two insets differ, reds here.
//
// The 1px slack on the edge comparisons is subpixel layout, not tolerance
// for being wrong: browsers round fractional box edges, and two boxes that
// abut exactly can read a hair apart.
//
// A MISPLACED BANNER THROWS ITS OWN NUMBERS. `pageYes` reports a thrown
// message verbatim, so a banner that is in the wrong place fails with the
// four boxes it was measured against rather than with a bare "never
// satisfied" -- which is the difference between a finding and a rerun.
const playtestDrainBannerPlacement = `(function () {
                   var notice = document.querySelector('` + playtestDrainNotice + `');
                   if (!notice) { return false; }
                   var host = notice.parentElement;
                   var column = document.getElementById('main-col');
                   var topbar = document.getElementById('topbar');
                   var scroll = document.getElementById('feed-scroll');
                   if (!host || !column || !topbar || !scroll) { return false; }
                   var b = host.getBoundingClientRect();
                   var t = topbar.getBoundingClientRect();
                   var s = scroll.getBoundingClientRect();
                   var c = column.getBoundingClientRect();
                   if (host.parentElement === column &&
                       b.height > 0 && b.width > 0 &&
                       b.top >= t.bottom - 1 &&
                       b.bottom <= s.top + 1 &&
                       b.width >= c.width * 0.95 &&
                       Math.abs((b.left - c.left) - (c.right - b.right)) <= 1) { return true; }
                   throw new Error("the drain banner is not where the ruling puts it: host parent is #" +
                     (host.parentElement ? host.parentElement.id : "<detached>") +
                     " banner=" + JSON.stringify(b) +
                     " topbar=" + JSON.stringify(t) +
                     " scrollZone=" + JSON.stringify(s) +
                     " mainColumn=" + JSON.stringify(c)); })()`

// playtestScheduleDrain arms a drain MINUTES out through the ordinary
// interactive command, answering its reason prompt the standard ERT way: the
// reader is bound for the duration of the one call, so the command runs its
// own argument collection rather than being bypassed.
func playtestScheduleDrain(t *testing.T, s *playtestScenario, minutes int) {
	t.Helper()
	s.E.Eval(`(cl-letf (((symbol-function 'completing-read)
                          (lambda (&rest _) ` + elispString(playtestDrainReason) + `)))
                (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(minutes) + `)
                t)`)
}

// TestPlaytestScheduledDrainBanner is plan J.57: a scheduled shutdown, the
// standing banner it raises in two places at once, and the cancel that takes
// both of them down.
func TestPlaytestScheduledDrainBanner(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "19-drain",
		"Plan J.57. A scheduled shutdown, the standing drain banner it puts on Emacs's mode "+
			"line and across the webapp's main column, and the cancel that takes both down.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	// THE NEGATIVE IS ASSERTED BEFORE THE POSITIVE. A page that drew a
	// notice for reasons of its own would satisfy every wait below without
	// the schedule having done anything.
	s.awaitInPage(t, "the drain banner's host to be carrying nothing before any schedule stands",
		`document.querySelector('`+playtestDrainBannerHost+`') &&
         document.querySelector('`+playtestDrainBannerHost+`').children.length === 0`)
	p.note("one repository registered with its panel open, nothing scheduled",
		"the webapp drew its footer, and the drain banner's host holds no children")

	playtestScheduleDrain(t, s, playtestDrainMinutes)
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs", `(and agent-repl-link-drain t)`)
	if arm := e.EvalString(`(format "%s" (plist-get (plist-get agent-repl-link-drain :reason) :arm))`); arm != ":"+playtestDrainReason {
		t.Fatalf("the standing drain's reason arm is %q, want %q", arm, ":"+playtestDrainReason)
	}
	// The segment's own composition is the subject here, which is this
	// layer's sanctioned exception to "never scrape human text where a
	// variable exists".
	segment := e.EvalString(`(or agent-repl-link-drain-segment "")`)
	if !strings.HasPrefix(segment, "drain ") || !strings.HasSuffix(segment, "· "+playtestDrainReason) {
		t.Fatalf("the drain segment is %q, want \"drain HH:MM · %s\"", segment, playtestDrainReason)
	}
	// TWO STEPS, SMALLEST FIRST, so a failure names which half is missing:
	// the notice itself, and then the reason arm riding it. The arm is the
	// FACT the banner's words are composed from (`drawDrainNotice` puts the
	// oneof's own case on `[data-arm]` and the rendered words beside it), so
	// asserting it is asserting that the daemon's reason -- not a default --
	// reached the page.
	s.awaitInPage(t, "the webapp's own drain notice to be drawn",
		`document.querySelector('`+playtestDrainNotice+`')`)
	s.awaitInPage(t, "the drain notice to carry the daemon's own reason arm",
		`document.querySelector('`+playtestDrainReasonMark+`')`)
	// THE RULED PLACEMENT. See this file's header: the proto names no
	// geometry, so the lead's ruling is what is pinned, and it is pinned
	// programmatically rather than left to the picture.
	s.awaitInPage(t, "the drain banner to be drawn in the main column, above the feed, spanning that column",
		playtestDrainBannerPlacement)
	p.capture("drain-scheduled",
		fmt.Sprintf("`agent-repl-daemon-shutdown-schedule` %d minutes out, reason %q", playtestDrainMinutes, playtestDrainReason),
		fmt.Sprintf("`agent-repl-link-drain` carries the reason arm `:%s`, `agent-repl-link-drain-segment` renders %q, "+
			"the webapp's notice carries the same arm on `[data-arm]`, and its host is a child of `#main-col` whose box "+
			"sits between the topbar's bottom edge and the scroll zone's top edge, centred and spanning at least "+
			"95%% of that column's width",
			playtestDrainReason, segment),
		"The WEBAPP draws a STANDING DRAIN BANNER across the MAIN COLUMN -- an outlined amber card spanning "+
			"the column from the sidebar rail to the panel's right edge, off each edge by a small even gutter, "+
			"immediately under the thin topbar and immediately above the feed -- "+
			"reading \"daemon restart scheduled · maintenance · in 4m ...\". It is NOT in the sidebar and NOT in "+
			"the footer. Emacs's own `drain HH:MM · maintenance` segment is asserted as a string rather than read "+
			"off this picture: it lives in `global-mode-string`, which Doom's mode line renders on the right of "+
			"whatever line has room, so which line carries it is not this playbook's claim.")

	// THE CANCEL, through the ordinary command. It takes no arguments and
	// prompts for nothing, so nothing is stubbed for it.
	e.Eval(`(agent-repl-daemon-shutdown-cancel)`)
	e.AwaitTrue("the daemon's drain_cancelled push to reach Emacs", `(if agent-repl-link-drain nil t)`)
	if segment := e.EvalString(`(or agent-repl-link-drain-segment "")`); segment != "" {
		t.Fatalf("the drain segment is %q after the cancel, want it gone from the mode line entirely", segment)
	}
	// THE HOST IS EMPTY, not merely hidden. `clearDrain` replaces the host's
	// children, so an empty host is the page's own statement that no notice
	// stands -- a stylesheet that hid a notice still there would satisfy a
	// visibility check and leave the fact behind it.
	s.awaitInPage(t, "the webapp's drain banner to come down",
		`document.querySelector('`+playtestDrainBannerHost+`').children.length === 0`)
	p.capture("drain-cancelled",
		"`agent-repl-daemon-shutdown-cancel` -- the standing schedule dropped",
		"`agent-repl-link-drain` is nil, `agent-repl-link-drain-segment` is empty, and the webapp's banner host holds no children",
		"THE BANNER IS GONE. The main column is back to topbar-then-feed with no coloured strip between them, "+
			"the feed occupies the height the banner had, and Emacs's mode line no longer carries a `drain ...` "+
			"segment. Compare against the previous picture: the ONLY difference should be the banner's absence.")
}

// ---------------------------------------------------------------------------
// J.58 -- SHUTDOWN NOW
// ---------------------------------------------------------------------------

// TestPlaytestShutdownNowTakesEveryTabDown is plan J.58: the daemon asked to
// exit now, with TWO workspaces registered and a panel open on one of them.
//
// THE CLAIM HAS THREE HALVES and the picture is only about the third:
//
//   - every tab GOES DOWN -- the link drops on its own at the transport,
//     which is how this product detects a daemon that left (there are no
//     keepalive frames), and the reconnect loop is ARMED rather than the
//     outage being swallowed;
//   - EMACS DOES NOT WEDGE -- the heartbeat answers after the stop, and the
//     tab bar still draws both workspaces. A stop that took the editor's own
//     view down with the daemon would be the wedge this step names;
//   - the WEBAPP says so: the page's streams die and the failure overlay
//     draws its `daemonUnreachable` card.
//
// The stop is the module's OWN command. Emacs never kills a daemon: it sends
// `UpdateShutdownSchedule{now}` and the daemon exits itself, which is the
// only shutdown that strands nothing.
func TestPlaytestShutdownNowTakesEveryTabDown(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "19-shutdown-now",
		"Plan J.58. The daemon asked to shut down NOW with two workspaces registered and a panel "+
			"open: every tab goes down, the webapp says it lost the daemon, Emacs keeps answering, "+
			"and an ensure brings the whole view back.")
	p, e := s.Book, s.E

	// THE PANEL IS OPENED ON THE FIRST WORKSPACE BEFORE THE SECOND EXISTS,
	// which is the order every multi-workspace playbook here uses:
	// registering SELECTS, so a panel opened after both registrations would
	// be opened on the workspace this playbook does not act on.
	firstRepo := s.repoAt(t, "repo-one")
	first := s.register(t, firstRepo.Dir)
	s.openPanel(t)
	second := s.register(t, s.repoAt(t, "repo-two").Dir)
	e.AwaitEval("the second workspace to become the selected one on registration",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == second })
	// And the selection is put back on the workspace whose panel is open, so
	// the picture below is of the page this playbook drove.
	e.Eval(`(agent-repl-switch-to-project ` + elispString(firstRepo.Dir) + `)`)
	e.AwaitEval("the first workspace to be selected again, with its panel open",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == first })

	// A REAL TURN FIRST, so the tabs this step takes down are tabs that were
	// serving. A stop against a world where nothing ever ran would make the
	// same waits over a world that proves less.
	s.submit(t, "draw one plain prose answer before the daemon is stopped")
	s.awaitArm(t, first, "the first workspace's turn to settle, which proves the daemon served", emGHISettledArms...)
	if names := s.tabNames(); len(names) != 2 {
		t.Fatalf("the tab bar draws %v before the stop, want both %q and %q", names, first, second)
	}
	p.capture("before-stop",
		"two repositories registered, a panel open on the first, and one prose turn run to its terminal",
		fmt.Sprintf("the tab bar draws both %q and %q, and the first workspace's arm is settled", first, second),
		"THE REVIEWER'S BASELINE. Two tabs in the bar, the panel open on the first with its feed carrying "+
			"the prompt bubble and the answer beneath it, and a footer that is not reporting any outage. "+
			"Nothing is broken in this picture.")

	pid := e.EvalInt(emHODaemonPIDForm)
	if pid <= 0 {
		t.Fatalf("the launcher holds no live daemon process (pid %d) before the stop", pid)
	}

	// THE STOP.
	e.Eval(`(agent-repl-frontend-daemon-stop)`)
	// The daemon's OWN exit is the edge, so the pid leaving the process
	// table is asserted before anything downstream of it: a link that went
	// down while the daemon was still running would be a different defect
	// wearing this one's assertions. `daemonStopBound` is the layer's named
	// bound for exactly this phase -- the daemon flushing its writes and
	// exiting, the kernel closing the socket, Emacs's sentinel running.
	awaitPIDsGone(t, []int{pid}, daemonStopBound)
	e.AwaitEvalFor(daemonStopBound, "the link to go down when the daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	e.AwaitEvalFor(daemonStopBound, "the reconnect loop to be armed",
		`(and (timerp agent-repl-link--reconnect-timer) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	// NO WEDGE, and this is the assertion that says it: one more round trip
	// through Emacs's command loop after the act. Every `Eval` already fails
	// immediately when the heartbeat has missed, so this names the act the
	// editor survived rather than introducing a bound of its own.
	emGHIAssertResponsive(t, e, "the daemon's immediate shutdown")
	// THE VIEW IS KEPT. The daemon is the source of WHICH workspaces exist,
	// but a daemon that went away does not retract them: both tabs are still
	// drawn, which is what makes the outage recoverable rather than a reset.
	if names := s.tabNames(); len(names) != 2 {
		t.Fatalf("the tab bar draws %v after the daemon exited, want both %q and %q kept", names, first, second)
	}
	s.awaitInPage(t, "the webapp's failure overlay to draw its daemon-unreachable card",
		`document.querySelector('`+playtestUnreachableCard+`')`)
	p.capture("daemon-gone",
		"`agent-repl-frontend-daemon-stop` -- the daemon accepted the immediate shutdown and exited",
		fmt.Sprintf("the daemon's pid %d has left the process table, `agent-repl-link-up-p` is nil, the reconnect "+
			"timer is armed, Emacs answered `(+ 1 1)` afterwards, the tab bar still draws both %q and %q, and the "+
			"page carries a `daemonUnreachable` failure card", pid, first, second),
		"EVERY TAB IS DOWN AND NOTHING IS WEDGED. Both tabs are STILL DRAWN in the bar -- the view was kept, "+
			"not torn down -- and neither of them is painted as if work were running. Inside the panel the webapp "+
			"draws a client-local failure card reading \"lost the connection to the daemon; reconnecting\", and the "+
			"feed's two bubbles from the earlier turn are still there beneath it. Emacs's own frame is intact: mode "+
			"lines, tab bar and minibuffer all drawn.")

	// A DAEMON RETURNS, launched the way it was the first time.
	e.Eval(`(agent-repl-frontend-daemon-ensure)`)
	emHOAwaitNewDaemon(t, e, pid)
	emHOAwaitLinkUp(t, e, "the link to come back when a daemon returns")
	e.AwaitEvalFor(daemonLinkBound, "the reconnect loop to stand down once the link is up",
		`(if (timerp agent-repl-link--reconnect-timer) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	// THE ROSTER REBUILDS THE SAME TWO TABS. They are the daemon's to
	// publish, so this is the fresh daemon's statement rather than Emacs's
	// memory of the old one.
	e.AwaitEval("the tab bar to carry both workspaces again on the fresh daemon",
		emHOTabBarNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == 2 })
	p.capture("daemon-back",
		"`agent-repl-frontend-daemon-ensure` -- a fresh daemon launched and adopted",
		fmt.Sprintf("a NEW daemon pid is live (the old one was %d), `agent-repl-link-up-p` is non-nil, the reconnect "+
			"timer stood down, and the tab bar carries two workspaces again", pid),
		"THE OUTAGE IS OVER. Both tabs are drawn as before. The failure card is gone from the panel -- the "+
			"overlay is empty again -- and nothing in this picture reports a disconnection. This is the "+
			"`before-stop` picture again, and any difference from it other than the feed's scroll position is "+
			"something the restart did not restore.")
}

// ---------------------------------------------------------------------------
// J.59 -- THE TWO RESTARTS
// ---------------------------------------------------------------------------

// TestPlaytestForcedRestartInterruptsTheTurn is the FORCED half of plan
// J.59: `SPC o C-c` with a prefix argument, against a genuinely live turn.
//
// The fake's `!interrupt` scenario parks inside `awaitInterrupt()`, so the
// turn does not end on its own and "mid-turn" is a fact rather than a race:
// the only thing that can end it is the interrupt this step issues.
//
// THE CONTRACT HAS TWO HALVES and both are asserted, because the first alone
// cannot tell a stopped turn from a resumed one: the turn stops -- the arm
// settles `:interrupted` and the feed draws the interrupted terminal -- and
// the agent is NOT resumed afterwards.
func TestPlaytestForcedRestartInterruptsTheTurn(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "19-restart-forced",
		"Plan J.59, the forced half. A parked turn cut down by a prefix-argument workspace restart: "+
			"the interrupted terminal in the feed, and an agent that is not quietly resumed behind it.")
	p, e := s.Book, s.E

	name := s.register(t, s.repoAt(t, "repo").Dir)
	s.openPanel(t)
	s.submit(t, emGHIParkedPrompt)
	s.awaitArm(t, name, "the turn to be running before the interrupt", emGHIRunningArms...)
	s.awaitInPage(t, "the parked turn's own prompt bubble to be drawn",
		`document.querySelector('.bubble.user')`)
	p.note("`"+emGHIParkedPrompt+"` submitted with composer RET, parking the turn inside the fake's own wait",
		"the workspace's roster arm is in the running half, and the feed carries the prompt bubble")

	// THE FORCED RESTART. `agent-repl-restart-workspace` takes FORCE as its
	// first documented argument, so the prefix is supplied the way the
	// command itself reads it rather than by pressing a key that would then
	// also have to satisfy the picker -- and the binding is asserted, so a
	// `SPC o C-c` that resolved elsewhere fails here rather than later.
	if want, got := "agent-repl-restart-workspace", e.LeaderBinding("o C-c"); got != want {
		t.Fatalf("SPC o C-c resolves to %q, want %q", got, want)
	}
	e.Eval(`(agent-repl-restart-workspace t ` + elispString(name) + `)`)

	// `:interrupted` BY NAME. Any other settled arm would say the turn ENDED
	// rather than that it was stopped, which is the whole distinction here.
	s.awaitArm(t, name, "the roster arm to settle interrupted", playtestInterruptedArm)
	s.awaitInPage(t, "the feed's interrupted terminal row to be drawn",
		`document.querySelector('`+playtestInterruptedRow+`')`)
	s.captureArm(t, "turn-interrupted", name,
		"`SPC o C-c` with a prefix argument, against the parked turn",
		playtestInterruptedArm,
		"THE TURN WAS STOPPED, NOT FINISHED. In the feed, beneath the prompt bubble, the turn's terminal "+
			"row says the turn was INTERRUPTED -- not a completed answer, and not a failure card. Nothing in "+
			"the panel is drawn as still running: no thinking indicator, no live tool arc.")

	// THE AGENT IS NOT RESUMED. The arm STAYS settled -- a resumption would
	// move it back into the running half -- and no prompt was re-driven from
	// the queue behind this playbook's back.
	if got := s.awaitArm(t, name, "the roster arm to stay settled after the interrupt", emGHISettledArms...); got != playtestInterruptedArm {
		t.Fatalf("the roster arm moved to %s after the forced restart, want it to stay %s: the agent was resumed",
			got, playtestInterruptedArm)
	}
	emGHIAwaitHeldPrompts(t, e, name, "no prompt to be held after a forced restart", 0)
	p.note("the roster arm re-read after the interrupt, and the prompt queue read as data",
		"the arm is still "+playtestInterruptedArm+" and `agent-repl-prompt-queue-pending` is empty, so nothing was resumed")
}

// TestPlaytestGracefulRestartHoldsThePrompt is the GRACEFUL half of plan
// J.59: a restart with no prefix argument WAITS for the running turn, and
// the prompt a user writes meanwhile is HELD rather than refused.
//
// THE PARKED TURN IS THE FAKE'S TURN GATE, not `!interrupt`. A graceful
// restart waits for the turn to finish, and `!interrupt` parks inside a wait
// only a FORCED restart resolves -- so a gated turn is the one park with two
// exits, and the finish edge this playbook is about can actually happen.
//
// UNDELIVERED USER INTENT MAY NEVER BE SILENTLY DISCARDED, which is the
// claim: the difference between "the restart lost my prompt" and "the
// restart delayed it".
func TestPlaytestGracefulRestartHoldsThePrompt(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "19-graceful-restart-gate")
	s := newPlaytestScenario(t, "19-restart-graceful",
		"Plan J.59, the graceful half. A restart with no prefix argument waits for the running turn, "+
			"the prompt written meanwhile is held rather than refused, and it is delivered on the finish edge.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, emGHIGatedPrompt))
	p, e := s.Book, s.E

	name := s.register(t, s.repoAt(t, "repo").Dir)
	s.openPanel(t)
	s.submit(t, emGHIGatedPrompt)
	s.awaitArm(t, name, "the gated turn to be running before the restart", emGHIRunningArms...)

	// The user writes a second prompt mid-turn and enqueues it through the
	// ordinary command. The binding is asserted first: a `SPC j RET` that
	// resolved elsewhere would queue nothing and fail a wait later with no
	// cause attached.
	if want, got := "agent-repl-queue-deferred-prompt", e.LeaderBinding("j RET"); got != want {
		t.Fatalf("SPC j RET resolves to %q, want %q", got, want)
	}
	typeIntoComposer(e, s.Input, playtestHeldPromptText)
	e.Eval(`(agent-repl-queue-deferred-prompt)`)

	// HELD, NOT REFUSED -- and the composer was cleared, because the text is
	// now the queue's rather than the draft's.
	emGHIAwaitHeldPrompts(t, e, name, "the deferred prompt to be held", 1)
	if got := e.EvalString(`(with-current-buffer ` + elispString(s.Input) + ` (buffer-string))`); got != "" {
		t.Fatalf("the composer holds %q after the deferral, want it emptied into the queue", got)
	}
	p.capture("prompt-held",
		"a second prompt written mid-turn and deferred with `SPC j RET`",
		fmt.Sprintf("`agent-repl-prompt-queue-pending` holds exactly one entry for %q and the composer buffer is empty", name),
		"THE PROMPT IS NOT LOST. The gated turn is still running in the feed -- the first prompt's bubble is "+
			"there with the turn still live beneath it -- and the composer at the bottom of the frame is EMPTY: "+
			"the text the user just wrote has left the draft. Nothing in the picture reports a refusal.")

	// THE GRACEFUL RESTART -- no prefix argument, so the turn is not forced
	// down.
	e.Eval(`(agent-repl-restart-workspace nil ` + elispString(name) + `)`)
	// The graceful restart WAITS for the turn and nothing else will end it,
	// so the gate is opened HERE -- deliberately after the restart is
	// issued, because the restart has to be in flight while the turn still
	// is.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}

	// THE QUEUE DRAINS ON THE FINISH EDGE the restart produced. One entry
	// per finished turn is the queue's own contract, and one entry is all
	// this playbook wrote.
	s.awaitArm(t, name, "the gated turn to settle after the graceful restart", emGHISettledArms...)
	emGHIAwaitHeldPrompts(t, e, name, "the held prompt to drain on the finish edge", 0)
	// AND IT WAS DELIVERED, not merely dropped from the queue. The prompt's
	// own bubble in the feed is what says the text reached the agent; an
	// emptied queue alone is equally consistent with the prompt being
	// discarded, which is the failure this whole playbook exists for.
	s.awaitInPage(t, "the held prompt's own bubble to be drawn in the feed",
		`(function () {
                   var bubbles = document.querySelectorAll('.bubble.user');
                   for (var i = 0; i < bubbles.length; i++) {
                     if (bubbles[i].textContent.indexOf(`+jsString(playtestHeldPromptText)+`) !== -1) { return true; }
                   }
                   return false; })()`)
	s.awaitTailClearsFooter(t)
	p.capture("prompt-delivered",
		"the turn gate opened, so the gated turn reached its ordinary terminal and the restart's finish edge fired",
		fmt.Sprintf("the roster arm for %q is settled, `agent-repl-prompt-queue-pending` is empty, and a `.bubble.user` "+
			"carrying %q is drawn in the feed", name, playtestHeldPromptText),
		"THE DELAYED PROMPT RAN. The feed carries TWO user bubbles: the gated first prompt with its answer, and "+
			"beneath them the held prompt (\""+playtestHeldPromptText+"\") with its own turn. The prompt was DELAYED "+
			"by the restart, never discarded -- a feed showing only the first prompt is the defect this picture "+
			"exists to catch.")
}

// playtestHeldPromptText is the second prompt J.59's graceful half writes
// mid-turn. It is named because the manifest sentence quotes it and the page
// assertion searches for it, and a string spelled twice drifts.
const playtestHeldPromptText = "the held prompt"

// ---------------------------------------------------------------------------
// J.60 -- HANDOVER
// ---------------------------------------------------------------------------
//
// THE SUCCESSOR IS PROVOKED FOR REAL, and it has to be. Emacs attaches one
// from EXACTLY ONE push: a `shutdown_announced` that CARRIES AN ADDRESS,
// published only by `daemon/internal/rollout/handover.go` when a self-merge
// rollout lands on the daemon's own checkout. A plain bounce's announcement
// carries no address and cannot stand in, and dialing an address nothing
// announced would photograph Emacs's own dial rather than the handover.
//
// So the arrangement is `emacs_handover_e2e_test.go`'s scenario 40, reused
// rather than restated -- the daemon's own checkout as environment, a merge
// gate that passes, a fake deploy chain -- with ONE addition that is the
// whole point of photographing it: the workspace whose tabs and session must
// survive is OPEN, with a live webview, so what the handover does to the
// EDITOR and to the PAGE is visible rather than merely readable.

// TestPlaytestHandoverKeepsTheTabsAndReconnects is plan J.60.
func TestPlaytestHandoverKeepsTheTabsAndReconnects(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	selfRepo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "self-repo"))
	gate := harness.NewTestAllScript(t, selfRepo.Dir)
	gate.SetExitCode(0)
	gate.SetStdout("e2e: passed in 1s\n")
	// The rollout's DEPLOY CHAIN. Without it the trigger resolves
	// `bin/deploy-all.sh` relative to the daemon's own cwd, the exec fails,
	// and the self-reload aborts BEFORE a successor is ever spawned -- so no
	// announcement could carry an address and no handover could begin.
	deploy := harness.NewFakeDeployScript(t, filepath.Join(selfRepo.Dir, "bin"))
	deploy.SetExitCode(0)
	s := newPlaytestScenario(t, "19-handover",
		"Plan J.60. A real blue-green handover at freeness: the tabs survive it, the adopted session "+
			"keeps answering on the successor, and a daemon that then goes away surfaces and reconnects.",
		WithEmacsEnv("AGENT_REPL_SELF_REPO_DIR", selfRepo.Dir),
		WithEmacsEnv("AGENT_REPL_TEST_ALL_SCRIPT", gate.Path),
		WithEmacsEnv("AGENT_REPL_DEPLOY_SCRIPT", deploy.Path))
	p, e := s.Book, s.E

	// THE WORKSPACE THAT MUST SURVIVE, and it is OPEN. Its panel is what
	// makes this a playbook rather than a second copy of scenario 40: the
	// handover has to leave a live webview serving off the successor.
	play := s.register(t, s.repoAt(t, "repo-play").Dir)
	s.openPanel(t)
	s.submit(t, "draw one plain prose answer on the outgoing daemon")
	s.awaitArm(t, play, "the pre-handover turn to settle on the outgoing daemon", emGHISettledArms...)

	// The hooks are armed BEFORE anything can announce, so neither edge can
	// be missed: a promotion that completed between two polls would
	// otherwise be indistinguishable from one that never happened.
	e.Eval(emHO40Instrument)

	// The daemon's own checkout is registered too, so the roster carries the
	// repository section the create command picks from.
	beforeSelf := e.EvalStrings(emGHIWorkspaceNamesForm)
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(selfRepo.Dir) + `)`)
	label := emHO40AwaitSectionLabel(t, e, selfRepo.Dir)
	e.AwaitEval("the daemon's own checkout to appear in Emacs's registry",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(beforeSelf)+1 })

	// The trigger workspace, created through the ordinary command, with its
	// opening turn let run to a terminal -- a merge is enqueued from idle,
	// never mid-turn.
	beforeCreate := e.EvalStrings(emGHIWorkspaceNamesForm)
	emHO40Create(t, e, label)
	after := decodeStrings(e.AwaitEval("the trigger workspace to appear in Emacs's registry",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(beforeCreate)+1 }))
	trigger := emHO40AddedName(t, beforeCreate, after)
	s.awaitArm(t, trigger, "the trigger workspace's opening turn to conclude", emGHISettledArms...)

	// THE SELECTION IS PUT BACK ON THE PLAY WORKSPACE. Creating a workspace
	// SELECTS it, so the trigger has been the current one since it was made
	// -- and the merge below takes it away entirely, which would leave the
	// editor with no current workspace at all. A composer RET then resolves
	// no workspace and the send is gated as `ws=none`, which is exactly what
	// this playbook measured before the switch was added.
	// THE WORKSPACE PICKER, not the project one. `agent-repl-switch-to-project`
	// with a PROJECT ROOT goes through `projectile-switch-project-by-name`,
	// which only moves for a root projectile itself knows; with NO argument
	// it completes over `agent-repl--live-ws-names` and switches by
	// WORKSPACE NAME, which is the roster's own vocabulary and the one this
	// playbook has. So the binding is asserted and the picker is answered,
	// which is how every prompting verb is driven here.
	if want, got := "agent-repl-switch-to-project", e.LeaderBinding("p p"); got != want {
		t.Fatalf("SPC p p resolves to %q, want %q", got, want)
	}
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(play) + `)))
              (agent-repl-switch-to-project)
              t)`)
	e.AwaitEval("the play workspace to be the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == play })

	tabsBefore := s.tabNames()
	if len(tabsBefore) < 3 {
		t.Fatalf("the tab bar draws %v before the handover, want at least the play, self and trigger workspaces", tabsBefore)
	}
	p.capture("before-handover",
		"a play workspace with its panel open and one settled turn, the daemon's own checkout registered, and a trigger workspace created on it",
		fmt.Sprintf("the tab bar draws %v, and every one of those workspaces is settled", tabsBefore),
		"THE REVIEWER'S BASELINE for the handover. Three or more tabs in the bar, the panel open on the play "+
			"workspace with its prompt bubble and answer in the feed, and NO banner of any kind across the main "+
			"column. Nothing is restarting in this picture.")

	// THE TRIGGER: a scripted commit on a daemon-subsystem path, merged
	// through the ordinary command. The merge lands, the rollout classifies,
	// and the handover announces WITH an address.
	triggerDir := e.EvalString(`(or (plist-get (agent-repl-host-ref ` + elispString(trigger) + `) :dir) "")`)
	if triggerDir == "" {
		t.Fatalf("the trigger workspace %q holds no worktree directory, want the daemon-minted one", trigger)
	}
	sha := selfRepo.CommitIn(triggerDir, emHO40SelfMergeTriggerPath, "trigger\n")
	selfRepo.SetPaths(sha, emHO40SelfMergeTriggerPath)
	e.Eval(`(agent-repl-merge-workspace ` + elispString(trigger) + `)`)

	// A SUCCESSOR IS ATTACHED, from the announced address. This is the half
	// no plain bounce can produce.
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

	// AT FREENESS THE SUCCESSOR IS PROMOTED, and to the SAME address it was
	// attached at. A promotion to some other address would be a reconnect
	// wearing the handover's name.
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
		t.Fatalf("the promoted connection is at %q, want the attached successor's %q: this is a reconnect, not a handover",
			promoted, successor)
	}

	// THE TABS SURVIVE IT. They are the daemon's to publish and the
	// SUCCESSOR is the daemon now, so this is the new daemon's roster rather
	// than Emacs's memory of the old one.
	//
	// EVERY TAB EXCEPT THE TRIGGER'S, and that exception is the product
	// rather than a softened claim: the trigger workspace was MERGED, which
	// is what provoked the rollout, and a merged workspace's worktree is
	// gone -- the outgoing daemon records "the workspace's worktree is gone;
	// it is not handed over" and Emacs tore its tab down at the merge. So a
	// trigger tab still drawn here would be the defect, not its absence.
	survivors := playtestWithout(tabsBefore, trigger)
	if len(survivors) != len(tabsBefore)-1 {
		t.Fatalf("the pre-handover tab bar %v does not carry the trigger workspace %q", tabsBefore, trigger)
	}
	e.AwaitEval("the tab bar to carry every surviving pre-handover workspace on the successor",
		emHOTabBarNamesForm,
		func(raw json.RawMessage) bool {
			drawn := decodeStrings(raw)
			for _, want := range survivors {
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
	// THE PAGE CAME BACK. `transferred` re-points the webview
	// (`lisp/host.el`), so the panel is live against the SUCCESSOR -- the
	// footer's status word is non-empty only once the new daemon's
	// `WatchFooter` push has arrived and been rendered.
	s.awaitPageMounted(t)
	p.capture("after-handover",
		"the self-merge merged, the rollout classified, a successor spawned, attached and promoted at freeness",
		fmt.Sprintf("`agent-repl-link-handover-functions` and `agent-repl-link-promote-functions` both fired for %q, "+
			"the tab bar still carries %v, and the panel's page is live again (its footer status is non-empty)",
			promoted, survivors),
		"THE HANDOVER LEFT THE EDITOR STANDING. The same tabs are in the bar as in the `before-handover` "+
			"picture EXCEPT the trigger workspace's, which is gone because it was merged -- that is what "+
			"provoked this rollout. The panel still shows the play workspace's feed with its earlier prompt and answer. "+
			"The page is live against a DIFFERENT daemon than the one that drew it: the footer reports a status "+
			"rather than an outage, and no failure card is drawn.")

	// THE ADOPTED SESSION CONTINUES. A second prompt on the SAME workspace,
	// answered by the successor, is what says the session was adopted rather
	// than merely that its tab survived.
	s.submit(t, "answer this one on the successor")
	s.awaitArm(t, play, "the post-handover turn to run on the successor", emGHIRunningArms...)
	s.awaitArm(t, play, "the post-handover turn to settle on the successor", emGHISettledArms...)
	s.awaitTailClearsFooter(t)
	p.capture("adopted-session-continues",
		"a second prompt submitted with composer RET after the promotion",
		fmt.Sprintf("%q's arm moved through the running half and settled again, on the promoted connection at %q", play, promoted),
		"THE CONVERSATION CONTINUED ACROSS THE HANDOVER. The feed carries BOTH turns: the one answered by the "+
			"outgoing daemon and, beneath it, the one answered by its successor. The session was adopted, not "+
			"restarted -- a feed showing only the new turn would mean the history was lost with the old daemon.")

	// AND A DAEMON GOING AWAY SURFACES AND RECONNECTS. The successor is the
	// daemon now, so this is the ordinary stop against it.
	//
	// NO LAUNCHER PID IS READ HERE, and its absence is the product rather
	// than a gap: `agent-repl--frontend-daemon-process` holds the child
	// EMACS spawned, and the daemon now serving was spawned by its
	// PREDECESSOR'S deploy chain. `emHODaemonPIDForm` answers -1 for it,
	// which is correct and says nothing about whether it is serving. What
	// says that is the link, and the link is what this step stops.
	if pid := e.EvalInt(emHODaemonPIDForm); pid > 0 {
		t.Fatalf("the launcher holds a live daemon process (pid %d) after the handover, want none: "+
			"the serving daemon is the successor its predecessor deployed, not a child of this Emacs", pid)
	}
	e.Eval(`(agent-repl-frontend-daemon-stop)`)
	e.AwaitEvalFor(daemonStopBound, "the link to go down when the promoted daemon exits",
		`(if (agent-repl-link-up-p) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	e.AwaitEvalFor(daemonStopBound, "the reconnect loop to be armed",
		`(and (timerp agent-repl-link--reconnect-timer) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	s.awaitInPage(t, "the webapp's failure overlay to draw its daemon-unreachable card",
		`document.querySelector('`+playtestUnreachableCard+`')`)
	p.capture("daemon-down",
		"`agent-repl-frontend-daemon-stop` against the promoted successor",
		"`agent-repl-link-up-p` is nil, the reconnect timer is armed, and the page carries a `daemonUnreachable` failure card",
		"THE OUTAGE IS VISIBLE RATHER THAN SILENT. The panel draws the client-local failure card reading "+
			"\"lost the connection to the daemon; reconnecting\", over a feed that still carries both turns. "+
			"The tabs are still in the bar.")

	e.Eval(`(agent-repl-frontend-daemon-ensure)`)
	// THIS ensure spawns a daemon of Emacs's OWN, so the launcher holds a
	// live pid again -- which is the fact that separates a reconnect onto a
	// fresh daemon from a link that merely came back.
	emHOAwaitNewDaemon(t, e, -1)
	emHOAwaitLinkUp(t, e, "the link to come back when a daemon returns")
	e.AwaitEvalFor(daemonLinkBound, "the reconnect loop to stand down once the link is up",
		`(if (timerp agent-repl-link--reconnect-timer) nil t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	emGHIAssertResponsive(t, e, "a handover followed by a stop and an ensure")
	p.note("`agent-repl-frontend-daemon-ensure` after the outage",
		"the launcher holds a live daemon pid of its own, the link is up, the reconnect timer stood down, and Emacs still answers `(+ 1 1)`")
}

// playtestWithout answers XS with EXCEPT removed, preserving order.
func playtestWithout(xs []string, except string) []string {
	out := make([]string, 0, len(xs))
	for _, x := range xs {
		if x != except {
			out = append(out, x)
		}
	}
	return out
}
